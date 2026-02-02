# =============================================
# Load data (assuming it exists from your script)
# =============================================

# Load the data
cash_rate_daily <- readRDS("combined_data/all_data.Rds") %>%
  mutate(scrape_date = as.Date(scrape_time)) %>%
  group_by(scrape_date, date) %>%
  slice_max(scrape_time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(scrape_date, date, cash_rate)

load("combined_data/rmse_new.RData")

# Meeting schedule
meeting_schedule <- tibble(
  meeting_date = as.Date(c(
    "2022-02-01", "2022-03-01", "2022-04-05", "2022-05-03", "2022-06-07",
    "2022-07-05", "2022-08-02", "2022-09-06", "2022-10-04", "2022-11-01", "2022-12-06",
    "2023-02-07", "2023-03-07", "2023-04-04", "2023-05-02", "2023-06-06",
    "2023-07-04", "2023-08-01", "2023-09-05", "2023-10-03", "2023-11-07", "2023-12-05",
    "2024-02-06", "2024-03-19", "2024-05-07", "2024-06-18", "2024-08-06",
    "2024-09-24", "2024-11-05", "2024-12-10",
    "2025-02-18", "2025-04-01", "2025-05-20", "2025-07-08", "2025-08-12",
    "2025-09-30", "2025-11-04", "2025-12-09",
    "2026-02-03", "2026-03-17", "2026-05-05", "2026-06-16", "2026-08-04",
    "2026-09-15", "2026-11-03", "2026-12-08"
  ))
) %>%
  mutate(
    expiry = if_else(
      day(meeting_date) >= days_in_month(meeting_date) - 1,
      ceiling_date(meeting_date, "month"),
      floor_date(meeting_date, "month")
    )
  ) %>%
  select(expiry, meeting_date)

# Get RBA historical data
rba_historical <- read_rba(series_id = "FIRMMCRTD") %>% arrange(date)
latest_rt <- rba_historical %>% slice_max(date, n = 1) %>% pull(value)

# Process data (simplified version of your script)
all_dates <- sort(unique(cash_rate_daily$scrape_date))

# Create estimates (using your logic)
blend_weight <- function(days_to_meeting) {
  pmax(0, pmin(1, 1 - days_to_meeting / 30))
}

# Generate all estimates
all_list_area <- purrr::map(all_dates, function(scr_date) {
  historical_rate <- rba_historical %>%
    filter(date <= scr_date) %>%
    slice_max(date, n = 1, with_ties = FALSE) %>%
    pull(value)
  
  initial_rt_at_scrape <- if(length(historical_rate) > 0) historical_rate else latest_rt
  
  df_rates <- cash_rate_daily %>% 
    filter(scrape_date == scr_date) %>%
    select(expiry = date, forecast_rate = cash_rate)
  
  df <- meeting_schedule %>%
    distinct(expiry, meeting_date) %>%
    left_join(df_rates, by = "expiry") %>%
    arrange(expiry) %>%
    filter(!is.na(forecast_rate))
  
  if (nrow(df) == 0) return(NULL)
  
  prev_implied <- NA_real_
  out <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    rt_in <- if (is.na(prev_implied)) initial_rt_at_scrape else prev_implied
    
    r_tp1 <- if (row$meeting_date < row$expiry) {
      row$forecast_rate
    } else {
      nb <- (day(row$meeting_date)-1) / days_in_month(row$expiry)
      na <- 1 - nb
      (row$forecast_rate - rt_in * nb) / na
    }
    
    out[[i]] <- tibble(
      scrape_date = scr_date,
      meeting_date = row$meeting_date,
      implied_mean = r_tp1,
      days_to_meeting = as.integer(row$meeting_date - scr_date),
      previous_rate = rt_in
    )
    prev_implied <- r_tp1
  }
  bind_rows(out)
})

all_estimates_area <- all_list_area %>%
  purrr::compact() %>%
  bind_rows() %>%
  filter(days_to_meeting >= 0) %>%
  left_join(rmse_days, by = "days_to_meeting") %>%
  rename(stdev = finalrmse)

# Replace bad stdev values
max_rmse <- max(rmse_days$finalrmse, na.rm = TRUE)
bad_sd <- !is.finite(all_estimates_area$stdev) | is.na(all_estimates_area$stdev) | all_estimates_area$stdev <= 0
all_estimates_area$stdev[bad_sd] <- max_rmse

# Create buckets
current_rate <- rba_historical %>% filter(date == max(date)) %>% pull(value)
bucket_centers_ext <- seq(0.1, 6.1, by = 0.25)
half_width_ext <- 0.125

bucket_list_ext <- vector("list", nrow(all_estimates_area))
for (i in seq_len(nrow(all_estimates_area))) {
  mu_i <- all_estimates_area$implied_mean[i]
  sigma_i <- all_estimates_area$stdev[i]
  d_i <- all_estimates_area$days_to_meeting[i]
  
  if (!is.finite(mu_i)) next
  if (!is.finite(sigma_i) || sigma_i <= 0) sigma_i <- 0.01
  
  p_vec <- sapply(bucket_centers_ext, function(b) {
    lower <- b - half_width_ext
    upper <- b + half_width_ext
    pnorm(upper, mean = mu_i, sd = sigma_i) - pnorm(lower, mean = mu_i, sd = sigma_i)
  })
  
  p_vec[!is.finite(p_vec) | p_vec < 0] <- 0
  s <- sum(p_vec, na.rm = TRUE)
  if (is.finite(s) && s > 0) p_vec <- p_vec / s else p_vec[] <- 0
  
  nearest <- order(abs(bucket_centers_ext - mu_i))[1:2]
  b1 <- min(bucket_centers_ext[nearest])
  b2 <- max(bucket_centers_ext[nearest])
  denom <- (b2 - b1)
  w2 <- if (denom > 0) (mu_i - b1) / denom else 0
  w2 <- min(max(w2, 0), 1)
  l_vec <- numeric(length(bucket_centers_ext))
  l_vec[which(bucket_centers_ext == b1)] <- 1 - w2
  l_vec[which(bucket_centers_ext == b2)] <- w2
  
  blend <- blend_weight(d_i)
  v <- blend * l_vec + (1 - blend) * p_vec
  
  bucket_list_ext[[i]] <- tibble(
    scrape_date = all_estimates_area$scrape_date[i],
    meeting_date = all_estimates_area$meeting_date[i],
    implied_mean = mu_i,
    stdev = sigma_i,
    days_to_meeting = d_i,
    bucket = bucket_centers_ext,
    probability = v
  )
}

all_estimates_buckets_ext <- bind_rows(bucket_list_ext) %>%
  mutate(move = sprintf("%.2f%%", bucket))

rate_labels <- sprintf("%.2f%%", sort(unique(bucket_centers_ext)))
all_estimates_buckets_ext <- all_estimates_buckets_ext %>%
  mutate(move = factor(move, levels = rate_labels))

# =============================================
# UI
# =============================================

ui <- fluidPage(
  titlePanel("RBA Cash Rate Probability Heatmaps"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "meeting_date",
        "Select RBA Meeting:",
        choices = NULL,  # Will be populated in server
        selected = NULL
      ),
      hr(),
      h4("Meeting Information"),
      textOutput("meeting_info"),
      hr(),
      p("This visualization shows the evolution of market-implied probabilities 
         for the cash rate at the selected RBA meeting date.")
    ),
    
    mainPanel(
      width = 9,
      plotlyOutput("heatmap", height = "700px")
    )
  )
)

# =============================================
# Server
# =============================================

server <- function(input, output, session) {
  
  # Get future meetings
  future_meetings <- meeting_schedule %>%
    filter(meeting_date >= min(all_dates)) %>%
    arrange(meeting_date) %>%
    pull(meeting_date)
  
  # Update meeting choices
  meeting_choices <- setNames(
    as.character(future_meetings),
    format(future_meetings - days(1), "%d %B %Y")
  )
  
  updateSelectInput(session, "meeting_date", choices = meeting_choices, 
                   selected = meeting_choices[length(meeting_choices)])
  
  # Meeting info text
  output$meeting_info <- renderText({
    req(input$meeting_date)
    mt <- as.Date(input$meeting_date)
    meeting_date_proper <- mt - days(1)
    
    paste0(
      "Meeting Date: ", format(meeting_date_proper, "%d %B %Y"), "\n",
      "Days from today: ", as.integer(meeting_date_proper - Sys.Date())
    )
  })
  
  # Generate heatmap
  output$heatmap <- renderPlotly({
    req(input$meeting_date)
    
    mt <- as.Date(input$meeting_date)
    meeting_date_proper <- mt - days(1)
    
    # Filter data for this meeting
    df_mt_heat <- all_estimates_buckets_ext %>%
      filter(as.Date(meeting_date) == mt) %>%
      group_by(scrape_date, move, bucket) %>%
      summarise(probability = sum(probability, na.rm = TRUE), .groups = "drop") %>%
      arrange(scrape_date, move)
    
    if (nrow(df_mt_heat) == 0) {
      return(plotly_empty())
    }
    
    # Process data
    df_mt_heat <- df_mt_heat %>%
      filter(!is.na(scrape_date), !is.na(probability),
             is.finite(probability), probability >= 0, !is.na(move)) %>%
      mutate(probability = pmin(probability, 1.0),
             probability = pmax(probability, 0.0))
    
    start_xlim_mt <- min(df_mt_heat$scrape_date, na.rm = TRUE)
    end_xlim_mt <- meeting_date_proper
    
    available_moves <- unique(df_mt_heat$move[!is.na(df_mt_heat$move)])
    valid_move_levels <- rate_labels[rate_labels %in% available_moves]
    
    df_mt_heat <- df_mt_heat %>%
      filter(move %in% valid_move_levels) %>%
      mutate(move = factor(move, levels = valid_move_levels))
    
    # Forward fill
    all_dates_seq <- seq.Date(from = start_xlim_mt, to = end_xlim_mt, by = "day")
    
    bucket_lookup <- df_mt_heat %>% select(move, bucket) %>% distinct()
    
    complete_grid <- expand.grid(
      scrape_date = all_dates_seq,
      move = valid_move_levels,
      stringsAsFactors = FALSE
    ) %>%
      mutate(move = factor(move, levels = valid_move_levels)) %>%
      left_join(bucket_lookup, by = "move")
    
    df_mt_heat <- complete_grid %>%
      left_join(df_mt_heat %>% select(scrape_date, move, probability),
                by = c("scrape_date", "move"))
    
    last_data_dates <- df_mt_heat %>%
      filter(!is.na(probability)) %>%
      group_by(move) %>%
      summarise(last_date = max(scrape_date), .groups = "drop")
    
    df_mt_heat <- df_mt_heat %>%
      left_join(last_data_dates, by = "move") %>%
      group_by(move) %>%
      arrange(scrape_date) %>%
      mutate(probability = ifelse(scrape_date <= last_date,
                                  zoo::na.locf(probability, na.rm = FALSE),
                                  NA_real_)) %>%
      select(-last_date) %>%
      ungroup()
    
    # Actual cash rate line
    actual_rate_line <- rba_historical %>%
      filter(date >= start_xlim_mt, date <= end_xlim_mt) %>%
      mutate(
        rate_label = sprintf("%.2f%%", value),
        closest_level = sapply(value, function(v) {
          valid_move_levels[which.min(abs(as.numeric(gsub("%", "", valid_move_levels)) - v))]
        })
      )
    
    # Prepare data for Plotly
    heat_matrix <- df_mt_heat %>%
      pivot_wider(id_cols = scrape_date, names_from = move, values_from = probability) %>%
      arrange(scrape_date)
    
    dates <- heat_matrix$scrape_date
    heat_matrix <- heat_matrix %>% select(-scrape_date)
    
    # Create hover text
    hover_text <- matrix("", nrow = nrow(heat_matrix), ncol = ncol(heat_matrix))
    for (i in seq_len(nrow(heat_matrix))) {
      for (j in seq_len(ncol(heat_matrix))) {
        prob_val <- heat_matrix[i, j]
        if (!is.na(prob_val)) {
          hover_text[i, j] <- paste0(
            "Date: ", format(dates[i], "%d %b %Y"), "<br>",
            "Cash Rate: ", colnames(heat_matrix)[j], "<br>",
            "Probability: ", sprintf("%.1f%%", prob_val * 100)
          )
        }
      }
    }
    
    # Create plot
    fig <- plot_ly(
      x = dates,
      y = colnames(heat_matrix),
      z = t(as.matrix(heat_matrix)),
      type = "heatmap",
      colorscale = list(
        c(0.00, "#FFFACD"), c(0.15, "#FFD700"), c(0.30, "#FFA500"),
        c(0.45, "#FF6347"), c(0.60, "#FF1493"), c(0.75, "#8B008B"),
        c(0.90, "#4B0082"), c(1.00, "#2E0854")
      ),
      zmin = 0, zmax = 1,
      hoverinfo = "text",
      text = t(hover_text),
      colorbar = list(title = "Probability", tickformat = ".0%", len = 0.6)
    )
    
    # Add actual cash rate line
    if (nrow(actual_rate_line) > 0) {
      fig <- fig %>%
        add_trace(
          x = actual_rate_line$date,
          y = actual_rate_line$closest_level,
          type = "scatter", mode = "lines",
          name = "Actual Cash Rate",
          line = list(color = "#0066CC", width = 2),
          hoverinfo = "skip"
        )
    }
    
    fig %>%
      layout(
        title = paste("Cash Rate Probabilities for", format(meeting_date_proper, "%d %B %Y")),
        xaxis = list(title = "Date", tickformat = "%b-%Y"),
        yaxis = list(title = "Cash Rate (%)", type = "category"),
        hovermode = "closest",
        legend = list(orientation = "v", yanchor = "top", y = 0.99,
                     xanchor = "left", x = 0.02,
                     bgcolor = "rgba(255, 255, 255, 0.7)")
      )
  })
}

# =============================================
# Run App
# =============================================

shinyApp(ui = ui, server = server)
