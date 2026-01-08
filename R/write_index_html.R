# write_index_html.R

suppressPackageStartupMessages({
  library(stringr)
})


# ==============================================================================
# READ AND DISPLAY RBA SUMMARY
# ===========

intro_paragraph <- '
  <p style="max-width: 900px; margin: 0 auto 30px auto; text-align: center; font-size: 1.1rem; color: #444;">
    This website is built by Zac Gross and provides a daily snapshot of <strong>futures-implied expectations</strong> for the Reserve Bank of Australia\'s cash rate,
    based on ASX 30-day interbank futures data and historical data. These expectations update automatically based off code by Matt Cowgill.
  </p>'

# ====== Analytics snippet ======
ga_id <- "G-5J5TP6ZN7H"
plausible_domain <- "isaacgross.net"

analytics_snippet <- sprintf('
<!-- Google tag (gtag.js) -->
<script async src="https://www.googletagmanager.com/gtag/js?id=G-5J5TP6ZN7H"></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag("js", new Date());

  gtag("config", "G-5J5TP6ZN7H");
</script>')
# ==============================

# Ensure target directory exists
if (!dir.exists("docs/meetings")) dir.create("docs/meetings", recursive = TRUE)
if (!dir.exists("docs/meeting_lines")) dir.create("docs/meeting_lines", recursive = TRUE)

# Find meeting HTMLs (interactive heatmaps)
html_basenames <- list.files(
  "docs/meetings",
  pattern = "^daily_heatmap_\\d{4}-\\d{2}-\\d{2}\\.html$",
  full.names = FALSE
)

# Get current date
current_date <- Sys.Date()

# Sort and separate into future and past meetings
future_cards <- character(0)
past_cards <- character(0)

if (length(html_basenames) > 0) {
  dates_chr <- str_match(html_basenames, "daily_heatmap_(\\d{4}-\\d{2}-\\d{2})\\.html")[, 2]
  dates_obj <- as.Date(dates_chr, format = "%Y-%m-%d")
  
  # Separate future and past
  future_idx <- dates_obj >= current_date
  past_idx <- dates_obj < current_date
  
# Sort future meetings (earliest first)
  if (any(future_idx)) {
    future_files <- html_basenames[future_idx]
    future_dates <- dates_obj[future_idx]
    future_ord <- order(future_dates, decreasing = FALSE, na.last = TRUE)
    future_files <- future_files[future_ord]
    future_dates <- future_dates[future_ord]
    
    future_cards <- vapply(
      seq_along(future_files),
      function(i) {
        file <- file.path("meetings", future_files[i])
        sprintf(
          '<div class="chart-card">
  <iframe src="%s" class="chart-iframe" frameborder="0"></iframe>
</div>', 
          file
        )
      },
      character(1)
    )
  }
  
  # Sort past meetings (most recent first) - now as static images
  if (any(past_idx)) {
    past_files <- html_basenames[past_idx]
    past_dates <- dates_obj[past_idx]
    past_ord <- order(past_dates, decreasing = TRUE, na.last = TRUE)
    past_files <- past_files[past_ord]
    past_dates <- past_dates[past_ord]
    
past_cards <- vapply(
      seq_along(past_files),
      function(i) {
        # Look for corresponding PNG file
        png_file <- sub("\\.html$", ".png", past_files[i])
        png_path <- file.path("meetings", png_file)
        date_label <- format(past_dates[i], "%d %B %Y")
        
        # Check if PNG exists, otherwise fall back to iframe
        if (file.exists(file.path("docs", png_path))) {
          sprintf(
            '<div class="chart-card">
  <h3 style="margin: 0 0 15px 0; color: #2c3e50;">%s</h3>
  <img src="%s" alt="%s" class="expandable" style="width: 100%%; height: auto; border-radius: 6px;" />
</div>', 
            date_label, png_path, date_label
          )
        } else {
          # Fallback to iframe if PNG doesn't exist
          file <- file.path("meetings", past_files[i])
          sprintf(
            '<div class="chart-card">
  <h3 style="margin: 0 0 15px 0; color: #2c3e50;">%s</h3>
  <iframe src="%s" class="chart-iframe" frameborder="0"></iframe>
</div>', 
            date_label, file
          )
        }
      },
      character(1)
    )
  }
}

# Interactive line chart section - now using HTML file
interactive_line_section <- ""
if (file.exists("docs/line_interactive.html")) {
  interactive_line_section <- '
  <h1 style="margin-top:60px; text-align:center;">
    Forecasts for the Next RBA Meeting Derived from ASX Futures
  </h1>
  <div style="
      display: flex;
      justify-content: center;
      margin: 40px 0;
    ">
    <iframe 
      src="line_interactive.html" 
      style="
        width: 80%;
        height: 600px;
        border: none;
        border-radius: 15px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
        background: white;
      "
      frameborder="0"
    ></iframe>
  </div>
  <p style="max-width: 800px; margin: 20px auto; text-align: center; font-size: 1rem; color: #555; line-height: 1.6;">
    This chart shows the market-implied probability of different cash rate outcomes at the next RBA meeting, hover over the chart to explore daily probabilities. 
    <a href="plots/line_charts/line.png" target="_blank" style="color: #3498db; text-decoration: none; font-weight: 500;">View static version →</a>
  </p>'
} else if (file.exists("docs/plots/line_charts/line.png")) {
  # Fallback to PNG if HTML doesn't exist
  interactive_line_section <- '
  <h1 style="margin-top:60px; text-align:center;">
    Forecasts for the Next RBA Meeting Derived from ASX Futures
  </h1>
  <div style="
      display: flex;
      justify-content: center;
      margin: 40px 0;
    ">
    <img 
      src="plots/line_charts/line.png" 
      alt="Next RBA Meeting Line Chart"
      class="expandable"
      style="
        width: 80%;
        height: auto;
        border-radius: 15px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
      "
    />
  </div>
  <p style="max-width: 800px; margin: 20px auto; text-align: center; font-size: 1rem; color: #555; line-height: 1.6;">
    This chart shows the market-implied probability of different cash rate outcomes at the next RBA meeting.
  </p>'
}

forecast_paths_section <- ""
if (file.exists("docs/cash_rate_forecast_paths.html")) {
  forecast_paths_section <- '
  <h1 style="margin-top:60px; text-align:center;">
    Cash Rate Forecast Paths Around Key Events
  </h1>
  <div style="
      display: flex;
      justify-content: center;
      margin: 40px 0;
    ">
    <iframe
      src="cash_rate_forecast_paths.html"
      style="
        width: 90%;
        height: 650px;
        border: none;
        border-radius: 15px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
        background: white;
      "
      frameborder="0"
    ></iframe>
  </div>
  <p style="max-width: 900px; margin: 20px auto; text-align: center; font-size: 1rem; color: #555; line-height: 1.6;">
    Interactive forecast paths around RBA meetings, CPI releases, and labour force publications. Hover to inspect each scrape and
    use the scroll wheel or pinch gesture to zoom the timeline.
    <a href="cash_rate_forecast_paths.html" target="_blank" style="color: #3498db; text-decoration: none; font-weight: 500;">
      Open full-screen →
    </a>
    <a href="plots/forecast_paths/cash_rate_forecast_paths.png" target="_blank" style="color: #3498db; text-decoration: none; font-weight: 500;">
      View static version →
    </a>
  </p>'
} else if (file.exists("docs/plots/forecast_paths/cash_rate_forecast_paths.png")) {
  forecast_paths_section <- '
  <h1 style="margin-top:60px; text-align:center;">
    Cash Rate Forecast Paths Around Key Events
  </h1>
  <div style="
      display: flex;
      justify-content: center;
      margin: 40px 0;
    ">
    <img
      src="plots/forecast_paths/cash_rate_forecast_paths.png"
      alt="Cash rate forecast paths"
      class="expandable"
      style="
        width: 90%;
        height: auto;
        border-radius: 15px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
      "
    />
  </div>
  <p style="max-width: 900px; margin: 20px auto; text-align: center; font-size: 1rem; color: #555; line-height: 1.6;">
    Forecast paths around RBA meetings, CPI releases, and labour force publications. Interactive version currently unavailable.
  </p>'
}

# Future meetings section
# Line probability charts by meeting (future and past)
line_prob_files <- list.files(
  "docs/meeting_lines",
  pattern = "^line_probabilities_meeting_\\d{4}-\\d{2}-\\d{2}\\.png$",
  full.names = FALSE
)

future_line_cards <- character(0)
past_line_cards <- character(0)

if (length(line_prob_files) > 0) {
  line_prob_dates <- str_match(line_prob_files, "line_probabilities_meeting_(\\d{4}-\\d{2}-\\d{2})\\.png")[, 2]
  line_prob_dates_obj <- as.Date(line_prob_dates, format = "%Y-%m-%d")

  order_idx <- order(line_prob_dates_obj, decreasing = FALSE, na.last = TRUE)
  line_prob_files <- line_prob_files[order_idx]
  line_prob_dates_obj <- line_prob_dates_obj[order_idx]

  future_line_cards <- vapply(
    which(line_prob_dates_obj >= current_date),
    function(i) {
      png_path <- file.path("meeting_lines", line_prob_files[i])
      date_label <- format(line_prob_dates_obj[i], "%d %B %Y")

      sprintf(
        '<div class="chart-card">
  <h3 style="margin: 0 0 15px 0; color: #2c3e50;">%s</h3>
  <img src="%s" alt="Meeting probability lines for %s" class="expandable" style="width: 100%%; height: auto; border-radius: 6px;" />
</div>',
        date_label, png_path, date_label
      )
    },
    character(1)
  )

  past_line_cards <- vapply(
    which(line_prob_dates_obj < current_date),
    function(i) {
      png_path <- file.path("meeting_lines", line_prob_files[i])
      date_label <- format(line_prob_dates_obj[i], "%d %B %Y")

      sprintf(
        '<div class="chart-card">
  <h3 style="margin: 0 0 15px 0; color: #2c3e50;">%s</h3>
  <img src="%s" alt="Meeting probability lines for %s" class="expandable" style="width: 100%%; height: auto; border-radius: 6px;" />
</div>',
        date_label, png_path, date_label
      )
    },
    character(1)
  )
}

# Future meetings visualization tabs
heatmap_tab <- if (length(future_cards) > 0) {
  paste('<div class="grid">', paste(future_cards, collapse = "\n"), '</div>')
} else {
  '<p style="text-align:center;">No upcoming RBA meeting heatmaps available.</p>'
}

line_tab <- if (length(future_line_cards) > 0) {
  paste('<div class="grid">', paste(future_line_cards, collapse = "\n"), '</div>')
} else {
  '<p style="text-align:center;">No upcoming RBA meeting line charts available.</p>'
}

future_meeting_section <- sprintf('
  <section>
    <h1>Cash Rate Target Probabilities By RBA Meeting</h1>
    <div class="tab-buttons" role="tablist">
      <button class="tab-button active" data-target="heatmap" aria-pressed="true">Heatmaps</button>
      <button class="tab-button" data-target="line" aria-pressed="false">Line Charts</button>
    </div>
    <div class="tab-content active" id="tab-heatmap">%s</div>
    <div class="tab-content" id="tab-line">%s</div>
  </section>',
  heatmap_tab,
  line_tab
)

# Past meetings section
past_meeting_section <- if (length(past_cards) > 0) {
  paste('<h1 style="margin-top:60px; text-align:center;">Previous Meetings</h1>\n<div class="grid">',
        paste(past_cards, collapse = "\n"), '</div>')
} else {
  ""
}

past_line_section <- if (length(past_line_cards) > 0) {
  paste('<h1 style="margin-top:60px; text-align:center;">Past Meeting Line Charts</h1>\n<div class="grid">',
        paste(past_line_cards, collapse = "\n"), '</div>')
} else {
  ""
}

# Assemble HTML
html <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <title>Rate Outcome Probabilities by RBA Meeting</title>
 
  <style>
    body {
      font-family: "Segoe UI", Roboto, sans-serif;
      background-color: #f5f7fa;
      color: #333;
      margin: 0;
      padding: 40px 20px;
    }
    h1 {
      text-align: center;
      margin-bottom: 30px;
      font-size: 2rem;
      color: #2c3e50;
    }
    .grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(900px, 1fr));
      gap: 30px;
      padding: 10px;
      max-width: 2200px;
      margin: 0 auto;
    }
    .tab-buttons {
      display: flex;
      gap: 10px;
      justify-content: center;
      margin: 20px 0 30px 0;
      flex-wrap: wrap;
    }
    .tab-button {
      padding: 10px 18px;
      border: 1px solid #d1d5db;
      background: #f8fafc;
      border-radius: 999px;
      cursor: pointer;
      font-weight: 600;
      color: #334155;
      transition: all 0.2s ease;
    }
    .tab-button:hover {
      background: #e2e8f0;
    }
    .tab-button.active {
      background: #2563eb;
      color: #fff;
      border-color: #1d4ed8;
      box-shadow: 0 8px 16px rgba(37, 99, 235, 0.2);
    }
    .tab-content {
      display: none;
    }
    .tab-content.active {
      display: block;
    }
    .chart-card {
      background: #fff;
      border-radius: 10px;
      box-shadow: 0 4px 10px rgba(0, 0, 0, 0.05);
      padding: 18px;
      text-align: center;
    }
    .chart-iframe {
      width: 100%%;
      height: 600px;
      border-radius: 6px;
      background: white;
    }
    .expandable {
      cursor: pointer;
      transition: opacity 0.2s;
    }
    .expandable:hover {
      opacity: 0.85;
    }
    
    /* Lightbox modal styles */
    .lightbox {
      display: none;
      position: fixed;
      z-index: 9999;
      left: 0;
      top: 0;
      width: 100%%;
      height: 100%%;
      background-color: rgba(0, 0, 0, 0.9);
      align-items: center;
      justify-content: center;
    }
    .lightbox.active {
      display: flex;
    }
    .lightbox-content {
      max-width: 95%%;
      max-height: 95%%;
      object-fit: contain;
      border-radius: 8px;
    }
    .lightbox-close {
      position: absolute;
      top: 20px;
      right: 35px;
      color: #f1f1f1;
      font-size: 40px;
      font-weight: bold;
      cursor: pointer;
      transition: color 0.2s;
    }
    .lightbox-close:hover {
      color: #bbb;
    }
    
    @media (max-width: 1400px) {
      .grid {
        grid-template-columns: repeat(auto-fill, minmax(700px, 1fr));
        max-width: 1600px;
      }
      .chart-iframe {
        height: 550px;
      }
      .tab-buttons {
        flex-direction: column;
        align-items: stretch;
      }
    }
    @media (max-width: 768px) {
      .grid {
        grid-template-columns: 1fr;
        max-width: 95vw;
      }
      .chart-iframe {
        height: 500px;
      }
      .lightbox-close {
        top: 10px;
        right: 20px;
        font-size: 30px;
      }
    }
  </style>
</head>
  <body>

  %s

  %s

  %s

  %s

  %s



  <!-- Lightbox Modal -->
  <div id="lightbox" class="lightbox">
    <span class="lightbox-close">&times;</span>
    <img class="lightbox-content" id="lightbox-img" alt="Expanded view">
  </div>
  
  <script>
    // Get the lightbox elements
    const lightbox = document.getElementById("lightbox");
    const lightboxImg = document.getElementById("lightbox-img");
    const closeBtn = document.querySelector(".lightbox-close");
    
    // Add click listeners to all expandable images
    document.querySelectorAll(".expandable").forEach(img => {
      img.addEventListener("click", function() {
        lightbox.classList.add("active");
        lightboxImg.src = this.src;
        lightboxImg.alt = this.alt;
      });
    });
    
    // Close the lightbox when clicking the X button
    closeBtn.addEventListener("click", function() {
      lightbox.classList.remove("active");
    });
    
    // Close the lightbox when clicking outside the image
    lightbox.addEventListener("click", function(e) {
      if (e.target === lightbox) {
        lightbox.classList.remove("active");
      }
    });

    // Close the lightbox with Escape key
    document.addEventListener("keydown", function(e) {
      if (e.key === "Escape" && lightbox.classList.contains("active")) {
        lightbox.classList.remove("active");
      }
    });

    // Tab switching for future meetings
    const tabButtons = document.querySelectorAll(".tab-button");
    const tabContents = document.querySelectorAll(".tab-content");

    tabButtons.forEach(button => {
      button.addEventListener("click", () => {
        const target = button.dataset.target;

        tabButtons.forEach(btn => {
          btn.classList.remove("active");
          btn.setAttribute("aria-pressed", "false");
        });

        tabContents.forEach(content => {
          content.classList.toggle("active", content.id === `tab-${target}`);
        });

        button.classList.add("active");
        button.setAttribute("aria-pressed", "true");
      });
    });
  </script>

</body>
</html>
',
  analytics_snippet,

  intro_paragraph,

  interactive_line_section,

  forecast_paths_section,

  future_meeting_section
)

# Write output
writeLines(html, "docs/index.html")
message("✅ index.html written with ", length(future_cards), " upcoming meeting charts and ", length(past_cards), " past meeting charts.")
