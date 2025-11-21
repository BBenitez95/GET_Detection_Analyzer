#!/usr/bin/env Rscript
################################################################################
# GET Detection Analyzer
# 
# Detects Gas Exchange Threshold (GET) and Respiratory Compensation Point (RCP)
# from graded exercise test data using piecewise linear regression.
#
################################################################################

if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  shiny,
  ggplot2,
  readxl,
  openxlsx,
  zoo,
  DT,
  segmented
)

# Helper Functions ============================================================

calculate_vo2_peak_rolling <- function(time, vo2, window_sec) {
  if (length(time) < 2 || length(vo2) < 2) {
    return(max(vo2, na.rm = TRUE))
  }
  
  window_min <- window_sec / 60
  half_window <- window_min / 2
  
  rolling_avgs <- numeric(length(time))
  
  for (i in seq_along(time)) {
    t_center <- time[i]
    in_window <- (time >= (t_center - half_window)) & (time <= (t_center + half_window))
    rolling_avgs[i] <- mean(vo2[in_window], na.rm = TRUE)
  }
  
  max(rolling_avgs, na.rm = TRUE)
}

piecewise_fit <- function(x, y, segments = 2) {
  tryCatch({
    df <- data.frame(x = x, y = y)
    lm_model <- lm(y ~ x, data = df)
    seg_model <- segmented(lm_model, seg.Z = ~x, npsi = segments - 1)
    bp_info <- summary(seg_model)$psi
    bp_x <- bp_info[1, "Est."]
    bp_y <- predict(seg_model, newdata = data.frame(x = bp_x))
    slopes <- slope(seg_model)$x
    slope1 <- slopes[1, 1]
    slope2 <- slopes[2, 1]
    r_squared <- summary(seg_model)$r.squared
    
    list(
      bp_x = bp_x,
      bp_y = bp_y,
      slope1 = slope1,
      slope2 = slope2,
      r_squared = r_squared,
      model = seg_model,
      success = TRUE
    )
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

calculate_vo2_peak <- function(time, vo2, window_sec = 30) {
  if (length(time) < 3 || length(vo2) < 3) {
    return(max(vo2, na.rm = TRUE))
  }
  
  time_diffs <- diff(time)
  median_interval <- median(time_diffs, na.rm = TRUE)
  
  if (median_interval == 0) {
    return(max(vo2, na.rm = TRUE))
  }
  
  n_points <- max(3, round(window_sec / median_interval))
  n_points <- min(n_points, length(vo2))
  
  rolling_avg <- rollapply(vo2, width = n_points, FUN = mean, 
                           align = "center", fill = NA, partial = TRUE)
  
  max(rolling_avg, na.rm = TRUE)
}

get_vo2_at_time <- function(time, vo2, target_time, window_sec = 30) {
  if (length(time) < 3 || length(vo2) < 3) {
    idx <- which.min(abs(time - target_time))
    return(vo2[idx])
  }
  
  time_diffs <- diff(time)
  median_interval <- median(time_diffs, na.rm = TRUE)
  
  if (median_interval == 0) {
    idx <- which.min(abs(time - target_time))
    return(vo2[idx])
  }
  
  n_points <- max(3, round(window_sec / median_interval))
  n_points <- min(n_points, length(vo2))
  
  rolling_avg <- rollapply(vo2, width = n_points, FUN = mean, 
                           align = "center", fill = NA, partial = TRUE)
  
  idx <- which.min(abs(time - target_time))
  rolling_avg[idx]
}

load_threshold_data <- function(df) {
  if (is.null(df) || ncol(df) < 4L) {
    stop(
      "DATA FORMAT ERROR\n\n",
      "Required format:\n",
      "  Column 1: Time (numeric)\n",
      "  Column 2: VO2 (numeric)\n",
      "  Column 3: VCO2 (numeric)\n",
      "  Column 4: VE (numeric)\n\n",
      "Please verify your file structure."
    )
  }
  
  first_row_vals <- as.character(df[1, 1:4])
  is_header <- any(
    grepl("time|vo2|vco2|ve|t|min|sec", first_row_vals, ignore.case = TRUE) |
      suppressWarnings(is.na(as.numeric(first_row_vals)))
  )
  
  if (is_header && nrow(df) > 1) {
    df <- df[-1, ]
  }
  
  time_raw <- suppressWarnings(as.numeric(df[[1]]))
  vo2_raw <- suppressWarnings(as.numeric(df[[2]]))
  vco2_raw <- suppressWarnings(as.numeric(df[[3]]))
  ve_raw <- suppressWarnings(as.numeric(df[[4]]))
  
  if (anyNA(time_raw) || anyNA(vo2_raw) || anyNA(vco2_raw) || anyNA(ve_raw)) {
    na_counts <- c(
      sum(is.na(time_raw)),
      sum(is.na(vo2_raw)),
      sum(is.na(vco2_raw)),
      sum(is.na(ve_raw))
    )
    
    stop(
      "DATA FORMAT ERROR\n\n",
      "Unable to convert data to numeric values.\n",
      "NA values found in columns: ", paste(which(na_counts > 0), collapse = ", "), "\n\n",
      "Common causes:\n",
      "  - Text or non-numeric entries in data\n",
      "  - Multiple header rows (only 1 header row supported)\n",
      "  - Merged cells in Excel\n",
      "  - Special characters or units in data cells\n\n",
      "Please ensure all data cells contain only numbers."
    )
  }
  
  if (length(time_raw) < 10L) {
    stop(
      "INSUFFICIENT DATA\n\n",
      "Minimum 10 data points required.\n",
      "Current file contains ", length(time_raw), " points."
    )
  }
  
  list(
    time = time_raw,
    vo2 = vo2_raw,
    vco2 = vco2_raw,
    ve = ve_raw
  )
}

show_error_modal <- function(title, message) {
  showModal(modalDialog(
    title = title,
    tags$div(
      style = "font-family: 'Segoe UI', Arial, sans-serif; 
               font-size: 14px; 
               line-height: 1.6;
               color: #2c3e50;
               padding: 10px;",
      HTML(gsub("\n", "<br/>", message))
    ),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "m"
  ))
}

# UI ==========================================================================

ui <- navbarPage(
  "GET Detection Analyzer",
  
  tabPanel("Analysis",
           tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');
      
      body { 
        font-family: 'Inter', 'Segoe UI', Arial, sans-serif; 
        background: #f8f9fa;
        color: #2c3e50;
      }
      
      h2 { 
        color: #1a252f; 
        font-weight: 700;
        letter-spacing: -0.5px;
      }
      
      .well { 
        background: #ffffff; 
        border-radius: 6px; 
        box-shadow: 0 1px 3px rgba(0,0,0,0.08); 
        padding: 18px; 
        margin-bottom: 16px; 
        border: 1px solid #dee2e6;
      }
      
      .well h4 { 
        color: #1a252f; 
        font-weight: 600; 
        margin-bottom: 14px; 
        padding-bottom: 8px; 
        border-bottom: 2px solid #495057;
        font-size: 15px;
      }
      
      .btn-primary { 
        background: #495057; 
        border: none; 
        border-radius: 4px; 
        font-weight: 600;
        transition: all 0.2s;
        padding: 8px 16px;
      }
      
      .btn-primary:hover { 
        background: #343a40;
        transform: translateY(-1px);
        box-shadow: 0 2px 4px rgba(0,0,0,0.15);
      }
      
      .btn-info {
        background: #6c757d;
        border: none;
        border-radius: 4px;
        font-weight: 600;
      }
      
      .btn-info:hover {
        background: #5a6268;
      }
      
      .btn-success {
        background: #2f9e44;
        border: none;
        border-radius: 4px;
        font-weight: 600;
      }
      
      .btn-success:hover {
        background: #268a3b;
      }
      
      .main-plot-card, .results-card { 
        background: #ffffff; 
        border-radius: 6px; 
        padding: 20px; 
        box-shadow: 0 1px 3px rgba(0,0,0,0.08); 
        margin-bottom: 16px;
        border: 1px solid #dee2e6;
      }
      
      .results-card h4 {
        color: #1a252f;
        font-weight: 600;
        margin-bottom: 16px;
        font-size: 16px;
      }
      
      table.dataTable thead th { 
        background: #f1f3f5; 
        font-weight: 600;
        color: #1a252f;
        border-bottom: 2px solid #dee2e6;
      }
      
      .btn-sm {
        background: #6c757d;
        border: none;
        color: white;
        border-radius: 4px;
        transition: all 0.2s;
      }
      
      .btn-sm:hover {
        background: #5a6268;
        transform: translateY(-1px);
      }
      
      .shiny-notification {
        background: #ffffff;
        border-left: 4px solid #6c757d;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        font-family: 'Inter', sans-serif;
        border-radius: 4px;
        padding: 14px 18px;
      }
      
      .shiny-notification-warning {
        border-left-color: #856404;
        background: #fff3cd;
        color: #856404;
      }
      
      .shiny-notification-error {
        border-left-color: #721c24;
        background: #f8d7da;
        color: #721c24;
      }
      
      .modal-content {
        border-radius: 6px;
        border: none;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .modal-header {
        background: #f8f9fa;
        border-bottom: 1px solid #dee2e6;
        border-radius: 6px 6px 0 0;
      }
      
      .modal-title {
        color: #1a252f;
        font-weight: 600;
      }
    "))),
           
           sidebarLayout(
             sidebarPanel(
               width = 3,
               
               wellPanel(
                 h4("Data Input"),
                 fileInput("file_upload", "Select File", 
                           accept = c(".csv", ".txt", ".xlsx")),
                 conditionalPanel(
                   "output.show_sheet", 
                   selectInput("sheet_name", "Sheet", NULL)
                 ),
                 selectInput("vo2peak_window", "VO₂peak rolling window",
                             choices = c("10 seconds" = "10",
                                         "20 seconds" = "20",
                                         "30 seconds" = "30"),
                             selected = "30")
               ),
               
               wellPanel(
                 h4("Pre-processing"),
                 checkboxInput("smooth_data", "Smooth data (5% window)", FALSE),
                 checkboxInput("remove_outliers", "Remove outliers (LOWESS)", FALSE),
                 fluidRow(
                   column(6, tags$label("SD threshold", style = "font-size: 13px;")),
                   column(6, numericInput("sd_thresh", NULL, 3, 0.1, step = 0.1, width = "100%"))
                 )
               ),
               
               wellPanel(
                 h4("Threshold Detection"),
                 actionButton("detect_get", "Detect GET", 
                              class = "btn-success btn-block"),
                 tags$br(),
                 actionButton("detect_rcp", "Detect RCP", 
                              class = "btn-primary btn-block")
               ),
               
               wellPanel(
                 h4("Display & Export"),
                 selectInput("plot_select", "Figure", 
                             choices = c("GET", "RCP"),
                             selected = "GET"),
                 downloadButton("save_plot", "Save Plot", 
                                class = "btn-sm btn-block"),
                 tags$br(),
                 downloadButton("save_table", "Export Results", 
                                class = "btn-sm btn-block")
               ),
               
               wellPanel(
                 actionButton("show_help", "Help & Documentation", 
                              class = "btn-info btn-block", icon = icon("question-circle"))
               )
             ),
             
             mainPanel(
               width = 9,
               div(class = "main-plot-card", 
                   plotOutput("main_plot", height = "500px")),
               div(class = "results-card", 
                   h4("Threshold Detection Results"), 
                   DTOutput("results_table"))
             )
           )
  ),
  
  tabPanel("About",
           fluidRow(
             column(10, offset = 1,
                    div(class = "main-plot-card", style = "margin-top: 20px;",
                        h2("GET Detection Analyzer"),
                        h4("Version 1.0"),
                        hr(),
                        h4("Description"),
                        p("Automated detection of respiratory thresholds during graded exercise testing using piecewise linear regression with the complete dataset."),
                        tags$ul(
                          tags$li(HTML("<strong>GET:</strong> Gas Exchange Threshold (VCO₂ vs VO₂) - First breakpoint")),
                          tags$li(HTML("<strong>RCP:</strong> Respiratory Compensation Point (VE vs VCO₂) - Second breakpoint"))
                        ),
                        hr(),
                        h4("Citation"),
                        p(style = "font-family: monospace; background: #f8f9fa; padding: 12px; border-left: 3px solid #495057;",
                          "Benitez, B., Succi, PJ., & Snell, E. (2025). GET Detection Analyzer: Automated threshold detection for graded exercise tests (Version 1.0) [Software]. https://github.com/BBenitez95/GET_Detection_Analyzer"
                        ),
                        hr(),
                        h4("License"),
                        p("MIT License - Open source software provided \"AS IS\" without warranty."),
                        hr(),
                        h4("Contact"),
                        p(HTML("<strong>Dr. Brian Benitez</strong><br/>
                 Stetson University<br/>
                 Health Sciences<br/>
                 Email: bbenitez@stetson.edu"))
                    )
             )
           )
  )
)

# Server ======================================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    time = NULL,
    vo2 = NULL,
    vco2 = NULL,
    ve = NULL,
    filepath = NULL,
    sheets = NULL,
    vo2_peak = NULL,
    outlier_indices = NULL,
    results = list(
      get = NULL,
      rcp = NULL
    )
  )
  
  # Help Modal ================================================================
  
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "Help & Documentation",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      div(class = "help-content",
          
          h3("Overview"),
          p("The GET Detection Analyzer identifies respiratory thresholds from graded exercise test data using piecewise linear regression."),
          
          h3("Data Format"),
          p("Input requires four columns: Time (seconds or minutes), VO₂ (L/min or mL/min), VCO₂ (L/min or mL/min), and VE (L/min). Optional header row is automatically detected and removed during import. Supported file formats: CSV, TXT, XLSX."),
          
          h3("Analysis Procedure"),
          tags$ol(
            tags$li("Import graded exercise test data to exhaustion"),
            tags$li("Configure VO₂peak calculation window (10, 20, or 30 seconds)"),
            tags$li("Apply optional pre-processing (smoothing, outlier removal)"),
            tags$li("Detect GET using complete dataset"),
            tags$li("Detect RCP using complete dataset"),
            tags$li("Verify physiological validity (GET_VO₂ < RCP_VO₂)"),
            tags$li("Export threshold parameters and fitted models")
          ),
          
          h3("Detection Methods"),
          
          h4("Gas Exchange Threshold (GET)"),
          p(strong("V-slope Method:"), " Detects the breakpoint in the linear relationship between VCO₂ and VO₂."),
          p("The GET represents the exercise intensity at which VCO₂ production begins to increase disproportionately relative to VO₂, indicating the onset of sustained anaerobic metabolism and the transition from moderate to heavy-intensity exercise."),
          p(strong("Implementation:"), " Piecewise linear regression with two segments fit to VCO₂ vs VO₂ data. The breakpoint represents GET."),
          p(strong("Expected range:"), " Typically 50–70% of VO₂peak"),
          
          h4("Respiratory Compensation Point (RCP)"),
          p(strong("Ventilatory Method:"), " Detects the breakpoint in the linear relationship between VE and VCO₂."),
          p("The RCP represents the exercise intensity at which minute ventilation increases disproportionately relative to VCO₂, indicating metabolic acidosis and hyperventilatory compensation. Marks the transition from heavy to severe-intensity exercise."),
          p(strong("Implementation:"), " Piecewise linear regression with two segments fit to VE vs VCO₂ data. The breakpoint represents RCP."),
          p(strong("Expected range:"), " Typically 80–90% of VO₂peak"),
          
          h3("VO₂peak Determination"),
          p("VO₂peak is calculated as the highest moving average VO₂ over a specified time window. For each time point ", em("t"), ", all VO₂ values occurring within the interval [", em("t"), " - window/2, ", em("t"), " + window/2] are averaged. VO₂peak is defined as the maximum of these averaged values across all time points."),
          p("Window options: 10, 20, or 30 seconds (default: 30 s)"),
          
          h3("Pre-processing Options"),
          
          h4("Smoothing"),
          p("Applies a centered moving average filter with window size equal to 5% of the data length (minimum 3 points). Reduces high-frequency noise while preserving underlying physiological responses."),
          
          h4("Outlier Removal"),
          p("Uses locally weighted scatterplot smoothing (LOWESS, span = 0.1) to estimate baseline trends for VO₂, VCO₂, and VE independently. Data points where any variable exceeds the specified standard deviation threshold (default: 3 SD) from its baseline are removed prior to threshold detection."),
          
          h3("Output"),
          
          h4("Threshold Detection Table"),
          tags$ul(
            tags$li("VO₂peak (L/min and 100% reference)"),
            tags$li("GET: VO₂ at threshold (L/min and % of peak)"),
            tags$li("RCP: VO₂ at threshold (L/min and % of peak)"),
            tags$li("Breakpoint coordinates for each threshold"),
            tags$li("Pre- and post-breakpoint slopes"),
            tags$li("R² (goodness of fit, typically >0.95)")
          ),
          
          h4("Exported Files"),
          p(strong("Plot (PNG):"), " Data points with fitted piecewise linear model, vertical line marking breakpoint location, threshold annotation with VO₂ and % of peak. Outliers shown as red X's if removed."),
          p(strong("Excel file:"), " Three sheets containing (1) Threshold Results, (2) Analysis Settings, and (3) Raw Data."),
          
          h3("Interpretation"),
          
          h4("Physiological Validation"),
          p("GET must occur at a lower VO₂ than RCP. If RCP ≤ GET, the software displays a warning indicating the results are physiologically implausible and should be verified."),
          
          h3("References"),
          p("Beaver, W.L., Wasserman, K., & Whipp, B.J. (1986). A new method for detecting anaerobic threshold by gas exchange. ", em("Journal of Applied Physiology"), ", 60(6), 2020–2027."),
          p("Wasserman, K., Hansen, J.E., Sue, D.Y., Stringer, W.W., & Whipp, B.J. (2012). ", em("Principles of Exercise Testing and Interpretation"), " (5th ed.). Lippincott Williams & Wilkins.")
      )
    ))
  })
  
  # File Upload ===============================================================
  
  output$show_sheet <- reactive({
    !is.null(rv$sheets) && length(rv$sheets) > 0
  })
  outputOptions(output, "show_sheet", suspendWhenHidden = FALSE)
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    rv$results$get <- NULL
    rv$results$rcp <- NULL
    rv$vo2_peak <- NULL
    rv$outlier_indices <- NULL
    
    fp  <- input$file_upload$datapath
    ext <- tools::file_ext(input$file_upload$name)
    
    rv$time <- NULL
    rv$vo2  <- NULL
    rv$vco2 <- NULL
    rv$ve   <- NULL
    rv$sheets <- NULL
    rv$filepath <- NULL
    
    if (ext == "xlsx") {
      tryCatch({
        sheets <- excel_sheets(fp)
        if (length(sheets) == 0L) stop("No readable sheets found.")
        rv$sheets   <- sheets
        rv$filepath <- fp
        updateSelectInput(session, "sheet_name",
                          choices  = rv$sheets,
                          selected = rv$sheets[1])
      }, error = function(e) {
        show_error_modal(
          "File Error",
          paste0(
            "Unable to read Excel file.\n\n",
            "Ensure the file is not corrupted and contains valid data.\n\n",
            "Technical details: ", e$message
          )
        )
      })
      
    } else {
      tryCatch({
        df   <- read.csv(fp, header = FALSE, stringsAsFactors = FALSE)
        vals <- load_threshold_data(df)
        rv$time <- vals$time
        rv$vo2  <- vals$vo2
        rv$vco2 <- vals$vco2
        rv$ve   <- vals$ve
        window_sec <- as.numeric(input$vo2peak_window)
        rv$vo2_peak <- calculate_vo2_peak_rolling(vals$time, vals$vo2, window_sec)
      }, error = function(e) {
        rv$time <- NULL
        rv$vo2  <- NULL
        rv$vco2 <- NULL
        rv$ve   <- NULL
        show_error_modal("Data Format Error", e$message)
      })
    }
  })
  
  observeEvent(input$sheet_name, {
    req(rv$filepath, input$sheet_name)
    
    rv$results$get <- NULL
    rv$results$rcp <- NULL
    rv$vo2_peak <- NULL
    rv$outlier_indices <- NULL
    
    rv$time <- NULL
    rv$vo2  <- NULL
    rv$vco2 <- NULL
    rv$ve   <- NULL
    
    tryCatch({
      df   <- read_excel(rv$filepath, sheet = input$sheet_name, col_names = FALSE)
      vals <- load_threshold_data(df)
      rv$time <- vals$time
      rv$vo2  <- vals$vo2
      rv$vco2 <- vals$vco2
      rv$ve   <- vals$ve
      window_sec <- as.numeric(input$vo2peak_window)
      rv$vo2_peak <- calculate_vo2_peak_rolling(vals$time, vals$vo2, window_sec)
    }, error = function(e) {
      rv$time <- NULL
      rv$vo2  <- NULL
      rv$vco2 <- NULL
      rv$ve   <- NULL
      show_error_modal("Sheet Error", e$message)
    })
  })
  
  observe({
    input$smooth_data
    input$remove_outliers
    input$sd_thresh
    
    if (!is.null(isolate(rv$results$get)) || 
        !is.null(isolate(rv$results$rcp))) {
      rv$results$get <- NULL
      rv$results$rcp <- NULL
      rv$outlier_indices <- NULL
      showNotification(
        "Pre-processing settings changed. Re-run threshold detection.", 
        type = "warning", 
        duration = 4
      )
    }
  })
  
  # Data Processing ===========================================================
  
  get_filtered_data <- reactive({
    req(rv$time, rv$vo2, rv$vco2, rv$ve)
    
    df <- data.frame(
      time = rv$time,
      vo2 = rv$vo2,
      vco2 = rv$vco2,
      ve = rv$ve
    )
    
    outlier_idx <- NULL
    
    if (input$remove_outliers) {
      baseline_vo2 <- lowess(df$vo2 ~ df$time, f = 0.1)$y
      baseline_vco2 <- lowess(df$vco2 ~ df$time, f = 0.1)$y
      baseline_ve <- lowess(df$ve ~ df$time, f = 0.1)$y
      
      resid_vo2 <- df$vo2 - baseline_vo2
      resid_vco2 <- df$vco2 - baseline_vco2
      resid_ve <- df$ve - baseline_ve
      
      keep <- (abs(resid_vo2) < input$sd_thresh * sd(resid_vo2)) &
        (abs(resid_vco2) < input$sd_thresh * sd(resid_vco2)) &
        (abs(resid_ve) < input$sd_thresh * sd(resid_ve))
      
      outlier_idx <- which(!keep)
      
      if (length(outlier_idx) > 0) {
        df <- df[keep, ]
        showNotification(
          sprintf("%d outlier(s) removed", length(outlier_idx)),
          type = "message",
          duration = 3
        )
      }
    }
    
    rv$outlier_indices <- outlier_idx
    
    if (input$smooth_data) {
      w <- max(3, as.integer(nrow(df) * 0.05))
      df$vo2 <- as.numeric(rollapply(df$vo2, width = w, FUN = mean, 
                                     align = "center", fill = NA, partial = TRUE))
      df$vco2 <- as.numeric(rollapply(df$vco2, width = w, FUN = mean, 
                                      align = "center", fill = NA, partial = TRUE))
      df$ve <- as.numeric(rollapply(df$ve, width = w, FUN = mean, 
                                    align = "center", fill = NA, partial = TRUE))
      df <- df[complete.cases(df), ]
    }
    
    window_sec <- as.numeric(input$vo2peak_window)
    rv$vo2_peak <- calculate_vo2_peak_rolling(df$time, df$vo2, window_sec)
    
    df
  })
  
  # GET Detection =============================================================
  
  observeEvent(input$detect_get, {
    data <- get_filtered_data()
    req(data)
    
    withProgress(message = "Detecting GET...", {
      x <- data$vo2
      y <- data$vco2
      
      result <- piecewise_fit(x, y)
      
      if (!result$success) {
        showNotification(
          paste0("GET detection failed: ", result$error),
          type = "error",
          duration = 10
        )
        return()
      }
      
      pct_vo2peak <- (result$bp_x / rv$vo2_peak) * 100
      
      rv$results$get <- list(
        bp_x = result$bp_x,
        bp_y = result$bp_y,
        vo2 = result$bp_x,
        pct_vo2peak = pct_vo2peak,
        slope1 = result$slope1,
        slope2 = result$slope2,
        r_squared = result$r_squared,
        model = result$model,
        x_data = x,
        y_data = y
      )
      
      updateSelectInput(session, "plot_select", selected = "GET")
      
      showNotification(
        sprintf("GET detected at VO₂ = %.3f (%.1f%% of peak)", result$bp_x, pct_vo2peak),
        type = "message",
        duration = 5
      )
    })
  })
  
  # RCP Detection =============================================================
  
  observeEvent(input$detect_rcp, {
    data <- get_filtered_data()
    req(data)
    
    withProgress(message = "Detecting RCP...", {
      x <- data$vco2
      y <- data$ve
      
      result <- piecewise_fit(x, y)
      
      if (!result$success) {
        showNotification(
          paste0("RCP detection failed: ", result$error),
          type = "error",
          duration = 10
        )
        return()
      }
      
      idx <- which.min(abs(data$vco2 - result$bp_x))
      vo2_at_rcp <- data$vo2[idx]
      pct_vo2peak <- (vo2_at_rcp / rv$vo2_peak) * 100
      
      rv$results$rcp <- list(
        bp_x = result$bp_x,
        bp_y = result$bp_y,
        vo2 = vo2_at_rcp,
        pct_vo2peak = pct_vo2peak,
        slope1 = result$slope1,
        slope2 = result$slope2,
        r_squared = result$r_squared,
        model = result$model,
        x_data = x,
        y_data = y
      )
      
      updateSelectInput(session, "plot_select", selected = "RCP")
      
      showNotification(
        sprintf("RCP detected at VO₂ = %.3f (%.1f%% of peak)", vo2_at_rcp, pct_vo2peak),
        type = "message",
        duration = 5
      )
      
      if (!is.null(rv$results$get)) {
        if (vo2_at_rcp <= rv$results$get$vo2) {
          showNotification(
            "WARNING: RCP detected at lower or equal VO₂ than GET. This is physiologically unusual. 
            Please verify your data and detection results.",
            type = "warning",
            duration = 10
          )
        }
      }
    })
  })
  
  # Main Plot =================================================================
  
  output$main_plot <- renderPlot({
    data <- get_filtered_data()
    req(data)
    
    plot_type <- input$plot_select
    
    if (plot_type == "GET") {
      p <- ggplot(data, aes(x = vo2, y = vco2)) +
        geom_point(size = 3, shape = 21, fill = "#495057", 
                   color = "white", stroke = 1.2, alpha = 0.8) +
        labs(title = "GET Detection (V-Slope Method)", 
             x = expression(paste("VO"[2], " (L/min)")),
             y = expression(paste("VCO"[2], " (L/min)"))) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold", size = 18, color = "#1a252f"),
          axis.title = element_text(face = "bold", size = 14),
          panel.grid.major = element_line(color = "#e9ecef"),
          panel.grid.minor = element_line(color = "#f8f9fa"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
      
      if (!is.null(rv$outlier_indices) && length(rv$outlier_indices) > 0) {
        outlier_data <- data.frame(
          vo2 = rv$vo2[rv$outlier_indices],
          vco2 = rv$vco2[rv$outlier_indices]
        )
        p <- p + geom_point(
          data = outlier_data,
          aes(x = vo2, y = vco2),
          size = 3, shape = 4, color = "#c92a2a", stroke = 1.5,
          alpha = 0.6
        )
      }
      
      if (!is.null(rv$results$get)) {
        res <- rv$results$get
        x_seq <- seq(min(res$x_data), max(res$x_data), length.out = 200)
        y_pred <- predict(res$model, newdata = data.frame(x = x_seq))
        
        p <- p +
          geom_line(data = data.frame(x = x_seq, y = y_pred),
                    aes(x = x, y = y), color = "#2f9e44", size = 1.5) +
          geom_vline(xintercept = res$bp_x, linetype = "dashed", 
                     color = "#1864ab", size = 1) +
          annotate("text", x = res$bp_x, y = max(data$vco2) * 0.95,
                   label = sprintf("GET\nVO₂ = %.2f L/min\n%.1f%% of peak", 
                                   res$vo2, res$pct_vo2peak),
                   hjust = -0.1, vjust = 1, size = 4, fontface = "bold")
      }
      
    } else if (plot_type == "RCP") {
      p <- ggplot(data, aes(x = vco2, y = ve)) +
        geom_point(size = 3, shape = 21, fill = "#495057", 
                   color = "white", stroke = 1.2, alpha = 0.8) +
        labs(title = "RCP Detection", 
             x = expression(paste("VCO"[2], " (L/min)")),
             y = "VE (L/min)") +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold", size = 18, color = "#1a252f"),
          axis.title = element_text(face = "bold", size = 14),
          panel.grid.major = element_line(color = "#e9ecef"),
          panel.grid.minor = element_line(color = "#f8f9fa"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
      
      if (!is.null(rv$outlier_indices) && length(rv$outlier_indices) > 0) {
        outlier_data <- data.frame(
          vco2 = rv$vco2[rv$outlier_indices],
          ve = rv$ve[rv$outlier_indices]
        )
        p <- p + geom_point(
          data = outlier_data,
          aes(x = vco2, y = ve),
          size = 3, shape = 4, color = "#c92a2a", stroke = 1.5,
          alpha = 0.6
        )
      }
      
      if (!is.null(rv$results$rcp)) {
        res <- rv$results$rcp
        x_seq <- seq(min(res$x_data), max(res$x_data), length.out = 200)
        y_pred <- predict(res$model, newdata = data.frame(x = x_seq))
        
        p <- p +
          geom_line(data = data.frame(x = x_seq, y = y_pred),
                    aes(x = x, y = y), color = "#c92a2a", size = 1.5) +
          geom_vline(xintercept = res$bp_x, linetype = "dashed", 
                     color = "#1864ab", size = 1) +
          annotate("text", x = res$bp_x, y = max(data$ve) * 0.95,
                   label = sprintf("RCP\nVCO₂ = %.2f L/min\nVO₂ = %.2f L/min\n%.1f%% of peak", 
                                   res$bp_x, res$vo2, res$pct_vo2peak),
                   hjust = -0.1, vjust = 1, size = 4, fontface = "bold")
      }
    }
    
    print(p)
  })
  
  # Results Table =============================================================
  
  output$results_table <- renderDT({
    results_list <- list()
    
    if (!is.null(rv$vo2_peak)) {
      results_list[[length(results_list) + 1]] <- data.frame(
        Type = "VO₂ peak",
        VO2_L_min = sprintf("%.3f", rv$vo2_peak),
        Percent_VO2peak = "100.0%",
        Breakpoint_X = "—",
        Breakpoint_Y = "—",
        Slope_1 = "—",
        Slope_2 = "—",
        R_squared = "—",
        stringsAsFactors = FALSE
      )
    }
    
    if (!is.null(rv$results$get)) {
      res <- rv$results$get
      results_list[[length(results_list) + 1]] <- data.frame(
        Type = "GET",
        VO2_L_min = sprintf("%.3f", res$vo2),
        Percent_VO2peak = sprintf("%.1f%%", res$pct_vo2peak),
        Breakpoint_X = sprintf("%.3f", res$bp_x),
        Breakpoint_Y = sprintf("%.3f", res$bp_y),
        Slope_1 = sprintf("%.3f", res$slope1),
        Slope_2 = sprintf("%.3f", res$slope2),
        R_squared = sprintf("%.4f", res$r_squared),
        stringsAsFactors = FALSE
      )
    }
    
    if (!is.null(rv$results$rcp)) {
      res <- rv$results$rcp
      results_list[[length(results_list) + 1]] <- data.frame(
        Type = "RCP",
        VO2_L_min = sprintf("%.3f", res$vo2),
        Percent_VO2peak = sprintf("%.1f%%", res$pct_vo2peak),
        Breakpoint_X = sprintf("%.3f", res$bp_x),
        Breakpoint_Y = sprintf("%.2f", res$bp_y),
        Slope_1 = sprintf("%.3f", res$slope1),
        Slope_2 = sprintf("%.3f", res$slope2),
        R_squared = sprintf("%.4f", res$r_squared),
        stringsAsFactors = FALSE
      )
    }
    
    if (length(results_list) == 0) {
      results_df <- data.frame(
        Message = "No data loaded. Upload a file to begin.",
        stringsAsFactors = FALSE
      )
      
      datatable(
        results_df,
        options = list(
          dom = "t",
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        selection = "none"
      )
    } else {
      results_df <- do.call(rbind, results_list)
      
      datatable(
        results_df,
        options = list(
          dom = "t",
          ordering = FALSE,
          pageLength = 10,
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        selection = "none",
        colnames = c(
          "Type",
          "VO₂ (L/min)",
          "% VO₂peak",
          "Breakpoint X",
          "Breakpoint Y",
          "Slope 1",
          "Slope 2",
          "R²"
        )
      ) %>%
        formatStyle(
          "Type",
          target = "row",
          fontWeight = styleEqual("VO₂ peak", "bold"),
          backgroundColor = styleEqual("VO₂ peak", "#f8f9fa")
        )
    }
  })
  
  # Export Handlers ===========================================================
  
  output$save_plot <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      plot_name <- gsub(" ", "_", tolower(input$plot_select))
      paste0("GET_", plot_name, "_", timestamp, ".png")
    },
    content = function(file) {
      data <- get_filtered_data()
      req(data)
      
      plot_type <- input$plot_select
      
      if (plot_type == "GET") {
        p <- ggplot(data, aes(x = vo2, y = vco2)) +
          geom_point(size = 3, shape = 21, fill = "#495057", 
                     color = "white", stroke = 1.2, alpha = 0.8) +
          labs(title = "GET Detection (V-Slope Method)", 
               x = expression(paste("VO"[2], " (L/min)")),
               y = expression(paste("VCO"[2], " (L/min)"))) +
          theme_minimal(base_size = 13) +
          theme(
            plot.title = element_text(face = "bold", size = 18, color = "#1a252f"),
            axis.title = element_text(face = "bold", size = 14),
            panel.grid.major = element_line(color = "#e9ecef"),
            panel.grid.minor = element_line(color = "#f8f9fa"),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
          )
        
        if (!is.null(rv$outlier_indices) && length(rv$outlier_indices) > 0) {
          outlier_data <- data.frame(
            vo2 = rv$vo2[rv$outlier_indices],
            vco2 = rv$vco2[rv$outlier_indices]
          )
          p <- p + geom_point(
            data = outlier_data,
            aes(x = vo2, y = vco2),
            size = 3, shape = 4, color = "#c92a2a", stroke = 1.5,
            alpha = 0.6
          )
        }
        
        if (!is.null(rv$results$get)) {
          res <- rv$results$get
          x_seq <- seq(min(res$x_data), max(res$x_data), length.out = 200)
          y_pred <- predict(res$model, newdata = data.frame(x = x_seq))
          
          p <- p +
            geom_line(data = data.frame(x = x_seq, y = y_pred),
                      aes(x = x, y = y), color = "#2f9e44", size = 1.5) +
            geom_vline(xintercept = res$bp_x, linetype = "dashed", 
                       color = "#1864ab", size = 1)
        }
        
      } else if (plot_type == "RCP") {
        p <- ggplot(data, aes(x = vco2, y = ve)) +
          geom_point(size = 3, shape = 21, fill = "#495057", 
                     color = "white", stroke = 1.2, alpha = 0.8) +
          labs(title = "RCP Detection", 
               x = expression(paste("VCO"[2], " (L/min)")),
               y = "VE (L/min)") +
          theme_minimal(base_size = 13) +
          theme(
            plot.title = element_text(face = "bold", size = 18, color = "#1a252f"),
            axis.title = element_text(face = "bold", size = 14),
            panel.grid.major = element_line(color = "#e9ecef"),
            panel.grid.minor = element_line(color = "#f8f9fa"),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
          )
        
        if (!is.null(rv$outlier_indices) && length(rv$outlier_indices) > 0) {
          outlier_data <- data.frame(
            vco2 = rv$vco2[rv$outlier_indices],
            ve = rv$ve[rv$outlier_indices]
          )
          p <- p + geom_point(
            data = outlier_data,
            aes(x = vco2, y = ve),
            size = 3, shape = 4, color = "#c92a2a", stroke = 1.5,
            alpha = 0.6
          )
        }
        
        if (!is.null(rv$results$rcp)) {
          res <- rv$results$rcp
          x_seq <- seq(min(res$x_data), max(res$x_data), length.out = 200)
          y_pred <- predict(res$model, newdata = data.frame(x = x_seq))
          
          p <- p +
            geom_line(data = data.frame(x = x_seq, y = y_pred),
                      aes(x = x, y = y), color = "#c92a2a", size = 1.5) +
            geom_vline(xintercept = res$bp_x, linetype = "dashed", 
                       color = "#1864ab", size = 1)
        }
      }
      
      ggsave(
        file,
        plot   = p,
        width  = 10,
        height = 7,
        dpi    = 300
      )
    }
  )
  
  output$save_table <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("GET_threshold_results_", timestamp, ".xlsx")
    },
    content = function(file) {
      data <- get_filtered_data()
      req(data)
      
      results_list <- list()
      
      if (!is.null(rv$vo2_peak)) {
        results_list[[length(results_list) + 1]] <- data.frame(
          Type = "VO2_peak",
          VO2_L_min = rv$vo2_peak,
          Percent_VO2peak = 100.0,
          Breakpoint_X = NA,
          Breakpoint_Y = NA,
          Slope_1 = NA,
          Slope_2 = NA,
          R_squared = NA,
          stringsAsFactors = FALSE
        )
      }
      
      if (!is.null(rv$results$get)) {
        res <- rv$results$get
        results_list[[length(results_list) + 1]] <- data.frame(
          Type = "GET",
          VO2_L_min = res$vo2,
          Percent_VO2peak = res$pct_vo2peak,
          Breakpoint_X = res$bp_x,
          Breakpoint_Y = res$bp_y,
          Slope_1 = res$slope1,
          Slope_2 = res$slope2,
          R_squared = res$r_squared,
          stringsAsFactors = FALSE
        )
      }
      
      if (!is.null(rv$results$rcp)) {
        res <- rv$results$rcp
        results_list[[length(results_list) + 1]] <- data.frame(
          Type = "RCP",
          VO2_L_min = res$vo2,
          Percent_VO2peak = res$pct_vo2peak,
          Breakpoint_X = res$bp_x,
          Breakpoint_Y = res$bp_y,
          Slope_1 = res$slope1,
          Slope_2 = res$slope2,
          R_squared = res$r_squared,
          stringsAsFactors = FALSE
        )
      }
      
      results_df <- if (length(results_list) > 0) {
        do.call(rbind, results_list)
      } else {
        data.frame(
          Message = "No thresholds detected",
          stringsAsFactors = FALSE
        )
      }
      
      settings_df <- data.frame(
        Parameter = c(
          "Analysis_timestamp",
          "Software_version",
          "VO2_peak_calculation",
          "VO2_peak_rolling_window_sec",
          "Smoothing_applied",
          "Outlier_removal_applied",
          "SD_threshold",
          "Number_outliers_removed",
          "Number_data_points",
          "VO2_peak_L_min",
          "GET_detected",
          "RCP_detected"
        ),
        Value = c(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          "1.0",
          "Highest rolling average (time-based)",
          input$vo2peak_window,
          if (input$smooth_data) "YES" else "NO",
          if (input$remove_outliers) "YES" else "NO",
          if (input$remove_outliers) as.character(input$sd_thresh) else "N/A",
          if (!is.null(rv$outlier_indices)) as.character(length(rv$outlier_indices)) else "0",
          as.character(nrow(data)),
          if (!is.null(rv$vo2_peak)) sprintf("%.3f", rv$vo2_peak) else "N/A",
          if (!is.null(rv$results$get)) "YES" else "NO",
          if (!is.null(rv$results$rcp)) "YES" else "NO"
        ),
        stringsAsFactors = FALSE
      )
      
      raw_df <- data.frame(
        Time = data$time,
        VO2_L_min = data$vo2,
        VCO2_L_min = data$vco2,
        VE_L_min = data$ve,
        Delta_VCO2_minus_VO2 = data$vco2 - data$vo2
      )
      
      write.xlsx(
        list(
          Threshold_Results = results_df,
          Analysis_Settings = settings_df,
          Raw_Data = raw_df
        ),
        file,
        rowNames = FALSE
      )
    }
  )
}

# Run Application =============================================================

shinyApp(ui = ui, server = server)