library(shiny)
library(httr)
library(jsonlite)
library(readxl)
library(writexl)
library(DT)

# Increase max upload size to 50MB
options(shiny.maxRequestSize = 50 * 1024^2)

# ── Claude evaluation function ────────────────────────────────────────────────
evaluate_abstract <- function(abstract_text, api_key) {
  url <- "https://api.anthropic.com/v1/messages"
  
  prompt <- paste0(
    "You are a scientific literature analyst specializing in toxicology and epidemiology. ",
    "Evaluate the following abstract and estimate the probability (0-100) that the FULL TEXT paper ",
    "will provide empirical evidence for each of the three types of concordance:\n\n",
    "1. Dose-Response Concordance: Evidence that greater exposure leads to greater effect (clear dose gradient).\n",
    "2. Temporal Concordance: Evidence that exposure precedes the outcome in time (time-course or sequential data).\n",
    "3. Incidence Concordance: Evidence that exposure rates align with disease/outcome rates across populations or groups.\n\n",
    "Return ONLY a valid JSON object with exactly these three keys and integer values 0-100. ",
    "No explanation, no markdown, no code fences. Example: ",
    "{\"dose_response\": 75, \"temporal\": 60, \"incidence\": 30}\n\n",
    "Abstract:\n", abstract_text
  )
  
  payload <- list(
    model      = "claude-haiku-4-5-20251001",
    max_tokens = 100,
    messages   = list(list(role = "user", content = prompt))
  )
  
  tryCatch({
    res <- POST(
      url,
      add_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "Content-Type"      = "application/json"
      ),
      body   = toJSON(payload, auto_unbox = TRUE),
      encode = "raw",
      timeout(30)
    )
    
    raw_text <- content(res)$content[[1]]$text
    if (is.null(raw_text)) return(list(dose_response = NA, temporal = NA, incidence = NA))
    
    # Extract JSON object robustly
    clean      <- gsub("```[a-zA-Z]*|```", "", raw_text)
    clean      <- trimws(clean)
    json_match <- regmatches(clean, regexpr("\\{[^}]+\\}", clean))
    if (length(json_match) == 0) return(list(dose_response = NA, temporal = NA, incidence = NA))
    parsed <- fromJSON(json_match)
    
    list(
      dose_response = as.integer(parsed$dose_response),
      temporal      = as.integer(parsed$temporal),
      incidence     = as.integer(parsed$incidence)
    )
  }, error = function(e) {
    list(dose_response = NA, temporal = NA, incidence = NA)
  })
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&family=Syne:wght@400;600;800&display=swap",
      rel  = "stylesheet"
    ),
    tags$style(HTML("
      :root {
        --bg:       #0d0f14;
        --surface:  #151820;
        --border:   #252a35;
        --accent:   #00e5ff;
        --accent2:  #ff6b35;
        --accent3:  #7fff6b;
        --text:     #e8eaf0;
        --muted:    #6b7280;
        --radius:   6px;
      }

      * { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        background: var(--bg);
        color: var(--text);
        font-family: 'Syne', sans-serif;
        min-height: 100vh;
      }

      /* ── Header ── */
      .kera-header {
        border-bottom: 1px solid var(--border);
        padding: 28px 40px 20px;
        display: flex;
        align-items: baseline;
        gap: 18px;
        background: linear-gradient(180deg, #0a0c10 0%, var(--bg) 100%);
      }
      .kera-logo {
        font-family: 'Space Mono', monospace;
        font-size: 28px;
        font-weight: 700;
        letter-spacing: 6px;
        color: var(--accent);
        text-shadow: 0 0 24px rgba(0,229,255,0.35);
      }
      .kera-subtitle {
        font-size: 12px;
        color: var(--muted);
        letter-spacing: 2px;
        text-transform: uppercase;
        font-family: 'Space Mono', monospace;
      }

      /* ── Layout ── */
      .kera-body {
        display: grid;
        grid-template-columns: 300px 1fr;
        gap: 0;
        min-height: calc(100vh - 77px);
      }

      /* ── Sidebar ── */
      .kera-sidebar {
        border-right: 1px solid var(--border);
        padding: 32px 24px;
        background: var(--surface);
        display: flex;
        flex-direction: column;
        gap: 28px;
      }

      .section-label {
        font-size: 10px;
        letter-spacing: 3px;
        text-transform: uppercase;
        color: var(--muted);
        font-family: 'Space Mono', monospace;
        margin-bottom: 10px;
      }

      .kera-input-group { display: flex; flex-direction: column; gap: 6px; }

      .kera-file-btn {
        display: block;
        width: 100%;
        padding: 12px 16px;
        background: transparent;
        border: 1px dashed var(--border);
        border-radius: var(--radius);
        color: var(--text);
        font-family: 'Space Mono', monospace;
        font-size: 11px;
        cursor: pointer;
        transition: all 0.2s;
        text-align: center;
      }
      .kera-file-btn:hover { border-color: var(--accent); color: var(--accent); }

      /* override Shiny file input */
      .shiny-input-container { width: 100% !important; }
      input[type='file'] {
        background: transparent !important;
        border: 1px dashed var(--border) !important;
        border-radius: var(--radius) !important;
        color: var(--muted) !important;
        font-family: 'Space Mono', monospace !important;
        font-size: 11px !important;
        padding: 10px !important;
        width: 100% !important;
        cursor: pointer !important;
      }
      .form-control, input[type='text'], input[type='password'] {
        background: var(--bg) !important;
        border: 1px solid var(--border) !important;
        border-radius: var(--radius) !important;
        color: var(--text) !important;
        font-family: 'Space Mono', monospace !important;
        font-size: 12px !important;
        padding: 10px 12px !important;
        width: 100% !important;
        transition: border-color 0.2s !important;
      }
      .form-control:focus { border-color: var(--accent) !important; outline: none !important; box-shadow: 0 0 0 2px rgba(0,229,255,0.1) !important; }
      label { font-size: 11px !important; color: var(--muted) !important; font-family: 'Space Mono', monospace !important; letter-spacing: 1px !important; margin-bottom: 4px !important; }

      /* ── Run button ── */
      .run-btn {
        width: 100%;
        padding: 14px;
        background: var(--accent);
        color: #000;
        border: none;
        border-radius: var(--radius);
        font-family: 'Space Mono', monospace;
        font-size: 13px;
        font-weight: 700;
        letter-spacing: 2px;
        cursor: pointer;
        transition: all 0.2s;
        text-transform: uppercase;
      }
      .run-btn:hover { background: #33ecff; box-shadow: 0 0 20px rgba(0,229,255,0.4); }
      .run-btn:disabled { background: var(--border); color: var(--muted); cursor: not-allowed; box-shadow: none; }

      /* ── Download button ── */
      .dl-btn {
        width: 100%;
        padding: 12px;
        background: transparent;
        color: var(--accent3);
        border: 1px solid var(--accent3);
        border-radius: var(--radius);
        font-family: 'Space Mono', monospace;
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 2px;
        cursor: pointer;
        transition: all 0.2s;
        text-transform: uppercase;
        text-decoration: none;
        display: block;
        text-align: center;
      }
      .dl-btn:hover { background: rgba(127,255,107,0.08); box-shadow: 0 0 16px rgba(127,255,107,0.2); }

      /* ── Status bar ── */
      .status-bar {
        padding: 10px 14px;
        border-radius: var(--radius);
        font-family: 'Space Mono', monospace;
        font-size: 11px;
        line-height: 1.5;
      }
      .status-idle    { background: rgba(107,114,128,0.1); color: var(--muted); border: 1px solid var(--border); }
      .status-running { background: rgba(0,229,255,0.08); color: var(--accent); border: 1px solid rgba(0,229,255,0.3); }
      .status-done    { background: rgba(127,255,107,0.08); color: var(--accent3); border: 1px solid rgba(127,255,107,0.3); }
      .status-error   { background: rgba(255,107,53,0.08); color: var(--accent2); border: 1px solid rgba(255,107,53,0.3); }

      /* ── Progress bar ── */
      .progress-wrap {
        background: var(--border);
        border-radius: 3px;
        height: 4px;
        overflow: hidden;
      }
      .progress-fill {
        height: 4px;
        background: linear-gradient(90deg, var(--accent), var(--accent3));
        border-radius: 3px;
        transition: width 0.4s ease;
        box-shadow: 0 0 8px rgba(0,229,255,0.5);
      }

      /* ── Main panel ── */
      .kera-main {
        padding: 32px 36px;
        overflow-x: auto;
      }

      .results-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 24px;
      }
      .results-title {
        font-size: 18px;
        font-weight: 800;
        letter-spacing: 1px;
        color: var(--text);
      }
      .results-count {
        font-family: 'Space Mono', monospace;
        font-size: 11px;
        color: var(--muted);
        background: var(--surface);
        border: 1px solid var(--border);
        border-radius: 20px;
        padding: 4px 12px;
      }

      /* ── DataTable overrides ── */
      .dataTables_wrapper { color: var(--text) !important; }
      table.dataTable { background: var(--surface) !important; border-collapse: separate !important; border-spacing: 0 4px !important; }
      table.dataTable thead th {
        background: var(--bg) !important;
        color: var(--muted) !important;
        font-family: 'Space Mono', monospace !important;
        font-size: 10px !important;
        letter-spacing: 2px !important;
        text-transform: uppercase !important;
        border-bottom: 1px solid var(--border) !important;
        border-top: none !important;
        padding: 10px 14px !important;
        font-weight: 400 !important;
      }
      table.dataTable tbody td {
        background: var(--surface) !important;
        color: var(--text) !important;
        border: none !important;
        font-size: 13px !important;
        padding: 12px 14px !important;
        vertical-align: middle !important;
      }
      table.dataTable tbody tr:hover td { background: #1e2230 !important; }
      .dataTables_filter input { background: var(--bg) !important; border: 1px solid var(--border) !important; color: var(--text) !important; border-radius: var(--radius) !important; font-family: 'Space Mono', monospace !important; font-size: 11px !important; padding: 6px 10px !important; }
      .dataTables_length select { background: var(--bg) !important; border: 1px solid var(--border) !important; color: var(--text) !important; border-radius: var(--radius) !important; }
      .dataTables_info, .dataTables_filter label, .dataTables_length label { color: var(--muted) !important; font-family: 'Space Mono', monospace !important; font-size: 11px !important; }
      .paginate_button { color: var(--muted) !important; font-family: 'Space Mono', monospace !important; font-size: 11px !important; }
      .paginate_button.current { background: var(--accent) !important; color: #000 !important; border-radius: var(--radius) !important; border: none !important; }

      /* ── Probability badges ── */
      .prob-badge {
        display: inline-block;
        min-width: 46px;
        padding: 4px 10px;
        border-radius: 20px;
        font-family: 'Space Mono', monospace;
        font-size: 12px;
        font-weight: 700;
        text-align: center;
      }
      .prob-high   { background: rgba(127,255,107,0.15); color: #7fff6b; border: 1px solid rgba(127,255,107,0.3); }
      .prob-medium { background: rgba(255,220,80,0.12);  color: #ffdc50; border: 1px solid rgba(255,220,80,0.3); }
      .prob-low    { background: rgba(255,107,53,0.12);  color: #ff6b35; border: 1px solid rgba(255,107,53,0.3); }
      .prob-na     { background: rgba(107,114,128,0.12); color: #6b7280; border: 1px solid rgba(107,114,128,0.3); }

      /* ── Empty state ── */
      .empty-state {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        height: 400px;
        gap: 16px;
        color: var(--muted);
      }
      .empty-icon { font-size: 48px; opacity: 0.3; }
      .empty-text { font-family: 'Space Mono', monospace; font-size: 12px; letter-spacing: 2px; text-transform: uppercase; }

      /* ── Spinner ── */
      @keyframes spin { to { transform: rotate(360deg); } }
      .spinner {
        display: inline-block;
        width: 10px; height: 10px;
        border: 2px solid transparent;
        border-top-color: var(--accent);
        border-radius: 50%;
        animation: spin 0.8s linear infinite;
        margin-right: 6px;
        vertical-align: middle;
      }

      /* ── Shiny progress bar override ── */
      #shiny-notification-panel {
        bottom: 20px !important;
        top: auto !important;
        right: 20px !important;
        width: 320px !important;
      }
      .shiny-notification {
        background: #151820 !important;
        border: 1px solid rgba(0,229,255,0.3) !important;
        border-radius: 6px !important;
        color: #e8eaf0 !important;
        font-family: 'Space Mono', monospace !important;
        font-size: 11px !important;
        padding: 16px 18px !important;
        box-shadow: 0 4px 24px rgba(0,0,0,0.5) !important;
      }
      .shiny-notification-message {
        color: #00e5ff !important;
        font-weight: 700 !important;
        font-size: 12px !important;
        letter-spacing: 1px !important;
      }
      .shiny-notification-detail {
        color: #6b7280 !important;
        font-size: 10px !important;
        margin-top: 4px !important;
      }
      .shiny-progress-bar {
        background: linear-gradient(90deg, #00e5ff, #7fff6b) !important;
        height: 3px !important;
        box-shadow: 0 0 8px rgba(0,229,255,0.6) !important;
      }
      .progress {
        background: #252a35 !important;
        height: 3px !important;
        border-radius: 3px !important;
        margin-top: 10px !important;
      }
      /* scrollbar */
      ::-webkit-scrollbar { width: 6px; height: 6px; }
      ::-webkit-scrollbar-track { background: var(--bg); }
      ::-webkit-scrollbar-thumb { background: var(--border); border-radius: 3px; }
    "))
  ),
  
  # Header
  div(class = "kera-header",
      div(class = "kera-logo", "KERA"),
      div(class = "kera-subtitle", "Key Event Relationship Analyser")
  ),
  
  # Body
  div(class = "kera-body",
      
      # ── Sidebar ──
      div(class = "kera-sidebar",
          
          div(
            div(class = "section-label", "01 — Upload"),
            fileInput("file", NULL,
                      accept  = c(".xlsx", ".xls"),
                      placeholder = "Drop Excel file here"
            )
          ),
          
          div(
            div(class = "section-label", "02 — API Key"),
            passwordInput("api_key", NULL,
                          value       = Sys.getenv("ANTHROPIC_API_KEY"),
                          placeholder = "sk-ant-..."
            ),
            tags$small(style = "color: #4b5563; font-family: 'Space Mono', monospace; font-size: 10px;",
                       "Auto-loaded from .Renviron"
            )
          ),
          
          div(
            div(class = "section-label", "03 — Column"),
            textInput("abstract_col", NULL,
                      value       = "Abstract",
                      placeholder = "Column name"
            )
          ),
          
          actionButton("run_btn", "▶  Run Evaluation",
                       class = "run-btn"
          ),
          
          uiOutput("status_ui"),
          uiOutput("progress_ui"),
          uiOutput("download_ui")
      ),
      
      # ── Main panel ──
      div(class = "kera-main",
          uiOutput("results_header_ui"),
          uiOutput("table_ui")
      )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    results  = NULL,
    status   = "idle",
    message  = "Upload an Excel file and click Run",
    progress = 0,
    total    = 0
  )
  
  # Status UI
  output$status_ui <- renderUI({
    cls <- switch(rv$status,
                  idle    = "status-bar status-idle",
                  running = "status-bar status-running",
                  done    = "status-bar status-done",
                  error   = "status-bar status-error"
    )
    spinner <- if (rv$status == "running") tags$span(class = "spinner") else NULL
    div(class = cls, spinner, rv$message)
  })
  
  # Progress bar
  output$progress_ui <- renderUI({
    if (rv$status != "running") return(NULL)
    pct <- if (rv$total > 0) round(rv$progress / rv$total * 100) else 0
    div(
      div(class = "progress-wrap",
          div(class = "progress-fill", style = paste0("width:", pct, "%"))
      ),
      tags$small(style = "color: var(--muted); font-family: 'Space Mono', monospace; font-size: 10px; margin-top: 4px; display: block;",
                 paste0(rv$progress, " / ", rv$total, " abstracts")
      )
    )
  })
  
  # Download button
  output$download_ui <- renderUI({
    if (is.null(rv$results)) return(NULL)
    downloadButton("download_btn", "↓  Download Results",
                   class = "dl-btn"
    )
  })
  
  # Results header
  output$results_header_ui <- renderUI({
    if (is.null(rv$results)) return(NULL)
    n <- nrow(rv$results)
    div(class = "results-header",
        div(class = "results-title", "Evaluation Results"),
        div(class = "results-count", paste0(n, " abstracts"))
    )
  })
  
  # Empty state / table
  output$table_ui <- renderUI({
    if (is.null(rv$results)) {
      return(div(class = "empty-state",
                 div(class = "empty-icon", "◈"),
                 div(class = "empty-text", "Awaiting evaluation")
      ))
    }
    DTOutput("results_table")
  })
  
  # Run evaluation
  observeEvent(input$run_btn, {
    
    # Validate inputs manually instead of req() so we can show error messages
    if (is.null(input$file)) {
      rv$status  <- "error"
      rv$message <- "Please upload an Excel file first."
      return()
    }
    if (nchar(trimws(input$api_key)) == 0) {
      rv$status  <- "error"
      rv$message <- "API key is missing. Check your .Renviron file."
      return()
    }
    
    rv$status   <- "running"
    rv$message  <- "Reading file..."
    rv$results  <- NULL
    rv$progress <- 0
    
    tryCatch({
      df <- read_excel(input$file$datapath)
      
      col <- trimws(input$abstract_col)
      if (!col %in% names(df)) {
        rv$status  <- "error"
        rv$message <- paste0("Column '", col, "' not found. Available columns: ", paste(names(df), collapse = ", "))
        return()
      }
      
      abstracts <- df[[col]]
      n         <- nrow(df)
      rv$total  <- n
      rv$message <- paste0("Starting evaluation of ", n, " abstracts...")
      
      dose_resp <- rep(NA_integer_, n)
      temporal  <- rep(NA_integer_, n)
      incidence <- rep(NA_integer_, n)
      
      withProgress(message = "Evaluating abstracts...", value = 0, {
        for (i in seq_len(n)) {
          rv$message  <- paste0("Evaluating abstract ", i, " of ", n, "...")
          incProgress(1/n, detail = paste0("Abstract ", i, " of ", n))
          result      <- evaluate_abstract(as.character(abstracts[i]), trimws(input$api_key))
          dose_resp[i] <- result$dose_response
          temporal[i]  <- result$temporal
          incidence[i] <- result$incidence
          rv$progress  <- i
          Sys.sleep(0.3)
        }
      })
      
      out_df <- df
      out_df[["Dose-Response Probability (%)"]] <- dose_resp
      out_df[["Temporal Probability (%)"]]      <- temporal
      out_df[["Incidence Probability (%)"]]     <- incidence
      
      rv$results <- out_df
      rv$status  <- "done"
      rv$message <- paste0("✓ Done — ", n, " abstracts evaluated")
      
    }, error = function(e) {
      rv$status  <- "error"
      rv$message <- paste0("Error: ", conditionMessage(e))
    })
  })
  
  # Render table
  output$results_table <- renderDT({
    req(rv$results)
    df <- rv$results
    
    # Format probability columns with badges
    prob_cols <- c("Dose-Response Probability (%)", "Temporal Probability (%)", "Incidence Probability (%)")
    for (col in prob_cols) {
      if (col %in% names(df)) {
        df[[col]] <- sapply(df[[col]], function(v) {
          if (is.na(v)) return('<span class="prob-badge prob-na">N/A</span>')
          cls <- if (v >= 70) "prob-high" else if (v >= 45) "prob-medium" else "prob-low"
          paste0('<span class="prob-badge ', cls, '">', v, '%</span>')
        })
      }
    }
    
    # Truncate abstract for display
    if ("Abstract" %in% names(df)) {
      df[["Abstract"]] <- substr(df[["Abstract"]], 1, 180)
      df[["Abstract"]] <- ifelse(
        nchar(rv$results[["Abstract"]]) > 180,
        paste0(df[["Abstract"]], "…"),
        df[["Abstract"]]
      )
    }
    
    datatable(df,
              escape    = FALSE,
              rownames  = FALSE,
              selection = "none",
              options   = list(
                pageLength  = 10,
                scrollX     = TRUE,
                dom         = 'frtip',
                columnDefs  = list(
                  list(width = "300px", targets = which(names(df) == "Abstract") - 1),
                  list(className = "dt-center", targets = which(names(df) %in% prob_cols) - 1)
                )
              )
    )
  }, server = FALSE)
  
  # Download handler
  output$download_btn <- downloadHandler(
    filename = function() paste0("KERA_evaluated_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content  = function(file) write_xlsx(rv$results, file)
  )
}

shinyApp(ui, server)
