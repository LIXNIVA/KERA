library(shiny)
library(httr)
library(jsonlite)
library(readxl)
library(writexl)
library(DT)

options(shiny.maxRequestSize = 50 * 1024^2)

# ── Claude evaluation function ─────────────────────────────────────────────────
evaluate_abstract <- function(abstract_text, api_key) {
  url <- "https://api.anthropic.com/v1/messages"
  
  prompt <- paste0(
    "You are a scientific literature analyst specializing in toxicology and epidemiology. ",
    "Evaluate the following abstract and estimate the probability (0-100) that the FULL TEXT paper ",
    "will provide empirical evidence for each of the three types of concordance:\n\n",
    "1. Dose-Response Concordance: Evidence that greater exposure leads to greater effect (clear dose gradient).\n",
    "2. Temporal Concordance: Evidence that exposure precedes the outcome in time (time-course or sequential data).\n",
    "3. Incidence Concordance: Evidence that exposure rates align with disease/outcome rates across populations or groups.\n\n",
    "Return ONLY a valid JSON object with exactly these four keys. ",
    "dose_response, temporal, incidence must be integers 0-100. ",
    "note must be a single sentence (max 20 words) briefly explaining the key reasoning. ",
    "No markdown, no code fences. Example: ",
    "{\"dose_response\": 75, \"temporal\": 60, \"incidence\": 30, \"note\": \"Wide dose range tested with significant DNA adduct increase; short 2-3 day exposure window noted.\"}\n\n",
    "Abstract:\n", abstract_text
  )
  
  payload <- list(
    model      = "claude-haiku-4-5-20251001",
    max_tokens = 200,
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
    if (is.null(raw_text)) return(list(dose_response = NA, temporal = NA, incidence = NA, note = NA))
    
    clean      <- gsub("```[a-zA-Z]*|```", "", raw_text)
    clean      <- trimws(clean)
    # Extract JSON - handle note field with text
    json_match <- regmatches(clean, regexpr("\\{.*\\}", clean, perl = TRUE))
    if (length(json_match) == 0) return(list(dose_response = NA, temporal = NA, incidence = NA, note = NA))
    parsed <- fromJSON(json_match)
    
    list(
      dose_response = as.integer(parsed$dose_response),
      temporal      = as.integer(parsed$temporal),
      incidence     = as.integer(parsed$incidence),
      note          = as.character(parsed$note)
    )
  }, error = function(e) {
    list(dose_response = NA, temporal = NA, incidence = NA, note = NA)
  })
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&family=Inter:wght@400;500;600;700&display=swap",
      rel  = "stylesheet"
    ),
    tags$style(HTML("
      :root {
        --bg:        #ffffff;
        --surface:   #f8f9fc;
        --surface2:  #f1f3f8;
        --border:    #e2e6ef;
        --accent:    #2563eb;
        --accent-lt: #eff6ff;
        --accent2:   #dc2626;
        --accent3:   #16a34a;
        --text:      #111827;
        --muted:     #6b7280;
        --radius:    8px;
        --shadow:    0 1px 4px rgba(0,0,0,0.08);
      }

      * { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        background: var(--bg);
        color: var(--text);
        font-family: 'Inter', sans-serif;
        min-height: 100vh;
      }

      /* ── Header ── */
      .kera-header {
        border-bottom: 1px solid var(--border);
        padding: 20px 36px;
        display: flex;
        align-items: center;
        gap: 16px;
        background: #fff;
        box-shadow: var(--shadow);
      }
      .kera-logo {
        font-family: 'Space Mono', monospace;
        font-size: 22px;
        font-weight: 700;
        letter-spacing: 5px;
        color: var(--accent);
      }
      .kera-subtitle {
        font-size: 12px;
        color: var(--muted);
        letter-spacing: 1.5px;
        text-transform: uppercase;
        font-family: 'Space Mono', monospace;
        border-left: 2px solid var(--border);
        padding-left: 16px;
      }

      /* ── Layout ── */
      .kera-body {
        display: grid;
        grid-template-columns: 280px 1fr;
        min-height: calc(100vh - 65px);
      }

      /* ── Sidebar ── */
      .kera-sidebar {
        border-right: 1px solid var(--border);
        padding: 28px 20px;
        background: var(--surface);
        display: flex;
        flex-direction: column;
        gap: 24px;
      }

      .section-label {
        font-size: 10px;
        letter-spacing: 2px;
        text-transform: uppercase;
        color: var(--muted);
        font-family: 'Space Mono', monospace;
        margin-bottom: 8px;
        font-weight: 700;
      }

      .shiny-input-container { width: 100% !important; }

      input[type='file'] {
        background: #fff !important;
        border: 1.5px dashed var(--border) !important;
        border-radius: var(--radius) !important;
        color: var(--muted) !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 12px !important;
        padding: 10px !important;
        width: 100% !important;
        cursor: pointer !important;
        transition: border-color 0.2s !important;
      }
      input[type='file']:hover { border-color: var(--accent) !important; }

      .form-control, input[type='text'], input[type='password'] {
        background: #fff !important;
        border: 1.5px solid var(--border) !important;
        border-radius: var(--radius) !important;
        color: var(--text) !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 13px !important;
        padding: 9px 12px !important;
        width: 100% !important;
        transition: border-color 0.2s, box-shadow 0.2s !important;
      }
      .form-control:focus {
        border-color: var(--accent) !important;
        outline: none !important;
        box-shadow: 0 0 0 3px rgba(37,99,235,0.1) !important;
      }
      label {
        font-size: 11px !important;
        color: var(--muted) !important;
        font-family: 'Inter', sans-serif !important;
        letter-spacing: 0.5px !important;
        margin-bottom: 5px !important;
        font-weight: 500 !important;
      }

      /* ── Run button ── */
      .run-btn {
        width: 100%;
        padding: 12px;
        background: var(--accent);
        color: #fff;
        border: none;
        border-radius: var(--radius);
        font-family: 'Space Mono', monospace;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 1.5px;
        cursor: pointer;
        transition: all 0.2s;
        text-transform: uppercase;
        box-shadow: 0 2px 8px rgba(37,99,235,0.25);
      }
      .run-btn:hover { background: #1d4ed8; box-shadow: 0 4px 16px rgba(37,99,235,0.35); }
      .run-btn:disabled { background: var(--border); color: var(--muted); cursor: not-allowed; box-shadow: none; }

      /* ── Download buttons ── */
      .dl-btn, .dl-pmid-btn {
        width: 100%;
        padding: 10px;
        border-radius: var(--radius);
        font-family: 'Space Mono', monospace;
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 1px;
        cursor: pointer;
        transition: all 0.2s;
        text-transform: uppercase;
        text-decoration: none;
        display: block;
        text-align: center;
        margin-top: 6px;
      }
      .dl-btn {
        background: transparent;
        color: var(--accent3);
        border: 1.5px solid var(--accent3);
      }
      .dl-btn:hover { background: #f0fdf4; }

      .dl-pmid-btn {
        background: transparent;
        color: var(--accent);
        border: 1.5px solid var(--accent);
      }
      .dl-pmid-btn:hover { background: var(--accent-lt); }

      /* ── Status bar ── */
      .status-bar {
        padding: 10px 13px;
        border-radius: var(--radius);
        font-family: 'Inter', sans-serif;
        font-size: 12px;
        line-height: 1.5;
        font-weight: 500;
      }
      .status-idle    { background: var(--surface2); color: var(--muted); border: 1px solid var(--border); }
      .status-running { background: #eff6ff; color: var(--accent); border: 1px solid #bfdbfe; }
      .status-done    { background: #f0fdf4; color: var(--accent3); border: 1px solid #bbf7d0; }
      .status-error   { background: #fef2f2; color: var(--accent2); border: 1px solid #fecaca; }

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

      /* ── Shiny progress notification ── */
      #shiny-notification-panel {
        bottom: 20px !important;
        top: auto !important;
        right: 20px !important;
        width: 300px !important;
      }
      .shiny-notification {
        background: #fff !important;
        border: 1px solid var(--border) !important;
        border-left: 4px solid var(--accent) !important;
        border-radius: var(--radius) !important;
        color: var(--text) !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 12px !important;
        padding: 14px 16px !important;
        box-shadow: 0 4px 20px rgba(0,0,0,0.12) !important;
      }
      .shiny-notification-message {
        color: var(--accent) !important;
        font-weight: 600 !important;
        font-size: 13px !important;
      }
      .shiny-notification-detail { color: var(--muted) !important; font-size: 11px !important; margin-top: 3px !important; }
      .shiny-progress-bar { background: var(--accent) !important; height: 3px !important; }
      .progress { background: var(--border) !important; height: 3px !important; border-radius: 3px !important; margin-top: 8px !important; }

      /* ── Main panel ── */
      .kera-main { padding: 28px 32px; overflow-x: auto; background: #fff; }

      .results-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 20px;
        padding-bottom: 16px;
        border-bottom: 1px solid var(--border);
      }
      .results-title { font-size: 17px; font-weight: 700; color: var(--text); }
      .results-count {
        font-family: 'Space Mono', monospace;
        font-size: 11px;
        color: var(--muted);
        background: var(--surface);
        border: 1px solid var(--border);
        border-radius: 20px;
        padding: 4px 12px;
      }

      /* ── DataTable ── */
      .dataTables_wrapper { color: var(--text) !important; font-family: 'Inter', sans-serif !important; }
      table.dataTable {
        background: #fff !important;
        border-collapse: collapse !important;
        width: 100% !important;
      }
      table.dataTable thead th {
        background: var(--surface) !important;
        color: var(--muted) !important;
        font-family: 'Space Mono', monospace !important;
        font-size: 10px !important;
        letter-spacing: 1.5px !important;
        text-transform: uppercase !important;
        border-bottom: 2px solid var(--border) !important;
        border-top: 1px solid var(--border) !important;
        padding: 10px 14px !important;
        font-weight: 700 !important;
      }
      table.dataTable tbody td {
        background: #fff !important;
        color: var(--text) !important;
        border-bottom: 1px solid var(--border) !important;
        font-size: 13px !important;
        padding: 11px 14px !important;
        vertical-align: middle !important;
      }
      table.dataTable tbody tr:hover td { background: var(--accent-lt) !important; }
      table.dataTable tbody tr.selected td { background: #eff6ff !important; border-left: 3px solid var(--accent) !important; }
      .dataTables_filter input {
        background: #fff !important;
        border: 1.5px solid var(--border) !important;
        color: var(--text) !important;
        border-radius: var(--radius) !important;
        font-size: 12px !important;
        padding: 6px 10px !important;
      }
      .dataTables_length select {
        background: #fff !important;
        border: 1.5px solid var(--border) !important;
        color: var(--text) !important;
        border-radius: var(--radius) !important;
        padding: 4px 8px !important;
      }
      .dataTables_info, .dataTables_filter label, .dataTables_length label {
        color: var(--muted) !important;
        font-size: 12px !important;
      }
      .paginate_button { color: var(--muted) !important; font-size: 12px !important; border-radius: 4px !important; }
      .paginate_button.current {
        background: var(--accent) !important;
        color: #fff !important;
        border-radius: 4px !important;
        border: none !important;
      }
      .paginate_button:hover { background: var(--accent-lt) !important; color: var(--accent) !important; }

      /* ── Probability badges ── */
      .prob-badge {
        display: inline-block;
        min-width: 48px;
        padding: 3px 10px;
        border-radius: 20px;
        font-family: 'Space Mono', monospace;
        font-size: 12px;
        font-weight: 700;
        text-align: center;
      }
      .prob-high   { background: #dcfce7; color: #15803d; border: 1px solid #bbf7d0; }
      .prob-medium { background: #fef9c3; color: #a16207; border: 1px solid #fde68a; }
      .prob-low    { background: #fee2e2; color: #b91c1c; border: 1px solid #fecaca; }
      .prob-na     { background: #f3f4f6; color: #9ca3af; border: 1px solid #e5e7eb; }

      /* ── Selection info bar ── */
      .selection-bar {
        display: flex;
        align-items: center;
        gap: 12px;
        padding: 10px 14px;
        background: var(--accent-lt);
        border: 1px solid #bfdbfe;
        border-radius: var(--radius);
        margin-bottom: 14px;
        font-size: 12px;
        color: var(--accent);
        font-weight: 500;
      }

      /* ── Empty state ── */
      .empty-state {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        height: 380px;
        gap: 12px;
        color: var(--muted);
      }
      .empty-icon { font-size: 40px; opacity: 0.25; }
      .empty-text { font-family: 'Space Mono', monospace; font-size: 11px; letter-spacing: 2px; text-transform: uppercase; }

      /* scrollbar */
      ::-webkit-scrollbar { width: 6px; height: 6px; }
      ::-webkit-scrollbar-track { background: var(--surface); }
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
                      accept      = c(".xlsx", ".xls"),
                      placeholder = "Choose Excel file"
            )
          ),
          
          div(
            div(class = "section-label", "02 — API Key"),
            passwordInput("api_key", NULL,
                          value       = Sys.getenv("ANTHROPIC_API_KEY"),
                          placeholder = "sk-ant-..."
            ),
            tags$small(style = "color: #9ca3af; font-size: 10px;", "Auto-loaded from .Renviron")
          ),
          
          div(
            div(class = "section-label", "03 — Abstract Column"),
            textInput("abstract_col", NULL,
                      value       = "Abstract",
                      placeholder = "Column name"
            )
          ),
          
          div(
            div(class = "section-label", "04 — Min Probability Filter"),
            sliderInput("min_prob", NULL,
                        min = 0, max = 100, value = 50, step = 5,
                        post = "%"
            )
          ),
          
          actionButton("run_btn", "▶  Run Evaluation", class = "run-btn"),
          
          uiOutput("status_ui"),
          uiOutput("download_ui")
      ),
      
      # ── Main panel ──
      div(class = "kera-main",
          uiOutput("results_header_ui"),
          uiOutput("selection_bar_ui"),
          uiOutput("table_ui")
      )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    results = NULL,
    status  = "idle",
    message = "Upload an Excel file and click Run"
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
  
  # Download buttons
  output$download_ui <- renderUI({
    if (is.null(rv$results)) return(NULL)
    tagList(
      downloadButton("download_btn",  "↓  Download Full Results", class = "dl-btn"),
      downloadButton("download_pmid", "↓  Download Selected PMIDs", class = "dl-pmid-btn")
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
  
  # Selection info bar
  output$selection_bar_ui <- renderUI({
    if (is.null(rv$results)) return(NULL)
    sel <- input$results_table_rows_selected
    n_sel <- length(sel)
    if (n_sel == 0) {
      div(class = "selection-bar",
          tags$span("☑"),
          tags$span("Click rows to select abstracts for PMID export. Use the filter slider to highlight high-probability papers.")
      )
    } else {
      div(class = "selection-bar",
          tags$span("✓"),
          tags$strong(paste0(n_sel, " abstract(s) selected")),
          tags$span("— click '↓ Download Selected PMIDs' to export")
      )
    }
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
    
    rv$status  <- "running"
    rv$message <- "Reading file..."
    rv$results <- NULL
    
    tryCatch({
      df  <- read_excel(input$file$datapath)
      col <- trimws(input$abstract_col)
      
      if (!col %in% names(df)) {
        rv$status  <- "error"
        rv$message <- paste0("Column '", col, "' not found. Available: ", paste(names(df), collapse = ", "))
        return()
      }
      
      abstracts <- df[[col]]
      n         <- nrow(df)
      
      dose_resp <- rep(NA_integer_, n)
      temporal  <- rep(NA_integer_, n)
      incidence <- rep(NA_integer_, n)
      notes     <- rep(NA_character_, n)
      
      withProgress(message = "Evaluating abstracts...", value = 0, {
        for (i in seq_len(n)) {
          rv$message <- paste0("Evaluating ", i, " of ", n, "...")
          incProgress(1/n, detail = paste0("Abstract ", i, " of ", n))
          result       <- evaluate_abstract(as.character(abstracts[i]), trimws(input$api_key))
          dose_resp[i] <- result$dose_response
          temporal[i]  <- result$temporal
          incidence[i] <- result$incidence
          notes[i]     <- result$note
          Sys.sleep(0.3)
        }
      })
      
      out_df <- df
      out_df[["Dose-Response (%)"]] <- dose_resp
      out_df[["Temporal (%)"]]      <- temporal
      out_df[["Incidence (%)"]]     <- incidence
      out_df[["Note"]]              <- notes
      
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
    df       <- rv$results
    min_prob <- input$min_prob
    
    prob_cols <- c("Dose-Response (%)", "Temporal (%)", "Incidence (%)")
    
    # Color-code probability cells
    for (col in prob_cols) {
      if (col %in% names(df)) {
        df[[col]] <- sapply(df[[col]], function(v) {
          if (is.na(v)) return('<span class="prob-badge prob-na">N/A</span>')
          cls <- if (v >= 70) "prob-high" else if (v >= 45) "prob-medium" else "prob-low"
          paste0('<span class="prob-badge ', cls, '">', v, '%</span>')
        })
      }
    }
    
    # Truncate abstract
    abs_col <- trimws(input$abstract_col)
    if (abs_col %in% names(df)) {
      orig_len <- nchar(as.character(rv$results[[abs_col]]))
      df[[abs_col]] <- substr(as.character(rv$results[[abs_col]]), 1, 160)
      df[[abs_col]] <- ifelse(orig_len > 160, paste0(df[[abs_col]], "…"), df[[abs_col]])
    }
    
    # Highlight rows above min_prob threshold
    raw      <- rv$results
    prob_raw <- pmax(
      ifelse(is.na(raw[["Dose-Response (%)"]]), 0L, raw[["Dose-Response (%)"]]),
      ifelse(is.na(raw[["Temporal (%)"]]),      0L, raw[["Temporal (%)"]]),
      ifelse(is.na(raw[["Incidence (%)"]]),     0L, raw[["Incidence (%)"]]))
    highlight_rows <- which(prob_raw >= min_prob) - 1  # 0-indexed for JS
    
    datatable(df,
              escape    = FALSE,
              rownames  = FALSE,
              selection = list(mode = "multiple", selected = highlight_rows + 1),
              options   = list(
                pageLength = 10,
                scrollX    = TRUE,
                dom        = 'frtip',
                columnDefs = list(
                  list(width = "260px", targets = which(names(df) == abs_col) - 1),
                  list(width = "200px", targets = which(names(df) == "Note") - 1),
                  list(className = "dt-center", targets = which(names(df) %in% prob_cols) - 1)
                ),
                rowCallback = JS(paste0(
                  "function(row, data, index) {",
                  "  var highlighted = ", jsonlite::toJSON(highlight_rows), ";",
                  "  if (highlighted.indexOf(index) > -1) {",
                  "    $(row).css('font-weight', '500');",
                  "  }",
                  "}"
                ))
              )
    )
  }, server = FALSE)
  
  # Download full results
  output$download_btn <- downloadHandler(
    filename = function() paste0("KERA_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content  = function(file) write_xlsx(rv$results, file)
  )
  
  # Download selected PMIDs
  output$download_pmid <- downloadHandler(
    filename = function() paste0("KERA_selected_PMIDs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"),
    content  = function(file) {
      sel <- input$results_table_rows_selected
      df  <- rv$results
      
      if (length(sel) == 0) {
        # If nothing selected, export all PMIDs above threshold
        min_prob <- input$min_prob
        raw      <- rv$results
        prob_raw <- pmax(
          ifelse(is.na(raw[["Dose-Response (%)"]]), 0L, raw[["Dose-Response (%)"]]),
          ifelse(is.na(raw[["Temporal (%)"]]),      0L, raw[["Temporal (%)"]]),
          ifelse(is.na(raw[["Incidence (%)"]]),     0L, raw[["Incidence (%)"]]))
        sel <- which(prob_raw >= min_prob)
      }
      
      pmids <- if ("PMID" %in% names(df)) {
        as.character(df$PMID[sel])
      } else {
        paste0("Row_", sel)
      }
      
      # Format as PubMed search string
      pubmed_query <- paste(pmids, collapse = "[uid] OR ")
      pubmed_query <- paste0(pubmed_query, "[uid]")
      
      writeLines(c(
        "# KERA Selected PMIDs",
        paste0("# Generated: ", Sys.time()),
        paste0("# Count: ", length(pmids)),
        "",
        "## Individual PMIDs:",
        pmids,
        "",
        "## PubMed Search String (paste into PubMed):",
        pubmed_query
      ), file)
    }
  )
}

shinyApp(ui, server)
