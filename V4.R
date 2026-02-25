library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(writexl)
library(plotly)

options(shiny.maxRequestSize = 200 * 1024^2)

app_css <- "
  body { background-color: #f5f7fa; font-family: 'Segoe UI', sans-serif; }
  .navbar { background-color: #2c3e50 !important; border: none; }
  .navbar-default .navbar-brand,
  .navbar-default .navbar-nav > li > a { color: white !important; }
  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:hover {
    background-color: #3498db !important; color: white !important;
  }
  .navbar-default .navbar-nav > li > a:hover { background-color: #34495e !important; }
  .page-wrap  { padding: 25px 15px; }
  .summary-box { background: white; border-radius: 8px; padding: 20px;
                 box-shadow: 0 2px 8px rgba(0,0,0,0.08); margin-bottom: 20px; }
  .upload-area { border: 2px dashed #3498db; border-radius: 8px; padding: 30px;
                 text-align: center; background: white; margin-bottom: 20px; }
  .event-tag { display: inline-block; background: #eaf4fb; color: #2980b9;
               border: 1px solid #aed6f1; border-radius: 15px;
               padding: 4px 12px; margin: 4px; font-size: 13px; }
  .section-header { border-left: 4px solid #3498db; padding-left: 12px; margin-bottom: 15px; }
  .ke-box-upstream   { border-top: 4px solid #e67e22; }
  .ke-box-downstream { border-top: 4px solid #27ae60; }
  .ke-box-taxa       { border-top: 4px solid #2980b9; }
  .overlap-box       { border-top: 4px solid #8e44ad; }
  .viz-box           { border-top: 4px solid #e74c3c; }
  .stat-number { font-size: 2em; font-weight: bold; margin: 0; }
  .stat-label  { color: #7f8c8d; margin: 0; font-size: 0.9em; }
  .arrow-center { text-align: center; font-size: 1.8em; padding-top: 38px; color: #95a5a6; }
  .result-badge { display: inline-block; background: #8e44ad; color: white;
                  border-radius: 20px; padding: 6px 18px; font-size: 1.1em;
                  font-weight: bold; margin-bottom: 15px; }
  .page-notice { background: #fef9e7; border: 1px solid #f9ca74; border-radius: 8px;
                 padding: 18px; text-align: center; color: #7f6000; }
  .chart-ctrl  { background: #f8f9fa; border-radius: 8px; padding: 15px; margin-bottom: 15px; }
"

# ─────────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title  = "📊 Key Event Relationship Analyzer",
  id     = "navbar",
  header = tags$head(tags$style(HTML(app_css))),
  
  # ══════════════════════════════════════════════════════════════════
  # PAGE 1 — Upload & Summary
  # ══════════════════════════════════════════════════════════════════
  tabPanel("📁 Upload & Summary",
           div(class = "page-wrap",
               div(class = "upload-area",
                   fileInput("file", label = NULL,
                             accept = c(".xlsx", ".xls"),
                             buttonLabel = "📂 Choose Excel File",
                             placeholder = "No file selected — drag & drop or click to browse")
               ),
               uiOutput("summary_ui")
           )
  ),
  
  # ══════════════════════════════════════════════════════════════════
  # PAGE 2 — Key Event Analysis
  # ══════════════════════════════════════════════════════════════════
  tabPanel("🔬 Key Event Analysis",
           div(class = "page-wrap",
               uiOutput("page2_notice"),
               uiOutput("ke_selector_ui"),
               uiOutput("overlap_ui")
           )
  ),
  
  # ══════════════════════════════════════════════════════════════════
  # PAGE 3 — Visualisation
  # ══════════════════════════════════════════════════════════════════
  tabPanel("📈 Visualisation",
           div(class = "page-wrap",
               uiOutput("page3_notice"),
               uiOutput("viz_ui")
           )
  )
)

# ─────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Load & normalise ───────────────────────────────────────────────────────
  data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath)
    if ("date" %in% colnames(df) && !"pubdate" %in% colnames(df))
      df <- df %>% rename(pubdate = date)
    df
  })
  
  unique_events <- reactive({
    req(data())
    sort(unique(trimws(na.omit(c(data()$event_1, data()$event_2)))))
  })
  
  event_counts <- reactive({
    req(data())
    all_ev <- trimws(na.omit(c(data()$event_1, data()$event_2)))
    tbl <- as.data.frame(table(all_ev), stringsAsFactors = FALSE)
    colnames(tbl) <- c("Event Name", "Count")
    tbl[order(-tbl$Count), ]
  })
  
  # ── PAGE 1 : Summary ──────────────────────────────────────────────────────
  output$summary_ui <- renderUI({
    req(event_counts())
    df <- data(); ev <- event_counts()
    tagList(
      fluidRow(
        column(4, div(class = "summary-box", style = "text-align:center;",
                      p(class = "stat-number", style = "color:#2c3e50;", nrow(df)),
                      p(class = "stat-label", "Total Records"))),
        column(4, div(class = "summary-box", style = "text-align:center;",
                      p(class = "stat-number", style = "color:#3498db;", nrow(ev)),
                      p(class = "stat-label", "Unique Events"))),
        column(4, div(class = "summary-box", style = "text-align:center;",
                      p(class = "stat-number", style = "color:#27ae60;", length(unique(df$PMID))),
                      p(class = "stat-label", "Unique PMIDs")))
      ),
      div(class = "summary-box",
          h4(class = "section-header", "🏷️ All Unique Events"),
          div(lapply(ev$`Event Name`, function(e) tags$span(class = "event-tag", e)))
      ),
      div(class = "summary-box",
          h4(class = "section-header", "📋 Event Frequency Table"),
          DTOutput("event_table")
      ),
      div(class = "summary-box",
          h4(class = "section-header", "📄 Raw Data Preview"),
          DTOutput("raw_table")
      )
    )
  })
  
  output$event_table <- renderDT({
    req(event_counts())
    datatable(event_counts(), options = list(pageLength = 10, dom = 'ftp'), rownames = FALSE) %>%
      formatStyle("Count",
                  background = styleColorBar(event_counts()$Count, "#aed6f1"),
                  backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })
  
  output$raw_table <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 5, dom = 'ftp', scrollX = TRUE), rownames = FALSE)
  })
  
  # ── PAGE 2 : Notice ───────────────────────────────────────────────────────
  output$page2_notice <- renderUI({
    if (is.null(input$file))
      div(class = "page-notice",
          tags$b("⚠️ No file loaded yet."), tags$br(),
          "Please go to the ", tags$b("Upload & Summary"), " tab first.")
  })
  
  # ── PAGE 2 : Selector ─────────────────────────────────────────────────────
  output$ke_selector_ui <- renderUI({
    req(unique_events())
    evs <- unique_events()
    div(class = "summary-box",
        h3(class = "section-header", "🔬 Key Event & Taxa Overlap Analysis"),
        p("Select events for each role and an optional taxa/species, then click Search."),
        fluidRow(
          column(3,
                 div(class = "summary-box ke-box-upstream",
                     h4("⬆️ Upstream Key Event(s)"),
                     selectInput("upstream", NULL, choices = evs, multiple = TRUE, selectize = TRUE),
                     tags$small("Select upstream event(s)", style = "color:#7f8c8d;")
                 )
          ),
          column(1, div(class = "arrow-center", "→")),
          column(3,
                 div(class = "summary-box ke-box-downstream",
                     h4("⬇️ Downstream Key Event(s)"),
                     selectInput("downstream", NULL, choices = evs, multiple = TRUE, selectize = TRUE),
                     tags$small("Select downstream event(s)", style = "color:#7f8c8d;")
                 )
          ),
          column(1, div(class = "arrow-center", "🔗")),
          column(4,
                 div(class = "summary-box ke-box-taxa",
                     h4("🧬 Taxa / Species"),
                     selectInput("taxa", NULL, choices = c("(All / No filter)" = "", evs),
                                 multiple = TRUE, selectize = TRUE),
                     tags$small("Optional: filter by taxa/species", style = "color:#7f8c8d;")
                 )
          )
        ),
        div(style = "text-align:center; margin-top:10px;",
            actionButton("search", "🔍 Find Overlapping PMIDs",
                         class = "btn btn-primary btn-lg",
                         style = "background-color:#8e44ad; border-color:#7d3c98; color:white;")
        )
    )
  })
  
  # ── PAGE 2 : Compute overlap ───────────────────────────────────────────────
  overlap_result <- eventReactive(input$search, {
    req(data(), input$upstream, input$downstream)
    df       <- data()
    taxa_sel <- input$taxa
    use_taxa <- length(taxa_sel) > 0 && any(nzchar(taxa_sel))
    
    pmids_matching <- function(events) {
      df %>%
        filter(trimws(event_1) %in% events | trimws(event_2) %in% events) %>%
        pull(PMID) %>% unique()
    }
    
    pmids_up   <- pmids_matching(input$upstream)
    pmids_down <- pmids_matching(input$downstream)
    
    if (use_taxa) {
      pmids_taxa    <- pmids_matching(taxa_sel)
      overlap_pmids <- Reduce(intersect, list(pmids_up, pmids_down, pmids_taxa))
    } else {
      pmids_taxa    <- NULL
      overlap_pmids <- intersect(pmids_up, pmids_down)
    }
    
    overlap_rows <- df %>% filter(PMID %in% overlap_pmids) %>% distinct()
    
    base_summary <- overlap_rows %>%
      group_by(PMID, title, pubdate) %>%
      summarise(
        Upstream_Events = paste(unique(c(
          event_1[trimws(event_1) %in% input$upstream],
          event_2[trimws(event_2) %in% input$upstream]
        )), collapse = ", "),
        Downstream_Events = paste(unique(c(
          event_1[trimws(event_1) %in% input$downstream],
          event_2[trimws(event_2) %in% input$downstream]
        )), collapse = ", "),
        Taxa_Species = if (use_taxa) {
          paste(unique(c(
            event_1[trimws(event_1) %in% taxa_sel],
            event_2[trimws(event_2) %in% taxa_sel]
          )), collapse = ", ")
        } else { "—" },
        .groups = "drop"
      ) %>%
      arrange(PMID)
    
    if ("abstract" %in% colnames(df)) {
      abstract_lookup <- df %>% select(PMID, abstract) %>% distinct(PMID, .keep_all = TRUE)
      base_summary <- base_summary %>%
        left_join(abstract_lookup, by = "PMID") %>%
        select(PMID, title, pubdate, Upstream_Events, Downstream_Events, Taxa_Species, abstract)
    }
    
    list(
      n_upstream     = length(pmids_up),
      n_downstream   = length(pmids_down),
      n_taxa         = if (use_taxa) length(pmids_taxa) else NA,
      n_overlap      = length(overlap_pmids),
      taxa_used      = use_taxa,
      taxa_sel       = taxa_sel,
      upstream_sel   = input$upstream,
      downstream_sel = input$downstream,
      has_abstract   = "abstract" %in% colnames(df),
      pmid_summary   = base_summary,
      overlap_rows   = overlap_rows
    )
  })
  
  # ── PAGE 2 : Results UI ───────────────────────────────────────────────────
  output$overlap_ui <- renderUI({
    req(overlap_result())
    res <- overlap_result()
    tagList(
      div(class = "summary-box overlap-box",
          h3(class = "section-header", "📌 Overlap Results"),
          div(style = "background:#f8f9fa; border-radius:8px; padding:12px; margin-bottom:18px; font-size:0.95em;",
              tags$b("Upstream KE: "),
              tags$span(style = "color:#e67e22;", paste(res$upstream_sel, collapse = ", ")), tags$br(),
              tags$b("Downstream KE: "),
              tags$span(style = "color:#27ae60;", paste(res$downstream_sel, collapse = ", ")), tags$br(),
              tags$b("Taxa/Species: "),
              tags$span(style = "color:#2980b9;",
                        if (res$taxa_used) paste(res$taxa_sel, collapse = ", ") else "(no filter)")
          ),
          fluidRow(
            column(3, div(style = "text-align:center; padding:15px; background:#fef9e7; border-radius:8px;",
                          p(class = "stat-number", style = "color:#e67e22;", res$n_upstream),
                          p(class = "stat-label", "PMIDs — Upstream"))),
            column(3, div(style = "text-align:center; padding:15px; background:#eafaf1; border-radius:8px;",
                          p(class = "stat-number", style = "color:#27ae60;", res$n_downstream),
                          p(class = "stat-label", "PMIDs — Downstream"))),
            column(3, div(style = "text-align:center; padding:15px; background:#eaf2fb; border-radius:8px;",
                          p(class = "stat-number", style = "color:#2980b9;",
                            if (res$taxa_used) res$n_taxa else "—"),
                          p(class = "stat-label", "PMIDs — Taxa/Species"))),
            column(3, div(style = "text-align:center; padding:15px; background:#f5eef8; border-radius:8px;",
                          p(class = "stat-number", style = "color:#8e44ad;", res$n_overlap),
                          p(class = "stat-label", "PMIDs — ALL Overlap")))
          ),
          tags$br(),
          if (res$n_overlap == 0) {
            div(style = "text-align:center; color:#e74c3c; padding:20px;",
                h4("⚠️ No overlapping PMIDs found for the selected combination."))
          } else {
            tagList(
              div(style = "text-align:center;",
                  tags$span(class = "result-badge",
                            paste0("✅ ", res$n_overlap, " PMID(s) match all selected criteria"))
              ),
              div(style = "text-align:right; margin-bottom:10px;",
                  downloadButton("download_results", "⬇️ Download Results (.xlsx)",
                                 style = "background-color:#27ae60; border-color:#1e8449; color:white;")
              ),
              h4(class = "section-header",
                 paste0("📋 Overlapping PMID Summary (", res$n_overlap, " PMIDs)")),
              DTOutput("overlap_table")
            )
          }
      )
    )
  })
  
  output$overlap_table <- renderDT({
    req(overlap_result())
    res    <- overlap_result()
    df_out <- res$pmid_summary
    col_names <- c("PMID", "Title", "Year", "Upstream Event(s)", "Downstream Event(s)", "Taxa/Species")
    if (res$has_abstract) col_names <- c(col_names, "Abstract")
    dt <- datatable(df_out,
                    options = list(
                      pageLength = 10, dom = 'ftp', scrollX = TRUE,
                      columnDefs = if (res$has_abstract) {
                        list(list(targets = ncol(df_out) - 1,
                                  render = JS("function(data,type,row){",
                                              "if(type==='display'&&data&&data.length>120){",
                                              "return '<span title=\"'+data+'\">'+data.substr(0,120)+'...</span>';}",
                                              "return data;}")))
                      } else { list() }
                    ),
                    rownames = FALSE, colnames = col_names, escape = FALSE
    ) %>%
      formatStyle("Upstream_Events",   color = "#e67e22", fontWeight = "bold") %>%
      formatStyle("Downstream_Events", color = "#27ae60", fontWeight = "bold") %>%
      formatStyle("Taxa_Species",      color = "#2980b9", fontWeight = "bold")
    if (res$has_abstract)
      dt <- dt %>% formatStyle("abstract", fontSize = "0.85em", color = "#555")
    dt
  })
  
  output$download_results <- downloadHandler(
    filename = function() paste0("overlap_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content  = function(file) {
      req(overlap_result())
      res    <- overlap_result()
      df_out <- res$pmid_summary
      col_names <- c("PMID", "Title", "Year", "Upstream_Events", "Downstream_Events", "Taxa_Species")
      if (res$has_abstract) col_names <- c(col_names, "Abstract")
      colnames(df_out) <- col_names
      write_xlsx(df_out, file)
    }
  )
  
  # ══════════════════════════════════════════════════════════════════
  # PAGE 3 — Visualisation
  # ══════════════════════════════════════════════════════════════════
  
  output$page3_notice <- renderUI({
    if (is.null(input$file)) {
      div(class = "page-notice",
          tags$b("⚠️ No file loaded yet."), tags$br(),
          "Please go to ", tags$b("Upload & Summary"), " to upload your file, then run a search in ",
          tags$b("Key Event Analysis"), " first.")
    } else if (is.null(overlap_result())) {
      div(class = "page-notice",
          tags$b("⚠️ No search results yet."), tags$br(),
          "Please go to ", tags$b("Key Event Analysis"), " and run a search first.")
    }
  })
  
  output$viz_ui <- renderUI({
    req(overlap_result())
    res <- overlap_result()
    req(res$n_overlap > 0)
    
    tagList(
      
      # ── Row 1: Downstream events + Upstream events ──────────────────────
      fluidRow(
        
        # Chart 1 — Downstream KE bar chart
        column(6,
               div(class = "summary-box viz-box",
                   h4(class = "section-header", "⬇️ Downstream Key Events — PMID Count"),
                   div(class = "chart-ctrl",
                       sliderInput("ds_top_n", "Show top N events:",
                                   min = 1, max = min(20, length(res$downstream_sel) + 10),
                                   value = min(10, length(res$downstream_sel) + 5), step = 1)
                   ),
                   plotlyOutput("chart_downstream", height = "380px")
               )
        ),
        
        # Chart 2 — Upstream KE bar chart
        column(6,
               div(class = "summary-box", style = "border-top: 4px solid #e67e22;",
                   h4(class = "section-header", "⬆️ Upstream Key Events — PMID Count"),
                   div(class = "chart-ctrl",
                       sliderInput("us_top_n", "Show top N events:",
                                   min = 1, max = min(20, length(res$upstream_sel) + 10),
                                   value = min(10, length(res$upstream_sel) + 5), step = 1)
                   ),
                   plotlyOutput("chart_upstream", height = "380px")
               )
        )
      ),
      
      # ── Row 2: Taxa chart + Publication year trend ───────────────────────
      fluidRow(
        
        # Chart 3 — Taxa/Species bar chart (only if taxa used)
        column(6,
               div(class = "summary-box", style = "border-top: 4px solid #2980b9;",
                   h4(class = "section-header", "🧬 Taxa / Species — PMID Count"),
                   if (!res$taxa_used) {
                     p("No taxa filter was applied. Select taxa in Key Event Analysis to see this chart.",
                       style = "color:#7f8c8d; padding:20px;")
                   } else {
                     plotlyOutput("chart_taxa", height = "380px")
                   }
               )
        ),
        
        # Chart 4 — Publication year trend
        column(6,
               div(class = "summary-box", style = "border-top: 4px solid #8e44ad;",
                   h4(class = "section-header", "📅 Overlapping PMIDs by Publication Year"),
                   plotlyOutput("chart_year", height = "380px")
               )
        )
      ),
      
      # ── Row 3: Co-occurrence heatmap ─────────────────────────────────────
      fluidRow(
        column(12,
               div(class = "summary-box", style = "border-top: 4px solid #16a085;",
                   h4(class = "section-header", "🔥 Upstream × Downstream Co-occurrence Heatmap"),
                   p("Each cell shows the number of overlapping PMIDs for each upstream–downstream pair.",
                     style = "color:#7f8c8d; font-size:0.9em;"),
                   plotlyOutput("chart_heatmap", height = "420px")
               )
        )
      )
      
    )
  })
  
  # ── Helper: count PMIDs per individual event across overlap rows ──────────
  count_event_pmids <- function(overlap_rows, event_list) {
    purrr::map_dfr(event_list, function(ev) {
      n <- overlap_rows %>%
        filter(trimws(event_1) == ev | trimws(event_2) == ev) %>%
        pull(PMID) %>% unique() %>% length()
      tibble(Event = ev, PMID_Count = n)
    })
  }
  
  # Chart 1 — Upstream
  output$chart_upstream <- renderPlotly({
    req(overlap_result())
    res  <- overlap_result()
    rows <- res$overlap_rows
    df_chart <- count_event_pmids(rows, res$upstream_sel) %>%
      arrange(desc(PMID_Count)) %>%
      head(input$us_top_n) %>%
      mutate(Event = factor(Event, levels = rev(Event)))
    
    plot_ly(df_chart, x = ~PMID_Count, y = ~Event, type = "bar",
            orientation = "h",
            marker = list(color = "#e67e22",
                          line = list(color = "#ca6f1e", width = 1)),
            hovertemplate = "<b>%{y}</b><br>PMIDs: %{x}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Number of PMIDs", zeroline = FALSE),
        yaxis = list(title = "", automargin = TRUE),
        plot_bgcolor  = "#fafafa",
        paper_bgcolor = "white",
        margin = list(l = 10, r = 20, t = 10, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # Chart 2 — Downstream
  output$chart_downstream <- renderPlotly({
    req(overlap_result())
    res  <- overlap_result()
    rows <- res$overlap_rows
    df_chart <- count_event_pmids(rows, res$downstream_sel) %>%
      arrange(desc(PMID_Count)) %>%
      head(input$ds_top_n) %>%
      mutate(Event = factor(Event, levels = rev(Event)))
    
    plot_ly(df_chart, x = ~PMID_Count, y = ~Event, type = "bar",
            orientation = "h",
            marker = list(color = "#27ae60",
                          line = list(color = "#1e8449", width = 1)),
            hovertemplate = "<b>%{y}</b><br>PMIDs: %{x}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Number of PMIDs", zeroline = FALSE),
        yaxis = list(title = "", automargin = TRUE),
        plot_bgcolor  = "#fafafa",
        paper_bgcolor = "white",
        margin = list(l = 10, r = 20, t = 10, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Chart 3 — Taxa
  output$chart_taxa <- renderPlotly({
    req(overlap_result())
    res  <- overlap_result()
    req(res$taxa_used)
    rows <- res$overlap_rows
    df_chart <- count_event_pmids(rows, res$taxa_sel) %>%
      arrange(desc(PMID_Count)) %>%
      mutate(Event = factor(Event, levels = rev(Event)))
    
    plot_ly(df_chart, x = ~PMID_Count, y = ~Event, type = "bar",
            orientation = "h",
            marker = list(color = "#2980b9",
                          line = list(color = "#1a5276", width = 1)),
            hovertemplate = "<b>%{y}</b><br>PMIDs: %{x}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Number of PMIDs", zeroline = FALSE),
        yaxis = list(title = "", automargin = TRUE),
        plot_bgcolor  = "#fafafa",
        paper_bgcolor = "white",
        margin = list(l = 10, r = 20, t = 10, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Chart 4 — Publication year
  output$chart_year <- renderPlotly({
    req(overlap_result())
    res <- overlap_result()
    df_year <- res$pmid_summary %>%
      mutate(Year = as.character(pubdate)) %>%
      count(Year, name = "PMIDs") %>%
      arrange(Year)
    
    plot_ly(df_year, x = ~Year, y = ~PMIDs, type = "bar",
            marker = list(color = "#8e44ad",
                          line = list(color = "#6c3483", width = 1)),
            hovertemplate = "<b>Year: %{x}</b><br>PMIDs: %{y}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Publication Year", type = "category"),
        yaxis = list(title = "Number of PMIDs", zeroline = FALSE),
        plot_bgcolor  = "#fafafa",
        paper_bgcolor = "white",
        margin = list(l = 10, r = 20, t = 10, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Chart 5 — Co-occurrence heatmap
  output$chart_heatmap <- renderPlotly({
    req(overlap_result())
    res  <- overlap_result()
    rows <- res$overlap_rows
    
    up_events   <- res$upstream_sel
    down_events <- res$downstream_sel
    
    # Build matrix: rows = upstream, cols = downstream
    mat <- outer(up_events, down_events, FUN = Vectorize(function(u, d) {
      rows %>%
        filter(PMID %in% (rows %>%
                            filter(trimws(event_1) == u | trimws(event_2) == u) %>% pull(PMID))) %>%
        filter(trimws(event_1) == d | trimws(event_2) == d) %>%
        pull(PMID) %>% unique() %>% length()
    }))
    
    rownames(mat) <- up_events
    colnames(mat) <- down_events
    
    plot_ly(
      x = colnames(mat),
      y = rownames(mat),
      z = mat,
      type = "heatmap",
      colorscale = list(
        list(0,   "#f0f3ff"),
        list(0.5, "#7fb3f5"),
        list(1,   "#154360")
      ),
      hovertemplate = "Upstream: <b>%{y}</b><br>Downstream: <b>%{x}</b><br>PMIDs: <b>%{z}</b><extra></extra>",
      showscale = TRUE
    ) %>%
      layout(
        xaxis = list(title = "Downstream Key Events", tickangle = -30, automargin = TRUE),
        yaxis = list(title = "Upstream Key Events",   automargin = TRUE),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin = list(l = 20, r = 20, t = 20, b = 80)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
}

shinyApp(ui = ui, server = server)
