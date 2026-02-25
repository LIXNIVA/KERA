library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(writexl)
library(plotly)
library(ggplot2)

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
  .format-badge { display: inline-block; background: #eaf4fb; color: #2980b9;
                  border: 1px solid #aed6f1; border-radius: 4px;
                  padding: 2px 8px; margin: 2px; font-size: 12px; font-family: monospace; }
  .dl-bar { display: flex; gap: 8px; align-items: center; justify-content: flex-end;
            padding: 6px 0 10px 0; }
  .dl-bar .btn { font-size: 12px; padding: 4px 12px; }
  .dl-label { font-size: 12px; color: #7f8c8d; margin-right: 4px; }
"

# ─────────────────────────────────────────────────────────────────────────────
# Helper: read any supported file format
# ─────────────────────────────────────────────────────────────────────────────
read_uploaded_file <- function(path, ext) {
  ext <- tolower(ext)
  if (ext %in% c("xlsx", "xls")) {
    read_excel(path)
  } else if (ext == "csv") {
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext == "tsv") {
    read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext == "txt") {
    first_line <- readLines(path, n = 1, warn = FALSE)
    sep <- if (grepl("\t", first_line)) "\t" else if (grepl(",", first_line)) "," else ";"
    read.delim(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE, header = TRUE)
  } else {
    stop(paste("Unsupported file format: .", ext))
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# ggplot2 chart builders  (used for high-res downloads)
# ─────────────────────────────────────────────────────────────────────────────
gg_theme_clean <- function() {
  theme_minimal(base_size = 14) +
    theme(
      panel.background  = element_rect(fill = "#fafafa", colour = NA),
      plot.background   = element_rect(fill = "white",   colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#e0e0e0"),
      panel.grid.minor   = element_blank(),
      axis.text  = element_text(size = 12),
      axis.title = element_text(size = 13),
      plot.margin = margin(12, 20, 12, 12)
    )
}

make_gg_bar <- function(df_chart, fill_colour, x_label, title_label) {
  ggplot(df_chart, aes(x = PMID_Count, y = Event)) +
    geom_col(fill = fill_colour, colour = darken_hex(fill_colour, 0.2), width = 0.7) +
    geom_text(aes(label = PMID_Count), hjust = -0.3, size = 3.8, colour = "#333333") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = x_label, y = NULL, title = title_label) +
    gg_theme_clean() +
    theme(plot.title = element_text(face = "bold", size = 14, colour = "#2c3e50"))
}

make_gg_year <- function(df_year) {
  ggplot(df_year, aes(x = Year, y = PMIDs)) +
    geom_col(fill = "#8e44ad", colour = "#6c3483", width = 0.7) +
    geom_text(aes(label = PMIDs), vjust = -0.4, size = 3.8, colour = "#333333") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Publication Year", y = "Number of PMIDs",
         title = "Overlapping PMIDs by Publication Year") +
    gg_theme_clean() +
    theme(
      plot.title   = element_text(face = "bold", size = 14, colour = "#2c3e50"),
      axis.text.x  = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#e0e0e0")
    )
}

make_gg_heatmap <- function(mat) {
  df_heat <- expand.grid(
    Upstream   = rownames(mat),
    Downstream = colnames(mat),
    stringsAsFactors = FALSE
  )
  df_heat$Count <- as.vector(mat)
  ggplot(df_heat, aes(x = Downstream, y = Upstream, fill = Count)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = Count), size = 3.8, colour = "white", fontface = "bold") +
    scale_fill_gradient(low = "#d6eaf8", high = "#154360", name = "PMIDs") +
    labs(x = "Downstream Key Events", y = "Upstream Key Events",
         title = "Upstream × Downstream Co-occurrence Heatmap") +
    theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      axis.text.x      = element_text(angle = 35, hjust = 1, size = 11),
      axis.text.y      = element_text(size = 11),
      axis.title       = element_text(size = 13),
      plot.title       = element_text(face = "bold", size = 14, colour = "#2c3e50"),
      legend.position  = "right",
      plot.margin      = margin(12, 20, 12, 12)
    )
}

# Simple hex colour darkener for bar outlines
darken_hex <- function(hex, amount = 0.15) {
  rgb_vals <- col2rgb(hex) / 255
  rgb_vals <- pmax(rgb_vals - amount, 0)
  rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3])
}

# ─────────────────────────────────────────────────────────────────────────────
# Reusable download-button bar UI
# ─────────────────────────────────────────────────────────────────────────────
dl_bar_ui <- function(tiff_id, jpg_id) {
  div(class = "dl-bar",
      tags$span(class = "dl-label", "💾 Download at 600 DPI:"),
      downloadButton(tiff_id, "TIFF",
                     style = "background:#2c3e50; border-color:#1a252f; color:white; font-size:12px; padding:4px 14px;"),
      downloadButton(jpg_id,  "JPG",
                     style = "background:#3498db; border-color:#2178a8; color:white; font-size:12px; padding:4px 14px;")
  )
}

# ─────────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title  = "📊 Key Event Relationship Analyzer (KERA)",
  id     = "navbar",
  header = tags$head(tags$style(HTML(app_css))),
  
  # ══════════════════════════════════════════════════════════════════
  tabPanel("📁 Upload & Summary",
           div(class = "page-wrap",
               div(class = "upload-area",
                   fileInput("file", label = NULL,
                             accept = c(".xlsx", ".xls", ".csv", ".tsv", ".txt",
                                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                        "application/vnd.ms-excel",
                                        "text/csv", "text/tab-separated-values", "text/plain"),
                             buttonLabel = "📂 Choose File",
                             placeholder = "No file selected — drag & drop or click to browse"),
                   div(style = "margin-top: 6px; margin-bottom: 8px; color: #555; font-size: 0.9em; background: #fef9e7; border: 1px solid #f9ca74; border-radius: 6px; padding: 8px 14px; display: inline-block;",
                       tags$span("💡 Upload the file named "),
                       tags$b(style = "font-family: monospace; color: #2c3e50;", "AOPhF"),
                       tags$span(" after decompressing the downloaded archive from "),
                       tags$b("AOPhelpfinder"),
                       tags$span(".")
                   ),
                   div(style = "margin-top: 8px; color: #7f8c8d; font-size: 0.85em;",
                       "Supported formats: ",
                       tags$span(class = "format-badge", ".xlsx"),
                       tags$span(class = "format-badge", ".xls"),
                       tags$span(class = "format-badge", ".csv"),
                       tags$span(class = "format-badge", ".tsv"),
                       tags$span(class = "format-badge", ".txt")
                   )
               ),
               uiOutput("summary_ui")
           )
  ),
  
  # ══════════════════════════════════════════════════════════════════
  tabPanel("🔬 Key Event Relationship (KER) Analysis",
           div(class = "page-wrap",
               uiOutput("page2_notice"),
               uiOutput("ke_selector_ui"),
               uiOutput("overlap_ui")
           )
  ),
  
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
    file_name <- input$file$name
    ext       <- tools::file_ext(file_name)
    df <- tryCatch(
      read_uploaded_file(input$file$datapath, ext),
      error = function(e) {
        showNotification(paste("Error reading file:", conditionMessage(e)), type = "error", duration = 10)
        return(NULL)
      }
    )
    req(!is.null(df))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
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
  
  # ── PAGE 1 ────────────────────────────────────────────────────────────────
  output$summary_ui <- renderUI({
    req(event_counts())
    df <- data(); ev <- event_counts()
    ext_label <- toupper(tools::file_ext(input$file$name))
    tagList(
      div(class = "summary-box", style = "padding: 12px 20px; margin-bottom: 10px;",
          tags$span(style = "color: #7f8c8d; font-size: 0.9em;",
                    "📄 Loaded: ", tags$b(input$file$name),
                    tags$span(class = "format-badge", style = "margin-left:8px;", ext_label)
          )
      ),
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
  
  # ── PAGE 2 ────────────────────────────────────────────────────────────────
  output$page2_notice <- renderUI({
    if (is.null(input$file))
      div(class = "page-notice",
          tags$b("⚠️ No file loaded yet."), tags$br(),
          "Please go to the ", tags$b("Upload & Summary"), " tab first.")
  })
  
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
      # Row 1: Upstream (left) + Downstream (right) bar charts
      fluidRow(
        column(6,
               div(class = "summary-box", style = "border-top: 4px solid #e67e22;",
                   h4(class = "section-header", "⬆️ Upstream Key Events — PMID Count"),
                   div(class = "chart-ctrl",
                       sliderInput("us_top_n", "Show top N events:",
                                   min = 1, max = min(20, length(res$upstream_sel) + 10),
                                   value = min(10, length(res$upstream_sel) + 5), step = 1)
                   ),
                   plotlyOutput("chart_upstream", height = "380px"),
                   dl_bar_ui("dl_us_tiff", "dl_us_jpg")
               )
        ),
        column(6,
               div(class = "summary-box viz-box",
                   h4(class = "section-header", "⬇️ Downstream Key Events — PMID Count"),
                   div(class = "chart-ctrl",
                       sliderInput("ds_top_n", "Show top N events:",
                                   min = 1, max = min(20, length(res$downstream_sel) + 10),
                                   value = min(10, length(res$downstream_sel) + 5), step = 1)
                   ),
                   plotlyOutput("chart_downstream", height = "380px"),
                   dl_bar_ui("dl_ds_tiff", "dl_ds_jpg")
               )
        )
      ),
      # Row 2: Taxa + Year
      fluidRow(
        column(6,
               div(class = "summary-box", style = "border-top: 4px solid #2980b9;",
                   h4(class = "section-header", "🧬 Taxa / Species — PMID Count"),
                   if (!res$taxa_used) {
                     p("No taxa filter was applied. Select taxa in Key Event Analysis to see this chart.",
                       style = "color:#7f8c8d; padding:20px;")
                   } else {
                     tagList(
                       plotlyOutput("chart_taxa", height = "380px"),
                       dl_bar_ui("dl_taxa_tiff", "dl_taxa_jpg")
                     )
                   }
               )
        ),
        column(6,
               div(class = "summary-box", style = "border-top: 4px solid #8e44ad;",
                   h4(class = "section-header", "📅 Overlapping PMIDs by Publication Year"),
                   plotlyOutput("chart_year", height = "380px"),
                   dl_bar_ui("dl_year_tiff", "dl_year_jpg")
               )
        )
      ),
      # Row 3: Heatmap
      fluidRow(
        column(12,
               div(class = "summary-box", style = "border-top: 4px solid #16a085;",
                   h4(class = "section-header", "🔥 Upstream × Downstream Co-occurrence Heatmap"),
                   p("Each cell shows the number of overlapping PMIDs for each upstream–downstream pair.",
                     style = "color:#7f8c8d; font-size:0.9em;"),
                   plotlyOutput("chart_heatmap", height = "420px"),
                   dl_bar_ui("dl_heat_tiff", "dl_heat_jpg")
               )
        )
      )
    )
  })
  
  # ── Shared data helpers ────────────────────────────────────────────────────
  count_event_pmids <- function(overlap_rows, event_list) {
    purrr::map_dfr(event_list, function(ev) {
      n <- overlap_rows %>%
        filter(trimws(event_1) == ev | trimws(event_2) == ev) %>%
        pull(PMID) %>% unique() %>% length()
      tibble(Event = ev, PMID_Count = n)
    })
  }
  
  # Reactive data frames for each chart (shared by Plotly + ggplot download)
  df_upstream <- reactive({
    req(overlap_result(), input$us_top_n)
    res <- overlap_result()
    count_event_pmids(res$overlap_rows, res$upstream_sel) %>%
      arrange(desc(PMID_Count)) %>%
      head(input$us_top_n) %>%
      mutate(Event = factor(Event, levels = rev(Event)))
  })
  
  df_downstream <- reactive({
    req(overlap_result(), input$ds_top_n)
    res <- overlap_result()
    count_event_pmids(res$overlap_rows, res$downstream_sel) %>%
      arrange(desc(PMID_Count)) %>%
      head(input$ds_top_n) %>%
      mutate(Event = factor(Event, levels = rev(Event)))
  })
  
  df_taxa <- reactive({
    req(overlap_result())
    res <- overlap_result()
    req(res$taxa_used)
    count_event_pmids(res$overlap_rows, res$taxa_sel) %>%
      arrange(desc(PMID_Count)) %>%
      mutate(Event = factor(Event, levels = rev(Event)))
  })
  
  df_year <- reactive({
    req(overlap_result())
    res <- overlap_result()
    res$pmid_summary %>%
      mutate(Year = as.character(pubdate)) %>%
      count(Year, name = "PMIDs") %>%
      arrange(Year)
  })
  
  heatmap_mat <- reactive({
    req(overlap_result())
    res  <- overlap_result()
    rows <- res$overlap_rows
    up_events   <- res$upstream_sel
    down_events <- res$downstream_sel
    mat <- outer(up_events, down_events, FUN = Vectorize(function(u, d) {
      rows %>%
        filter(PMID %in% (rows %>%
                            filter(trimws(event_1) == u | trimws(event_2) == u) %>% pull(PMID))) %>%
        filter(trimws(event_1) == d | trimws(event_2) == d) %>%
        pull(PMID) %>% unique() %>% length()
    }))
    rownames(mat) <- up_events
    colnames(mat) <- down_events
    mat
  })
  
  # ── Plotly interactive charts ──────────────────────────────────────────────
  output$chart_upstream <- renderPlotly({
    df_chart <- df_upstream()
    plot_ly(df_chart, x = ~PMID_Count, y = ~Event, type = "bar", orientation = "h",
            marker = list(color = "#e67e22", line = list(color = "#ca6f1e", width = 1)),
            hovertemplate = "<b>%{y}</b><br>PMIDs: %{x}<extra></extra>") %>%
      layout(xaxis = list(title = "Number of PMIDs", zeroline = FALSE),
             yaxis = list(title = "", automargin = TRUE),
             plot_bgcolor = "#fafafa", paper_bgcolor = "white",
             margin = list(l = 10, r = 20, t = 10, b = 40)) %>%
      config(displayModeBar = FALSE)
  })
  
  output$chart_downstream <- renderPlotly({
    df_chart <- df_downstream()
    plot_ly(df_chart, x = ~PMID_Count, y = ~Event, type = "bar", orientation = "h",
            marker = list(color = "#27ae60", line = list(color = "#1e8449", width = 1)),
            hovertemplate = "<b>%{y}</b><br>PMIDs: %{x}<extra></extra>") %>%
      layout(xaxis = list(title = "Number of PMIDs", zeroline = FALSE),
             yaxis = list(title = "", automargin = TRUE),
             plot_bgcolor = "#fafafa", paper_bgcolor = "white",
             margin = list(l = 10, r = 20, t = 10, b = 40)) %>%
      config(displayModeBar = FALSE)
  })
  
  output$chart_taxa <- renderPlotly({
    df_chart <- df_taxa()
    plot_ly(df_chart, x = ~PMID_Count, y = ~Event, type = "bar", orientation = "h",
            marker = list(color = "#2980b9", line = list(color = "#1a5276", width = 1)),
            hovertemplate = "<b>%{y}</b><br>PMIDs: %{x}<extra></extra>") %>%
      layout(xaxis = list(title = "Number of PMIDs", zeroline = FALSE),
             yaxis = list(title = "", automargin = TRUE),
             plot_bgcolor = "#fafafa", paper_bgcolor = "white",
             margin = list(l = 10, r = 20, t = 10, b = 40)) %>%
      config(displayModeBar = FALSE)
  })
  
  output$chart_year <- renderPlotly({
    df_y <- df_year()
    plot_ly(df_y, x = ~Year, y = ~PMIDs, type = "bar",
            marker = list(color = "#8e44ad", line = list(color = "#6c3483", width = 1)),
            hovertemplate = "<b>Year: %{x}</b><br>PMIDs: %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "Publication Year", type = "category"),
             yaxis = list(title = "Number of PMIDs", zeroline = FALSE),
             plot_bgcolor = "#fafafa", paper_bgcolor = "white",
             margin = list(l = 10, r = 20, t = 10, b = 40)) %>%
      config(displayModeBar = FALSE)
  })
  
  output$chart_heatmap <- renderPlotly({
    mat <- heatmap_mat()
    plot_ly(x = colnames(mat), y = rownames(mat), z = mat, type = "heatmap",
            colorscale = list(list(0, "#f0f3ff"), list(0.5, "#7fb3f5"), list(1, "#154360")),
            hovertemplate = "Upstream: <b>%{y}</b><br>Downstream: <b>%{x}</b><br>PMIDs: <b>%{z}</b><extra></extra>",
            showscale = TRUE) %>%
      layout(
        xaxis = list(title = "Downstream Key Events", tickangle = -30, automargin = TRUE),
        yaxis = list(title = "Upstream Key Events", automargin = TRUE),
        plot_bgcolor = "white", paper_bgcolor = "white",
        margin = list(l = 20, r = 20, t = 20, b = 80)) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── High-res download handlers (600 DPI via ggplot2) ──────────────────────
  
  # Helper to create a download handler for a given chart
  make_dl_handler <- function(get_gg, base_name, fmt) {
    downloadHandler(
      filename = function() {
        ext <- if (fmt == "tiff") "tiff" else "jpg"
        paste0(base_name, "_600dpi_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        p <- get_gg()
        n_events <- if (inherits(p$data, "data.frame")) nrow(p$data) else 10
        plot_h <- max(4, n_events * 0.35 + 1.5)   # dynamic height
        if (fmt == "tiff") {
          ggsave(file, plot = p, device = "tiff", dpi = 600,
                 width = 10, height = plot_h, units = "in", compression = "lzw")
        } else {
          ggsave(file, plot = p, device = "jpeg", dpi = 600,
                 width = 10, height = plot_h, units = "in", quality = 95)
        }
      }
    )
  }
  
  # Upstream
  output$dl_us_tiff <- make_dl_handler(
    function() make_gg_bar(df_upstream(), "#e67e22", "Number of PMIDs", "Upstream Key Events — PMID Count"),
    "upstream_ke", "tiff")
  output$dl_us_jpg  <- make_dl_handler(
    function() make_gg_bar(df_upstream(), "#e67e22", "Number of PMIDs", "Upstream Key Events — PMID Count"),
    "upstream_ke", "jpg")
  
  # Downstream
  output$dl_ds_tiff <- make_dl_handler(
    function() make_gg_bar(df_downstream(), "#27ae60", "Number of PMIDs", "Downstream Key Events — PMID Count"),
    "downstream_ke", "tiff")
  output$dl_ds_jpg  <- make_dl_handler(
    function() make_gg_bar(df_downstream(), "#27ae60", "Number of PMIDs", "Downstream Key Events — PMID Count"),
    "downstream_ke", "jpg")
  
  # Taxa
  output$dl_taxa_tiff <- make_dl_handler(
    function() make_gg_bar(df_taxa(), "#2980b9", "Number of PMIDs", "Taxa / Species — PMID Count"),
    "taxa_ke", "tiff")
  output$dl_taxa_jpg  <- make_dl_handler(
    function() make_gg_bar(df_taxa(), "#2980b9", "Number of PMIDs", "Taxa / Species — PMID Count"),
    "taxa_ke", "jpg")
  
  # Year
  output$dl_year_tiff <- make_dl_handler(
    function() make_gg_year(df_year()),
    "publication_year", "tiff")
  output$dl_year_jpg  <- make_dl_handler(
    function() make_gg_year(df_year()),
    "publication_year", "jpg")
  
  # Heatmap
  output$dl_heat_tiff <- make_dl_handler(
    function() make_gg_heatmap(heatmap_mat()),
    "cooccurrence_heatmap", "tiff")
  output$dl_heat_jpg  <- make_dl_handler(
    function() make_gg_heatmap(heatmap_mat()),
    "cooccurrence_heatmap", "jpg")
  
}

shinyApp(ui = ui, server = server)
