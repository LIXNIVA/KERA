library(shiny)
library(httr)
library(jsonlite)
library(readxl)
library(writexl)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)
library(purrr)

options(shiny.maxRequestSize = 200 * 1024^2)

# Null-coalescing operator — available globally
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

# ══════════════════════════════════════════════════════════════════════════════
# HELPERS
# ══════════════════════════════════════════════════════════════════════════════

read_uploaded_file <- function(path, ext) {
  ext <- tolower(ext)
  if (ext %in% c("xlsx","xls")) read_excel(path)
  else if (ext == "csv")  read.csv(path, stringsAsFactors=FALSE, check.names=FALSE)
  else if (ext == "tsv")  read.delim(path, stringsAsFactors=FALSE, check.names=FALSE)
  else if (ext == "txt") {
    first_line <- readLines(path, n=1, warn=FALSE)
    sep <- if (grepl("\t",first_line)) "\t" else if (grepl(",",first_line)) "," else ";"
    read.delim(path, sep=sep, stringsAsFactors=FALSE, check.names=FALSE, header=TRUE)
  } else stop(paste("Unsupported file format:", ext))
}

darken_hex <- function(hex, amount=0.15) {
  v <- col2rgb(hex)/255
  rgb(pmax(v[1]-amount,0), pmax(v[2]-amount,0), pmax(v[3]-amount,0))
}

gg_theme_clean <- function() {
  theme_minimal(base_size=14) +
    theme(panel.background=element_rect(fill="#fafafa",colour=NA),
          plot.background=element_rect(fill="white",colour=NA),
          panel.grid.major.y=element_blank(),
          panel.grid.major.x=element_line(colour="#e0e0e0"),
          panel.grid.minor=element_blank(),
          axis.text=element_text(size=12), axis.title=element_text(size=13),
          plot.margin=margin(12,20,12,12))
}

make_gg_bar <- function(df_chart, fill_colour, x_label, title_label) {
  ggplot(df_chart, aes(x=PMID_Count, y=Event)) +
    geom_col(fill=fill_colour, colour=darken_hex(fill_colour,0.2), width=0.7) +
    geom_text(aes(label=PMID_Count), hjust=-0.3, size=3.8, colour="#333333") +
    scale_x_continuous(expand=expansion(mult=c(0,0.12))) +
    labs(x=x_label, y=NULL, title=title_label) + gg_theme_clean() +
    theme(plot.title=element_text(face="bold",size=14,colour="#2c3e50"))
}

make_gg_year <- function(df_year) {
  ggplot(df_year, aes(x=Year, y=PMIDs)) +
    geom_col(fill="#8e44ad", colour="#6c3483", width=0.7) +
    geom_text(aes(label=PMIDs), vjust=-0.4, size=3.8, colour="#333333") +
    scale_y_continuous(expand=expansion(mult=c(0,0.12))) +
    labs(x="Publication Year", y="Number of PMIDs", title="Overlapping PMIDs by Publication Year") +
    gg_theme_clean() +
    theme(plot.title=element_text(face="bold",size=14,colour="#2c3e50"),
          axis.text.x=element_text(angle=45,hjust=1),
          panel.grid.major.x=element_blank(), panel.grid.major.y=element_line(colour="#e0e0e0"))
}

make_gg_heatmap <- function(mat) {
  df_heat <- expand.grid(Upstream=rownames(mat), Downstream=colnames(mat), stringsAsFactors=FALSE)
  df_heat$Count <- as.vector(mat)
  ggplot(df_heat, aes(x=Downstream, y=Upstream, fill=Count)) +
    geom_tile(colour="white", linewidth=0.5) +
    geom_text(aes(label=Count), size=3.8, colour="white", fontface="bold") +
    scale_fill_gradient(low="#d6eaf8", high="#154360", name="PMIDs") +
    labs(x="Downstream Key Events", y="Upstream Key Events",
         title="Upstream × Downstream Co-occurrence Heatmap") +
    theme_minimal(base_size=13) +
    theme(plot.background=element_rect(fill="white",colour=NA),
          axis.text.x=element_text(angle=35,hjust=1,size=11),
          axis.text.y=element_text(size=11), axis.title=element_text(size=13),
          plot.title=element_text(face="bold",size=14,colour="#2c3e50"),
          legend.position="right", plot.margin=margin(12,20,12,12))
}

dl_bar_ui <- function(tiff_id, jpg_id) {
  div(class="dl-bar",
      tags$span(class="dl-label", "💾 Download at 600 DPI:"),
      downloadButton(tiff_id, "TIFF", style="background:#2c3e50;border-color:#1a252f;color:white;font-size:12px;padding:4px 14px;"),
      downloadButton(jpg_id,  "JPG",  style="background:#3498db;border-color:#2178a8;color:white;font-size:12px;padding:4px 14px;")
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# CLAUDE EVALUATION
# ══════════════════════════════════════════════════════════════════════════════

evaluate_abstract <- function(abstract_text, api_key) {
  url <- "https://api.anthropic.com/v1/messages"
  prompt <- paste0(
    "You are a scientific literature analyst specializing in toxicology and epidemiology. ",
    "Evaluate the following abstract using the strict scoring rubrics below, then extract stressor and species.\n\n",
    
    "=== TASK 1: CONCORDANCE SCORING (integer 0-100) ===\n",
    "Score each concordance type based ONLY on evidence signals present in the abstract. ",
    "Do not infer or assume what the full text might contain beyond what is described.\n\n",
    
    "1. DOSE-RESPONSE CONCORDANCE\n",
    "Definition: Evidence that greater exposure leads to greater biological effect (monotonic dose gradient).\n",
    "Scoring rubric:\n",
    "  80-100: Multiple dose/concentration levels tested with a clear monotonic gradient explicitly reported ",
    "(e.g. low/medium/high dose groups, EC50/LC50 curves, benchmark dose modelling).\n",
    "  50-79:  Some dose levels mentioned but gradient is unclear, non-monotonic, or only two dose levels compared.\n",
    "  20-49:  Single dose or exposure level used; no dose-response design; effect reported at one concentration only.\n",
    "  0-19:   No dose information present; purely correlational, observational with no exposure quantification, ",
    "or in silico/review study with no original dose data.\n\n",
    
    "2. TEMPORAL CONCORDANCE\n",
    "Definition: Evidence that exposure precedes the outcome in time; time-course or sequential data are reported.\n",
    "Scoring rubric:\n",
    "  80-100: Explicit time-course measurements at multiple time points; latency periods reported; ",
    "longitudinal or prospective design with clear temporal sequence of exposure then effect.\n",
    "  50-79:  Exposure timing is mentioned and precedes outcome, but only one or two time points measured; ",
    "or retrospective design with documented exposure history.\n",
    "  20-49:  Cross-sectional design; timing implied but not directly measured; acute single-timepoint study.\n",
    "  0-19:   No temporal information; simultaneous measurement of exposure and outcome; ",
    "or purely mechanistic/in silico study with no time dimension.\n\n",
    
    "3. INCIDENCE CONCORDANCE\n",
    "Definition: Evidence that exposure rates align with disease or outcome rates across populations or groups.\n",
    "Scoring rubric:\n",
    "  80-100: Population-level incidence, prevalence, or morbidity/mortality rates explicitly compared across ",
    "high- vs low-exposure groups or geographic areas with different exposure levels.\n",
    "  50-79:  Group-level differences in outcome frequency reported (e.g. % affected per group) but not ",
    "formally expressed as incidence rates; or ecological study with indirect exposure proxies.\n",
    "  20-49:  Single population studied with no unexposed comparison; effect reported as mean difference ",
    "rather than incidence; animal study comparing groups but not epidemiological in design.\n",
    "  0-19:   No population or incidence data; purely mechanistic, cellular, or in vitro study; ",
    "individual-level data only with no group comparison.\n\n",
    
    "=== TASK 2: EXTRACTION ===\n",
    "- stressor: Chemical(s) or non-chemical stressor(s) studied (e.g. compound name, radiation, ",
    "temperature, hypoxia, UV, nanoparticles, noise). Separate multiple with semicolons. null if absent.\n",
    "- species: Organism(s) or taxa studied (e.g. Mytilus galloprovincialis, Danio rerio, human, rat). ",
    "Separate multiple with semicolons. null if absent.\n",
    "- note: One sentence (max 25 words) citing the specific abstract evidence that drove your scores.\n\n",
    
    "Return ONLY a valid JSON object with exactly six keys. No markdown, no code fences.\n",
    "Example:\n",
    "{\"dose_response\":82,\"temporal\":55,\"incidence\":15,",
    "\"note\":\"Three dose levels with monotonic DNA adduct increase; single 48h timepoint; lab study only.\",",
    "\"stressor\":\"benzo[a]pyrene\",\"species\":\"Mytilus galloprovincialis\"}\n\n",
    "Abstract:\n", abstract_text
  )
  payload <- list(model="claude-haiku-4-5-20251001", max_tokens=400,
                  messages=list(list(role="user", content=prompt)))
  tryCatch({
    res      <- POST(url,
                     add_headers("x-api-key"=api_key, "anthropic-version"="2023-06-01", "Content-Type"="application/json"),
                     body=toJSON(payload, auto_unbox=TRUE), encode="raw", timeout(30))
    raw_text <- content(res)$content[[1]]$text
    na_result <- list(dose_response=NA, temporal=NA, incidence=NA,
                      note=NA_character_, stressor=NA_character_, species=NA_character_)
    if (is.null(raw_text)) return(na_result)
    clean      <- trimws(gsub("```[a-zA-Z]*|```","",raw_text))
    json_match <- regmatches(clean, regexpr("\\{.*\\}", clean, perl=TRUE))
    if (length(json_match)==0) return(na_result)
    parsed <- fromJSON(json_match)
    safe_chr <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      v <- as.character(x[[1]])
      if (nchar(trimws(v)) == 0 || v == "NULL" || v == "null") return(NA_character_)
      v
    }
    safe_int <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_integer_)
      suppressWarnings(as.integer(x[[1]]))
    }
    list(
      dose_response = safe_int(parsed$dose_response),
      temporal      = safe_int(parsed$temporal),
      incidence     = safe_int(parsed$incidence),
      note          = safe_chr(parsed$note),
      stressor      = safe_chr(parsed$stressor),
      species       = safe_chr(parsed$species)
    )
  }, error=function(e) list(dose_response=NA, temporal=NA, incidence=NA,
                            note=NA_character_, stressor=NA_character_, species=NA_character_))
}

prob_badge <- function(v) {
  if (is.na(v)) return('<span class="prob-badge prob-na">N/A</span>')
  cls <- if (v>=70) "prob-high" else if (v>=45) "prob-medium" else "prob-low"
  paste0('<span class="prob-badge ',cls,'">',v,'%</span>')
}

# ══════════════════════════════════════════════════════════════════════════════
# SHARED CSS
# ══════════════════════════════════════════════════════════════════════════════

app_css <- "
  /* ── Base ── */
  body { background:#f5f7fa; font-family:'Segoe UI',sans-serif; color:#1a1d23; }

  /* ── Navbar ── */
  .navbar { background-color:#1e3a5f !important; border:none; box-shadow:0 2px 8px rgba(0,0,0,.15); }
  .navbar-default .navbar-brand { color:#fff !important; font-size:18px; font-weight:700; letter-spacing:3px; }
  .navbar-default .navbar-nav > li > a { color:rgba(255,255,255,.8) !important; font-size:13px; }
  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:hover { background:#2563eb !important; color:#fff !important; }
  .navbar-default .navbar-nav > li > a:hover { background:rgba(255,255,255,.1) !important; color:#fff !important; }

  /* ── Page layout ── */
  .page-wrap { padding:25px 20px; }
  .summary-box { background:white; border-radius:8px; padding:20px;
                 box-shadow:0 2px 8px rgba(0,0,0,.07); margin-bottom:20px; }
  .upload-area { border:2px dashed #3498db; border-radius:8px; padding:30px;
                 text-align:center; background:white; margin-bottom:20px; }
  .section-header { border-left:4px solid #2563eb; padding-left:12px; margin-bottom:15px; color:#1e3a5f; }
  .stat-number { font-size:2em; font-weight:bold; margin:0; }
  .stat-label  { color:#7f8c8d; margin:0; font-size:.9em; }

  /* ── Event tags ── */
  .event-tag { display:inline-block; background:#eaf4fb; color:#2980b9;
               border:1px solid #aed6f1; border-radius:15px;
               padding:4px 12px; margin:4px; font-size:13px; }

  /* ── KE boxes ── */
  .ke-box-upstream   { border-top:4px solid #e67e22; }
  .ke-box-downstream { border-top:4px solid #27ae60; }
  .ke-box-taxa       { border-top:4px solid #2980b9; }
  .ke-box-stressor   { border-top:4px solid #c0392b; }
  .overlap-box       { border-top:4px solid #8e44ad; }
  .viz-box           { border-top:4px solid #e74c3c; }
  .arrow-center { text-align:center; font-size:1.8em; padding-top:38px; color:#95a5a6; }
  .result-badge { display:inline-block; background:#8e44ad; color:white;
                  border-radius:20px; padding:6px 18px; font-size:1.1em;
                  font-weight:bold; margin-bottom:15px; }
  .page-notice { background:#fef9e7; border:1px solid #f9ca74; border-radius:8px;
                 padding:18px; text-align:center; color:#7f6000; margin-bottom:16px; }
  .chart-ctrl  { background:#f8f9fa; border-radius:8px; padding:15px; margin-bottom:15px; }
  .format-badge { display:inline-block; background:#eaf4fb; color:#2980b9;
                  border:1px solid #aed6f1; border-radius:4px;
                  padding:2px 8px; margin:2px; font-size:12px; font-family:monospace; }
  .dl-bar { display:flex; gap:8px; align-items:center; justify-content:flex-end;
            padding:6px 0 10px 0; }
  .dl-bar .btn { font-size:12px; padding:4px 12px; }
  .dl-label { font-size:12px; color:#7f8c8d; margin-right:4px; }
  .modal-pmid-title { font-size:1.1em; font-weight:bold; color:#2c3e50;
                      margin-bottom:10px; padding:8px 12px;
                      background:#f0f3ff; border-radius:6px; border-left:4px solid #3498db; }

  /* ── KERA tab: sidebar layout ── */
  .kera-two-col { display:grid; grid-template-columns:280px 1fr; min-height:calc(100vh - 110px); }
  .kera-sidebar { border-right:1px solid #e2e6ed; padding:24px 18px; background:#f8f9fb;
                  display:flex; flex-direction:column; gap:18px; }
  .kera-main    { padding:26px 30px; background:#f5f7fa; overflow-x:auto; }
  .kera-section-label { font-size:10px; letter-spacing:3px; text-transform:uppercase;
                        color:#6b7280; font-family:'Courier New',monospace; margin-bottom:6px; font-weight:700; }
  .kera-sidebar .form-control,
  .kera-sidebar input[type='text'],
  .kera-sidebar input[type='password'] {
    background:white !important; border:1px solid #e2e6ed !important;
    border-radius:6px !important; font-size:12px !important; padding:8px 10px !important;
    color:#1a1d23 !important; width:100% !important;
  }
  .kera-sidebar input[type='file'] {
    background:white !important; border:1px dashed #e2e6ed !important;
    border-radius:6px !important; font-size:11px !important; padding:8px !important;
    width:100% !important; cursor:pointer !important;
  }
  .kera-sidebar label { font-size:10px !important; color:#6b7280 !important;
                        letter-spacing:1px !important; margin-bottom:3px !important; }
  .run-btn { width:100%; padding:12px; background:#2563eb; color:#fff; border:none;
             border-radius:6px; font-family:'Courier New',monospace; font-size:12px;
             font-weight:700; letter-spacing:2px; cursor:pointer; transition:all .2s;
             text-transform:uppercase; }
  .run-btn:hover { background:#1d4ed8; box-shadow:0 4px 12px rgba(37,99,235,.25); }
  .dl-full-btn { width:100%; padding:10px; background:transparent; color:#16a34a;
                 border:1px solid #16a34a; border-radius:6px; font-family:'Courier New',monospace;
                 font-size:11px; font-weight:700; letter-spacing:1px; cursor:pointer;
                 text-transform:uppercase; text-decoration:none; display:block;
                 text-align:center; margin-bottom:6px; transition:all .2s; }
  .dl-full-btn:hover { background:#f0fdf4; }
  .dl-pmid-btn { width:100%; padding:10px; background:transparent; color:#2563eb;
                 border:1px solid #2563eb; border-radius:6px; font-family:'Courier New',monospace;
                 font-size:11px; font-weight:700; letter-spacing:1px; cursor:pointer;
                 text-transform:uppercase; text-decoration:none; display:block;
                 text-align:center; transition:all .2s; }
  .dl-pmid-btn:hover { background:#eff6ff; }
  .status-bar { padding:9px 12px; border-radius:6px; font-family:'Courier New',monospace;
                font-size:11px; line-height:1.5; }
  .status-idle    { background:#f3f4f6; color:#6b7280; border:1px solid #e2e6ed; }
  .status-running { background:#eff6ff; color:#2563eb; border:1px solid #bfdbfe; }
  .status-done    { background:#f0fdf4; color:#16a34a; border:1px solid #bbf7d0; }
  .status-error   { background:#fff7f5; color:#dc2626; border:1px solid #fecaca; }
  @keyframes spin { to { transform:rotate(360deg); } }
  .spinner { display:inline-block; width:10px; height:10px; border:2px solid #bfdbfe;
             border-top-color:#2563eb; border-radius:50%; animation:spin .8s linear infinite;
             margin-right:6px; vertical-align:middle; }

  /* ── Probability badges ── */
  .prob-badge { display:inline-block; min-width:44px; padding:3px 8px; border-radius:20px;
                font-family:'Courier New',monospace; font-size:11px; font-weight:700; text-align:center; }
  .prob-high   { background:#f0fdf4; color:#16a34a; border:1px solid #bbf7d0; }
  .prob-medium { background:#fefce8; color:#ca8a04; border:1px solid #fef08a; }
  .prob-low    { background:#fff7ed; color:#ea580c; border:1px solid #fed7aa; }
  .prob-na     { background:#f3f4f6; color:#9ca3af; border:1px solid #e5e7eb; }

  /* ── DataTable (global) ── */
  table.dataTable tbody tr:hover td { background:#f5f7ff !important; }
  table.dataTable tbody tr.selected td { background:#eff6ff !important; border-left:3px solid #2563eb !important; }

  /* ── Empty state ── */
  .empty-state { display:flex; flex-direction:column; align-items:center; justify-content:center;
                 height:340px; gap:12px; color:#9ca3af; background:white;
                 border:1px solid #e2e6ed; border-radius:8px; }
  .empty-icon { font-size:36px; opacity:.2; }
  .empty-text { font-size:11px; letter-spacing:2px; text-transform:uppercase; font-family:'Courier New',monospace; }

  /* ── Shiny progress notification ── */
  #shiny-notification-panel { bottom:20px !important; top:auto !important; right:20px !important; width:300px !important; }
  .shiny-notification { background:#fff !important; border:1px solid #e2e6ed !important;
    border-left:4px solid #2563eb !important; border-radius:8px !important;
    font-size:12px !important; padding:14px 16px !important;
    box-shadow:0 4px 20px rgba(0,0,0,.1) !important; }
  .shiny-notification-message { color:#2563eb !important; font-weight:600 !important; }
  .shiny-notification-detail  { color:#6b7280 !important; font-size:11px !important; }
  .shiny-progress-bar { background:#2563eb !important; height:3px !important; }
  .progress { background:#e2e6ed !important; height:3px !important; border-radius:3px !important; margin-top:8px !important; }

  /* ── Sub-header bar ── */
  .kera-subheader {
    display: flex;
    justify-content: space-between;
    align-items: center;
    background: #162d4a;
    border-bottom: 1px solid rgba(255,255,255,0.08);
    padding: 9px 20px;
  }
  .kera-subheader-left { display:flex; align-items:center; gap:10px; }
  .kera-subheader-title {
    font-size: 15px;
    font-weight: 600;
    color: rgba(255,255,255,0.82);
    letter-spacing: 0.3px;
    font-family: 'Segoe UI', sans-serif;
  }
  .kera-aop-link {
    color: rgba(255,255,255,0.8) !important;
    text-decoration: underline !important;
    text-underline-offset: 2px;
  }
  .kera-aop-link:hover { color: #fff !important; }
  .kera-manual-btn {
    font-size: 13px;
    font-weight: 700;
    letter-spacing: 1px;
    background: rgba(255,255,255,0.1);
    color: #fff !important;
    text-decoration: none !important;
    padding: 4px 12px;
    border-radius: 4px;
    border: 1px solid rgba(255,255,255,0.25);
    font-family: 'Segoe UI', sans-serif;
    transition: background 0.2s;
  }
  .kera-manual-btn:hover { background: rgba(255,255,255,0.2) !important; }

  /* ── Footer ── */
  .kera-footer {
    text-align: center;
    padding: 18px 20px;
    background: #f8f9fb;
    border-top: 1px solid #e2e6ed;
    font-size: 18px;
    color: #4b5563;
    font-family: 'Segoe UI', sans-serif;
  }
  .kera-footer a {
    color: #2563eb !important;
    text-decoration: none;
  }
  .kera-footer a:hover { text-decoration: underline !important; }

  /* ── Scrollbar ── */
  ::-webkit-scrollbar { width:6px; height:6px; }
  ::-webkit-scrollbar-track { background:#f5f7fa; }
  ::-webkit-scrollbar-thumb { background:#d1d5db; border-radius:3px; }
"

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- navbarPage(
  title  = "KERA",
  id     = "navbar",
  header = tagList(
    tags$head(tags$style(HTML(app_css))),
    tags$div(class = "kera-subheader",
             tags$div(class = "kera-subheader-left",
                      tags$span(class = "kera-subheader-title",
                                "Key Event Relationship Analyser for ",
                                tags$a("AOPhelpFinder", href="https://aop-helpfinder.u-paris-sciences.fr/",
                                       target="_blank", class="kera-aop-link")
                      )
             ),
             tags$div(class = "kera-subheader-right",
                      tags$a(href="www_static/KERA_User_Manual.pdf", target="_blank",
                             download="KERA_User_Manual.pdf", class="kera-manual-btn",
                             "📄 User Manual")
             )
    )
  ),
  
  # ── Tab 1: Upload & Summary ────────────────────────────────────────────────
  tabPanel("📁 Upload & Summary",
           div(class="page-wrap",
               div(class="upload-area",
                   fileInput("file_conc", label=NULL,
                             accept=c(".xlsx",".xls",".csv",".tsv",".txt"),
                             buttonLabel="📂 Choose File",
                             placeholder="No file selected — drag & drop or click to browse"),
                   div(style="margin-top:10px; margin-bottom:8px; color:#7f6000; font-size:0.88em; background:#fef9e7; border:1px solid #f9ca74; border-radius:6px; padding:8px 14px; display:inline-block;",
                       tags$span("💡 Upload the file named "),
                       tags$b(style="font-family:monospace; color:#2c3e50;", "AOPhF"),
                       tags$span(" after decompressing the downloaded archive from "),
                       tags$a("AOPhelpFinder",
                              href="https://aop-helpfinder.u-paris-sciences.fr/",
                              target="_blank",
                              style="color:#2563eb; font-weight:bold; text-decoration:none;"),
                       tags$span(".")
                   ),
                   div(style="margin-top:8px;color:#7f8c8d;font-size:.85em;",
                       "Supported formats: ",
                       tags$span(class="format-badge",".xlsx"),
                       tags$span(class="format-badge",".xls"),
                       tags$span(class="format-badge",".csv"),
                       tags$span(class="format-badge",".tsv"),
                       tags$span(class="format-badge",".txt")
                   )
               ),
               uiOutput("logo_ui"),
               uiOutput("summary_ui")
           )
  ),
  
  # ── Tab 2: Key Event Analysis ──────────────────────────────────────────────
  tabPanel("🔬 Key Event Analysis",
           div(class="page-wrap",
               uiOutput("page2_notice"),
               uiOutput("ke_selector_ui"),
               uiOutput("overlap_ui")
           )
  ),
  
  # ── Tab 3: Visualisation ──────────────────────────────────────────────────
  tabPanel("📈 Visualisation",
           div(class="page-wrap",
               uiOutput("page3_notice"),
               uiOutput("viz_ui")
           )
  ),
  
  # ── Tab 4: Concordance Evaluation (KERA AI) ───────────────────────────────
  tabPanel("🤖 Concordance Evaluation",
           div(class="kera-two-col",
               
               # Sidebar
               div(class="kera-sidebar",
                   div(
                     div(class="kera-section-label", "01 — Upload Excel"),
                     fileInput("file_kera", NULL, accept=c(".xlsx",".xls"), placeholder="Choose Excel file")
                   ),
                   div(
                     div(class="kera-section-label", "02 — Claude API Key"),
                     passwordInput("api_key", NULL,
                                   value=Sys.getenv("ANTHROPIC_API_KEY"), placeholder="sk-ant-..."),
                     tags$small(style="color:#9ca3af;font-size:10px;font-family:'Courier New',monospace;",
                                "Auto-loaded from .Renviron")
                   ),
                   div(
                     div(class="kera-section-label", "03 — Abstract Column Name"),
                     textInput("abstract_col", NULL, value="Abstract", placeholder="Column name")
                   ),
                   div(
                     div(class="kera-section-label", "04 — Probability Threshold"),
                     sliderInput("min_prob", NULL, min=0, max=100, value=50, step=5, post="%")
                   ),
                   actionButton("run_btn", "▶  Run Evaluation", class="run-btn"),
                   uiOutput("kera_status_ui"),
                   uiOutput("kera_download_ui")
               ),
               
               # Main
               div(class="kera-main",
                   uiOutput("kera_results_header"),
                   uiOutput("kera_selection_bar"),
                   uiOutput("kera_table_ui")
               )
           )
  ),
  footer = tags$div(class = "kera-footer",
                    "📧 Contact: ",
                    tags$a("Li Xie", href="mailto:li.xie@niva.no"), " — li.xie@niva.no",
                    tags$span(style="margin:0 12px; color:#d1d5db;", "|"),
                    "Norwegian Institute for Water Research (NIVA)",
                    tags$span(style="margin:0 12px; color:#d1d5db;", "|"),
                    tags$a("📄 User Manual", href="www_static/KERA_User_Manual.pdf", target="_blank",
                           download="KERA_User_Manual.pdf")
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # Locate www/ folder relative to app file or working directory
  app_dir  <- tryCatch(dirname(normalizePath(sys.frames()[[1]]$filename)), error=function(e) getwd())
  www_path <- file.path(app_dir, "www")
  if (!dir.exists(www_path)) www_path <- file.path(getwd(), "www")
  if (dir.exists(www_path)) addResourcePath("www_static", www_path)
  
  # ── Logo ───────────────────────────────────────────────────────────────────
  output$logo_ui <- renderUI({
    logo_file <- file.path(www_path, "KERA.png")
    if (file.exists(logo_file)) {
      div(style = "text-align:center; margin:24px 0 8px 0;",
          tags$img(
            src   = "www_static/KERA.png",
            style = "max-width:420px; width:70%; height:auto; opacity:0.92;"
          )
      )
    }
  })
  
  # ── Concordance data (tabs 1-3) ────────────────────────────────────────────
  conc_data <- reactive({
    req(input$file_conc)
    ext <- tools::file_ext(input$file_conc$name)
    df  <- tryCatch(
      read_uploaded_file(input$file_conc$datapath, ext),
      error=function(e) { showNotification(paste("Error:",conditionMessage(e)), type="error", duration=10); NULL }
    )
    req(!is.null(df))
    df <- as.data.frame(df, stringsAsFactors=FALSE)
    if ("date" %in% colnames(df) && !"pubdate" %in% colnames(df)) df <- df %>% rename(pubdate=date)
    df
  })
  
  unique_events <- reactive({
    req(conc_data())
    sort(unique(trimws(na.omit(c(conc_data()$event_1, conc_data()$event_2)))))
  })
  
  event_counts <- reactive({
    req(conc_data())
    all_ev <- trimws(na.omit(c(conc_data()$event_1, conc_data()$event_2)))
    tbl    <- as.data.frame(table(all_ev), stringsAsFactors=FALSE)
    colnames(tbl) <- c("Event Name","Count")
    tbl[order(-tbl$Count),]
  })
  
  # ── TAB 1: Summary ──────────────────────────────────────────────────────────
  output$summary_ui <- renderUI({
    req(event_counts())
    df <- conc_data(); ev <- event_counts()
    ext_label <- toupper(tools::file_ext(input$file_conc$name))
    tagList(
      div(class="summary-box", style="padding:12px 20px;margin-bottom:10px;",
          tags$span(style="color:#7f8c8d;font-size:.9em;",
                    "📄 Loaded: ", tags$b(input$file_conc$name),
                    tags$span(class="format-badge", style="margin-left:8px;", ext_label))),
      fluidRow(
        column(4, div(class="summary-box", style="text-align:center;",
                      p(class="stat-number", style="color:#2c3e50;", nrow(df)),
                      p(class="stat-label","Total Records"))),
        column(4, div(class="summary-box", style="text-align:center;",
                      p(class="stat-number", style="color:#3498db;", nrow(ev)),
                      p(class="stat-label","Unique Events"))),
        column(4, div(class="summary-box", style="text-align:center;",
                      p(class="stat-number", style="color:#27ae60;", length(unique(df$PMID))),
                      p(class="stat-label","Unique PMIDs")))
      ),
      div(class="summary-box",
          h4(class="section-header","🏷️ All Unique Events"),
          div(lapply(ev$`Event Name`, function(e) tags$span(class="event-tag", e)))),
      div(class="summary-box",
          h4(class="section-header","📋 Event Frequency Table"),
          DTOutput("event_table")),
      div(class="summary-box",
          h4(class="section-header","📄 Raw Data Preview"),
          DTOutput("raw_table"))
    )
  })
  
  output$event_table <- renderDT({
    req(event_counts())
    datatable(event_counts(), options=list(pageLength=10,dom='ftp'), rownames=FALSE) %>%
      formatStyle("Count", background=styleColorBar(event_counts()$Count,"#aed6f1"),
                  backgroundSize="100% 80%", backgroundRepeat="no-repeat", backgroundPosition="center")
  })
  
  output$raw_table <- renderDT({
    req(conc_data())
    datatable(conc_data(), options=list(pageLength=5,dom='ftp',scrollX=TRUE), rownames=FALSE)
  })
  
  # ── TAB 2: Key Event Analysis ──────────────────────────────────────────────
  output$page2_notice <- renderUI({
    if (is.null(input$file_conc))
      div(class="page-notice", tags$b("⚠️ No file loaded."), tags$br(),
          "Please go to ", tags$b("Upload & Summary"), " tab first.")
  })
  
  output$ke_selector_ui <- renderUI({
    req(unique_events())
    evs <- unique_events()
    div(class="summary-box",
        h3(class="section-header","🔬 Key Event & Taxa Overlap Analysis"),
        p("Select events for each role, an optional taxa/species and stressor, then click Search."),
        fluidRow(
          # Upstream
          column(3, div(class="summary-box ke-box-upstream",
                        h4("⬆️ Upstream Key Event(s)"),
                        selectInput("upstream", NULL, choices=evs, multiple=TRUE, selectize=TRUE),
                        tags$small("Select upstream event(s)", style="color:#7f8c8d;"))),
          # Arrow
          column(1, div(class="arrow-center", "→")),
          # Downstream
          column(3, div(class="summary-box ke-box-downstream",
                        h4("⬇️ Downstream Key Event(s)"),
                        selectInput("downstream", NULL, choices=evs, multiple=TRUE, selectize=TRUE),
                        tags$small("Select downstream event(s)", style="color:#7f8c8d;"))),
          # Link icon
          column(1, div(class="arrow-center", "🔗")),
          # Taxa
          column(2, div(class="summary-box ke-box-taxa",
                        h4("🧬 Taxa / Species"),
                        selectInput("taxa", NULL,
                                    choices=c("(All / No filter)"="", evs),
                                    multiple=TRUE, selectize=TRUE),
                        tags$small("Optional: filter by taxa/species", style="color:#7f8c8d;"))),
          # Stressor — NEW
          column(2, div(class="summary-box ke-box-stressor",
                        h4("⚗️ Stressor"),
                        selectInput("stressor", NULL,
                                    choices=c("(All / No filter)"="", evs),
                                    multiple=TRUE, selectize=TRUE),
                        tags$small("Optional: filter by chemical/stressor", style="color:#7f8c8d;")))
        ),
        div(style="text-align:center;margin-top:10px;",
            actionButton("search","🔍 Find Overlapping PMIDs", class="btn btn-primary btn-lg",
                         style="background:#8e44ad;border-color:#7d3c98;color:white;"))
    )
  })
  
  overlap_result <- eventReactive(input$search, {
    req(conc_data(), input$upstream, input$downstream)
    df           <- conc_data()
    taxa_sel     <- input$taxa
    stressor_sel <- input$stressor
    use_taxa     <- length(taxa_sel) > 0 && any(nzchar(taxa_sel))
    use_stressor <- length(stressor_sel) > 0 && any(nzchar(stressor_sel))
    
    pmids_matching <- function(events) {
      df %>% filter(trimws(event_1) %in% events | trimws(event_2) %in% events) %>%
        pull(PMID) %>% unique()
    }
    
    pmids_up       <- pmids_matching(input$upstream)
    pmids_down     <- pmids_matching(input$downstream)
    pmids_taxa     <- if (use_taxa)     pmids_matching(taxa_sel)     else NULL
    pmids_stressor <- if (use_stressor) pmids_matching(stressor_sel) else NULL
    
    filter_list <- list(pmids_up, pmids_down)
    if (use_taxa)     filter_list <- c(filter_list, list(pmids_taxa))
    if (use_stressor) filter_list <- c(filter_list, list(pmids_stressor))
    overlap_pmids <- Reduce(intersect, filter_list)
    
    overlap_rows <- df %>% filter(PMID %in% overlap_pmids) %>% distinct()
    
    base_summary <- overlap_rows %>%
      group_by(PMID, title, pubdate) %>%
      summarise(
        Upstream_Events   = paste(unique(c(event_1[trimws(event_1) %in% input$upstream],
                                           event_2[trimws(event_2) %in% input$upstream])), collapse=", "),
        Downstream_Events = paste(unique(c(event_1[trimws(event_1) %in% input$downstream],
                                           event_2[trimws(event_2) %in% input$downstream])), collapse=", "),
        Taxa_Species      = if (use_taxa) paste(unique(c(
          event_1[trimws(event_1) %in% taxa_sel], event_2[trimws(event_2) %in% taxa_sel]
        )), collapse=", ") else "—",
        Stressor          = if (use_stressor) paste(unique(c(
          event_1[trimws(event_1) %in% stressor_sel], event_2[trimws(event_2) %in% stressor_sel]
        )), collapse=", ") else "—",
        .groups="drop"
      ) %>% arrange(PMID)
    
    if ("abstract" %in% colnames(df)) {
      base_summary <- base_summary %>%
        left_join(df %>% select(PMID, abstract) %>% distinct(PMID, .keep_all=TRUE), by="PMID") %>%
        select(PMID, title, pubdate, Upstream_Events, Downstream_Events, Taxa_Species, Stressor, abstract)
    }
    
    list(
      n_upstream    = length(pmids_up),
      n_downstream  = length(pmids_down),
      n_taxa        = if (use_taxa)     length(pmids_taxa)     else NA,
      n_stressor    = if (use_stressor) length(pmids_stressor) else NA,
      n_overlap     = length(overlap_pmids),
      taxa_used     = use_taxa,
      taxa_sel      = taxa_sel,
      stressor_used = use_stressor,
      stressor_sel  = stressor_sel,
      upstream_sel  = input$upstream,
      downstream_sel= input$downstream,
      has_abstract  = "abstract" %in% colnames(df),
      pmid_summary  = base_summary,
      overlap_rows  = overlap_rows
    )
  })
  
  output$overlap_ui <- renderUI({
    req(overlap_result())
    res <- overlap_result()
    tagList(
      div(class="summary-box overlap-box",
          h3(class="section-header","📌 Overlap Results"),
          # Filter summary
          div(style="background:#f8f9fa;border-radius:8px;padding:12px;margin-bottom:18px;font-size:.95em;",
              tags$b("Upstream KE: "),
              tags$span(style="color:#e67e22;", paste(res$upstream_sel, collapse=", ")), tags$br(),
              tags$b("Downstream KE: "),
              tags$span(style="color:#27ae60;", paste(res$downstream_sel, collapse=", ")), tags$br(),
              tags$b("Taxa/Species: "),
              tags$span(style="color:#2980b9;",
                        if (res$taxa_used) paste(res$taxa_sel, collapse=", ") else "(no filter)"), tags$br(),
              tags$b("Stressor: "),
              tags$span(style="color:#c0392b;",
                        if (res$stressor_used) paste(res$stressor_sel, collapse=", ") else "(no filter)")
          ),
          # Stat boxes
          fluidRow(
            column(2, div(style="text-align:center;padding:15px;background:#fef9e7;border-radius:8px;",
                          p(class="stat-number", style="color:#e67e22;", res$n_upstream),
                          p(class="stat-label","PMIDs — Upstream"))),
            column(2, div(style="text-align:center;padding:15px;background:#eafaf1;border-radius:8px;",
                          p(class="stat-number", style="color:#27ae60;", res$n_downstream),
                          p(class="stat-label","PMIDs — Downstream"))),
            column(3, div(style="text-align:center;padding:15px;background:#eaf2fb;border-radius:8px;",
                          p(class="stat-number", style="color:#2980b9;",
                            if (res$taxa_used) res$n_taxa else "—"),
                          p(class="stat-label","PMIDs — Taxa/Species"))),
            column(3, div(style="text-align:center;padding:15px;background:#fdf2f0;border-radius:8px;",
                          p(class="stat-number", style="color:#c0392b;",
                            if (res$stressor_used) res$n_stressor else "—"),
                          p(class="stat-label","PMIDs — Stressor"))),
            column(2, div(style="text-align:center;padding:15px;background:#f5eef8;border-radius:8px;",
                          p(class="stat-number", style="color:#8e44ad;", res$n_overlap),
                          p(class="stat-label","PMIDs — ALL Overlap")))
          ),
          tags$br(),
          if (res$n_overlap == 0) {
            div(style="text-align:center;color:#e74c3c;padding:20px;",
                h4("⚠️ No overlapping PMIDs found."))
          } else {
            tagList(
              div(style="text-align:center;",
                  tags$span(class="result-badge",
                            paste0("✅ ", res$n_overlap, " PMID(s) match all criteria"))),
              div(style="text-align:right;margin-bottom:10px;",
                  downloadButton("download_results","⬇️ Download Results (.xlsx)",
                                 style="background:#27ae60;border-color:#1e8449;color:white;")),
              h4(class="section-header",
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
    col_names <- c("PMID","Title","Year","Upstream Event(s)","Downstream Event(s)","Taxa/Species","Stressor")
    if (res$has_abstract) col_names <- c(col_names, "Abstract")
    
    dt <- datatable(
      df_out,
      options=list(
        pageLength=10, dom='ftp', scrollX=TRUE,
        columnDefs=if (res$has_abstract)
          list(list(targets=ncol(df_out)-1,
                    render=JS("function(data,type,row){",
                              "if(type==='display'&&data&&data.length>120){",
                              "return '<span title=\"'+data+'\">'+data.substr(0,120)+'...</span>';}",
                              "return data;}")))
        else list()
      ),
      rownames=FALSE, colnames=col_names, escape=FALSE
    ) %>%
      formatStyle("Upstream_Events",   color="#e67e22", fontWeight="bold") %>%
      formatStyle("Downstream_Events", color="#27ae60", fontWeight="bold") %>%
      formatStyle("Taxa_Species",      color="#2980b9", fontWeight="bold") %>%
      formatStyle("Stressor",          color="#c0392b", fontWeight="bold")
    
    if (res$has_abstract)
      dt <- dt %>% formatStyle("abstract", fontSize=".85em", color="#555")
    dt
  })
  
  output$download_results <- downloadHandler(
    filename=function() paste0("overlap_results_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".xlsx"),
    content =function(file) {
      res    <- overlap_result()
      df_out <- res$pmid_summary
      col_names <- c("PMID","Title","Year","Upstream_Events","Downstream_Events","Taxa_Species","Stressor")
      if (res$has_abstract) col_names <- c(col_names, "Abstract")
      colnames(df_out) <- col_names
      write_xlsx(df_out, file)
    }
  )
  
  # ── TAB 3: Visualisation ───────────────────────────────────────────────────
  output$page3_notice <- renderUI({
    if (is.null(input$file_conc))
      div(class="page-notice", tags$b("⚠️ No file loaded."), tags$br(),
          "Upload in ", tags$b("Upload & Summary"), " then run a search.")
    else if (is.null(overlap_result()))
      div(class="page-notice", tags$b("⚠️ No search results yet."), tags$br(),
          "Run a search in ", tags$b("Key Event Analysis"), " first.")
  })
  
  count_event_pmids <- function(overlap_rows, event_list) {
    purrr::map_dfr(event_list, function(ev) {
      n <- overlap_rows %>%
        filter(trimws(event_1)==ev | trimws(event_2)==ev) %>%
        pull(PMID) %>% unique() %>% length()
      tibble(Event=ev, PMID_Count=n)
    })
  }
  
  df_upstream   <- reactive({
    req(overlap_result(), input$us_top_n)
    res <- overlap_result()
    count_event_pmids(res$overlap_rows, res$upstream_sel) %>%
      arrange(desc(PMID_Count)) %>% head(input$us_top_n) %>%
      mutate(Event=factor(Event, levels=rev(Event)))
  })
  df_downstream <- reactive({
    req(overlap_result(), input$ds_top_n)
    res <- overlap_result()
    count_event_pmids(res$overlap_rows, res$downstream_sel) %>%
      arrange(desc(PMID_Count)) %>% head(input$ds_top_n) %>%
      mutate(Event=factor(Event, levels=rev(Event)))
  })
  df_taxa       <- reactive({
    req(overlap_result())
    res <- overlap_result(); req(res$taxa_used)
    count_event_pmids(res$overlap_rows, res$taxa_sel) %>%
      arrange(desc(PMID_Count)) %>% mutate(Event=factor(Event, levels=rev(Event)))
  })
  df_year       <- reactive({
    req(overlap_result())
    overlap_result()$pmid_summary %>%
      mutate(Year=as.character(pubdate)) %>%
      count(Year, name="PMIDs") %>% arrange(Year)
  })
  heatmap_mat   <- reactive({
    req(overlap_result())
    res  <- overlap_result(); rows <- res$overlap_rows
    mat  <- outer(res$upstream_sel, res$downstream_sel, FUN=Vectorize(function(u, d) {
      rows %>%
        filter(PMID %in% (rows %>% filter(trimws(event_1)==u | trimws(event_2)==u) %>% pull(PMID))) %>%
        filter(trimws(event_1)==d | trimws(event_2)==d) %>%
        pull(PMID) %>% unique() %>% length()
    }))
    rownames(mat) <- res$upstream_sel; colnames(mat) <- res$downstream_sel
    mat
  })
  
  output$viz_ui <- renderUI({
    req(overlap_result())
    res <- overlap_result(); req(res$n_overlap > 0)
    tagList(
      fluidRow(
        column(6, div(class="summary-box", style="border-top:4px solid #e67e22;",
                      h4(class="section-header","⬆️ Upstream Key Events — PMID Count"),
                      div(class="chart-ctrl",
                          sliderInput("us_top_n","Show top N:", min=1,
                                      max=min(20, length(res$upstream_sel)+10),
                                      value=min(10, length(res$upstream_sel)+5), step=1)),
                      plotlyOutput("chart_upstream", height="380px"),
                      dl_bar_ui("dl_us_tiff","dl_us_jpg"))),
        column(6, div(class="summary-box viz-box",
                      h4(class="section-header","⬇️ Downstream Key Events — PMID Count"),
                      div(class="chart-ctrl",
                          sliderInput("ds_top_n","Show top N:", min=1,
                                      max=min(20, length(res$downstream_sel)+10),
                                      value=min(10, length(res$downstream_sel)+5), step=1)),
                      plotlyOutput("chart_downstream", height="380px"),
                      dl_bar_ui("dl_ds_tiff","dl_ds_jpg")))
      ),
      fluidRow(
        column(6, div(class="summary-box", style="border-top:4px solid #2980b9;",
                      h4(class="section-header","🧬 Taxa / Species — PMID Count"),
                      if (!res$taxa_used) p("No taxa filter applied.", style="color:#7f8c8d;padding:20px;")
                      else tagList(plotlyOutput("chart_taxa", height="380px"),
                                   dl_bar_ui("dl_taxa_tiff","dl_taxa_jpg")))),
        column(6, div(class="summary-box", style="border-top:4px solid #8e44ad;",
                      h4(class="section-header","📅 Overlapping PMIDs by Year"),
                      plotlyOutput("chart_year", height="380px"),
                      dl_bar_ui("dl_year_tiff","dl_year_jpg")))
      ),
      fluidRow(
        column(12, div(class="summary-box", style="border-top:4px solid #16a085;",
                       h4(class="section-header","🔥 Upstream × Downstream Co-occurrence Heatmap"),
                       p("Each cell = overlapping PMIDs for each upstream–downstream pair.",
                         style="color:#7f8c8d;font-size:.9em;"),
                       plotlyOutput("chart_heatmap", height="420px"),
                       dl_bar_ui("dl_heat_tiff","dl_heat_jpg")))
      )
    )
  })
  
  output$chart_upstream   <- renderPlotly({
    df <- df_upstream()
    plot_ly(df, x=~PMID_Count, y=~Event, type="bar", orientation="h", source="upstream_click",
            marker=list(color="#e67e22", line=list(color="#ca6f1e", width=1)),
            hovertemplate="<b>%{y}</b><br>Click to view PMIDs<extra></extra>") %>%
      layout(xaxis=list(title="Number of PMIDs", zeroline=FALSE),
             yaxis=list(title="", automargin=TRUE),
             plot_bgcolor="#fafafa", paper_bgcolor="white",
             margin=list(l=10,r=20,t=10,b=40)) %>%
      config(displayModeBar=FALSE) %>% event_register("plotly_click")
  })
  
  output$chart_downstream <- renderPlotly({
    df <- df_downstream()
    plot_ly(df, x=~PMID_Count, y=~Event, type="bar", orientation="h", source="downstream_click",
            marker=list(color="#27ae60", line=list(color="#1e8449", width=1)),
            hovertemplate="<b>%{y}</b><br>Click to view PMIDs<extra></extra>") %>%
      layout(xaxis=list(title="Number of PMIDs", zeroline=FALSE),
             yaxis=list(title="", automargin=TRUE),
             plot_bgcolor="#fafafa", paper_bgcolor="white",
             margin=list(l=10,r=20,t=10,b=40)) %>%
      config(displayModeBar=FALSE) %>% event_register("plotly_click")
  })
  
  output$chart_taxa       <- renderPlotly({
    df <- df_taxa()
    plot_ly(df, x=~PMID_Count, y=~Event, type="bar", orientation="h", source="taxa_click",
            marker=list(color="#2980b9", line=list(color="#1a5276", width=1)),
            hovertemplate="<b>%{y}</b><br>Click to view PMIDs<extra></extra>") %>%
      layout(xaxis=list(title="Number of PMIDs", zeroline=FALSE),
             yaxis=list(title="", automargin=TRUE),
             plot_bgcolor="#fafafa", paper_bgcolor="white",
             margin=list(l=10,r=20,t=10,b=40)) %>%
      config(displayModeBar=FALSE) %>% event_register("plotly_click")
  })
  
  output$chart_year       <- renderPlotly({
    df <- df_year()
    plot_ly(df, x=~Year, y=~PMIDs, type="bar", source="year_click",
            marker=list(color="#8e44ad", line=list(color="#6c3483", width=1)),
            hovertemplate="<b>Year: %{x}</b><br>Click to view PMIDs<extra></extra>") %>%
      layout(xaxis=list(title="Publication Year", type="category"),
             yaxis=list(title="Number of PMIDs", zeroline=FALSE),
             plot_bgcolor="#fafafa", paper_bgcolor="white",
             margin=list(l=10,r=20,t=10,b=40)) %>%
      config(displayModeBar=FALSE) %>% event_register("plotly_click")
  })
  
  output$chart_heatmap    <- renderPlotly({
    mat <- heatmap_mat()
    plot_ly(x=colnames(mat), y=rownames(mat), z=mat, type="heatmap", source="heatmap_click",
            colorscale=list(list(0,"#f0f3ff"), list(0.5,"#7fb3f5"), list(1,"#154360")),
            hovertemplate="Upstream: <b>%{y}</b><br>Downstream: <b>%{x}</b><br>Click to view PMIDs<extra></extra>",
            showscale=TRUE) %>%
      layout(xaxis=list(title="Downstream Key Events", tickangle=-30, automargin=TRUE),
             yaxis=list(title="Upstream Key Events", automargin=TRUE),
             plot_bgcolor="white", paper_bgcolor="white",
             margin=list(l=20,r=20,t=20,b=80)) %>%
      config(displayModeBar=FALSE) %>% event_register("plotly_click")
  })
  
  # High-res downloads
  make_dl_handler <- function(get_gg, base_name, fmt) {
    downloadHandler(
      filename=function() paste0(base_name, "_600dpi_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".",
                                 if (fmt=="tiff") "tiff" else "jpg"),
      content =function(file) {
        p <- get_gg()
        n_events <- if (inherits(p$data,"data.frame")) nrow(p$data) else 10
        plot_h   <- max(4, n_events*0.35+1.5)
        if (fmt=="tiff") ggsave(file, plot=p, device="tiff", dpi=600, width=10, height=plot_h,
                                units="in", compression="lzw")
        else             ggsave(file, plot=p, device="jpeg", dpi=600, width=10, height=plot_h,
                                units="in", quality=95)
      }
    )
  }
  output$dl_us_tiff   <- make_dl_handler(function() make_gg_bar(df_upstream(),   "#e67e22","Number of PMIDs","Upstream KE — PMID Count"),   "upstream_ke",          "tiff")
  output$dl_us_jpg    <- make_dl_handler(function() make_gg_bar(df_upstream(),   "#e67e22","Number of PMIDs","Upstream KE — PMID Count"),   "upstream_ke",          "jpg")
  output$dl_ds_tiff   <- make_dl_handler(function() make_gg_bar(df_downstream(), "#27ae60","Number of PMIDs","Downstream KE — PMID Count"), "downstream_ke",        "tiff")
  output$dl_ds_jpg    <- make_dl_handler(function() make_gg_bar(df_downstream(), "#27ae60","Number of PMIDs","Downstream KE — PMID Count"), "downstream_ke",        "jpg")
  output$dl_taxa_tiff <- make_dl_handler(function() make_gg_bar(df_taxa(),       "#2980b9","Number of PMIDs","Taxa — PMID Count"),           "taxa_ke",              "tiff")
  output$dl_taxa_jpg  <- make_dl_handler(function() make_gg_bar(df_taxa(),       "#2980b9","Number of PMIDs","Taxa — PMID Count"),           "taxa_ke",              "jpg")
  output$dl_year_tiff <- make_dl_handler(function() make_gg_year(df_year()),                                                                "publication_year",     "tiff")
  output$dl_year_jpg  <- make_dl_handler(function() make_gg_year(df_year()),                                                                "publication_year",     "jpg")
  output$dl_heat_tiff <- make_dl_handler(function() make_gg_heatmap(heatmap_mat()),                                                         "cooccurrence_heatmap", "tiff")
  output$dl_heat_jpg  <- make_dl_handler(function() make_gg_heatmap(heatmap_mat()),                                                         "cooccurrence_heatmap", "jpg")
  
  # Click-to-drill modal
  modal_data_rv  <- reactiveVal(NULL)
  modal_title_rv <- reactiveVal("pmid_results")
  
  show_pmid_modal <- function(df_modal, title_label, accent) {
    has_abs   <- "abstract" %in% colnames(df_modal)
    col_names <- c("PMID","Title","Year","Upstream Event(s)","Downstream Event(s)","Taxa/Species","Stressor")
    if (has_abs) col_names <- c(col_names, "Abstract")
    modal_data_rv(df_modal)
    modal_title_rv(gsub("[^A-Za-z0-9_-]","_", title_label))
    output$modal_pmid_table <- renderDT({
      dt <- datatable(df_modal,
                      options=list(pageLength=10, dom="ftp", scrollX=TRUE),
                      rownames=FALSE, colnames=col_names, escape=FALSE) %>%
        formatStyle("Upstream_Events",   color="#e67e22", fontWeight="bold") %>%
        formatStyle("Downstream_Events", color="#27ae60", fontWeight="bold") %>%
        formatStyle("Taxa_Species",      color="#2980b9", fontWeight="bold") %>%
        formatStyle("Stressor",          color="#c0392b", fontWeight="bold")
      dt
    })
    showModal(modalDialog(
      title=NULL, size="l", easyClose=TRUE, footer=modalButton("Close"),
      tags$style(HTML(".modal-dialog{max-width:92vw!important;width:92vw!important;}.modal-body{padding:20px 28px;}")),
      div(style=paste0("border-left:4px solid ",accent,";padding:10px 16px;background:#f4f6f7;border-radius:6px;",
                       "margin-bottom:16px;display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:8px;"),
          span(style="font-size:1.08em;font-weight:bold;color:#2c3e50;flex:1;", title_label),
          div(style="display:flex;gap:8px;align-items:center;",
              tags$span(style="font-size:12px;color:#7f8c8d;","Download:"),
              downloadButton("modal_dl_xlsx","XLSX",
                             style="background:#27ae60;border-color:#1e8449;color:white;font-size:12px;padding:4px 14px;"),
              downloadButton("modal_dl_csv", "CSV",
                             style="background:#2980b9;border-color:#1a5276;color:white;font-size:12px;padding:4px 14px;"))),
      DTOutput("modal_pmid_table")
    ))
  }
  
  output$modal_dl_xlsx <- downloadHandler(
    filename=function() paste0(modal_title_rv(), "_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".xlsx"),
    content =function(file) { req(modal_data_rv()); write_xlsx(modal_data_rv(), file) })
  output$modal_dl_csv  <- downloadHandler(
    filename=function() paste0(modal_title_rv(), "_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".csv"),
    content =function(file) { req(modal_data_rv()); write.csv(modal_data_rv(), file, row.names=FALSE) })
  
  pmids_for_event <- function(ev) {
    res   <- overlap_result()
    pmids <- res$overlap_rows %>%
      filter(trimws(event_1)==ev | trimws(event_2)==ev) %>%
      pull(PMID) %>% unique()
    res$pmid_summary %>% filter(PMID %in% pmids)
  }
  
  observe({ click <- event_data("plotly_click", source="upstream_click");   req(click, overlap_result())
  ev <- as.character(click$y); df <- isolate(pmids_for_event(ev))
  isolate(show_pmid_modal(df, paste0("Upstream KE: ", ev, " — ", nrow(df), " PMID(s)"), "#e67e22")) })
  
  observe({ click <- event_data("plotly_click", source="downstream_click"); req(click, overlap_result())
  ev <- as.character(click$y); df <- isolate(pmids_for_event(ev))
  isolate(show_pmid_modal(df, paste0("Downstream KE: ", ev, " — ", nrow(df), " PMID(s)"), "#27ae60")) })
  
  observe({ click <- event_data("plotly_click", source="taxa_click");       req(click, overlap_result())
  ev <- as.character(click$y); df <- isolate(pmids_for_event(ev))
  isolate(show_pmid_modal(df, paste0("Taxa/Species: ", ev, " — ", nrow(df), " PMID(s)"), "#2980b9")) })
  
  observe({ click <- event_data("plotly_click", source="year_click");       req(click, overlap_result())
  yr <- as.character(click$x)
  df <- isolate(overlap_result()$pmid_summary %>%
                  mutate(yr_str=as.character(pubdate)) %>%
                  filter(yr_str==yr) %>% select(-yr_str))
  isolate(show_pmid_modal(df, paste0("Year: ", yr, " — ", nrow(df), " PMID(s)"), "#8e44ad")) })
  
  observe({ click <- event_data("plotly_click", source="heatmap_click");    req(click, overlap_result())
  up_ev <- as.character(click$y); dn_ev <- as.character(click$x)
  df <- isolate({
    res  <- overlap_result(); rows <- res$overlap_rows
    p_up <- rows %>% filter(trimws(event_1)==up_ev | trimws(event_2)==up_ev) %>% pull(PMID) %>% unique()
    p_dn <- rows %>% filter(trimws(event_1)==dn_ev | trimws(event_2)==dn_ev) %>% pull(PMID) %>% unique()
    res$pmid_summary %>% filter(PMID %in% intersect(p_up, p_dn))
  })
  isolate(show_pmid_modal(df, paste0(up_ev, " × ", dn_ev, " — ", nrow(df), " PMID(s)"), "#16a085")) })
  
  # ── TAB 4: Concordance Evaluation ─────────────────────────────────────────
  rv <- reactiveValues(results=NULL, status="idle", message="Upload an Excel file and click Run")
  
  selected_rows <- reactive({
    if (is.null(rv$results)) return(integer(0))
    df    <- rv$results
    min_p <- input$min_prob %||% 50
    prob_max <- pmax(
      ifelse(is.na(df[["Dose-Response Probability (%)"]]), 0L, df[["Dose-Response Probability (%)"]]),
      ifelse(is.na(df[["Temporal Probability (%)"]]),      0L, df[["Temporal Probability (%)"]]),
      ifelse(is.na(df[["Incidence Probability (%)"]]),     0L, df[["Incidence Probability (%)"]])
    )
    which(prob_max >= min_p)
  })
  
  output$kera_status_ui <- renderUI({
    cls <- switch(rv$status,
                  idle    = "status-bar status-idle",
                  running = "status-bar status-running",
                  done    = "status-bar status-done",
                  error   = "status-bar status-error")
    spinner <- if (rv$status=="running") tags$span(class="spinner") else NULL
    div(class=cls, spinner, rv$message)
  })
  
  output$kera_download_ui <- renderUI({
    if (is.null(rv$results)) return(NULL)
    tagList(
      downloadButton("download_btn", "↓  Download Full Results", class="dl-full-btn"),
      downloadButton("pmid_btn",     "↓  Download Selected PMIDs", class="dl-pmid-btn")
    )
  })
  
  output$kera_results_header <- renderUI({
    if (is.null(rv$results)) return(NULL)
    nsel <- length(selected_rows())
    div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:16px;",
        div(style="font-size:16px;font-weight:700;", "Concordance Evaluation Results"),
        div(style="display:flex;gap:8px;",
            div(style="font-family:'Courier New',monospace;font-size:11px;color:#6b7280;background:white;border:1px solid #e2e6ed;border-radius:20px;padding:3px 11px;",
                paste0(nrow(rv$results)," abstracts")),
            if (nsel>0)
              div(style="font-family:'Courier New',monospace;font-size:11px;color:#2563eb;background:#eff6ff;border:1px solid #bfdbfe;border-radius:20px;padding:3px 11px;",
                  paste0(nsel," above threshold"))
        )
    )
  })
  
  output$kera_selection_bar <- renderUI({
    if (is.null(rv$results)) return(NULL)
    sel   <- input$results_table_rows_selected
    n_sel <- length(sel)
    style <- "display:flex;align-items:center;gap:10px;padding:9px 13px;background:#eff6ff;border:1px solid #bfdbfe;border-radius:6px;margin-bottom:14px;font-size:12px;color:#2563eb;font-weight:500;"
    if (n_sel==0)
      div(style=style, "☑ Click rows to select abstracts for PMID export. The probability threshold auto-selects rows.")
    else
      div(style=style, "✓", tags$strong(paste0(n_sel," abstract(s) selected")), "— click ↓ Download Selected PMIDs")
  })
  
  output$kera_table_ui <- renderUI({
    if (is.null(rv$results))
      return(div(class="empty-state",
                 div(class="empty-icon","◈"),
                 div(class="empty-text","Awaiting evaluation")))
    DTOutput("results_table")
  })
  
  observeEvent(input$run_btn, {
    if (is.null(input$file_kera))        { rv$status<-"error"; rv$message<-"Please upload an Excel file first."; return() }
    if (nchar(trimws(input$api_key))==0) { rv$status<-"error"; rv$message<-"API key is missing. Check .Renviron."; return() }
    rv$status <- "running"; rv$message <- "Reading file..."; rv$results <- NULL
    tryCatch({
      df        <- read_excel(input$file_kera$datapath)
      col_input <- trimws(input$abstract_col)
      col_match <- names(df)[tolower(names(df)) == tolower(col_input)]
      if (length(col_match)==0) {
        rv$status  <- "error"
        rv$message <- paste0("Column '",col_input,"' not found. Available: ",paste(names(df),collapse=", "))
        return()
      }
      col       <- col_match[1]
      abstracts <- df[[col]]; n <- nrow(df)
      dose_resp <- rep(NA_integer_,n); temporal  <- rep(NA_integer_,n)
      incidence <- rep(NA_integer_,n); notes     <- rep(NA_character_,n)
      stressors <- rep(NA_character_,n); species_v <- rep(NA_character_,n)
      withProgress(message="Evaluating abstracts...", value=0, {
        for (i in seq_len(n)) {
          incProgress(1/n, detail=paste0("Abstract ",i," of ",n))
          r             <- evaluate_abstract(as.character(abstracts[i]), trimws(input$api_key))
          dose_resp[i]  <- r$dose_response; temporal[i]  <- r$temporal
          incidence[i]  <- r$incidence;     notes[i]     <- r$note
          stressors[i]  <- r$stressor;      species_v[i] <- r$species
          Sys.sleep(0.3)
        }
      })
      out_df <- df
      out_df[["Dose-Response Probability (%)"]] <- dose_resp
      out_df[["Temporal Probability (%)"]]      <- temporal
      out_df[["Incidence Probability (%)"]]     <- incidence
      out_df[["Note"]]                          <- notes
      out_df[["Stressor"]]                      <- stressors
      out_df[["Species"]]                       <- species_v
      rv$results <- out_df; rv$status <- "done"
      rv$message <- paste0("✓ Done — ", n, " abstracts evaluated")
    }, error=function(e) { rv$status<-"error"; rv$message<-paste0("Error: ",conditionMessage(e)) })
  })
  
  output$results_table <- renderDT({
    req(rv$results)
    df            <- rv$results
    sel_rows      <- selected_rows()
    abs_col_input <- trimws(input$abstract_col)
    abs_col_match <- names(df)[tolower(names(df)) == tolower(abs_col_input)]
    abs_col       <- if (length(abs_col_match)>0) abs_col_match[1] else abs_col_input
    prob_cols     <- c("Dose-Response Probability (%)","Temporal Probability (%)","Incidence Probability (%)")
    for (col in prob_cols) {
      if (col %in% names(df)) df[[col]] <- sapply(df[[col]], prob_badge)
    }
    if (abs_col %in% names(df)) {
      orig     <- nchar(as.character(rv$results[[abs_col]]))
      df[[abs_col]] <- paste0(substr(as.character(df[[abs_col]]),1,150), ifelse(orig>150,"…",""))
    }
    datatable(df, escape=FALSE, rownames=FALSE,
              selection=list(mode="multiple", selected=sel_rows, target="row"),
              options=list(pageLength=10, scrollX=TRUE, dom="frtip",
                           columnDefs=list(
                             list(width="240px", targets=which(names(df)==abs_col)-1),
                             list(width="200px", targets=which(names(df)=="Note")-1),
                             list(width="150px", targets=which(names(df)=="Stressor")-1),
                             list(width="150px", targets=which(names(df)=="Species")-1),
                             list(className="dt-center", targets=which(names(df) %in% prob_cols)-1)
                           )
              )
    )
  }, server=FALSE)
  
  output$download_btn <- downloadHandler(
    filename=function() paste0("KERA_evaluated_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".xlsx"),
    content =function(file) write_xlsx(rv$results, file))
  
  output$pmid_btn <- downloadHandler(
    filename=function() paste0("KERA_PMIDs_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".txt"),
    content =function(file) {
      df      <- rv$results
      tbl_sel <- input$results_table_rows_selected
      rows    <- if (length(tbl_sel)>0) tbl_sel else selected_rows()
      if (length(rows)==0 || !"PMID" %in% names(df)) { writeLines("No PMIDs selected.", file); return() }
      pmids        <- as.character(df[["PMID"]][rows])
      pmids        <- pmids[!is.na(pmids) & nchar(pmids)>0]
      pubmed_query <- paste(paste0(pmids,"[PMID]"), collapse=" OR ")
      writeLines(c("# KERA Selected PMIDs",
                   paste0("# Generated: ", format(Sys.time(),"%Y-%m-%d %H:%M:%S")),
                   paste0("# Total: ", length(pmids)), "",
                   "## Individual PMIDs:", pmids, "",
                   "## PubMed Search String:", pubmed_query), file)
    }
  )
}

shinyApp(ui=ui, server=server)