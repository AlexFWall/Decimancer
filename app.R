library(shiny)
library(bslib)
library(readxl)
library(writexl)
library(DT)

source("R/coord_parser.R")
source("R/column_detector.R")

# -- UI -----------------------------------------------------------------------
ui <- page_sidebar(
  title = tags$div(
    style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
    tags$div(
      tags$span("Decimancer",
                style = "font-family:'Rosarivo',serif; font-size:2.1em;"),
      tags$br(),
      tags$span("Convert any geographic coordinates to decimal degrees.",
                style = "font-size:0.75em; opacity:0.85;")
    ),
    tags$div(
      style = "text-align:center;",
      tags$a(href = "https://alexfwall.github.io/Palaeomancer/",
             target = "_blank",
             class = "btn",
             style = "white-space:nowrap; background-color:#58B62C; color:white; border:none; font-size:1.2em; padding:0.5em 1.5em;",
             "Palaeomancer"),
      tags$div("Creator's site", style = "font-size:0.65em; opacity:0.7; margin-top:0.2em;")
    )
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#58B62C"
  ),

  sidebar = sidebar(
    title = NULL,
    width = 320,

    h4("1. Upload File"),
    tags$ul(class = "text-muted small", style = "padding-left:1.2em;",
      tags$li("Decimancer will append standardised decimal degree values in two new columns."),
      tags$li("Header row required."),
      tags$li("Mixed formats permitted."),
      tags$li(actionLink("load_example", "Try an example dataset"))
    ),
    fileInput("file_upload",
              label = NULL,
              accept = c(".csv", ".xlsx", ".xls"),
              placeholder = "CSV or Excel file"),

    h4("2. Select Columns"),
    p("Auto-detected columns are pre-selected. Change if needed.",
      class = "text-muted small"),
    selectInput("lat_col", "Latitude Column", choices = NULL),
    selectInput("lon_col", "Longitude Column", choices = NULL),

    h4("3. Convert"),
    actionButton("convert_btn", "Convert Coordinates",
                 class = "btn-primary btn-lg w-100 mt-2"),

    hr(),

    h4("4. Download Result"),
    radioButtons("download_format", "Format:",
                 choices = c("CSV" = "csv", "Excel" = "xlsx"),
                 selected = "csv", inline = TRUE),
    downloadButton("download_btn", "Download Converted File",
                   class = "btn w-100",
                   style = "background-color:#58B62C; color:white; border:none;")
  ),

  # Google Fonts + custom CSS
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Rosarivo:ital@0;1&display=swap",
              rel = "stylesheet"),
    tags$style(HTML(
      ".navbar-brand { flex-grow: 1; margin-right: 0 !important; }
       table.dataTable tbody td {
         white-space: nowrap;
         overflow: hidden;
         text-overflow: ellipsis;
         max-width: 300px;
       }"
    ))
  ),

  layout_columns(
    col_widths = 12,

    card(
      card_header(uiOutput("data_preview_header")),
      card_body(
        uiOutput("data_preview_ui"),
        min_height = "250px"
      )
    ),

    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Conversion Summary",
        div(
          actionButton("toggle_cols", "Show all columns",
                       class = "btn-outline-secondary btn-sm me-2"),
          actionButton("filter_failed", "Show failed rows",
                       class = "btn-outline-danger btn-sm",
                       style = "display:none;")
        )
      ),
      card_body(
        uiOutput("conversion_summary")
      )
    ),

    card(
      card_header("Converted Data Preview"),
      card_body(
        DTOutput("result_preview"),
        min_height = "250px"
      )
    )
  )
)

# -- Server --------------------------------------------------------------------
server <- function(input, output, session) {

  # Track toggle states — coord-only view is the default
  show_coord_only <- reactiveVal(TRUE)
  show_failed_only <- reactiveVal(FALSE)

  # ---------- Data Preview card: textarea vs DT --------------------------------

  output$data_preview_header <- renderUI({
    if (is.null(uploaded_data())) "Quick Convert" else "Data Preview"
  })

  output$data_preview_ui <- renderUI({
    if (is.null(uploaded_data())) {
      textAreaInput("quick_text",
                    label = NULL,
                    placeholder = paste0(
                      "Type or paste coordinates here, one per line\u2026\n",
                      "Examples:\n",
                      "  33\u00B051'54\"S, 151\u00B012'36\"E\n",
                      "  33 51 54 S\n",
                      "  -33.865\n",
                      "  33.865S / 151.21E"),
                    rows = 8,
                    width = "100%")
    } else {
      DTOutput("data_preview_dt")
    }
  })

  # ---------- Quick-convert reactive ------------------------------------------

  quick_convert_data <- reactive({
    req(input$quick_text)
    text <- input$quick_text
    if (nchar(trimws(text)) == 0) return(NULL)

    lines <- strsplit(text, "\n")[[1]]
    lines <- lines[nchar(trimws(lines)) > 0]
    if (length(lines) == 0) return(NULL)

    results <- lapply(lines, parse_coord_line)
    data.frame(
      Input        = vapply(results, `[[`, character(1), "input"),
      Latitude_DD  = vapply(results, `[[`, numeric(1),   "lat"),
      Longitude_DD = vapply(results, `[[`, numeric(1),   "lon"),
      stringsAsFactors = FALSE
    )
  })

  # ---------- Data source (file upload OR example) -----------------------------

  uploaded_data <- reactiveVal(NULL)
  uploaded_name <- reactiveVal(NULL)

  observeEvent(input$file_upload, {
    ext <- tools::file_ext(input$file_upload$name)
    df <- NULL
    if (ext == "csv") {
      df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE,
                     check.names = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      df <- as.data.frame(readxl::read_excel(input$file_upload$datapath),
                          stringsAsFactors = FALSE)
    } else {
      showNotification("Unsupported file type. Please upload CSV or Excel.",
                       type = "error")
      return()
    }
    uploaded_data(df)
    uploaded_name(input$file_upload$name)
  })

  observeEvent(input$load_example, {
    df <- read.csv("www/example_australian_towns.csv",
                   stringsAsFactors = FALSE, check.names = FALSE)
    uploaded_data(df)
    uploaded_name("example_australian_towns.csv")
  })

  # Update column selectors on upload
  observeEvent(uploaded_data(), {
    df <- uploaded_data()
    req(df)
    cols <- names(df)
    detected <- detect_coord_columns(df)

    updateSelectInput(session, "lat_col",
                      choices = cols,
                      selected = if (!is.null(detected$lat_col)) detected$lat_col else cols[1])
    updateSelectInput(session, "lon_col",
                      choices = cols,
                      selected = if (!is.null(detected$lon_col)) detected$lon_col else cols[min(2, length(cols))])
  })

  # File data preview (rendered when file is uploaded)
  output$data_preview_dt <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data(),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })

  # ---------- File conversion (button-triggered) ------------------------------

  converted_data <- eventReactive(input$convert_btn, {
    df <- uploaded_data()
    req(df, input$lat_col, input$lon_col)

    lat_raw <- as.character(df[[input$lat_col]])
    lon_raw <- as.character(df[[input$lon_col]])

    df$Latitude_DD  <- validate_latitudes(parse_coordinates(lat_raw))
    df$Longitude_DD <- validate_longitudes(parse_coordinates(lon_raw))

    # Reset toggles on new conversion (coord-only is the default)
    show_coord_only(TRUE)
    show_failed_only(FALSE)
    updateActionButton(session, "toggle_cols", label = "Show all columns")
    updateActionButton(session, "filter_failed", label = "Show failed rows")

    df
  })

  # Toggle: coord columns only (file mode only)
  observeEvent(input$toggle_cols, {
    req(converted_data())
    current <- show_coord_only()
    show_coord_only(!current)
    updateActionButton(session, "toggle_cols",
                       label = if (!current) "Show all columns" else "Show coord columns only")
  })

  # Toggle: failed rows only (file mode only)
  observeEvent(input$filter_failed, {
    req(converted_data())
    current <- show_failed_only()
    show_failed_only(!current)
    updateActionButton(session, "filter_failed",
                       label = if (!current) "Show all rows" else "Show failed rows")
  })

  # ---------- Conversion Summary (handles both modes) -------------------------

  output$conversion_summary <- renderUI({
    if (is.null(uploaded_data())) {
      # -- Quick convert mode --
      df <- quick_convert_data()
      if (is.null(df)) {
        return(tagList(
          tags$script(HTML("$('#filter_failed').hide(); $('#toggle_cols').hide();")),
          tags$p(class = "text-muted", "Type coordinates above to see results.")
        ))
      }

      n_total    <- nrow(df)
      n_lat_ok   <- sum(!is.na(df$Latitude_DD))
      n_lon_ok   <- sum(!is.na(df$Longitude_DD))
      n_lat_fail <- n_total - n_lat_ok
      n_lon_fail <- n_total - n_lon_ok

      return(tagList(
        tags$script(HTML("$('#filter_failed').hide(); $('#toggle_cols').hide();")),
        tags$p(tags$strong("Lines parsed: "), n_total),
        tags$p(tags$strong("Latitude: "),
               tags$span(style = "color:green;", n_lat_ok, " parsed"),
               if (n_lat_fail > 0) tagList(", ", tags$span(style = "color:red;", n_lat_fail, " failed"))),
        tags$p(tags$strong("Longitude: "),
               tags$span(style = "color:green;", n_lon_ok, " parsed"),
               if (n_lon_fail > 0) tagList(", ", tags$span(style = "color:red;", n_lon_fail, " failed")))
      ))
    }

    # -- File mode --
    df <- converted_data()
    req(df)

    n_total    <- nrow(df)
    n_lat_ok   <- sum(!is.na(df$Latitude_DD))
    n_lon_ok   <- sum(!is.na(df$Longitude_DD))
    n_lat_fail <- n_total - n_lat_ok
    n_lon_fail <- n_total - n_lon_ok
    has_failures <- n_lat_fail > 0 || n_lon_fail > 0

    if (has_failures) {
      shinyjs_show <- "$('#filter_failed').show();"
    } else {
      shinyjs_show <- "$('#filter_failed').hide();"
    }

    tagList(
      tags$script(HTML(paste0(shinyjs_show, " $('#toggle_cols').show();"))),
      tags$p(tags$strong("Total rows: "), n_total),
      tags$p(tags$strong("Latitude: "),
             tags$span(style = "color:green;", n_lat_ok, " parsed"),
             if (n_lat_fail > 0) tagList(", ", tags$span(style = "color:red;", n_lat_fail, " failed"))),
      tags$p(tags$strong("Longitude: "),
             tags$span(style = "color:green;", n_lon_ok, " parsed"),
             if (n_lon_fail > 0) tagList(", ", tags$span(style = "color:red;", n_lon_fail, " failed"))),
      if (has_failures)
        tags$p(tags$em("Use the \"Show failed rows\" button above to inspect failures."),
               style = "color: #856404;")
    )
  })

  # ---------- Result Preview (handles both modes) -----------------------------

  output$result_preview <- renderDT({
    if (is.null(uploaded_data())) {
      # -- Quick convert mode --
      df <- quick_convert_data()
      req(df)

      df[] <- lapply(df, function(col) {
        col[is.na(col)] <- "NA"
        col
      })

      dt <- datatable(df,
                      options = list(pageLength = 10, scrollX = TRUE),
                      rownames = FALSE)

      if ("Latitude_DD" %in% names(df) && "Longitude_DD" %in% names(df)) {
        dt <- dt |> formatStyle(c("Latitude_DD", "Longitude_DD"),
                                backgroundColor = "#d4edda")
      }
      return(dt)
    }

    # -- File mode --
    df <- converted_data()
    req(df)

    # Filter to failed rows if toggled
    if (show_failed_only()) {
      df <- df[is.na(df$Latitude_DD) | is.na(df$Longitude_DD), , drop = FALSE]
    }

    # Select columns if toggled
    if (show_coord_only()) {
      coord_cols <- c(input$lat_col, input$lon_col, "Latitude_DD", "Longitude_DD")
      coord_cols <- coord_cols[coord_cols %in% names(df)]
      df <- df[, coord_cols, drop = FALSE]
    }

    # Replace NA with "NA" string so empty cells are visible in the table
    df[] <- lapply(df, function(col) {
      col[is.na(col)] <- "NA"
      col
    })

    dt <- datatable(df,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)

    # Highlight the DD columns if they're visible
    if ("Latitude_DD" %in% names(df) && "Longitude_DD" %in% names(df)) {
      dt <- dt |> formatStyle(c("Latitude_DD", "Longitude_DD"),
                              backgroundColor = "#d4edda")
    }
    dt
  })

  # ---------- Download handler (file mode only) -------------------------------

  output$download_btn <- downloadHandler(
    filename = function() {
      original_name <- tools::file_path_sans_ext(uploaded_name() %||% "decimancer_output")
      ext <- input$download_format
      paste0(original_name, "_converted.", ext)
    },
    content = function(file) {
      df <- converted_data()
      if (input$download_format == "csv") {
        write.csv(df, file, row.names = FALSE)
      } else {
        writexl::write_xlsx(df, file)
      }
    }
  )
}

shinyApp(ui, server)
