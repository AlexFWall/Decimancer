library(shiny)
library(bslib)
library(readxl)
library(writexl)
library(DT)

source("R/coord_parser.R")
source("R/column_detector.R")

# -- UI -----------------------------------------------------------------------
ui <- page_sidebar(
  title = "Decimancer — convert coordinates to decimal degrees",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#58B62C"
  ),

  sidebar = sidebar(
    title = "Settings",
    width = 320,

    h4("1. Upload File"),
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
                   class = "btn-success w-100"),

    hr(),
    tags$p(
      tags$a(href = "https://alexfwall.github.io/Palaeomancer/",
             target = "_blank",
             "\u2190 Back to Palaeomancer"),
      class = "text-muted small"
    )
  ),

  # Custom CSS for single-line rows
  tags$head(tags$style(HTML(
    "table.dataTable tbody td {
       white-space: nowrap;
       overflow: hidden;
       text-overflow: ellipsis;
       max-width: 300px;
     }"
  ))),

  layout_columns(
    col_widths = 12,

    card(
      card_header("Data Preview"),
      card_body(
        DTOutput("data_preview"),
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

  # Reactive: uploaded data
  uploaded_data <- reactive({
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)

    if (ext == "csv") {
      df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE,
                     check.names = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      df <- as.data.frame(readxl::read_excel(input$file_upload$datapath),
                          stringsAsFactors = FALSE)
    } else {
      showNotification("Unsupported file type. Please upload CSV or Excel.",
                       type = "error")
      return(NULL)
    }
    df
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

  # Uploaded data preview
  output$data_preview <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data(),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })

  # Reactive: conversion result
  converted_data <- eventReactive(input$convert_btn, {
    df <- uploaded_data()
    req(df, input$lat_col, input$lon_col)

    lat_raw <- as.character(df[[input$lat_col]])
    lon_raw <- as.character(df[[input$lon_col]])

    df$Latitude_DD  <- parse_coordinates(lat_raw)
    df$Longitude_DD <- parse_coordinates(lon_raw)

    # Reset toggles on new conversion (coord-only is the default)
    show_coord_only(TRUE)
    show_failed_only(FALSE)
    updateActionButton(session, "toggle_cols", label = "Show all columns")
    updateActionButton(session, "filter_failed", label = "Show failed rows")

    df
  })

  # Toggle: coord columns only
  observeEvent(input$toggle_cols, {
    req(converted_data())
    current <- show_coord_only()
    show_coord_only(!current)
    updateActionButton(session, "toggle_cols",
                       label = if (!current) "Show all columns" else "Show coord columns only")
  })

  # Toggle: failed rows only
  observeEvent(input$filter_failed, {
    req(converted_data())
    current <- show_failed_only()
    show_failed_only(!current)
    updateActionButton(session, "filter_failed",
                       label = if (!current) "Show all rows" else "Show failed rows")
  })

  # Conversion summary
  output$conversion_summary <- renderUI({
    df <- converted_data()
    req(df)

    n_total    <- nrow(df)
    n_lat_ok   <- sum(!is.na(df$Latitude_DD))
    n_lon_ok   <- sum(!is.na(df$Longitude_DD))
    n_lat_fail <- n_total - n_lat_ok
    n_lon_fail <- n_total - n_lon_ok
    has_failures <- n_lat_fail > 0 || n_lon_fail > 0

    # Show/hide the filter button based on whether there are failures
    if (has_failures) {
      shinyjs_show <- "$('#filter_failed').show();"
    } else {
      shinyjs_show <- "$('#filter_failed').hide();"
    }

    tagList(
      tags$script(HTML(shinyjs_show)),
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

  # Converted data preview (responds to both toggles)
  output$result_preview <- renderDT({
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

  # Download handler (always downloads full data, all columns)
  output$download_btn <- downloadHandler(
    filename = function() {
      original_name <- tools::file_path_sans_ext(input$file_upload$name)
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
