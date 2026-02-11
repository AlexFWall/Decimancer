library(shiny)
library(bslib)
library(readxl)
library(writexl)
library(DT)

source("R/coord_parser.R")
source("R/column_detector.R")

# -- UI -----------------------------------------------------------------------
ui <- page_sidebar(
  title = "Decimancer",
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
                   class = "btn-success w-100")
  ),

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
      card_header("Conversion Summary"),
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

    df
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

    tagList(
      tags$p(tags$strong("Total rows: "), n_total),
      tags$p(tags$strong("Latitude parsed: "),
             n_lat_ok, " success, ",
             tags$span(style = if (n_lat_fail > 0) "color:red;" else "",
                       n_lat_fail, " failed")),
      tags$p(tags$strong("Longitude parsed: "),
             n_lon_ok, " success, ",
             tags$span(style = if (n_lon_fail > 0) "color:red;" else "",
                       n_lon_fail, " failed")),
      if (n_lat_fail > 0 || n_lon_fail > 0)
        tags$p(tags$em("Rows that could not be parsed will have NA in the _DD columns."),
               style = "color: #856404;")
    )
  })

  # Converted data preview
  output$result_preview <- renderDT({
    df <- converted_data()
    req(df)
    datatable(df,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) |>
      formatStyle(c("Latitude_DD", "Longitude_DD"),
                  backgroundColor = "#d4edda")
  })

  # Download handler
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
