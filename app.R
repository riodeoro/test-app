library(shiny)
library(xml2)
library(DT)

ui <- fluidPage(
  titlePanel("Test app"),
  
  verbatimTextOutput("connectionStatus"),
  
  h4("Site Data"),
  DTOutput("siteTable"),
  
  h4("Raw Response"),
  verbatimTextOutput("rawResponse")
)

server <- function(input, output, session) {
  fetchData <- function() {
    # Get API key from environment variables
    system_key <- Sys.getenv("API_SYSTEM_KEY")
    
    if (system_key == "") {
      return(list(
        status = "Error: API_SYSTEM_KEY environment variable not set",
        data = NULL
      ))
    }
    
    api_url <- paste0(
      "https://el-086-api.elements360.aem.eco/aem/DataAPI?method=GetSiteMetaData&system_key=",
      system_key,
      "&format=xml"
    )
    
    tryCatch({
      xml_content <- readLines(url(api_url))
      xml_text <- paste(xml_content, collapse = "\n")
      
      return(list(
        status = "Connected successfully",
        data = xml_text
      ))
    }, error = function(e) {
      return(list(
        status = paste("Error connecting to API:", e$message),
        data = NULL
      ))
    })
  }
  
  api_data <- reactiveVal(NULL)
  
  observe({
    result <- fetchData()
    api_data(result)
  })
  
  
  output$connectionStatus <- renderText({
    result <- api_data()
    if (is.null(result)) return("Connecting...")
    return(result$status)
  })
  
  # Display raw response
  output$rawResponse <- renderText({
    result <- api_data()
    if (is.null(result) || is.null(result$data)) return("No data available")
    return(result$data)
  })
  
  output$siteTable <- renderDT({
    result <- api_data()
    
    if (is.null(result) || is.null(result$data)) {
      return(datatable(
        data.frame(message = "No data available."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    xml_data <- tryCatch({
      read_xml(result$data)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(xml_data)) {
      return(datatable(
        data.frame(message = "Error parsing XML response"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    rows <- xml_find_all(xml_data, "//row")
    
    if (length(rows) == 0) {
      return(datatable(
        data.frame(message = "No site data found in the response"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    safe_xml_text <- function(node, xpath) {
      result <- tryCatch({
        node_found <- xml_find_first(node, xpath)
        if (length(node_found) > 0) xml_text(node_found) else NA_character_
      }, error = function(e) {
        NA_character_
      })
      return(result)
    }
    
    site_data <- do.call(rbind, lapply(rows, function(row) {
      data.frame(
        or_site_id = safe_xml_text(row, "./or_site_id"),
        site_id = safe_xml_text(row, "./site_id"),
        location = safe_xml_text(row, "./location"),
        owner = safe_xml_text(row, "./owner"),
        system_id = safe_xml_text(row, "./system_id"),
        latitude = safe_xml_text(row, "./latitude_dec"),
        longitude = safe_xml_text(row, "./longitude_dec"),
        elevation = safe_xml_text(row, "./elevation"),
        stringsAsFactors = FALSE
      )
    }))
    
    datatable(
      site_data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)
