library(shiny)
library(data.table)

# Helper functions from the original script
get_days_in_month <- function(year, month) {
    if (month == 2) {
        if ((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)) {
            return(29)
        } else {
            return(28)
        }
    } else if (month %in% c(4, 6, 9, 11)) {
        return(30)
    } else {
        return(31)
    }
}

validate_date <- function(year, month, day) {
    if (month < 1 || month > 12) return(FALSE)
    max_days <- get_days_in_month(year, month)
    if (day < 1 || day > max_days) return(FALSE)
    return(TRUE)
}

# Data processing functions
download_weather_data <- function(station_code, date_info) {
    base_url <- "https://www.for.gov.bc.ca/ftp/HPR/external/!publish/BCWS_DATA_MART/"
    all_data <- data.table()
    
    if (date_info$type == "exact_dates") {
        all_data <- process_date_range(base_url, date_info$start_date, date_info$end_date, station_code)
    } else {
        year <- if(date_info$type == "single_year") date_info$year else NULL
        start_year <- if(date_info$type == "year_range") date_info$start_year else year
        end_year <- if(date_info$type == "year_range") date_info$end_year else year
        
        for (current_year in start_year:end_year) {
            year_data <- try_consolidated_file(base_url, current_year, station_code)
            
            if (is.null(year_data)) {
                start_date <- sprintf("%d-01-01", current_year)
                end_date <- sprintf("%d-12-31", current_year)
                year_data <- process_date_range(base_url, start_date, end_date, station_code)
            }
            
            if (!is.null(year_data) && nrow(year_data) > 0) {
                all_data <- rbindlist(list(all_data, year_data), fill = TRUE)
            }
        }
    }
    
    if (nrow(all_data) == 0) {
        return(NULL)
    }
    
    # Reformat DATE_TIME column
    all_data[, DATE_TIME := sprintf("%s-%s-%s %s:00",
                                   substr(DATE_TIME, 1, 4),    # Year
                                   substr(DATE_TIME, 5, 6),    # Month
                                   substr(DATE_TIME, 7, 8),    # Day
                                   substr(DATE_TIME, 9, 10))]  # Hour
    
    return(all_data)
}

try_consolidated_file <- function(base_url, year, station_code) {
    # This function is a placeholder for trying to download consolidated files
    # In the original script this functionality was implied but not implemented
    return(NULL)
}

process_date_range <- function(base_url, start_date, end_date, station_code) {
    all_data <- data.table()
    files_processed <- 0
    
    start_parts <- as.numeric(strsplit(start_date, "-")[[1]])
    end_parts <- as.numeric(strsplit(end_date, "-")[[1]])
    
    current_date <- list(year = start_parts[1], month = start_parts[2], day = start_parts[3])
    end_date_list <- list(year = end_parts[1], month = end_parts[2], day = end_parts[3])
    
    while(TRUE) {
        if (!validate_date(current_date$year, current_date$month, current_date$day)) {
            break
        }
        
        filename <- sprintf("%d-%02d-%02d.csv", 
                          current_date$year, current_date$month, current_date$day)
        file_url <- paste0(base_url, current_date$year, "/", filename)
        
        tryCatch({
            temp_data <- fread(file_url)
            temp_data <- temp_data[STATION_CODE == station_code]
            
            if (nrow(temp_data) > 0) {
                all_data <- rbindlist(list(all_data, temp_data), fill = TRUE)
                files_processed <- files_processed + 1
            }
        }, error = function(e) {
            # Silently continue on error
        })
        
        # Check if we've reached the end date
        if (current_date$year == end_date_list$year && 
            current_date$month == end_date_list$month && 
            current_date$day == end_date_list$day) {
            break
        }
        
        # Increment date
        current_date$day <- current_date$day + 1
        if (current_date$day > get_days_in_month(current_date$year, current_date$month)) {
            current_date$day <- 1
            current_date$month <- current_date$month + 1
            if (current_date$month > 12) {
                current_date$month <- 1
                current_date$year <- current_date$year + 1
            }
        }
    }
    
    if (files_processed == 0) {
        return(NULL)
    }
    
    return(all_data)
}

# UI Definition
ui <- fluidPage(
    titlePanel("BC Weather Data Downloader"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("station_code", "Station Code:", ""),
            
            radioButtons("date_option", "Select Date Range:", 
                         choices = list("Single Year" = 1, 
                                        "Range of Years" = 2, 
                                        "Exact Date Range" = 3),
                         selected = 1),
            
            # Single Year
            conditionalPanel(
                condition = "input.date_option == 1",
                numericInput("single_year", "Year:", 2023, min = 1987, max = 2025)
            ),
            
            # Year Range
            conditionalPanel(
                condition = "input.date_option == 2",
                numericInput("start_year", "Start Year:", 2020, min = 1987, max = 2025),
                numericInput("end_year", "End Year:", 2023, min = 1987, max = 2025)
            ),
            
            # Exact Date Range
            conditionalPanel(
                condition = "input.date_option == 3",
                dateInput("start_date", "Start Date:", value = Sys.Date() - 30),
                dateInput("end_date", "End Date:", value = Sys.Date())
            ),
            
            radioButtons("filter_type", "Data Frequency:", 
                         choices = list("Hourly Observations" = "hourlies", 
                                        "Daily Observations (noon readings)" = "dailies"),
                         selected = "hourlies"),
            
            actionButton("download_btn", "Download Data", class = "btn-primary")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Status", 
                         verbatimTextOutput("status_output"),
                         textOutput("data_summary")),
                tabPanel("Data Preview", 
                         dataTableOutput("data_preview")),
                tabPanel("Download",
                         uiOutput("download_ui"))
            )
        )
    )
)

# Server logic
server <- function(input, output, session) {
    # Reactive values to store state
    values <- reactiveValues(
        data = NULL,
        status_messages = "",
        filename = NULL,
        processing = FALSE
    )
    
    # Function to add status messages
    add_status <- function(msg) {
        values$status_messages <- paste0(values$status_messages, msg, "\n")
    }
    
    # Main download process
    observeEvent(input$download_btn, {
        values$processing <- TRUE
        values$data <- NULL
        values$status_messages <- ""
        values$filename <- NULL
        
        # Validate inputs
        if (trimws(input$station_code) == "") {
            add_status("Error: Station code cannot be empty.")
            values$processing <- FALSE
            return()
        }
        
        # Prepare date info based on selection
        date_info <- list()
        if (input$date_option == "1") {
            date_info$type <- "single_year"
            date_info$year <- input$single_year
            add_status(paste("Processing single year:", date_info$year))
        } else if (input$date_option == "2") {
            if (input$start_year > input$end_year) {
                add_status("Error: Start year must be before end year.")
                values$processing <- FALSE
                return()
            }
            date_info$type <- "year_range"
            date_info$start_year <- input$start_year
            date_info$end_year <- input$end_year
            add_status(paste("Processing year range:", date_info$start_year, "to", date_info$end_year))
        } else {
            start_date <- format(input$start_date, "%Y-%m-%d")
            end_date <- format(input$end_date, "%Y-%m-%d")
            
            if (start_date > end_date) {
                add_status("Error: Start date must be before end date.")
                values$processing <- FALSE
                return()
            }
            
            date_info$type <- "exact_dates"
            date_info$start_date <- start_date
            date_info$end_date <- end_date
            add_status(paste("Processing date range:", date_info$start_date, "to", date_info$end_date))
        }
        
        # Download data
        add_status("Downloading weather data...")
        tryCatch({
            data <- download_weather_data(input$station_code, date_info)
            
            if (is.null(data) || nrow(data) == 0) {
                add_status("No data found for the specified station and time period.")
                values$processing <- FALSE
                return()
            }
            
            # Filter if needed
            if (input$filter_type == "dailies") {
                add_status("Filtering for noon (12:00) observations only...")
                data <- data[grepl(" 12:00$", DATE_TIME)]
                
                if (nrow(data) == 0) {
                    add_status("No noon observations found in the dataset")
                    values$processing <- FALSE
                    return()
                }
            }
            
            # Store data and create filename
            values$data <- data
            station_name <- unique(data$STATION_NAME)[1]
            
            if (date_info$type == "exact_dates") {
                values$filename <- sprintf("%s_%s_to_%s_BCWS_WX_OBS%s.csv",
                                        station_name,
                                        date_info$start_date,
                                        date_info$end_date,
                                        if(input$filter_type == "dailies") "_dailies" else "")
            } else if (date_info$type == "year_range") {
                values$filename <- sprintf("%s_%d-%d_BCWS_WX_OBS%s.csv",
                                        station_name,
                                        date_info$start_year,
                                        date_info$end_year,
                                        if(input$filter_type == "dailies") "_dailies" else "")
            } else {
                values$filename <- sprintf("%s_%d_BCWS_WX_OBS%s.csv",
                                        station_name,
                                        date_info$year,
                                        if(input$filter_type == "dailies") "_dailies" else "")
            }
            
            add_status("Processing complete!")
            add_status(paste("Number of observations:", nrow(data)))
            
        }, error = function(e) {
            add_status(paste("Error:", e$message))
        })
        
        values$processing <- FALSE
    })
    
    # Output status updates
    output$status_output <- renderText({
        values$status_messages
    })
    
    # Data summary
    output$data_summary <- renderText({
        if (!is.null(values$data)) {
            paste("Data loaded successfully with", nrow(values$data), "observations.")
        }
    })
    
    # Data preview
    output$data_preview <- renderDataTable({
        if (!is.null(values$data)) {
            head(values$data, 50)
        }
    })
    
    # Download UI
    output$download_ui <- renderUI({
        if (!is.null(values$data) && !is.null(values$filename)) {
            downloadButton("download_file", "Download CSV File")
        } else {
            p("No data available for download yet. Please download data first.")
        }
    })
    
    # Download handler
    output$download_file <- downloadHandler(
        filename = function() {
            values$filename
        },
        content = function(file) {
            fwrite(values$data, file)
        }
    )
}

# Run the app
shinyApp(ui = ui, server = server)
