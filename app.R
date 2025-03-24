library(shiny)
library(dplyr)
library(reactable)
library(bslib)
library(haven)

# Source the helper functions
source("nsse.R")
source("functions.R")
source("ui.R")

# Server logic
server <- function(input, output, session) {
  data <- reactiveVal()
  careless_results <- reactiveVal()
  
  # Add popups after survey selection
  observeEvent(input$surveySelect, {
    if (input$surveySelect != "Click here for options") {
      show_upload_instructions(session)
    }
    
    # Conditionally source files based on survey selection
    # if (input$surveySelect == "Fake Study") {
    #   source("fake study.R", local = TRUE)
    #   # Optionally, remove the other source from memory if needed
    #   # rm(list = ls(envir = .GlobalEnv, pattern = "nsse"), envir = .GlobalEnv)
  #}
     else if (input$surveySelect == "NSSE") {
      source("nsse.R", local = TRUE)
      # Optionally, remove the other source from memory if needed
      # rm(list = ls(envir = .GlobalEnv, pattern = "fake"), envir = .GlobalEnv)
    }
  })
  
  
  
  observeEvent(input$run, {
    req(input$file)
    
    # Read the uploaded file
    df <- read_uploaded_file(input$file)
    
    # Check for missing variables
    missing_vars <- check_missing_vars(df)
    
    # Show screen if any variables are missing
    if (length(missing_vars) > 0) {
      showModal(show_missing_vars_modal(missing_vars))
    }
  })
  
  observeEvent(input$run, {
    req(input$file)
    
    # Remove recoded or estimated variables not used in the analysis
    df <- remove_recoded_vars(df)
    
    # Clean the uploaded data
    df <- clean_data(df)
    
    # Identify careless responses - now stores results in reactive value
    results <- identify_careless_responses(df)
    careless_results(results)
    
    # Process the summary table data using the new results structure
    summary_df <- calculate_summary(
      results$data, 
      results$flags, 
      results$step_results
    )
    data(summary_df)
    
    # Let user know analysis is complete
    output$analysisCompleteOverview <- renderUI({
      create_analysis_complete_message()
    })
    
    output$analysisComplete <- renderUI({
      create_analysis_complete_message()
    })
    
    output$summaryText <- renderText({
      create_summary_text(data())
    })
    
    output$summaryTable <- renderReactable({
      req(data())
      create_summary_table(data())
    })
    
    get_survey_prefix <- function(survey_selection) {
      switch(survey_selection,
             "National Survey of Student Engagement (NSSE)" = "nsse",
             "survey"
      )
    }
    
    output$downloadData <- downloadHandler(
      filename = function() {
        survey_prefix <- get_survey_prefix(input$surveySelect)
        paste(survey_prefix, "_survey_low_effort_responses_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        results <- careless_results()
        
        new_wb <- createWorkbook()
        header_style <- createStyle(textDecoration = "bold", fontSize = 12)
        text_style <- createStyle(fontSize = 12, wrapText = TRUE)
        bullet_style <- createStyle(fontSize = 12, wrapText = TRUE, indent = 1)
        
        addWorksheet(new_wb, "About")
        about_text <- c(
          "This Excel file has your data and results from the 'Detecting Low Effort Surveys' tool. It contains the following sheets:",
          "",
          "1. 'Your Summary': A table displaying the frequency (and percentage) of how often respondents exhibited behaviors indicating low-effort responding.",
          "2. 'Your Data': Your data with additional columns indicating which responses were considered 'low effort' based on violating at least one of the criteria examined. These are responses that you may wish to consider removing from future analyses.",
          "",
          "If you have any questions, please contact Steve at stevensherrin@gmail.com with the title 'Detecting Low Effort Surveys'."
        )
        writeData(new_wb, "About", about_text, startCol = 1, startRow = 1)
        addStyle(new_wb, "About", header_style, rows = 1, cols = 1)
        addStyle(new_wb, "About", text_style, rows = 2:2, cols = 1, gridExpand = TRUE)
        addStyle(new_wb, "About", bullet_style, rows = 3:5, cols = 1, gridExpand = TRUE)
        addStyle(new_wb, "About", text_style, rows = 6:6, cols = 1, gridExpand = TRUE)
        setColWidths(new_wb, "About", cols = 1, widths = 100)
        
        addWorksheet(new_wb, "Your Summary")
        writeData(new_wb, "Your Summary", data())
        for (col in 1:ncol(data())) {
          setColWidths(new_wb, "Your Summary", cols = col, widths = "auto")
        }
        addStyle(new_wb, "Your Summary", header_style, rows = 1, cols = 1:ncol(data()), gridExpand = TRUE)
        
        last_row <- nrow(data()) + 1
        last_row_style <- createStyle(fgFill = "black", fontColour = "white")
        addStyle(new_wb, "Your Summary", last_row_style, rows = last_row, cols = 1:ncol(data()), gridExpand = TRUE)
        
        addWorksheet(new_wb, "Your Data")
        df2 <- results$data
        df2 <- df2 %>% 
          rename(`Row # in Original Data` = unique_id) %>%
          select(`Row # in Original Data`, everything())
        writeData(new_wb, "Your Data", df2)
        
        saveWorkbook(new_wb, file, overwrite = TRUE)
      }
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
