library(shiny)
library(dplyr)
library(reactable)
library(bslib)
library(haven)

# Source the helper functions
source("helpers.R")
source("library_functions.R")

# Downloading issue in Chrome. Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# UI definition
ui <- fluidPage(
  theme = bslib::bs_theme(
    primary = "#007BFF",
    secondary = "#6C757D"
  ),
  tags$head(
    tags$style(HTML("
      .shiny-input-container {  
        font-family: 'IBM Plex Sans', sans-serif;
      }
      .icon {
        font-size: 24px;
        vertical-align: middle;
        margin-right: 10px;
      }
      .footer {
        font-size: 12px;
        color: #6C757D;
        margin-top: 20px;
        text-align: right;
      }
      .header {
        display: flex;
        align-items: center;
        margin-bottom: 20px;
      }
      .header img {
        margin-right: 10px;
      }
      .safety-icon {
        font-size: 24px;
        color: #28a745;
        vertical-align: middle;
        margin-right: 10px;
      }
      .overview-icon {
        font-size: 24px;
        color: #007BFF;
        vertical-align: middle;
        margin-right: 10px;
      }
      .main-title {
        font-size: 36px;
        font-weight: bold;
        text-align: left;
        margin-bottom: 20px;
      }
    "))
  ),
  
  # Main header with the title
  div(
    class = "header",
    div(
      style = "
      background: #007BFF;
      padding: 20px;
      border-radius: 10px;
      display: inline-block;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    ",
      div(
        style = "display: flex; align-items: center; gap: 15px;",
        # Icon container
        span(
          style = "
          font-size: 32px;
          background: white;
          color: #007BFF;
          width: 50px;
          height: 50px;
          display: flex;
          align-items: center;
          justify-content: center;
          border-radius: 10px;
        ",
          "📊"
        ),
        # Title text
        h1(
          style = "
          font-family: 'IBM Plex Sans', sans-serif;
          color: white;
          font-weight: bold;
          font-size: 28px;
          margin: 0;
          line-height: 1.2;
        ",
          "Detecting Low-Effort",
          br(),
          "IR Survey Responses")
        )
      )
    ),
  
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("surveySelect", "Select your survey", 
                  choices = c("Click here for options" = "Click here for options",
                              "NSSE" = "National Survey of Student Engagement (NSSE)")),
      fileInput("file", "Upload the raw data file (e.g. .csv, .xlsx):"),
      actionButton("run", "Run")#,
      #downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 div(
                   style = "padding: 20px; max-width: 1000px; margin: 0 auto;",
                   
                   # Analysis Complete Message (when present)
                   uiOutput("analysisCompleteOverview"),
                   
                   # Welcome Section
                   div(
                     style = "
    background: #f8f9fa;
    color: #2C3E50;
    padding: 25px;
    border: 1px solid #e0e0e0;
    border-radius: 10px;
    margin-bottom: 30px;
  ",
                     h2(style = "margin: 0 0 15px 0; font-size: 24px; color: #007BFF;", 
                        "Welcome to the IR Low-Effort Survey Response Detector")
                   ),
                   
                   # Main Features
                   div(
                     style = "
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
        gap: 20px;
        margin-bottom: 30px;
      ",
                     
                     # What It Does
                     div(
                       style = "
          background: white;
          padding: 20px;
          border-radius: 10px;
          border: 1px solid #e0e0e0;
        ",
                       div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                           span(class = "overview-icon", style = "font-size: 24px;", "📊"),
                           h3(style = "color: #007BFF; margin: 0 0 0 10px;", "What It Does")
                       ),
                       p(style = "margin: 0; line-height: 1.6;",
                         "Automatically screens your IR survey responses for behavior that might indicate low-effort responses.")
                     ),
                     
                     # Data Privacy
                     div(
                       style = "
    background: white;
    padding: 20px;
    border-radius: 10px;
    border: 1px solid #e0e0e0;
  ",
                       div(
                         style = "display: flex; align-items: center; margin-bottom: 15px;",
                         span(class = "safety-icon", style = "font-size: 24px;", "🔒"),
                         h3(style = "color: #007BFF; margin: 0 0 0 10px;", "Data Privacy")
                       ),
                       p(
                         style = "margin: 0; line-height: 1.6;",
                         "Your data stays on your computer. Using ", 
                         a(
                           href = "https://posit-dev.github.io/r-shinylive/",
                           target = "_blank",
                           "ShinyLive",
                           style = "color: #007BFF; text-decoration: none;"
                         ),
                         ", a new tool for the R language, everything runs locally in your browser with no external servers involved."
                       )
                     )),
                   
                   # Getting Started
                   div(
                     style = "
        background: white;
        padding: 25px;
        border-radius: 10px;
        border: 1px solid #e0e0e0;
      ",
                     div(style = "display: flex; align-items: center; margin-bottom: 15px;",
                         span(style = "font-size: 24px;", "🚀"),
                         h3(style = "color: #007BFF; margin: 0 0 0 10px;", "Getting Started")
                     ),
                     div(
                       style = "
          background: #f8f9fa;
          padding: 20px;
          border-radius: 8px;
          line-height: 1.6;
        ",
                       HTML("
          <ol style='margin: 0; padding-left: 20px;'>
            <li>Select your survey type from the dropdown menu</li>
            <li>Upload your raw data file (e.g. .csv, .xlsx, .sav format)</li>
            <li>Click 'Run' to begin the analysis</li>
          </ol>
        ")
                     )
                   )
                 )
        
        ),
        tabPanel("Summary",
                 tags$div(
                   tags$br(),
                   
                   # Analysis Summary at the top
                   tags$div(
                     style = "background-color: #e8f4f8; padding: 20px; border-radius: 5px; margin-bottom: 30px;",
                     tags$h4(style = "color: #2C3E50; margin-bottom: 15px;", 
                             "Analysis Summary"),
                     tags$p(style = "font-size: 16px; line-height: 1.6;", 
                            uiOutput("summaryText"))
                   ),
                   
                   # Add the React component here
                   tags$div(
                     id = "summary-explanation",
                     tags$div(class = "summary-explanation-container")
                   ),
                   
                   # Summary Table
                   tags$h3(style = "font-weight: bold;", "Summary Table"),
                   reactableOutput("summaryTable")
                 )
        ),
        
        
        tabPanel("Explore Data",
                 selectInput("dataViewSelect", "See Low-Effort Responses for:",
                             choices = c(
                               "Filter 1: Survey Duration" = "Completed survey in 3 minutes or less",
                               "Filter 2: Skipped Questions" = "Skipped over 25% of survey questions",
                               "Filter 3: Straightlined 15 Responses" = "Straightlined 15 Responses",
                               "Filters 4 & 5: Repeated Straightline Behavior" = "Repeated Straightline Behavior",
                               "Filter 6: Other Repetitive Behavior" = "Other Repetitive Behavior",
                               "Filter 7: Hours Questions" = "Extreme Responses to `How many hours per week?` Questions"  # Keep this exact text
                             )
                 ),
                 HTML("<p><i>This table indicates which responses were flagged for this particular issue, suggesting a low-effort response.</i></p>"),
                 reactableOutput("individualExamplesTable")
        ),
        tabPanel("Download Data",
                 div(
                   style = "padding: 20px; max-width: 1200px; margin: 0 auto;",
                   
                   # Download Section at Top
                   div(
                     style = "
        background: #007BFF;
        color: white;
        padding: 25px;
        border-radius: 10px;
        margin-bottom: 30px;
        text-align: center;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      ",
                     h2(style = "margin: 0 0 15px 0; font-size: 24px;", "Download Your Analysis Results"),
                     downloadButton("downloadData", "Download Excel File", 
                                    style = "
          font-size: 16px;
          padding: 12px 24px;
          background-color: white;
          color: #007BFF;
          border: none;
          font-weight: bold;
        "
                     ),
                     p(style = "margin: 15px 0 0 0; font-size: 14px; opacity: 0.9;",
                       "File will be saved as 'nsse_survey_low_effort_responses_[DATE].xlsx'")
                   ),
                   
                   # What's Included Section - Simplified
                   div(
                     style = "
        background: white;
        padding: 25px;
        border-radius: 10px;
        border: 1px solid #e0e0e0;
      ",
                     h3(style = "color: #007BFF; margin-top: 0;", "What's Included in Your Download"),
                     
                     div(
                       style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin-top: 15px;",
                       
                       # About Sheet
                       div(
                         style = "padding: 15px; background: #f8f9fa; border-radius: 8px;",
                         h4(style = "color: #666; margin: 0 0 10px 0;", "📝 About Sheet"),
                         p(style = "margin: 0; color: #666;", "Overview of file contents and interpretation guide")
                       ),
                       
                       # Summary Sheet
                       div(
                         style = "padding: 15px; background: #f8f9fa; border-radius: 8px;",
                         h4(style = "color: #666; margin: 0 0 10px 0;", "📊 Summary Sheet"),
                         p(style = "margin: 0; color: #666;", "Analysis results showing flagged responses at each step")
                       ),
                       
                       # Data Sheet
                       div(
                         style = "padding: 15px; background: #f8f9fa; border-radius: 8px;",
                         h4(style = "color: #666; margin: 0 0 10px 0;", "💾 Data Sheet"),
                         p(style = "margin: 0; color: #666;", "Your dataset with new columns indicating flagged responses")
                     )
                   )
                 ))
        
        ),
        tabPanel("About",
                 tabPanel("About",
                          div(
                            style = "padding: 20px; max-width: 1000px; margin: 0 auto;",
                            
                            # Mission Statement
            
                            
                            # Main Content
                            div(
                              style = "
        background: white;
        padding: 25px;
        border-radius: 10px;
        border: 1px solid #e0e0e0;
        margin-bottom: 30px;
      ",
                              h3(style = "color: #007BFF; margin-top: 0;", "How It Works"),
                              p(style = "line-height: 1.6;",
                                "Our tool employs a sequential approach to identify respondents whose behavior may indicate low-effort responses. Responses pass through filters one by one, with flagged responses excluded from subsequent steps. The filters are prioritized based on the severity of the violation, starting with the most serious issues."),
                              
                              div(
                                style = "
          background: #f8f9fa;
          padding: 20px;
          border-radius: 8px;
          margin: 15px 0;
        ",
                                HTML("
          <p style='margin: 0 0 15px 0;'><strong>Methodology:</strong></p>
          <ol style='margin: 0; padding-left: 20px; line-height: 1.6;'>
            <li>Each step focuses on a specific pattern of low-effort responding.</li>
            <li>Flagged responses are removed before proceeding to the next check.</li>
          </ol>
        ")
                              )
                            ),
                            
                            # Additional Resources
                            div(
                              style = "
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        gap: 20px;
        margin-bottom: 30px;
      ",
                              
                              # Documentation Link
                              div(
                                style = "
          background: #f8f9fa;
          padding: 20px;
          border-radius: 8px;
          border: 1px solid #e0e0e0;
        ",
                                h4(style = "color: #007BFF; margin-top: 0;", "📚 Documentation"),
                                p(style = "line-height: 1.6;", 
                                  "For detailed methodology and research background, visit our ",
                                  a(href = "https://github.com/stevensherrin/IR-Low-Effort-Survey-Responses-App/tree/main/supporting%20materials",
                                    target = "_blank",
                                    "documentation page", 
                                    style = "color: #007BFF; text-decoration: none;")
                                )
                              ),
                              
                              # Source Code Link
                              div(
                                style = "
          background: #f8f9fa;
          padding: 20px;
          border-radius: 8px;
          border: 1px solid #e0e0e0;
        ",
                                h4(style = "color: #007BFF; margin-top: 0;", "💻 Source Code"),
                                p(style = "line-height: 1.6;",
                                  "View and contribute to the code on ",
                                  a(href = "https://github.com/stevensherrin",
                                    target = "_blank",
                                    "GitHub", 
                                    style = "color: #007BFF; text-decoration: none;"),
                                  ". Open source for non-commercial use."
                                )
                              )
                            ),
                            
                            # Credits Footer
                            div(
                              style = "
        background: #f8f9fa;
        padding: 20px;
        border-radius: 8px;
        text-align: center;
        color: #666;
      ",
                              p(style = "margin: 0;",
                                "Developed by ",
                                a(href = "https://www.linkedin.com/in/steven-sherrin",
                                  target = "_blank",
                                  "Steven Sherrin", 
                                  style = "color: #007BFF; text-decoration: none;"),
                                " (Wentworth Institute of Technology)"
        )
                            ))))
        
        
      )
    )
  ),
  div(class = "footer",
      HTML("<p>This dashboard was created by <a href='https://www.linkedin.com/in/steven-sherrin' target='_blank'>Steven Sherrin</a>. Research was conducted by <a href='https://www.linkedin.com/in/ingerbergom' target='_blank'>Inger Bergom</a> (Harvard University) and <a href='https://www.linkedin.com/in/steven-sherrin' target='_blank'>Steven Sherrin</a> (Wentworth Institute of Technology).</p>")
  )
)

# Server logic
server <- function(input, output, session) {
  data <- reactiveVal()
  
  # Add popups after survey selection
  observeEvent(input$surveySelect, {
    if (input$surveySelect == "National Survey of Student Engagement (NSSE)") {
      showModal(modalDialog(
        title = "NSSE Data Upload Instructions",
        "For the National Survey of Student Engagement (NSSE), you can upload the raw data file in either .sav (SPSS), .csv, or .xlsx format. IMPORTANT: Make sure to upload the raw data file exactly as it was sent by NSSE (i.e. no edits).",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
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
      showModal(modalDialog(
        title = "Note About Missing Variables",
        HTML(paste0(
          "<p>The following variables are not present in your data: <br><br>",
          "<strong>", paste(missing_vars, collapse = ", "), "</strong></p>",
          "<p>This is common and may occur for a couple reasons:</p>",
          "<ul>",
          "<li>You're using data from a historical version of the survey when these questions weren't included</li>",
          #"<li>Your institution opted out of certain question sets</li>",
          "<li>Variable names changed across survey years</li>",
          "</ul>",
          "<p>The analysis will still run, but some checks may be limited to the variables that are present in your data.</p>"
        )),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  observeEvent(input$run, {
    req(input$file)
    
    # Remove recoded or estimated variables not used in the analysis
    df <- remove_recoded_vars(df)
    
    # Clean the uploaded data
    df <- clean_data(df)
    
    # Identify careless responses
    df <- identify_careless_responses(df)
    
    # Process the summary table data
    summary_df <- calculate_summary(df)
    data(summary_df)
    
    # Let user know analysis is complete
    output$analysisCompleteOverview <- renderUI({
      HTML('
    <div style="
      background: #007BFF;
      color: white;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      margin: 20px 0;
      animation: fadeInUp 0.5s ease-out;
    ">
      <div style="display: flex; align-items: center;">
        <span style="font-size: 24px; margin-right: 15px;">✅</span>
        <div>
          <h3 style="margin: 0; font-size: 20px; font-weight: bold;">Analysis Successfully Completed!</h3>
          <p style="margin: 10px 0 0 0; font-size: 16px;">Your data has been processed and is ready to review. Here\'s what you can do next:</p>
          <ul style="margin: 10px 0 0 0; padding-left: 20px;">
            <li>Go to <strong>Summary</strong> to see an overview of your results</li>
            <li>Visit <strong>Explore Data</strong> to examine individual responses</li>
            <li>Use <strong>Download Data</strong> to export your analyzed dataset</li>
          </ul>
        </div>
      </div>
    </div>
    <style>
      @keyframes fadeInUp {
        from {
          opacity: 0;
          transform: translateY(20px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
    </style>
  ')
    })
    
    # Update the duplicate message to match
    output$analysisComplete <- renderUI({
      HTML('
    <div style="
      background: #007BFF;
      color: white;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      margin: 20px 0;
      animation: fadeInUp 0.5s ease-out;
    ">
      <div style="display: flex; align-items: center;">
        <span style="font-size: 24px; margin-right: 15px;">✅</span>
        <div>
          <h3 style="margin: 0; font-size: 20px; font-weight: bold;">Analysis Successfully Completed!</h3>
          <p style="margin: 10px 0 0 0; font-size: 16px;">Your data has been processed and is ready to review. Here\'s what you can do next:</p>
          <ul style="margin: 10px 0 0 0; padding-left: 20px;">
            <li>Go to <strong>Summary</strong> to see an overview of your results</li>
            <li>Visit <strong>Explore Data</strong> to examine individual responses</li>
            <li>Use <strong>Download Data</strong> to export your analyzed dataset</li>
          </ul>
        </div>
      </div>
    </div>
    <style>
      @keyframes fadeInUp {
        from {
          opacity: 0;
          transform: translateY(20px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
    </style>
  ')
    })
    
    
    # Render summary text
    output$summaryText <- renderText({
      all_responses <- data()$`Remaining Valid Responses`[1]
      final_valid <- tail(data()$`Remaining Valid Responses`, 1)
      total_removed <- tail(data()$`Responses Filtered in this Step`, 1)
      percent_removed <- total_removed / all_responses * 100
      
      paste0("You started with <b style='text-decoration: underline double;'>", all_responses, 
             "</b> respondents. After searching for several criteria, we flagged <b style='text-decoration: underline double;'>", 
             total_removed, "</b>  responses that potentially indicate low effort. (<b style='text-decoration: underline double;'>", 
             round(percent_removed, 1), "% of total</b>). You now have <b style='text-decoration: underline double;'>", 
             final_valid, "</b> remaining responses. For details on the specific issues found, see the table below. For methodology details, see About.")
    })
    
    
    # Render summary table
    output$summaryTable <- renderReactable({
      req(data())
      
      reactable(
        data(),
        defaultPageSize = nrow(data()), # Show all rows
        columns = list(
          `Step Description` = colDef(
            name = "",
            cell = function(value) {
              div(style = list(fontStyle = "italic"), value)
            },
            headerStyle = list(verticalAlign = "top"),
            minWidth = 350,
            maxWidth = 350
          ),
          `Remaining Valid Responses` = colDef(
            name = "Remaining Valid Responses",
            headerStyle = list(textAlign = "center"),
            style = list(textAlign = "center"),
            minWidth = 150,
            maxWidth = 150
          ),
          `Responses Filtered in this Step` = colDef(
            name = "Responses Filtered in this Step",
            headerStyle = list(textAlign = "center"),
            style = list(textAlign = "center"),
            minWidth = 150,
            maxWidth = 150
          ),
          `Percent of All Responses Filtered in this Step` = colDef(
            name = "Percent of All Responses Filtered in this Step",
            headerStyle = list(textAlign = "center"),
            format = colFormat(percent = TRUE, digits = 1),
            style = list(textAlign = "center"),
            minWidth = 150,
            maxWidth = 150
          ),
          `Total Responses Flagged for this Issue` = colDef(
            name = "Total Responses with this Issue",
            headerStyle = list(textAlign = "center"),
            style = list(textAlign = "center"),
            minWidth = 150,
            maxWidth = 150
          ),
          `Percent of All Responses with this Issue` = colDef(
            name = "Percent of All Responses with this Issue",
            headerStyle = list(textAlign = "center"),
            style = list(textAlign = "center"),
            minWidth = 150,
            maxWidth = 150
          )
        ),
        #defaultSorted = list(Step = "asc"),
        rowStyle = function(index) {
          if (index == 1) {
            list(
              fontWeight = "bold",
              background = "#f7f7f7"
            )
          } else if (index == nrow(data())) {
            list(
              background = "darkred",
              color = "white",
              fontWeight = "bold",
              border = "2px solid #cc0000"
            )
          } else {
            NULL
          }
        }
      )
    })
    
    
    # output$diagram <- renderDiagrammeR({
    #   generate_diagram(summary_df)
    # })
    
    
    # Render Explore Data table
    output$individualExamplesTable <- renderReactable({
      req(data()) # Ensure data is loaded
      
      if (input$dataViewSelect == "Completed survey in 3 minutes or less") {
        process_individual_examples(df, "Completed survey in 3 minutes or less")
        columns <- colnames(step_1_filtered)
        
        column_defs <- setNames(lapply(columns, function(col) {
          if (col == "Duration (minutes)") {
            colDef(
              cell = function(value) {
                if (!is.na(value) && value <= 3) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;", value)
                } else {
                  value
                }
              }
            )
          } else {
            colDef(
              headerStyle = list(textAlign = "center"),
              style = list(textAlign = "center")
            )
          }
        }), columns)
        
        reactable(step_1_filtered, defaultPageSize = 10, columns = column_defs, 
                  style = list(width = "75%")) # Limit the width of the table because it's only two columns
        
      } else if (input$dataViewSelect == "Skipped over 25% of survey questions") {
        process_individual_examples(df, "Skipped over 25% of survey questions")
        columns <- colnames(step_2_filtered) # Get the column names of the data
        
        # Create a named list of column definitions
        column_defs <- setNames(lapply(columns, function(col) {
          colDef(
            cell = function(value) {
              if (col == "Percentage of Missing Values") {
                if (!is.na(value) && value >= 25) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;", value)
                } else {
                  value
                }
              } else {
                if (is.na(value)) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;")
                } else {
                  value
                }
              }
            },
            headerStyle = list(textAlign = "center"),
            style = list(textAlign = "center")
          )
        }), columns)
        
        reactable(step_2_filtered, defaultPageSize = 10, columns = column_defs)
        
      }
      
      else if (input$dataViewSelect == "Straightlined 15 Responses") {
        process_individual_examples(df, "Straightlined 15 Responses")
        columns <- colnames(step_3_values) # Get the column names of the data
        
        # Create a named list of column definitions with specific styling for "Straightline Length" column
        column_defs <- setNames(lapply(columns, function(col) {
          if (col == "Straightline Length") {
            colDef(
              cell = function(value) {
                if (!is.na(value) && value >= 15) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;", value)
                } else {
                  value
                }
              }
            )
          } else {
            colDef(
              headerStyle = list(textAlign = "center"),
              style = list(textAlign = "center")
            )
          }
        }), columns)
        
        reactable(step_3_values, defaultPageSize = 10, columns = column_defs)
      }
      
      else if (input$dataViewSelect == "Repeated Straightline Behavior") {
        process_individual_examples(df, "Repeated Straightline Behavior")
        columns <- colnames(step_4_values) # Get the column names of the data
        
        # Create a named list of column definitions with specific styling for the specified columns
        column_defs <- setNames(lapply(columns, function(col) {
          if (col %in% c("# of Times Straightlined 7 or More Responses", "# of Subscales Straightlined")) {
            colDef(
              cell = function(value) {
                if (!is.na(value) && value >= 3) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;", value)
                } else {
                  value
                }
              }
            )
          } else {
            colDef(
              headerStyle = list(textAlign = "center"),
              style = list(textAlign = "center")
            )
          }
        }), columns)
        
        reactable(step_4_values, defaultPageSize = 10, columns = column_defs)
      }
      
      else if (input$dataViewSelect == "Other Repetitive Behavior") {
        process_individual_examples(df, "Other Repetitive Behavior")
        columns <- colnames(step_6_values) # Get the column names of the data
        
        # Create a named list of column definitions with specific styling for the specified columns
        column_defs <- setNames(lapply(columns, function(col) {
          if (col %in% c("Flagged for Repetitive Behavior")) {
            colDef(
              cell = function(value) {
                if (!is.na(value) && value == 1) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;", value)
                } else {
                  value
                }
              }
            )
          } else {
            colDef(
              headerStyle = list(textAlign = "center"),
              style = list(textAlign = "center")
            )
          }
        }), columns)
        
        reactable(step_6_values, defaultPageSize = 10, columns = column_defs)
      }
      
      else if (input$dataViewSelect == "Extreme Responses to `How many hours per week?` Questions") {
        process_individual_examples(df, "Extreme Responses to `How many hours per week?` Questions")
        columns <- colnames(step_7_values) # Get the column names of the data
        
        # Add some styling for the hours values
        column_defs <- setNames(lapply(columns, function(col) {
          if (col == "Total Hours per Week") {
            colDef(
              cell = function(value) {
                if (!is.na(value) && value > 140) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;", value)
                } else {
                  value
                }
              }
            )
          } else {
            colDef(
              headerStyle = list(textAlign = "center"),
              style = list(textAlign = "center")
            )
          }
        }), columns)
        
        reactable(step_7_values, defaultPageSize = 10, columns = column_defs)
      }
    })
    
    
    # Allow download of processed data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("nsse_survey_low_effort_responses_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        # Create a new workbook
        new_wb <- createWorkbook()
        
        # Define styles
        header_style <- createStyle(textDecoration = "bold", fontSize = 14)
        text_style <- createStyle(fontSize = 12, wrapText = TRUE)
        bullet_style <- createStyle(fontSize = 12, wrapText = TRUE, indent = 1)
        
        # Add "About" sheet with the specified text
        addWorksheet(new_wb, "About")
        
        about_text <- c(
          "This Excel file has your data and results from the 'Detecting Low Effort Surveys' tool. It contains the following sheets:",
          "",
          "1. 'Your Summary': A table displaying the frequency (and percentage) of how often respondents exhibited behaviors indicating low-effort responding.",
          "2. 'Your Data': Your data with additional columns indicating which responses were considered ‘low effort’ based on violating at least one of the criteria examined. These are responses that you may wish to consider removing from future analyses.",
          "",
          "If you have any questions, please contact Steve at stevensherrin@gmail.com with the title 'Detecting Low Effort Surveys'."
        )
        
        # Write the "About" text to the first column
        writeData(new_wb, "About", about_text, startCol = 1, startRow = 1)
        
        # Apply styles to the text
        addStyle(new_wb, "About", header_style, rows = 1, cols = 1)
        addStyle(new_wb, "About", text_style, rows = 2:2, cols = 1, gridExpand = TRUE)
        addStyle(new_wb, "About", bullet_style, rows = 3:5, cols = 1, gridExpand = TRUE)
        addStyle(new_wb, "About", text_style, rows = 6:6, cols = 1, gridExpand = TRUE)
        
        # Adjust column width to fit content
        setColWidths(new_wb, "About", cols = 1, widths = 100)
        
        # Add the summary sheet with data
        addWorksheet(new_wb, "Your Summary")
        writeData(new_wb, "Your Summary", data())
        
        # Autofit the column widths for the "Your Summary" sheet
        for (col in 1:ncol(data())) {
          setColWidths(new_wb, "Your Summary", cols = col, widths = "auto")
        }
        
        # Bold the top row of "Your Summary"
        addStyle(new_wb, "Your Summary", header_style, rows = 1, cols = 1:ncol(data()), gridExpand = TRUE)
        
        # Style the last row of "Your Summary" with black fill and white font
        last_row <- nrow(data()) + 1
        last_row_style <- createStyle(fgFill = "black", fontColour = "white")
        addStyle(new_wb, "Your Summary", last_row_style, rows = last_row, cols = 1:ncol(data()), gridExpand = TRUE)
        
        # Add a data sheet with detailed data
        addWorksheet(new_wb, "Your Data")
        
        df2 <- df
        df2 <- df2 %>% rename(`Row # in Original Data` = unique_id)
        writeData(new_wb, "Your Data", df2)
        
        # Autofit the column widths for the "Your Data" sheet
        for (col in 1:ncol(df)) {
          setColWidths(new_wb, "Your Data", cols = col, widths = "auto")
        }
        
        # Bold the column names of "Your Data"
        addStyle(new_wb, "Your Data", header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
        
        # Apply light blue background to the first two columns of "Your Data"
        light_blue_style <- createStyle(fgFill = "#ADD8E6")
        addStyle(new_wb, "Your Data", light_blue_style, rows = 1:nrow(df) + 1, cols = 1:2, gridExpand = TRUE)
        
        # Save the new workbook
        saveWorkbook(new_wb, file, overwrite = TRUE)
      }
    )
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)