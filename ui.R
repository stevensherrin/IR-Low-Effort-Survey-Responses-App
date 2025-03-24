library(shiny)
library(bslib)

# Downloading issue in Chrome. Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Function to show missing variables modal
show_missing_vars_modal <- function(missing_vars) {
  modalDialog(
    title = "Note About Missing Variables",
    HTML(paste0(
      "<p>The following variables are not present in your data: <br><br>",
      "<strong>", paste(missing_vars, collapse = ", "), "</strong></p>",
      "<p>This is common and may occur for a couple reasons:</p>",
      "<ul>",
      "<li>You're using data from a historical version of the survey when these questions weren't included</li>",
      "<li>Variable names changed across survey years</li>",
      "</ul>",
      "<p>The analysis will still run, but some checks may be limited to the variables that are present in your data.</p>"
    )),
    easyClose = TRUE,
    footer = modalButton("OK")
  )
}

# Function for analysis complete message
create_analysis_complete_message <- function() {
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
}

# Function for summary text
create_summary_text <- function(data) {
  all_responses <- data$`Remaining Unflagged Responses`[1]
  final_valid <- tail(data$`Remaining Unflagged Responses`, 1)
  total_removed <- tail(data$`Responses Flagged in this Step`, 1)
  percent_removed <- total_removed / all_responses * 100
  
  paste0(
    "You started with <b style='text-decoration: underline double;'>", all_responses, 
    "</b> respondents. After searching for several criteria, we flagged <b style='text-decoration: underline double;'>", 
    total_removed, "</b>  responses that potentially indicate low effort (<b style='text-decoration: underline double;'>", 
    round(percent_removed, 1), "% of total</b>). For details on the specific issues found, see the table below. For methodology details, see About."
  )
}

# Create summary table configuration
create_summary_table <- function(data) {
  reactable(
    data,
    defaultPageSize = nrow(data), # Show all rows
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
      `Remaining Unflagged Responses` = colDef(
        name = "Remaining Unflagged Responses",
        headerStyle = list(textAlign = "center"),
        style = list(textAlign = "center"),
        minWidth = 150,
        maxWidth = 150
      ),
      `Responses Flagged in this Step` = colDef(
        name = "Responses Flagged in this Step",
        headerStyle = list(textAlign = "center"),
        style = list(textAlign = "center"),
        minWidth = 150,
        maxWidth = 150
      ),
      `Percent of All Responses Flagged in this Step` = colDef(
        name = "Percent of All Responses Flagged in this Step",
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
    rowStyle = function(index) {
      if (index == 1) {
        list(
          fontWeight = "bold",
          background = "#f7f7f7"
        )
      } else if (index == nrow(data)) {
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
}

# Function to show missing variables modal
show_missing_vars_modal <- function(missing_vars) {
  modalDialog(
    title = "Note About Missing Variables",
    HTML(paste0(
      "<p>The following variables are not present in your data: <br><br>",
      "<strong>", paste(missing_vars, collapse = ", "), "</strong></p>",
      "<p>This is common and may occur for a couple reasons:</p>",
      "<ul>",
      "<li>You're using data from a historical version of the survey when these questions weren't included</li>",
      "<li>Variable names changed across survey years</li>",
      "</ul>",
      "<p>The analysis will still run, but some checks may be limited to the variables that are present in your data.</p>"
    )),
    easyClose = TRUE,
    footer = modalButton("OK")
  )
}

# Format summary table
format_percentage_cell <- function(value) {
  value <- min(max(value, 0), 1)
  gradientValue <- scales::rescale(value, c(0, 1), c(0, 1))
  color <- colorRampPalette(c("white", "red"))(100)[as.integer(gradientValue * 99) + 1]
  style <- paste("background-color:", color, "; color: black;")
  htmltools::span(style = style, scales::percent(value, accuracy = 0.1))
}

# UI
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
                              #"Fake Study" = "Fake Study",
                              "NSSE" = "National Survey of Student Engagement (NSSE)")),
      fileInput("file", "Upload the raw data file (e.g. .csv, .xlsx):"),
      actionButton("run", "Run")
    ),
    mainPanel(
      tabsetPanel(
        # Overview tab
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
                     )
                   ),
                   
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
        
        # "Download Data" tab
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
                   )
                 )
        ),
        tabPanel("About",
                 div(
                   style = "padding: 20px; max-width: 1000px; margin: 0 auto;",
                   
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
                   )
                 )
        )
      )
    )
  ),
  div(class = "footer",
      HTML("<p>This dashboard was created by <a href='https://www.linkedin.com/in/steven-sherrin' target='_blank'>Steven Sherrin</a>. Research was conducted by <a href='https://www.linkedin.com/in/ingerbergom' target='_blank'>Inger Bergom</a> (Harvard University) and <a href='https://www.linkedin.com/in/steven-sherrin' target='_blank'>Steven Sherrin</a> (Wentworth Institute of Technology).</p>")
  )
)