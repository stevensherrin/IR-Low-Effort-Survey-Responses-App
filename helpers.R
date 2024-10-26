# Load necessary libraries
#library(tidyverse)
library(openxlsx) #YES
library(writexl) #Yes
library(dplyr) #YES
library(tidyr) #YES
library(ggplot2) #YES
library(purrr) #YES
library(janitor) #YES
library(gtools) #YES
library(conflicted) #YES
library(reactable) #YES
library(scales) #YES
library(crosstalk) #YES
library(htmltools) #Yes

conflicts_prefer(base::as.numeric())
conflicts_prefer(base::is.character())
conflicts_prefer(base::`&&`)
conflicts_prefer(base::`||`)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

done <- 0

# List of required variables
required_vars <- c(
  "askquest", "drafts", "unprepared", "attendart",
  "CLaskhelp", "CLexplain", "CLstudy", "CLproject", "present",
  "RIintegrate", "RIsocietal", "RIdiverse", "RIownview", "RIperspect",
  "RInewview", "RIconnect", "SFcareer", "SFotherwork", "SFdiscuss",
  "SFperform", "memorize", "HOapply", "HOanalyze", "HOevaluate",
  "HOform", "ETgoals", "ETorganize", "ETexample", "ETdraftfb",
  "ETfeedback", "QRconclude", "QRproblem", "QRevaluate", 
  "DDrace", "DDeconomic", "DDreligion", "DDpolitical",
  "LSreading", "LSnotes", "LSsummary",
  "challenge", "intern", "leader", "learncom", "abroad",
  "research", "capstone", "servcourse",
  "QIstudent", "QIadvisor", "QIfaculty", "QIstaff", "QIadmin",
  "empstudy", "SEacademic", "SElearnsup", "SEdiverse", "SEsocial", "SEwellness", "SEnonacad", "SEactivities", "SEevents",
  "pgwrite", "pgwspeak","pgthink", "pganalyze", "pgwork", "pgothers", "pgvalues", "pgdiverse", "pgprobsolve", "pgcitizen",
  "tmprephrs","tmcocurrhrs", "tmworkonhrs", "tmworkoffhrs", "tmservicehrs",
  "tmrelaxhrs", "tmcarehrs", "tmcommutehrs", "duration"
)

# Function to check required variables
check_missing_vars <- function(df) {
  df_columns <- colnames(df)
  missing_vars <- setdiff(required_vars, df_columns)
  
  if (length(missing_vars) > 0) {
    print(paste("The following columns are missing from your raw NSSE file:", 
                paste(missing_vars, collapse = ", "), 
                ". Some of this may be due to analyzing a particular year from NSSE, especially older years. Keep in mind this may prevent the analysis from running properly."))
  } else {
    print("All required columns are present.")
  }
  
  return(missing_vars)
}

# Function to remove recoded and estimated variables
remove_recoded_vars <- function(df) {
  recoded_vars <- c("unpreparedr", "wrshortnum", "wrmednum", "wrlongnum", "wrpages",
                    "HIPsumFY", "HIPsumSR", "QIstudentR", "QIadvisorR", "QIfacultyR",
                    "QIstaffR", "QIadminR", "tmprep", "tmcocurr", "tmworkon", "tmworkoff",
                    "tmworkhrs", "tmservice", "tmrelax", "tmcare", "tmcommute", "tmread",
                    "reading", "tmreadinghrscol", "wrshort", "wrmed", "wrlong",
                    "wrshortnum", "wrmednum", "wrlongnum", "wrpages")
  
  df <- df %>% select(-any_of(recoded_vars))
  print("Flagged recoded and/or estimated variables not used in analyses.")
  
  return(df)
}

# Function to read the uploaded file
read_uploaded_file <- function(file) {
  ext <- tools::file_ext(file$name)
  
  if (ext == "csv") {
    df <- read.csv(file$datapath)
  } else if (ext == "xlsx") {
    df <- openxlsx::read.xlsx(file$datapath)
  } else if (ext == "sav") {
    df <- haven::read_sav(file$datapath)
    df <- as.data.frame(df)
  } else {
    stop("Invalid file type. Please upload a .csv, .xlsx, or .sav file.")
  }
  
  assign("df", df, envir = .GlobalEnv)
  return(df)
}

# Function to calculate longstring
calculate_longstring_ratio <- function(df, set_name, set_columns) {
  # Check if all columns exist in the dataframe
  existing_columns <- intersect(set_columns, colnames(df))
  
  if (length(existing_columns) > 0) {
    # Calculate longstring only for existing columns
    df[[paste0(set_name, "_longstring")]] <- as.numeric(
      longstring(df[, existing_columns, drop = FALSE]) / 
        rowSums(!is.na(df[, existing_columns, drop = FALSE]))
    )
  } else {
    # If no columns exist, set ratio to 0
    df[[paste0(set_name, "_longstring")]] <- 0
  }
  
  return(df)
}

# Function to calculate repeated patterns
calculate_repeated_pattern_percentage <- function(data, lag) {
  # Handle non-numeric columns
  data_numeric <- data %>%
    select_if(function(x) is.numeric(x) | all(grepl("^[0-9]+$", na.omit(x))))
  
  # Convert remaining character columns to numeric if possible
  data_numeric <- data_numeric %>%
    mutate(across(everything(), as.numeric))
  
  # Calculate the number of matches with lagged values
  matches <- rowSums(data_numeric == lag(data_numeric, n = lag, default = NA), na.rm = TRUE)
  
  # Calculate total non-NA values
  total_values <- rowSums(!is.na(data_numeric))
  
  # Calculate percentage
  percentage <- (matches / total_values) * 100
  
  return(percentage)
}

# Function to clean and prepare the dataframe
clean_data <- function(df) {
  df <- clean_names(df)
  print("Column names successfully cleaned")
  
  na_or_empty_names <- which(is.na(names(df)) | names(df) == "")
  
  if (length(na_or_empty_names) > 0) {
    cat("Columns with NA or empty names:", na_or_empty_names, "\n")
    names(df)[na_or_empty_names] <- paste("V", na_or_empty_names, sep = "")
  } else {
    print("No columns with NA or empty names")
  }
  
  df <- df %>% mutate(unique_id = paste0(row_number()))
  print("Unique identifier for each row created")
  
  assign("df", df, envir = .GlobalEnv)
  return(df)
}

# Function to identify careless responses
identify_careless_responses <- function(df) {
  global_flags <- list()
  
  df_main_page <- df %>%
    select(unique_id, duration, askquest:s_eevents) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.))))
  
  first_page_ids <- df_main_page$unique_id
  
  # Step 1: Screen low duration
  df_main_page$duration <- as.numeric(df_main_page$duration)
  global_flags$step_1 <- sum(df_main_page$duration <= 3 & !is.na(df_main_page$duration))
  
  step_1 <- df_main_page %>%
    filter(duration > 3 | is.na(duration)) %>%
    select(-duration)
  
  print("Step 1 completed")
  
  # Step 2: Screen for high number of missing values
  global_flags$step_2 <- sum(rowSums(is.na(df_main_page %>% select(-unique_id, -duration))) >= 15, na.rm = TRUE)
  step_2 <- step_1 %>%
    filter(rowSums(is.na(step_1)) < 15)
  print("Step 2 completed")
  
  # Step 3: Screen for longstrings of 15 or more
  df_main_page$longstring <- longstring(df_main_page %>% select(-unique_id, -duration))
  global_flags$step_3 <- sum(df_main_page$longstring >= 15, na.rm = TRUE)
  
  step_3 <- step_2 %>%
    mutate(longstring = longstring(step_2)) 
  
  step_3_values <- step_3 %>%
    select(unique_id, longstring, everything())
  
  step_3 <- step_3 %>%
    filter(longstring < 15)
  print("Step 3 completed")
  
  # Step 4: Screen for repeated longstrings of more than 6
  df_main_page$longstring_7_or_more <- longstring_n_times(df_main_page %>% select(-unique_id, -duration), threshold = 6)$count_longstr
  global_flags$step_4 <- sum(df_main_page$longstring_7_or_more >= 3, na.rm = TRUE)
  
  step_4 <- step_3 %>%
    mutate(longstring_7_or_more = longstring_n_times(step_3, threshold = 6)$count_longstr) 
  
  step_4_values <- step_4 %>%
    select(unique_id, longstring_7_or_more, everything())
  
  step_4 <- step_4 %>%
    filter(longstring_7_or_more < 3)
  print("Step 4 completed")
  
  # Step 5: Screen for respondents who answer 3 or more scales with 0 variance
  question_sets <- list(
    q_set_1 = c("askquest", "c_laskhelp", "c_lexplain", "c_lstudy", "c_lproject", "present"),
    q_set_2 = c("r_iintegrate", "r_isocietal", "r_idiverse", "r_iownview", "r_iperspect", "r_inewview", "r_iconnect"),
    q_set_3 = c("memorize", "h_oapply", "h_oanalyze", "h_oevaluate", "h_oform"),
    q_set_4 = c("e_tgoals", "e_torganize", "e_texample", "e_tdraftfb", "e_tfeedback", "e_tcriteria", "e_treview", "e_tprefer", "e_tdemonstrate"),
    q_set_5 = c("d_drace", "d_deconomic", "d_dreligion", "d_dpolitical", "d_dsexorient", "d_dcountry"),
    q_set_6 = c("intern", "leader", "learncom", "abroad", "research", "capstone", "servcourse"),
    q_set_7 = c("empstudy", "s_eacademic", "s_elearnsup", "s_ediverse", "s_esocial", "s_ewellness", "s_enonacad", "s_eactivities", "s_eevents"),
    q_set_8 = c("tmprephrs", "tmcocurrhrs", "tmworkonhrs", "tmworkoffhrs", "tmservicehrs", "tmrelaxhrs", "tmcarehrs", "tmcommutehrs")
  )
  
  vars_in_question_sets <- unique(unlist(question_sets))
  
  # Calculate global flag for step 5 using full dataset
  df_main_scales <- df_main_page
  for (i in 1:length(question_sets)) {
    set_name <- paste0("q_set_", i)
    set_columns <- question_sets[[set_name]]
    df_main_scales <- calculate_longstring_ratio(df_main_scales, set_name, set_columns)
  }
  df_main_scales <- df_main_scales %>%
    mutate(longstring_3_or_more_scales = rowSums(select(., starts_with("q_set_")) == 1))
  global_flags$step_5 <- sum(df_main_scales$longstring_3_or_more_scales >= 3, na.rm = TRUE)
  
  step_4$unique_id <- as.character(step_4$unique_id)
  df$unique_id <- as.character(df$unique_id)
  
  step_5 <- step_4 %>%
    left_join(df %>% select(unique_id, any_of(vars_in_question_sets)), by = "unique_id", suffix = c("", ".df")) %>%
    select(-ends_with(".df"))
  
  for (i in 1:length(question_sets)) {
    set_name <- paste0("q_set_", i)
    set_columns <- question_sets[[set_name]]
    step_5 <- calculate_longstring_ratio(step_5, set_name, set_columns)
  }
  
  step_5 <- step_5 %>%
    mutate(longstring_3_or_more_scales = rowSums(select(., starts_with("q_set_")) == 1)) 
  
  step_5_values <- step_5 %>%
    select(unique_id, longstring_3_or_more_scales)
  
  step_5 <- step_5 %>%
    filter(longstring_3_or_more_scales < 3)
  print("Step 5 completed")
  
  # Step 6: Screen for 2-value repetitive pattern
  # Calculate global flag using full dataset
  df_main_patterns <- df_main_page %>%
    select(-duration) %>%
    mutate(
      rp_2 = calculate_repeated_pattern_percentage(across(everything()), 2),
      rp_3 = calculate_repeated_pattern_percentage(across(everything()), 3),
      rp_4 = calculate_repeated_pattern_percentage(across(everything()), 4),
      rp_5 = calculate_repeated_pattern_percentage(across(everything()), 5)
    ) %>%
    mutate(repetitive = ifelse(rowSums(select(., starts_with("rp_")) >= 60) > 0, 1, 0))
  global_flags$step_6 <- sum(df_main_patterns$repetitive == 1, na.rm = TRUE)
  
  step_6 <- df %>%
    select(unique_id, askquest:s_eevents) %>%
    filter(unique_id %in% step_5$unique_id) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.))))
  
  step_6 <- step_6 %>%
    mutate(rp_2 = calculate_repeated_pattern_percentage(across(everything()), 2),
           rp_3 = calculate_repeated_pattern_percentage(across(everything()), 3),
           rp_4 = calculate_repeated_pattern_percentage(across(everything()), 4),
           rp_5 = calculate_repeated_pattern_percentage(across(everything()), 5))
  
  step_6 <- step_6 %>%
    mutate(repetitive = ifelse(rowSums(select(., starts_with("rp_")) >= 60) > 0, 1, 0))
  
  step_6_values <- step_6 %>%
    select(unique_id, repetitive, contains("rp_"), everything())
  
  step_6 <- step_6 %>%
    filter(repetitive == 0 | is.na(repetitive) == TRUE)
  print("Step 6 completed")
  
  # Step 7: Screen for invalid responses to "hours" question set
  # Calculate global flag using full dataset
  df_hours_all <- df %>%
    select(unique_id, tmprephrs:tmcommutehrs) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
    mutate(total_hours_per_week = rowSums(select(., -unique_id), na.rm = TRUE))
  global_flags$step_7 <- sum(df_hours_all$total_hours_per_week > 140, na.rm = TRUE)
  
  df_hours <- df %>%
    select(unique_id, tmprephrs:tmcommutehrs) %>%
    filter(unique_id %in% step_6$unique_id) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.))))
  
  df_hours$total_hours_per_week <- rowSums(select(df_hours, -unique_id), na.rm = TRUE)
  
  step_7 <- step_6 %>%
    left_join(df_hours, by = "unique_id")
  
  step_7_values <- step_7 %>%
    select(unique_id, total_hours_per_week, contains("tm"))
  
  step_7 <- step_7 %>%
    mutate(total_hours_per_week = ifelse(is.na(total_hours_per_week), 0, total_hours_per_week)) %>%
    filter(total_hours_per_week <= 140)
  print("Step 7 completed")
  
  # Step 8: Screen for unusual responses to highly correlated items
  # Calculate global flag using full dataset
  df_main_corr <- df %>%
    select(unique_id, askquest:sameinst) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
    select(where(~!all(is.na(.)))) %>%
    mutate(
      mv_syn = psychsyn(select(., -unique_id), critval = .50),
      mv_mahad = scale(mahad(select(., -unique_id)))
    )
  global_flags$step_8 <- sum(df_main_corr$mv_mahad >= 3 | df_main_corr$mv_syn <= 0, na.rm = TRUE)
  
  step_8 <- df %>%
    select(unique_id, askquest:sameinst) %>%
    filter(unique_id %in% step_7$unique_id) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
    select(where(~!all(is.na(.))))
  
  step_8_filtered <- step_8 %>%
    select(-unique_id)
  
  step_8_filtered <- step_8_filtered %>%
    mutate(across(everything(), scale))
  
  step_8$mv_syn <- psychsyn(step_8_filtered, critval = .50)
  step_8$mv_mahad <- scale(mahad(step_8_filtered))
  
  step_8 <- step_8 %>%
    filter(mv_mahad < 3 & mv_syn > 0)
  
  print("Step 8 completed")
  
  # Rest of the function remains exactly the same
  steps <- list(df, step_1, step_2, step_3, step_4, step_5, step_6, step_7, step_8)
  
  # Initialize an empty vector to store the changes
  row_changes <- vector("numeric", length = length(steps) - 1)
  
  # Loop through the dataframes to calculate the changes in number of rows
  for (i in 1:(length(steps) - 1)) {
    row_changes[i] <- nrow(steps[[i + 1]]) - nrow(steps[[i]])
  }
  
  assign("row_changes", row_changes, envir = .GlobalEnv)
  assign("global_flags", global_flags, envir = .GlobalEnv)
  
  # Find last step each respondent was included in
  last_occurrence <- data.frame(unique_id = unique(df$unique_id), last_dataframe = NA)
  
  for (i in seq_along(last_occurrence$unique_id)) {
    unique_id <- last_occurrence$unique_id[i]
    for (j in length(steps):1) {
      if (unique_id %in% steps[[j]]$unique_id) {
        last_occurrence$last_dataframe[i] <- paste("step", j-1, sep = "_")
        break
      }
    }
  }
  
  last_occurrence <- last_occurrence %>%
    mutate(last_dataframe = recode(last_dataframe, 
                                   `step_0` = "Completed survey in 3 minutes or less",
                                   `step_1` = "Skipped over 25% of survey questions",
                                   `step_2` = "Straightlined 15 or more responses in a row",
                                   `step_3` = "Had 3 or more times they straightlined least 7 responses in a row",
                                   `step_4` = "Straightlined 3 or more scales",
                                   `step_5` = "Made repetitive pattern (e.g. AB-AB-AB) 60% or more of the time",
                                   `step_6` = "Unrealistic responses to quantitative question set (e.g. hours per week)",
                                   `step_7` = "Highly unusual responses to highly correlated items",
                                   `step_8` = "")) %>%
    rename(`Reason for Flag` = last_dataframe)
  
  last_occurrence$`Flagged for Low Effort?` <- ifelse(last_occurrence$`Reason for Flag` == "", "No", "Yes")
  
  last_occurrence <- last_occurrence %>%
    select(unique_id, `Flagged for Low Effort?`, `Reason for Flag`)
  
  df <- df %>%
    left_join(last_occurrence, by = "unique_id") %>%
    select(`Flagged for Low Effort?`, `Reason for Flag`, unique_id, everything())
  
  assign("df", df, envir = .GlobalEnv)
  assign("step_1", step_1, envir = .GlobalEnv)
  assign("step_2", step_2, envir = .GlobalEnv)
  assign("step_3", step_3, envir = .GlobalEnv)
  assign("step_4", step_4, envir = .GlobalEnv)
  assign("step_5", step_5, envir = .GlobalEnv)
  assign("step_6", step_6, envir = .GlobalEnv)
  assign("step_7", step_7, envir = .GlobalEnv)
         assign("df", df, envir = .GlobalEnv)
         assign("step_1", step_1, envir = .GlobalEnv)
         assign("step_2", step_2, envir = .GlobalEnv)
         assign("step_3", step_3, envir = .GlobalEnv)
         assign("step_4", step_4, envir = .GlobalEnv)
         assign("step_5", step_5, envir = .GlobalEnv)
         assign("step_6", step_6, envir = .GlobalEnv)
         assign("step_7", step_7, envir = .GlobalEnv)
         assign("step_8", step_8, envir = .GlobalEnv)
         
         assign("step_3_values", step_3_values, envir = .GlobalEnv)
         assign("step_4_values", step_4_values, envir = .GlobalEnv)
         assign("step_5_values", step_5_values, envir = .GlobalEnv)
         assign("step_6_values", step_6_values, envir = .GlobalEnv)
         assign("step_7_values", step_7_values, envir = .GlobalEnv)
         
         # Add global flag columns
         df <- df %>%
           mutate(
             `Flag: Duration <= 3 min` = unique_id %in% df_main_page$unique_id[df_main_page$duration <= 3 & !is.na(df_main_page$duration)],
             `Flag: >25% Missing` = unique_id %in% df_main_page$unique_id[rowSums(is.na(df_main_page %>% select(-unique_id, -duration))) >= 15],
             `Flag: Longstring >= 15` = unique_id %in% df_main_page$unique_id[df_main_page$longstring >= 15],
             `Flag: Repeated Longstring >= 7` = unique_id %in% df_main_page$unique_id[df_main_page$longstring_7_or_more >= 3],
             `Flag: 3+ Scales Straightlined` = unique_id %in% df_main_scales$unique_id[df_main_scales$longstring_3_or_more_scales >= 3],
             `Flag: Repetitive Pattern` = unique_id %in% df_main_patterns$unique_id[df_main_patterns$repetitive == 1],
             `Flag: Hours > 140` = unique_id %in% df_hours_all$unique_id[df_hours_all$total_hours_per_week > 140],
             `Flag: Unusual Correlations` = unique_id %in% df_main_corr$unique_id[df_main_corr$mv_mahad >= 3 | df_main_corr$mv_syn <= 0]
           ) %>%
           # Convert logical to Yes/No
           mutate(across(starts_with("Flag:"), ~ifelse(., "Yes", "No"))) %>%
           # Reorder columns to put flags after the existing flag columns
           select(
             #`Row in Dataset` = unique_id,
             `Flagged for Low Effort?`,
             `Primary Reason for Flag` = `Reason for Flag`,
             starts_with("Flag:"),
             everything()
           )
         return(df)
}

# Function to process the data for summary table
calculate_summary <- function(df) {
  # First get all unique respondents flagged at each step
  flagged_respondents <- list()
  for(i in 1:8) {
    step_name <- paste0("step_", i)
    current_step <- get(step_name, envir = .GlobalEnv)
    previous_step <- if(i == 1) df else get(paste0("step_", i-1), envir = .GlobalEnv)
    
    # Get IDs that were in previous step but not in current step
    flagged_respondents[[i]] <- setdiff(previous_step$unique_id, current_step$unique_id)
  }
  
  # In calculate_summary function:

  # Start with the initial summary dataframe
  summary_df <- tibble(
    `Step Description` = c(
      "Initial Number of Survey Responses",  # New first row
      "Raw Dataset",
      "Survey completed in 3 minutes or less", 
      "Skipped over 25% of survey questions",
      "Straightlined 15 or more responses in a row",
      "Had 3 or more times they straightlined at least 7 responses in a row",
      "Straightlined 3 or more scales",
      "Made repetitive pattern (e.g. AB-AB-AB) 60% or more of the time",
      "Unrealistic response to quantitative question (e.g. hours per week)",
      "Highly unusual responses to highly correlated items"),
    `Remaining Valid Responses` = c(
      nrow(df),  # Starting count
      nrow(df),
      nrow(df) - length(flagged_respondents[[1]]),
      nrow(df) - length(unique(unlist(flagged_respondents[1:2]))),
      nrow(df) - length(unique(unlist(flagged_respondents[1:3]))),
      nrow(df) - length(unique(unlist(flagged_respondents[1:4]))),
      nrow(df) - length(unique(unlist(flagged_respondents[1:5]))),
      nrow(df) - length(unique(unlist(flagged_respondents[1:6]))),
      nrow(df) - length(unique(unlist(flagged_respondents[1:7]))),
      nrow(df) - length(unique(unlist(flagged_respondents[1:8])))
    ),
    `Responses Filtered in this Step` = c(
      NA_integer_,  # Blank for first row
      0,
      length(flagged_respondents[[1]]),
      length(flagged_respondents[[2]]),
      length(flagged_respondents[[3]]),
      length(flagged_respondents[[4]]),
      length(flagged_respondents[[5]]),
      length(flagged_respondents[[6]]),
      length(flagged_respondents[[7]]),
      length(flagged_respondents[[8]])
    ),
    `Percent of All Responses Filtered in this Step` = c(
      NA_real_,  # Blank for first row
      0,
      length(flagged_respondents[[1]]) / nrow(df),
      length(flagged_respondents[[2]]) / nrow(df),
      length(flagged_respondents[[3]]) / nrow(df),
      length(flagged_respondents[[4]]) / nrow(df),
      length(flagged_respondents[[5]]) / nrow(df),
      length(flagged_respondents[[6]]) / nrow(df),
      length(flagged_respondents[[7]]) / nrow(df),
      length(flagged_respondents[[8]]) / nrow(df)
    ),
    `Total Responses Flagged for this Issue` = c(
      NA_integer_,  # Blank for first row
      0,
      global_flags$step_1,
      global_flags$step_2,
      global_flags$step_3,
      global_flags$step_4,
      global_flags$step_5,
      global_flags$step_6,
      global_flags$step_7,
      global_flags$step_8
    ),
    `Percent of All Responses with this Issue` = scales::percent(c(
      NA_real_,  # Blank for first row
      0,
      global_flags$step_1 / nrow(df),
      global_flags$step_2 / nrow(df),
      global_flags$step_3 / nrow(df),
      global_flags$step_4 / nrow(df),
      global_flags$step_5 / nrow(df),
      global_flags$step_6 / nrow(df),
      global_flags$step_7 / nrow(df),
      global_flags$step_8 / nrow(df)
    ), accuracy = 0.1)
  )

  # Number the steps (skip first row)
  summary_df$`Step Description`[3:nrow(summary_df)] <- paste0(
    "Step ", 1:(nrow(summary_df)-2), ": ", 
    summary_df$`Step Description`[3:nrow(summary_df)]
  )

  # Add final summary row
  all_flagged_ids <- unique(unlist(flagged_respondents))
  total_unique_flagged <- length(all_flagged_ids)
  total_unique_percentage <- total_unique_flagged / nrow(df)

  summary_df <- summary_df %>%
    bind_rows(
      tibble(
        `Step Description` = "Final Summary of All Steps",
        `Remaining Valid Responses` = nrow(df) - total_unique_flagged,
        `Responses Filtered in this Step` = total_unique_flagged,
        `Percent of All Responses Filtered in this Step` = total_unique_percentage,
        `Total Responses Flagged for this Issue` = NA_integer_,
        `Percent of All Responses with this Issue` = NA_character_
      )
    )

  assign("summary_df", summary_df, envir = .GlobalEnv)
  return(summary_df)
}
# Format summary table
format_percentage_cell <- function(value) {
  value <- min(max(value, 0), 1)
  gradientValue <- scales::rescale(value, c(0, 1), c(0, 1))
  color <- colorRampPalette(c("white", "red"))(100)[as.integer(gradientValue * 99) + 1]
  style <- paste("background-color:", color, "; color: black;")
  htmltools::span(style = style, scales::percent(value, accuracy = 0.1))
}

# Function to process individual examples based on selected view
process_individual_examples <- function(df, view_select) {
  if (view_select == "Completed survey in 3 minutes or less") {
    step_1_filtered <- df %>%
      select(unique_id, duration) %>%
      mutate(duration = round(as.numeric(duration), 1)) %>%
      arrange(duration) %>%
      rename(
        `Row in Dataset` = unique_id,
        `Duration (minutes)` = duration
      )
    
    assign("step_1_filtered", step_1_filtered, envir = .GlobalEnv)
    return(step_1_filtered)
    
  } else if (view_select == "Skipped over 25% of survey questions") {
    step_2_filtered <- step_1 %>%
      mutate(missing_percentage = round(rowSums(is.na(.)) / ncol(.) * 100, 1)) %>%
      select(missing_percentage, unique_id, everything()) %>%
      arrange(desc(missing_percentage)) %>%
      rename(
        `Row in Dataset` = unique_id,
        `Percentage of Missing Values` = missing_percentage
      )
    
    assign("step_2_filtered", step_2_filtered, envir = .GlobalEnv)
    return(step_2_filtered)
  } else if (view_select == "Straightlined 15 Responses") {
    step_3_values <- step_3_values %>%
      arrange(desc(longstring)) %>%
      rename(`Row in Dataset` = unique_id,
             `Straightline Length` = longstring)
    
    assign("step_3_values", step_3_values, envir = .GlobalEnv)
    return(step_3_values)
  } else if (view_select == "Repeated Straightline Behavior") {
    step_4_values <- merge(step_4_values, step_5_values, by = "unique_id", all.x = TRUE) %>%
      arrange(desc(longstring_7_or_more)) %>%
      rename(`Row in Dataset` = unique_id,
             `# of Times Straightlined 7 or More Responses` = longstring_7_or_more,
             `# of Subscales Straightlined` = longstring_3_or_more_scales) %>%
      select(`Row in Dataset`, 
             `# of Times Straightlined 7 or More Responses`, 
             `# of Subscales Straightlined`,
             everything()) %>%
      select(-longstring)
    
    assign("step_4_values", step_4_values, envir = .GlobalEnv)
    return(step_4_values)
  } else if (view_select == "Other Repetitive Behavior") {
    step_6_values <- step_6_values %>%
      arrange(desc(repetitive), desc(rp_2)) %>%
      rename(`Row in Dataset` = unique_id,
             `Flagged for Repetitive Behavior` = repetitive,
             `% of Times Previous Response Repeated` = rp_2,
             `% of Times 2nd Previous Response Repeated` = rp_3,
             `% of Times 3rd Previous Response Repeated` = rp_4,
             `% of Times 4th Previous Response Repeated` = rp_5) %>%
      mutate(across(contains("%"), ~ . / 100)) %>%
      mutate(across(contains("%"), ~ scales::percent(., accuracy = 0.1)))
    
    assign("step_6_values", step_6_values, envir = .GlobalEnv)
    return(step_6_values)
  } else if (view_select == "Extreme Responses to `How many hours per week?` Questions") {
      step_7_values <- df %>%
        select(unique_id, tmprephrs:tmcommutehrs) %>%
        mutate(across(-unique_id, ~as.numeric(as.character(.)))) %>%
        mutate(`Total Hours per Week` = rowSums(select(., -unique_id), na.rm = TRUE)) %>%
        mutate(`Flagged for Unrealistic Hours?` = ifelse(`Total Hours per Week` > 140, "Yes", "No")) %>%
        mutate(`Total Hours per Week` = ifelse(is.na(`Total Hours per Week`), 0, `Total Hours per Week`)) %>%
        arrange(desc(`Total Hours per Week`)) %>%
        rename(`Row in Dataset` = unique_id) %>%
        # Reorder columns to put total and flag first
        select(
          `Row in Dataset`,
          `Total Hours per Week`,
          `Flagged for Unrealistic Hours?`,
          everything()
        )
      
      assign("step_7_values", step_7_values, envir = .GlobalEnv)
      return(step_7_values)
  }
}