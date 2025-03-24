# Load necessary libraries
library(openxlsx)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(janitor)
library(gtools)
library(conflicted)
library(reactable)
library(scales)
library(crosstalk)
library(htmltools)

conflicts_prefer(base::as.numeric())
conflicts_prefer(base::is.character())
conflicts_prefer(base::`&&`)
conflicts_prefer(base::`||`)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

# Parameters
done <- 0

# Parameter: Filter 1
DURATION_THRESHOLD_MIN <- 3  # Minimum duration threshold in minutes

# Parameter: Filter 2
MISSING_THRESHOLD_PCT <- 50  # Maximum percentage of missing values allowed

# Parameter: Filter 3
LONGSTRING_THRESHOLD <- 15   # Maximum number of identical consecutive responses

# Parameter: Filter 4
REPEATED_LONGSTRING_THRESHOLD <- 6 # Number of consecutive identical responses to count as a longstring
REPEATED_LONGSTRING_COUNT <- 3  # Number of longstrings required to flag a response

# Parameter: Filter 5
SCALE_STRAIGHTLINE_THRESHOLD <- 4   # Number of scales that need to be straightlined to flag a response

# Parameter: Filter 6
PATTERN_PERCENTAGE_THRESHOLD <- 60  # Percentage threshold for repetitive patterns
PATTERN_LENGTHS <- c(2, 3, 4, 5) # Lengths of patterns to check for (e.g. AB-AB-AB, ABC-ABC-ABC)

# Parameter: Filter 7
MAX_WEEKLY_HOURS <- 140    # Maximum realistic total weekly hours for "how many hours per week" question set

# Parameter: Filter 8
MAHALANOBIS_THRESHOLD <- 4.0    # Threshold for Mahalanobis distance
PSYCHSYN_THRESHOLD <- 0.0       # Threshold for psychometric synonyms correlation

# List of required variables to run analyses
# Users who are missing any of these variables will be given an alert before running the data.
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

# List of NSSE subscales
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

# Main function to identify careless responses
identify_careless_responses <- function(df) {
  # Initialize results storage
  results <- list()
  step_results <- list()
  
  # Create initial dataframe with numeric values
  df_main <- df %>%
    select(unique_id, duration, askquest:s_eevents) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.))))
  
  # Keep track of all flagged IDs across steps
  all_flagged_ids <- character(0)
  
  # Step 1: Duration check
  # First calculate for all responses
  step_1_condition <- df_main$duration <= DURATION_THRESHOLD_MIN  & !is.na(df_main$duration)
  total_step_1_flags <- sum(step_1_condition, na.rm = TRUE)
  
  # Then process for sequential filtering
  step_results[[1]] <- process_step(
    df = df_main,
    step_number = 1,
    condition_fn = function(x) x$duration <= DURATION_THRESHOLD_MIN  & !is.na(x$duration),
    flag_threshold = DURATION_THRESHOLD_MIN,
    step_desc = paste("Completed survey in", DURATION_THRESHOLD_MIN, "minutes or less")
  )
  all_flagged_ids <- union(all_flagged_ids, 
                           df_main$unique_id[step_results[[1]]$flagged])
  
  # Step 2: Missing values check
  # First calculate for all responses
  df_for_missing <- select(df_main, -unique_id, -duration)
  step_2_condition <- rowSums(is.na(df_for_missing)) / ncol(df_for_missing) * 100 > MISSING_THRESHOLD_PCT
  total_step_2_flags <- sum(step_2_condition, na.rm = TRUE)
  
  # Then process for sequential filtering
  step_results[[2]] <- process_step(
    df = df_main[!df_main$unique_id %in% all_flagged_ids, ],
    step_number = 2,
    condition_fn = function(x) {
      missing_pct <- rowSums(is.na(select(x, -unique_id, -duration))) / 
        ncol(select(x, -unique_id, -duration)) * 100
      missing_pct > MISSING_THRESHOLD_PCT
    },
    flag_threshold = MISSING_THRESHOLD_PCT,
    step_desc = paste("More than", MISSING_THRESHOLD_PCT, "% of survey questions missing")
  )
  all_flagged_ids <- union(all_flagged_ids, 
                           df_main$unique_id[!df_main$unique_id %in% all_flagged_ids][step_results[[2]]$flagged])
  
  # Step 3: Longstring check
  # First calculate for all responses
  step_3_condition <- longstring(select(df_main, -unique_id, -duration))
  total_step_3_flags <- sum(step_3_condition >= LONGSTRING_THRESHOLD, na.rm = TRUE)
  
  # Then process for sequential filtering
  step_results[[3]] <- process_step(
    df = df_main[!df_main$unique_id %in% all_flagged_ids, ],
    step_number = 3,
    condition_fn = function(x) {
      ls_values <- longstring(select(x, -unique_id, -duration))
      ls_values >= LONGSTRING_THRESHOLD
    },
    flag_threshold = LONGSTRING_THRESHOLD,
    step_desc = paste("Straightlined", LONGSTRING_THRESHOLD, "or more responses in a row")
  )
  all_flagged_ids <- union(all_flagged_ids, 
                           df_main$unique_id[!df_main$unique_id %in% all_flagged_ids][step_results[[3]]$flagged])
  
  # Step 4: Repeated longstring check
  # First calculate for all responses
  step_4_condition <- longstring_n_times(select(df_main, -unique_id, -duration), 
                                      threshold = REPEATED_LONGSTRING_THRESHOLD)$count_longstr
  total_step_4_flags <- sum(step_4_condition >= REPEATED_LONGSTRING_COUNT, na.rm = TRUE)
  
  # Then process for sequential filtering
  step_results[[4]] <- process_step(
    df = df_main[!df_main$unique_id %in% all_flagged_ids, ],
    step_number = 4,
    condition_fn = function(x) {
      ls_counts <- longstring_n_times(select(x, -unique_id, -duration), 
                                      threshold = REPEATED_LONGSTRING_THRESHOLD)$count_longstr
      ls_counts >= REPEATED_LONGSTRING_COUNT
    },
    flag_threshold = REPEATED_LONGSTRING_COUNT,
    step_desc = paste("Had", REPEATED_LONGSTRING_COUNT, "or more times they straightlined at least", 
                      REPEATED_LONGSTRING_THRESHOLD, "responses in a row")
  )
  all_flagged_ids <- union(all_flagged_ids, 
                           df_main$unique_id[!df_main$unique_id %in% all_flagged_ids][step_results[[4]]$flagged])
  
  # Step 5: Scale straightlining check
  # First calculate for all responses
  df_scales_all <- df_main
  for (i in seq_along(question_sets)) {
    set_name <- paste0("q_set_", i)
    set_columns <- question_sets[[set_name]]
    df_scales_all <- calculate_longstring_ratio(df_scales_all, set_name, set_columns)
  }
  step_5_condition <- rowSums(select(df_scales_all, starts_with("q_set_")) == 1)
  total_step_5_flags <- sum(step_5_condition >= 4, na.rm = TRUE)
  
  # Then process for sequential filtering
  df_scales_all <- df_main
  for (i in seq_along(question_sets)) {
    set_name <- paste0("q_set_", i)
    set_columns <- question_sets[[set_name]]
    df_scales_all <- calculate_longstring_ratio(df_scales_all, set_name, set_columns)
  }
  step_5_condition <- rowSums(select(df_scales_all, starts_with("q_set_")) == 1)
  total_step_5_flags <- sum(step_5_condition >= SCALE_STRAIGHTLINE_THRESHOLD, na.rm = TRUE)
  
  # Then process for sequential filtering
  step_results[[5]] <- process_step(
    df = df_main[!df_main$unique_id %in% all_flagged_ids, ],
    step_number = 5,
    condition_fn = function(x) {
      df_scales <- x
      for (i in seq_along(question_sets)) {
        set_name <- paste0("q_set_", i)
        set_columns <- question_sets[[set_name]]
        df_scales <- calculate_longstring_ratio(df_scales, set_name, set_columns)
      }
      scale_count <- rowSums(select(df_scales, starts_with("q_set_")) == 1)
      scale_count >= SCALE_STRAIGHTLINE_THRESHOLD
    },
    flag_threshold = SCALE_STRAIGHTLINE_THRESHOLD,
    step_desc = paste("Straightlined", SCALE_STRAIGHTLINE_THRESHOLD, "or more scales")
  )
  all_flagged_ids <- union(all_flagged_ids, 
                           df_main$unique_id[!df_main$unique_id %in% all_flagged_ids][step_results[[5]]$flagged])
  
  # Step 6: Repetitive pattern check
  # First calculate for all responses
  step_6_condition <- data.frame(
    unique_id = df_main$unique_id
  )
  
  # Add columns for each pattern length
  for (length in PATTERN_LENGTHS) {
    col_name <- paste0("rp_", length)
    step_6_condition[[col_name]] <- calculate_repeated_pattern_percentage(
      select(df_main, -duration), length
    )
  }
  
  # Calculate total pattern flags
  pattern_cols <- paste0("rp_", PATTERN_LENGTHS)
  total_step_6_flags <- sum(
    rowSums(step_6_condition[pattern_cols] >= PATTERN_PERCENTAGE_THRESHOLD) > 0, 
    na.rm = TRUE
  )
  
  # Then process for sequential filtering
  step_results[[6]] <- process_step(
    df = df_main[!df_main$unique_id %in% all_flagged_ids, ],
    step_number = 6,
    condition_fn = function(x) {
      x <- select(x, -duration)
      patterns <- data.frame(
        map_dfc(PATTERN_LENGTHS, function(length) {
          set_names(
            list(calculate_repeated_pattern_percentage(x, length)),
            paste0("rp_", length)
          )
        })
      )
      rowSums(patterns >= PATTERN_PERCENTAGE_THRESHOLD) > 0
    },
    flag_threshold = PATTERN_PERCENTAGE_THRESHOLD,
    step_desc = paste0(
      "Made repetitive pattern (e.g. ",
      paste(rep(LETTERS[1:PATTERN_LENGTHS[1]], 3), collapse = ""),
      ") ", PATTERN_PERCENTAGE_THRESHOLD, "% or more of the time"
    )
  )
  all_flagged_ids <- union(all_flagged_ids, 
                           df_main$unique_id[!df_main$unique_id %in% all_flagged_ids][step_results[[6]]$flagged])
  
  
  # Step 7: Hours check
  # First calculate for all responses
  df_hours <- df %>%
    select(unique_id, tmprephrs:tmcommutehrs) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.))))
  
  step_7_condition <- rowSums(select(df_hours, -unique_id), na.rm = TRUE)
  total_step_7_flags <- sum(step_7_condition > MAX_WEEKLY_HOURS, na.rm = TRUE)
  
  # Then process for sequential filtering
  step_results[[7]] <- process_step(
    df = df_hours[!df_hours$unique_id %in% all_flagged_ids, ],
    step_number = 7,
    condition_fn = function(x) {
      total_hours <- rowSums(select(x, -unique_id), na.rm = TRUE)
      total_hours > MAX_WEEKLY_HOURS
    },
    flag_threshold = MAX_WEEKLY_HOURS,
    step_desc = paste("Reported more than", MAX_WEEKLY_HOURS, 
                      "total hours per week across all activities")
  )
  all_flagged_ids <- union(all_flagged_ids, 
                           df_hours$unique_id[!df_hours$unique_id %in% all_flagged_ids][step_results[[7]]$flagged])
  
  # Step 8: Unusual response patterns
  # First calculate for all responses
  df_unusual <- df %>%
    select(unique_id, askquest:sameinst) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
    select(where(~!all(is.na(.))))
  
  step_8_condition_mahad <- calculate_mahalanobis(df_unusual)
  step_8_condition_syn <- calculate_psychsyn(df_unusual)
  total_step_8_flags <- sum(
    (step_8_condition_mahad > MAHALANOBIS_THRESHOLD | step_8_condition_syn <= PSYCHSYN_THRESHOLD) & 
      !is.na(step_8_condition_mahad) & !is.na(step_8_condition_syn), 
    na.rm = TRUE
  )
  
  # Then process for sequential filtering
  mahad_dist <- calculate_mahalanobis(
    df_unusual[!df_unusual$unique_id %in% all_flagged_ids, ]
  )
  syn_values <- calculate_psychsyn(
    df_unusual[!df_unusual$unique_id %in% all_flagged_ids, ]
  )
  
  step_results[[8]] <- process_step(
    df = df_unusual[!df_unusual$unique_id %in% all_flagged_ids, ],
    step_number = 8,
    condition_fn = function(x) {
      (mahad_dist > MAHALANOBIS_THRESHOLD | syn_values <= PSYCHSYN_THRESHOLD) & 
        !is.na(mahad_dist) & !is.na(syn_values)
    },
    flag_threshold = MAHALANOBIS_THRESHOLD,
    step_desc = paste0(
      "Had highly unusual responses (Mahalanobis distance > ", 
      MAHALANOBIS_THRESHOLD,
      " or psychometric synonyms correlation <= ",
      PSYCHSYN_THRESHOLD,
      ")"
    )
  )
  all_flagged_ids <- union(all_flagged_ids, 
                           df_unusual$unique_id[!df_unusual$unique_id %in% all_flagged_ids][step_results[[8]]$flagged])
  
  # Compile final results
  results$steps <- step_results
  
  # Store total flags for each issue
  results$global_flags <- c(
    total_step_1_flags,
    total_step_2_flags,
    total_step_3_flags,
    total_step_4_flags,
    total_step_5_flags,
    total_step_6_flags,
    total_step_7_flags,
    total_step_8_flags
  )
  
  # Create flags dataframe
  flags_df <- data.frame(
    unique_id = df$unique_id,
    step_flagged = NA_character_
  )
  
  # Determine which step each response was first flagged in
  for (i in seq_along(step_results)) {
    newly_flagged_ids <- setdiff(
      if(i == 1) df$unique_id else step_results[[i-1]]$remaining_ids,
      step_results[[i]]$remaining_ids
    )
    flags_df$step_flagged[flags_df$unique_id %in% newly_flagged_ids] <- 
      step_results[[i]]$step_desc
  }
  
  
  # Create flag columns based on the initial calculations for each issue
  flag_columns <- data.frame(unique_id = df$unique_id)
  
  # Duration flags
  flag_columns[[paste("Flag: Completed survey in", DURATION_THRESHOLD_MIN, "minutes or less")]] <- 
    df_main$duration <= DURATION_THRESHOLD_MIN & !is.na(df_main$duration)
  
  # Missing values flags
  flag_columns[[paste("Flag: More than", MISSING_THRESHOLD_PCT, "% of survey questions missing")]] <- 
    rowSums(is.na(df_for_missing)) / ncol(df_for_missing) * 100 > MISSING_THRESHOLD_PCT
  
  # Longstring flags
  flag_columns[[paste("Flag: Straightlined", LONGSTRING_THRESHOLD, "or more responses in a row")]] <- 
    step_3_condition >= LONGSTRING_THRESHOLD
  
  # Repeated longstring flags
  flag_columns[[paste("Flag: Had", REPEATED_LONGSTRING_COUNT, "or more longstrings of", 
                      REPEATED_LONGSTRING_THRESHOLD, "or more responses")]] <- 
    step_4_condition >= REPEATED_LONGSTRING_COUNT
  
  # Scale straightlining flags
  flag_columns[[paste("Flag: Straightlined", SCALE_STRAIGHTLINE_THRESHOLD, "or more scales")]] <- 
    step_5_condition >= SCALE_STRAIGHTLINE_THRESHOLD
  
  # Repetitive pattern flags
  flag_columns[[paste0(
    "Flag: Made repetitive pattern ", 
    PATTERN_PERCENTAGE_THRESHOLD, 
    "% or more of the time"
  )]] <- 
    rowSums(step_6_condition[pattern_cols] >= PATTERN_PERCENTAGE_THRESHOLD) > 0
  
  # Hours flags
  flag_columns[[paste("Flag: Reported more than", MAX_WEEKLY_HOURS, 
                      "total hours per week")]] <- 
    step_7_condition > MAX_WEEKLY_HOURS
  
  # Unusual response flags
  flag_columns[[paste0(
    "Flag: Unusual response patterns (Mahalanobis > ", 
    MAHALANOBIS_THRESHOLD,
    " or synonyms <= ",
    PSYCHSYN_THRESHOLD,
    ")"
  )]] <- 
    (step_8_condition_mahad > MAHALANOBIS_THRESHOLD | step_8_condition_syn <= PSYCHSYN_THRESHOLD) & 
    !is.na(step_8_condition_mahad) & !is.na(step_8_condition_syn)
  
  # Convert TRUE/FALSE to Yes/No
  flag_columns <- flag_columns %>%
    mutate(across(starts_with("Flag:"), ~ifelse(., "Yes", "No")))
  
  # Then modify df_result creation:
  df_result <- df %>%
    left_join(flags_df, by = "unique_id") %>%
    left_join(flag_columns, by = "unique_id") %>%
    mutate(
      `Flagged for Low Effort?` = ifelse(!is.na(step_flagged), "Yes", "No"),
      `Reason for Flag` = step_flagged
    ) %>%
    select(`Flagged for Low Effort?`, `Reason for Flag`, 
           starts_with("Flag:"), everything(), -step_flagged)
  
  return(list(
    data = df_result,
    flags = results$global_flags,
    step_results = step_results
  ))
}

# Function to calculate summary
calculate_summary <- function(processed_data, global_flags, step_results) {
  # Create summary dataframe
  summary_df <- tibble(
    `Step Description` = c(
      "Initial Number of Survey Responses",  # First row
      "Raw Dataset",
      sapply(step_results, function(x) x$step_desc)
    ),
    `Remaining Unflagged Responses` = c(
      nrow(processed_data),  # Starting count
      nrow(processed_data),
      sapply(step_results, function(x) length(x$remaining_ids))
    ),
    `Responses Flagged in this Step` = c(
      NA_integer_,  # Blank for first row
      0,
      sapply(seq_along(step_results), function(i) {
        if (i == 1) {
          length(setdiff(processed_data$unique_id, step_results[[i]]$remaining_ids))
        } else {
          length(setdiff(
            step_results[[i-1]]$remaining_ids,
            step_results[[i]]$remaining_ids
          ))
        }
      })
    )
  )
  
  # Add percentage columns
  summary_df <- summary_df %>%
    mutate(
      `Percent of All Responses Flagged in this Step` = 
        `Responses Flagged in this Step` / nrow(processed_data),
      `Total Responses Flagged for this Issue` = 
        c(NA_integer_, 0, unname(global_flags)),
      `Percent of All Responses with this Issue` = 
        scales::percent(`Total Responses Flagged for this Issue` / nrow(processed_data), 
                        accuracy = 0.1, na.rm = TRUE)
    )
  
  # Number the steps (skip first two rows)
  summary_df$`Step Description`[3:nrow(summary_df)] <- paste0(
    "Step ", 1:(nrow(summary_df)-2), ": ", 
    summary_df$`Step Description`[3:nrow(summary_df)]
  )
  
  # Add final summary row
  total_flagged <- sum(summary_df$`Responses Flagged in this Step`, na.rm = TRUE)
  total_percentage <- total_flagged / nrow(processed_data)
  
  summary_df <- summary_df %>%
    bind_rows(
      tibble(
        `Step Description` = "Final Summary of All Steps",
        `Remaining Unflagged Responses` = nrow(processed_data) - total_flagged,
        `Responses Flagged in this Step` = total_flagged,
        `Percent of All Responses Flagged in this Step` = total_percentage,
        `Total Responses Flagged for this Issue` = NA_integer_,
        `Percent of All Responses with this Issue` = NA_character_
      )
    )
  
  return(summary_df)
}

# Function to show NSSE upload instructions
show_upload_instructions <- function(session) {
  showModal(modalDialog(
    title = "NSSE Data Upload Instructions",
    "For the National Survey of Student Engagement (NSSE), you can upload the raw data file in either .sav (SPSS), .csv, or .xlsx format. IMPORTANT: Make sure to upload the raw data file exactly as it was sent by NSSE (i.e. no edits).",
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
}
