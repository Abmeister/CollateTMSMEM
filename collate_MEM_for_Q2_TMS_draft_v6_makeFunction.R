






#This is a draft attempt to export data from TMS MEM files (output by QTracks) into an excel file for analysis.
#Using separate scripts to extract RMT50 (see other script), this is for SICI and SICF.
#Other script TBC for other variables...

# File Organization:
# Data files must be in a folder With a subfolder for each subject:  
# 1.SourceData
#  / 201
#  / 202
#  / ... 
# Inside each are 15+ MEM files for each session, for each side, for each d


#Change Log
#I had to rename 2 of the 203 files as a quick hack cuz they had TP2 code instead of QP2.
#Can we rename... QP2C40920B and TP2C40920B
#rename QUARTS-203_TX WK7 RCX AM)QP2C41104A --> duplicate, so removed entire file on AbbeyANaltysis COpy


# before running this script, you have exported the .mem files from QTRacksP to .csv files via excel

library("readr")
library(stringr)
library(data.table)

#Tidyverse

# browser() 

# File Organization:
# Data files must be in a folder With a subfolder for each subject:  
# 1.SourceData
#  / 201
#  / 202
#  / ... 
# Inside each are 15+ MEM files for each session, for each side, for each day

# Define subs to extract 

list_subj_nums <- c('201','203','204','205') #no 202?

# set folder location
setwd("C:/Users/AbrahaoLab/Sync/Abrahao Lab/Abbey Analysis/Analysis/QuARTS2_TMS/1.SourceData")

source_path <- "C:/Users/AbrahaoLab/Sync/Abrahao Lab/Abbey Analysis/Analysis/QuARTS2_TMS/1.SourceData"

# input_folders <- paste0('QUARTS-', paste(list_subj_nums))

input_folders <- list.dirs(
  path = source_path, # replace with the directory you want
  full.names = TRUE, # include the directory in the result
  recursive = FALSE
)

# Create output an R Dataframe structure

Q2_TMS_collated <- setNames(data.frame(matrix(ncol = 14, nrow = 0)), c("ID", "Group", "Site", "Date", "VisitDay", "TotalPulses", "Test", "Side_cx", "L_or_R_cx","Onset_cx", "ConditStim", "ISI_ms", "Value", "Diff_percent"))

colnames(Q2_TMS_collated)<- c("ID", "Group", "Site", "Date", "VisitDay", "TotalPulses", "Test", "Side_cx", "L_or_R_cx","Onset_cx", "ConditStim", "ISI_ms", "Value", "Diff_percent") 

# define conditions & default variables

Test_types <- c("RMT50", "RMT200", "RMT1000", "TSICI",  "TSICF", "TSICIvCS", "ASICI_rel") # not being used currently

Visi_day_types <- c("0_avg_am","0_bsl_1_am","0_bsl_2_am","0_bsl_3_am","0_bsl_1_pm","0_bsl_2_pm","0_bsl_3_pm","2_out_1","2_out_2","2_out_avg","3_fu_1","3_fu_2","3_fu_avg")

############################### FUNCTION 

collate_all <- function(list_subj_nums, source_path){
 
  tryCatch({
    # Check if data is provided
    if (missing(list_subj_nums) || missing(source_path)) {
      stop("Error: Input data is missing or no source path provided")
    }
  
  
  # doing as data table, cuz rbind keeps the col names then, but doesnt for dataa.frame??
  # Initialize an empty data frame to store results
  Q2_TMS_collated <- data.table(ID = character(), 
                                Group = character(), 
                                Site = character(), 
                                Date = character(), 
                                VisitDay = character(), 
                                TotalPulses = numeric(),
                                Test = character(), 
                                Side_cx = character(), 
                                L_or_R_cx = character(), 
                                Onset_Cx = character(), 
                                CondStim = numeric(),
                                ISI_ms = numeric(), 
                                Value_MSO = numeric(), 
                                Diff_percent = numeric(), 
                                stringsAsFactors = FALSE)
  
  # Initialize an empty data frame to store results
  Q2_tSICIp_extracted <- data.table(ID = character(), 
                                    Group = character(), 
                                    Side_cx = character(), 
                                    L_or_R_cx = character(), 
                                    Onset_Cx = character(), 
                                    VisitDay = character(), 
                                    CondStim = numeric(),
                                    ISI_ms = numeric(), 
                                    Value_MSO = numeric(), 
                                    Diff_percent = numeric(), 
                                    stringsAsFactors = FALSE)
  
  # For the first subject...
  
  for (i in 1:length(all_paths)) {
    
    subj_count <- i
    
    temp.ID <- list_subj_nums[subj_count]
    
    # print to command line for debugging
    sprintf("analyzing data for subj %d of total %d subs", i, length(list_subj_nums))
    
    current_path <- input_folders[i]
    
    setwd(current_path)
    
    # length(list.files(current_path))
    
    # Extract only the AM files for now
    
    input_files <- list.files(
      path = current_path, # replace with the directory you want
      # pattern = "QUARTS-2.*\\AM.*\\.", # has "QUARTS-2", followed by 0 or more characters,
      #                            # then "AM", and then nothing else ($)
      pattern = "QUARTS-2.*\\.MEM", # has "QUARTS-2", followed by 0 or more characters,
      # then "AM", and then nothing else ($)
      full.names = TRUE # include the directory in the result
    )
    
    sprintf("subj %d has %d files to extract", length(list_subj_nums), length(input_files))
    
    length(input_files) # check, should be 40 files per person ??
    
    # For the first data file
    # i <- 1 # counter for all the  files
    
    for (ii in 1:length(input_files)) {
      
      # ii <- 2 # DEBUG 
      
      file <- input_files[ii]
      file_count <- ii
      
      tempdf <- readr::read_delim(file, delim = "\t", show_col_types = FALSE, col_names = FALSE, guess_max = 5)
      
      sprintf("currently extracting sub %d, file %d of %d name: %s", length(list_subj_nums), file_count, length(input_files), input_files[ii])
      
      
      # 0. Get Demographics - TBC
      
      temp <- setNames(data.frame(matrix(ncol = 14, nrow = 0)), c("ID", "Group", "Site", "Date", "VisitDay", "TotalPulses", "Test", "Side_cx", "L_or_R_cx","Onset_cx", "ConditStim", "ISI_ms", "Value", "Diff_percent"))
      
      # RedCapIndex --> c("QZD_FileName", "Comments") # to use for REDCAP!!!!
      
      temp.QZD_FileName <- tempdf[[1,2]]
      temp.muscle <- tempdf[[13,2]]
      temp.comments <- tempdf[[16,2]]
      
      # terms <- c("Age", "Sex", "Muscle", "Handedness", "Operator", "TMS Coil")
      temp.group <- tempdf[[14,2]] #should be "Subject type""
      temp.site <- "tor" #doesn't change
      temp.date <- tempdf[[4,2]]
      temp.L_or_R_cx <- substr(tempdf[[11,2]], 1, 1) #check this! L>R
      temp.handed <- tempdf[[10,2]]
      temp.operator <- tempdf[[15,2]]
      
      temp.Total_pulses <- NaN # where is this?
      temp.Onset_Cx <- NaN #where is this from?
      temp.Test <- NaN #the first one
      
      # Create  SIDE cx FROM LOGICALS - need to check this later against demographic info form Coordinators
      
      temp_char <- substring(tempdf[11, 2], 1, 1)
      if (!is.na(temp_char) & !is.na(temp.handed)) {
        if (grepl("^L", temp_char, ignore.case = FALSE) & grepl("^L", temp.handed, ignore.case = FALSE)) {
          temp.Side_cx <- "d"
        } else if (grepl("^R", temp_char, ignore.case = FALSE) & grepl("^L", temp.handed, ignore.case = FALSE)) {
          temp.Side_cx <- "d"
        } else if ((grepl("^L", temp_char, ignore.case = FALSE) & grepl("^R", temp.handed, ignore.case = FALSE)) || 
                   (grepl("^R", temp_char, ignore.case = FALSE) & grepl("^L", temp.handed, ignore.case = FALSE))) {
          temp.Side_cx <- "nd"
        }
      } else {
        temp.Side_cx <- NaN  # Assign NA if values are missing
      }
      
      
      # 1. Get Conditions
      
      
      # 1.2. Get the Visit Day Variable - hard coded for now
      
      char_skip <- nchar(current_path)+11+2 # taken from path name, so make sure to change if this changes!!
      
      # soft code later using: pattern
      pattern <- "_([^_]+)_QP2"
      
      # Apply the regex to extract the matching part
      extracted_strings <- sapply(input_files, function(input_files) {
        match <- regmatches(input_files, regexec(pattern, input_files))
        if (length(match[[1]]) > 1) {
          return(match[[1]][2]) # Return the first match between underscores
        } else {
          return(NA) # Return NA if no match found
        }
      })
      
      # # Display the extracted strings - Debugging
      # print(extracted_strings)
      
      # # Display the results
      result <- data.frame(Filename = input_files, Extracted = extracted_strings, stringsAsFactors = FALSE)
      
      # print(result)
      
      
      if (grepl('BSL D', file) == TRUE) { # pattern for baseline filenames
        
        day_tmp <- substring(file, char_skip, char_skip+2)
        week_tmp <- substring(file, char_skip+4, char_skip+4+3)
        time_tmp <- substring(file, char_skip+13, char_skip+13+1)
        tmp.OnsetCx <- substring(file, char_skip+9, char_skip+9+2)
        
      } else if (grepl('TX D', file) == TRUE) { # pattern for treatement week filenames
        
        day_tmp <- substring(file, char_skip, char_skip+1)
        week_tmp <- substring(file, char_skip+3, char_skip+3+1)
        time_tmp <- substring(file, char_skip+10, char_skip+10+1)
        tmp.OnsetCx <- substring(file, char_skip+6, char_skip+6+2)
        
      } else if (grepl('TX WK', file) == TRUE) { # pattern for followup filenames
        
        day_tmp <- substring(file, char_skip, char_skip+1)
        week_tmp <- substring(file, char_skip+3, char_skip+4+2)
        time_tmp <- substring(file, char_skip+12, char_skip+12+1)
        tmp.OnsetCx <- substring(file, char_skip+8, char_skip+8+2)
        
      }
      
      # RENAME ACCording to new codes:
      
      # old_names <- as.character(1:13) # use mathcing function?
      
      temp.visit_day <- result$Extracted[ii] # doing based on file index
      
      # ##########################
      
      # 2. Get RMT values
      
      
      rmt_table <- data.frame(Term = character(), Value = character(), stringsAsFactors = FALSE)
      
      # Define the list of terms to search for
      terms <- c("RMT50", "RMT200", "RMT1000")
      # terms <- list(c("RMT50", "RMT200", "RMT1000"))
 
      # Initialize an empty list to store results
      lines <- readLines(file)
      
      # Initialize a data frame with the three term names and default NaN values
      rmt_table <- data.frame(Test = terms, Value = NaN, stringsAsFactors = FALSE)
      
      # Loop through the terms and extract matching rows of data per test
      for (i in seq_along(terms)) {
        
        current_term <- terms[i]
        
        # Find lines containing the term
        matching_lines <- grep(paste0("\\b", current_term, "\\b"), lines, value = TRUE)
        
        # Initialize value as NaN (default if term is missing)
        value <- NaN
        
        # Extract the value if the term is found
        for (line in matching_lines) {
          match <- regmatches(line, regexec(paste0(current_term, "\\s*=\\s*(\\S+)"), line))
          
          if (length(match[[1]]) > 1) {
            value <- as.numeric(match[[1]][2])  # Extract numeric value
            # break  # Exit loop after the first valid match
          }
        }
        
        # Update the data frame with the extracted value
        rmt_table$Value[i] <- value
      }
      
      # Print the final table
      # print(rmt_table) # debug
      
      # # save to the big master data file
      
      for (r in 1:3) { # hard coded for 3 terms (RMT50, 200, 1000)
        
        temp.Test <- rmt_table$Test[r]
        temp.ISI <- NaN
        temp.Value <- rmt_table$Value[r]
        temp.DiffPercent <- NaN 
        
        new_tms_row <- list(temp.ID, temp.group, temp.site, temp.date, temp.visit_day, temp.Total_pulses, temp.Test, temp.Side_cx, temp.L_or_R_cx, temp.Onset_Cx, temp.CondStim, temp.ISI, temp.Value, temp.DiffPercent)
        
        
        # Q2_TMS_collated <- setNames(data.frame(matrix(ncol = 14, nrow = 0)), c("ID", "Group", "Site", "Date", "VisitDay", "TotalPulses", "Test", "Side_cx", "L_or_R_cx","Onset_cx", "ConditStim", "ISI_ms", "Value", "Diff_percent"))
        
        
        # combine into existing data frame
        
        Q2_TMS_collated <- rbind(Q2_TMS_collated, new_tms_row)
        
      }
      
      # reset all vars
      
      ########################
      # 3. Get the t-SICI values (deliminated by = sign)
      
      patterns <- c("RMT50", 
                    "RMT200",          
                    # "RMT1000",# do we want 1 or 4 or average?
                    "T-SICIp", 
                    "T-SICI(70%)1.0ms",
                    "T-SICI(70%)2.5ms",
                    "T-SICI(70%)3.0ms",
                    "!T-SICFvISI(%RMT)(Parallel)") 
      # not being used right now...
      
      # SICI - easier version for now
      
      lines <- readLines(file)
      
      # start_row <- grep("T-SICIp", lines)
      # 
      # # Ensure the pattern is found
      # if (length(start_row) == 0) {
      #   stop("Pattern 'T-SICIp' not found in the file.")
      # }
      # 
      # # Extract rows starting one line after the identified row
      # data_rows <- lines[(start_row -3):length(lines)]
      # 
      # # Initialize an empty data frame to store results
      # sici_table <- data.frame(Variable = character(), Value = numeric(), stringsAsFactors = FALSE)
      # 
      # # Loop through each row and extract variable names and values
      # for (line in data_rows) {
      #   if (grepl("=", line)) { # Check if the line contains '='
      #     parts <- strsplit(line, "=")[[1]] # Split the line at '='
      #     variable <- trimws(parts[1]) # Get the variable name (left side)
      #     value <- as.numeric(trimws(parts[2])) # Get the numeric value (right side)
      #     
      #     # Add to results data frame
      #     sici_table <- rbind(sici_table, data.frame(Variable = variable, Value = value, stringsAsFactors = FALSE))
      #     
      #   }
      # }
      
      # # Display the results
      # print(results)
      
      #               Variable        Value
      # 1           T-SICI(70%)1.0ms  28.6
      # 2           T-SICI(70%)2.5ms   8.3
      # 3           T-SICI(70%)3.0ms  14.1
      # 4             T-SICIp-NCross   5.0
      # 5              T-SICIp-NSkip   0.0
      # 6            T-SICIp-NArtRej   0.0
      # 7           T-SICIp-NArtMiss   0.0
      # 8 3\t200\t70\tLogRegress\t(n    NA
      
      
      # Part 2: Extract SICI - Using match for rows related to "(70%)"
      
      # Version 2 - 
      # Find the index of the target line
      test_line_id <- grep("!T-SICIvISI\\(\\%RMT\\)\\(Parallel\\)", lines)
      
      # Ensure the target line is found and enough lines exist after it
      if (length(test_line_id) > 0 && (test_line_id + 4) <= length(lines)) {
        
        # Skip the next line and extract the following 3 lines
        extracted_lines <- lines[(test_line_id + 2):(test_line_id + 4)]
        
        # Convert extracted lines into a data frame
        isi_data <- do.call(rbind, lapply(extracted_lines, function(line) {
          split_line <- unlist(strsplit(line, "\t"))  # Split by spaces \\s+
          if (length(split_line) >= 2) {
            return(data.frame(ISI = split_line[1], Value = as.numeric(split_line[2]), stringsAsFactors = FALSE))
          }
          return(NULL)
        }))
        
        # Print the extracted data frame
        # print(isi_data) # debug
        
      } else {
        print("Target line not found or insufficient lines after it.")
      }
      
      # isi_table <- data.frame(isi_data) # don't need to do now?
      
      # do I want to make one of these for each subject on each day?
      
      
      temp.CondStim <- 70 # 70 for SICI, 90 for SICF % hard coded for now
      temp.Test <- "tSICIp"
      
      for (r in 1:3) { # hard coded for 3 ISIs in SICI test
        
        temp.ISI <- isi_data$ISI[r]
        temp.Value <- isi_data$Value[r]
        temp.DiffPercent <- NaN #isi_table$Diff_percent[r] 
        
        new_SICI_row <- list(temp.ID, temp.group, temp.Side_cx, temp.L_or_R_cx, temp.Onset_Cx, temp.visit_day, temp.CondStim, temp.ISI, temp.Value, temp.DiffPercent)
        
        # combine into existing data frame
        
        Q2_tSICIp_extracted <- rbind(Q2_tSICIp_extracted, new_SICI_row)
        
        # reformat for the big collated data file'
        
        new_tms_row <- list(temp.ID, temp.group, temp.site, temp.date, temp.visit_day, temp.Total_pulses, temp.Test, temp.Side_cx, temp.L_or_R_cx, temp.Onset_Cx, temp.CondStim, temp.ISI, temp.Value, temp.DiffPercent)
        
        # combine into existing data frame
        
        Q2_TMS_collated <- rbind(Q2_TMS_collated, new_tms_row)
        
      }
      
      # SICF
      
      
      # # SICI % RMT
      # 
      # lines <- readLines(file)
      # start_row <- grep("!T-SICIvISI(%RMT)(Parallel)", lines)
      # 
      # # Ensure the pattern is found
      # if (length(start_row) == 0) {
      #   stop("Pattern 'T-SICIp' not found in the file.")
      # }
      # 
      # # Extract rows starting one line after the identified row
      # data_rows <- lines[(start_row + 1):(start_row + 4 )]
      # 
      # # Initialize an empty data frame to store results
      # results <- data.frame(Variable = character(), Value = numeric(), stringsAsFactors = FALSE)
      # 
      # # Loop through each row and extract variable names and values
      # for (line in data_rows) {
      # 
      #     variable <- line[1] # Get the variable name (left side)
      #     value <- as.numeric(line[2]) # Get the numeric value (right side)
      #     
      #     # Add to results data frame
      #     results <- rbind(results, data.frame(Variable = variable, Value = value, stringsAsFactors = FALSE))
      #   
      # }
      # 
      # # Display the results
      # print(results)
      # 
      # 
      # # 3. Get thers for later
      # 
      # 
      
      ########
      
      # RMT50
      
      # RMT200
      
      # RMT1000
      
      # new_row <- c(list_subj_nums[subj_count], day_tmp, side_tmp, time_tmp, substring(tempdf[1], 9,13))
      
      #   
      # # put this into a row
      # 
      #  temp_row <- list(temp.ID, temp.group, temp.site, temp.date, temp.visit_day, temp.totalPulse, temp.Test, temp.Side_cx, temp.L_or_R_cx, temp.Onset_Cx, temp.CondStim, temp.ISI, temp.Value, temp.DiffPercent)
      #  
      # # temp
      # 
      # # add that data to a new row in dataframe
      #   
      #   
      # Q2_RMT50_extracted <- rbind(Q2_RMT50_extracted, new_row)
      
      # # cycle through all the tests:
      #   for (iii in 1:length(Test_types)) {
      # 
      #   # save the dataframe
      #   #Q2_RMT50_extracted
      #   
      #  
      #   }
      
      
    }
    
  }
  
  
  # writing data to csv
  # setwd("C:/Users/AbrahaoLab/Sync/Abrahao Lab/Abbey Analysis/Analysis/QuARTS2/2.RawData/")
  
  # Get the current date in YYYY-MM-DD format
  current_date <- format(Sys.Date(), "%Y-%m-%d")
  
  out_path <- "C:/Users/AbrahaoLab/Sync/Abrahao Lab/Abbey Analysis/Analysis/QuARTS2_TMS/2.RawData/"
  
  # Define base filenames with the date
  csv_raw_filename <- sprintf("QuARTS2_Raw_TMS_extracted_test_%s.csv", current_date)
  xls_raw_filename <- sprintf("QuARTS2_Raw_TMS_extracted_test_%s.xlsx", current_date)
  
  
  raw_filename <- "QuARTS2_Raw_TMS_extracted_test.csv"
  raw_filepath <- sprintf("C:/Users/AbrahaoLab/Sync/Abrahao Lab/Abbey Analysis/Analysis/QuARTS2_TMS/2.RawData/%s",csv_raw_filename)
  
  
  # Message to indicate process started
  message("Saving data...")
  
  write.csv2(Q2_TMS_collated,                   
             raw_filepath,
             row.names = TRUE)
  
  # do same for SICI only file:
  geterrmessage()
  
  
  # Load necessary library
  library(openxlsx)
  # Save as Excel file (.xls/.xlsx)
  xls_filepath <- file.path(out_path, xls_raw_filename)
  write.xlsx(Q2_TMS_collated,
             file = xls_filepath,
             rowNames = TRUE)

  
  # Print confirmation messages
  print(paste("CSV file saved to:", raw_filepath))
  # print(paste("Excel file saved to:", xls_filepath))
# } # end of function

}, error = function(e) {
  message("An error occurred: ", e$message)
  return(NULL)  # Return NULL in case of failure
})
}
# Example function usage
# result <- collate_all(Q2_TMS_collated)

