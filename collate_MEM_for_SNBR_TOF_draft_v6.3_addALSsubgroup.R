






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
# rename any variables related to study name:
# e.g., Toferson_Analysis, Group, Test_types

library("readr")
library(stringr)
library(data.table)

# Define subs to extract 

# list_subj_nums <- c('039','063','077','093', '098') #ALS SNBR Toferson Patients, should be 5/6 (055 didn't have MEPs)
# Group = 'TOF'
Group = 'HealthyControl'
Group = 'ALSSubgroup'
Groups = c("ALS","HC", "TOF", "ZGC") # note, gonna be alphabetical, so need to change this.. 

# set folder location
setwd("C:/Users/AbrahaoLab/Sync/Abrahao Lab/Abbey Analysis/Analysis/Toferson_Analysis/1.SourceData")

source_path <- "C:/Users/AbrahaoLab/Sync/Abrahao Lab/Abbey Analysis/Analysis/Toferson_Analysis/1.SourceData"

# input_folders <- paste0('QUARTS-', paste(list_subj_nums))

input_folders <- list.dirs(
  path = source_path, # replace with the directory you want
  full.names = TRUE, # include the directory in the result
  recursive = FALSE
)

# Create output an R Dataframe structure

TMS_outcomes_collated <- setNames(data.frame(matrix(ncol = 16, nrow = 0)), c("ID", "Group", "Site", "Date", "Age", "Sex", "VisitType", "Muscle", "Test", "Comments", "L_or_R_cx","Onset_Cx", "CondStim", "ISI_ms", "Value_MSO", "Diff_percent"))

colnames(TMS_outcomes_collated)<- c("ID", "Group", "Site", "Date", "Age", "Sex", "VisitType", "Muscle", "Test", "Comments", "L_or_R_cx","Onset_Cx", "CondStim", "ISI_ms", "Value_MSO", "Diff_percent") 

# define conditions & default variables

Test_types <- c("RMT50", "RMT200", "RMT1000", "TSICI",  "TSICF", "TSICIvCS", "ASICI_rel") # not being used currently

# for SNBR, could also be CSP, SICIvsCS, and other experimental measures

visit_day_types <- c("Baseline","Followup1","Followup2","Followup3", "Followup4") # for SNBR

# for QuARTS2
# Visi_day_types <- c("0_avg_am","0_bsl_1_am","0_bsl_2_am","0_bsl_3_am","0_bsl_1_pm","0_bsl_2_pm","0_bsl_3_pm","2_out_1","2_out_2","2_out_avg","3_fu_1","3_fu_2","3_fu_avg")

# add checks, if no SICI, if no SICF....


############################### FUNCTION 

collate_all <- function(list_subj_nums, source_path){
 
  tryCatch({
    # Check if data is provided
    if (missing(list_subj_nums) || missing(source_path)) {
      stop("Error: Input data is missing or no source path provided")
    }
  
  
  # doing as data table, cuz rbind keeps the col names then, but doesnt for dataa.frame??
  # Initialize an empty data frame to store results
  TMS_outcomes_collated <- data.table(ID = character(), 
                                Group = character(), 
                                Site = character(), 
                                Date = character(), 
                                Age = numeric(), 
                                Sex = numeric(), 
                                VisitType = character(), 
                                Muscle = character(),
                                Test = character(), 
                                Side_cx = character(), 
                                L_or_R_cx = character(), 
                                Onset_Cx = character(), 
                                CondStim = numeric(),
                                ISI_ms = numeric(), 
                                Value_MSO = numeric(), 
                                Diff_percent = numeric(), 
                                stringsAsFactors = FALSE)
  
  # For the first folder/group/subject...
  
  for (i in 1) { #1:length(all_paths)) { # 3 = toferson, hack for now
    
    # subj_count <- i
    group_count <- i
    temp.group <- Groups[i]
    # temp.ID <- list_subj_nums[subj_count]
    
    # print to command line for debugging
    sprintf("analyzing data for group %s of total %d group subfolders", Groups[i], length(Groups))
    current_path <- input_folders[i]
    setwd(current_path)

    # define the way to recognize file names
    if (Groups[i] == "ALS") {
      
      file_name_pattern = "SNBR.*\\.MEM" # has "QUARTS-2", followed by 0 or more characters,
      current_path = sprintf("%s/TSICIp", current_path)
    } else if (Groups[i] == "TOF") {
      pattern = "TOF_SNBR.*\\.MEM"
    } else {
      file_name_pattern = "SNBR.*\\.MEM" # has "SNBR", followed by 0 or more characters,
      # pattern = "QUARTS-2.*\\AM.*\\.", # has "QUARTS-2", followed by 0 or more characters,
      # # then "AM", and then nothing else ($)   
    }
    
    # Get a list of files to extract
    
    input_files <- list.files(
      path = current_path, # replace with the directory you want
      pattern = file_name_pattern,

      full.names = TRUE # include the directory in the result
    )
    
    # insert a check here that none are same subj ID as TOF pts! (currently removing from source)
    
    # sprintf("subj %d has %d files to extract", length(list_subj_nums), length(input_files)) # for QuARTS
    sprintf("group %s has %d files to extract", Groups[i], length(input_files))
    
    # length(input_files) # debugging check, here
    
    # -----------------------------
    # For the first data file
    # -----------------------------
    # i <- 1 # counter for all the  files
    

    
    for (ii in 1:length(input_files)) {
      
      # ii <- 2 # DEBUG 
      
      file <- input_files[ii]
      file_count <- ii
      # temp.ID <- list_subj_nums[ii] # not going to work uinless subfolder structure... 
      
      tempdf <- readr::read_delim(file, delim = "\t", show_col_types = FALSE, col_names = FALSE, guess_max = 5)
      
      sprintf("currently extracting sub %s, file %d of %d name: %s", list_subj_nums[subj_count], file_count, length(input_files), input_files[ii])
      # fix this...
      
      # ##########################
      # 0. Get Demographics - TBC
      # ##########################
      
      temp <- setNames(data.frame(matrix(ncol = 16, nrow = 0)), c("ID", "Group", "Site", "Date", "Age", "Sex", "VisitType", "Muscle", "Test", "Comments", "L_or_R_cx","Onset_Cx", "CondStim", "ISI_ms", "Value_MSO", "Diff_percent"))
      
      # RedCapIndex --> c("QZD_FileName", "Comments") # to use for REDCAP!!!!
      temp.ID <- tempdf[[2,2]]
      # Rename levels of the 'ID' factor
      temp$ID <- sub(".*SNBR-(\\d{3}).*", "\\1", temp$ID)# Extract the substring between 'SNBR-' and '_'
      
      # temp.ID <- substring(tempdf[[2,2]],6,8)
      
      temp.QZD_FileName <- tempdf[[1,2]]
      temp.muscle <- tempdf[[13,2]]
      temp.comments <- tempdf[[16,2]]
      temp.age <- substring(tempdf[[19,2]],1,2) # works so long as they arent older than `100 years`
      temp.sex <- substring(tempdf[[20,2]],1,1) # 1 = Male, 2 = Female
      
      # terms <- c("Age", "Sex", "Muscle", "Handedness", "Operator", "TMS Coil")
      temp.group <- tempdf[[14,2]] #input_folders[i] #tempdf[[14,2]] #Groups[i]
      temp.site <- "tor" #doesn't change
      temp.date <- tempdf[[4,2]]
      temp.L_or_R_cx <- substr(tempdf[[11,2]], 1, 1) #check this! L>R
      temp.handed <- tempdf[[10,2]]
      temp.operator <- tempdf[[15,2]]
      
      temp.Total_pulses <- NaN # from rTMS in QuaRTS2
      temp.Onset_Cx <- NaN #In clinical demogaphic notes
      temp.Test <- NaN #the first one
      temp.Side_cx <- NaN # need to get this from demographics or some notes
      
      # ##########################
      # 1. Get Conditions
      # ##########################
      
      # 1.2. Get the Visit Day Variable - hard coded for now
      # temp.visit_day <- "B"
      
      if (Groups[i] == "TOF") { # for TOF group, the visit day was coded in the filename.
        
        pattern <- "_([^_]+)_TP2"  # Apply the regex to extract the matching part
        extracted_strings <- sapply(input_files, function(input_files) {
          match <- regmatches(input_files, regexec(pattern, input_files)) 
          if (length(match[[1]]) > 1) {
            return(match[[1]][2]) # Return the first match between underscores
          } else {
            return(NA) # Return NA if no match found
          }
        })
        # # Display the results
        result <- data.frame(Filename = input_files, Extracted = extracted_strings, stringsAsFactors = FALSE)
        # print(result) # debug
        temp.visit_day <- result$Extracted[ii] # doing based on file index
        
      } else { # for other groups, assume baseline #HACK

        temp.visit_day <- "B"

      }
      # Fix later - build in to check the date of visit, and if multiple per Ps, then make it a follow-ups
      
      # ##########################
      # 2. Get RMT values
      # ##########################
      
      rmt_table <- data.frame(Term = character(), Value = character(), stringsAsFactors = FALSE)
      
      # Define the list of terms to search for
      terms <- c("RMT50", "RMT200", "RMT1000")

      # Initialize an empty list to store results
      lines <- readLines(file)
      
      # Initialize a data frame with the three term names and default NaN values
      rmt_table <- data.frame(Test = terms, Value = NaN, stringsAsFactors = FALSE)
      
      # Loop through the terms and extract matching rows of data per test
      for (t in seq_along(terms)) {
        
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
        
        new_tms_row <- list(temp.ID, temp.group, temp.site, temp.date, temp.age, temp.sex, temp.visit_day, temp.muscle, temp.Test, temp.comments, temp.L_or_R_cx, temp.Onset_Cx, temp.CondStim, temp.ISI, temp.Value, temp.DiffPercent)
        
        
        # TMS_outcomes_collated <- setNames(data.frame(matrix(ncol = 14, nrow = 0)), c("ID", "Group", "Site", "Date", "VisitDay", "TotalPulses", "Test", "Side_cx", "L_or_R_cx","Onset_Cx", "CondStim", "ISI_ms", "Value", "Diff_percent"))
        
        
        # combine into existing data frame
        
        TMS_outcomes_collated <- rbind(TMS_outcomes_collated, new_tms_row)
        
      }
      
      # reset all vars
      
      ##-----------------------------------------------
      # 3. Get the t-SICIp values 
      ##-----------------------------------------------
      
      # patterns <- c("RMT50", 
      #               "RMT200",          
      #               # "RMT1000", 
      #               "T-SICIp", "!T-SICIvISI\\(\\%RMT\\)\\(Parallel\\)"
      #               "T-SICI(70%)1.0ms",
      #               "T-SICI(70%)2.5ms",
      #               "T-SICI(70%)3.0ms",
      #               "!T-SICFvISI(%RMT)(Parallel)") 
      # not being used right now... cuz need extra symbols below... 
      # Fix - need to cerate a list of the test bnames and the correspnding patterns for below, TSIFCF etc
      
      
      lines <- readLines(file)
      
      # Extract SICI - Using match for rows related to "(70%)"
      
      test_line_id <- grep("!T-SICIvISI\\(\\%RMT\\)\\(Parallel\\)", lines) # Find the index of the target line
      
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
      
      temp.CondStim <- 70 # 70 for SICI, 90 for SICF % hard coded for now # fix this, get from table.. 
      temp.Test <- "tSICIp"
      
      for (r in 1:3) { # hard coded for 3 ISIs in SICI test
        
        temp.ISI <- isi_data$ISI[r]
        temp.Value <- isi_data$Value[r]
        temp.DiffPercent <- NaN #isi_table$Diff_percent[r] 
        
        # put data into a row (corresponding to names above)

        new_tms_row <- list(temp.ID, temp.group, temp.site, temp.date, temp.age, temp.sex, temp.visit_day, temp.muscle, temp.Test, temp.comments, temp.L_or_R_cx, temp.Onset_Cx, temp.CondStim, temp.ISI, temp.Value, temp.DiffPercent)
        
        # combine using rbind into existing data frame
        
        TMS_outcomes_collated <- rbind(TMS_outcomes_collated, new_tms_row)
        
      }
      
      # 
      ##-----------------------------------------------
      # 4. Get the t-SICFp values 
      ##-----------------------------------------------
     
      # test_line_id <- grep("!T-SICFvISI\\(\\%RMT\\)\\(Parallel\\)", lines) # Find the index of the target line
      # 
      # # Ensure the target line is found and enough lines exist after it
      # if (length(test_line_id) > 0 && (test_line_id + 4) <= length(lines)) {
      #   
      #   # Skip the next line and extract the following 3 lines
      #   extracted_lines <- lines[(test_line_id + 2):(test_line_id + 4)]
      #   
      #   # Convert extracted lines into a data frame
      #   isi_data <- do.call(rbind, lapply(extracted_lines, function(line) {
      #     split_line <- unlist(strsplit(line, "\t"))  # Split by spaces \\s+
      #     if (length(split_line) >= 2) {
      #       return(data.frame(ISI = split_line[1], Value = as.numeric(split_line[2]), stringsAsFactors = FALSE))
      #     }
      #     return(NULL)
      #   }))
      #   
      #   # Print the extracted data frame
      #   # print(isi_data) # debug
      #   
      # } else {
      #   print("Target line not found or insufficient lines after it.")
      # }
      # 
      # temp.CondStim <- 100 # 70 for SICI, 100 for SICF % hard coded for now # fix this, get from table.. 
      # temp.Test <- "tSICFp"
      # 
      # for (r in 1:3) { # hard coded for 3 ISIs in SICI test
      #   
      #   temp.ISI <- isi_data$ISI[r]
      #   temp.Value <- isi_data$Value[r]
      #   temp.DiffPercent <- NaN #isi_table$Diff_percent[r] 
      #   
      #   # put data into a row (corresponding to names above)
      #   
      #   new_tms_row <- list(temp.ID, temp.group, temp.site, temp.date, temp.age, temp.sex, temp.visit_day, temp.muscle, temp.Test, temp.comments, temp.L_or_R_cx, temp.Onset_Cx, temp.CondStim, temp.ISI, temp.Value, temp.DiffPercent)
      #   
      #   # combine using rbind into existing data frame
      #   
      #   TMS_outcomes_collated <- rbind(TMS_outcomes_collated, new_tms_row)
      #   
      # }
      
      ##-----------------------------------------------
      # ----- EXPLORATORY OUTCOMES 
      ##-----------------------------------------------
      
      
      ##-----------------------------------------------
      # 5. Get the a-SICI values 
      ##-----------------------------------------------
      
      
      ##-----------------------------------------------
      # 6. Get the a-SICI alues 
      ##-----------------------------------------------
      
      ##-----------------------------------------------
      # 7. Get the SICI vs CS alues 
      ##-----------------------------------------------
      
      ##-----------------------------------------------
      # 8. Get CSP values *might be in another MEM file if version 2018 or older
      ##-----------------------------------------------
      
      
    }
   
    # -----------------------------
    # Save new EXCL files to RAW
    # -----------------------------
    
      # Get the current date in YYYY-MM-DD format
      current_date <- format(Sys.Date(), "%Y-%m-%d")
      
      # Define base filenames with the date
      if (Groups[i] == "TOF") {
        
        csv_raw_filename <- sprintf("Tof_Raw_TMS_extracted_%s.csv", current_date)
        xls_raw_filename <- sprintf("Tof_Raw_TMS_extracted_%s.xlsx", current_date)
        
      } else if (Groups[i] == "HC") {
    
      # Define base filenames with the date
      csv_raw_filename <- sprintf("HC_Raw_TMS_extracted_%s.csv", current_date)
      xls_raw_filename <- sprintf("HC_Raw_TMS_extracted_%s.xlsx", current_date)
      
      } else if (Groups[i] == "ALS") {
        
        # Define base filenames with the date
        csv_raw_filename <- sprintf("ALS_Raw_TMS_extracted_%s.csv", current_date)
        xls_raw_filename <- sprintf("ALS_Raw_TMS_extracted_%s.xlsx", current_date)
        
      } else if (Groups[i] == "ZGC") {
        
        # Define base filenames with the date
        csv_raw_filename <- sprintf("GC_Raw_TMS_extracted_%s.csv", current_date)
        xls_raw_filename <- sprintf("GC_Raw_TMS_extracted_%s.xlsx", current_date)
        
      } else {
        
        # Define base filenames with the date
        csv_raw_filename <- sprintf("SNBR_Raw_TMS_extracted_%s.csv", current_date)
        xls_raw_filename <- sprintf("SNBR_Raw_TMS_extracted_%s.xlsx", current_date)
        
      }
    
     
  }
  
  
  # writing data to csv
  # setwd("C:/Users/AbrahaoLab/Sync/Abrahao Lab/Abbey Analysis/Analysis/QuARTS2/2.RawData/")
  
  # TMS_outcomes_collated <- TMS_outcomes_collated %>%
  #       mutate(Date = as.Date(Date, format="%d/%m/%Y"))  # Adjust date format if needed
  # 
  
  out_path <- "C:/Users/AbrahaoLab/Sync/Abrahao Lab/Abbey Analysis/Analysis/Toferson_Analysis/2.RawData/"
  
  raw_filepath <- sprintf("C:/Users/AbrahaoLab/Sync/Abrahao Lab/Abbey Analysis/Analysis/Toferson_Analysis/2.RawData/%s",csv_raw_filename)
  
  
  # Message to indicate process started
  message("Saving data...")
  
  write.csv2(TMS_outcomes_collated,                   
             raw_filepath,
             row.names = TRUE)
  
  # do same for EXCEL only file:
  geterrmessage()
  
  
  # Load necessary library
  library(openxlsx)
  # Save as Excel file (.xls/.xlsx)
  xls_filepath <- file.path(out_path, xls_raw_filename)
  write.xlsx(TMS_outcomes_collated,
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
# result <- collate_all(TMS_outcomes_collated)

