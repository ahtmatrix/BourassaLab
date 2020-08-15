install.packages("dplyr")
install.packages("readxl")

library(dplyr)
library(readxl)


# get list of imotions.txt files
imotion_files <-list.files(pattern = 'Imotions.txt', full.names = TRUE)

# get list of all subject paradigm ".xlsx" files
excel_files <- list.files(pattern = ".xlsx"    , full.names = TRUE)


# gets subject name from the paradigm file
get_subject_name <- function(excel_file) {
  read_excel(path = excel_file,
             col_names = FALSE,
             range = "A1")
}

#--------------------------------------------------------#
# Extract RAW data

# gets Raw data from imotion file
extract_raw_data <- function(imotion_file) {
  options(max.print = .Machine$integer.max)
  
  mydata <-
    read.delim(
      imotion_file,
      skip = 5,
      header = TRUE,
      sep = "\t",
      dec = "."
    )
  
  mydatasubset <- mydata[grep("ABMRawEEG", mydata$EventSource),]
  mydatanew <- mydatasubset[c(28, 30:38)]
}

# loops through each imotion file and writes out the Raw to tsv
for (each in imotion_files) {
  
  raw_data <- extract_raw_data(each)
  
  filename <- paste(sub("_Imotions.txt", "", each))
  
  #select everything besides first SDKTimestamp column
  raw_data_filtered = select(raw_data, -SDKTimeStamp..Raw.)
  
  # write to tsv file
  write.table(
    raw_data_filtered,
    file = paste(filename, "_RAW", ".txt", sep = ""),
    sep = '\t',
    col.names = FALSE,
    row.names = FALSE
  )
}

#-------------------------------------------------------------------------------#
# Extract EVENT data

# get list of imotions.txt files
imotion_files <-list.files(pattern = 'Imotions.txt', full.names = TRUE)

# get list of all subject paradigm ".xlsx" files
excel_files <- list.files(pattern = ".xlsx"    , full.names = TRUE)

correct_num_total_trials <- 596

# make vector of only valued types in this case the ATN for EEG study
experiment_types <- c("CCI", "NCC", "CCC", "SCI", "NCI", "SCC")

# gets the event data from the imotion file
extract_event_data <- function(file) {
  options(max.print = .Machine$integer.max)
  mydata <-
    read.delim(
      file,
      skip = 5,
      header = TRUE,
      sep = "\t",
      dec = "."
    )
  mydatasubset <-
    mydata[grep("ABMThirdPartyTrigger", mydata$LiveMarker),]
  mydatanew <- mydatasubset[c(28, 42)]
  
}

# gets the unique name from the filename of the imotion file
get_imotions_filename <- function(imotion_file) {
  filename <- paste(sub("_Imotions.txt", "", imotion_file))
}

# converts Imotion timestamps to milliseconds
convert_to_ms <- function(ptf, raw_data_first_timestamp) {
  ms <- as.numeric(ptf) %% 1000
  ss <- floor(as.numeric(ptf) / 1000) %% 100
  mm <- floor(as.numeric(ptf) / 100000)
  mm * 60000 + ss * 1000 + (ms - raw_data_first_timestamp)
}

# get the latencies and convert into ms
extract_imotions_data <-
  function(imotion_file, correct_num_total_trials) {

    # add code to get first line of imotions files
    con <- file(imotion_file, "r")
    print(readLines(con, n = 1))
    
    
    raw_data <- extract_raw_data(imotion_file)
    
    # save first time stamp will be used in ms conversion function convert_to_ms()
    raw_data_first_timestamp <- raw_data$SDKTimeStamp..Raw.[1]
    
    event_data <- extract_event_data(imotion_file)
    
    # select first column only
    event_times <- select(event_data, "SDKTimeStamp..Raw.")
    
    # for ATN task 10 practice trials + 288 real trials = 298 trials
    # 298 trials x 2 (onset and offset) = 596
    # get total number of trials
    total_trials <- nrow(event_times)
    
    if (total_trials != correct_num_total_trials) {
      print("there are extras times")
      print(paste("num of trials =" , total_trials))
    }
    
    #remove first 22 rows and the last row
    df_to_enumerate <-
      tibble::enframe(event_times[c(-1:-22, -total_trials),])
    #should end up with 576 rows
    
    print(paste("num of rows in filtered times = ", nrow(df_to_enumerate)))
    
    # create alternating sequence of numbers
    alternating_1_and_2 <-
      tibble::enframe(as.vector(rep(c(1, 2), nrow(df_to_enumerate) / 2)))
    
    #merge enumeration dataframe and df_to_enumerate
    alt_merged <- bind_cols(df_to_enumerate, alternating_1_and_2)
    
    # select rows only with 1 in them
    df_rows_only_ones <- filter(alt_merged, value1 == 1)
    
    # select only the timees and column with 1s in it
    df_col_only_ones <- select(df_rows_only_ones, value, value1)
    
    #set new column names
    colnames(df_col_only_ones) <- c("latency", "type")
    
    
    #convert times to milliseconds
    latency_ms <- as.data.frame(
      sapply(
        df_col_only_ones$latency,
        convert_to_ms,
        raw_data_first_timestamp = raw_data_first_timestamp
      )
    )
    return(latency_ms)
  }



latency_ms <-
  as.data.frame(
    sapply(
      imotion_files,
      extract_imotions_data,
      correct_num_total_trials = correct_num_total_trials
    )
  )


extract_task_types <- function(excel_file, valid_values) {
  #read in paradigm data
  paradigm_data <- read_excel(path = excel_file, range = "H30:H328")
  
  # print(get_subject_name(excel_file))
  
  # rename column name
  colnames(paradigm_data) <- c("Type")
  
  # filter out rows only valid based on above
  filtered_paradigm_data <-
    filter(paradigm_data, Type %in% valid_values)
}

task_types <-
  as.data.frame(sapply(excel_files, extract_task_types, valid_values = experiment_types))






for (each in 1:ncol(latency_ms)) {
  merged <- bind_cols(latency_ms[each], task_types[each])
  colnames(merged) <- c("latency", "type")
  
  
  filename <-
    sub(
      "..",
      "",
      sub(
        "_Imotions.txt.sapply.df_col_only_ones.latency..convert_to_ms..raw_data_first_timestamp...raw_data_first_timestamp.",
        "",
        colnames(latency_ms[each])
      )
    )
  
  print(paste("writing = ", filename))
  write.table(
    merged,
    file = paste(filename, "_EVENT", ".txt", sep = ""),
    sep = '\t',
    col.names = TRUE,
    row.names = FALSE,
    quote = FALSE
  )
}

