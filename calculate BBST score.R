# installs dependencies REMOVE THESE IF YOU KNOW THAT YOU HAVE THESE PACKAGES ALREADY
install.packages("dplyr")
install.packages("readxl")


library(dplyr)
library(readxl)

# functions
percent_change <- function(perfA, perfB) {
  a <- perfA - perfB
  
  b <- a / perfA
  
  c <- b * 100
  
  return <- c
}

# get list of all .xlsx files
files_list <- list.files(pattern = '.xlsx', full.names = TRUE)

# preallocate vectors
names <- c()
scores <- c()
date_created <- c()

# make the results dataframe with the subject name
results <- c()

# parameters for extracting data from the excel file
excel_file_range = "H77:J127"


# iterate over all files
for (data in files_list) {
  tbl_excel   <-
    read_excel(path = data,
               col_types = "numeric",
               range = excel_file_range)
  
  # get the name of the subject
  subject_name <-
    read_excel(path = data,
               col_names = FALSE,
               range = "A1")
  
  
  if (nrow(tbl_excel) != 0) {
    names <- append(names, sub("SubjectName ", "", subject_name))
    
    # select only the keyboard response times
    tbl_final <- select(tbl_excel,-`Image5-Keyboard-Responses`)
    
    # get the standard deviation of the entire response column
    stdDev <- sd(tbl_final$`Image5-Keyboard-ResponseTime`)
    
    # keep this do not use new stddev
    threeSD <- stdDev * 3
    
    slice1 <- slice(tbl_final, 1:25)
    
    slice2 <- slice(tbl_final, 26:50)
    
    performanceA <-
      mean(slice1$`Image5-Keyboard-ResponseTime`) / mean(slice1$`Image5-Keyboard-IsCorrect`)
    
    performanceB <-
      mean(slice2$`Image5-Keyboard-ResponseTime`) / mean(slice2$`Image5-Keyboard-IsCorrect`)
    
    
    upper_lim <- (mean(c(performanceA, performanceB)) + threeSD)
    lower_lim <- 200
    
    
    slice1filtered <- filter(slice1, between(slice1$`Image5-Keyboard-ResponseTime`, lower_lim, upper_lim ))
    
    slice2filtered <- filter(slice2, between(slice2$`Image5-Keyboard-ResponseTime`, lower_lim, upper_lim ))
    
    
    perfA_adjusted <-
      mean(slice1filtered$`Image5-Keyboard-ResponseTime`) / mean(slice1$`Image5-Keyboard-IsCorrect`)
    
    
    perfB_adjusted <-
      mean(slice2filtered$`Image5-Keyboard-ResponseTime`) / mean(slice2$`Image5-Keyboard-IsCorrect`)
    
    
    final_percent_change <-
      percent_change(perfA_adjusted, perfB_adjusted)
    
    
    scores <- append(scores, final_percent_change)
  }
  
}

# put results into a dataframe
results <-
  data.frame(Names = names,
             Scores = scores)


# write the results dataframe into a csv file
write.table(
  results,
  file = "BBST scores.csv",
  sep = '\t',
  col.names = TRUE,
  row.names = FALSE,
  quote = FALSE
)

