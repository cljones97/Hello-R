#Precondtions: 
#       directory = specdata in current working directory
#       pollutant = sulfate or nitrate
#       id = spreadsheet number. 1 <= id <= 332
#Postcondition:
#       Returns the mean of the specified list of pollutants that have no 
#       unknown values 
pollutantmean <- function(directory, pollutant, id = 1:332) {

  file_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)

  #Create empty data frame to later store data table
  pollutant_df <- data.frame()
  
  for(i in id) {

    #Just reads file in csv format
    #header is a logical value indicating whether the file contains the names of the variables as its first line. 
    #binds data frame by rows
    readCSV <- read.csv(file_list[i], header = TRUE)
    pollutant_df <- rbind(pollutant_df, readCSV)
  }
  mean(pollutant_df[,pollutant], na.rm = TRUE)
  
}

complete <- function(directory, id = 1:332) {

  file_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  #Create empty data frame to later store data table
  complete_df <- data.frame()
  
  for(i in id) {
    readCSV <- read.csv(file_list[i], header = TRUE)
    #stores the sum of complete cases
    sumCases <- sum(complete.cases(readCSV))
    #creates new data frame that stores binds sumCases with i by row
    new_frame <- data.frame(i, sumCases)
    #then we bind our empty data frame with our temp data frame
    complete_df <- rbind(complete_df, new_frame)
    
  }
  #set column names to be id and nobs from example
  colnames(complete_df) <- c("id", "nobs")
  complete_df

}

corr <- function(directory, threshold = 0) {
  filesList <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  cors <- numeric()
  for (i in 1:332) {
    data <- read.csv(filesList[i])
    if (sum(complete.cases(data)) > threshold) {
      cors <- c(cors, cor(data[["sulfate"]], data[["nitrate"]], use = "complete.obs"))
    }
  }
  cors
}
