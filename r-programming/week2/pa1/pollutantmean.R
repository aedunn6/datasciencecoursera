pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  #if (pollutant != "sulfate" | pollutant != "nitrate")
    # throw error

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  files <- list.files(directory, full.names=TRUE)     # read in files
  all_data <- do.call(rbind, lapply(files, read.csv)) # bind all files together
  data_subset <- data.frame()                         # subset all data by given IDs
  for (i in id) {                                
    data_subset <- rbind(data_subset, all_data[which(all_data[, "ID"] == i),])
  }
  mean(data_subset[, pollutant], na.rm=TRUE)          # find the mean of the pollutant on the subset
}
