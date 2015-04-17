complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  files <- list.files(directory, full.names=TRUE)     # read in files
  all_data <- do.call(rbind, lapply(files, read.csv)) # bind all files together

  # create temp subsets of the data by ID
  #   add IDs to a vector and sum of complete cases to another vector
  ids <- c()
  nobs <- c()
  for (i in id) {
    data_subset_i <- data.frame()
    data_subset_i <- rbind(data_subset_i, all_data[which(all_data[, "ID"] == i),])
    ids <- append(ids, i)
    nobs <- append(nobs, sum(complete.cases(data_subset_i)))
  }
  complete_data <- data.frame(ids, nobs)              # create a dataframe of IDs and complete case sums
  complete_data
}
