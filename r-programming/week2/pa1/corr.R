corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0

  ## Return a numeric vector of correlations

  files <- list.files(directory, full.names=TRUE)     # read in files
  all_data <- do.call(rbind, lapply(files, read.csv)) # bind all files together

  # find the IDs corresponding to the number of complete cases for each ID above the threshold
  id <- complete(directory)[which(completes[, "nobs"] > threshold),][,c("ids")]

  correlations <- numeric(0)
  for (i in id) {
    data_subset <- data.frame()                       # subset all data by the IDs from above ^
    data_subset <- rbind(data_subset, all_data[which(all_data[, "ID"] == i),])
    
    nitrates <- data_subset[complete.cases(data_subset),][, "nitrate"]
    sulfates <- data_subset[complete.cases(data_subset),][, "sulfate"]
    
    correlations <- append(correlations, cor(nitrates, sulfates))
  }

  correlations
}
