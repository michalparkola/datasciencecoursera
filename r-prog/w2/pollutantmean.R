pollutantmean <- function(directory, pollutant, id=1:332) {
  files = dir(directory)
  acc <- c()
  for (f in files[id]) {
    readings <- read.csv(file.path(directory,f))
    acc <- c(acc, readings[, pollutant])
  }
  mean(acc, na.rm = TRUE)
}

# print(pollutantmean("specdata", "sulfate", 1:10))
# print(pollutantmean("specdata", "nitrate", 70:72))
# print(pollutantmean("specdata", "nitrate", 23))