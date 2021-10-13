complete <- function(directory, id=1:332) {
  files = dir(directory)
  acc <- data.frame(id=c(), nobs=c())
  for (i in id) {
    readings <- read.csv(file.path(directory, files[i]))
    n <- sum(complete.cases(readings))
    acc <- rbind(acc, data.frame(id=i, nobs=n))
  }
  acc
}

# print(complete("specdata", 1))
# print(complete("specdata", c(2, 4, 8, 10, 12)))
# print(complete("specdata", 30:25))
# print(complete("specdata", 3))