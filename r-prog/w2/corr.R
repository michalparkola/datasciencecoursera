corr <- function(directory, threshold = 0) {
  files = dir(directory) # vector of file names
  acc <- c()
  for (f in files) {
    readings <- read.csv(file.path(directory, f)) # data.frame "Date","sulfate","nitrate","ID"
    c <- sum(complete.cases(readings)) # integer
    if (c <= threshold) {
      next
    } else {
      acc <- c(acc, cor(readings$sulfate, readings$nitrate, use="complete.obs"))
    }
  }
  acc
}

# cr<-corr("specdata", 150)
# print('corr(specdata, 150)')
# print(head(cr))
# print(summary(cr))

# cr <- corr("specdata", 400)
# print('corr(specdata, 400)')
# print(head(cr))
# print(summary(cr))

# cr <- corr("specdata", 5000)
# print('corr(specdata, 400)')
# print(summary(cr))
# print(length(cr))

# cr <- corr("specdata")
# print('corr(specdata)')
# print(summary(cr))
# print(length(cr))