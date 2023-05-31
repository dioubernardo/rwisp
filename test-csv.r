
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./R/rwisp.r")
result <- rwispfromcsv("test_files/test.csv")
print(result)
