
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./rwisp.r")
result <- rwispfromcsv("test.csv")
print(result)
