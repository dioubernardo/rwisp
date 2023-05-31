
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(devtools);
load_all(".");

result <- rwispfromcsv("tests/test.csv")
print(result)
