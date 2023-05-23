
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

csv <- read.table('test.csv', header = FALSE, sep = ",")

ncriterios <- ncol(csv) - 1
nalternativas <- nrow(csv) - 4
  
weights <- as.numeric(gsub(",", ".", csv[3,2:(ncriterios+1)]))
alternatives <- as.character(csv[5:(4+nalternativas),1])

types <- tolower(as.character(csv[2,2:(ncriterios+1)]))

data <- csv[5:(4+nalternativas),2:(ncriterios+1)]
data[,] <- apply(data[,], 2, function(x) as.numeric(gsub(",", ".", x)))

rm(csv, ncriterios, nalternativas)

source("./rwisp.r")
result <- wispcalc(data, alternatives, types, weights)
print(result)
