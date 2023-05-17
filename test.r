
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

csv <- read.table('test.csv', header = FALSE, sep = ",")

ncriterios <- ncol(csv) - 1
nalternativas <- nrow(csv) - 4
  
pesos <- as.numeric(gsub(",", ".", csv[3,2:(ncriterios+1)]))
alternativas <- as.character(csv[5:(4+nalternativas),1])

tipos <- tolower(as.character(csv[2,2:(ncriterios+1)]))

matriz <- csv[5:(4+nalternativas),2:(ncriterios+1)]
matriz[,] <- apply(matriz[,], 2, function(x) as.numeric(gsub(",", ".", x)))

rm(csv, ncriterios, nalternativas)

source("./rwisp.r")
result <- wispcalc(matriz, alternativas, tipos, pesos)
print(result)
