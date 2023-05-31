
alternatives <- c("A1", "A2", "A3", "A4", "A5")
optimizations <- c("max", "min", "max", "max")
weights <- c(0.3, 0.25, 0.25, 0.2)
data <- matrix(c(
  c(80000, 50000, 70000, 65000, 75000), # criterion 1 values
  c(400, 150, 250, 300, 100), # criterion 2 values
  c(6, 6, 9, 4, 6), # criterion 3 values
  c(9, 7, 7, 8, 6) # criterion 4 values
), nrow=5, ncol=4)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./R/rwisp.r")
result <- wispcalc(data, alternatives, optimizations, weights)
print(result)
