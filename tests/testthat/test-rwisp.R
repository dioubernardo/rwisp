
test_doi_result <- function(result){
  # Normalization test
  expect_equal(
    round(as.vector(result$normalizedData[, "C1"]), digits = 2), 
    c(0.50, 0.67, 1.00, 0.83, 0.33)
  )
  expect_equal(
    round(as.vector(result$normalizedData[, "C2"]), digits = 2), 
    c(0.88, 0.75, 0.50, 0.75, 1.00)
  )
  expect_equal(
    round(as.vector(result$normalizedData[, "C3"]), digits = 2), 
    c(0.67, 0.83, 0.83, 1.00, 0.50)
  )
  expect_equal(
    round(as.vector(result$normalizedData[, "C4"]), digits = 2), 
    c(0.67, 0.83, 1.00, 0.83, 0.67)
  )
  expect_equal(
    round(as.vector(result$normalizedData[, "C5"]), digits = 2), 
    c(1.00, 0.83, 0.67, 0.50, 1.00)
  )
  expect_equal(
    round(as.vector(result$normalizedData[, "C6"]), digits = 2), 
    c(0.67, 0.83, 0.83, 1.00, 0.50)
  )
  expect_equal(
    round(as.vector(result$normalizedData[, "C7"]), digits = 2), 
    c(1.00, 0.83, 0.83, 0.67, 1.00)
  )
  expect_equal(
    round(as.vector(result$normalizedData[, "C8"]), digits = 2), 
    c(0.89, 0.89, 1.00, 0.78, 0.78)
  )
  # @TODO: The calculated value is 0.625 in the article is 0.63
  # Note that for rounding off a 5, the IEC 60559 standard (see also ‘IEEE 754’)
  # read help("round")
  expect_equal(
    round(as.vector(result$normalizedData[, "C9"]), digits = 2), 
    c(0.63, 0.75, 0.88, 1.00, 0.88),
    tolerance = 0.1
  )
  expect_equal(
    round(as.vector(result$normalizedData[, "C10"]), digits = 2), 
    c(0.89, 1.00, 1.00, 1.00, 0.89)
  )
  
  # Utility calculation test
  expect_equal(
    round(as.vector(result$utilities[, "uiwsd"]), digits = 2), 
    c(-0.01, 0.12, 0.22, 0.21, -0.08)
  )
  expect_equal(
    round(as.vector(result$utilities[, "uiwpd"]), digits = 5), 
    c(-0.00005, -0.00003, -0.00002, -0.00002, -0.00007)
  )  
  expect_equal(
    round(as.vector(result$utilities[, "uiwsr"]), digits = 2), 
    c(0.98, 1.33, 1.71, 1.76, 0.82)
  )
  expect_equal(
    round(as.vector(result$utilities[, "uiwpr"]), digits = 4), 
    c(0.0013, 0.0054, 0.0175, 0.0132, 0.0003)
  )  
  
  # Utility normalization test
  expect_equal(
    round(as.vector(result$utilities[, "u2iwsd"]), digits = 3), 
    c(0.812, 0.914, 1.000, 0.993, 0.754)
  )
  expect_equal(
    round(as.vector(result$utilities[, "u2iwpd"]), digits = 5), 
    c(0.99997, 0.99999, 1.00000, 1.00000, 0.99995)
  )
  expect_equal(
    round(as.vector(result$utilities[, "u2iwsr"]), digits = 3), 
    c(0.719, 0.844, 0.983, 1.000, 0.659)  
  )
  expect_equal(
    round(as.vector(result$utilities[, "u2iwpr"]), digits = 3), 
    c(0.984, 0.988, 1.000, 0.996, 0.983)  
  )
  
  # ranking test
  expect_equal(
    rownames(result$ui), 
    c("A4", "A3", "A2", "A1", "A5")  
  )
  
  # ui value test
  expect_equal(
    round(as.vector(result$ui[, "ui"]), digits = 3), 
    c(0.997, 0.996, 0.937, 0.879, 0.849)  
  )
}

test_that("Test data from DOI 10.3390/axioms10040347", {
  
  alternatives <- c("A1", "A2", "A3", "A4", "A5")
  optimizations <- c("max", "min", "max", "max", "min", "max", "min", "max", "min", "max")
  weights <- c(0.07, 0.07, 0.07, 0.14, 0.2, 0.08, 0.12, 0.125, 0.05, 0.075)
  data <- matrix(c(
    c(3, 4, 6, 5, 2), # criterion 1 values
    c(7, 6, 4, 6, 8), # criterion 2 values
    c(4, 5, 5, 6, 3), # criterion 3 values
    c(4, 5, 6, 5, 4), # criterion 4 values
    c(6, 5, 4, 3, 6), # criterion 5 values
    c(4, 5, 5, 6, 3), # criterion 6 values
    c(6, 5, 5, 4, 6), # criterion 7 values
    c(8, 8, 9, 7, 7), # criterion 8 values
    c(5, 6, 7, 8, 7), # criterion 9 values
    c(8, 9, 9, 9, 8) # criterion 10 values
  ), nrow=5, ncol=10)
  
  # test errors
  expect_error({
    aux <- optimizations
    aux[1] <- 'xx'
    result <- wispcalc(data, alternatives, aux, weights)
  })

  expect_error({
    aux <- weights
    aux[1] <- aux[1] + 0.01
    result <- wispcalc(data, alternatives, optimizations, aux)
  })

  result <- wispcalc(data, alternatives, optimizations, weights)
  test_doi_result(result)  
})

test_that("Test Excel CSV data from DOI 10.3390/axioms10040347", {
  result <- rwispfromcsv(system.file("extdata", "example.csv", package = "rwisp"))
  test_doi_result(result)
})

test_that("Test CSV data from DOI 10.3390/axioms10040347", {
  result <- rwispfromcsv(system.file("extdata", "example2.csv", package = "rwisp"))
  test_doi_result(result)
})

test_that("Test wrong files", {

  tempfile <- tempfile("data", fileext = c(".csv"))
  csv <- read.csv2(system.file("extdata", "example.csv", package = "rwisp"), header = FALSE)
  
  expect_error({
    csv[4,1] <- 'erro'
    write.table(csv, file = tempfile, sep=";", quote=FALSE, row.names = FALSE, col.names = FALSE)
    result <- rwispfromcsv(tempfile)
  })

  expect_error({
    csv[3,1] <- 'erro'
    write.table(csv, file = tempfile, sep=";", quote=FALSE, row.names = FALSE, col.names = FALSE)
    result <- rwispfromcsv(tempfile)
  })

  expect_error({
    csv[2,1] <- 'erro'
    write.table(csv, file = tempfile, sep=";", quote=FALSE, row.names = FALSE, col.names = FALSE)
    result <- rwispfromcsv(tempfile)
  })

  expect_error({
    csv[1,1] <- 'erro'
    write.table(csv, file = tempfile, sep=";", quote=FALSE, row.names = FALSE, col.names = FALSE)
    result <- rwispfromcsv(tempfile)
  })
  
  file.remove(tempfile)
  
})




test_that("Test only max criteria", {
  
  alternatives <- c("A1", "A2", "A3")
  optimizations <- c("max", "max")
  weights <- c(0.5, 0.5)
  data <- matrix(c(
    c(3, 4, 6), # criterion 1 values
    c(7, 6, 4) # criterion 2 values
  ), nrow=3, ncol=2)
  result <- wispcalc(data, alternatives, optimizations, weights)

  # Utility calculation test
  expect_equal(
    round(as.vector(result$utilities[, "uiwsd"]), digits = 3), 
    c(0.750, 0.762, 0.786)
  )
  expect_equal(
    round(as.vector(result$utilities[, "uiwpd"]), digits = 3), 
    c(0.125, 0.143, 0.143)
  )
  expect_equal(
    round(as.vector(result$utilities[, "uiwsr"]), digits = 3), 
    c(0.750, 0.762, 0.786)  
  )
  expect_equal(
    round(as.vector(result$utilities[, "uiwpr"]), digits = 3), 
    c(0.125, 0.143, 0.143)  
  )
  
  # Utility normalization test
  expect_equal(
    round(as.vector(result$utilities[, "u2iwsd"]), digits = 3), 
    c(0.980, 0.987, 1.000)
  )
  expect_equal(
    round(as.vector(result$utilities[, "u2iwpd"]), digits = 3), 
    c(0.984, 1.000, 1.000)
  )
  expect_equal(
    round(as.vector(result$utilities[, "u2iwsr"]), digits = 3), 
    c(0.980, 0.987, 1.000)  
  )
  expect_equal(
    round(as.vector(result$utilities[, "u2iwpr"]), digits = 3), 
    c(0.984, 1.000, 1.000)  
  )
  
  # ranking test
  expect_equal(
    rownames(result$ui), 
    c("A3", "A2", "A1")  
  )
  
  # ui value test
  expect_equal(
    round(as.vector(result$ui[, "ui"]), digits = 3), 
    c(1.000, 0.993, 0.982)  
  )

})


test_that("Test only min criteria", {
  
  alternatives <- c("A1", "A2", "A3")
  optimizations <- c("min", "min")
  weights <- c(0.5, 0.5)
  data <- matrix(c(
    c(3, 4, 6), # criterion 1 values
    c(7, 6, 4) # criterion 2 values
  ), nrow=3, ncol=2)
  result <- wispcalc(data, alternatives, optimizations, weights)
  
  # Utility calculation test
  expect_equal(
    round(as.vector(result$utilities[, "uiwsd"]), digits = 3), 
    c(-0.750, -0.762, -0.786)
  )
  expect_equal(
    round(as.vector(result$utilities[, "uiwpd"]), digits = 3), 
    c(-0.125, -0.143, -0.143)
  )
  expect_equal(
    round(as.vector(result$utilities[, "uiwsr"]), digits = 3), 
    c(1.333, 1.312, 1.273)  
  )
  expect_equal(
    round(as.vector(result$utilities[, "uiwpr"]), digits = 3), 
    c(8, 7, 7)  
  )
  
  # Utility normalization test
  expect_equal(
    round(as.vector(result$utilities[, "u2iwsd"]), digits = 3), 
    c(1.000, 0.952, 0.857)
  )
  expect_equal(
    round(as.vector(result$utilities[, "u2iwpd"]), digits = 3), 
    c(1.000, 0.980, 0.980)
  )
  expect_equal(
    round(as.vector(result$utilities[, "u2iwsr"]), digits = 3), 
    c(1.000, 0.991, 0.974)  
  )
  expect_equal(
    round(as.vector(result$utilities[, "u2iwpr"]), digits = 3), 
    c(1.000, 0.889, 0.889)  
  )
  
  # ranking test
  expect_equal(
    rownames(result$ui), 
    c("A1", "A2", "A3")  
  )
  
  # ui value test
  expect_equal(
    round(as.vector(result$ui[, "ui"]), digits = 3), 
    c(1.000, 0.953, 0.925)  
  )
  
})
