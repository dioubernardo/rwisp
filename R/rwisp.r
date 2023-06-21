#' Integrated Simple Weighted Sum Product Method - WISP
#' 
#' Implementation of An Integrated Simple Weighted Sum Product Method - WISP
#' More information about the method at https://doi.org/10.1109/TEM.2021.3075783
#' More information about the implementation at https://github.com/dioubernardo/rwisp/blob/main/README.md
#' 
#' @importFrom utils count.fields read.csv read.csv2
#' @param data A numeric data matrix, columns are the criteria, rows are the alternatives
#' @param alternatives A character vector with the identification of alternatives
#' @param optimizations A character vector with definition of minimization or maximization for each criterion, expected 'min' or 'max' only
#' @param weights A numeric vector with the criteria weights, the sum of all must be 1
#' @returns list with 3 matrix, ui = ranking and the global ui, normalizedData = normalized data, utilities = utility values
#' @export
#' @examples
#' alternatives <- c("A1", "A2", "A3", "A4", "A5")
#' optimizations <- c("max", "min", "max", "max", "min", "max", "min", "max", "min", "max")
#' weights <- c(0.07, 0.07, 0.07, 0.14, 0.2, 0.08, 0.12, 0.125, 0.05, 0.075)
#' data <- matrix(c(
#'   c(3, 4, 6, 5, 2), # criterion 1 values
#'   c(7, 6, 4, 6, 8), # criterion 2 values
#'   c(4, 5, 5, 6, 3), # criterion 3 values
#'   c(4, 5, 6, 5, 4), # criterion 4 values
#'   c(6, 5, 4, 3, 6), # criterion 5 values
#'   c(4, 5, 5, 6, 3), # criterion 6 values
#'   c(6, 5, 5, 4, 6), # criterion 7 values
#'   c(8, 8, 9, 7, 7), # criterion 8 values
#'   c(5, 6, 7, 8, 7), # criterion 9 values
#'   c(8, 9, 9, 9, 8) # criterion 10 values
#' ), nrow=5, ncol=10)
#' result <- wispcalc(data, alternatives, optimizations, weights)
wispcalc <- function(data, alternatives, optimizations, weights) {
  tryCatch({
    
    imax <- nrow(data)
    jmax <- ncol(data)
    
    # optimizations validation
    hascriteriamin <- FALSE
    hascriteriamax <- FALSE
    for (j in 1:jmax) {
      if (optimizations[j] == 'max') {
        optimizations[j] <- 1
        hascriteriamax <- TRUE
      } else if (optimizations[j] == 'min') {
        optimizations[j] <- -1
        hascriteriamin <- TRUE
      } else{
        stop("Only 'min' or 'max' are valid for criteria optimization")
      }
    }
    optimizations <- as.numeric(optimizations)
    
    # weights validation
    if (sum(weights) != 1)
      stop("The sum of the weights must be equal to 1")

    # normalization
    normalizedData <- matrix(0, imax, jmax)
    colnames(normalizedData) <- paste("C", 1:jmax, sep="")
    rownames(normalizedData) <- alternatives
    for (j in 1:jmax) {
      max <- max(data[, j])
      for (i in 1:imax) {
        normalizedData[i, j] <- as.numeric(data[i, j]) / max
      }
    }
    
    # calculo das medidas de utilidade
    uiwsd <- numeric(imax)
    uiwpd <- numeric(imax)
    uiwsr <- numeric(imax)
    uiwpr <- numeric(imax)
    
    for (i in 1:imax) {
      uiwsdmin <- 0
      uiwsdmax <- 0
      
      uiwpdmin <- if (hascriteriamin) 1 else 0
      uiwpdmax <- if (hascriteriamax) 1 else 0
      
      for (j in 1:jmax) {
        v <- weights[j] * normalizedData[i, j]
        if (optimizations[j] == 1) {
          uiwsdmax <- uiwsdmax + v
          uiwpdmax <- uiwpdmax * v
        } else{
          uiwsdmin <- uiwsdmin + v
          uiwpdmin <- uiwpdmin * v
        }
      }
      
      uiwsd[i] <- uiwsdmax - uiwsdmin
      uiwpd[i] <- uiwpdmax - uiwpdmin
      
      if (hascriteriamin == FALSE){
        uiwsdmin <- 1
        uiwpdmin <- 1
      } 
      if (hascriteriamax == FALSE){
        uiwsdmax <- 1
        uiwpdmax <- 1
      }
      
      uiwsr[i] <- uiwsdmax / uiwsdmin
      uiwpr[i] <- uiwpdmax / uiwpdmin
    }
    
    # recalcular utilidades
    u2iwsd <- numeric(imax)
    u2iwpd <- numeric(imax)
    u2iwsr <- numeric(imax)
    u2iwpr <- numeric(imax)
    
    uiwsdmax <- max(uiwsd)
    uiwpdmax <- max(uiwpd)
    uiwsrmax <- max(uiwsr)
    uiwprmax <- max(uiwpr)
    
    for (i in 1:imax) {
      u2iwsd[i] <- (1 + uiwsd[i]) / (1 + uiwsdmax)
      u2iwpd[i] <- (1 + uiwpd[i]) / (1 + uiwpdmax)
      u2iwsr[i] <- (1 + uiwsr[i]) / (1 + uiwsrmax)
      u2iwpr[i] <- (1 + uiwpr[i]) / (1 + uiwprmax)
    }

    # utility global
    ui <- matrix(0, imax, 2)
    colnames(ui) <- c('position','ui')
    rownames(ui) <- alternatives
    for (i in 1:imax) {
      ui[i,2] <- (u2iwsd[i] +  u2iwpd[i] +  u2iwsr[i] +  u2iwpr[i]) / 4
    }
    ui <- ui[order(as.numeric(ui[,2]), decreasing = TRUE), ]
    ui[,1] <- c(1:imax)
    
    # utilities matrix
    utilities <- matrix(c(uiwsd, uiwpd, uiwsr, uiwpr, u2iwsd, u2iwpd, u2iwsr, u2iwpr), imax, 8)
    colnames(utilities) <- c('uiwsd', 'uiwpd', 'uiwsr', 'uiwpr', 'u2iwsd', 'u2iwpd', 'u2iwsr', 'u2iwpr')
    rownames(utilities) <- alternatives
    
    return(list("ui" = ui, "normalizedData" = normalizedData, "utilities" = utilities))
  },
  error = function(err) {
    stop(paste("Error: ", err))
  })
}

#' Abstraction for extracting data from a CSV file to run the wispcalc function
#' 
#' File requirements: 
#'  - Separated by comma or semicolon
#'  - Do not use thousands separator
#' Example file in https://github.com/dioubernardo/rwisp/blob/main/inst/extdata/example.csv
#' @param file the name of the file
#' @returns list with 3 matrix, ui = ranking and the global ui, normalizedData = normalized data, utilities = utility values
#' @export 
#' @examples
#' result <- rwispfromcsv(system.file("extdata", "example.csv", package = "rwisp"))
rwispfromcsv <- function(file){
  tryCatch({
    
    L <- readLines(file, n = 1)
    numfields <- count.fields(textConnection(L), sep = ";")
    if (numfields == 1){
      csv <- read.csv(file, header = FALSE)
    }else{
      csv <- read.csv2(file, header = FALSE)
    }

    if (tolower(csv[1,1]) != 'criteria')
      stop("This file is out of standards. The first line must contain the criteria. See the example file.")
    if (tolower(csv[2,1]) != 'optimization')
      stop("This file is out of standards. The second line must contain the optimizations. See the example file.")
    if (tolower(csv[3,1]) != 'weight')
      stop("This file is out of standards. The third line must contain the weights. See the example file.")
    if (csv[4,1] != '')
      stop("This file is out of standards. The fourth line must be empty. See the example file.")
    
    ncriteria <- ncol(csv) - 1
    nalternatives <- nrow(csv) - 4
    
    weights <- as.numeric(gsub(",", ".", csv[3,2:(ncriteria+1)]))
    alternatives <- as.character(csv[5:(4+nalternatives),1])
    
    optimizations <- tolower(as.character(csv[2,2:(ncriteria+1)]))
    
    data <- csv[5:(4+nalternatives),2:(ncriteria+1)]
    data[,] <- apply(data[,], 2, function(x) as.numeric(gsub(",", ".", x)))

    result <- wispcalc(data, alternatives, optimizations, weights)
    return(result)
  },
  error = function(err) {
    stop(geterrmessage())
  })
}
