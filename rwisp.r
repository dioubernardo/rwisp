#' Implementation of An Integrated Simple Weighted Sum Product Method - WISP
#' more information see https://doi.org/10.1109/TEM.2021.3075783
#' 
#' @param data A numeric data matrix, columns are the criteria, rows are the alternatives
#' @param alternatives A character vector with the identification of alternatives
#' @param types A character vector with definition of minimization or maximization for each criterion, expected 'min' or 'max' only
#' @param weights A numeric vector with the criteria weights
#' @returns A matrix with the alternatives and their global utilities, sorted in descending order of utility.
wispcalc <- function(data, alternatives, types, weights) {
  tryCatch({
    
    imax = nrow(data)
    jmax = ncol(data)
    
    # type validation
    hascriteriamin <- FALSE
    hascriteriamax <- FALSE
    for (j in 1:jmax) {
      if (types[j] == 'max') {
        types[j] <- 1
        hascriteriamax <- TRUE
      } else if (types[j] == 'min') {
        types[j] <- -1
        hascriteriamin <- TRUE
      } else{
        stop("Only 'min' or 'max' are valid for criteria types")
      }
    }
    types <- as.numeric(types)
    
    # weights validation
    if (sum(weights) != 1)
      stop("The sum of the weights must be equal to 1")

    # normalization
    normalizedData <- matrix(0, imax, jmax)
    colnames(normalizedData) = paste("C", 1:jmax, sep="")
    rownames(normalizedData) = alternatives
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
        if (types[j] == 1) {
          uiwsdmax <- uiwsdmax + v
          uiwpdmax <- uiwpdmax * v
        } else{
          uiwsdmin <- uiwsdmin + v
          uiwpdmin <- uiwpdmin * v
        }
      }
      
      uiwsd[i] = uiwsdmax - uiwsdmin
      uiwpd[i] = uiwpdmax - uiwpdmin
      
      if (hascriteriamin == FALSE) uiwsdmin = 1;
      if (hascriteriamax == FALSE) uiwpdmax = 1;
      
      uiwsr[i] = uiwsdmax / uiwsdmin
      uiwpr[i] = uiwpdmax / uiwpdmin
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
      u2iwsd[i] <- uiwsd[i] / (1 + uiwsdmax)
      u2iwpd[i] <- uiwpd[i] / (1 + uiwpdmax)
      u2iwsr[i] <- uiwsr[i] / (1 + uiwsrmax)
      u2iwpr[i] <- uiwpr[i] / (1 + uiwprmax)
    }

    # utilidade global
    ui <- matrix(0, imax, 1)
    colnames(ui) <- c('ui')
    rownames(ui) = alternatives
    for (i in 1:imax) {
      ui[i,1] = (u2iwsd[i] +  u2iwpd[i] +  u2iwsr[i] +  u2iwpr[i]) / 4
    }
   # ui <- ui[order(as.numeric(ui[,1]), decreasing = TRUE), ]

    # utilities matrix
    utilities <- matrix(c(uiwsd, uiwpd, uiwsr, uiwpr, u2iwsd, u2iwpd, u2iwsr, u2iwpr), imax, 8)
    colnames(utilities) <- c('uiwsd', 'uiwpd', 'uiwsr', 'uiwpr', 'u2iwsd', 'u2iwpd', 'u2iwsr', 'u2iwpr')
    rownames(utilities) = alternatives
    
    return(list("ui" = ui, "normalizedData" = normalizedData, "utilities" = utilities))
  },
  error = function(err) {
    stop(paste("Error: ", err))
  })
}

#' Abstraction for extracting data from a CSV file to run the wispcalc function
#' File requirements: 
#'  - Separated by comma or semicolon
#'  - Do not use thousands separator
#' Example file in https://github.com/dioubernardo/rwisp/blob/main/test.csv
#' @param file the name of the file
#' @returns A matrix with the alternatives and their global utilities, sorted in descending order of utility.
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
      stop('Non-standard file the first line must contain the criteria, see example file')
    if (tolower(csv[2,1]) != 'type')
      stop('Non-standard file the second line must contain the types, see example file')
    if (tolower(csv[3,1]) != 'weight')
      stop('Non-standard file the third line must contain the weights, see example file')
    if (csv[4,1] != '')
      stop('Non-default file the fourth line must be empty, see example file')

    ncriteria <- ncol(csv) - 1
    nalternatives <- nrow(csv) - 4
    
    weights <- as.numeric(gsub(",", ".", csv[3,2:(ncriteria+1)]))
    alternatives <- as.character(csv[5:(4+nalternatives),1])
    
    types <- tolower(as.character(csv[2,2:(ncriteria+1)]))
    
    data <- csv[5:(4+nalternatives),2:(ncriteria+1)]
    data[,] <- apply(data[,], 2, function(x) as.numeric(gsub(",", ".", x)))

    result <- wispcalc(data, alternatives, types, weights)
    return(result)
  },
  error = function(err) {
    stop(geterrmessage())
  })
}
