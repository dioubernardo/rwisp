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
    norm_data <- matrix(0, imax, jmax)
    for (j in 1:jmax) {
      max <- max(data[, j])
      for (i in 1:imax) {
        norm_data[i, j] <- as.numeric(data[i, j]) / max
      }
    }
    
    # calculo das medidas de utilidade
    uiwsd <- numeric(i)
    uiwpd <- numeric(i)
    uiwsr <- numeric(i)
    uiwpr <- numeric(i)
    
    for (i in 1:imax) {
      uiwsdmin <- 0
      uiwsdmax <- 0
      
      uiwpdmin <- if (hascriteriamin) 1 else 0
      uiwpdmax <- if (hascriteriamax) 1 else 0
      
      for (j in 1:jmax) {
        v <- weights[j] * norm_data[i, j]
        if (types[j] == 1) {
          uiwsdmax <- uiwsdmax + v
          uiwpdmax <- uiwpdmax * v
        } else{
          uiwsdmin <- uiwsdmin + v
          uiwpdmin <- uiwpdmin * v
        }
      }
      
      # @TODO: verificar as formulas de quando não tem ambos os critérios
      uiwsd[i] = uiwsdmax - uiwsdmin
      uiwpd[i] = uiwpdmax - uiwpdmin
      uiwsr[i] = uiwsdmax / uiwsdmin
      uiwpr[i] = uiwpdmax / uiwpdmin
    }
    
    # recalcular utilidades
    üiwsd <- numeric(i)
    üiwpd <- numeric(i)
    üiwsr <- numeric(i)
    üiwpr <- numeric(i)
    
    uiwsdmax <- max(uiwsd)
    uiwpdmax <- max(uiwpd)
    uiwsrmax <- max(uiwsr)
    uiwprmax <- max(uiwpr)
    
    for (i in 1:imax) {
      üiwsd[i] <- uiwsd[i] / (1 + uiwsdmax)
      üiwpd[i] <- uiwpd[i] / (1 + uiwpdmax)
      üiwsr[i] <- uiwsr[i] / (1 + uiwsrmax)
      üiwpr[i] <- uiwpr[i] / (1 + uiwprmax)
    }
    
    # utilidade global
    ui <- numeric(i)
    for (i in 1:imax) {
      ui[i] = (üiwsd[i] +  üiwpd[i] +  üiwsr[i] +  üiwpr[i]) / 4
    }
    
    # montagem da data final
    result <- matrix(c(alternatives, ui), imax, 2)
    result <- result[order(result[, 2], decreasing = TRUE), ]
    
    return(result)
  },
  error = function(err) {
    stop(paste("Error: ", err))
  })
}