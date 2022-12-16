#' Generate a SQL CASE WHEN statement from a mapping file
#'
#' This function constructs a CASE WHEN THEN statement from a mapping file. 
#' It assumes that the first column of the mapping file contains the original
#' WHEN values, and the second column contains the THEN values (the values
#' to be mapped to.)
#'
#' @param inputfile Path to the mapping file
#' @return A string that represents the constructed CASE statement
#' @export
casewhen <- function(inputfile){
  mapping <- read.csv(inputfile)
  statement <- "\nCASE"
  for (i in 1:nrow(mapping)){
    statement = paste(statement, " WHEN ", "'", mapping[i, 1],
                      "'", " THEN ",
                      "'", mapping[i, 2], "'", "\n", sep="")
  }
  cat(statement)
  return(statement)
}

