#' Generate a SQL CASE WHEN statement from a mapping file
#'
#' This function constructs a CASE WHEN THEN statement from a mapping file or
#' dataframe. It assumes that the first column of the mapping data contains the
#' original WHEN values, and the second column contains the THEN values (the
#' values to be mapped to.)
#'
#' @param inputfile Mapping dataframe OR path to the mapping file
#' @return A string that represents the constructed CASE statement
#' @export
casewhen <- function(inputfile){
  if (class(inputfile)=="character") {
    mapping <- read.csv(inputfile)
  } else {
    mapping <- inputfile
  }
  statement <- "\nCASE"
  for (i in 1:nrow(mapping)){
    statement = paste(statement, " WHEN ", "'", mapping[i, 1],
                      "'", " THEN ",
                      "'", mapping[i, 2], "'", "\n", sep="")
  }
  cat(statement)
  return(statement)
}

