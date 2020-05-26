#' conversion_table
#'
#' @name conversion_table
#' @docType data

#' convert
#'
#' @param values vector of values for conversion
#' @param from origin code name
#' @param to destination code name
#' @param selfEmployed vector of origin codes for self-employed
#' @param unemployed vector of origin codes for unemployed
#'
#' @return Data table of oring and destination codes
#' @import data.table
#' @export
#'
#' @examples
#' convert(values = c("9321","9322","9330","1210"), from="isco_88", to = "isei")

convert <- function(values, from = "isco_88", to = "egp", selfEmployed = NULL, unemployed = NULL){

  valid_keys = c(selfEmployed, unemployed, conversion_table[[from]])

  if(any(!values %in% valid_keys)){
    warning("One or more inputs is not a valid code")
    miss = values[!values %in% valid_keys]
    values = values[values %in% valid_keys]
    cat("Not returning results for: ", paste(miss), "\n")
  }

  vars = c("from", "to")

  data = data.table(
    conversion_table[,from, with=FALSE],
    conversion_table[,to,with=FALSE]
  )

  names(data) <- vars

  result = data[from %in% values, c(vars), with = FALSE]
  result = unique(result, by = "from")

  if(!is.null(selfEmployed) & to == "egp"){
    result[values %in% selfEmployed,2] <- 6
  }

  if(!is.null(unemployed) & to == "egp"){
    result[values %in% unemployed, 2] <- 12
  }

  return(result)
}

utils::globalVariables(c("conversion_table"))
