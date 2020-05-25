#' convert
#'
#' @param values
#' @param from
#' @param to
#' @param selfEmployed
#' @param unemployed
#'
#' @return
#' @export
#'
#' @examples

convert <- function(values, from = "isco_88", to = "egp", selfEmployed = NULL, unemployed = NULL){

  valid_keys = c(selfEmployed, unemployed, conversion_table[[from]])

  if(any(!values %in% valid_keys)){
    warning("One or more inputs is not a valid code")
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
    result[ISCO %in% selfEmployed,2] <- 6
  }

  if(!is.null(unemployed) & to == "egp"){
    result[ISCO %in% unemployed, 2] <- 12
  }

  return(result)
}

convert(values = c("9321","9322","9330","1210"), to="isco_08")
