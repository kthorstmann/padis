



#' Match Two Items From Data Frame
#'
#' This function matches two items from a data frame \code{data} by looking for their common stem (\code{var_stem}), and the pattern at the end that makes each variable name unique (\code{end_pattern}). The function is mainly used in \code{\link[padis]{imaginer}}.
#'
#' @param data The data frame in which the variables should be found
#' @param var_stem The stem of the items, i.e. what both item-names share
#' @param end_pattern The pattern at the end of the variable that makes both variables unique
#'
#' @return A vector of length 2 with the two identified variables names
#' @export
#'
#' @examples
#' data <- imaginer_example_data
#' match_ir(data, "PSP1.variable", c(".r", ".i"))
match_ir <- function(data, var_stem, end_pattern = c(".r", ".i")){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(var_stem))
  stopifnot(is.character(end_pattern))
  end_pattern_1 <- end_pattern[1]
  end_pattern_2 <- end_pattern[2]
  var_stem <- stringr::str_replace(var_stem, "\\.$", "")
  names <- names(data)
  r_var <- paste0(var_stem, end_pattern_1)
  i_var <- paste0(var_stem, end_pattern_2)

  grep_match_r <- grep(names, pattern = r_var, value = TRUE)
  grep_match_i <- grep(names, pattern = i_var, value = TRUE)

  if (!any(names == r_var)){stop("The ", end_pattern_1, " variable (" , r_var,") could not be found in the data frame. Check stem?")}
  if (!any(names == i_var)){stop("The ", end_pattern_2, " variable (" , i_var,")could not be found in the data frame. Check stem?")}

  if(length(grep_match_r) > 1) {stop("More than one variable has the stem '", var_stem, "':\n", grep_match_r)}
  if(length(grep_match_i) > 1) {stop("More than one variable has the stem '", var_stem, "':\n", grep_match_i)}

  if(r_var == i_var) {stop("The identified variables are identical:'", r_var, "'")}

  c(r_var, i_var)
}


#' Match Two Items, Select, and Document
#'
#' This function matches two items (identified based on their stem, see \code{\link[padis]{match_ir}}), documents which of the two items had a value and not a missing value, and returns a data frame with two or all plus two colums (depending on \code{return_complete_data}), where the first columns are the original columns from \code{data}, and the last two colums are the origin of the value and the value itself.
#'
#' @param data The data frame that contains the variables
#' @param var_stem The stem of the item, i.e. the character stringr that both items share. Passed on to \code{\link[padis]{match_ir}}
#' @param return_complete_data Logical, if \code{TRUE}, then the complete data frame is returned along with the two new values
#' @param end_pattern The end pattern to search for. Default is \code{c(".r", ".i")}
#' @param code_pattern The code pattern to be used. Default is \code{c(1, 0)}. This means that if a value originates from the .r-variable, it is documented with a 1, and if it originates from a .i-variable, it is documented with a 0. If both columns (.r and .i) contain a missing value for a person, this value will be coded as 999.
#'
#' @return A data frame with either 2 colums or all original columns and two additional columns.
#' @export
#'
#' @examples
#' data <- imaginer_example_data
#' imaginer(data, var_stem="PSP1.variable.", return_complete_data=TRUE)
imaginer <- function(data, var_stem, return_complete_data = FALSE, end_pattern = c(".r", ".i"), code_pattern = c(1, 0)){

  vars <- match_ir(data, var_stem, end_pattern)
  matched_new <- stringr::str_replace(var_stem, "\\.$", "")
  # this construction is because we do not know their order (if someone changes .r and .i)
  var_name_1 <- names(data[vars[1]])
  var_name_2 <- names(data[vars[2]])

  origin_var_name <- paste0(matched_new, ".real.imagined")
  # now check where the value came from
  origin_var <- vector(length=nrow(data))
  na_1 <- is.na(data[var_name_1])
  na_2 <- is.na(data[var_name_2])
  origin_var[na_1] <- code_pattern[1]
  origin_var[na_2] <- code_pattern[2]

  ## check that not both entries are missing or NA
  if (any(rowSums(cbind(na_1, na_2)) != 1)) {
    message("Some persons had missing values on both variables in the variable pair '", matched_new, "'.")
  }

  return_var <- rep(999, nrow(data))
  return_var[!na_1] <- data[!na_1, var_name_1]
  return_var[!na_2] <- data[!na_2, var_name_2]

  ## now generate output
  df_out <- data.frame(origin_var, return_var)
  names(df_out) <- c(origin_var_name, matched_new)

  if (return_complete_data) {
    return <- cbind(data, df_out)
  } else {
    return <- df_out
  }
  return(return)
}





