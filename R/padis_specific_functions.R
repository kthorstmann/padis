



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
#' @param code_pattern The code pattern to be used. Default is \code{c(1, 0)}. This means that if a value originates from the .r-variable, it is documented with a 1, and if it originates from a .i-variable, it is documented with a 0 in the indicator variable. If both columns (.r and .i) contain a missing value for a person, this value will be coded as 999.
#' @param return_indiciator Logical. Should the indicator variable be returned as well? Default is to \code{TRUE}. If set to \code{FALSE}, only the final, merged variable is returned
#'
#' @return A data frame with either 2 colums or all original columns and two additional columns.
#' @export
#'
#' @examples
#' data <- imaginer_example_data
#' imaginer(data, var_stem="PSP1.variable.", return_complete_data=TRUE)
#'
#' ## return without the indicator variable
#' imaginer(data, var_stem="PSP1.variable.", return_complete_data=TRUE,
#' return_indicator = FALSE)
imaginer <- function(data, var_stem, return_complete_data = FALSE,
                     end_pattern = c(".r", ".i"), code_pattern = c(1, 0),
                     return_indicator = TRUE){

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
  if (return_indicator) {
    df_out <- data.frame(origin_var, return_var)
    names(df_out) <- c(origin_var_name, matched_new)
  } else {
    df_out <- data.frame(return_var)
    names(df_out) <- c(matched_new)
  }


  if (return_complete_data) {
    return <- cbind(data, df_out)
  } else {
    return <- df_out
  }
  return(return)
}



#' Turn Daily Data Into Issue Long Data
#'
#' This function is very specific and can only turn the daily data into the Selection long data.
#'
#' @param data The daily data that should be turned into the long form data
#'
#' @return Returns the Selection data-set
#' @export
#'
#' @examples
#' ## not run
#' # these examples work only with the correct data frames
#' data_dy <- read.csv("Raw Diary Selection Data.csv", sep = ";", stringsAsFactors = FALSE)
#' issue <- get_issue_long(data_dy)
#' head(issue)
get_selection_long <- function(data){
  data_dy <- data
  data_dy_names <- names(data_dy)
  sought_variables_id <- c("PAR.ID", "Day.Num", "Day.UniqueID", "Day.DaysIn", "Issue.Num.1", "Issue.UniqueID.1")

  ## nested variables
  sought_patterns <- c("Sought.+\\.1$", "PSP.Num.+\\.1$", "PSP.Unique.+\\.1$",
                       "Sought.+\\.2$", "PSP.Num.+\\.2$", "PSP.Unique.+\\.2$")

  sought_vars <- purrr::map(sought_patterns, ~ grep(x = data_dy_names, pattern = ., value = TRUE))
  search_pattern <- c(rep("\\.\\d+\\.1$", 3), rep("\\.\\d+\\.2$", 3))

  gather_sought <- function(vars, search_pattern){
    sought_data <- data_dy[c(sought_variables_id, vars)]
    long_data <- gather_multiple(sought_data, key_vars=sought_variables_id, search = search_pattern)
    long_data
  }

  long_data_list <- purrr::map2(.x=sought_vars, .y=search_pattern, ~ gather_sought(vars=.x, search_pattern=.y))
  nrows <- unlist(purrr::map(long_data_list, nrow))

  ## add indicator variable for sought issue
  sought_issue <- rep(1, nrows[1])
  long_data_list_1 <- purrr::map(long_data_list[c(1:3)], ~ cbind(., sought_issue))

  sought_issue <- rep(2, nrows[1])
  long_data_list_2 <- purrr::map(long_data_list[c(4:6)], ~ cbind(., sought_issue))

  sought <- rbind(long_data_list_1[[1]], long_data_list_2[[1]])
  num <- rbind(long_data_list_1[[2]], long_data_list_2[[2]])
  unique <- rbind(long_data_list_1[[3]], long_data_list_2[[3]])

  id_vars <- sought[c(sought_variables_id, "sought_issue")]
  sought_select <- dplyr::select(sought, dplyr::matches("Sought\\.."))
  num_select <- dplyr::select(num, dplyr::matches("Num\\.."))
  unique_select <- unique["PSP.UniqueID"]

  sought_long <- cbind(id_vars, sought_select, num_select, unique_select)
  sought_long
}


#' Transform Daily and Intake Data In Five Different Data Sets
#'
#' This function is very specific and takes as input only the two data frames daily and intake raw data. It will then return the five data sets, named 1. "par_data", 2. "psp_long", 3. "day_data", 4. "issue_long", 5. "selection_long")
#'
#' @param daily_data The Daily Raw data
#' @param intake_data The Raw Intake data
#' @param write_to Where the results should be written to. Can either be "workspace" (default), "xlsx", or "csv". The last two write the resulting data frames into the working directory either as .xlsx-files or .csv-files
#' @param overwrite Logical, should an existing data frame/object be overwritten? Default is to \code{FALSE}
#' @param folder The folder in which the resulting data sets should be written. If no folder is specified, the current working directory (result of \code{getwd()}) is used
#'
#' @return The function returns the five different data sets in long format
#' @export
#'
#' @examples
#' ## not run
#' # these examples work only with the correct data frames
#' data_dy <- read.csv("Raw Diary Selection Data.csv", sep = ";", stringsAsFactors = FALSE)
#' data_in <- xlsx::read.xlsx("Raw Intake Sample.xlsx", 1)
#' transform_data(data_dy, data_in)
transform_data <- function(daily_data, intake_data, write_to = "workspace", overwrite = FALSE, folder = NULL){
  data_in <- intake_data
  data_dy <- daily_data
  ## 1. participant data set:
  par_data <- padis::select_vars(data_in, ids = c("PAR.ID"), prefix = "^PAR")

  ## 2. PSP level data set:
  psp_data <- padis::select_vars(data_in, ids = c("PAR.ID"), prefix = "^PSP")
  psp_long <- padis::gather_multiple(psp_data, key_vars=c("PAR.ID"), search="PSP\\d+\\.")

  ## 3. DAY level (they are already correctly aggregated)
  day_data <- padis::select_vars(data_dy, ids = c("PAR.ID"), prefix = "^Day")

  ## 4. Issue level
  issue_data <- padis::select_vars(data_dy, ids = c("PAR.ID", "Day.Num", "Day.UniqueID", "Day.DaysIn"), prefix = "Issue")
  issue_long <- padis::gather_multiple(issue_data, key_vars=c("PAR.ID", "Day.Num", "Day.UniqueID", "Day.DaysIn"), search="Issue")

  ## 5. sought level data
  selection_long <- padis::get_selection_long(data_dy)

  ### combine all data sets
  ## make list with data frames
  names_df_long <- c("PAR_data", "PSP_data", "DAY_data", "ISSUE_data", "SELECTION_data")
  datasets <- c("par_data", "psp_long", "day_data", "issue_long", "selection_long")

  if (write_to == "workspace") {
    eval(parse(text = paste0("write_to_ws(", datasets,", overwrite = ", overwrite, ")")))
  } else
    if (write_to == "xlsx" || write_to == "csv") {
      eval(parse(text =
                   paste0("write_to_wd(",datasets,", folder = '", folder, "', type = '", write_to,"', name = '", names_df_long,"', overwrite = '",   overwrite,"')")
      ))
      message("The outputfiles were written to you working directory or the folder you have specified")
    }
}





#' Transform Diary Data and Create a Next or Previous Day Variable
#'
#' The function follows following rule: First, it treats each \code{id} separately. It then looks in the column \code{days_in} for each numeric value \code{x} that has a preceding or follwing lower or higher value \code{y} with a difference of \code{x - y = 1} (e.g. if there is a 4, it searches for a 3 or a 5, depending on the setting of \code{next_day}). If it finds a preceding value \code{y} that is one smaller than the initial value \code{x}, it returns corresponding row-wise values from the columns \code{variables} and stores them as in a new variable.
#'
#' @param data The data frame that contains the variable
#' @param id The ID variable of the participants
#' @param days_in The variable identifying the day of each measurement. Must be numeric
#' @param variables The variables that should be transformed to a next day variable
#' @param prefix The prefix that is added to the names of the transformed variables. Default is NULL, which will set the prefix accoring to the \code{next_day} argument
#' @param next_day Logical. If \code{TRUE}, the next day is returned. If \code{FALSE}, the previous day is returned
#'
#'
#' @return Returns a data frame with the original variables as well as the transformed variables
#' @export
#'
#' @examples
#' data <- next_day_data
#' previous_day <- other_day(data)
#' next_day <- other_day(data, next_day = TRUE)
other_day <- function(data,
                      id = "PAR.ID",
                      days_in = "Day.DaysIn",
                      variables = c("Day.Wellbeing", "Day.Closeness"),
                      prefix = NULL,
                      next_day = FALSE) {

  if (is.null(prefix)) {
    if (next_day == TRUE) {
      prefix <- "N"
      message ("Prefix was set automatically to N. You can overwrite this using the 'prefix' argument.")
    } else if (next_day == FALSE) {
      prefix <- "P"
      message ("Prefix was set automatically to P. You can overwrite this using the 'prefix' argument.")
    }
  }

  stopifnot(is.numeric(data[,days_in]))
  df_list <- split(x=data, f = data[id])
  wrap_list <- function(x){
    v <- x[,days_in]
    stopifnot(is.numeric(v))
    if (any(table(v) > 1)) {
      stop("There are non-unique values in ", days_in, ". Please check. Each day must be unique")
    }
    wrap_vars <- function(variable){
      return_prev <- function(y){

        if (next_day == TRUE) {
          select_row <- y - 1
        } else if (next_day == FALSE) {
          select_row <- y + 1
        }

        if (is.na(y)) {
          out <- NA
        } else
          if (select_row %in% v) {
            take <- select_row == v
            take[is.na(take)] <- FALSE
            out <- x[take, variable]
          } else {
            out <- NA
          }
        out
      }
      return_values <- sapply(v, return_prev)
      df <- data.frame(return_values)
      if (ncol(df) != 1) {
        "Something went wrong - please check your input data frame. Maybe non-unique IDs or days?"
      }
      names(df) <- paste0(prefix, variable)
      df
    }
    df_transformed <- data.frame(purrr::map(variables, wrap_vars))
    x_out <- cbind(x, df_transformed)
    x_out
  }
  df_list_transformed <- purrr::map(df_list, wrap_list)
  df_out_transformed <- do.call("rbind", df_list_transformed)
  rownames(df_out_transformed) <- rownames(data)
  return(df_out_transformed)
}


#' Combine .r and .i Pairs into a .c Variable
#'
#' @param data The data frame that contains the .r and .i variable pairs. The function handles characters and numerics, but not factors.
#' @param ignore_double logical. If set to TRUE, cases in which both .i and .r contain values will be ignored and NA will be defined to the .c variable. Default is to \code{FALSE}
#' @param combine_data logical. If set to TRUE, the original data frame will be combined with the new data frame. Default is to \code{TRUE}
#'
#' @return Returns a data frame with a combined variable .c, which contains the values of the .i and .r variables.
#' @export
#'
#' @examples
#' real_imagined(real_imagined_data)
#' real_imagined(real_imagined_data, ignore_double = TRUE)
#' real_imagined(real_imagined_data, ignore_double = TRUE, combine_data=FALSE)
real_imagined <- function(data, ignore_double = FALSE, combine_data = TRUE){
  if (any(sapply(data, is.factor))) {stop("data contains at least one factor. Please convert to character before proceeding")}
  data_names <- names(data)
  r_variables <- grep(x = data_names, pattern = "\\.r$", value = TRUE)
  i_variables <- grep(x = data_names, pattern = "\\.i$", value = TRUE)
  r_stems <- stringr::str_replace(r_variables, "\\.r$", "")
  i_stems <- stringr::str_replace(i_variables, "\\.i$", "")
  stems <- intersect(r_stems, i_stems)

  combine_i_r <- function(stem, data, ignore_double = FALSE){
    r_var <- paste0(stem, ".r")
    i_var <- paste0(stem, ".i")

    # throw error if both variables have a value, and not one of them has NA
    count_na <- function(x){ sum(is.na(x)) }
    pair_data <- data[,c(r_var, i_var)]

    # check if is character:
    if (all(apply(pair_data, 2, is.character))) {
      pair_data[pair_data == ""] <- NA
      message("The item-pair ", stem, " is character. Empty values were set to NA")
    }

    na_per_pair <- apply(pair_data, 1, count_na)

    if (!ignore_double) {
      if (any(na_per_pair == 0)) {
        stop("The item-pair ", stem, " (-.i and -.r) contains rows where both variables have non-missing values.
             \n Please check this variable. \n You can also set 'ignore_double' to TRUE to proceed, which will set NA for these pairs in the .c variable.")
      }
      } else {
        if (any(na_per_pair == 0)) { warning("The item-pair ", stem, " (-.i and -.r) contains rows where both variables have non-missing values. \n
                                             The function proceeded because you set 'ignore_double' to TRUE, which will set NA for these pairs in the .c variable. Nevertheless, please check.")}
        pair_data[na_per_pair == 0, ] <- NA
        }

    combine_rows <- function(x, y) {if (is.na(x)) {y} else if (is.na(y)) {x} else {NA}}
    combined_values <- purrr::map2(.x=pair_data[,r_var], .y = pair_data[,i_var], .f = combine_rows)
    out <- unlist(combined_values)
    out[is.na(out)] <- NA
    out_df <- data.frame(out)
    colnames(out_df) <- paste0(stem, ".c")
    return(out_df)
  }
  out_df_list <- purrr::map(stems, ~ combine_i_r(., data, ignore_double = ignore_double))
  out_c_df <- data.frame(out_df_list)
  if (combine_data) {
    data_out <- cbind(data, out_c_df)
  } else {
    data_out <- out_c_df
  }
  return(data_out)
}



