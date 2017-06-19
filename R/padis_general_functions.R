

#' Select variables from a data frame based on a prefix
#'
#' @param data The data frame to look for the variables
#' @param ids The ID-variable(s) that should also be kept
#' @param prefix The prefix or pattern of the variables that are taken along
#'
#' @return It returns a data frame that contains only a subset of the previous data frame
#' @export
#'
#' @examples
#' data <- mtcars
#' select_vars(data, "mpg", "^d.")
select_vars <- function(data, ids, prefix){
  select_prefix <- grep(x = names(data), pattern = prefix, value = TRUE)
  select_vars <- unique(c(ids, select_prefix))
  data[select_vars]
}


#' Swaps character elements around strings
#'
#' This function is useful to generate the item names that are needed for the transformation from long to wide format
#'
#' @param string The string that should be swapped
#' @param separator The separator where the stringr should be splitted (takes always the first hit)
#' @param unifyer The character stringr with which the string should be merged again
#'
#' @return Returns a character stringr
#' @export
#'
#' @examples
#' swap_char("this.string", ".", "_")
swap_char <- function(string, separator = ".", unifyer = "_"){
  splitted <- stringr::str_split_fixed(string, pattern=paste0("\\", separator), n = 2)
  paste(splitted[,2], splitted[,1], sep = unifyer)
}


#' Generate unique item stems from item names
#'
#' Using this function and the \code{\link[padis]{select}} function, a lot of variables can easily be subsetted/selected from a data frame.
#'
#' @param strings The item names that are used to generate the strings
#' @param search The unique element that identify a single item
#'
#' @return Returns the unique stems of a item list
#' @export
#'
#' @examples
#' names <- paste("var_", 1:5, sep = "")
#' generate_stems(names, search = "_\\d")
generate_stems <- function(strings, search = "\\d+$"){
  all_stems <- stringr::str_replace_all(strings, search, "")
  t <- table(all_stems)
  valid_stems <- t[t > 1]
  re_stems <- names(valid_stems)
  re_stems
}


#' Gather one variable set from wide to long
#'
#' @param data The data frame that contains the variables
#' @param key_vars The key-variables or id variables that should be the grouping variables in the long format
#' @param varying The variables that vary and should be stacked in the long format
#' @param keep_others If \code{TRUE}, all other variables of the data frame are kept as well and stacked in the long format
#' @param new_name The new name of the stacked variable in the long format
#'
#' @return Returns a data frame in long format
#' @export
#'
#' @examples
#' gather_one(data=wide_example_data, key_vars = "id", varying = c("var_p_1", "var_p_2"), new_name = "var")
gather_one <- function(data, key_vars, varying, keep_others = FALSE, new_name = NULL){
  if (is.null(new_name)) {
    new_name <- generate_stems(varying)
  }
  gather_var <- varying
  if (!keep_others) {
    names_complete <- names(data)
    data <- data[c(key_vars, gather_var)]
  }
  gathered <- tidyr::gather_(data = data,
                             key_col = "group_id",
                             value_col = new_name,
                             gather_cols = gather_var)
  gathered$group_id <- NULL
  gathered
}


#' Gather multiple variables at the same time and turn them into long format
#'
#' @param data The data frame that contains the variables
#' @param key_vars The key or id variables that should be the grouping variables in the long format
#' @param varying The variables that are varying and should be returned in a long format. If NULL, all other variables except \code{key_vars} are taken and turned into long format. Default is to search for numbers at the end (\code{"\\d$"}
#' @param search The search string that is used to detect item-stems
#'
#' @return A data frame in long format, with the \code{key_vars} as grouping variables and the \code{varying} item-stems as stacked variables.
#' @export
#'
#' @examples
#' gather_multiple(wide_example_data, key_vars = c("id", "id2"), varying = NULL)
gather_multiple <- function(data, key_vars, varying = NULL, search = "\\d$"){
  stopifnot(is.data.frame(data))
  if (is.null(varying)){
    all_names <- names(data)
    varying <- setdiff(all_names, key_vars)
  }
  stems <- generate_stems(varying, search = search)
  varying_list <- purrr::map(stems, ~ grep(x=varying, pattern=., value = TRUE))
  long_list <- purrr::map2(varying_list, stems, ~ gather_one(data=data, key_vars = key_vars, varying=.x, new_name=.y))
  long_df <- as.data.frame(long_list)
  long_re_df <- long_df[c(key_vars, stems)]
  long_re_df
}



#' Check for existing objects in working directory
#'
#' The function checks if an object exists in the current working direcotry or specified folder.
#'
#' @param check The object that is checked for in the current folder
#' @param folder The folder that is checked in. If NULL, then the current working directory is checked
#'
#' @return logical, does the tested object exist in the folder?
#' @export
#'
check_exist_wd <- function(check = NULL, folder = NULL){
  stopifnot(is.character(check))
  stopifnot(is.character(folder) || is.null(folder))
  if (is.null(folder)) {
    all_files <- list.files()
  } else {
    all_files <- list.files(path = folder)
  }
  file_in_folder <- any(check == all_files)
  file_in_folder
}



#' Checks if an object exists in the global environment
#'
#' The global environment is the same as the workspace.
#'
#' @param test_object The object that is tested/checked
#'
#' @return logical, does the tested object exist in the workspace?
#' @export
#'
#' @examples
#' a <- 1
#' check_exist_ws(a)
#' rm(a)
#' check_exist_ws(a)
check_exist_ws <- function(test_object){
  exists(deparse(substitute(test_object)), envir = .GlobalEnv)
}

#' Write an object out of a function directly into the workspace
#'
#' This function is used to wirte objects from a function directly to the workspace or global environment.
#'
#' @param object The object to be written into the workspace
#' @param overwrite Logical, if the object exists already in the workspace, it will be overwritten if set to \code{TRUE}
#'
#' @return Returns the object in the workspace.
#' @export
#'
write_to_ws <- function(object, overwrite = FALSE){
  exists_in_ws <- exists(deparse(substitute(object)), envir = .GlobalEnv)
  if (exists_in_ws && overwrite == FALSE) {stop("The object '", deparse(substitute(object)), "' already exists in current workspace")}
  assign(deparse(substitute(object)), object, envir=globalenv())
  if (overwrite) {message("Object '", deparse(substitute(object)), "' was overwritten!")}
}




#' Write data frames directly to the working directory
#'
#' The function can be used to write data frames into the working directory (or any other specified path), either as .xlsx or .csv files.
#'
#' @param data The data frame that should be saved
#' @param folder The folder in whoch the data is stored. If NULL, the current working directory is used.
#' @param type The type of the output, either \code{"xlsx} or \code{"csv}
#' @param name The name of the file. If NULL, the name of the object is used.
#' @param overwrite Logical, if \code{TRUE}, the file saved, even if it already exists. Default is to \code{FALSE}.
#'
#' @return The required output as a data file in the specified folder.
#' @export
#'
write_to_wd <- function(data, folder = NULL, type = "xlsx", name = NULL, overwrite = FALSE){
  stopifnot(type == "xlsx" || type == "csv")
  # if no folder is specified, write to wd

  if (is.null(folder)) {
    folder <- getwd()
  }
  if (is.null(name)) {
    name <- deparse(substitute(data))
  }

  check_exist_wd(name, folder=folder)
  if (type == "xlsx") {
    file_name <- paste0(name,".", type)
    if (check_exist_wd(file_name, folder=folder)) {
      stop("The file '", file_name, "' already exists in the chosen working directory:", folder, ". \nPlease set 'overwrite = TRUE' or remove the file before proceeding")
    }
    xlsx::write.xlsx(data, paste0(folder, "/", name, ".", type), row.names=FALSE)
  }
  if (type == "csv") {
    if (check_exist_wd(file_name, folder=folder)) {
      stop("The file '", file_name, "' already exists in the chosen working directory:", folder, ". \nPlease set 'overwrite = TRUE' or remove the file before proceeding")
    }
    write.csv(data, paste0(folder, "/", name, ".", type), row.names=FALSE)
  }
}







