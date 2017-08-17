

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
  valid_stems <- t[t >= 1]
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
#' @param show_origin Logical, if \code{TRUE}, and additional variable will be created showing from which variables the values originated. Default is to \code{FALSE}.
#'
#' @return Returns a data frame in long format
#' @export
#'
#' @examples
#' gather_one(data=wide_example_data, key_vars = "id", varying = c("var_p_1", "var_p_2"), new_name = "var")
gather_one <- function(data, key_vars, varying, keep_others = FALSE, new_name = NULL, show_origin = FALSE){
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
  if (show_origin) {
    gathered$group_id <- NULL
  }
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
#' @param type The type of the output, either \code{"xlsx"} or \code{"csv"}
#' @param name The name of the file. If NULL, the name of the object is used.
#' @param overwrite Logical, if \code{TRUE}, the file saved, even if it already exists. Default is to \code{FALSE}.
#'
#' @return The required output as a data file in the specified folder.
#' @export
#'
write_to_wd <- function(data, folder = NULL, type = "xlsx", name = NULL, overwrite = FALSE){
  stopifnot(type == "xlsx" || type == "csv")

  if (is.null(folder)) {
    folder <- getwd()
  }
  if (is.null(name)) {
    name <- deparse(substitute(data))
  }

  file_name <- paste0(name,".", type)
  if (type == "xlsx") {
    if (check_exist_wd(file_name, folder=folder) && overwrite == FALSE) {
      stop("The file '", file_name, "' already exists in the chosen working directory:", folder, ". \nPlease set 'overwrite = TRUE' or remove the file before proceeding")
    }
    xlsx::write.xlsx(data, paste0(folder, "/", name, ".", type), row.names=FALSE)
  }
  if (type == "csv") {
    if (check_exist_wd(file_name, folder=folder) && overwrite == FALSE) {
      stop("The file '", file_name, "' already exists in the chosen working directory:", folder, ". \nPlease set 'overwrite = TRUE' or remove the file before proceeding")
    }
    write.csv(data, paste0(folder, "/", name, ".", type), row.names=FALSE)
  }
}

#' Turns factors to character strings in a data frame
#'
#' @param data The data frame that should be converted
#'
#' @return A data frame in which all factors are converted to character strings.
#' @export
#'
#' @examples
#' data <- data.frame(a = c(1, 2, 3), b = as.factor(c(a, b, c)))
#' fac_to_chr(data)
fac_to_chr <- function(data){
  i <- sapply(data, is.factor)
  data[i] <- lapply(data[i], as.character)
  if (any(i)) {
    msg_part <- paste0("- ", names(i[i]), sep = "\n")
    message("The following variables were converted from factor to string\n", msg_part)
    return(data)
  }
  return(data)
}


#' Compute summary statistics within groups in a nested data frame
#'
#' This function allows the computation of some summary statistics within groups/persons or other clusters. The data needs to be in a long format. At least one variable needs to be a grouping-variable, e.g. \code{id}. Other variables of the data frame should be numeric.
#'
#' @param data The data frame in long-format that contains the variables to be analysed
#' @param id Character string. The id or grouping variable, in which other observations are nested
#' @param remove_var Character string. Variables that should be removed before the computation proceeds (otherwise, the function assumes that all other variables should be used for the computation)
#' @param prefix_out The prefix for the variables that are returned. Default is \code{NULL}. If \code{NULL}, the input from \code{id} is used as prefix.
#' @param intake_var Character string. Specific variables (e.g. names of these variables) for which the ananylses should be run, i. e. if only a subset of variables should be used.
#' @param out_values The values to be returned. Can be either
#' \describe{
#'   \item{\code{"mean"}}{Computes the mean within each group/id. Missing values are removed before computation.}
#'   \item{\code{"sd"}}{Computes the sd each group/id. Missing values are removed before computation.}
#'   \item{\code{"count"}}{Computes the number of cases within each group/id, including missings.}
#'   \item{\code{"sum"}}{Computes the sum of values within each group/id. Missing values are removed before computation.}
#'   \item{\code{"missing"}}{Counts the number of missing values in each group/id.}
#'   \item{\code{"cor"}}{Computes the within-correlation for each variable within each group/id. Pearson correlation (\code{\link{cor}}) with \code{"pairwise.complete.obs"} is used.}
#'   \item{\code{"max"}}{Returns the maximum in each group/id, ignoring NAs.}
#'   \item{\code{"min"}}{Returns the minimum in each group/id, ignoring NAs.}
#'   \item{\code{"true"}}{Per group/id, returns 0 if the group/id contains any other value than 0, otherwise returns 1. Missings are ignored.}
#' }
#'
#' @return Returns a data frame in wide format (i. e. one row per group/id). Variable names are the original variable names with a correspondng prefix and an underscore (e. g. \code{mean_} for the mean). For the correlations, the names of the two variables that are correlated with each other are pasted together and the prefix \code{cor} is added, e. g. \code{cor.var_1.var_2} for the correlation between \code{var_1} and \code{var_2}.
#' @export
#'
#' @examples
#' df <- aggregate_df(wide_example_data, id="id")
#' head(df)
#' data <- ssd.day
#' id = "PAR.ID"
aggregate_df <- function(data, id, remove_var = NULL,
                         prefix_out = NULL,
                         intake_var = NULL,
                         out_values = c("mean", "sd", "count", "sum", "missing", "cor", "min", "max", "true")){

  stopifnot(is.character(id))
  stopifnot(is.character(intake_var) || is.null(intake_var))
  stopifnot(is.data.frame(data))

  if (is.null(prefix_out)) {prefix_out <- id}

  if (is.null(intake_var)) {
    compute_var <- setdiff(names(data), c(id, remove_var))
  } else {
    compute_var <- intake_var
  }

  data <- padis::fac_to_chr(data[c(id,compute_var)])

  ## check that there are only numerics in the data frame
  i <- sapply(data[compute_var], is.numeric)
  if (any(!i)) {
    to_remove <- paste0("- ", names(i[!i]), collapse = "\n")
    message("Some non-id variables were not numeric and where therefore removed before the computation could proceed: \n", to_remove)
    compute_var <- names(i[i])
  }

  if (length(compute_var) == 1 && "cor" %in% out_values) {
    out_values[out_values == "cor"] <- NA
    out_values <- c(na.omit(out_values))
    message("Since only one numeric variable was used, no correlation can be computed")
  }


  ## if prefixes have to be changed, then change them here
  unique_id <- unique(data[,id])

  if (any(is.na(unique_id)) ) {
    message("The id-variable contains missings. Computation proceeded, but please check.")
    unique_id <- na.omit(unique_id)
  }

  group <- data.frame(unique_id, stringsAsFactors = FALSE)
  names(group) <- id
  df <- group

  if ("mean" %in% out_values) {
    within_mean <- aggregate(data[, compute_var], list(data[,id]), function(x) mean(x, na.rm = TRUE))[-1]
    names(within_mean) <- paste0(prefix_out, ".", names(within_mean), ".mean")
    data.table::setnames(within_true, old = paste0(prefix_out, ".Group.1.true"), new = id)
    df <- merge(df, within_true, by = id)
  }
  if ("sd" %in% out_values) {
    within_sd <- aggregate(data[, compute_var], list(data[,id]), function(x) sd(x, na.rm = TRUE))[-1]
    names(within_sd) <- paste0(prefix_out, ".", names(within_sd), ".sd")
    data.table::setnames(within_true, old = paste0(prefix_out, ".Group.1.true"), new = id)
    df <- merge(df, within_true, by = id)
  }
  if ("sum" %in% out_values) {
    within_sum <- aggregate(data[, compute_var], list(data[,id]), function(x) sum(x, na.rm = TRUE))[-1]
    names(within_sum) <- paste0(prefix_out, ".", names(within_sum), ".sum")
    data.table::setnames(within_true, old = paste0(prefix_out, ".Group.1.true"), new = id)
    df <- merge(df, within_true, by = id)
  }
  if ("count" %in% out_values) {
    within_n <- aggregate(data[, compute_var], list(data[,id]), function(x) sum(!is.na(x)))[-1]
    names(within_n) <- paste0(prefix_out, ".", names(within_n), ".n")
    data.table::setnames(within_true, old = paste0(prefix_out, ".Group.1.true"), new = id)
    df <- merge(df, within_true, by = id)
  }
  if ("missing" %in% out_values) {
    within_na <- aggregate(data[, compute_var], list(data[,id]), function(x) sum(is.na(x)))[-1]
    names(within_na) <- paste0(prefix_out, ".", names(within_na), ".na")
    data.table::setnames(within_true, old = paste0(prefix_out, ".Group.1.true"), new = id)
    df <- merge(df, within_true, by = id)
  }
  if ("max" %in% out_values) {
    max_na <- function(x){ ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))}
    within_max <- aggregate(data[, compute_var], list(data[,id]), function(x) max_na(x))[-1]
    names(within_max) <- paste0(prefix_out, ".", names(within_max), ".max")
    data.table::setnames(within_true, old = paste0(prefix_out, ".Group.1.true"), new = id)
    df <- merge(df, within_true, by = id)
  }
  if ("min" %in% out_values) {
    min_na <- function(x){ ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))}
    within_min <- aggregate(data[, compute_var], list(data[,id]), function(x) min_na(x))[-1]
    names(within_min) <- paste0(prefix_out, ".", names(within_min), ".min")
    data.table::setnames(within_true, old = paste0(prefix_out, ".Group.1.true"), new = id)
    df <- merge(df, within_true, by = id)
  }
  if ("true" %in% out_values) {

    # apply(sub, 2, function(x) check_0(x, na.rm = TRUE))
    # aggregate(data[, compute_var], list(data[,id]), function(x) check_0(x, na.rm = TRUE))[-1]
    check_0 <- function(x, na.rm = TRUE){
      if (na.rm) x <- na.omit(x)
      ifelse(any(x != 0), 1, 0)
    }
    within_true <- aggregate(data[, compute_var], list(data[,id]), function(x) check_0(x, na.rm = TRUE))
    names(within_true) <- paste0(prefix_out, ".", names(within_true), ".true")
    data.table::setnames(within_true, old = paste0(prefix_out, ".Group.1.true"), new = id)
    df <- merge(df, within_true, by = id)
  }


  # correlations
  if ("cor" %in% out_values) {
    get_cor <- function(i_id, data, compute_var, id_column){
      id_data <- data[data[,id_column] == i_id,]
      m <- stats::cor(id_data[compute_var], use = "pairwise.complete.obs") ### arguments passed on to cor
      # taken from
      # https://stackoverflow.com/questions/12116207/flatten-matrix-in-r-to-four-columns-indexes-and-upper-lower-triangles
      ut <- upper.tri(m)
      cor_df <- data.frame(i = rownames(m)[row(m)[ut]],
                           j = rownames(m)[col(m)[ut]],
                           cor = t(m)[ut]) ## could add p-values here
      names <- paste(prefix_out, cor_df[,"i"], cor_df[,"j"], "cor", sep = ".")
      cors <- cor_df[,"cor"]
      names(cors) <- names
      cors
    }
    id_correlations <- purrr::map(unique_id, ~ get_cor(i_id = ., data = data, compute_var = compute_var, id_column=id))
    within_cor <- data.frame(do.call("rbind", id_correlations))
    df <- cbind(df, within_cor)
  }
  return(df)
}


#' Merge One Data Frame Into Several Data Frames
#'
#' @param data_from The data frame from which to merge
#' @param id_var The id or group-variable to merge by
#' @param data_list_to A list of data frames into which the data frame \code{data_from} is merged into
#' @param merge_down Logical, should the \code{data_from} be duplicated in order to merge down (\code{TRUE}), or should the duplicate rows be removed first in order to merge up? (\code{FALSE}). Default is to \code{TRUE}
#' @param select_vars A character vector of the variables that should be selected from the data frame \code{data_from}. If NULL, all variables are selected and merged into the other data frames in \code{data_list_to}
#'
#' @return A list containing data frames with the same length as \code{data_list_to}.
#' @export
#'
#' @examples
#' ## simulate data
#' # long data
#'data_long_1 <- data.frame(PAR.ID = letters[1:10],
#'                          var1 = sample(1:10, 10),
#'                          var2 = sample(1:10, 10))
#'data_long_2 <- data.frame(PAR.ID = sort(rep(letters[1:10], 3)),
#'                          var1 = sort(rep(1:10, 3)),
#'                          var2 = sort(rep(11:20, 3)))
#'data_long_2_er <- data.frame(PAR.ID = sort(rep(letters[1:10], 3)),
#'                          var1 = sort(rep(1:10, 3)),
#'                          var2 = sort(rep(11:20, 3)))
#'data_long_2_er[3, "var2"] <- 12 ## add a variable that is not the same as the other variables
#'data_long_3 <- data.frame(PAR.ID = rep(letters[1:10], 4),
#'                          var1 = sample(1:40, 40),
#'                          var2 = sample(1:40, 40))
#'# short data
#'data_short_2 <- data.frame(PAR.ID = rep(letters[1:10], 1),
#'                           var1 = sample(1:40, 10),
#'                           var2 = sample(1:40, 10))
#'data_short_3 <- data.frame(PAR.ID = rep(letters[1:10], 1),
#'                           var1 = sample(1:40, 10),
#'                           var2 = sample(1:40, 10))
#'## merge down, i. e. duplicate rows when going from a higher data set (e.g .level-2) to a lower data set (e.g. level-1)
#'merge_multiple_df(data_long_1, id_var="PAR.ID", list(data_long_2, data_long_3), merge_down = TRUE)
#'
#'## merge up, i. e. make the longer data frame short first and merge then
#'merge_multiple_df(data_from = data_long_2, id_var="PAR.ID", data_list_to = list(data_short_2, data_short_3), merge_down = FALSE)
#'## merge up, i. e. make the longer data frame short first and merge then and give a warning if duplicates arise
#'merge_multiple_df(data_from = data_long_2_er, id_var="PAR.ID", data_list_to = list(data_short_2, data_short_3), merge_down = FALSE)
merge_multiple_df <- function(data_from, id_var, data_list_to, merge_down = TRUE, select_variables = NULL) {

  if(!is.null(select_variables)) {
    data_from <- padis::select_vars(data=data_from, ids=id_var, prefix=select_variables)
  }

  stopifnot(is.data.frame(data_from))
  stopifnot(is.logical(merge_down))

  if (!merge_down) {
    data_from <- data_from[!duplicated(data_from),]
    if (any(table(data_from[id_var]) > 1)) {
      which_not_unique <- names(which(table(data_from[id_var]) > 1))
      which_not_unique <- paste0(which_not_unique, collapse ="\n")
      warning("the following ids contain do not contain unique values in their variables: \n", which_not_unique)
    }
  }
  purrr::map(data_list_to, ~ dplyr::full_join(data_from, ., by = id_var))
}


