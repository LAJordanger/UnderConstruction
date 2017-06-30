################################################################################
#####  2015-01-14

#' Help function for \code{\link{under_construction}}
#'
#' It's a pain to write out manually the required "argument-list"
#' approach for the use of \code{under_construction}, but this
#' function can be used to fix that.
#'
#' @param .uc_fun  The function we want to investigate.
#'
#' @param high_details_level Logic value, default \code{FALSE}, to be
#'     delivered to \code{under_construction}.
#' 
#' @param clean_workspace Logic value, default \code{FALSE}, to be
#'     delivered to \code{under_construction}.
#'
#' @param script_files Character vector, default \code{""} (i.e. an
#'     empty string), to be delivered to \code{under_construction}.
#'
#' @return This function returns some lines of text, that gives three
#'     chunks of code to simplify the setup when working with the
#'     function \code{under_construction}, i.e. it will create an
#'     argument list containing all the arguments of \code{.fun} with
#'     the default values included, and this list will then be used to
#'     feed arguments to the \code{dotsMethods} argument of
#'     \code{under_construction}.  Code needed for an inspection of
#'     the result is also included.
#'
#' @export


uc_help <- function(
    .uc_fun,
    high_details_level = FALSE,
    clean_workspace = FALSE,
    script_files = "") {
###-------------------------------------------------------------------
    ##  Investigate the function.
    .fun_info <- fun_info(target = deparse(substitute(.uc_fun)))
    ##  Extract the name and package, the function itself and the
    ##  formals (only from the first component).  TASK: Should add a
    ##  message if more than one package is listed, which implies that
    ##  there might be a risk that something unintended might happen
    ##  as a consequence.
#####  Add code for message...
    fun_name <- .fun_info$name
    .package <- .fun_info$package[1]
    .uc_fun <- .fun_info$functions[[1]]
    arguments <- .fun_info$formals[[1]]
###-------------------------------------------------------------------
    ##  Create a name for the argument-list based on 'fun_name'.
    arg_list_name <- paste(
        "arg_list_",
        fun_name,
        sep = "")
###-------------------------------------------------------------------
    ##  Identify any character-valued default-values.
    character_arguments <- vapply(X = arguments,
                                  FUN = is.character,
                                  FUN.VALUE = logical(1))
    ##  Adjust those arguments to ensure correct result.
    for (.arg in names(arguments[character_arguments]))
        arguments[[.arg]] <- deparse(arguments[[.arg]])
    ##  Identify if a 'dotsMethods' argument is present (and if so,
    ##  find the position of it).
    any_dotsMethods <- any(names(arguments) == "...")
    pos_dotsMethods <- ifelse(
        test = any_dotsMethods,
        yes  = which(names(arguments) == "..."),
        no   = -1)
    ##  "Hide" 'dotsMethods', when present.
    if (any_dotsMethods)
        names(arguments)[pos_dotsMethods] <- "##  ..."
    ##  Check if dotsMethods are the last argument of '.uc_fun'.
    dotsMethods_last_argument <- length(arguments) == pos_dotsMethods
###-------------------------------------------------------------------
    ##  Specify line-break and indentation (4 spaces).  To variants,
    ##  starting with and without a comma.
    lbi <- "\n    "
    comma_lbi <- paste(",", lbi, sep = "")
    ##  Create a default name for the assignment of a successful
    ##  computation (after the testing has been finished)
    .result_name <- paste(
        ".tmp_uc_",
        fun_name,
        sep = "")
###-------------------------------------------------------------------
    ##  Return to the workflow the desired chunks of code.
    cat(
        paste("\n",
              arg_list_name,
              " <- list(",
              lbi,
              ##  Adjustment based on 'dotsMethods_last_argument'
              ## paste(paste(head(x = names(arguments),
              ##                  n = length(arguments) - dotsMethods_last_argument),
              ##             arguments,
              ##             sep = " = "),
              ##       collapse = comma_lbi),
              if (pos_dotsMethods != 1)
                  paste(paste(head(x = names(arguments),
                                   n = length(arguments) - dotsMethods_last_argument),
                              head(x = arguments,
                                   n = length(arguments) - dotsMethods_last_argument),
                              sep = " = "),
                        collapse = comma_lbi),
              ")",
              if (dotsMethods_last_argument)
                  paste("\n",
                        tail(x = names(arguments),
                             n = 1),
                        " = )",
                        sep = ""),
              "\n\n",
              ##  Create a default name for the result
              .result_name,
              " <- ",
              ##  Write the code for "under_construction".
              "under_construction(",
              lbi,
###  Add the function.
              ".uc_fun = ",
              fun_name,
              comma_lbi,
###  Add the reference to the package.
              ".uc_package_name = ",
              deparse(.package),
              comma_lbi,
###  Add the references to the argument-list
              paste(paste(names(arguments),
                          paste(arg_list_name,
                                names(arguments),
                                sep = "$"),
                          sep = " = "),
                    collapse = comma_lbi),
              comma_lbi,
###  Add the other arguments
              "high_details_level = ",
              high_details_level,
              comma_lbi,
              "clean_workspace = ",
              clean_workspace,
              comma_lbi,
              "script_files = ",
              ifelse(test = identical(script_files, ""),
                     yes  = "\"\"",
                     no   = script_files),
              ")",
              "\n\n",
###  Add some code for inspection of the result.
              "ls(search()[2], all.names = TRUE)",
              "\n\n",
              "str(",
              .result_name,
              ")",
              "\n\n",
###  Add code for the cleanup.
              "uc_cleanup()",
              "\n\n",
              sep = ""))
}
