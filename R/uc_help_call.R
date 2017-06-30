################################################################################
#####  2016-01-10

#' Help function for \code{\link{under_construction}}, using a call
#'
#' A help function similar to \code{\link{uc_help}}, but based on a
#' call (typically captured inside of some other function under
#' investigation).
#'
#' @param .uc_call The call we want to investigate, the name of the
#'     function will be extracted from this.
#'
#' @param .save_file A character string, to be used for the name of
#'     the file to which \code{.uc_call} will be saved.  The default
#'     value \code{NULL} will trigger the creation of a file name
#'     based on the function-part of \code{.uc_call}.
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
#' @return This function saves \code{.uc_call} to file and gives a
#'     line of code needed to source it back.  In addition it returns
#'     three chunks of code to simplify the setup when working with
#'     the function \code{under_construction}, i.e. it will create an
#'     argument list containing all the arguments of \code{.fun} with
#'     values from \code{.uc_call} and default values included, and
#'     this list will then be used to feed arguments to the
#'     \code{dotsMethods} argument of \code{under_construction}.  Some
#'     code needed for an inspection of the result is also included.
#'
#' @export


uc_help_call <- function(
    .uc_call,
    .save_file = NULL,
    high_details_level = FALSE,
    clean_workspace = FALSE,
    script_files = "") {
###-------------------------------------------------------------------
    ##  Get the name of '.uc_call' as symbol, for later referencing.
    call_name <- as.symbol(deparse(substitute(.uc_call)))
###-------------------------------------------------------------------
    ##  Investigate the function.
    .fun_info <- fun_info(target = .uc_call)
    ##  Extract the name, package and the functi itself (only from the
    ##  first component).  TASK: Should add a message if more than one
    ##  package is listed, which implies that there might be a risk
    ##  that something unintended might happen as a consequence.
#####  Add code for message...
    fun_name <- .fun_info$name
    .package <- .fun_info$package[1]
    .uc_fun <- .fun_info$functions[[1]]
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Before the call is saved to file, the arguments must be updated
###  to ensure the call will be self-sufficient later on when it's
###  loaded back into the work-flow.  There are two problems that
###  should be taken care of, and that is when the arguments recorded
###  in the call are symbols or calls.  The symbols should be replaced
###  with evaluated versions, and the calls (although it's not
###  strictly required) should be evaluated.  The evaluation of the
###  symbols should be straight forward, whereas the calls will be
###  solved by hijacking '.uc_fun'.  It's alas not possible to fix all
###  by the help of '.uc_fun', since recursive problems occurred.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Find the arguments in '.uc_call' that are themselves calls.
    .args_calls <- vapply(
        X = .uc_call,
        FUN = is.call,
        FUN.VALUE = logical(1))
    ##  Replace the other arguments with evaluated versions (with the
    ##  ordinary adjustment for 'NULL', and an additional twist to
    ##  avoid an error from evaluation ' ').
    for (.arg in tail(x = names(.uc_call)[! .args_calls], n = -1))
        ##  Do noting if 'NULL' is the value, else update.
        if (! is.null(.uc_call[[.arg]])) {
            .uc_call[[.arg]] <-
                if (identical(x = .uc_call[[.arg]],
                              y = bquote())) {
                    bquote()
                } else 
                    eval(.uc_call[[.arg]])
        }
    ##  Add 'dotsMethods'-arguments to '.uc_fun' (if present).
    if (length(.uc_call) > 1)
        update_formals(.fun = .uc_fun,
                       as.list(tail(x = .uc_call,
                                    n = -1)),
                       .list = TRUE)
    ##  Hijack the body of the function.
    .uc_fun_body <- body(.uc_fun)
    .uc_fun_body <- .uc_fun_body[c(1, 1, 1)]
    ##  Add code to capture the environment.
    .uc_fun_body[[2]] <- quote(.arg_env <- capture_env(local = TRUE)) 
    .uc_fun_body[[3]] <- quote(return(.arg_env))
    body(.uc_fun) <- .uc_fun_body
    ##  The function can now return the desired values for the
    ##  arguments that themselves was calls (given that no mandatory
    ##  arguments are missing from '.uc_call').
    .arg_env <- .uc_fun()
    ##  Update the calls-argument from '.uc_call' )
    for (.name in intersect(x = tail(names(.uc_call), n = -1),
                            y = names(.uc_call)[.args_calls]))
        if (is.null(.arg_env[[.name]])) {
            .uc_call[.name] <- list(NULL)
        } else {
            .uc_call[[.name]] <- .arg_env[[.name]]
        }
    kill(.name, .arg_env)
###-------------------------------------------------------------------
##  Save '.uc_call' to a file, create a file name if necessary.
        if (is.null(.save_file))
        .save_file <- paste(
        "uc_help_call__",
            fun_name,
        "__",
        format(Sys.time(), "%Y_%b_%d"),
        ".Rda",
        sep = "")
    ##  Update the desired object to save
    eval(bquote(.(call_name) <- .uc_call))
    ##  Save it with the desired name!
    eval(bquote(save(.(call_name), file = .(.save_file))))
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The creation of the code in the setup for 'under_construction'
###  requires an argument list with proper subsetting of 'uc_call.
###  This is done by updating the formals of '.uc_fun' with a suitable
###  adjusted copy of 'uc_call'.  Note that 'NULL' does not need
###  special treatment in this case, but empty arguments must be
###  replaces with a 'bquote()' for this to work.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Create a copy of '.uc_call' and insert references based on
    ##  positions or names (NB: names might not be present.)
    .uc_call_copy <- .uc_call
    ##  Record names, when present, or introduce empty names.
    .uc_call_names <-
        if (is.null(names(.uc_call))) {
            rep("", length(.uc_call))
        } else
            names(.uc_call)
    ##  Replace argument values with references to '.uc_call'
    if (length(.uc_call) > 1) {
        for (pos in 2:length(.uc_call)) 
            .uc_call_copy[[pos]] <-
                if (.uc_call_names[pos] != "") {
                    bquote(.(call_name)[[.(.uc_call_names[pos])]])
                } else
                    bquote(.(call_name)[[.(pos)]])
        ##  Update the formals of the local functions.
        update_formals(.fun = .uc_fun,
                       as.list(tail(x = .uc_call_copy,
                                    n = -1)),
                       .list = TRUE)
    }
    kill(.uc_call_copy, .uc_call_names, pos)
###-------------------------------------------------------------------
    ##  Create an argument-list for the function.
    arg_list_name <- paste(
        "arg_list_",
        fun_name,
        sep = "")
###-------------------------------------------------------------------
    ##  Record the formals of '.uc_fun', there might be stuff here not
    ##  originating from '.uc_call'.  Make sure that character strings
    ##  of lenght one are returned with quotation marks.
    arguments <- lapply(
        X = formals(.uc_fun),
        FUN = function(x) {
            if (identical(x = x, y = bquote()))
                return(quote(bquote()))
            if (all(is.character(x), length(x) == 1)) {
                deparse(x)
            } else {
                if (identical(x = eval(x), y = bquote())) {
                    quote(bquote())
                } else
                    x
            }
        })
###-------------------------------------------------------------------
    ##  Identify if a 'dotsMethods' argument is present, and position.
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
    ##  Specify line-break and indentation (4 spaces).  Two variants,
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
              "##  Load the call-object '",
              call_name,
              "' into the workspace.\n",
              "load(\"",
              .save_file,
              "\")",
              "\n\n",
              arg_list_name,
              " <- list(",
              lbi,
              ##  Add arguments,  based on 'dotsMethods_last_argument'
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
###  Add the function
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
###  Add code to "protect" the call from deletion.
              comma_lbi,
              "'do not delete me' = ",
              call_name,
              ")",
              "\n\n",
###  Add code for inspection of the result.
              "ls(search()[2], all.names = TRUE)",
              "\n\n",
              "str(",
              .result_name,
              ")",
              "\n\n",
###  Add code for removal of file, when finished testing.
              "##  unlink(\"",
              .save_file,
              "\")",
              "\n\n",
###  Add code for the cleanup.
              "uc_cleanup()",
              "\n\n",
              sep = ""))
}
    
