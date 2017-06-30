################################################################################
#'
#' Detachment based on options
#'
#' This function does the opposite of \code{options_attach}, that is to
#' say, it can use the options created by \code{options_attach} to
#' detach the specified components from the search-path.
#'
#' @param cleanup Logical value, default \code{FALSE}. This can be
#'     used to get rid of all the stuff that \code{options_attach}
#'     added to the search-path.  The \code{target}-argument will be
#'     ignored when \code{cleanup} is given as \code{TRUE}, and does
#'     not need to be specified in that case.
#'
#' @param target A character string that has to be one of the options
#'     set by \code{options_attach}.
#'
#' @return The result of this function will be that components added
#'     to the search-path by \code{options_attach} will be detached.
#'
#' @export


options_detach <- function(cleanup = FALSE,
                           target) {
###-------------------------------------------------------------------
    ##  Sanity-check the main option for the package.
    .main_name <- names(UC_defaults$options$onload)
    .main_option <- getOption(.main_name)
    .stored_options <- .main_option$attach_detach
    if (is.null(.main_option))
        error("A serious problem was detected!",
              c("This function requires the existence of an option",
                sQuote(.main_name),
                "that should have been set when the package was loaded."),
              c("This options is missing!"))
######  TASK: Create a minor helper `this_package` to deal with the
######  specification of the name of the package.
###-------------------------------------------------------------------
    ##  Prepare information to be used in this function.
    .overview <- structure(
        .Data = lapply(
            X = .stored_options,
            FUN = function(x) {
                options()[[x]]
            }),
        .Names = .stored_options)
    ##  Prepare informative text about existing values.
    .details <-
        if (length(.stored_options) == 0) {
            c("Note: Nothing has been registered that can be",
              " detached by this function...")
        } else 
            c(paste("The following option",
                    ifelse(
                        test = length(.stored_options) != 1,
                        yes  = "s are ",
                        no   = " is "),
                    "available, corresponding to the attached components ",
                    "as given below:",
                    sep = ""),
              lapply(
                  X = sort(names(.overview)),
                  FUN = function(x) {
                      paste("Option: ",
                            sQuote(x),
                            " > ",
                            paste(sQuote(.overview[[x]]),
                                  collapse = ", "),
                            sep = "")
                  }),
              paste("Hint: Set ",
                    sQuote("cleanup"),
                    " equal to ",
                    sQuote("TRUE"),
                    " if you want to remove everything.",
                    sep = ""))
###-------------------------------------------------------------------
    ##  Sanity check that arguments are given, and return an error
    ##  with details about available arguments.
    if (all(missing(target),
            ! cleanup)) 
        error(.argument = "target",
              "No argument detected!",
              .details)
###-------------------------------------------------------------------
    ##  Sanity check that `target` has the right properties, but only
    ##  when `cleanup` is `FALSE`
    if (! cleanup) {
        if (! all(c(is.character(target),
                    length(target) == 1)))
            error(.argument = "target",
                  "This argument must be a single character-string.",
                  .details)
###-------------------------------------------------------------------
##  Sanity-check that `target` matches an available option.
        if (! target %in% .stored_options)
            error(.argument = "target",
                  paste("The given value ",
                        sQuote(target),
                        " doesn't match any of the stored options.",
                        sep = ""),
                  .details)
    } else
        ##  Select everything as `target`
        target <- .stored_options
    kill(.details, .overview, cleanup)
###-------------------------------------------------------------------
    ##  Detach stuff.
    for (.target in target) {
        for (.x in options()[[.target]])
            ##  Use `try`-wrapping to prevent an error if the user for
            ##  some reason already has performed a manual detachment.
            try(expr = eval(bquote(detach(.(.x)))),
                silent = TRUE)
        ##  Remove `.target` from the options.
        .options_call <- call(name = "options",
                              .dummy_name = NULL)
        names(.options_call)[2] <- .target
        ##  Evaluate `.options_call`
        eval(.options_call)
    }
    kill(.target, .x, .options_call)
    ##  Update the value stored in the main option.
    .main_option$attach_detach <- setdiff(
        x = .stored_options,
        y = target)
    .main_option_call <- call(name = "options",
                              .dummy_name = .main_option)
    names(.main_option_call)[2] <- .main_name
    ##
    eval(.main_option_call)
###-------------------------------------------------------------------
    ##  Return invisible `NULL`
    invisible(NULL)
}
