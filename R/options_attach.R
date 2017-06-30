################################################################################
#'
#' Attachment with update of options
#'
#' This function is a wrapper around \code{attach}, that in addition
#' performs an update of the options.  The idea is that an option can
#' store the names of several attached objects, and latter on when
#' it's time to clean up it should be sufficient to specify the
#' relevant option in order to \code{detach}.  Note that the function
#' performs a sanity-check to ensure that no options set by other
#' packages should be affected.  The defaults for those arguments that
#' are to be sent to \code{attach}, are inherited from that function.
#'
#' @param what The object to attach.  Note: This argument will be sent
#'     to the \code{what}-argument of \code{attach}.
#'
#' @param pos The position in \code{search()} at which the object will
#'     be attached.  Note: This argument will be sent to the
#'     \code{pos}-argument of \code{attach}.
#'
#' @param name The name to use for the attached object.  Names
#'     starting with \code{package:} are reserved for \code{library}.
#'     If the search path already has an object named \code{name},
#'     then a minor adjustment will be made to ensure that the
#'     cleaning done by \code{options_detach} doesn't remove the wrong
#'     alternative.  Note: This argument will be sent to the
#'     \code{name}-argument of \code{attach}.
#'
#' @param warn.conflicts Logical argument.  When this is \code{TRUE},
#'     warnings will be printed about conflicts from attaching
#'     \code{what}.  A conflict is a function masking a function, or a
#'     non-function masking a non-function.  Note: This argument will
#'     be sent to the \code{warn.conflicts}-argument of \code{attach}.
#'
#' @param options_name A character string that specifies the name of
#'     the option to be used when storing the names of the attached
#'     environments.  Note that \code{options_name} will be tested
#'     against options added by other packages, and that an error will
#'     be called if there's an overlap - in particular, this function
#'     will not mess up any options set by other packages.  A
#'     "stacking" procedure will be used if \code{options_name}
#'     already has been created before (by this function), and the way
#'     the stacking is done is governed by the
#'     \code{options_front}-argument.
#' 
#' @param options_front A logical value, default \code{FALSE}, that
#'     decides how a new \code{name} should be added to
#'     \code{options_name} (when an earlier use of
#'     \code{options_attach} already created an occurrence of
#'     \code{options_name}).
#'
#' @param replace A logical value, default \code{TRUE}. This can be
#'     used to check that earlier versions of the attached environment
#'     (with the same combination of \code{name} and
#'     \code{options_name}) will be detached before the new
#'     environment specified by \code{what} is added.
#'
#' @return When successful, the function will attach the object
#'     specified in \code{what} and add its \code{name} to the option
#'     \code{options_name} as specified by \code{options_front}.
#'     Depending on the value of \code{replace}, any old attached
#'     environment with the same name as the present one will be
#'     replaced.  Note: No replacement will be done if the new and the
#'     old environment contains the same content.  If the positional
#'     order on the search-path is important, that might require that
#'     the existing environment has to be detached first.
#'
#' @export

##  Reminder: Formals updated at the end with defaults from `attach`.
options_attach <- function(what,
                           pos,  
                           name,
                           warn.conflicts,
                           options_name,
                           options_front = TRUE,
                           replace = TRUE) {
###-------------------------------------------------------------------
    ##  Sanity-check the main option for the package.
    .main_name <- names(UC_defaults$options$onload)
    .main_option <- getOption(.main_name)
    if (is.null(.main_option))
        error("A serious problem was detected!",
              c("This function requires the existence of an option",
                sQuote(.main_name),
                "that should have been set when the package was loaded."),
              c("This options is missing!"))
######  TASK: Create a minor helper `this_package` to deal with the
######  specification of the name of the package.
###-------------------------------------------------------------------    
    ##  Sanity-check that `options_name` has been given.
    if (missing(options_name))
        error(.argument = "options_name",
              c("This argument must be specified.  Use",
                sQuote("attach"),
                "if no update of the options are needed."))
    ##  Sanity-check that `options_name` is valid.
    if (! all(c(is.character(options_name),
                length(options_name) == 1)))
        error(.argument = "options_name",
              c("This argument must be a single character-string."))
    ##  Sanity-check that `options_name` doesn't start with `package:`
    if (str_detect(string = options_name,
                   pattern = "^package:"))
        error(.argument = "options_name",
              c("This argument can't start with",
                sQuote("package:")))
###-------------------------------------------------------------------
    ##  Sanity-check that `options_name` avoids the existing options
    ##  that was detected when the package was loaded.
    .old_attach_detach_op <- .main_option$attach_detach
    .avoid_these <- setdiff(
        x = names(options()),
        y = .old_attach_detach_op)
    if (any(.avoid_these == options_name))
        error(.argument = "options_name",
              c("The option",
                sQuote(options_name),
                "already exists (not set by this function).",
                "Please select another value."))    
    kill(.avoid_these)
###-------------------------------------------------------------------
    ##  In order to take into account the argument `replace`, it's
    ##  necessary to figure out the existing content of `options_name`
    ##  (this can be `NULL` if nothing has been stored yet).
    .old_options_content <- getOption(x = options_name)
#####  TASK: The present solution is good enough to resolve the cases
#####  I want to do in my code, but it's not really a general
#####  solution, since it will fail if some nitwit has used the
#####  function a few times with `replace` given as `FALSE`.
    if (replace) 
        ##  Check if there's a match.  If so, check if the new and old
        ##  environment in fact could be considered to be equal.
        if (name %in% .old_options_content) {
            .old_env <- as.environment(name)
            if (equal_environments(env1 = what,
                                   env2 = .old_env)) {
                ##  Same content > no need to do anything.
                return(invisible())
            } else
                ##  Different content > get rid of the old.
                eval(bquote(detach(name = .(name))))
        }
###-------------------------------------------------------------------
    ##  Check if any other components on the search-path starts with
    ##  the name `name`.
#####  TASK: This is perhaps something that should be redone in some
#####  other manner, due  to the new argument `replace`.
    .search_path <- search()
    .matches <- sum(str_detect(
        string = .search_path,
        pattern = paste("^", name, sep = "")))
    ##  If necessary adjust to get a unique name.
    if (.matches > 0)
        name <- paste(name, "___v", .matches + 1, sep = "")
###-------------------------------------------------------------------
    ##  Attach the desired object.
    do.call(what = "attach",
            args = list(what = what,
                        pos = pos,
                        name = name,
                        warn.conflicts = warn.conflicts))
    kill(what, pos, warn.conflicts)
###-------------------------------------------------------------------
    ##  Based on `options_front`, create new value.
    .new_options_content <- c(
        if (options_front)
            name,
        .old_options_content,
        if (! options_front)
            name)
    ##  Create the call to use, and reset the name to the correct one.
    .options_call <- call(name = "options",
                          .dummy_name = .new_options_content)
    names(.options_call)[2] <- options_name
    ##  Evaluate `.options_call`
    eval(.options_call)
    kill(.old_options_content, .new_options_content, .options_call)
###-------------------------------------------------------------------
    ##  Update `main_option` with the new information.
    .main_option$attach_detach <- unique(c(
        .old_attach_detach_op,
        options_name))
    .main_option_call <- call(name = "options",
                              .dummy_name = .main_option)
    names(.main_option_call)[2] <- .main_name
    ##
    eval(.main_option_call)
###-------------------------------------------------------------------
    ##  Return invisible `NULL`
    invisible(NULL)
}

##  Update the formals with the defaults from `attach`
for (.arg in names(formals(attach)))
    formals(options_attach)[[.arg]] <-
        formals(attach)[[.arg]]
rm(.arg)
