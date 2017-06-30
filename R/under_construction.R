################################################################################

#' Function to construct/revise functions
#'
#' Use \code{under_construction} to simplify the task of creating and
#' updating your code.  Warning: To get the full advantage of this
#' function, the logic argument \code{clean_workspace} should be
#' changed from its default value \code{FALSE} to \code{TRUE}.
#'
#' @details The idea of this function is to ease the construction and
#'     maintenance of your code, but it must be used in tandem with
#'     \code{capture_env} to reach this goal.  The code
#'     \code{capture_env()} should be included in the targeted
#'     function, in order to create a (cloned) copy of that functions
#'     environment in the workspace (the global environment).  The
#'     contribution of \code{under_construction} is to add the
#'     arguments of the targeted function (only when necessary).  It
#'     will also take care of \code{attach} and \code{detach} on the
#'     above mentioned environment, and it will list the content of
#'     the environment.  In addition, if the logic argument
#'     code{clean_workspace} is set to its recommended value
#'     \code{TRUE}, then the workspace will be cleansed for
#'     superfluous stuff at every running.
#'
#' @param .uc_fun The function that we want to construct.
#'
#' @param .uc_package_name The name of a package, that must be available on
#'     the search-path. Use this argument if \code{.uc_fun} is a part
#'     of a package, and the code need access to functions imported
#'     from other packages or to the internal objects of the package.
#'     The default value \code{NULL} will assume that there's no need
#'     for such additional information.
#'
#' @param ...  Arguments to be used by targeted function \code{.fun}.
#'     Note that \code{update_formals} is used in order to feed the
#'     arguments to \code{.fun}, and it's thus possible to add extra
#'     objects here if you want to protect them from annihilation when
#'     \code{clean_workspace} is set to true.
#'
#' @param high_details_level Logic argument with default value
#'     \code{FALSE}, in which case only the names of the objects in
#'     the captured function environment will be presented.  If
#'     \code{high_details_level} is set to \code{TRUE}, then
#'     \code{ls.str} will be used instead to give more details about
#'     the captured objects.
#'
#' @param clean_workspace Logic argument that should be set to
#'     \code{TRUE}, but whose default value nevertheless is
#'     \code{FALSE} (since some users might become a tad bit enervated
#'     if their precious workspace suddenly is wiped clean without any
#'     warning). When set to \code{TRUE}, the only objects in your
#'     workspace that will survive is those specified by \code{.fun}
#'     and the \code{dotsMethods}, i.e. \code{...}.
#'
#' @param script_files Use this if you need to run some scripts in
#'     order to get \code{.uc_fun} to work properly. Default value
#'     \code{""}, in which case nothing is done.  Note that this
#'     feature can increase the computational time quite a bit if the
#'     scripts are large.  If the scripts are mostly static, i.e. that
#'     they are not updated with new stuff while working with you
#'     targeted function, then it might be a better idea to source
#'     them (when needed ) outside of this function.
#'
#' @return When used in tandem with \code{capture_env}, this function
#'     will list and attach the objects that function captured.  If
#'     \code{clean_workspace} is set to \code{TRUE}, there will also
#'     be a cleansing of the workspace.  If there's no
#'     \code{capture_env()} inside the body of the targeted function,
#'     then the function will run as if called directly, and if it
#'     does not fail any results from it will be returned via
#'     \code{under_construction}.
#'
#' @export


#####  TASK: The present approach should hopefully work for functions
#####  written according to the way I do them, but in general I doubt
#####  this will be sufficient.  The problem is that a function might
#####  look up a value from a previously defined call/environment,
#####  without that being explicitly used as an argument in the
#####  function.  I think that it should be possible to get around
#####  this by a "meta-inspection" of the function-body containing the
#####  'capture_env' and compare the stuff found at higher levels with
#####  the stuff used in that function -- take care to make an
#####  exception for names that also occurs in the formals of the
#####  function.


#####  TASK, 2015-02-25: The present solution in this function is not
#####  quite satisfactory when it's used upon functions that calls
#####  themselves. It seems to be the case that the modified function
#####  used inside of this function doesn't understand that it's that
#####  one it should call - and bizarre behaviour results.  For the
#####  time being, I can't waste time on this, but I think it should
#####  be possible to get around this.  However, I should probably
#####  also make an adjustment in 'capture_env' in order to simplify
#####  the use of this when a function called by the function under
#####  inspection also might need an investigation at the same time.

under_construction <- function(
    .uc_fun,
    .uc_package_name = NULL,
    ...,
    high_details_level = FALSE,
    clean_workspace = FALSE,
    script_files = "") {
###-------------------------------------------------------------------
    ##  Capture the arguments to this function, including test of the
    ##  sanity of stuff given to '...'.
    spy_report <- spy()
###-------------------------------------------------------------------
    ##  Sanity check function '.uc_fun'.
    if (mode(.uc_fun) != "function")
        stop("\t",
             "In 'under_construction', \n\t",
             "the argument '.uc_fun', '",
             substitute(.uc_fun),
             "' is not a function.",
             call. = FALSE)
###-------------------------------------------------------------------
    ##  Sanity check of paths given in 'script_files'
    if (! identical(script_files, "")) {
        ## Check validity of files, relative working-directory.
        valid_path <- file.exists(script_files)
        if (! prod(valid_path))
            stop("\t",
                 "In 'under_construction', \n\t",
                 "Relative the working directory \n\t'",
                 getwd(),
                 "',\n\t",
                 "the following script-files where not found \n\t",
                 paste(
                     paste("'",
                           script_files[! valid_path],
                           "'",
                           sep = ""),
                     collapse = "\n\t"),
                 call. = FALSE)
###-------------------------------------------------------------------
        ##  Run the scripts.
        for (script in script_files)
            eval(expr = source(script),
                 envir = .GlobalEnv)
#####  TASK: This works, but it would be nice to turn of a lot of the
#####  "noise" generated by some of the commands.  This can be a
#####  low-priority task to play around with at home.  Moreover, when
#####  a conflict occurs with stuff in the workspace, it might be
#####  tempting to remove that too, but I need to think a bit more
#####  about that before I dare do anything along the line of removing
#####  stuff without any further ado.
        ##---  KIT
        rm(script, valid_path)
    }
    ##---  KIT
    rm(script_files)
###-------------------------------------------------------------------
    ##  Ensure that we start with a blank slate, to avoid interference
    ##  with stuff captured at an earlier stage.
    uc_cleanup()
###-------------------------------------------------------------------
    ##  If '.uc_package_name' is given, extract and attach the
    ##  required environments (internal objects from the package and
    ##  functions imported from other packages).
    if (! is.null(.uc_package_name))
        uc_extractor(.package_name = .uc_package_name,
                     .attach  = TRUE)
###-------------------------------------------------------------------
    ##  Define the name to be used for the captured environment
    ##  originating from '.uc_fun'.
    captured_environment <- 
        paste(".captured_environment_of___",
              deparse(substitute(.uc_fun)),
              sep = "")
#####  TASK: I am not sure if the idea above will work in general, if
#####  for example the function is given as "list_of_fun$fun2"...
#####  Check that out later on...  I suppose I could use 'pryr::ast'
#####  here to create a condensed version only based on the names,
#####  that should not be to hard to deal with.
###-------------------------------------------------------------------    
#############---------------------------------------------------------
###  The body of '.uc_fun' must be checked for occurrences of
###  'capture_env()' (with no arguments specified) and then those
###  calls must be adjusted such that the 'global_name'-argument is
###  set to the above defined 'captured_environment'.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Capture the body of '.uc_fun'
    .uc_fun_body <- body(.uc_fun)
###-------------------------------------------------------------------
    ##  Create a help-function that iteratively identifies the parts
    ##  of '.uc_fun_body' that equals 'capture_env()'.
#####  TASK: I think it might be useful to have this as a separate
#####  function, since it then could be used as a tool for more
#####  advanced investigations too.
    call_inspect <- function(.call, .pattern){
        ##  Initiate list for result
        result <- as.list(.call)
        names(result) <- seq_along(.call)
        ##  Identify the parts that does not contain '.pattern'.
        not_detected <- ! str_detect(string = as.character(.call),
                                     pattern = .pattern)
        ##  Remove the parts that doesn't contain '.pattern'
        result[not_detected] <- NULL
        ##  Let the function call itself to analyse the remaining
        ##  sub_calls of length larger than one.
        for (i in names(result))
            if (length(result[[i]]) > 1)
                result[[i]] <-
                    call_inspect(
                        .call = result[[i]],
                        .pattern = .pattern)
        ##  Return the result
        result
    }
###-------------------------------------------------------------------
    ##  Use the help-function to create a nested list whose names
    ##  (numbers as characters) identifies the levels to update.
    capture_env_pos <- call_inspect(
        .call = .uc_fun_body,
        .pattern = "capture_env()")
    ##---  KIT
    rm(call_inspect)
###-------------------------------------------------------------------
    ##  Update 'capture_env_pos' to a list of character-vectors.
    capture_env_pos <- str_split(
        string = names(unlist(capture_env_pos)),
        pattern = "\\.")
###-------------------------------------------------------------------
    ##  Loop over 'captured_env_pos' to replace any 'capture_env()'
    ##  with versions using the value in 'captured_environment' as the
    ##  name for the stored environment.
    for (i in seq_along(capture_env_pos))
        .uc_fun_body[[as.integer(capture_env_pos[[i]])]] <- 
            bquote(capture_env(global_name = .(captured_environment)))
    ##---  KIT
    rm(i, capture_env_pos)
###-------------------------------------------------------------------    
    ##  Update the body of '.uc_fun' with the revised version, and clean
    ##  out intermediate objects.
    body(.uc_fun) <- .uc_fun_body
    ##---  KIT
    rm(.uc_fun_body)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  In order to identify the objects in the global environment that
###  we want to protect from annihilation when 'clean_workspace' is
###  'TRUE', the function 'pryr::ast' is used together with
###  'capture.output'.  The back-tick, i.e. "`" is used to indicate
###  that an object is named, and it's thus possible to use
###  'str_locate' and 'str_sub' (from the 'stringr'-package') to
###  extract the top-level name of composite objects.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Use the 'call'-part of 'spy_report' to figure out more about
    ##  the arguments that are functions, calls or other stuff that
    ##  might be defined in the global workspace.
    .interesting_args <-
        names(spy_report$call)[
                 vapply(X = names(spy_report$call),
                        FUN = function(x) {
                            .value <- spy_report$call[[x]]
                            any(c(is.call(.value),
                                  is.expression(.value),
                                  is.function(.value),
                                  is.name(.value)))
                        },
                        FUN.VALUE = logical(1))]
    arg_list <- structure(
        .Data = as.list(.interesting_args),
        .Names = .interesting_args)
    ##  KIT
    rm(.interesting_args)
    ##---
    for (arg in names(arg_list)) {
        ast_quote <- bquote(ast(.(spy_report$call[[arg]])))
        arg_list[[arg]] <- capture.output(eval(ast_quote))
    }
    ##---  KIT
    rm(arg, ast_quote)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Vectors in 'arg_list' of length one can be named objects.  Longer
###  vectors _will_ be named objects, whose main-name (found in the
###  workspace or in an environment) is stored in the third component.
###  The back-tick "`" is the key to identifying the named objects.
###  There is a blank space at the end of the names, and even though
###  the length of this is one for all the cases I have considered, I
###  am not sure if that is generally true.  Thus, 'stringr::str_trim'
###  has been used for the removal of these trailing blank spaces.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Restrict 'arg_list' to those arguments that are named objects.
    for (arg in names(arg_list)) {
        ##  Figure out where the name is stored.
        inspect <-
            ifelse(
                test = length(arg_list[[arg]]) == 1,
                yes = 1,
                no = 3)
        ##  Identify where the back-tick is located, if present.
        back_tick_pos <- str_locate(
            string = arg_list[[arg]][[inspect]],
            pattern = "`")[1]
        ##---
        if (is.na(back_tick_pos)) {
            ##  Get rid of those that aren't named objects.
            arg_list[[arg]] <- NULL
        } else {
            ##  Record the names of the named objects.
            arg_list[[arg]] <- str_trim(
                string = str_sub(
                    string = arg_list[[arg]][[inspect]],
                    start = back_tick_pos + 1),
                side = "right")
        }
    }
###-------------------------------------------------------------------
    ##  Convert to a vector, duplicates doesn't matter.
    arg_list <- unlist(arg_list)
    ##---  KIT
    rm(arg, inspect, back_tick_pos)
####  Reminder: These arguments will be protected against deletion.
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Note that some functions might contain code, e.g. sanity checks,
###  which will fail if the name of the calling function does not
###  belong to a specified list of recognised functions.  It is thus
###  necessary to recreate a local version of '.uc_fun' with the same
###  name as it had in the calling environment, and then to evaluate
###  this function in order to investigate the code of interest.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Use 'bquote' and '.()' to create a quote to create a local
    ##  function with the same name as the one stored in 'arg_list'.
    originally_named_.uc_fun_quote <- bquote(
        assign(x = .(arg_list[".uc_fun"]),
               value = .uc_fun))
###-------------------------------------------------------------------
    ##  Evaluate the quote to create a function with the desired name,
    ##  i.e. the name stored in 'arg_list[".uc_fun"]'
    eval(originally_named_.uc_fun_quote)
    ##---  KIT
    rm(originally_named_.uc_fun_quote)
#####  TASK: I think this might fail in general settings, e.g. when
#####  '.uc_fun' comes from a list of functions...
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Next up is a call, based on the "original" function (the name is
###  the same, but the body has been slightly modified) and the
###  arguments delivered by the dotsMethods, i.e. '...'.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Create a call for '.uc_fun' based on 'spy_report', take care
    ##  to not use any of the arguments from this function.
    .uc_fun_call <- create_call(
        .cc_fun = .uc_fun,
        spy_report$envir,        
        .cc_list = TRUE,
        .cc_ignore = names(formals(under_construction)))
###-------------------------------------------------------------------
    ##  Use 'bquote' and '.()' to create a quote that replace '.uc_fun'
    ##  with the modified local version version.
    update_.uc_fun_call <- bquote(
        .uc_fun_call[[1]] <- as.symbol(.(arg_list[".uc_fun"])))
###-------------------------------------------------------------------
    ##  Evaluate to get the desired call.
    eval(update_.uc_fun_call)
    ##---  KIT
    rm(update_.uc_fun_call)
###-------------------------------------------------------------------
    ##  Try the function, to see what it returns.
    .uc_fun_result <- try(eval(.uc_fun_call),
                       silent = TRUE)
    ##---  KIT
    rm(.uc_fun_call, spy_report)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  If no 'capture_env()' was present in the body of '.uc_fun', then we
###  now either have a proper result or an error due to problems in
###  the code.  Errors might of course also be present before an
###  occurrence of a 'capture_env()', in which case 'traceback' should
###  be called in order to figure out a more reasonable place to
###  insert the 'capture_env()'-command.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Cleanup and return the result if no error occurred.
    if (! "try-error" %in% class(.uc_fun_result)) {
        uc_cleanup()
        return(.uc_fun_result)
    }
###-------------------------------------------------------------------
    ##  Return the error if it wasn't by demand of 'capture_env'.
    .uc_fun_error <- gsub(pattern = "Error : ",
                       replacement = "",
                       x = .uc_fun_result[1])
    ##---
    if (is.na(str_locate(string = .uc_fun_error,
                         pattern = "'capture_env'")[1]))
        stop(.uc_fun_error,
             call. = FALSE)
    ##---  KIT
    rm(.uc_fun_result, .uc_fun_error)
###-------------------------------------------------------------------
    ##  Code still running?  Then 'capture_env' has captured the
    ##  environment as the name given in 'captured_environment'.  We
    ##  now need to make a list of the captured objects.
    captured_names <- ls(all.names = TRUE,
                         envir = get(captured_environment))
###-------------------------------------------------------------------
    ##  Depending on 'clean_workspace', clean out the workspace.
    if (clean_workspace) {
        global_workspace <- ls(all.names = TRUE,
                               envir = .GlobalEnv)
        ##  Figure out what we want to keep, i.e. the stuff stored in
        ##  'arg_list' and 'captured_environment'.
        keep_these <-
            global_workspace %in% c(arg_list, captured_environment)
        ##  Get rid of the rest.
        rm(list = global_workspace[! keep_these],
           envir = .GlobalEnv)
###-------------------------------------------------------------------
        ##  Inform the user:
        cat("\n",
            "The workspace has been cleaned, with the exception of:",
            "\n\t'",
            paste(ls(all.names = TRUE,
                     envir = .GlobalEnv),
                  collapse = "',\n\t'"),
            "'.\n",
            sep = "")
        ##---  KIT
        rm(global_workspace, keep_these)
    } else {
        ##  Notify the user that it's highly recommended that the
        ##  default for 'clean_workspace' is changed to 'TRUE'
        cat("\n",
            "IMPORTANT MESSAGE!  READ THIS!",
            "\n",
            "Note that it's RECOMMENDED to change the argument 'clean_workspace'",
            "\n",
            "from it's default value 'FALSE' to 'TRUE'.",
            "\n",
            "The effect of setting this to 'TRUE' will be that all nonessential",
            "\n",
            "objects will be removed from the global workspace, and that the",
            "\n",
            "risk of problematic masking will dwindle.",
            "\n",
            "Please read the documentation for further details.",
            "\n",
            sep = "")
    }
    ##---  KIT
    rm(clean_workspace)
###-------------------------------------------------------------------
    ##  Attach 'captured_environment' and update the options-storage.
    do.call(what = "attach",
            args = list(what = get(captured_environment),
                        pos = 2,
                        name = captured_environment,
                        warn.conflicts = FALSE))
    if (is.null(getOption(x = "uc_temp_attached_env"))) {
        options(uc_temp_attached_env = captured_environment)
    } else
        options(uc_temp_attached_env =
                    c(getOption("uc_temp_attached_env"),
                      captured_environment))
###-------------------------------------------------------------------
    ##  Inform the user:
    cat("\n",
        "An environment has been captured from '",
        arg_list[".uc_fun"],
        "',\n",
        "at the point of 'capture_env()', containing the following object",
        ifelse(test = length(captured_names) == 1,
               yes = ":\n",
               no = "s:\n"),
        sep = "")
###-------------------------------------------------------------------
    ##  Add details based on 'high_details_level'.
    if (high_details_level) {
        ##  All details.
        print(ls.str(all.names = TRUE, envir = get(captured_environment)))
    } else {
        ##  Only the names.
        cat("\t'",
            paste(captured_names,
                  collapse = "',\n\t'"),
            "'.\n",
            sep = "")
    }
    ##---  KIT
    rm(high_details_level)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  When an environment is attached, conflicts can occur if objects
###  in the attached environment has names that coincide with the
###  names of objects already occurring in other attached
###  environments, in packages or in the workspace (the global
###  environment.  Conflicts against stuff contained in other attached
###  environments or packages should be safe to ignore (since the
###  latest attached environment is searched before these), but
###  conflicts with stuff in the global environment - that's quite
###  another cup of tea.  An investigation of the severity of any such
###  conflicts should be reported.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Investigate any conflicts with the global environment.
    conflicts <- intersect(
        x = ls(all.names = TRUE, envir = .GlobalEnv),
        y = captured_names)
###-------------------------------------------------------------------
    ##  Based on 'conflicts' inform the user about the status.
    if (length(conflicts) == 0) {
        ##  No problem.
        info_text <-
            c("\n\n",
              "The ",
              ifelse(test = length(captured_names) == 1,
                     yes = "captured object has",
                     no = paste(
                         length(captured_names),
                         " captured objects have",
                         sep = "")),
              " been successfully attached.",
              "\n",
              "You can now start working from the 'capture_env()'-spot inside of",
              "\n",
              "the body of the function '",
              arg_list[".uc_fun"],
              "'.\n\n")
    } else {
        ##  If conflicts occur, decide how severe they are.
        conflicts_index <- vector(mode = "logical",
                                  length = length(conflicts))
        names(conflicts_index) <- conflicts
        ##---
        for (conflict in conflicts)
            conflicts_index[conflict] <-
                ! identical(x = .GlobalEnv[[conflict]],
                            y = get(captured_environment)[[conflict]])
        ##---  KIT
        rm(conflict)
        ##  The set of conflicts that are critical.
        critical_conflicts <-
            names(conflicts_index[conflicts_index])
###-------------------------------------------------------------------
        ##  Create a text describing the severity of the conflicts.
        info_text <- 
            c("\n",
              "ALARM!",
              "\t",
              "The object",
              ifelse(test = length(conflicts) == 1,
                     " '",
                     "s '"),
              paste(conflicts,
                    collapse = "', '"),
              "',\n\t",
              "in the environment of the function that we want",
              "\n\t",
              "to investigate, '",
              arg_list[".uc_fun"],
              "',\n\t",
              ifelse(test = length(conflicts) == 1,
                     "has ",
                     "have "),
              "been masked by the global environment. \n",
              ##  Inform about the severity of the conflicts.
              "SEVERITY LEVEL:",
              "\t",
              if (length(critical_conflicts) != 0) {
                  c("HIGH",
                    "\n\t",
                    "The object",
                    ifelse(test = length(critical_conflicts) == 1,
                           " '",
                           "s '"),
                    paste(critical_conflicts,
                          collapse = "', '"),
                    "',\n\t",
                    ifelse(test = length(critical_conflicts) == 1,
                           "has a ",
                           "have "),
                    "different value",
                    ifelse(test = length(critical_conflicts) == 1,
                           " ",
                           "s "),
                    "in the global environment!",
                    "\n\t",
                    "Don't be a nitwit: Fix this before proceeding!")
              } else {
                  ##  No serious problem, but warn that global objects
                  ##  might be changed...
                  c("MEDIUM",
                    "\n\t",
                    "The value",
                    ifelse(test = length(conflicts) == 1,
                           " ",
                           "s "),
                    "of the masked object",
                    ifelse(test = length(conflicts) == 1,
                           " ",
                           "s "),
                    "coincide with ",
                    ifelse(test = length(conflicts) == 1,
                           "that ",
                           "those "),
                    "in the global environment.",
                    "\n\t",
                    "This will not matter for the further investigation of the function,",
                    "\n\t",
                    "but there is a chance that the global object",
                    ifelse(test = length(conflicts) == 1,
                           " ",
                           "s "),
                    "might be changed in the process."
                    )
              },
              "\n\n")
        ##---  KIT
        rm(conflicts_index, critical_conflicts)
    }
    cat(info_text, sep = "")
    ##---  KIT
    rm(arg_list, captured_names, conflicts, info_text)
###-------------------------------------------------------------------
    ##  Nothing more to do (in the present incarnation of the code).
    return(invisible(NULL))
}

#####  TASK: For function that does need to be run from the global
#####  environment (in order for e.g. sanity-check to be turned on),
#####  the present approach doesn't quite make the cut.  I toy with
#####  the idea of making a modified version that creates a
#####  (temporary) script that can be used for that kind of
#####  inspection.


###########################################################################
#####  2014-11-12
##  Note that we can't use the function 'under_construction' directly
##  on itself or on 'spy' that it depends upon.  For those cases a
##  reminder as given below must be used instead, i.e. we need to
##  include a setting where we in the global workspace take care of
##  what this functions does.  (Or we might perhaps work upon a copy
##  instead, I suppose that should work just as well...)




## rm(list=ls(all.names=TRUE))

## test_fun <- function(a = 3){
##     res <- 222 * a
##     res
##     a <- 10
##     capture_env()
## }


## a <- 10

## test_list <- list(a = 3, b= 2)


## try(detach(captured_result), silent = TRUE)

## tmp <- under_construction(.fun = test_fun, a = 5, d = 3, f = test_list$a,
##                           ## script_files = "runMe.R",
##                           clean_workspace = FALSE)

## ##
## if (exists("captured_result")) {
##     attach(captured_result)
##     ls(captured_result, all.names = TRUE) }


################################################################################
#####  2014-11-13

##  In case I need to include an if-statement before the activation
##  of 'capture_env()', e.g. if the code turns out to break down when
##  encountering the 15th element in a loop, then a more advanced
##  updating procedure for the body of the targeted function is
##  required.  We will need to investigate whether or not 'capture_env'
##  is contained in an if-statement, and then update that part.  It's
##  not a hard task to do, but I can't waste time on it now.  Below is
##  a little toy-function that indicates the structures that must be
##  investigated.


## test_fun324 <- function(a = 2131, b = "sagfav") {
##     if (a == 2144)
##         capture_env()
##     print("No problem")
## }

## inspects_if_capture <- body(test_fun324)

## length(inspects_if_capture)
## inspects_if_capture[[1]]
## inspects_if_capture[[2]]
## inspects_if_capture[[3]]

## if_detail <- inspects_if_capture[[2]]

## length(if_detail)

## if_detail[[1]]
## if_detail[[2]]
## if_detail[[3]]
