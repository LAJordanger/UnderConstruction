################################################################################
#####  2016-02-05

#' Capture the environment of a function.
#'
#' @details This function can, when inserted into the body of another
#'     function, capture the environment at the inserted place.  The
#'     rationale for this is to make it easier to hunt for errors or
#'     develop new code.  \code{attach} and \code{detach} can be used
#'     on the captured environment in order to get hold of (or get rid
#'     of) the internal objects of the targeted function.  This
#'     function can be used directly, but it might be more efficient
#'     to use the function \code{under_construction} (with the helpers
#'     \code{uc_help} and \code{uc_help_call}) instead, those
#'     functions will take care of a lot of the pesky details required
#'     to attaching and detaching the captured environments.
#'
#' @param local A logical value, default \code{FALSE}, that decides if
#'     a locally captured environment should be assigned to some
#'     object in the local workflow, or if the function should be
#'     terminated and the result returned to the global workspace.
#'
#' @param global_name The name to be used when the function is
#'     terminated and the environment should be investigated in the
#'     global workspace.
#'
#' @param save_env The environment where the result should be
#'     assigned.  Default value \code{.GlobalEnv}.
#'
#' @return This function clones the environment of the targeted
#'     function at the place of insertion.  If \code{local=TRUE}, then
#'     the cloned environment will be returned to the internal
#'     work-flow of the function, the default is to create an object
#'     in the global workspace, named by \code{global_name}.  Warning,
#'     be aware of the potential memory-problems that might occur if
#'     you attempt to store the temporary environment from several
#'     different steps in the function. 
#' 
#' @export


capture_env <- function(
    local = FALSE,
    global_name = "captured_result",
    save_env = .GlobalEnv) {
    ##  Capture the environment of the target (one level up).
    target_env <- sys.frame(which = -1)
    ##  Clone the environment.
    cloned_env <- as.environment(as.list(target_env,
                                         all.names = TRUE))
    ## ## ##  An occurrence of '...' in the environment makes it impossible
    ## ## ##  to use 'ls.str' with 'all.names=TRUE'. Remove it if present.
    ## ## if (any(ls(cloned_env, all.names=TRUE) == "..."))
    ## ##     rm("...", envir = cloned_env)
    ##  Return the result
    if (local) {
        ##  Return the cloned environment to the work-flow
        return(cloned_env)
    } else {
        ##  Create an object in the work-space.
        assign(x = global_name,
               value = cloned_env,
               envir = save_env)
##               envir = .GlobalEnv)
        ##  Terminate program.
        stop("\t",
             "Program stopped by demand of 'capture_env'.",
             "\n\t",
             "See '",
             global_name,
             "' for the captured environment.",
             call. = FALSE)
    }
}
