################################################################################

#' Clean up workspace and the search-path after using \code{under_construction}
#'
#' The function \code{under_construction} adds stuff to the
#' search-path, and it's also required with a cleanup afterwards.  This
#' function checks if it has been created a global option with the
#' name \code{uc_temp_attached_env}.  If that's the case, then it will
#' proceed to detach the environments mentioned there, and moreover
#' also remove the corresponding objects from the workspace if they
#' are still present there.
#'
#' @export

#####  TASK: This function needs to be rewritten relative the new
#####  regime in which the functions `options_attach` and
#####  `options_detach` plays the main role.

uc_cleanup <- function() {
    .uc_temp_attached_env <- getOption(x = "uc_temp_attached_env")
    if (is.null(.uc_temp_attached_env)) {
        return(invisible(NULL))
    } else {
        for (.env in .uc_temp_attached_env) {
            ##  Use 'try' to avoid nagging if the environment already
            ##  have been detached manually by the user.
            try(expr = do.call(what = "detach",
                               args = list(name = .env)),
                silent = TRUE)
            ##  Use 'suppressWarnings' to avoid nagging if the user
            ##  already has removed stuff from the workspace.
            suppressWarnings(expr = do.call(what = "rm",
                               args = list(.env)))
        }
        options(uc_temp_attached_env = NULL)
    }
    invisible(NULL)
}

