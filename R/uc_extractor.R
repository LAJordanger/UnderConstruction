################################################################################

#' Extract information from a package.
#'
#' In order to simplify the investigation by
#' \code{under_construction}, it might in many cases be preferable to
#' extract from a package the non-exported objects, and in addition
#' get our hands the functions that has been imported into the package.
#'
#' @param .package_name The name of the package to investigate.
#'
#' @param .attach Logical value, default \code{FALSE}, that decides if
#'     the extracted values should be added to the search-path.
#'
#' @return Depending on \code{.attach}.  When \code{FALSE}, a list
#'     with two environments, one containing any non-exported objects
#'     from the package and one containing copies of stuff that was
#'     imported into the package (stuff already available in the
#'     search-path will be ignored).  When \code{TRUE} these
#'     environments are added to the end of the search-path., and
#'     nothing is returned to the work-flow.  Note that
#'     \code{uc_cleanup} can be used to get rid of stuff again.
#'
#' @export


uc_extractor <- function(.package_name, .attach = FALSE) {
    ##  Perform a sanity-check before proceeding, i.e. check that the
    ##  package is available in the search-path.
    if (! paste("package:", .package_name, sep = "") %in% search())
        error(.argument = ".package_name",
              "Package not detected  on the search-path.")
###-------------------------------------------------------------------
    ##  Identify the non-exported components from the package.
    .package_env <- getNamespace(.package_name)
    .not_exported <- setdiff(
        x = ls(.package_env, all.names = TRUE),
        y = getNamespaceExports(.package_name))
    ##  Ignore stuff starting with ".__"
    .not_exported <-
        .not_exported[! stringr::str_detect(
                            string = .not_exported,
                            pattern = "^.__")]
    ##  Create a new environment and populate it.
    .env_not_exported <- new.env()
    for (.object in .not_exported) 
        .env_not_exported[[.object]] <- .package_env[[.object]]
    kill(.object, .not_exported, .package_env)
###-------------------------------------------------------------------
    ##  Identify the imported objects in the package.
    .getNamespaceImports <- getNamespaceImports(.package_name)
    ##  This is a rather messy list, containing stuff in two different
    ##  formats.  Use the redundancy to reduce to a simpler structure.
    .getNamespaceImports <-
        .getNamespaceImports[names(.getNamespaceImports) != ""]
    ##  Reminder: If everything from a package has been imported, then
    ##  the node of the list will be a vector with all the exported
    ##  entities from that package.  Otherwise only one value will be
    ##  present at each node, but the name of the package might be
    ##  present several times.  Ignore stuff that can be found from
    ##  the search-path.
    .keep_boolean <- ! paste("package:",
                             names(.getNamespaceImports),
                             sep = "") %in% search()
    .getNamespaceImports <- .getNamespaceImports[.keep_boolean]
    kill(.keep_boolean)
    ##  Create a new environment and populate it.
    .env_imported <- new.env()
    for (i in seq_along(.getNamespaceImports))
        for (.object in (.getNamespaceImports[[i]]))
            .env_imported[[.object]] <-     
                getExportedValue(
                    ns = names(.getNamespaceImports)[i],
                    name = .object)
    kill(i, .object, .getNamespaceImports)
    ##  Create list with informative names.
    .result <- structure(
        .Data = list(.env_not_exported, .env_imported),
        .Names = paste("uc",
                       .package_name,
                       c("not_exported",
                         "imported"),
                       sep = "_"))
    kill(.env_not_exported, .env_imported, .package_name)
###-------------------------------------------------------------------
    ##  Depending on '.attach' either attach the environments or
    ##  return '.result' to the workflow.
    if (.attach) {
        for (.env in names(.result))
            options_attach(
                what = .result[[.env]],
                pos = length(search()) + 1,
                name = .env,
                warn.conflicts = FALSE,
                options_name = UC_defaults$options$package,
                options_front = FALSE)
    } else
        .result
}
