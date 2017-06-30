################################################################################

##  Stuff to do when loading: Add new `options` to be used by other
##  functions when loading the package. 
## .onLoad <- function(libname = find.package("UnderConstruction"),
##                     pkgname = "UnderConstruction") {

.onLoad <- function(libname, pkgname) {
    ##  Add the option to be used by `options_attach` and
    ##  `options_detach`.
    options(UC_defaults$options$onload)
#####  TASK: Add sanity-test that prevents the package from being
#####  loaded if there's already an option available here.
    invisible(NULL)
}

##  I'm not sure how stuff should be taken here...

.onUnload <- function(libpath) { # = find.package("UnderConstruction")) {
    .main_option <- names(UC_defaults$options$onload)
    ##  `detach` everything stored in the component `attach_detach`.
    UnderConstruction::options_detach(cleanup = TRUE)
###-------------------------------------------------------------------
    ##  Remove the option that was added when the package was loded.
    .cleanup_call <- quote(options(a = NULL))
    names(.cleanup_call)[2] <- .main_option
    ##  Evaluate the call.
    eval(.cleanup_call)
    invisible(NULL)
}


#####  TASK: There's some details here that I don't understand.  I
#####  would like to get stuff out of the way when detaching too, but
#####  I'm not certain about the way that's supposed to be done.

## ##   This might be frowned upon, but does it do the trick?
## .onDetach <- UnderConstruction:::.onUnload

##  It seems to be the case that the attempt above trigger an error
##  for `.onUnload`, at least when dealing with `devtools::install`.
