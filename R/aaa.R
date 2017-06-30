###############
## Import from other pacakages collected here.
#' @importFrom leanRcoding kill error equal_environments fun_info spy update_formals create_call
#' @importFrom pryr ast dots where
#' @importFrom stringr str_detect str_locate str_split str_sub str_trim str_wrap
#' @importFrom utils capture.output head ls.str tail
#' @export

NULL

###############

###  Add stuff that might simplify some of the code later on.  The
###  idea is that some options should be used to store some
###  information when 'under_construction' is used, and that it should
###  be fairly simple to clean up when the work has been finished.

##  Reminder: The `avoid`-component is a feeble attempt at avoiding
##  messing up the options-structure of existing options.  The idea
##  being that the functions that adds options should not be allowed
##  to use an existing options name.  This does however only protect
##  those options already in use when the package was loaded.

UC_defaults <- list(
    options = list(onload = list(
                       UnderConstruction = list(
                           attach_detach = character(0))),
                   environment = "uc_temp_attached_env",
                   package =  "uc_temp_attached_fun"))

