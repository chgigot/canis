#------------------------------------------------------------------------------#
# Startup message
#------------------------------------------------------------------------------#
.onAttach <- function(libname, pkgname) {
    startupMsg <- cat(
        "Attaching package 'canis' (version: ",
        as.character(utils::packageVersion("canis")), ").\n",
        "To get started, type: package?canis\n",
        "To get more information, type: help(package = \"canis\")", sep = "")
    packageStartupMessage(startupMsg)
}
