#
# default options
#
# do not convert strings to factors by default
#
options(stringsAsFactors=FALSE)
#
# maximum number of lines to print
#
options(max.print=100)
#
# start of session
#
.First <- function()
{
    library(RSQLite)
    library(sqldf)
    #
    if (file.exists("source_all.R"))
    {
        #
        # local version
        #
        source("source_all.R")
    }
    else if (Sys.getenv("OMBT_ANALYTICS_BASE") != "")
    {
        #
        # official one
        #
        source(paste(Sys.getenv("OMBT_ANALYTICS_BASE"),
                     "rsrc",
                     "source_all.R",
                     sep="/"))
    }
}
#
# end of session
#
# .Last <- function()
# {
# }
