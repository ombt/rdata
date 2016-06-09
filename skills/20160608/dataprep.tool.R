#
# load required package
#
library(sqldf)
#
# use sql command to generate counts per level.
#
sqlcmd = "select ToolName, Knowlegeable as Knowledgeable, count(Knowlegeable) as Cnt from tool group by ToolName, Knowlegeable order by ToolName, Knowlegeable"
# 
tool.cnts = sqldf(sqlcmd)
head(tool.cnts)
#
# remove NA values
#
tool.cnts.no.na = tool.cnts[ ! is.na(tool.cnts$Knowledgeable),]
head(tool.cnts.no.na)
#
# group by Tool.Name 
#
grouped.toolname.tool.cnts.no.na <- 
    split(tool.cnts.no.na, 
          list(tool.cnts.no.na$ToolName))
#
# short cuts and remove any entries with no records.
#
g.t.tc.no.na <- 
    grouped.toolname.tool.cnts.no.na
g.t.tc.no.na.cnt <- 
    lapply(g.t.tc.no.na, nrow)
g.t.tc.no.na.nz <- 
    g.t.tc.no.na[names(g.t.tc.no.na.cnt[g.t.tc.no.na.cnt!=0])]
#
# X11 plot to see what it looks like for now.
# 
maxrow <- 2
maxcol <- 2
maxwidth <- 11.0
maxheight <- 9.0
pointsize <- 14
colors <- rainbow(3)
yesno <- list(Yes=1,No=2)
#
pcnt <- 0
for (tech in sort(names(g.t.tc.no.na.nz)))
{
    #
    # start up a new X11 display
    #
    pcnt <- pcnt + 1
    if ((pcnt%%(maxrow*maxcol)) == 1)
    {
        x11(width=maxwidth,
            height=maxheight,
            pointsize=pointsize)
        par(mfrow=c(maxrow,maxcol))
    }
    #
    # generate labels
    #
    mainlabel <- gsub(".", " ", as.character(tech), fixed=TRUE)
    totalcnts <- sum(g.t.tc.no.na.nz[[tech]]$Cnt)
    mainlabel <- paste(mainlabel, "(", totalcnts, ")")
    #
    # get data for pie charts
    #
    levels <- g.t.tc.no.na.nz[[tech]]$Knowledgeable
    nlevels <- length(g.t.tc.no.na.nz[[tech]]$Knowledgeable)
    cnts <- g.t.tc.no.na.nz[[tech]]$Cnt
    percentages <- round((cnts/sum(cnts))*100)
    labels <- paste("lvl=",levels,", cnt=", cnts,", ", percentages, "%", sep="")
    #
    slices <- g.t.tc.no.na.nz[[tech]]$Cnt
    #
    # draw pie chart
    #
    pie(slices, 
        labels=labels, 
        main=mainlabel,
        col=colors[as.integer(yesno[levels])+1],
        font=4)
}
#
# reset params for png file as output
#
maxrow <- 2
maxcol <- 2
maxwidth <- 1400
maxheight <- 800
pointsize <- 20
#
# start drawing graphs
#
fcnt <- 0
pcnt <- 0
for (tech in sort(names(g.t.tc.no.na.nz)))
{
    #
    # create output png file for pie charts
    #
    pcnt <- pcnt + 1
    if ((pcnt%%(maxrow*maxcol)) == 1)
    {
        closealldevs()
        fcnt <- fcnt + 1;
        png(width=maxwidth,
            height=maxheight,
            pointsize=pointsize,
            file=paste("skills-newtools-",fcnt,".png",sep=""))
        par(mfrow=c(maxrow,maxcol))
    }
    #
    # labels for charts
    #
    mainlabel <- gsub(".", " ", as.character(tech), fixed=TRUE)
    totalcnts <- sum(g.t.tc.no.na.nz[[tech]]$Cnt)
    mainlabel <- paste(mainlabel, "(", totalcnts, ")")
    #
    # data for charts
    #
    levels <- g.t.tc.no.na.nz[[tech]]$Knowledgeable
    nlevels <- length(g.t.tc.no.na.nz[[tech]]$Knowledgeable)
    cnts <- g.t.tc.no.na.nz[[tech]]$Cnt
    percentages <- round((cnts/sum(cnts))*100)
    labels <- paste("lvl=",levels,", cnt=", cnts,", ", percentages, "%", sep="")
    #
    slices <- g.t.tc.no.na.nz[[tech]]$Cnt
    #
    # draw chart
    #
    pie(slices, 
        labels=labels, 
        main=mainlabel,
        col=colors[as.integer(yesno[levels])+1],
        font=4)
}
# 
closealldevs()
# 
