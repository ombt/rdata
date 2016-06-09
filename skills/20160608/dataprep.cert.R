#
# load required package
#
library(sqldf)
#
# use sql command to generate counts per level.
#
sqlcmd = "select Grp, Certification, Achieved, count(Achieved) as Cnt from cert group by Grp, Certification, Achieved order by Grp, Certification, Achieved"
# 
cert.cnts = sqldf(sqlcmd)
head(cert.cnts)
#
# remove NA values
#
cert.cnts.no.na = cert.cnts[ ! is.na(cert.cnts$Achieved),]
head(cert.cnts.no.na)
#
# group by group and certification
#
grouped.group.cert.cert.cnts.no.na <- 
    split(cert.cnts.no.na, 
          list(cert.cnts.no.na$Grp,
               cert.cnts.no.na$Certification))
#
# short cuts and remove any entries with no records.
#
g.gc.cc.no.na <- 
    grouped.group.cert.cert.cnts.no.na
g.gc.cc.no.na.cnt <- 
    lapply(g.gc.cc.no.na, nrow)
g.gc.cc.no.na.nz <- 
    g.gc.cc.no.na[names(g.gc.cc.no.na.cnt[g.gc.cc.no.na.cnt!=0])]
#
# X11 plot to see what it looks like for now.
# 
maxrow <- 2
maxcol <- 2
maxwidth <- 11.0
maxheight <- 9.0
pointsize <- 14
colors <- rainbow(2)
yesno <- list(Yes=1,No=2)
#
pcnt <- 0
for (tech in sort(names(g.gc.cc.no.na.nz)))
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
    totalcnts <- sum(g.gc.cc.no.na.nz[[tech]]$Cnt)
    mainlabel <- paste(mainlabel, "(", totalcnts, ")")
    #
    # get data for pie charts
    #
    levels <- g.gc.cc.no.na.nz[[tech]]$Achieved
    nlevels <- length(g.gc.cc.no.na.nz[[tech]]$Achieved)
    cnts <- g.gc.cc.no.na.nz[[tech]]$Cnt
    percentages <- round((cnts/sum(cnts))*100)
    labels <- paste("lvl=",levels,", cnt=", cnts,", ", percentages, "%", sep="")
    #
    slices <- g.gc.cc.no.na.nz[[tech]]$Cnt
    #
    # draw pie chart
    #
    pie(slices, 
        labels=labels, 
        main=mainlabel,
        col=colors[as.integer(yesno[levels])],
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
for (tech in sort(names(g.gc.cc.no.na.nz)))
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
            file=paste("skills-cert-",fcnt,".png",sep=""))
        par(mfrow=c(maxrow,maxcol))
    }
    #
    # labels for charts
    #
    mainlabel <- gsub(".", " ", as.character(tech), fixed=TRUE)
    totalcnts <- sum(g.gc.cc.no.na.nz[[tech]]$Cnt)
    mainlabel <- paste(mainlabel, "(", totalcnts, ")")
    #
    # data for charts
    #
    levels <- g.gc.cc.no.na.nz[[tech]]$Achieved
    nlevels <- length(g.gc.cc.no.na.nz[[tech]]$Achieved)
    cnts <- g.gc.cc.no.na.nz[[tech]]$Cnt
    percentages <- round((cnts/sum(cnts))*100)
    labels <- paste("lvl=",levels,", cnt=", cnts,", ", percentages, "%", sep="")
    #
    slices <- g.gc.cc.no.na.nz[[tech]]$Cnt
    #
    # draw chart
    #
    pie(slices, 
        labels=labels, 
        main=mainlabel,
        col=colors[as.integer(yesno[levels])],
        font=4)
}
# 
closealldevs()
# 
