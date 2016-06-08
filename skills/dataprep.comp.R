#
# load required package
#
library(sqldf)
#
# use sql command to generate counts per level.
#
sqlcmd = "select Technology, Product, Activity, Level, count(Level) as Cnt from comp group by Technology, Product, Activity, Level order by Technology, Product, Activity, Level"
# 
comp.cnts = sqldf(sqlcmd)
head(comp.cnts)
#
# remove NA values
#
comp.cnts.no.na = comp.cnts[ ! is.na(comp.cnts$Level),]
head(comp.cnts.no.na)
#
# group by technology, product, activity
#
grouped.tech.prod.act.comp.cnts.no.na <- 
    split(comp.cnts.no.na, 
          list(comp.cnts.no.na$Technology,
               comp.cnts.no.na$Product,
               comp.cnts.no.na$Activity))
#
# short cuts and remove any entries with no records.
#
g.tpa.cc.no.na <- 
    grouped.tech.prod.act.comp.cnts.no.na
g.tpa.cc.no.na.cnt <- 
    lapply(g.tpa.cc.no.na, nrow)
g.tpa.cc.no.na.nz <- 
    g.tpa.cc.no.na[names(g.tpa.cc.no.na.cnt[g.tpa.cc.no.na.cnt!=0])]
#
# X11 plot to see what it looks like for now.
# 
maxrow <- 3
maxcol <- 2
maxwidth <- 11.0
maxheight <- 9.0
pointsize <- 14
colors <- rainbow(5)
#
pcnt <- 0
for (tech in sort(names(g.tpa.cc.no.na.nz)))
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
        # par(mfrow=c(maxrow,maxcol))
        par(mfcol=c(maxrow,maxcol))
    }
    #
    # generate labels
    #
    mainlabel <- gsub(".", " ", as.character(tech), fixed=TRUE)
    mainlabel <- gsub("5G 5G", "5G", mainlabel, fixed=TRUE)
    totalcnts <- sum(g.tpa.cc.no.na.nz[[tech]]$Cnt)
    mainlabel <- paste(mainlabel, "(", totalcnts, ")")
    #
    # get data for pie charts
    #
    levels <- g.tpa.cc.no.na.nz[[tech]]$Level
    nlevels <- length(g.tpa.cc.no.na.nz[[tech]]$Level)
    cnts <- g.tpa.cc.no.na.nz[[tech]]$Cnt
    percentages <- round((cnts/sum(cnts))*100)
    labels <- paste("lvl=",levels,", cnt=", cnts,", ", percentages, "%", sep="")
    #
    slices <- g.tpa.cc.no.na.nz[[tech]]$Cnt
    #
    # draw pie chart
    #
    pie(slices, 
        labels=labels, 
        main=mainlabel,
        col=colors[levels],
        font=4)
}
#
# reset params for png file as output
#
maxrow <- 2
maxcol <- 3
maxwidth <- 1400
maxheight <- 800
pointsize <- 20
#
# start drawing graphs
#
fcnt <- 0
pcnt <- 0
for (tech in sort(names(g.tpa.cc.no.na.nz)))
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
            file=paste("skills-competency-",fcnt,".png",sep=""))
        # par(mfrow=c(maxrow,maxcol))
        par(mfcol=c(maxrow,maxcol))
    }
    #
    # labels for charts
    #
    mainlabel <- gsub(".", " ", as.character(tech), fixed=TRUE)
    mainlabel <- gsub("5G 5G", "5G", mainlabel, fixed=TRUE)
    totalcnts <- sum(g.tpa.cc.no.na.nz[[tech]]$Cnt)
    mainlabel <- paste(mainlabel, "(", totalcnts, ")")
    #
    # data for charts
    #
    levels <- g.tpa.cc.no.na.nz[[tech]]$Level
    nlevels <- length(g.tpa.cc.no.na.nz[[tech]]$Level)
    cnts <- g.tpa.cc.no.na.nz[[tech]]$Cnt
    percentages <- round((cnts/sum(cnts))*100)
    labels <- paste("lvl=",levels,", cnt=", cnts,", ", percentages, "%", sep="")
    #
    slices <- g.tpa.cc.no.na.nz[[tech]]$Cnt
    #
    # draw chart
    #
    pie(slices, 
        labels=labels, 
        main=mainlabel,
        col=colors[levels],
        font=4)
}

closealldevs()

#
# g.tpa.cc <- grouped.tech.prod.act.comp.cnts
# g.tpa.cc.cnt <- lapply(g.tpa.cc, nrow)
# g.tpa.cc.nz <- g.tpa.cc[names(g.tpa.cc.cnt[g.tpa.cc.cnt!=0])]


# 
# comp.cnts = comp.cnts[order(comp.cnts$Technology,
#                             comp.cnts$Product,
#                             comp.cnts$Activity,
#                             comp.cnts$Level),]
# head(comp.cnts)
# 
# grouped.tech.prod.comp.cnts <- 
#     split(comp.cnts, 
#           list(comp.cnts$Technology,
#                comp.cnts$Product))
# 
# g.tp.cc <- grouped.tech.prod.comp.cnts 
# g.tp.cc.cnt <- lapply(g.tp.cc, nrow)
# g.tp.cc.nz <- g.tp.cc[names(g.tp.cc.cnt[g.tp.cc.cnt!=0])]
# 
# # g.tp.cc.nz 
# 
# # for (tech in names(g.tp.cc.nz))
# # {
#     # print(paste("tech ...", tech))
#     # print(g.tp.cc.nz[[tech]])
# # }
# 
# grouped.tech.prod.act.comp.cnts <- 
#     split(comp.cnts, 
#           list(comp.cnts$Technology,
#                comp.cnts$Product,
#                comp.cnts$Activity))
# closealldevs()
# 
# g.tpa.cc <- grouped.tech.prod.act.comp.cnts
# g.tpa.cc.cnt <- lapply(g.tpa.cc, nrow)
# g.tpa.cc.nz <- g.tpa.cc[names(g.tpa.cc.cnt[g.tpa.cc.cnt!=0])]
# 
# # g.tpa.cc.nz 
# 
# pcnt <- 0
# for (tech in names(g.tpa.cc.nz))
# {
#     pcnt <- pcnt + 1
#     if ((pcnt%%6) == 1)
#     {
#         x11()
#         par(mfrow=c(3,2))
#     }
#     # print(paste("tech ...", tech))
#     # print(g.tpa.cc.nz[[tech]])
#     #
#     label <- tech
#     #
#     levels <- g.tpa.cc.nz[[tech]]$Level
#     nlevels <- length(g.tpa.cc.nz[[tech]]$Level)
#     cnts <- g.tpa.cc.nz[[tech]]$Cnt
#     percentages <- round((cnts/sum(cnts))*100)
#     labels <- paste("lvl=",levels,",cnt=", cnts,", ", percentages, "%", sep="")
#     #
#     slices <- g.tpa.cc.nz[[tech]]$Cnt
#     #
#     pie(slices, 
#         labels=labels, 
#         main=label,
#         col=rainbow(nlevels))
# }
# 
# closealldevs()
# 
# for (fcnt in 1:20)
# {
#     fname <- paste("skills-",fcnt,".png",sep="")
#     if (file.exists(fname))
#     {
#         print(paste("deleting file ...", fname))
#         file.remove(fname)
#     }
# }
# 
# closealldevs()
# 
# # mar <- par("mar")
# # closealldevs()
# 
# maxrow <- 2
# maxcol <- 2
# maxwidth <- 1000
# maxheight <- 700
# pointsize <- 16
# 
# fcnt <- fcnt + 1;
# png(width=maxwidth,height=maxheight,pointsize=pointsize,
#     file=paste("skills-",fcnt,".png",sep=""))
# par(mfrow=c(maxrow,maxcol))
# 
# fcnt <- 0
# pcnt <- 0
# 
# for (tech in names(g.tpa.cc.nz))
# {
#     pcnt <- pcnt + 1
#     #
#     # print(paste("tech ...", tech))
#     # print(g.tpa.cc.nz[[tech]])
#     #
#     label <- tech
#     #
#     levels <- g.tpa.cc.nz[[tech]]$Level
#     nlevels <- length(g.tpa.cc.nz[[tech]]$Level)
#     cnts <- g.tpa.cc.nz[[tech]]$Cnt
#     percentages <- round((cnts/sum(cnts))*100)
#     labels <- paste("lvl=",levels,",cnt=", cnts,", ", percentages, "%", sep="")
#     #
#     slices <- g.tpa.cc.nz[[tech]]$Cnt
#     #
#     pie(slices, 
#         labels=labels, 
#         main=label,
#         col=rainbow(nlevels))
#     #
#     if ((pcnt%%(maxrow*maxcol-1)) == 1)
#     {
#         #par(mar=c(0,0,0,0))
#         plot(c(0, 1), c(0, 1), 
#              ann = F, 
#              bty = 'n', 
#              type = 'n', 
#              xaxt = 'n', 
#              yaxt = 'n')
#         text(x=0.5,y=0.5,
#             paste("Competence Level:\n",
#                   "Level 5 - World Class\n",
#                   "Level 4 - Advanced\n",
#                   "Level 3 - Intermediate\n",
#                   "Level 2 - Basic\n",
#                   "Level 1 - Initial\n", sep=""), 
#                   col="blue", cex=1.2)
#         closealldevs()
#         fcnt <- fcnt + 1;
#         png(width=maxwidth,height=maxheight,pointsize=pointsize,
#             file=paste("skills-",fcnt,".png",sep=""))
#         par(mfrow=c(maxrow,maxcol))
#     }
# }
# 
#  if ((pcnt%%(maxrow*maxcol-1)) != 1)
#  {
#      #par(mar=c(0,0,0,0))
#      plot(c(0, 1), c(0, 1), 
#           ann = F, 
#           bty = 'n', 
#           type = 'n', 
#           xaxt = 'n', 
#           yaxt = 'n')
#      text(x=0.5,y=0.5,
#           paste("Competence Level:\n",
#                 "Level 5 - World Class\n",
#                 "Level 4 - Advanced\n",
#                 "Level 3 - Intermediate\n",
#                 "Level 2 - Basic\n",
#                 "Level 1 - Initial\n", sep=""), 
#                 col="blue", cex=1.2)
# }
# 
# closealldevs()
