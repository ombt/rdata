library(sqldf)

sqlcmd = "select Technology, Product, Activity, Level, count(Level) as Cnt from comp group by Technology, Product, Activity, Level "

comp.cnts = sqldf(sqlcmd)
head(comp.cnts)

comp.cnts = comp.cnts[order(comp.cnts$Technology,
                            comp.cnts$Product,
                            comp.cnts$Activity,
                            comp.cnts$Level),]
head(comp.cnts)

grouped.tech.prod.comp.cnts <- 
    split(comp.cnts, 
          list(comp.cnts$Technology,
               comp.cnts$Product))

g.tp.cc <- grouped.tech.prod.comp.cnts 
g.tp.cc.cnt <- lapply(g.tp.cc, nrow)
g.tp.cc.nz <- g.tp.cc[names(g.tp.cc.cnt[g.tp.cc.cnt!=0])]

# g.tp.cc.nz 

# for (tech in names(g.tp.cc.nz))
# {
    # print(paste("tech ...", tech))
    # print(g.tp.cc.nz[[tech]])
# }

grouped.tech.prod.act.comp.cnts <- 
    split(comp.cnts, 
          list(comp.cnts$Technology,
               comp.cnts$Product,
               comp.cnts$Activity))

g.tpa.cc <- grouped.tech.prod.act.comp.cnts
g.tpa.cc.cnt <- lapply(g.tpa.cc, nrow)
g.tpa.cc.nz <- g.tpa.cc[names(g.tpa.cc.cnt[g.tpa.cc.cnt!=0])]

# g.tpa.cc.nz 

pcnt <- 0
for (tech in names(g.tpa.cc.nz))
{
    pcnt <- pcnt + 1
    if ((pcnt%%6) == 1)
    {
        x11()
        par(mfrow=c(3,2))
    }
    # print(paste("tech ...", tech))
    # print(g.tpa.cc.nz[[tech]])
    #
    label <- tech
    #
    levels <- g.tpa.cc.nz[[tech]]$Level
    nlevels <- length(g.tpa.cc.nz[[tech]]$Level)
    cnts <- g.tpa.cc.nz[[tech]]$Cnt
    percentages <- round((cnts/sum(cnts))*100)
    labels <- paste("lvl=",levels,",cnt=", cnts,", ", percentages, "%", sep="")
    #
    slices <- g.tpa.cc.nz[[tech]]$Cnt
    #
    pie(slices, 
        labels=labels, 
        main=label,
        col=rainbow(nlevels))
}

closealldevs()

fcnt <- 1
fname<-""
while (file.exists(fname<-paste("skills-",fcnt,".png",sep="")))
{
    print(paste("deleting file ...", fname))
    file.remove(fname)
    fcnt <- fcnt + 1
}

closealldevs()

# mar <- par("mar")
# closealldevs()

maxrow <- 3
maxcol <- 2
maxwidth <- 600
maxheight <- 500

fcnt <- fcnt + 1;
png(width=maxwidth,height=maxheight,
    file=paste("skills-",fcnt,".png",sep=""))
par(mfrow=c(maxrow,maxcol))

fcnt <- 0
pcnt <- 0

for (tech in names(g.tpa.cc.nz))
{
    pcnt <- pcnt + 1
    #
    # print(paste("tech ...", tech))
    # print(g.tpa.cc.nz[[tech]])
    #
    label <- tech
    #
    levels <- g.tpa.cc.nz[[tech]]$Level
    nlevels <- length(g.tpa.cc.nz[[tech]]$Level)
    cnts <- g.tpa.cc.nz[[tech]]$Cnt
    percentages <- round((cnts/sum(cnts))*100)
    labels <- paste("lvl=",levels,",cnt=", cnts,", ", percentages, "%", sep="")
    #
    slices <- g.tpa.cc.nz[[tech]]$Cnt
    #
    pie(slices, 
        labels=labels, 
        main=label,
        col=rainbow(nlevels))
    #
    if ((pcnt%%(maxrow*maxcol-1)) == 1)
    {
        #par(mar=c(0,0,0,0))
        plot(c(0, 1), c(0, 1), 
             ann = F, 
             bty = 'n', 
             type = 'n', 
             xaxt = 'n', 
             yaxt = 'n')
        text(x=0.5,y=0.5,
            paste("Competence Level:\n",
                  "Level 5 - World Class\n",
                  "Level 4 - Advanced\n",
                  "Level 3 - Intermediate\n",
                  "Level 2 - Basic\n",
                  "Level 1 - Initial\n", sep=""), 
                  col="blue", cex=1.6)
        closealldevs()
        fcnt <- fcnt + 1;
        png(width=maxwidth,height=maxheight,
            file=paste("skills-",fcnt,".png",sep=""))
        par(mfrow=c(maxrow,maxcol))
    }
}

 if ((pcnt%%(maxrow*maxcol-1)) != 1)
 {
     #par(mar=c(0,0,0,0))
     plot(c(0, 1), c(0, 1), 
          ann = F, 
          bty = 'n', 
          type = 'n', 
          xaxt = 'n', 
          yaxt = 'n')
     text(x=0.5,y=0.5,
          paste("Competence Level:\n",
                "Level 5 - World Class\n",
                "Level 4 - Advanced\n",
                "Level 3 - Intermediate\n",
                "Level 2 - Basic\n",
                "Level 1 - Initial\n", sep=""), 
                col="blue", cex=1.6)
}

closealldevs()
