#
plot_skills <- function(ptype="x11")
{
    if (ptype == "x11")
    {
        maxrow <- 2
        maxcol <- 1
        maxwidth <- 10
        maxheight <- 8.0
        pointsize <- 14
        colors <- rainbow(2)
    }
    else if (ptype == "png")
    {
        maxrow <- 2
        maxcol <- 1
        maxwidth <- 1400
        maxheight <- 800
        pointsize <- 20
        colors <- rainbow(2)
    }
    #
    oc <- orig.cert
    names(oc)
    #
    sources <- sort(unique(oc$Source))
    sources
    #
    for (source in sources)
    {
        print(paste("Source is ...", source))
        #
        src <- "Unknown"
        if (length(dummy <- grep("External", source)))
        {
            src <- "External"
        }
        else if (length(dummy <- grep("Internal", source)))
        {
            src <- "Internal"
        }
        print(paste("Source and src are ...", source, src))
        #
        groups <- sort(unique(oc$Group[oc$Source==source]))
        groups
        #
        fcnt <- 0
        pcnt <- 0
        for (group in groups)
        {
            heights <- c()
            labels <- c()
            print(paste("Source, Group are ...", source, group))
            certifications <- 
                sort(unique(oc$Certification[(oc$Group==group)&(oc$Source==source)]))
            for (certification in certifications)
            {
                print(paste("Source, Group, Certification are ...", source, group, certification))
                counts <- table(oc[(oc$Source==source)&(oc$Group==group)&(oc$Certification==certification),7], useNA="always")
                if (is.na(counts["Yes"]))
                {
                    counts["Yes"] <- 0
                }
                if (is.na(counts["No"]))
                {
                    counts["No"] <- 0
                }
                print(counts)
                if (counts["Yes"] > 0)
                {
                    labels <- c(labels, certification)
                    heights <- c(heights, counts["Yes"])
                }
            }
            if (length(heights) > 0)
            {
                pcnt <- pcnt + 1
                if ((pcnt%%2) == 1)
                {
                    if (ptype == "x11")
                    {
                        x11(width=maxwidth,
                            height=maxheight,
                            pointsize=pointsize)
                        par(mfrow=c(maxrow,maxcol))
                    }
                    else if (ptype == "png")
                    {
                        closealldevs()
                        fcnt <- fcnt + 1;
                        png(width=maxwidth,
                            height=maxheight,
                            pointsize=pointsize,
                            file=paste("skills-",src,"-cert-",fcnt,".png",sep=""))
                        par(mfrow=c(maxrow,maxcol))
                    }
                    else
                    {
                        x11()
                        par(mfrow=c(2,1))
                    }
                }
                maxy <- ((max(heights)%/%10)+1)*10
                barplot(heights,
                        main=paste(source,":", group),
                        names.arg=gsub(" ", "\n", labels),
                        ylim=c(0,maxy),
                        ylab="Head Count")
            }
        }
    }
    #
    if (ptype == "png")
    {
        closealldevs()
    }
}
