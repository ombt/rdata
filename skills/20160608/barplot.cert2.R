#
oc <- orig.cert
names(oc)
#
groups <- sort(unique(oc$Group))
groups
#
pcnt <- 0
for (group in groups)
{
    heights <- c()
    labels <- c()
    print(paste("Group is ...", group))
    certifications <- sort(unique(oc$Certification[oc$Group==group]))
    for (certification in certifications)
    {
        print(paste("Group, Certification are ...", group, certification))
        counts <- table(oc[(oc$Group==group)&(oc$Certification==certification),7], useNA="always")
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
            x11()
            par(mfrow=c(2,1))
        }
        maxy <- ((max(heights)%/%10)+1)*10
        barplot(heights,
                main=group,
                names.arg=gsub(" ", "\n", labels),
                ylim=c(0,maxy),
                ylab=group)
    }
}
