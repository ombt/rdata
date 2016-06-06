sqlcmd = "select Technology, Product, Activity, Level, count(Level) as Cnt from comp group by Technology, Product, Activity, Level "

comp.cnts = sqldf(sqlcmd)
head(comp.cnts)

comp.cnts = comp.cnts[order(comp.cnts$Technology,
                            comp.cnts$Product,
                            comp.cnts$Activity,
                            comp.cnts$Level),]
head(comp.cnts)

grouped.comp.cnts <- 
    split(comp.cnts, 
          list(comp.cnts$Technology,
               comp.cnts$Product))

grouped.comp.cnts

xx = grouped.comp.cnts

