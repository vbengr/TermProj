library(maps)
rm(list=ls())

ttype <- "tavg"

# Load data
if (!file.exists(paste0(ttype,"_out.RData"))) stop("File not found")
load(paste0(ttype,"_out.RData"))

out$LATRANGE <- cut(out$LATITUDE,seq(-90,90,30))
out$ELEVRANGE <- cut(out$GRELEV,seq(0,3000,1000))

par(las=1,cex.axis=1)

#boxplot(dT~reorder(STVEG, dT, median), data=out,horizontal=TRUE,notch=TRUE)

out$Trend50 <- factor(ifelse(out$pval50>0.05,"No Trend",
                           ifelse(out$dT50>0,"Increasing","Decreasing")))
out$Trend100 <- factor(ifelse(out$pval100>0.05,"No Trend",
                           ifelse(out$dT100>0,"Increasing","Decreasing")))
#out$dTRange <- cut(out$dT,-3:4)

map("world",col="gray")
palette(c("blue","red","green"))
points(out$LONGITUDE,out$LATITUDE,pch=19,col=out$Trend50,cex=0.5)
