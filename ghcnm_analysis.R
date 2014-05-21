library(maps)
rm(list=ls())

# Select average temperature data
ttype <- "tavg"
# Load data
if (!file.exists(paste0(ttype,"_out.RData"))) stop("File not found")
load(paste0(ttype,"_out.RData"))

# Append a trend attribute to the inventory. If p-value is > 0.05, slope is
# insignificant -> No trend. Print output table
inv.out$Trend50 <- factor(ifelse(inv.out$pval50>0.05,"No Trend",
                                 ifelse(inv.out$dT50>0,"Increasing","Decreasing")))
inv.out$Trend100 <- factor(ifelse(inv.out$pval100>0.05,"No Trend",
                                  ifelse(inv.out$dT100>0,"Increasing","Decreasing")))
print(table(inv.out$Trend50))
print(table(inv.out$Trend100))

# Plot function
plot_func <- function(i){
  dat1 <- dat.out[dat.out$ID==i,]
  inv1 <- inv.out[inv.out$ID==i,]
  main.title <- paste("Station ",inv1$ID," Lat = ",inv1$LATITUDE,
                      " Long = ",inv1$LONGITUDE)
  plot(dat1$YEAR,dat1$AVGTEMP,type="b",main=main.title,
       ylab="Temperature Anomaly, K",xlab="Year",
       xlim=c(1900,2014),ylim=c(-2,2))
  # Best fit line for 50-year trend
  a = inv.out$T50[inv.out$ID==i]
  b = inv.out$dT50[inv.out$ID==i]
  abline(a,b,col=2,lwd=2) 
  # Best fit line for 100-year trend where computed
  a = inv.out$T100[inv.out$ID==i]
  b = inv.out$dT100[inv.out$ID==i]
  if (!is.na(b)) abline(a, b,col=3,lwd=2)
  legend("topleft",legend=c("50-year Trend","100-year Trend"),
         lty=1,lwd=2,col=2:3,cex=0.8)
}

# Prepare plots for all stations
# Open graphics output file
pdf(file=paste0(ttype,"-plots.pdf"))
par(las=1,cex.axis=1)
for (i in levels(dat.out$ID)){
  plot_func(as.character(i))
}
dev.off()

# Prepare metafile plots for selected stations as examples
selID <- grep("BATES CREEK|PORT ELIZ|HARARE",inv.out$NAME)
png(file="SelPlots.png",width=480,height=960)
par(mfrow=c(3,1))
for (i in selID){
  plot_func(as.character(inv.out$ID[i]))
}
dev.off()

map("world",col="gray")
palette(c("blue","red","green"))
points(inv.out$LONGITUDE,inv.out$LATITUDE,pch=19,col=inv.out$Trend50,cex=0.5)
