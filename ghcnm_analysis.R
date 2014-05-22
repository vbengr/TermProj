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
# Stations by Numbers and Percentage, average temperature change per decade
# 50-year trend
print(table(inv.out$Trend50))
print(table(inv.out$Trend50)/sum(table(inv.out$Trend50)))
print(tapply(inv.out$dT50,inv.out$Trend50,mean,na.rm=TRUE)*10)
# 100-year trend
print(table(inv.out$Trend100))
print(table(inv.out$Trend100)/sum(table(inv.out$Trend100)))
print(tapply(inv.out$dT100,inv.out$Trend100,mean,na.rm=TRUE)*10)
# Is the temperature change accelerating? Check if dT50>dT100
table(inv.out$dT50>inv.out$dT100)

# Plot function
plot_func <- function(i){
  dat1 <- dat.out[dat.out$ID==i,]
  inv1 <- inv.out[inv.out$ID==i,]
  main.title <- paste0("Station ",inv1$ID," Lat = ",inv1$LATITUDE,
                      " Long = ",inv1$LONGITUDE)
  plot(dat1$YEAR,dat1$AVGTEMP,type="b",main=main.title,
       ylab="Temperature Anomaly, K",xlab="Year",
       xlim=c(1900,2014),ylim=c(-2,2))
  # Best fit line for 50-year trend
  a50 = inv.out$T50[inv.out$ID==i]
  b50 = inv.out$dT50[inv.out$ID==i]
  abline(a50,b50,col=2,lwd=2) 
  # Best fit line for 100-year trend where computed
  a100 = inv.out$T100[inv.out$ID==i]
  b100 = inv.out$dT100[inv.out$ID==i]
  if (!is.na(b100)) abline(a100, b100,col=3,lwd=2)
  legend("topleft",
         legend=c(paste0("50-year Trend: ",format(b50*10,digits=2)," deg/decade"),
                  paste0("100-year Trend: ",format(b100*10,digits=2)," deg/decade")),
         lty=1,lwd=2,col=2:3,cex=0.8)
}

# Prepare plots for all stations
# Open graphics output file
lplot <- FALSE
if (!lplot){
  pdf(file=paste0(ttype,"-plots.pdf"))
  par(las=1,cex.axis=1)
  for (i in levels(dat.out$ID)){
    plot_func(as.character(i))
  }
  dev.off()
}

# Prepare plots for selected stations as examples
selID <- grep("BATES CREEK|PORT ELIZ|HARARE",inv.out$NAME)
#png(file="Figure1.png",width=480,height=960)
pdf(file="Figure1.pdf",width=6,height=10)
par(mfrow=c(3,1),
    mar=c(5,10,3,10)
    )
for (i in selID){
  plot_func(as.character(inv.out$ID[i]))
}
dev.off()

pdf(file="Figure2.pdf",width=9,height=6)
map("world",col="darkgray")
palette(c("blue","red","green"))
points(inv.out$LONGITUDE,inv.out$LATITUDE,col=inv.out$Trend50,cex=0.2,pch=20)
legend("bottomleft",
       legend=c("Decreasing 50-Year Trend","Increasing 50-Year Trend","No 50-Year Trend"),
       pch=19,col=1:3,cex=0.7,bg="white")
dev.off()