rm(list=ls())
# Calculate the annual average temperature change from monthly average, minimum or
# maximum data over the past 50 (or 100) years at all stations with more than 
# 50 (or 100) years of data after 1950 (or 1900) and prepares plots.

for (ttype in c("tavg","tmin","tmax")){
  fn <- paste0(ttype,"-qca.RData")
  # Load data
  if (!file.exists(fn)) stop("File not found")
  load(fn)
  # Discard QA/QC columns and years before 1900
  dat <- dat[dat$YEAR>=1900,-grep("FLAG",names(dat))]
  # Discard stations with less than 50 years of data after 1950
  selSta <- which(table(dat$ID[dat$YEAR>1950])>=50)
  dat <- dat[dat$ID %in% levels(dat$ID)[selSta],]
  dat$ID <- factor(dat$ID)
  # Add columns to inventory dataframe for temperature change
  inv$dT50  <- inv$dT100 <- inv$pval50 <- inv$pval100 <- rep(NA,nrow(inv))
  # Open graphics output file
  pdf(file=paste0(ttype,"-plots.pdf"))
  # Loop through all selected stations
  for (i in levels(dat$ID)){
    dat1 <- dat[dat$ID==i,]
    inv1 <- inv[inv$ID==i,]
    # Compute the baseyear average monthly temperature for the station
    avg <- apply(dat1[dat1$YEAR>=1951&dat1$YEAR<=2000,4:15],2,mean,na.rm=TRUE)
    # Compute temperature anomaly by subtracting average from monthly value
    dat1[,4:15] <- t(apply(dat1[,4:15],1,function(x){x-avg}))/100
    # Compute annual average anomalies for the stattion
    dat1$ann <- apply(dat1[,4:15],1,mean,na.rm=TRUE)
    # If sufficient data, determine best fit line for the past period
    # At least 50 years of data since 1950
    lm1 <- lm(ann~YEAR,data=dat1[dat1$YEAR>=1964&dat1$YEAR<=2013,])
    # Multiply slope of best fit line  (deg per year) by 10 to obtain 
    # the decadal temperature change and write to inventory dataframe
    inv$dT50[inv$ID==i] <- coef(lm1)[2]*10
    # Extract regression p-value and write to inventory dataframe
    inv$pval50[inv$ID==i] <- summary(lm1)$coef[2,4]
    # At least 100 years of data
    if (nrow(dat1)>=100){
      lm2 <- lm(ann~YEAR,data=dat1[dat1$YEAR>=1914&dat1$YEAR<=2013,])
      # Multiply slope of best fit line  (deg per year) by 10 to obtain 
      # the decadal temperature change and write to inventory dataframe
      inv$dT100[inv$ID==i] <- coef(lm2)[2]*10
      # Extract regression p-value and write to inventory dataframe
      inv$pval100[inv$ID==i] <- summary(lm2)$coef[2,4]
    }
    # Create a plot of the data with regression line
    main.title <- paste(inv1$NAME,"Lat = ",inv1$LATITUDE,
                        "Long = ",inv1$LONGITUDE)
    plot(dat1$YEAR,dat1$ann,type="b",main=main.title,
        ylab="Temperature Anomaly, K",xlab="Year",
        xlim=c(1900,2014))
    if (!is.na(inv$dT50[inv$ID==i])) abline(lm1,col=2,lwd=2)
    if (!is.na(inv$dT100[inv$ID==i])) abline(lm2,col=3,lwd=2)
  }
dev.off()
out <- inv[!is.na(inv$dT50),]
save(file=paste0(ttype,"_out.RData"),list=c("out"))
}