rm(list=ls())
# Calculate the annual average temperature change from monthly average, minimum or
# maximum data over the past 50 (or 100) years at all stations with more than 
# 50 (or 100) years of data after 1950 (or 1900) and write data to output files.

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
  # Add columns to inventory dataframe for regression p-value, annual temperature 
  # change (slope dT50/dT100) and intercept (T50/T100) of best fit line
  inv$pval50 <- inv$pval100 <- rep(NA,nrow(inv))
  inv$dT50  <- inv$dT100 <- rep(NA,nrow(inv))
  inv$T50  <- inv$T100 <- rep(NA,nrow(inv))
  # Create empty data frame
  dat.out <- data.frame(ID=character(0),YEAR=integer(0),AVGTEMP=numeric(0))
  # Loop through all selected stations
  for (i in levels(dat$ID)){
    dat1 <- dat[dat$ID==i,]
    inv1 <- inv[inv$ID==i,]
    # Compute the baseyear average monthly temperature for the station
    avg <- apply(dat1[dat1$YEAR>=1951&dat1$YEAR<=2000,4:15],2,mean,na.rm=TRUE)
    # Compute temperature anomaly by subtracting average from monthly value
    dat1[,4:15] <- t(apply(dat1[,4:15],1,function(x){x-avg}))/100
    # Compute annual average anomalies for the stattion
    dat1$AVGTEMP <- apply(dat1[,4:15],1,mean,na.rm=TRUE)
    # Write normalized data to output
    dat.out <- rbind(dat.out,data.frame(ID=i,YEAR=dat1$YEAR,AVGTEMP=dat1$AVGTEMP))
    # If sufficient data, determine best fit line for the past period
    # At least 50 years of data since 1950
    lm1 <- lm(AVGTEMP~YEAR,data=dat1[dat1$YEAR>=1964&dat1$YEAR<=2013,])
    # Write slope = annual temperature change and intercept to inventory dataframe
    inv$T50[inv$ID==i] <- coef(lm1)[1]
    inv$dT50[inv$ID==i] <- coef(lm1)[2]
    # Extract regression p-value and write to inventory dataframe
    inv$pval50[inv$ID==i] <- summary(lm1)$coef[2,4]
    # At least 100 years of data
    if (nrow(dat1)>=100){
      lm2 <- lm(AVGTEMP~YEAR,data=dat1[dat1$YEAR>=1914&dat1$YEAR<=2013,])
      inv$T100[inv$ID==i] <- coef(lm2)[1]
      inv$dT100[inv$ID==i] <- coef(lm2)[2]
      inv$pval100[inv$ID==i] <- summary(lm2)$coef[2,4]
    }
  }
# Discard stations with no computed 50-year trend
inv.out <- inv[!is.na(inv$dT50),]
# Write time series data and inventory to output
save(file=paste0(ttype,"_out.RData"),list=c("inv.out","dat.out"))
}