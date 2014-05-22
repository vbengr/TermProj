rm(list=ls())

# Downloads minimum, average and maximum monthly temperature data from the NOAA
# Global Historical Climatology Network and reads the data into R objects for 
# further analysis. Missing data data ("-9999") are converted to NA.

baseurl <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/"
for (ttype in c("tavg","tmin","tmax")){
  # Filenames
  fn0 <- paste0("ghcnm.",ttype,".latest.qca.tar.gz")
  timestamp <- Sys.time()
  # Download archive to temporary directory
  temp <- tempfile()
  download.file(paste0(baseurl,fn0),temp)
  fnames <- untar(temp,list=TRUE)[7:8]
  untar(temp)
  unlink(temp)
  # Read Data File
  dat <- read.fwf(fnames[1],widths = c(11,4,4,rep(c(5,1,1,1),12)))
  names(dat) <- c("ID","YEAR","EVENT",
                paste0(rep(c("VALUE","DMFLAG","QCFLAG","DSFLAG"),12),rep(1:12,each=4)))
  dat$ID <- factor(dat$ID)
  dat[dat==-9999] <- NA
  # Read Inventory File
  inv <- read.fwf(fnames[2],widths = c(11,-1,8,-1,9,-1,6,-1,30,-1,
                                     4,1,5,2,2,2,2,1,2,16,1),fill=TRUE)
  names(inv) <- c("ID","LATITUDE","LONGITUDE","STNELEV","NAME",
                "GRELEV","POPCLS","POPSIZ","TOPO","STVEG",
                "STLOC","OCNDIS","AIRSTN","TOWNDIS","GRVEG",
                "POPCSS")
  inv$ID <- factor(inv$ID)
  # Save data frames and timestamp to Rdata file
  save(file=paste0(ttype,"-qca.RData"),list=c("dat","inv","timestamp"))
}