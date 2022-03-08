index=as.numeric(Sys.getenv("Sim"))
library(lubridate)
library(RCurl)
table=expand.grid(year=1979:2019,month=formatC(1:12,width = 2,flag = "0"))

file_name=paste0("ftp://arlftp.arlhq.noaa.gov/narr/NARR",paste0(table[index,"year"],table[index,"month"]))
setwd("/n/holyscratch01/koutrakis_lab/longxiang/HYSPLIT/")

cal=T
if(file.exists(paste0("NARR/NARR",table[index,"year"],table[index,"month"]))){
  if(file.size(paste0("NARR/NARR",table[index,"year"],table[index,"month"]))>2700000000){
    cal=F
  }else{
    cal=T
  }
}
if(cal){
  downloader::download( url = file_name,
                        destfile = paste0("NARR/NARR",table[index,"year"],table[index,"month"]),
                        method = "auto",
                        quiet = FALSE,
                        mode = "w",
                        cacheOK = TRUE)
}



