download_narr_daily<-function(var,year,points,folder="/n/koutrakis_lab/lab/One_Table/Data/NARR/"){
  if(file.exists(paste0(folder,var,"_",year,".nc"))){
    variable<-stack(paste0(folder,var,"_",year,".nc"))
  }else{
    link=paste0("https://downloads.psl.noaa.gov/Datasets/NARR/Dailies/monolevel/",var,".",year,".nc")
    curl::curl_download(url=link,destfile = paste0(folder,var,"_",year,".nc"),mode = "wb")
    variable<-stack(paste0(folder,var,"_",year,".nc"))
  }
  points<-spTransform(points,proj4string(variable))
  ext_value<-raster::extract(variable,points)
  ext_value<-c(ext_value)
  ext_value<-as.data.frame(ext_value)
  ext_value$doy<-as.integer(0:(nrow(ext_value)-1)/length(points))+1
  ext_value$Date=as.Date(paste0(year,"-01-01"))+ext_value$doy-1
  names(ext_value)[1]=var
  ext_value$ID=rep.int(x=as.character(points@data[,"ID"]),
                       1+time_length(interval(start=as.Date(paste0(year,"-01-01")),end=as.Date(paste0(year,"-12-31"))),"days"))
  ext_value=ext_value[,colnames(ext_value)!="doy"]
  #unlink(here::here("data","NARR",paste0(var,"_",year,".nc")))
  return(ext_value)
}

download_narr_daily_subsurface<-function(var,year,month,points,folder="/n/koutrakis_lab/lab/One_Table/Data/NARR/"){
  month_c=formatC(month,width = 2,flag=0)
  if(file.exists(paste0(folder,var,"_",year,month_c,".nc"))){
    variable<-stack(paste0(folder,var,"_",year,month_c,".nc"))
  }else{
    link=paste0("https://downloads.psl.noaa.gov/Datasets/NARR/subsurface/",var,".",year,month_c,".nc")
    curl::curl_download(url=link,destfile = paste0(folder,var,"_",year,month_c,".nc"),mode = "wb")
    variable<-stack(paste0(folder,var,"_",year,month_c,".nc"))
  }
  points<-spTransform(points,proj4string(variable))
  ext_value<-raster::extract(variable,points)
  ext_value<-c(ext_value)
  ext_value<-as.data.frame(ext_value)
  ext_value$dom<-as.integer(0:(nrow(ext_value)-1)/length(points))+1
  ext_value$Date=as.Date(paste0(year,"-",month_c,"-01"))+ext_value$dom-1
  names(ext_value)[1]=var
  ext_value$ID=rep.int(x=as.character(points@data[,"ID"]),
                       days_in_month(as.Date(paste0(year,"-",month_c,"-01"))))
  ext_value=ext_value[,colnames(ext_value)!="dom"]
  #unlink(here::here("data","NARR",paste0(var,"_",year,".nc")))
  return(ext_value)
}
