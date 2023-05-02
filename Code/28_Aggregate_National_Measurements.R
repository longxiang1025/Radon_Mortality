library(dplyr)
library(readr)
library(digest)
library(sf)
library(lubridate)
library(cowplot)
library(grid)
library(gridExtra)
library(ggplot2)
library(sp)
library(mgcv)
sf_use_s2(FALSE)

pattern <- function(x, size, pattern) {
  ex = list(
    horizontal = c(1, 2),
    vertical = c(1, 4),
    left2right = c(2, 4),
    right2left = c(1, 3)
  )
  fillgrid = st_make_grid(x, cellsize = size)
  endsf = lapply(1:length(fillgrid), function(j)
    sf::st_linestring(sf::st_coordinates(fillgrid[j])[ex[[pattern]], 1:2]))
  endsf = sf::st_sfc(endsf, crs = sf::st_crs(x))
  endsf = sf::st_intersection(endsf, x)
  endsf = endsf[sf::st_geometry_type(endsf)
                %in% c("LINESTRING", "MULTILINESTRING")]
  endsf = sf::st_line_merge(sf::st_union(endsf))
  return(endsf)
}

geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#Process the raw table data (No need to run every time)------------------------------------

ma_lab_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_MA_190628.csv",header=T)
ma_lab_data$TestPostalCode=as.character(ma_lab_data$TestPostalCode)

ma_lab_data2<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_20220430_MA.csv",header=F)
names(ma_lab_data2)=names(ma_lab_data)
ma_lab_data2$TestPostalCode=as.character(ma_lab_data2$TestPostalCode)
ma_lab_data=bind_rows(ma_lab_data,ma_lab_data2)

pa_lab_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_PA_190628.csv",header=T)
pa_lab_data$TestPostalCode<-as.character(pa_lab_data$TestPostalCode)

pa_lab_data_2<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_PA_190701_201031.csv",header=T)
pa_lab_data_2$TestPostalCode<-as.character(pa_lab_data_2$TestPostalCode)

pa_lab_data_3<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_20220430_PA.csv",header=F)
pa_lab_data_3=pa_lab_data_3[,c(1,3:16)]
names(pa_lab_data_3)=names(pa_lab_data)
pa_lab_data_3$TestPostalCode<-as.character(pa_lab_data_3$TestPostalCode)

pa_lab_data=bind_rows(pa_lab_data,pa_lab_data_2,pa_lab_data_3)

as_data=bind_rows(ma_lab_data,pa_lab_data)
as_data$ID=paste0(as_data$Checksum_TestAddress,as_data$TestPostalCode)

as_data$StartDate=as.character(as_data$StartDate)
as_data$EndDate=as.character(as_data$EndDate)

as_data<-as_data%>%mutate(StartDate=ifelse(StartDate=="00/00/0000",NA,StartDate))
as_data<-as_data%>%filter(!is.na(StartDate))
as_data$StartDate<-as.Date(as_data$StartDate,"%m/%d/%Y")

as_data<-as_data%>%mutate(EndDate=ifelse(EndDate=="00/00/0000",NA,EndDate))
as_data<-as_data%>%filter(!is.na(EndDate))
as_data$EndDate<-as.Date(as_data$EndDate,"%m/%d/%Y")

as_data$PCI.L=as.numeric(as.character(as_data$Result))
as_data[is.na(as_data$PCI.L),"PCI.L"]=0

as_data=as_data[,c("StartDate","EndDate","Checksum_TestAddress","TestState","TestPostalCode","Floor","Result","Method","PCI.L","ID","DeviceNumber")]
as_data$TestState=as.character(as_data$TestState)
as_data$Floor=as.character(as_data$Floor)
as_data$Result=as.character(as_data$Result)
as_data$Method=as.character(as_data$Method)
as_data$Checksum_TestAddress=as.character(as_data$Checksum_TestAddress)
##The AirChek data is not included because of the incompatible coding of address####
nc_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/harvard_201031.csv",header=T,sep = "\t")
nc_data$POSTALCODE=as.character(nc_data$POSTALCODE)
nc_data$STARTDATE=as.character(nc_data$STARTDATE)
nc_data$ENDDATE=as.character(nc_data$ENDDATE)

nc_data<-nc_data%>%mutate(STARTDATE=ifelse(STARTDATE=="00/00/0000",NA,STARTDATE))
nc_data<-nc_data%>%filter(!is.na(STARTDATE))
nc_data$STARTDATE<-as.Date(nc_data$STARTDATE,"%m/%d/%Y")

nc_data<-nc_data%>%mutate(ENDDATE=ifelse(ENDDATE=="00/00/0000",NA,ENDDATE))
nc_data<-nc_data%>%filter(!is.na(ENDDATE))
nc_data$ENDDATE<-as.Date(nc_data$ENDDATE,"%m/%d/%Y")
nc_data$ID=paste0(nc_data$FINGERPRINT,nc_data$PostalCode)
nc_data$ZIPCODE=substr(nc_data$POSTALCODE,1,5)
nc_data$FLOOR=as.character(nc_data$FLOOR)
nc_data$Result=as.character(nc_data$PCI.L)
nc_data=nc_data[,c("STARTDATE","ENDDATE","FINGERPRINT","STATE","ZIPCODE","FLOOR","Result","METHOD","PCI.L","ID","KITNUMBER")]
names(nc_data)=names(as_data)
nc_data$Checksum_TestAddress=as.character(nc_data$Checksum_TestAddress)
nc_data$TestState=as.character(nc_data$TestState)
nc_data$Method=as.character(nc_data$Method)
nc_data$PCI.L=as.numeric(nc_data$Result)
nc_data=nc_data%>%filter(Method=="AC")
nc_data$Method=("AirChek")

## Data from Alpha Energy----------------------------------
tx_lab_data1<-readxl::read_excel("/n/koutrakis_lab/lab/Group_Data/Radon/Data_2000_2010.xlsx")
tx_lab_data2<-readxl::read_excel("/n/koutrakis_lab/lab/Group_Data/Radon/Data_2011_2015.xlsx")
tx_lab_data3<-readxl::read_excel("/n/koutrakis_lab/lab/Group_Data/Radon/Data_2016_2021.xlsx")
tx_lab_data3=tx_lab_data3%>%dplyr::select(names(tx_lab_data2))
tx_lab_data3$LLD_c=as.double(tx_lab_data3$LLD_c)
tx_lab_data=bind_rows(tx_lab_data1,tx_lab_data2,tx_lab_data3)
tx_lab_data1$StartDate=as.Date(tx_lab_data1$startDate)
tx_lab_data1$EndDate=as.Date(tx_lab_data1$stopDate)
tx_lab_data$Checksum_TestAddress=
  apply(tx_lab_data[,"testAddress1"],MARGIN = 1,FUN = function(x) digest::digest(x))
print(Sys.time())
tx_lab_data=tx_lab_data%>%dplyr::filter(testType=="Activated Charcoal")
names(tx_lab_data)[27]="TestState"
names(tx_lab_data)[28]="TestPostalCode"
names(tx_lab_data)[11]="StartDate"
names(tx_lab_data)[13]="EndDate"
names(tx_lab_data)[31]="Floor"
names(tx_lab_data)[6]="PCI.L"
names(tx_lab_data)[7]="Result"
names(tx_lab_data)[30]="TestCountry"
tx_lab_data$Method="Alpha_AC"
tx_lab_data=tx_lab_data[tx_lab_data$TestCountry=="USA",]
tx_lab_data=tx_lab_data[tx_lab_data$testReason%in%c("Personal Knowledge","Real Estate Transaction"," Real-Estate Transaction","Real Estate Transaction & Personal Knowledge"),]
names(tx_lab_data)[1]="ID"
tx_lab_data$DeviceNumber=tx_lab_data$ID
tx_lab_data$ID=as.character(tx_lab_data$ID)
tx_lab_data$ID=paste0("AF",tx_lab_data$ID)
tx_lab_data=tx_lab_data[!is.na(tx_lab_data$PCI.L),]
tx_lab_data=tx_lab_data[!is.na(tx_lab_data$EndDate),]
tx_lab_data=tx_lab_data[!is.na(tx_lab_data$StartDate),]
tx_lab_data=tx_lab_data%>%dplyr::select(names(nc_data))
tx_lab_data$TestState=stringi::stri_trans_toupper(tx_lab_data$TestState)
tx_lab_data=tx_lab_data[tx_lab_data$TestState%in%state.abb,]
tx_lab_data=tx_lab_data%>%select(names(nc_data))
#Clean up the state names of alpha energy
lab_data=bind_rows(as_data,nc_data,tx_lab_data)
save(lab_data,file="Merged_National_Measurements_230306.RData")
