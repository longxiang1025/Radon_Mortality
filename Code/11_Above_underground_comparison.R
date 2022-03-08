library(here)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(raster)
library(psych)

nc_lab_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/harvard.csv",header=T,sep = "\t")
ma_lab_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_MA_190628.csv",header=T)
ma_lab_data$TestPostalCode=as.character(ma_lab_data$TestPostalCode)
pa_lab_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_PA_190628.csv",header=T)
pa_lab_data$TestPostalCode<-as.character(pa_lab_data$TestPostalCode)

load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","GeoData","Boundaries.RData"))
load(here::here("Data","Medium Data","GB_ZIPCODE.RData"))

#clean the date data of NC lab
nc_lab_data$STARTDATE=as.character(nc_lab_data$STARTDATE)
nc_lab_data$ENDDATE=as.character(nc_lab_data$ENDDATE)

nc_lab_data<-nc_lab_data%>%mutate(STARTDATE=ifelse(STARTDATE=="00/00/0000",NA,STARTDATE))
nc_lab_data<-nc_lab_data%>%filter(!is.na(STARTDATE))
nc_lab_data$STARTDATE<-as.Date(nc_lab_data$STARTDATE,"%m/%d/%Y")

nc_lab_data<-nc_lab_data%>%mutate(ENDDATE=ifelse(ENDDATE=="00/00/0000",NA,ENDDATE))
nc_lab_data<-nc_lab_data%>%filter(!is.na(ENDDATE))
nc_lab_data$ENDDATE<-as.Date(nc_lab_data$ENDDATE,"%m/%d/%Y")

nc_lab_data$ZIPCODE=substr(nc_lab_data$POSTALCODE,1,5)
nc_lab_data$PCI.L=as.numeric(as.character(nc_lab_data$PCI.L))
#nc_lab_data=nc_lab_data%>%filter(STATE%in%c("MA","NH","CT","ME","VT","RI"))
nc_lab_data=nc_lab_data%>%filter(ZIPCODE%in%gb_zip$ZIP)
#clean the date data for MA lab
ma_lab_data$StartDate=as.character(ma_lab_data$StartDate)
ma_lab_data$EndDate=as.character(ma_lab_data$EndDate)

ma_lab_data<-ma_lab_data%>%mutate(StartDate=ifelse(StartDate=="00/00/0000",NA,StartDate))
ma_lab_data<-ma_lab_data%>%filter(!is.na(StartDate))
ma_lab_data$StartDate<-as.Date(ma_lab_data$StartDate,"%m/%d/%Y")

ma_lab_data<-ma_lab_data%>%mutate(EndDate=ifelse(EndDate=="00/00/0000",NA,EndDate))
ma_lab_data<-ma_lab_data%>%filter(!is.na(EndDate))
ma_lab_data$EndDate<-as.Date(ma_lab_data$EndDate,"%m/%d/%Y")

ma_lab_data$PCI.L=as.numeric(as.character(ma_lab_data$Result))
ma_lab_data[is.na(ma_lab_data$PCI.L),"PCI.L"]=0
ma_lab_data=ma_lab_data%>%filter(TestPostalCode%in%gb_zip$ZIP)
#ma_lab_data=ma_lab_data%>%filter(TestState%in%c("MA","NH","CT","ME","VT","RI"))
#clean the date data for PA lab
pa_lab_data$StartDate=as.character(pa_lab_data$StartDate)
pa_lab_data$EndDate=as.character(pa_lab_data$EndDate)

pa_lab_data<-pa_lab_data%>%mutate(StartDate=ifelse(StartDate=="00/00/0000",NA,StartDate))
pa_lab_data<-pa_lab_data%>%filter(!is.na(StartDate))
pa_lab_data$StartDate<-as.Date(pa_lab_data$StartDate,"%m/%d/%Y")

pa_lab_data<-pa_lab_data%>%mutate(EndDate=ifelse(EndDate=="00/00/0000",NA,EndDate))
pa_lab_data<-pa_lab_data%>%filter(!is.na(EndDate))
pa_lab_data$EndDate<-as.Date(pa_lab_data$EndDate,"%m/%d/%Y")

pa_lab_data$PCI.L=as.numeric(as.character(pa_lab_data$Result))
pa_lab_data[is.na(pa_lab_data$PCI.L),"PCI.L"]=0
pa_lab_data=pa_lab_data%>%filter(TestPostalCode%in%gb_zip$ZIP)
#pa_lab_data=pa_lab_data%>%filter(TestState%in%c("MA","NH","CT","ME","VT","RI"))
#select the measurements in period
ma_lab_data$Year=lubridate::year(ma_lab_data$StartDate)
pa_lab_data$Year=lubridate::year(pa_lab_data$StartDate)
nc_lab_data$Year=lubridate::year(nc_lab_data$STARTDATE)

ma_lab_data=ma_lab_data%>%filter(Year>2003,Year<2020)
pa_lab_data=pa_lab_data%>%filter(Year>2003,Year<2020)
nc_lab_data=nc_lab_data%>%filter(Year>2003,Year<2020)
#
nc_first_dates=nc_lab_data%>%group_by(FINGERPRINT,TESTTYPE)%>%summarise(first_date=min(STARTDATE))
nc_lab_data=nc_lab_data%>%left_join(nc_first_dates)
nc_pairs=nc_lab_data%>%group_by(FINGERPRINT,TESTTYPE,STARTDATE)%>%
  filter(TESTTYPE%in%c("FIRST","FOLLOWUP","REALESTATE"),
         STARTDATE==first_date)%>%
  summarise(n_base=sum(FLOOR=="basement"),
            n_above=sum(FLOOR%in%c("first","second")))
nc_pairs=nc_pairs%>%filter(n_base>0,
                           n_above>0,
                           n_base<10)
nc_lab_data=nc_pairs%>%left_join(nc_lab_data)
#nc_lab_data[nc_lab_data$PCI.L==0,"PCI.L"]=runif(n=nrow(nc_lab_data[nc_lab_data$PCI.L==0,]),
#                                                min=0,
#                                                max=0.4)

nc_lab_data=nc_lab_data%>%
  filter(TESTTYPE%in%c("FIRST","FOLLOWUP","REALESTATE"))%>%
  filter(FLOOR%in%c("basement","first","second"))%>%
  filter(STARTDATE==first_date)%>%
  filter(FINGERPRINT%in%nc_pairs$FINGERPRINT)

ma_first_dates=ma_lab_data%>%group_by(Checksum_TestAddress)%>%summarise(First_Date=min(StartDate))
ma_lab_data=ma_lab_data%>%left_join(ma_first_dates)
ma_lab_data=ma_lab_data%>%filter(StartDate==First_Date)
ma_pairs=ma_lab_data%>%group_by(Checksum_TestAddress,StartDate)%>%
  summarise(n_base=sum(Floor=="Basement"),
            n_above=sum(Floor%in%c("First","Second")))
ma_pairs=ma_pairs%>%filter(n_base>0,
                           n_above>0,
                           n_base<10)
ma_lab_data=ma_pairs%>%left_join(ma_lab_data)

ma_lab_data=ma_lab_data%>%filter(Checksum_TestAddress%in%ma_pairs$Checksum_TestAddress,
                                Floor%in%c("Basement","First","Second"))

#
nc_lab_data$HOURS=as.numeric(as.character(nc_lab_data$HOURS))
nc_records=nc_lab_data%>%
  group_by(FINGERPRINT,STARTDATE,FLOOR=="basement")%>%
  summarise(mean_radon=mean(PCI.L),
            duration=mean(HOURS))
names(nc_records)=c("ID","Start_Date","Basement","Radon","Duration")
nc_records=bind_cols(nc_records%>%filter(Basement),
                     nc_records%>%filter(!Basement))
nc_records=nc_records[,c(1,2,4,9,10)]
names(nc_records)=c("ID","StartDate","Basement","Aboveground","Duration")
nc_records$lab="NC"
nc_records=nc_records%>%filter(Duration>71,Duration<169)

#
ma_records=ma_lab_data%>%group_by(Checksum_TestAddress,StartDate,Floor=="Basement")%>%summarise(mean_radon=mean(PCI.L),
                                                                                                mean_duration=mean(Hours))
names(ma_records)=c("ID","StartDate","Basement","Radon","Duration")
ma_records=bind_cols(ma_records%>%filter(Basement),
                     ma_records%>%filter(!Basement))
ma_records=ma_records[,c(1,2,4,9,10)]
names(ma_records)=c("ID","StartDate","Basement","Aboveground","Duration")
ma_records$lab="MA"
ma_records$ID=as.character(ma_records$ID)

ma_long_records=ma_records%>%filter(Duration>200)

ma_records=ma_records%>%filter(Duration>47,Duration<97)

gb_records=bind_rows(nc_records,ma_records,ma_long_records)
gb_records=gb_records%>%filter(Basement>0)

nc_ID_zipcode=unique(nc_lab_data[,c("FINGERPRINT","POSTALCODE","METHOD")])
names(nc_ID_zipcode)=c("ID","ZIPCODE","METHOD")
ma_ID_zipcode=unique(ma_lab_data[,c("Checksum_TestAddress","TestPostalCode","Method")])
names(ma_ID_zipcode)=c("ID","ZIPCODE","METHOD")
ma_ID_zipcode$ID=as.character(ma_ID_zipcode$ID)

ID_zipcode=bind_rows(nc_ID_zipcode,
                     ma_ID_zipcode)
gb_records=gb_records%>%left_join(ID_zipcode)
gb_records$ZIPCODE=substr(gb_records$ZIPCODE,1,5)
gb_records$METHOD=as.character(gb_records$METHOD)
save(file=here::here("Data","Above_Basement_Comparison.RData"),
     gb_records)
