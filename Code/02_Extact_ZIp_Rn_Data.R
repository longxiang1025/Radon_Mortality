library(here)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(raster)

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
#select the basement measurments
nc_lab_data<-nc_lab_data%>%filter(FLOOR%in%c("basement","unknown"))
ma_lab_data<-ma_lab_data%>%filter(Floor%in%c("Basement","Unknown"))
pa_lab_data<-pa_lab_data%>%filter(Floor%in%c("Basement","Unknown"))
# select measurement with reasonable duractions
nc_lab_data$HOURS=as.numeric(as.character(nc_lab_data$HOURS))
nc_lab_data=nc_lab_data[!is.na(nc_lab_data$HOURS),]
nc_lab_data<-nc_lab_data%>%filter(HOURS>71,HOURS<169)

ma_lab_data=ma_lab_data%>%filter(Hours>47,Hours<97)
pa_lab_data=pa_lab_data%>%filter(Hours>47,Hours<97)
# select first time measurement
nc_repair=nc_lab_data%>%group_by(FINGERPRINT)%>%summarise(repair_status=ifelse("AFTERREPAIRS"%in%TESTTYPE,TRUE,FALSE))
repair_date=nc_lab_data%>%
  right_join(nc_repair%>%filter(repair_status))%>%
  filter(TESTTYPE=="AFTERREPAIRS")%>%
  group_by(FINGERPRINT)%>%
  dplyr::select(FINGERPRINT,ENDDATE)%>%
  summarise(Repair_Date=min(ENDDATE))

names(repair_date)=c("FINGERPRINT","Repair_Date")
nc_lab_data=nc_lab_data%>%left_join(repair_date)

repaired=nc_lab_data%>%filter(!is.na(Repair_Date))
un_repaired=nc_lab_data%>%filter(is.na(Repair_Date))
#in the repaired home, remove all records after repairment except real property check
repaired=repaired%>%filter(!(ENDDATE>=Repair_Date & TESTTYPE%in%c("AFTERREPAIRS","FOLLOWUP")))
nc_lab_data=bind_rows(repaired,un_repaired)

ma_lab_data_date=ma_lab_data%>%group_by(Checksum_TestAddress)%>%summarise(StartDate=min(StartDate))
ma_lab_data=ma_lab_data_date%>%left_join(ma_lab_data)

pa_lab_data_date=pa_lab_data%>%group_by(Checksum_TestAddress)%>%summarise(StartDate=min(StartDate))
pa_lab_data=pa_lab_data_date%>%left_join(pa_lab_data)
#Exclude measurments below detection limit
nc_lab_data=nc_lab_data%>%filter(PCI.L>0.2,PCI.L<200)
ma_lab_data=ma_lab_data%>%filter(PCI.L>0.4,PCI.L<200)
pa_lab_data=pa_lab_data%>%filter(PCI.L>0.4,PCI.L<200)
#Merge them together
nc_lab_data$Type="AirChek"
ma_lab_data$Type="AccuStar"
pa_lab_data$Type="AccuStar"

nc_lab_data=nc_lab_data[,c("STARTDATE","ENDDATE","ZIPCODE","STATE","COUNTY","CITY","PCI.L","FINGERPRINT","HOURS","Type","METHOD")]
ma_lab_data=ma_lab_data[,c("StartDate","EndDate","TestPostalCode","TestState","County","TestCity","PCI.L","Checksum_TestAddress","Hours","Type","Method")]
#pa_lab_data=pa_lab_data[,c("StartDate","EndDate","TestPostalCode","TestState","County","TestCity","PCI.L","Checksum_TestAddress","Hours","Type","Method")]
names(ma_lab_data)=names(nc_lab_data)
#names(pa_lab_data)=names(nc_lab_data)
nc_lab_data$FINGERPRINT=as.character(nc_lab_data$FINGERPRINT)
ma_lab_data$FINGERPRINT=as.character(ma_lab_data$FINGERPRINT)
#pa_lab_data$FINGERPRINT=as.character(pa_lab_data$FINGERPRINT)

ne_radon=bind_rows(nc_lab_data,ma_lab_data)
#add month column and season column
ne_radon$Month=lubridate::month(ne_radon$STARTDATE)
ne_radon$Year=lubridate::year(ne_radon$STARTDATE)

#create a month season look up table
trans<-cbind.data.frame(Month=1:12,
                        Season=c("Winter","Winter","Spring","Spring","Spring","Summer",
                                 "Summer","Summer","Autumn","Autumn","Autumn","Winter"))
ne_radon=ne_radon%>%left_join(trans)
#The winter months (Jan Feb) in the new year are merged with previous year
ne_radon$Season_Year=ifelse(ne_radon$Month<3,ne_radon$Year-1,ne_radon$Year)
#the number of records in each zipcode
zip_season<-ne_radon%>%
  group_by(Season_Year,Season,ZIPCODE)%>%
  summarise(seaon_Rn=mean(PCI.L),season_var=sd(PCI.L),
            gm_season=exp(mean(log(PCI.L))),
            hours=mean(HOURS),
            gsd_season=exp(sd(log(PCI.L))),
            n_units=n_distinct(FINGERPRINT),n_obs=length(FINGERPRINT))

zip_month<-ne_radon%>%group_by(Year,Month,ZIPCODE)%>%
  summarise(month_Rn=mean(PCI.L),month_var=sd(PCI.L),
            gm_month=exp(mean(log(PCI.L))),
            gsd_month=exp(sd(log(PCI.L))),
            hours=mean(HOURS),
            n_units=n_distinct(FINGERPRINT),n_obs=length(FINGERPRINT))

zip_year=ne_radon%>%group_by(Year,ZIPCODE)%>%
  summarise(annual_Rn=mean(PCI.L),annual_var=sd(PCI.L),
            gm_annual=exp(mean(log(PCI.L))),
            gsd_annual=exp(sd(log(PCI.L))),
            hours=mean(HOURS),
            spring_prop=sum(Season=="Spring")/length(FINGERPRINT),
            summer_prop=sum(Season=="Summer")/length(FINGERPRINT),
            autumn_prop=sum(Season=="Autumn")/length(FINGERPRINT),
            winter_prop=sum(Season=="Winter")/length(FINGERPRINT),
            n_units=n_distinct(FINGERPRINT),n_obs=length(FINGERPRINT))
save(file = here::here("Data","Medium Data","NE_Rn_Obs.RData"),ne_radon)
save(file = here::here("Data","Medium Data","NE_Season_Rn.RData"),zip_season)
save(file = here::here("Data","Medium Data","NE_Month_Rn.RData"),zip_month)
save(file = here::here("Data","Medium Data","NE_Year_Rn.RData"),zip_year)


