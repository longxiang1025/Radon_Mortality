library(here)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(raster)

radon_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/harvard.csv",header=T,sep = "\t")
ma_lab_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_MA_190628.csv",header=T)
ma_lab_data$TestPostalCode=as.character(ma_lab_data$TestPostalCode)
pa_lab_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_PA_190628.csv",header=T)
pa_lab_data$TestPostalCode<-as.character(pa_lab_data$TestPostalCode)
radon_data_sec<-bind_rows(ma_lab_data,pa_lab_data)

load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","GeoData","Boundaries.RData"))

#clean the date data of NC lab
radon_data$STARTDATE=as.character(radon_data$STARTDATE)
radon_data$ENDDATE=as.character(radon_data$ENDDATE)

radon_data<-radon_data%>%mutate(STARTDATE=ifelse(STARTDATE=="00/00/0000",NA,STARTDATE))
radon_data<-radon_data%>%filter(!is.na(STARTDATE))
radon_data$STARTDATE<-as.Date(radon_data$STARTDATE,"%m/%d/%Y")

radon_data<-radon_data%>%mutate(ENDDATE=ifelse(ENDDATE=="00/00/0000",NA,ENDDATE))
radon_data<-radon_data%>%filter(!is.na(ENDDATE))
radon_data$ENDDATE<-as.Date(radon_data$ENDDATE,"%m/%d/%Y")

radon_data$ZIPCODE=substr(radon_data$POSTALCODE,1,5)
radon_data$PCI.L=as.numeric(as.character(radon_data$PCI.L))
radon_data=radon_data%>%filter(PCI.L<200)
radon_data=radon_data%>%filter(PCI.L>0.04)
radon_data$HOURS=as.numeric(as.character(radon_data$HOURS))
radon_data=radon_data%>%filter(HOURS>24,HOURS<200)

##clean the date of MA and PA labs
radon_data_sec$StartDate=as.character(radon_data_sec$StartDate)
radon_data_sec$EndDate=as.character(radon_data_sec$EndDate)

radon_data_sec<-radon_data_sec%>%mutate(StartDate=ifelse(StartDate=="00/00/0000",NA,StartDate))
radon_data_sec<-radon_data_sec%>%filter(!is.na(StartDate))
radon_data_sec$StartDate<-as.Date(radon_data_sec$StartDate,"%m/%d/%Y")

radon_data_sec<-radon_data_sec%>%mutate(EndDate=ifelse(EndDate=="00/00/0000",NA,EndDate))
radon_data_sec<-radon_data_sec%>%filter(!is.na(EndDate))
radon_data_sec$EndDate<-as.Date(radon_data_sec$EndDate,"%m/%d/%Y")

radon_data_sec$PCI.L=as.numeric(as.character(radon_data_sec$Result))
radon_data_sec[is.na(radon_data_sec$PCI.L),"PCI.L"]=0
radon_data_sec=radon_data_sec%>%filter(PCI.L<200)
radon_data_sec=radon_data_sec%>%filter(PCI.L>0.04)

ne_radon=radon_data%>%filter(STATE%in%c("MA","NH","CT","ME","VT","RI"))
ne_radon_sec=radon_data_sec%>%filter(TestState%in%c("MA","NH","CT","ME","VT","RI"))
#only keep the records of basement
ne_radon<-ne_radon%>%filter(FLOOR%in%c("basement","unknown"))
ne_radon_sec<-ne_radon_sec%>%filter(Floor%in%c("Basement","Unknown"))
#remove all records of "AFTERREPARIS" and the "FOLLOWUP" after that because it's always lower and not randomly sampled
ne_repair=ne_radon%>%group_by(FINGERPRINT)%>%summarise(repair_status=ifelse("AFTERREPAIRS"%in%TESTTYPE,TRUE,FALSE))
repair_date=ne_radon%>%
  right_join(ne_repair%>%filter(repair_status))%>%
  filter(TESTTYPE=="AFTERREPAIRS")%>%
  group_by(FINGERPRINT)%>%
  dplyr::select(FINGERPRINT,ENDDATE)%>%
  summarise(Repair_Date=min(ENDDATE))

names(repair_date)=c("FINGERPRINT","Repair_Date")
ne_radon=ne_radon%>%left_join(repair_date)

repaired=ne_radon%>%filter(!is.na(Repair_Date))
un_repaired=ne_radon%>%filter(is.na(Repair_Date))
#in the repaired home, remove all records after repairment except real property check
repaired=repaired%>%filter(!(ENDDATE>=Repair_Date & TESTTYPE%in%c("AFTERREPAIRS","FOLLOWUP")))
ne_radon=bind_rows(repaired,un_repaired)
ne_radon=ne_radon%>%filter%>%filter(lubridate::year(STARTDATE)>1990)

#In MA&PA lab, no test type is provided, just select the first wave of measurements
ne_radon_sec_date=ne_radon_sec%>%group_by(Checksum_TestAddress)%>%summarise(StartDate=min(StartDate))
ne_radon_sec=ne_radon_sec_date%>%left_join(ne_radon_sec)
ne_radon_sec=ne_radon_sec%>%filter%>%filter(lubridate::year(StartDate)>1990)
ne_radon_sec=ne_radon_sec%>%filter(Hours>24,Hours<200)

ne_radon$Month=lubridate::month(ne_radon$STARTDATE)
ne_radon_sec$Month=lubridate::month(ne_radon_sec$StartDate)

#Change the name of ne_radon_sec to the names of ne_radon
ne_radon=ne_radon[,c("STARTDATE","ENDDATE","ZIPCODE","STATE","COUNTY","CITY","LAB","Month","PCI.L","FINGERPRINT","HOURS")]
ne_radon_sec=ne_radon_sec[,c("StartDate","EndDate","TestPostalCode","TestState","County","TestCity","Lab","Month","PCI.L","Checksum_TestAddress","Hours")]
ne_radon_sec$Checksum_TestAddress=as.character(ne_radon_sec$Checksum_TestAddress)
names(ne_radon_sec)=names(ne_radon)

ne_radon=bind_rows(ne_radon,ne_radon_sec)
#create a month season look up table
trans<-cbind.data.frame(Month=1:12,
                        Season=c("Winter","Winter","Spring","Spring","Spring","Summer",
                                 "Summer","Summer","Autumn","Autumn","Autumn","Winter"))
ne_radon=ne_radon%>%left_join(trans)
ne_radon$Year=lubridate::year(ne_radon$STARTDATE)
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

# a total of 594 zipcodes have at least one month with over 5 radon measurements
# zipcode 02879 located in RI has 149 months, windham has 7 months.
#zip_summ<-zip_month%>%filter(n>4)%>%group_by(ZIPCODE)%>%count()%>%arrange(desc(n))

save(file = here::here("Data","Medium Data","NE_Rn_Obs.RData"),ne_radon)
save(file = here::here("Data","Medium Data","NE_Season_Rn.RData"),zip_season)
save(file = here::here("Data","Medium Data","NE_Month_Rn.RData"),zip_month)
save(file = here::here("Data","Medium Data","NE_Year_Rn.RData"),zip_year)


