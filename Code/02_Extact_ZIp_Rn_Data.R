library(here)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(raster)

radon_data<-read.csv(here::here("Data","harvard.csv"),header=T,sep = "\t")
load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","GeoData","Boundaries.RData"))

#clean the date data
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

ne_radon=radon_data%>%filter(STATE%in%c("MA","NH","CT","ME","VT","RI"))
#only keep the records of basement
ne_radon<-ne_radon%>%filter(FLOOR%in%c("basement","unknown"))
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
ne_radon=ne_radon%>%filter%>%filter(lubridate::year(STARTDATE)>1995)
#the number of records in each zipcode
zip_month<-ne_radon%>%
  group_by(year=lubridate::year(STARTDATE),month=lubridate::month(STARTDATE),ZIPCODE)%>%
  summarise(month_Rn=mean(PCI.L),month_var=sd(PCI.L),n_obs=n_distinct(FINGERPRINT))

zip_annual<-ne_radon%>%
  group_by(year=lubridate::year(STARTDATE),ZIPCODE)%>%
  summarise(annual_Rn=mean(PCI.L),annual_var=sd(PCI.L),
            n_winter=sum(month(STARTDATE)<4)+sum(month(STARTDATE)>11),
            n_spring=sum(month(STARTDATE)>3&month(STARTDATE)<7),
            n_summer=sum(month(STARTDATE)>6&month(STARTDATE)<10),
            n_autumn=sum(month(STARTDATE)>9&month(STARTDATE)<12),
            n_obs=n_distinct(FINGERPRINT))


# a total of 594 zipcodes have at least one month with over 5 radon measurements
# zipcode 02879 located in RI has 149 months, windham has 7 months.
#zip_summ<-zip_month%>%filter(n>4)%>%group_by(ZIPCODE)%>%count()%>%arrange(desc(n))

#finally, we have 2908 monthly measuements
zip_month_Rn=zip_month%>%filter(n_obs>2)%>%arrange(year,desc(month))
save(file=here::here("Data","Medium Data","Monthly_Rn.RData"),zip_month_Rn)
save(file = here::here("Data","Medium Data","Annual_Rn.RData"),zip_annual)
save(file = here::here("Data","Medium Data","NE_Rn.RData"),ne_radon)

