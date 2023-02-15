#The objective of this file is to group the ZIP Code-level prediction by county & year
library(dplyr)
library(ggplot2)

setwd("/n/koutrakis_lab/lab/Radon_Mortality")
load(here::here("Data","GeoData","2015_Shapes.RData"))

##-----See the percent of basement measurement in populated area---------------------
load("Merged_Measurements_201031.RData")
zipcode_prop=lab_data%>%
  dplyr::group_by(TestPostalCode)%>%
  dplyr::summarise(ba_p=mean(Floor%in%c("Basement","basement")),
                   n=length(ID))
zipcode_prop=zipcode_prop%>%dplyr::left_join(zips@data,by=c("TestPostalCode"="ZIP"))
#It seems that even in some extremely high populated area (most people live in multi-family buildings)
#most (>80%) radon measurements were in the basement (even laundary room). As a result, population density
#is not a good indicator of the percent of radon measurements in the basement.

###------Calculate the county-level radon concentrations predicted by ourselves-------
all_files=list.files("/n/koutrakis_lab/lab/Radon_Mortality/Data/State_Year/",full.names = T)
all_predictions=list()
l=1
for( f in all_files){
  load(f)
  all_predictions[[l]]=pred_list
  l=l+1
}
all_predictions=bind_rows(all_predictions)
average_predictions=all_predictions%>%group_by(ZIPCode,Year)%>%summarise(avg_base=mean(Conc_Basement,na.rm=T),
                                                                    avg_above=mean(Conc_Above,na.rm=T))
average_predictions=average_predictions%>%dplyr::left_join(zipcode_prop,by=c("ZIPCode"= "TestPostalCode"))
#Attach a crosswalk file to know the FIPS
load("/n/koutrakis_lab/lab/Group_Data/FIPS_ZIPCODE_Crosswalk.RData")
average_predictions=average_predictions%>%left_join(FIPS_ZIPCODE_TABLE,by=c("ZIPCode"="zips"))
average_predictions=average_predictions%>%dplyr::filter(!is.na(POPULATION))

#Re-aggregate average by FIPS
county_prediction=average_predictions%>%group_by(fips,Year)%>%summarise(avg_base=weighted.mean(x=avg_base,w=POPULATION,na.rm=T),
                                                                   avg_above=weighted.mean(x=avg_above,w=POPULATION,na.rm=T))


##-------------Calculate the county-level gross beta predicted by ourselves-----------
load("/n/koutrakis_lab/lab/Beta_Prediction/zip_annual_SD.RData")
zip_PR=zip_annual
zip_PR=zip_PR%>%left_join(zipcode_prop,by=c("ZIPCODE"="TestPostalCode"))
zip_PR=zip_PR%>%filter(!is.na(POPULATION))
zip_PR=zip_PR%>%left_join(FIPS_ZIPCODE_TABLE,by=c("ZIPCODE"="zips"))
county_pr=zip_PR%>%group_by(fips,Year)%>%summarise(avg_pr=weighted.mean(x=PR,w=POPULATION,na.rm=T))


##-------------Calculate the county-level air pollutants predicted by JDS---------------
pm_files=list.files("/n/koutrakis_lab/lab/Group_Data/ZIPCode_PM25_O3_NO2_JDS/annual_pm25",full.names = T)
pm_pred=list()
for(f in 1:length(pm_files)){
  temp=readRDS(pm_files[f])
  temp$year=1999+f
  pm_pred[[f]]=temp
}
pm_pred=bind_rows(pm_pred)
pm_pred=pm_pred%>%left_join(zipcode_prop,by=c("ZIP"="TestPostalCode"))
pm_pred=pm_pred%>%left_join(FIPS_ZIPCODE_TABLE,by=c("ZIP"="zips"))
pm_pred=pm_pred%>%dplyr::filter(!is.na(POPULATION))
county_pm=pm_pred%>%group_by(fips,year)%>%summarise(avg_pm=weighted.mean(x=pm25,w=POPULATION,na.rm=T))

no2_files=list.files("/n/koutrakis_lab/lab/Group_Data/ZIPCode_PM25_O3_NO2_JDS/annual_no2",full.names = T)
no2_pred=list()
for(f in 1:length(pm_files)){
  temp=readRDS(no2_files[f])
  temp$year=1999+f
  no2_pred[[f]]=temp
}
no2_pred=bind_rows(no2_pred)
no2_pred=no2_pred%>%left_join(zipcode_prop,by=c("ZIP"="TestPostalCode"))
no2_pred=no2_pred%>%left_join(FIPS_ZIPCODE_TABLE,by=c("ZIP"="zips"))
no2_pred=no2_pred%>%dplyr::filter(!is.na(POPULATION))
county_no2=no2_pred%>%group_by(fips,year)%>%summarise(avg_no2=weighted.mean(x=no2,w=POPULATION,na.rm=T))

o3_files=list.files("/n/koutrakis_lab/lab/Group_Data/ZIPCode_PM25_O3_NO2_JDS/annual_o3",full.names = T)
o3_pred=list()
for(f in 1:length(pm_files)){
  temp=readRDS(o3_files[f])
  temp$year=1999+f
  o3_pred[[f]]=temp
}
o3_pred=bind_rows(o3_pred)
o3_pred=o3_pred%>%left_join(zipcode_prop,by=c("ZIP"="TestPostalCode"))
o3_pred=o3_pred%>%left_join(FIPS_ZIPCODE_TABLE,by=c("ZIP"="zips"))
o3_pred=o3_pred%>%dplyr::filter(!is.na(POPULATION))
county_o3=o3_pred%>%group_by(fips,year)%>%summarise(avg_o3=weighted.mean(x=ozone,w=POPULATION,na.rm=T))
##----------Merge Berkeley Model Radon--------------
LBNL_prediction=read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/BerkeleyClean.csv")
LBNL_prediction$fips=formatC(LBNL_prediction$fips,width = 5,flag=0)
county_prediction=county_prediction%>%left_join(LBNL_prediction%>%dplyr::select(fips,GMest,state,area.name),
                                                by=c("fips"))


##----------Merge data together----------------------
county_prediction=county_prediction%>%
  left_join(county_pr,by=c("fips"="fips","Year"="Year"))%>%
  left_join(county_pm,by=c("fips"="fips","Year"="year"))%>%
  left_join(county_no2,by=c("fips"="fips","Year"="year"))%>%
  left_join(county_o3,by=c("fips"="fips","Year"="year"))
county_prediction=county_prediction%>%dplyr::filter(!is.na(GMest))
names(county_prediction)[5]="LBNL_Pred"
county_prediction=county_prediction%>%dplyr::select(fips,area.name,state,Year,
                                                    avg_base,avg_above,avg_pr,avg_pm,avg_no2,avg_o3)
names(county_prediction)=c("FIPS","County_Name","State","Year","Basement_Rn","Aboveground_Rn","Gross_Beta","PM25","NO2","O3")
write.csv(county_prediction,file="County_Level_Radon_PR_NE&MW.csv")

county_pred=county_pr%>%
  left_join(county_pm,by=c("fips"="fips","Year"="year"))%>%
  left_join(county_no2,by=c("fips"="fips","Year"="year"))%>%
  left_join(county_o3,by=c("fips"="fips","Year"="year"))
county_pred=county_pred%>%left_join(LBNL_prediction%>%dplyr::select(fips,state,area.name),
                                    by="fips")
names(county_pred)=c("FIPS","Year","Beta","PM","NO2","O3","State","Name")
write.csv(county_pred,file="County_Level_PR_AP_US.csv")

