library(dplyr)
library(tidyr)
library(here)
library(lubridate)
library(sf)
library(boot)
library(caret)

load(here::here("Data","Medium Data","Monthly_Mete.RData"))
load(here::here("Data","Medium Data","Rn_Geology.RData"))
load(here::here("Data","Medium Data","ZIP_Housing.RData"))
load(here::here("Data","Medium Data","NE_Year_Rn.RData"))
load(here::here("Data","Medium Data","NE_Month_Rn.RData"))
load(here::here("Data","Medium Data","NE_Season_Rn.RData"))


zip_month_rn=zip_month
zip_year_rn=zip_year
zip_season_rn=zip_season

zip_mete_record=zip_mete_record%>%mutate(Season= cut(x=month,breaks=c(0,2,6,9,11,12),labels=c("Winter","Spring","Summer","Autumn","Winter")))
zip_mete_record$Season_Year=ifelse(zip_mete_record$month<3,zip_mete_record$year-1,zip_mete_record$year)
zip_season_mete=zip_mete_record%>%group_by(ZIP,Season_Year,Season)%>%summarise(
   uwnd=mean(uwnd),
   vwnd=mean(vwnd),
   temp=mean(temp),
   albedo=mean(albedo),
   hpbl=mean(hpbl),
   rhum=mean(rhum),
   snowc=mean(snowc),
   soilm=mean(soilm),
   pcp=mean(pcp),
   soilt=mean(soilt),
   pres=mean(pres)
 )
zip_year_mete=zip_mete_record%>%group_by(ZIP,year)%>%summarise(
  uwnd=mean(uwnd),
  vwnd=mean(vwnd),
  temp=mean(temp),
  albedo=mean(albedo),
  hpbl=mean(hpbl),
  rhum=mean(rhum),
  snowc=mean(snowc),
  soilm=mean(soilm),
  pcp=mean(pcp),
  soilt=mean(soilt),
  pres=mean(pres)
)
zip_month_mete=zip_mete_record%>%group_by(ZIP,year,month)%>%summarise(
  uwnd=mean(uwnd),
  vwnd=mean(vwnd),
  temp=mean(temp),
  albedo=mean(albedo),
  hpbl=mean(hpbl),
  rhum=mean(rhum),
  snowc=mean(snowc),
  soilm=mean(soilm),
  pcp=mean(pcp),
  soilt=mean(soilt),
  pres=mean(pres)
)

radon_year_obs=zip_year_rn%>%
  left_join(zip_geo,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zips_house,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zip_year_mete,by=c("Year"="year",
                                 "ZIPCODE"="ZIP"))

radon_season_obs=zip_season_rn%>%
  left_join(zip_geo,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zips_house,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zip_season_mete,by=c("Season_Year"="Season_Year",
                                 "Season"="Season",
                                 "ZIPCODE"="ZIP"))

radon_month_obs=zip_month_rn%>%
  left_join(zip_geo,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zips_house,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zip_month_mete,by=c("Year"="year",
                                 "Month"="month",
                                 "ZIPCODE"="ZIP"))

save(file=here::here("Data","Medium Data","Valid_Rn_Measurement.RData"),radon_year_obs,radon_month_obs,radon_season_obs)
