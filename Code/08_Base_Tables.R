library(dplyr)
library(here)
library(boot)
library(weights)

load(here::here("Data","Medium Data","Valid_Rn_Measurement.RData"))
load(here::here("Data","Medium Data","Lag_Rn.RData"))
load(here::here("Data","Medium Data","zipcode_coords.RData"))
training_data=radon_month_obs
training_data$timestamp=12*(training_data$Year-1990)+training_data$Month
training_data=training_data%>%filter(Year>2004,Year<2019)
training_data=training_data%>%filter(n_units>4)
training_data$dist2fault=as.numeric(training_data$dist2fault)
training_data$gm_month=log(training_data$gm_month)
training_data=as.data.frame(training_data)
training_data=training_data%>%left_join(zip_coord,by=c("ZIPCODE"="ZIPCODE"))
training_data=training_data%>%left_join(zipcode_rn_lag,by=c("ZIPCODE"="ZIPCODE",
                                                            "timestamp"="Timestamp"))

training_data=na.omit(training_data)

#Table 1, spatial-temporal variation of radon
wt_std=function(x,wt){
  xm <- weighted.mean(x, wt)
  v <- sum(wt * (x - xm)^2)/length(x)
  return(sqrt(v))
}
#
training_data[as.integer(substr(training_data$ZIPCODE,1,3))<28,"STATE"]="MA"

training_data[as.integer(substr(training_data$ZIPCODE,1,3))>28&as.integer(substr(training_data$ZIPCODE,1,3))<30,"STATE"]="RI"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>29&as.integer(substr(training_data$ZIPCODE,1,3))<39,"STATE"]="NH"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>38&as.integer(substr(training_data$ZIPCODE,1,3))<50,"STATE"]="ME"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>50&as.integer(substr(training_data$ZIPCODE,1,3))<60,"STATE"]="VT"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>59&as.integer(substr(training_data$ZIPCODE,1,3))<70,"STATE"]="CT"
training_data[is.na(training_data$STATE),"STATE"]="RI"

t=training_data%>%group_by(as.integer((Year-2005)/5),STATE)%>%summarise(m=weighted.mean(exp(gm_month),n_units),
                                                                        sd=wt_std(x=exp(gm_month),wt=n_units),
                                                                        n=length(gm_month))
t=cbind.data.frame(t[1:6,2:5],t[7:12,3:5],t[13:18,3:5])
#Table 2(S), correlation table between radon and key predictor
features=c("Uranium","Rn_Potential","Thorium",
           "Units_2010_2013","Units_2000_2009","Units_1960_1969","Units_1950_1959",
           "Two_Room_Unit",
           "Five_Room_Unit",
           "Two_Bedroom_Unit",
           "Four_Bedroom_Unit",
           "Gas_Fuel",
           "Oil_Fuel",
           "rhum",
           "temp",
           "soilm",
           "soilt",
           "pcp",
           "AVG_NO4",
           "AVG_NO10",
           "AVG_NO200",
           "dist2fault")
wtd.cor(training_data$gm_month,training_data[,features])
