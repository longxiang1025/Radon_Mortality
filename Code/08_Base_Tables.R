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
training_data=training_data%>%filter(n_units>9)
training_data$dist2fault=as.numeric(training_data$dist2fault)
training_data$gm_month=log(training_data$gm_month)
training_data=as.data.frame(training_data)
training_data=training_data%>%left_join(zip_coord,by=c("ZIPCODE"="ZIPCODE"))
training_data=training_data%>%left_join(zipcode_rn_lag,by=c("ZIPCODE"="ZIPCODE",
                                                            "timestamp"="Timestamp"))
training_data[as.integer(substr(training_data$ZIPCODE,1,3))<28,"STATE"]="MA"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>28&as.integer(substr(training_data$ZIPCODE,1,3))<30,"STATE"]="RI"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>29&as.integer(substr(training_data$ZIPCODE,1,3))<39,"STATE"]="NH"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>38&as.integer(substr(training_data$ZIPCODE,1,3))<50,"STATE"]="ME"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>50&as.integer(substr(training_data$ZIPCODE,1,3))<60,"STATE"]="VT"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>59&as.integer(substr(training_data$ZIPCODE,1,3))<70,"STATE"]="CT"
training_data[is.na(training_data$STATE),"STATE"]="RI"
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


trans<-cbind.data.frame(Month=1:12,
                        Season=c("Winter","Winter","Spring","Spring","Spring","Summer",
                                 "Summer","Summer","Autumn","Autumn","Autumn","Winter"))
training_data=training_data%>%left_join(trans)


t=training_data%>%group_by(Year,Season)%>%summarise(m=weighted.mean(37*exp(gm_month),n_units),
                                                                         sd=sd(x=37*exp(gm_month)),
                                                                         n=length(gm_month))
ggplot(data=t)+
  geom_path(data=annual_season[annual_season$Season=="Spring",],aes(x=Year+0.15,y=m,color=Season),size=0.5)+
  geom_smooth(data=annual_season[annual_season$Season=="Spring",],aes(x=Year,y=m,color=Season),
              method = "gam",formula = y ~ poly(x-2004, 4),se=F)+
  geom_errorbar(data=annual_season[annual_season$Season=="Spring",],aes(x=Year+0.15,ymin=m-1.96*sd,ymax=m+1.96*sd,color=Season),size=0.5,width=0.25)+
  geom_point(data=annual_season[annual_season$Season=="Spring",],aes(x=Year+0.15,y=m,color=Season),size=2.5)+
  geom_path(data=annual_season[annual_season$Season=="Summer",],aes(x=Year,y=m,color=Season),size=0.5)+
  geom_smooth(data=annual_season[annual_season$Season=="Summer",],aes(x=Year,y=m,color=Season),
              method = "gam",formula = y ~ poly(x-2004, 4),se=F)+
  geom_errorbar(data=annual_season[annual_season$Season=="Summer",],aes(x=Year,ymin=m-1.96*sd,ymax=m+1.96*sd,color=Season),size=0.5,width=0.25)+
  geom_point(data=annual_season[annual_season$Season=="Summer",],aes(x=Year,y=m,color=Season),size=2.5)+
  geom_path(data=annual_season[annual_season$Season=="Autumn",],aes(x=Year+0.3,y=m,color=Season),size=0.5)+
  geom_smooth(data=annual_season[annual_season$Season=="Autumn",],aes(x=Year,y=m,color=Season),
              method = "gam",formula = y ~ poly(x-2004, 4),se=F)+
  geom_errorbar(data=annual_season[annual_season$Season=="Autumn",],aes(x=Year+0.3,ymin=m-1.96*sd,ymax=m+1.96*sd,color=Season),size=0.5,width=0.25)+
  geom_point(data=annual_season[annual_season$Season=="Autumn",],aes(x=Year+0.3,y=m,color=Season),size=2.5)+
  geom_path(data=annual_season[annual_season$Season=="Winter",],aes(x=Year+0.45,y=m,color=Season),size=0.5)+
  geom_smooth(data=annual_season[annual_season$Season=="Winter",],aes(x=Year,y=m,color=Season),
              method = "gam",formula = y ~ poly(x-2004, 4),se=F)+
  geom_errorbar(data=annual_season[annual_season$Season=="Winter",],aes(x=Year+0.45,ymin=m-1.96*sd,ymax=m+1.96*sd,color=Season),size=0.5,width=0.25)+
  geom_point(data=annual_season[annual_season$Season=="Winter",],aes(x=Year+0.45,y=m,color=Season),size=2.5)+
  scale_color_manual(breaks=c("Spring","Summer","Autumn","Winter"),
                     values = c("#00FF7F","#00755E","#FFCC33","#00BFFF"))+
  xlab("Year")+
  ylab("Average Radon Concentrations (Bq/m3)")+
  theme_bw()

library(MASS)
library(weights)
load(here::here("Data","Medium Data","Final_Model_Performance.RData"))
load(here::here("Data","GeoData","FIPS_ZIPCODE_Crosswalk.RData"))
m_preds$ZIPCODE=training_data$ZIPCODE
m_preds$Year=training_data$Year
m_preds$Month=training_data$Month
m_preds=m_preds%>%left_join(FIPS_ZIPCODE_TABLE,by=c("ZIPCODE"="zips"))

trans<-cbind.data.frame(Month=1:12,
                        Season=c("Winter","Winter","Spring","Spring","Spring","Summer",
                                 "Summer","Summer","Autumn","Autumn","Autumn","Winter"))
m_preds=m_preds%>%left_join(trans)
m_preds=m_preds%>%arrange((weights))

m_preds[,c(1:53,58:65)]=37*exp(m_preds[,c(1:53,58:65)])
model_perf=list()
for(m in 1:13){
 #weighted RMSE
 c=sqrt(wtd.var(m_preds[,paste0("M",m,"_R1_CV_Pred")]-m_preds$obs,weights = m_preds$weights)) 
 #weighted CV R2
 cr=corr(m_preds[,c(paste0("M",m,"_R1_CV_Pred"),"obs")],w=m_preds$weights)
 #weighted R2
 r=corr(m_preds[,c(paste0("M",m,"_Pred"),"obs")],w=m_preds$weights)
 model_perf[[m]]=cbind(c,cr,r,m)
}
model_perf=do.call(rbind,model_perf)
model_perf=as.data.frame(model_perf)
names(model_perf)=c("RMSE","CV_R2","R2","ID")

c=sqrt(wtd.var(m_preds$Ens_Pred-m_preds$obs,weights = m_preds$weights))
cr=corr(m_preds[,c(paste0("Ens_CV_Pred"),"obs")],w=m_preds$weights)
r=corr(m_preds[,c("Ens_Pred","obs")],w=m_preds$weights)

model_perf[14,]=cbind(c,cr,r,"Ens")

#This table for the patterns in prediction accuracy
temp=m_preds%>%group_by(fips,Season)%>%summarise(
  c=sqrt(wtd.var(Ens_CV_Pred-obs,weights =weights)),
  n=length(ZIPCODE)
)
counties=m_preds%>%group_by(fips)%>%summarise(n=length(fips))

temp=temp[temp$n>2,]
temp_spring=temp[temp$Season=="Spring",c("fips","c")]
names(temp_spring)=c("fips","spring")
temp_summer=temp[temp$Season=="Summer",c("fips","c")]
names(temp_summer)=c("fips","summer")
temp_autumn=temp[temp$Season=="Autumn",c("fips","c")]
names(temp_autumn)=c("fips","autumn")
temp_winter=temp[temp$Season=="Winter",c("fips","c")]
names(temp_winter)=c("fips","winter")

temp=counties%>%left_join(temp_spring)%>%
  left_join(temp_summer)%>%
  left_join(temp_autumn)%>%
  left_join(temp_winter)

write.csv(temp,file="season_accuracy.csv")

#this table for the patterns in observations
temp=m_preds%>%group_by(fips,Season)%>%summarise(
  m=mean(obs),
  v=sd(obs),
  n=length(ZIPCODE)
)
counties=m_preds%>%group_by(fips)%>%summarise(n=length(fips))

temp=temp[temp$n>2,]
temp_spring=temp[temp$Season=="Spring",c("fips","m","v","n")]
names(temp_spring)=c("fips","spring","sp_v","sp_n")
temp_summer=temp[temp$Season=="Summer",c("fips","m","v","n")]
names(temp_summer)=c("fips","summer","sm_v","sm_n")
temp_autumn=temp[temp$Season=="Autumn",c("fips","m","v","n")]
names(temp_autumn)=c("fips","autumn","at_v","at_n")
temp_winter=temp[temp$Season=="Winter",c("fips","m","v","n")]
names(temp_winter)=c("fips","winter","wt_v","wt_n")

temp=counties%>%left_join(temp_spring)%>%
  left_join(temp_summer)%>%
  left_join(temp_autumn)%>%
  left_join(temp_winter)

write.csv(temp,file="summary.csv")

#Total variaton
zip_var


