#The objective of this script is to generate the training dataset for national radon model#
#To help the model learn the variation of ZCTA-level radon related to types of detector or measurement
#floor, we can generate multiple stimulative ZCTA-level measurements based on one ZCTA-level measurement
#consisting of N samples
setwd("/n/koutrakis_lab/lab/Radon_Mortality/")
library(dplyr)
library(lubridate)
library(sf)
library(hexbin)
library(ggplot2)
library(cowplot)
library(EnvStats)
options(dplyr.summarise.inform = FALSE)
#A Function to create counterfactual ZCTA-level observations
sub_observations=function(data,n){
  #data:all measurements the ZCTA/month/year of interest (borrow from parent function)
  #n: The number of rows in a target sub-sample
  
  N=nrow(data)

  sub_measurements=sample(1:N,size = n,replace = T)
  sub_measurements=data[sub_measurements,]
  
  sub_observation=sub_measurements%>%group_by()%>%
    summarise(GMean=EnvStats::geoMean(Conc,na.rm = T),
              MMean=mean(Conc,na.rm=T),
              N=length(Conc),
              per_Basement=mean(Floor=="Basement",na.rm=T),
              per_AirChek=mean(Method=="AirChek",na.rm=T),
              per_LS=mean(Method=="LS",na.rm=T),
              per_AC=mean(Method=="AC",na.rm=T),
              per_Alpha=mean(Method=="Alpha_AC",na.rm=T))
  sub_observation$TestPostalCode=data$TestPostalCode[1]
  sub_observation$month=lubridate::month(data$StartDate)[1]
  sub_observation$year=lubridate::year(data$StartDate)[1]
  sub_observation$State=data$TestState[1]
  return(sub_observation)
}

generate_subsample=function(ZCTA,m,y,n,ratio=2){
  ZCTA_measurements=coloc_data[coloc_data$TestPostalCode==paste0(ZCTA),]
  ZCTA_measurements=ZCTA_measurements[(month(ZCTA_measurements$StartDate))==as.integer(m)&
                                      (year(ZCTA_measurements$StartDate))==as.integer(y),]
  n=nrow(ZCTA_measurements)
  size_list=list()
  l=1
  while(n>2){
    n=as.integer((n+1)/2)
    size_list[[l]]=rep(n,ratio^l)
    l=l+1
  }
  size_list=unlist(size_list)
  
  observation_list=list()
  for(i in 1:length(size_list)){
    observation_list[[i]]=
      sub_observations(data=ZCTA_measurements,n=size_list[i])
  }
  observation_list=bind_rows(observation_list)
  return(observation_list)
}

#First, load the filtered measurements
load(file="Cleaned_Raw_Data_230307.RData")
coloc_data=coloc_data[!is.na(coloc_data$TestPostalCode),]
#Calculate all possible ZCTA-level observations
radon_training_data=coloc_data%>%
  group_by(TestPostalCode,month=lubridate::month(StartDate),year=lubridate::year(StartDate),State=TestState)%>%
  summarise(GMean=EnvStats::geoMean(Conc,na.rm = T),
            MMean=mean(Conc,na.rm=T),
            N=length(Conc),
            per_Basement=mean(Floor=="Basement",na.rm=T),
            per_AirChek=mean(Method=="AirChek",na.rm=T),
            per_LS=mean(Method=="LS",na.rm=T),
            per_AC=mean(Method=="AC",na.rm=T),
            per_Alpha=mean(Method=="Alpha_AC",na.rm=T))

#For all ZCTA-level observations based on N (N>=4) measurements, they can be used to generate
#several counterfactual ZCTA-level observations based on n (n=N/2) measurements.
non_dividable_training_data=radon_training_data%>%filter(N<4)
dividable_training_data=radon_training_data%>%filter(N>3)

sub_training_data=list()
for( s in 1:nrow(dividable_training_data)){
  if(dividable_training_data[s,"TestPostalCode"]>20){
    ratio=2
  }else{
    ratio=3
  }
  sub_training_data[[s]]=generate_subsample(ZCTA=dividable_training_data[s,"TestPostalCode"],
                                            m=dividable_training_data[s,"month"],
                                            y=dividable_training_data[s,"year"],
                                            ratio=ratio)
  if(s%%1000==0){
    print(paste(s,Sys.time()))
  }
  if(s%%10000==0){
    save(file=paste0("Temp_1_",s,".RData"),sub_training_data)
    unlink(paste0("Temp_1_",s-10000,".RData"))
  }
}
sub_training_data=bind_rows(sub_training_data)
save(file="All_Dividable_Sub_Observations.RData",sub_training_data)

#Merge all training ZCTA-level observation together
load("All_Dividable_Sub_Observations.RData")
radon_training_data$Obs_Type="All"
sub_training_data$Obs_Type="Sub"
all_training_data=bind_rows(radon_training_data,sub_training_data)
save(file="National_Training_Data_Without_Predictors.RData",all_training_data)
