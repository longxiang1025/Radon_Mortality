library(dplyr)
library(here)
library(ggplot2)
library(purrr)

FIPS_List=c("25001","25005","25009","25017","25021",
            "25023","25027","33017","33015",
            "33013","33011","33001","44003","44005","44007")

load(here::here("Data","Medium Data","zipcode_mortality_NE_2nd.RData"))
load(here::here("Data","Medium Data","Pred_Rn_Data2.RData"))
load(here::here("Data","Medium Data","Monthly_Mete.RData"))
load(here::here("Data","Medium Data","zipcode_admission_NE_2nd.RData"))
#load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","Env_Exp.RData"))
#zips_sf<-st_as_sf(zips)
#Out of 10127 zipcodes, only 1492 zipcodes have predicted Rn Data
#pred_data<-pred_data%>%filter(POP_SQMI<8000)
pred_data<-radon_pred%>%group_by(Year,ZIPCODE)%>%summarise(Rn=mean(G_Radon))
pred_data$Year=as.numeric(as.character(pred_data$Year))
#pred_data$Year=1996+pred_data$Year
pred_data=pred_data%>%left_join(env_exp%>%dplyr::select(year,ZIP,pm25,education,poverty,popdensity,medhouseholdincome,medianhousevalue,pct_owner_occ),by=c("Year"="year","ZIPCODE"="ZIP"))
pred_data=pred_data%>%filter(!is.na(pm25))
zip_list=unique(pred_data$ZIPCODE)
#ggplot()+
#  geom_sf(data=zips_sf%>%filter(STATE%in%c("NH","MA","RI","CT","VT","ME")))+
#  geom_sf(data=zips_sf%>%filter(ZIP%in%zip_list),fill="Blue")
#length(unique(mortality_data$zipcode))
mete_data=zip_mete_record%>%group_by(ZIP,year)%>%summarise(s_temp=max(temp),w_temp=min(temp))
mete_data$s_temp=mete_data$s_temp-273.15
mete_data$w_temp=mete_data$w_temp-273.15

#mortality_data=mortality_data%>%filter(substr(FIPS,1,2)=="25")
mortality_data=mortality_data%>%filter(zipcode%in%zip_list)
mortality_data=mortality_data%>%left_join(admission_ne)
mortality_data=mortality_data%>%filter(n>200)
mortality_data=mortality_data%>%mutate(mort=death/n)

mortality_data=mortality_data%>%left_join(mete_data,by=c("zipcode"="ZIP","year"="year"))
#This is the crude model without 
data=mortality_data%>%left_join(pred_data,by=c("year"="Year","zipcode"="ZIPCODE"))
#data=data%>%left_join(env_exp)
data=data%>%filter(!is.na(Rn))
library(mgcv)
library(lme4)
library(betareg)
library(metafor)
data$year=as.factor(data$year)
data$zipcode=as.factor(data$zipcode)
data$FIPS=as.factor(data$FIPS)
data=data%>%filter(!is.na(Rn))

data$log_rn=log2(data$Rn)

temp_data=data%>%filter(n>200)
FIPS_list=temp_data%>%group_by(FIPS)%>%summarise(n=n_distinct(zipcode),n_year=n_distinct(year))
FIPS_list=FIPS_list%>%filter(n>5,n_year>2)

data=data[data$FIPS%in%FIPS_List,]
data=data[data$popdensity<8000,]

data$medhouseholdincome=(data$medhouseholdincome-mean(data$medhouseholdincome))/sd(data$medhouseholdincome)
data$medianhousevalue=(data$medianhousevalue-mean(data$medianhousevalue))/sd(data$medianhousevalue)

m=glmer(cbind(death,n)~Rn+pm25+age+female+white+medicaid+w_temp+s_temp+education+poverty+medhouseholdincome+year+(1|zipcode),
      family="binomial",data=data)
summary(m)

full_test=data%>%filter(as.numeric(as.character(year))<2016,
                        as.numeric(as.character(year))>2004)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death,n)~Rn+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death,n)~Rn+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(n))
full_meta=rma(yi=est,sei=sd,weights = n,data=full_test)
full_meta
#check for parallel trend assumption
full_test=data%>%filter(FIPS%in%FIPS_list$FIPS,
                        as.numeric(as.character(year))<2016,
                        as.numeric(as.character(year))>1999)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death,n)~Rn_Adj+pct_owner_occ+pm25+age+female+white+medicaid+w_temp+s_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death,n)~Rn_Adj+pct_owner_occ+pm25+age+female+white+medicaid+w_temp+s_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=n_distinct(zipcode))
full_meta=rma(yi=est,sei=sd,weights = n*n,data=full_test)
full_meta

#common shock assumption test
temp_data=data%>%filter(n>200,Rn_Adj<4)
FIPS_list=temp_data%>%group_by(FIPS)%>%summarise(n=n_distinct(zipcode),n_year=n_distinct(year))
FIPS_list=FIPS_list%>%filter(n>5,n_year>2)

common_test=temp_data%>%filter(FIPS%in%FIPS_list$FIPS,
                               as.numeric(as.character(year))<2016,
                               as.numeric(as.character(year))>1999)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death,n)~Rn_Adj+pm25+age+female+white+medicaid+w_temp+s_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death,n)~Rn_Adj+pm25+age+female+white+medicaid+w_temp+s_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=n_distinct(zipcode))
common_test=rma(yi=est,sei=sd,weights = n*n,data=common_test)
common_test

#check for potential interaction
full_test=data%>%filter(FIPS%in%FIPS_list$FIPS,
                        as.numeric(as.character(year))<2016,
                        as.numeric(as.character(year))>1999)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death,n)~Rn_Adj+pm25+I(Rn_Adj*pm25)+pct_owner_occ+age+female+white+medicaid+w_temp+s_temp+year+zipcode,
                                          family="binomial"))[4],
                             sd=sqrt(vcov(glm(cbind(death,n)~Rn_Adj+pm25+I(Rn_Adj*pm25)+pct_owner_occ+age+female+white+medicaid+w_temp+s_temp+year+zipcode,
                                              family="binomial"))[4,4]),
                             n=n_distinct(zipcode))
full_meta=rma(yi=est,sei=sd,weights = n*n,data=full_test)
full_meta
# for(l in seq(2,4,0.1)){
#   temp_data=data%>%filter(n>200,Rn_Adj<(l+2),Rn_Adj>l)
#   FIPS_list=temp_data%>%group_by(FIPS)%>%summarise(n=n_distinct(zipcode),n_year=n_distinct(year))
#   FIPS_list=FIPS_list%>%filter(n>5,n_year>2)
#   
#   common_test=temp_data%>%filter(FIPS%in%FIPS_list$FIPS,
#                                  as.numeric(as.character(year))<2016,
#                                  as.numeric(as.character(year))>1999)%>%
#     group_by(FIPS)%>%summarise(est=coef(glm(cbind(death,n)~log_rn+pm25+age+female+white+medicaid+w_temp+s_temp+year+zipcode,
#                                             family="binomial"))[2],
#                                sd=sqrt(vcov(glm(cbind(death,n)~log_rn+pm25+age+female+white+medicaid+w_temp+s_temp+year+zipcode,
#                                                 family="binomial"))[2,2]),
#                                n=sum(n))
#   common_test=rma(yi=est,sei=sd,weights = n,data=common_test)
#   common_test
#   print(paste(l,common_test$beta,common_test$ci.lb,common_test$ci.ub))
# }
  

full_no_race_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death,n)~log_rn+pm25+age+female+medicaid+w_temp+s_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death,n)~log_rn+pm25+age+female+medicaid+w_temp+s_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(n))
full_no_race_meta=rma(yi=est,sei=sd,weights = n,data=full_no_race_test)
full_no_race_meta

full_no_gender_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death,n)~log_rn+pm25+age+white+medicaid+w_temp+s_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death,n)~log_rn+pm25+age+white+medicaid+w_temp+s_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(n))
full_no_gender_meta=rma(yi=est,sei=sd,weights = n,data=full_no_gender_test)
full_no_gender_meta

full_no_med_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death,n)~log_rn+pm25+age+white+female+w_temp+s_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death,n)~log_rn+pm25+age+white+female+w_temp+s_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(n))
full_no_med_meta=rma(yi=est,sei=sd,weights = n,data=full_no_med_test)
full_no_med_meta

full_no_age_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death,n)~log_rn+pm25+medicaid+white+female+w_temp+s_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death,n)~log_rn+pm25+medicaid+white+female+w_temp+s_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(n))
full_no_age_meta=rma(yi=est,sei=sd,weights = n,data=full_no_age_test)
full_no_age_meta

full_no_pm_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death,n)~log_rn+age+female+medicaid+white+pm25+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death,n)~log_rn+age+female+medicaid+white+pm25+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(n))
full_no_pm_meta=rma(yi=est,sei=sd,weights = n,data=full_no_pm_test)
full_no_pm_meta

state_level=full_test%>%
  split(substr(.$FIPS,1,2)) %>%
  map(~ rma( yi=est,sei=sd, weights=n,data = .x)) %>%
  map_dfr(~ as.data.frame(t(rbind(coef(.),sqrt(vcov(.))))))

names(state_level)=c("est","sd")
state_level$state=c("CT","ME","MA","NH","RI","VT")


# plot zipcode interval ---------------------------------------------------
full_test=full_test%>%left_join(county_name%>%select(GEOID,NAME),by=c("FIPS"="GEOID"))
full_test$loc=1:nrow(full_test)
state_loc=full_test%>%group_by(substr(FIPS,1,2))%>%summarise(state_loc=mean(loc))
state_level$location=state_loc$state_loc
state_sec_bound=full_test%>%group_by(substr(FIPS,1,2))%>%summarise(state_loc=max(loc))

library(ggplot2)
ggplot(data=full_test)+
  geom_point(aes(x=FIPS,y=est))+
  geom_crossbar(aes(x=FIPS,y=est,ymin=est-1.9615*sd,ymax=est+1.9615*sd))+
  geom_hline(aes(yintercept=0),size=1.5)+
  geom_hline(aes(yintercept=1.4),size=2)+
  geom_errorbar(data=state_level,
                aes(x=location,ymin=3+4*(est-1.9615*sd),ymax=3+4*(est+1.9615*sd)),
                width=2,size=2)+
  geom_point(data=state_level,aes(x=location,y=4*est+3),size=3,shape=5,stroke=2,fill="white")+
  geom_hline(aes(yintercept=3),size=1.5)+
  geom_vline(xintercept = c(state_sec_bound$state_loc+0.5),color="lightgray")+
  ylab("% increase in mortality rate associated with 1 pCi/L increase in Rn")+
  scale_x_discrete(name="County Name",breaks=full_test$FIPS,labels=full_test$NAME)+
  scale_y_continuous(breaks = c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1,2.2,2.6,2.8,3,3.2,3.4,3.8),
                     labels=c("-100%","-80%","-60%","-40%","-20%","0%","20%","40%","60%","80%","100%","-20%","-10%","-5%","0%","5%","10%","20%"))+
  theme(axis.text.x = element_text(size=25,angle=90,hjust = 0.95,vjust = 0.2),
        axis.text.y = element_text(size=20),
        axis.ticks.length = unit(0.5,"lines"),
        axis.title.x = element_blank(),
        axis.title.y=element_text(size=25),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color="black",size=2,linetype = "solid"))

# Subgroup Analysis -------------------------------------------------------
f_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death_f,num_f)~log_rn+age_f+female+white_f+medicaid_f+pm25+w_temp+s_temp+year+zipcode,
                                          family="binomial"))[2],
                         sd=sqrt(vcov(glm(cbind(death_f,num_f)~log_rn+age_f+female+white_f+medicaid_f+pm25+w_temp+s_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                         n=sum(n))
f_meta=rma(yi=est,sei=sd,weights = n,data=f_test)
f_meta

m_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death_m,num_m)~log_rn+age_m+female+white_m+medicaid_m+pm25+w_temp+s_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death_m,num_m)~log_rn+age_m+female+white_m+medicaid_m+pm25+w_temp+s_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(n))
m_meta=rma(yi=est,sei=sd,weights = n,data=m_test)
m_meta

med_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death_med,num_med)~log_rn+med_age+med_female+med_white+medicaid+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death_med,num_med)~log_rn+med_age+med_white+med_female+medicaid+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(n))
med_meta=rma(yi=est,sei=sd,weights = n*n,data=med_test)
med_meta

nmed_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death_nmed,num_nmed)~log_rn+nmed_age+nmed_white+nmed_female+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death_nmed,num_nmed)~log_rn+nmed_age+nmed_white+nmed_female+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(n))
nmed_meta=rma(yi=est,sei=sd,weights = n,data=nmed_test)
nmed_meta

white_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death_wht,num_wht)~log_rn+wht_age+wht_med+wht_female+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death_wht,num_wht)~log_rn+wht_age+wht_med+wht_female+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(n))
white_meta=rma(yi=est,sei=sd,weights = n,data=white_test)
white_meta

a1_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death_a1,num_a1)~log_rn+age+a1_med+a1_female+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death_a1,num_a1)~log_rn+age+a1_med+a1_female+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(num_a1))
a1_meta=rma(yi=est,sei=sd,weights = n,data=a1_test)
a1_meta

a2_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death_a2,num_a2)~log_rn+age+a2_med+a2_female+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death_a2,num_a2)~log_rn+age+a2_med+a2_female+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(num_a2))
a2_meta=rma(yi=est,sei=sd,weights = n,data=a2_test)
a2_meta

a3_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death_a3,num_a3)~log_rn+a3_med+a3_female+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death_a3,num_a3)~log_rn+a3_med+a3_female+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(num_a3))
a3_meta=rma(yi=est,sei=sd,weights = n,data=a3_test)
a3_meta

a4_test=data%>%filter(FIPS%in%FIPS_list$FIPS)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(death_a4,num_a4)~log_rn+age+a4_med+a4_female+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(death_a4,num_a4)~log_rn+age+a4_med+a4_female+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(num_a4))
a4_meta=rma(yi=est,sei=sd,weights = n,data=a4_test)
a4_meta

COPD_test=data%>%filter(FIPS%in%FIPS_list$FIPS,COPD_death!=COPD_n,COPD_death>0)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(COPD_death,COPD_n)~log_rn+age_COPD+female_COPD+medicaid_COPD+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(COPD_death,COPD_n)~log_rn+age_COPD+female_COPD+medicaid_COPD+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(COPD_n))
COPD_meta=rma(yi=est,sei=sd,weights = n,data=COPD_test)
COPD_meta

CVD_test=data%>%filter(FIPS%in%FIPS_list$FIPS,CVD_death!=CVD_n,CVD_death>0)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(CVD_death,CVD_n)~log_rn+age_CVD+female_CVD+medicaid_CVD+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(CVD_death,CVD_n)~log_rn+age_CVD+female_CVD+medicaid_CVD+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(CVD_n))
CVD_meta=rma(yi=est,sei=sd,weights = n,data=CVD_test)
CVD_meta

DM_test=data%>%filter(FIPS%in%FIPS_list$FIPS,DM_death!=DM_n,DM_death>0)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(DM_death,DM_n)~log_rn+age_DM+female_DM+medicaid_DM+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(DM_death,DM_n)~log_rn+age_DM+female_DM+medicaid_DM+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(DM_n))
DM_meta=rma(yi=est,sei=sd,weights = n,data=DM_test)
DM_meta

CHF_test=data%>%filter(FIPS%in%FIPS_list$FIPS,CHF_death!=CHF_n,CHF_death>0)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(CHF_death,CHF_n)~log_rn+age_CHF+female_CHF+medicaid_CHF+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(CHF_death,CHF_n)~log_rn+age_CHF+female_CHF+medicaid_CHF+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(CHF_n))
CHF_meta=rma(yi=est,sei=sd,weights = n,data=CHF_test)
CHF_meta


IS_test=data%>%filter(FIPS%in%FIPS_list$FIPS,IS_death!=IS_n,IS_death>0)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(IS_death,IS_n)~log_rn+age_IS+female_IS+medicaid_IS+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(IS_death,IS_n)~log_rn+age_IS+female_IS+medicaid_IS+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(IS_n))
IS_meta=rma(yi=est,sei=sd,weights = n,data=IS_test)
IS_meta

AMI_test=data%>%filter(FIPS%in%FIPS_list$FIPS,AMI_death!=AMI_n,AMI_death>0)%>%
  group_by(FIPS)%>%summarise(est=coef(glm(cbind(AMI_death,AMI_n)~log_rn+age_AMI+female_AMI+medicaid_AMI+pm25+s_temp+w_temp+year+zipcode,
                                          family="binomial"))[2],
                             sd=sqrt(vcov(glm(cbind(AMI_death,AMI_n)~log_rn+age_AMI+female_AMI+medicaid_AMI+pm25+s_temp+w_temp+year+zipcode,
                                              family="binomial"))[2,2]),
                             n=sum(AMI_n))
AMI_meta=rma(yi=est,sei=sd,weights = n,data=AMI_test)
AMI_meta

model_tank=list()
data$FIPS=as.character(data$FIPS)
for(f in 1:nrow(FIPS_list)){
  sub=as.data.frame(FIPS_list[f,"FIPS"])$FIPS
  temp=data%>%filter(FIPS==sub)
  nzips=length(unique(temp$zipcode))
  if(nzips>2){
    m=glm(cbind(death,n)~Rn_Adj*medicaid+pm25+female+age+white+medicaid+w_temp+s_temp+year+zipcode,
          family="binomial",data=temp)
    inter_index=length(coef(m))
    inter_coef=coef(m)[inter_index]
    inter_sd=sqrt(vcov(m)[inter_index,inter_index])
    model_tank[[f]]=cbind.data.frame(inter_coef,inter_sd,sub,nzips)
  }
}
models=bind_rows(model_tank)
rma(yi=inter_coef,sei=inter_sd,weights = nzips*nzips,data=models)


#
zip_list=unique(mortality_data$zipcode)
#ggplot()+
# geom_sf(data=zips_sf%>%filter(STATE%in%c("NH","MA","RI","CT","VT","ME")))+
# geom_sf(data=zips_sf%>%filter(ZIP%in%zip_list),fill="Blue")
#length(unique(mortality_data$zipcode))

FIPS_list<-unique(mortality_data$FIPS)
#FIPS_list=FIPS_list[substr(FIPS_list,1,2)=="25"]
#create a four column table, col1 ZIP1, col2 ZIP2, Year1, Year2, ZIP1 and ZIP2 must be in the same county
all_pairs=list()
for(interval in 1:10){
  mort_data<-list()
  f=1
  for(fips in FIPS_list){
    fips_mortality=mortality_data%>%filter(FIPS==fips)
    zips=unique(fips_mortality$zipcode)
    zips_num=fips_mortality%>%group_by(zipcode)%>%count()
    zips=zips_num%>%filter(n==16)
    zips<-as.data.frame(zips)
    zips=zips%>%dplyr::select(zipcode)
    names(zips)="zips"
    if(nrow(zips)>2){
      zips$ID=1:nrow(zips)
      zips_arrange=t(combn(nrow(zips),2))
      #zips_arrange=rbind(zips_arrange,cbind(zips_arrange[,2],zips_arrange[,1]))
      zips_arrange<-as.data.frame(zips_arrange)
      names(zips_arrange)=c("left_zip","right_zip")
      zips_arrange$ID=1:nrow(zips_arrange)
      col3=2000:(2015-interval)
      col4=(2000+interval):2015
      year_comp=cbind.data.frame(col3,col4)
      names(year_comp)=c("Start","End")
      year_comp$ID=1:nrow(year_comp)
      
      FIPS_table=expand.grid(zips_arrange$ID,year_comp$ID)
      names(FIPS_table)=c("ZIP_Arrange_ID","Year_Comp_ID")
      FIPS_table=FIPS_table%>%left_join(zips_arrange,by=c("ZIP_Arrange_ID"="ID"))
      FIPS_table=FIPS_table%>%left_join(year_comp,by=c("Year_Comp_ID"="ID"))
      #the left zip is zips.x
      FIPS_table=FIPS_table%>%left_join(zips,by=c("left_zip"="ID"))
      #the right zip is zip.y
      FIPS_table=FIPS_table%>%left_join(zips,by=c("right_zip"="ID"))
      #Add four columns for exposure and mortality
      FIPS_table=FIPS_table%>%left_join(mortality_data,by=c("zips.x"="zipcode","Start"="year"))
      names(FIPS_table)[(ncol(FIPS_table)-43):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-43):ncol(FIPS_table)],".start.x")
      FIPS_table=FIPS_table%>%left_join(mortality_data,by=c("zips.y"="zipcode","Start"="year"))
      names(FIPS_table)[(ncol(FIPS_table)-43):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-43):ncol(FIPS_table)],".start.y")
      FIPS_table=FIPS_table%>%left_join(mortality_data,by=c("zips.x"="zipcode","End"="year"))
      names(FIPS_table)[(ncol(FIPS_table)-43):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-43):ncol(FIPS_table)],".end.x")
      FIPS_table=FIPS_table%>%left_join(mortality_data,by=c("zips.y"="zipcode","End"="year"))
      names(FIPS_table)[(ncol(FIPS_table)-43):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-43):ncol(FIPS_table)],".end.y")
      
      FIPS_table=FIPS_table%>%left_join(pred_data,by=c("zips.x"="ZIPCODE","Start"="Year"))
      names(FIPS_table)[(ncol(FIPS_table)-1):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-1):ncol(FIPS_table)],".start.x")
      FIPS_table=FIPS_table%>%left_join(pred_data,by=c("zips.y"="ZIPCODE","Start"="Year"))
      names(FIPS_table)[(ncol(FIPS_table)-1):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-1):ncol(FIPS_table)],".start.y")
      
      FIPS_table=FIPS_table%>%left_join(pred_data,by=c("zips.x"="ZIPCODE","End"="Year"))
      names(FIPS_table)[(ncol(FIPS_table)-1):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-1):ncol(FIPS_table)],".end.x")
      FIPS_table=FIPS_table%>%left_join(pred_data,by=c("zips.y"="ZIPCODE","End"="Year"))
      names(FIPS_table)[(ncol(FIPS_table)-1):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-1):ncol(FIPS_table)],".end.y")
      
      FIPS_table=FIPS_table%>%dplyr::mutate(FIPS=FIPS.start.x,
                                            rn.start.x=Rn_Adj.start.x,
                                            rn.end.x=Rn_Adj.end.x,
                                            rn.start.y=Rn_Adj.start.y,
                                            pm.start.x=pm25.start.x,
                                            pm.end.x=pm25.end.x,
                                            pm.start.y=pm25.start.y,
                                            pm.end.y=pm25.end.y,
                                            rn.end.y=Rn_Adj.end.y)
      FIPS_table=FIPS_table%>%mutate(
        Dif_Mort=mort.end.y-mort.start.y-mort.end.x+mort.start.x,
        Dif_Rn=rn.end.y-rn.start.y-rn.end.x+rn.start.x,
        Dif_Mort_m=mort_m.end.y-mort_m.start.y-mort_m.end.x+mort_m.start.x,
        Dif_Mort_f=mort_f.end.y-mort_f.start.y-mort_f.end.x+mort_f.start.x,
        Dif_Mort_med=mort_med.end.y-mort_med.start.y-mort_med.end.x+mort_med.start.x,
        Dif_Mort_no_med=mort_no_med.end.y-mort_no_med.start.y-mort_no_med.end.x+mort_no_med.start.x,
        Dif_Mort_white=mort_white.end.y-mort_white.start.y-mort_white.end.x+mort_white.start.x,
        Dif_Mort_no_white=mort_no_white.end.y-mort_no_white.start.y-mort_no_white.end.x+mort_no_white.start.x,
        Dif_Mort_a1=mort_a1.end.y-mort_a1.start.y-mort_a1.end.x+mort_a1.start.x,
        Dif_Mort_a2=mort_a2.end.y-mort_a2.start.y-mort_a2.end.x+mort_a2.start.x,
        Dif_Mort_a3=mort_a3.end.y-mort_a3.start.y-mort_a3.end.x+mort_a3.start.x,
        Dif_Mort_a4=mort_a4.end.y-mort_a4.start.y-mort_a4.end.x+mort_a4.start.x,
        Dif_Mort_COPD=COPD_mort.end.y-COPD_mort.start.y-COPD_mort.end.x+COPD_mort.start.x,
        Dif_Mort_N_COPD=N_COPD_mort.end.y-N_COPD_mort.start.y-N_COPD_mort.end.x+N_COPD_mort.start.x,
        Dif_Mort_CVD=CVD_mort.end.y-CVD_mort.start.y-CVD_mort.end.x+CVD_mort.start.x,
        Dif_Mort_N_CVD=N_CVD_mort.end.y-N_CVD_mort.start.y-N_CVD_mort.end.x+N_CVD_mort.start.x,
        Dif_Mort_CSD=CSD_mort.end.y-CSD_mort.start.y-CSD_mort.end.x+CSD_mort.start.x,
        Dif_Mort_N_CSD=N_CSD_mort.end.y-N_CSD_mort.start.y-N_CSD_mort.end.x+N_CSD_mort.start.x,
        Dif_Mort_CHF=CHF_mort.end.y-CHF_mort.start.y-CHF_mort.end.x+CHF_mort.start.x,
        Dif_Mort_N_CHF=N_CHF_mort.end.y-N_CHF_mort.start.y-N_CHF_mort.end.x+N_CHF_mort.start.x,
        Dif_Mort_DM=DM_mort.end.y-DM_mort.start.y-DM_mort.end.x+DM_mort.start.x,
        Dif_Mort_N_DM=N_DM_mort.end.y-N_DM_mort.start.y-N_DM_mort.end.x+N_DM_mort.start.x,
        Dif_Mort_IS=IS_mort.end.y-IS_mort.start.y-IS_mort.end.x+IS_mort.start.x,
        Dif_Mort_N_IS=N_IS_mort.end.y-N_IS_mort.start.y-N_IS_mort.end.x+N_IS_mort.start.x,
        Dif_Mort_AMI=AMI_mort.end.y-AMI_mort.start.y-AMI_mort.end.x+AMI_mort.start.x,
        Dif_Mort_N_AMI=N_AMI_mort.end.y-N_AMI_mort.start.y-N_AMI_mort.end.x+N_AMI_mort.start.x,
        Dif_pm=pm.end.y-pm.start.y-pm.end.x+pm.start.x,
        Dif_age=age.end.y-age.start.y-age.end.x+age.start.x,
        Dif_white=white.end.y-white.start.y-white.end.x+white.start.x,
        Dif_medicaid=medicaid.end.y-medicaid.start.y-medicaid.end.x+medicaid.start.x,
        Dif_s_temp=s_temp.end.y-s_temp.start.y-s_temp.end.x+s_temp.start.x,
        Dif_w_temp=w_temp.end.y-w_temp.start.y-w_temp.end.x+w_temp.start.x,
        Dif_Rn_w_temp=rn.end.y*w_temp.end.y-rn.start.y*w_temp.start.y-rn.end.x*w_temp.end.x+rn.start.x*w_temp.start.x,
        Dif_Rn_s_temp=rn.end.y*s_temp.end.y-rn.start.y*s_temp.start.y-rn.end.x*s_temp.end.x+rn.start.x*s_temp.start.x,
        Dif_Rn_pm=rn.end.y*pm.end.y-rn.start.y*pm.start.y-rn.end.x*pm.end.x+rn.start.x*pm.start.x,
        Dif_Rn_age=rn.end.y*age.end.y-rn.start.y*age.start.y-rn.end.x*age.end.x+rn.start.x*age.start.x
      )
      
      f=f+1
      FIPS_table=FIPS_table%>%filter(!is.na(Dif_Mort))
      mort_data[[f]]=FIPS_table
      print(fips) 
    }
  }
  mort_data<-bind_rows(mort_data)
  mort_data$beta=mort_data$Dif_Mort/mort_data$Dif_Rn
  #mort_data=mort_data%>%filter(substr(FIPS,1,2)=="25")
  mort_data$beta_pm=mort_data$Dif_Mort/mort_data$Dif_pm
  mort_data=mort_data%>%mutate(mean_Treatment=(Rn_Adj.start.x+Rn_Adj.start.y+
                                                 Rn_Adj.end.x+Rn_Adj.end.y)/4)
  mort_data=mort_data%>%mutate(w=((Rn_Adj.start.x-mean_Treatment)^2+
                                    (Rn_Adj.start.y-mean_Treatment)^2+
                                    (Rn_Adj.end.x-mean_Treatment)^2+
                                    (Rn_Adj.end.y-mean_Treatment)^2)/4)
  all_pairs[[interval]]=mort_data
}

#mort_data=mort_data%>%filter(abs(beta)<1)
#which.max(mort_data$beta)
#which.min(mort_data$beta)
all_pairs=bind_rows(all_pairs)
all_pairs=all_pairs%>%filter(!is.na(w))
#library(MASS)
#library(mgcv)
#mod<-bam(Dif_Mort~s(Rn.start.x)+s(Dif_Rn),data=mort_data)
m<-lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%filter(w<4),weights = w)
summary(m)

m_ma<-lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%
           filter(w<4,substr(FIPS,1,2)=="25"),weights = w)
summary(m_ma)

m_ct<-lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%
           filter(w<4,substr(FIPS,1,2)=="09"),weights = w)
summary(m_ct)

m_ri<-lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%
           filter(w<4,substr(FIPS,1,2)=="44"),weights = w)
summary(m_ri)

m_nh<-lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%
             filter(w<4,substr(FIPS,1,2)=="33"),weights = w)
summary(m_nh)

m_vt<-lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%
           filter(w<4,substr(FIPS,1,2)=="50"),weights = w)
summary(m_vt)

m_me<-lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%
           filter(w<4,substr(FIPS,1,2)=="23"),weights = w)
summary(m_me)

m_m=lm(Dif_Mort_m~0+Dif_Rn,data=all_pairs%>%filter(w<4),weights = w)
summary(m_m)

m_f=lm(Dif_Mort_f~0+Dif_Rn,data=all_pairs%>%filter(w<4),weights=w)
summary(m_f)

m_a1<-lm(Dif_Mort_a1~0+Dif_Rn,all_pairs%>%filter(w<4),weights = w)
summary(m_a1)

m_a2<-lm(Dif_Mort_a2~0+Dif_Rn,all_pairs%>%filter(w<4),weights = w)
summary(m_a2)

m_a3<-lm(Dif_Mort_a3~0+Dif_Rn,all_pairs%>%filter(w<4),weights = w)
summary(m_a3)

m_a4<-lm(Dif_Mort_a4~0+Dif_Rn,all_pairs%>%filter(w<2),weights = w)
summary(m_a4)

m_1<-lm(Dif_Mort~Dif_Rn,data=all_pairs%>%filter((End-Start)==1,w<15),weights = w)
m_2<-lm(Dif_Mort~Dif_Rn,data=all_pairs%>%filter((End-Start)==2,w<15),weights = w)
m_3<-lm(Dif_Mort~Dif_Rn,data=all_pairs%>%filter((End-Start)==3,w<15),weights = w)
m_4<-lm(Dif_Mort~Dif_Rn,data=all_pairs%>%filter((End-Start)==4,w<15),weights = w)
m_5<-lm(Dif_Mort~Dif_Rn,data=all_pairs%>%filter((End-Start)==5,w<15),weights = w)
m_6<-lm(Dif_Mort~Dif_Rn,data=all_pairs%>%filter((End-Start)==6,w<15),weights = w)
m_7<-lm(Dif_Mort~Dif_Rn,data=all_pairs%>%filter((End-Start)==7,w<15),weights = w)
m_8<-lm(Dif_Mort~Dif_Rn,data=all_pairs%>%filter((End-Start)==8,w<15),weights = w)

m_2_3=lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%filter(Rn_Adj.start.x>2,Rn_Adj.start.y>2,Rn_Adj.end.x>2,Rn_Adj.end.y>2,
                                                   Rn_Adj.end.x<3,Rn_Adj.end.y<3,Rn_Adj.start.x<3,Rn_Adj.start.y<3,w<4),weights = w)
summary(m_2_3)

m_3_4=lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%filter(Rn_Adj.start.x>3,Rn_Adj.start.y>3,
                                                   Rn_Adj.end.x<4,Rn_Adj.end.y<4),weights = w)
summary(m_3_4)

m_4_5=lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%filter(Rn_Adj.start.x>4,Rn_Adj.start.y>4,
                                                   Rn_Adj.end.x<5,Rn_Adj.end.y<5),weights = w)
summary(m_4_5)

m_5_6=lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%filter(Rn_Adj.start.x>5,Rn_Adj.start.y>5,
                                                   Rn_Adj.end.x<6,Rn_Adj.end.y<6),weights = w)
summary(m_5_6)

m_6_7=lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%filter(Rn_Adj.start.x>6,Rn_Adj.start.y>6,
                                                   Rn_Adj.end.x<7,Rn_Adj.end.y<7),weights = w)
summary(m_6_7)

m_white=lm(Dif_Mort_white~0+Dif_Rn,
           all_pairs%>%filter(w<4),weights = w)
summary(m_white)

m_no_white=lm(Dif_Mort_no_white~0+Dif_Rn,
              all_pairs%>%filter(w<4),weights = w)
summary(m_no_white)

m_med=lm(Dif_Mort_med~0+Dif_Rn,
         data=all_pairs%>%filter(n.start.x>500,n.start.y>500),weights = w)
summary(m_med)

m_no_med=lm(Dif_Mort_no_med~0+Dif_Rn,
            data=all_pairs%>%filter(n.start.x>500,n.start.y>500),weights = w)
summary(m_no_med)

#chronic disease
m_COPD=lm(Dif_Mort_COPD~0+Dif_Rn,
          data=all_pairs%>%filter(!is.na(Dif_Mort_COPD),
                                  n_COPD.start.x>100,
                                  n_COPD.start.y>100,w<4),weights = w)
summary(m_COPD)

m_N_COPD=lm(Dif_Mort_N_COPD~0+Dif_Rn,
          data=all_pairs%>%filter(!is.na(Dif_Mort_COPD),
                                  n_COPD.start.x>100,
                                  n_COPD.start.y>100,w<4),weights = w)
summary(m_N_COPD)


m_DM=lm(Dif_Mort_DM~0+Dif_Rn,
        data=all_pairs%>%filter(!is.na(Dif_Mort_DM),
                                n_DM.start.x>100,n_DM.start.y>100,w<4),weights = w)
summary(m_DM)

m_CHF=lm(Dif_Mort_CHF~0+Dif_Rn,
         data=all_pairs%>%filter(!is.na(Dif_Mort_CHF),
                                 n_CHF.start.x>100,n_CHF.start.y>100,w<4),weights = w)
summary(m_CHF)

m_CVD=lm(Dif_Mort_CVD~0+Dif_Rn,
         data=all_pairs%>%filter(!is.na(Dif_Mort_CVD),
                                 n_CVD.start.x>100,n_CVD.start.y>100,w<4),weights = w)
summary(m_CVD)

#afterwath impact of acute disease
fips_est=all_pairs%>%filter(w<4)%>%group_by(FIPS)%>%summarise(coef=(coef(lm(Dif_Mort~0+Dif_Rn,weights = w))),
                                       sd=sqrt(diag(vcov(lm(Dif_Mort~0+Dif_Rn,weights = w)))))
fips_est$t=(fips_est$coef/fips_est$sd)
summary(fips_est$t>0)

m_age<-lm(Dif_Mort~0+Dif_Rn+Dif_age,all_pairs%>%filter(w<4),weights=w)
summary(m_age)

m_white<-lm(Dif_Mort~0+Dif_Rn+Dif_white,all_pairs%>%filter(w<4),weights=w)
summary(m_white)

m_medicaid<-lm(Dif_Mort~0+Dif_Rn+Dif_medicaid,all_pairs%>%filter(w<4),weights=w)
summary(m_medicaid)

m_pm<-lm(Dif_Mort~0+Dif_Rn+Dif_pm,data=all_pairs%>%filter(w<4),weights = w)
summary(m_pm)

m_s_temp<-lm(Dif_Mort~0+Dif_Rn+Dif_s_temp,data=all_pairs%>%filter(w<4),weights = w)
summary(m_s_temp)

m_w_temp<-lm(Dif_Mort~0+Dif_Rn+Dif_w_temp,data=all_pairs%>%filter(w<4),weights = w)
summary(m_w_temp)

m_int_pm=lm(Dif_Mort~0+Dif_Rn+Dif_pm+Dif_Rn_pm,data=all_pairs%>%filter(w<2),weights = w)
summary(m_int_pm)

m_int_stemp=lm(Dif_Mort~0+Dif_Rn+Dif_s_temp+Dif_Rn_s_temp,data=all_pairs%>%filter(w<2),weights = w)
summary(m_int_stemp)

m_int_wtemp=lm(Dif_Mort~0+Dif_Rn+Dif_w_temp+Dif_Rn_w_temp,data=all_pairs%>%filter(w<2),weights = w)
summary(m_int_wtemp)

m_int_age=lm(Dif_Mort~0+Dif_Rn+Dif_age+Dif_Rn_age,data=all_pairs%>%filter(w<15),weights = w)
summary(m_int_age)

#Experiment, not confined by county boundary
mortality_data=mortality_data%>%filter(substr(FIPS,1,2)=="25")
zips=unique(mortality_data$zipcode)
zips_num=mortality_data%>%group_by(zipcode)%>%count()
zips=zips_num%>%filter(n==16)
zips<-as.data.frame(zips)
zips=zips%>%dplyr::select(zipcode)
names(zips)="zips"

zips$ID=1:nrow(zips)
zips_arrange=t(combn(nrow(zips),2))
zips_arrange=rbind(zips_arrange,cbind(zips_arrange[,2],zips_arrange[,1]))
zips_arrange<-as.data.frame(zips_arrange)
names(zips_arrange)=c("left_zip","right_zip")
zips_arrange$ID=1:nrow(zips_arrange)
col3=2000:2014
col4=2001:2015

year_comp=cbind.data.frame(col3,col4)
names(year_comp)=c("Start","End")
year_comp$ID=1:nrow(year_comp)

FIPS_table=expand.grid(zips_arrange$ID,year_comp$ID)
names(FIPS_table)=c("ZIP_Arrange_ID","Year_Comp_ID")
FIPS_table=FIPS_table%>%left_join(zips_arrange,by=c("ZIP_Arrange_ID"="ID"))
FIPS_table=FIPS_table%>%left_join(year_comp,by=c("Year_Comp_ID"="ID"))
#the left zip is zips.x
FIPS_table=FIPS_table%>%left_join(zips,by=c("left_zip"="ID"))
#the right zip is zip.y
FIPS_table=FIPS_table%>%left_join(zips,by=c("right_zip"="ID"))
#Add four columns for exposure and mortality
FIPS_table=FIPS_table%>%left_join(mortality_data,by=c("zips.x"="zipcode","Start"="year"))
names(FIPS_table)[(ncol(FIPS_table)-18):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-18):ncol(FIPS_table)],".start.x")
FIPS_table=FIPS_table%>%left_join(mortality_data,by=c("zips.y"="zipcode","Start"="year"))
names(FIPS_table)[(ncol(FIPS_table)-18):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-18):ncol(FIPS_table)],".start.y")
FIPS_table=FIPS_table%>%left_join(mortality_data,by=c("zips.x"="zipcode","End"="year"))
names(FIPS_table)[(ncol(FIPS_table)-18):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-18):ncol(FIPS_table)],".end.x")
FIPS_table=FIPS_table%>%left_join(mortality_data,by=c("zips.y"="zipcode","End"="year"))
names(FIPS_table)[(ncol(FIPS_table)-18):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-18):ncol(FIPS_table)],".end.y")

FIPS_table=FIPS_table%>%left_join(pred_data,by=c("zips.x"="ZIPCODE","Start"="Year"))
names(FIPS_table)[(ncol(FIPS_table)-2):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-2):ncol(FIPS_table)],".start.x")
FIPS_table=FIPS_table%>%left_join(pred_data,by=c("zips.y"="ZIPCODE","Start"="Year"))
names(FIPS_table)[(ncol(FIPS_table)-2):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-2):ncol(FIPS_table)],".start.y")

FIPS_table=FIPS_table%>%left_join(pred_data,by=c("zips.x"="ZIPCODE","End"="Year"))
names(FIPS_table)[(ncol(FIPS_table)-2):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-2):ncol(FIPS_table)],".end.x")
FIPS_table=FIPS_table%>%left_join(pred_data,by=c("zips.y"="ZIPCODE","End"="Year"))
names(FIPS_table)[(ncol(FIPS_table)-2):ncol(FIPS_table)]=paste0(names(FIPS_table)[(ncol(FIPS_table)-2):ncol(FIPS_table)],".end.y")

FIPS_table=FIPS_table%>%dplyr::mutate(FIPS=FIPS.x,
                                      rn.start.x=Rn_Adj.start.x,
                                      rn.end.x=Rn_Adj.end.x,
                                      rn.start.y=Rn_Adj.start.y,
                                      pm.start.x=pm25.start.x,
                                      pm.end.x=pm25.end.x,
                                      pm.start.y=pm25.start.y,
                                      pm.end.y=pm25.end.y,
                                      rn.end.y=Rn_Adj.end.y)
FIPS_table=FIPS_table%>%mutate(
  Dif_Mort=mort.end.y-mort.start.y-mort.end.x+mort.start.x,
  Dif_Rn=rn.end.y-rn.start.y-rn.end.x+rn.start.x,
  Dif_pm=pm.end.y-pm.start.y-pm.end.x+pm.start.x,
  Dif_Mort_m=mort_m.end.y-mort_m.start.y-mort_m.end.x+mort_m.start.x,
  Dif_Mort_f=mort_f.end.y-mort_f.start.y-mort_f.end.x+mort_f.start.x,
  Dif_Mort_a1=mort_a1.end.y-mort_a1.start.y-mort_a1.end.x+mort_a1.start.x,
  Dif_Mort_a2=mort_a2.end.y-mort_a2.start.y-mort_a2.end.x+mort_a2.start.x,
  Dif_s_temp=s_temp.end.y-s_temp.start.y-s_temp.end.x+s_temp.start.x,
  Dif_w_temp=w_temp.end.y-w_temp.start.y-w_temp.end.x+w_temp.start.x,
  Dif_medicaid=medicaid.end.y-medicaid.start.y-medicaid.end.x+medicaid.start.x,
  Dif_white=white.end.y-white.start.y-white.end.x+white.start.x,
  Dif_Rn_w_temp=rn.end.y*w_temp.end.y-rn.start.y*w_temp.start.y-rn.end.x*w_temp.end.x+rn.start.x*w_temp.start.x,
  Dif_Rn_s_temp=rn.end.y*s_temp.end.y-rn.start.y*s_temp.start.y-rn.end.x*s_temp.end.x+rn.start.x*s_temp.start.x,
  Dif_Rn_pm=rn.end.y*pm.end.y-rn.start.y*pm.start.y-rn.end.x*pm.end.x+rn.start.x*pm.start.x)


FIPS_table$beta=FIPS_table$Dif_Mort/FIPS_table$Dif_Rn
FIPS_table=FIPS_table%>%mutate(mean_mort=(mort.start.x+mort.start.y+mort.end.x+mort.end.y)/4)
FIPS_table=FIPS_table%>%mutate(w=((mort.start.x-mean_mort)^2+
                                    (mort.start.y-mean_mort)^2+
                                    (mort.end.x-mean_mort)^2+
                                    (mort.end.y-mean_mort)^2)/4)
library(mgcv)
#mod<-bam(Dif_Mort~s(Dif_s_temp),data=FIPS_table)

m=lm(Dif_Mort~Dif_Rn+Dif_w_temp+Dif_Rn_w_temp,data=FIPS_table,weights = w)
