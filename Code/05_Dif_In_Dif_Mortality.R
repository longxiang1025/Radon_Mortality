library(dplyr)
library(here)
library(ggplot2)
library(purrr)

load(here::here("Data","Medium Data","zipcode_mortality.RData"))
load(here::here("Data","Medium Data","Pred_Rn_Data.RData"))
load(here::here("Data","Medium Data","Monthly_Mete.RData"))
load(here::here("Data","Medium Data","zipcode_admission.RData"))
#load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","Env_Exp.RData"))
#zips_sf<-st_as_sf(zips)
#Out of 10127 zipcodes, only 1492 zipcodes have predicted Rn Data
pred_data<-pred_data%>%filter(POP_SQMI<5000)
pred_data<-pred_data%>%group_by(Year,ZIPCODE)%>%summarise(Rn_Adj=mean(Rn_Adj))
pred_data$Year=as.numeric(as.character(pred_data$Year))
#pred_data$Year=1996+pred_data$Year
pred_data=pred_data%>%left_join(env_exp%>%dplyr::select(year,ZIP,pm25),by=c("Year"="year","ZIPCODE"="ZIP"))
pred_data=pred_data%>%filter(!is.na(pm25))
zip_list=unique(pred_data$ZIPCODE)
#ggplot()+
#  geom_sf(data=zips_sf%>%filter(STATE%in%c("NH","MA","RI","CT","VT","ME")))+
#  geom_sf(data=zips_sf%>%filter(ZIP%in%zip_list),fill="Blue")
#length(unique(mortality_data$zipcode))
mete_data=zip_mete_record%>%group_by(ZIP,year)%>%summarise(s_temp=max(temp),w_temp=min(temp))
mete_data$s_temp=mete_data$s_temp-273.15
mete_data$w_temp=mete_data$w_temp-273.15

mortality_data=mortality_data%>%filter(substr(FIPS,1,2)=="25")
mortality_data=mortality_data%>%filter(zipcode%in%zip_list)
mortality_data=mortality_data%>%left_join(zipcode_admission)
mortality_data=mortality_data%>%filter(n>200)
mortality_data=mortality_data%>%mutate(mort=death/n)

mortality_data=mortality_data%>%left_join(mete_data,by=c("zipcode"="ZIP","year"="year"))
#This is the crude model without 
# data=mortality_data%>%left_join(pred_data,by=c("year"="Year","zipcode"="ZIPCODE"))
# data=data%>%left_join(env_exp)
# library(mgcv)
# data$year=as.factor(data$year)
# data$zipcode=as.factor(data$zipcode)
# data$FIPS=as.factor(data$FIPS)
# mod=bam(mort~Rn_Adj+pm25+white+medicaid+w_temp+s_temp+year+zipcode+s(FIPS,bs="re"),family=betar(link="logit"),data=data%>%filter(substr(FIPS,1,2)=="25"))
#
zip_list=unique(mortality_data$zipcode)
#ggplot()+
# geom_sf(data=zips_sf%>%filter(STATE%in%c("NH","MA","RI","CT","VT","ME")))+
# geom_sf(data=zips_sf%>%filter(ZIP%in%zip_list),fill="Blue")
#length(unique(mortality_data$zipcode))

FIPS_list<-unique(mortality_data$FIPS)
FIPS_list=FIPS_list[substr(FIPS_list,1,2)=="25"]
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
  mort_data=mort_data%>%filter(substr(FIPS,1,2)=="25")
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
#library(MASS)
#library(mgcv)
#mod<-bam(Dif_Mort~s(Rn.start.x)+s(Dif_Rn),data=mort_data)
m<-lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%filter(w<2),weights = w)
summary(m)

m_a1<-lm(Dif_Mort_a1~0+Dif_Rn,all_pairs%>%filter(w<2),weights = w)
summary(m_a1)

m_a2<-lm(Dif_Mort_a2~0+Dif_Rn,all_pairs%>%filter(w<2),weights = w)
summary(m_a2)

m_a3<-lm(Dif_Mort_a3~0+Dif_Rn,all_pairs%>%filter(w<2),weights = w)
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

m_2_3=lm(Dif_Mort~0+Dif_Rn,data=all_pairs%>%filter(Rn_Adj.start.x>2,Rn_Adj.start.y>2,
                                                   Rn_Adj.end.x<3,Rn_Adj.end.y<3),weights = w)
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

m_m=lm(Dif_Mort_m~0+Dif_Rn,data=all_pairs%>%filter(w<2),weights = w)
summary(m_m)


m_f=lm(Dif_Mort_f~0+Dif_Rn,data=all_pairs%>%filter(w<2),weights=w)
summary(m_f)

m_white=lm(Dif_Mort_white~0+Dif_Rn,
           all_pairs%>%filter(w<1),weights = w)
summary(m_white)

m_no_white=lm(Dif_Mort_no_white~0+Dif_Rn,
              all_pairs%>%filter(w<1),weights = w)
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
                                  n_COPD.start.y>100),weights = w)
summary(m_COPD)

m_N_COPD=lm(Dif_Mort_N_COPD~0+Dif_Rn,
             data=all_pairs%>%filter(!is.na(Dif_Mort_COPD),
                                     n_COPD.start.x>100,
                                     n_COPD.start.y>100),weights = w)
summary(m_N_COPD)

m_DM=lm(Dif_Mort_DM~0+Dif_Rn,
        data=all_pairs%>%filter(!is.na(Dif_Mort_DM),
                                n_DM.start.x>50,n_DM.start.y>50),weights = w)
summary(m_DM)

m_N_DM=lm(Dif_Mort_N_DM~0+Dif_Rn,
          data=all_pairs%>%filter(!is.na(Dif_Mort_DM),
                                  n_DM.start.x>50,n_DM.start.y>50),weights = w)
summary(m_N_DM)

m_CHF=lm(Dif_Mort_CHF~Dif_Rn,
        data=all_pairs%>%filter(!is.na(Dif_Mort_CHF),
                                n_CHF.start.x>50,n_CHF.start.y>50),weights = w)
summary(m_CHF)

m_N_CHF=lm(Dif_Mort_N_CHF~0+Dif_Rn,
          data=all_pairs%>%filter(!is.na(Dif_Mort_CHF),
                                  n_CHF.start.x>50,n_CHF.start.y>50),weights = w)
summary(m_N_CHF)
#afterwath impact of acute disease
m_IS=lm(Dif_Mort_IS~0+Dif_Rn+Dif_age,
         data=all_pairs%>%filter(!is.na(Dif_Mort_IS),
                                 n_IS.start.x>100,n_IS.start.y>100),weights = w)
summary(m_IS)

m_N_IS=lm(Dif_Mort_N_IS~0+Dif_Rn+Dif_age,
           data=all_pairs%>%filter(!is.na(Dif_Mort_IS),
                                   n_IS.start.x>100,n_IS.start.y>100),weights = w)
summary(m_N_IS)

m_AMI=lm(Dif_Mort_AMI~0+Dif_Rn,
         data=all_pairs%>%filter(!is.na(Dif_Mort_AMI),
                                 n_AMI.start.x>50,n_AMI.start.y>50),weights = w)
summary(m_AMI)

m_N_AMI=lm(Dif_Mort_N_AMI~0+Dif_Rn,
           data=all_pairs%>%filter(!is.na(Dif_Mort_AMI),
                                   n_AMI.start.x>50,n_AMI.start.y>50),weights = w)
summary(m_N_AMI)

all_pairs%>%group_by(FIPS)%>%summarise(q=quantile(w,0.95))

m_25001=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25001",w<0.43),weights = w))
m_25003=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25003",w<0.49),weights=w))
m_25005=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25005",w<0.91)),weights=w)
m_25007=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25005",w<0.62)),weights=w)
m_25009=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25009",w<3),weights = w))
m_25011=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25011",w<0.82),weights=w))
m_25013=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25013",w<0.665),weights=w))
m_25015=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25015",w<0.414),weights = w))
m_25017=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25017",w<1.7),weights = w))
m_25021=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25021",w<1.2),weights=w))
m_25023=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25023",w<0.955),weights = w))
m_25027=summary(lm(Dif_Mort~0+Dif_Rn,
                   data=all_pairs%>%filter(FIPS=="25027",w<0.549),weights = w))

m_age<-lm(Dif_Mort~0+Dif_Rn+Dif_age,all_pairs%>%filter(w<1),weights=w)
summary(m_age)

m_white<-lm(Dif_Mort~0+Dif_Rn+Dif_white,all_pairs%>%filter(w<1),weights=w)
summary(m_white)

m_medicaid<-lm(Dif_Mort~0+Dif_Rn+Dif_medicaid,all_pairs%>%filter(w<1),weights=w)
summary(m_medicaid)

m_pm<-lm(Dif_Mort~0+Dif_Rn+Dif_pm,data=all_pairs%>%filter(w<2),weights = w)
summary(m_pm)

m_s_temp<-lm(Dif_Mort~0+Dif_Rn+Dif_s_temp,data=all_pairs%>%filter(w<2),weights = w)
summary(m_s_temp)

m_w_temp<-lm(Dif_Mort~0+Dif_Rn+Dif_w_temp,data=all_pairs%>%filter(w<2),weights = w)
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
       