library(dplyr)
library(tidyr)
library(here)
library(lubridate)
library(sf)
library(lme4)
#load(here::here("Data","Medium Data","Monthly_Rn.RData"))
load(here::here("Data","Medium Data","Monthly_Mete.RData"))
load(here::here("Data","Medium Data","Rn_Geology.RData"))
#load(here::here("Data","Medium Data","Annual_Rn.RData"))
load(here::here("Data","Medium Data","NE_Rn.RData"))
ne_radon$Year=lubridate::year(ne_radon$STARTDATE)
ne_radon$Month=lubridate::month(ne_radon$STARTDATE)
ne_radon<-ne_radon%>%mutate(Season= cut(x=lubridate::month(STARTDATE),breaks=c(0,2,6,9,11,12),labels=c("Winter","Spring","Summer","Autumn","winter")))
ne_radon$Season<-as.character(ne_radon$Season)
ne_radon[ne_radon$Season=="winter","Season"]="Winter"
ne_radon=ne_radon%>%left_join(zip_mete_record,by=c("ZIPCODE"="ZIP","Year"="year","Month"="month"))
ne_radon=ne_radon%>%left_join(zips_geo,by=c("ZIPCODE"="ZIP"))
ne_radon=ne_radon%>%filter(PCI.L<38)
ne_radon$geometry=NULL

#valid_measurements=ne_radon%>%group_by(Year,Month,ZIPCODE)%>%summarise(mean_Rn=mean(PCI.L),var_Rn=sd(PCI.L),n_obs=n_distinct(FINGERPRINT))
#save(file=here::here("Data","Medium Data","Valid_Rn_Measurement.RData"),valid_measurements)
load(here::here("Data","Medium Data","Valid_Rn_Measurement.RData"))
valid_measurements=valid_measurements%>%filter(n_obs>4)
valid_measurements=valid_measurements%>%left_join(zip_mete_record,by=c("ZIPCODE"="ZIP","Year"="year","Month"="month"))
valid_measurements=valid_measurements%>%left_join(zips_geo,by=c("ZIPCODE"="ZIP"))
valid_measurements$L_radon=log10(valid_measurements$mean_Rn)
valid_measurements=as.data.frame(valid_measurements)
valid_measurements=valid_measurements%>%dplyr::select(temp,hpbl,Uranium,Rn_Potential,Sur_42,Sur_43,Sur_45,Sur_81,Sur_82,Slope,Month,Year,ZIPCODE,L_radon,POP_SQMI,mean_Rn,n_obs)
valid_measurements=valid_measurements %>% mutate(id = row_number())
valid_measurements=valid_measurements%>%filter(L_radon>(-1))
valid_measurements<-valid_measurements%>%mutate(Season= cut(x=Month,breaks=c(0,2,6,9,11,12),labels=c("Winter","Spring","Summer","Autumn","winter")))
valid_measurements$Season<-as.character(valid_measurements$Season)
valid_measurements[valid_measurements$Season=="winter","Season"]="Winter"
valid_measurements$Season=as.factor(valid_measurements$Season)
valid_measurements$Month=as.factor(valid_measurements$Month)
valid_measurements=valid_measurements%>%filter(!is.na(temp))
#ne_radon$Year=year(ne_radon$STARTDATE)-1996
#ne_radon$Year=as.factor(ne_radon$Year)
#ne_radon$Month=as.factor(ne_radon$Month)
#ne_radon=ne_radon%>%dplyr::select(temp,hpbl,Uranium,Rn_Potential,Sur_42,Sur_43,Sur_45,Sur_81,Sur_82,Slope,Month,Year,ZIPCODE,PCI.L,FINGERPRINT,POP_SQMI)
#ne_radon$L_radon=log(ne_radon$PCI.L)
#ne_radon<-ne_radon%>%filter(PCI.L>0)

#ne_radon=ne_radon%>%drop_na()


library(h2o)
h2o.init()
features<-c("temp","hpbl","Uranium","Rn_Potential","Slope","POP_SQMI","Month","Year","Sur_42","Sur_43","Sur_45","Sur_81","Sur_82","Season")

# for(r in 1:10){
#   train <- valid_measurements %>% sample_frac(.90)
#   test  <- anti_join(valid_measurements, train, by = 'id')
#   h_data_train=as.h2o(train)
#   h_data_test=as.h2o(test)
#   m=h2o.randomForest(y="L_radon",
#                      x=features,
#                      ntrees = 300,
#                      mtries = 5,
#                      max_depth = 100,
#                      min_rows = 3,
#                      nbins_cats = 5,
#                      nfolds = 10,
#                      training_frame = h_data_train)
#   pred_test=h2o.predict(m,h_data_test)
#   pred_test=as.data.frame(pred_test)
#   pred_test$Rn_Adj=exp(pred_test$predict)
#   plot(x=test$mean_Rn,y=pred_test$Rn_Adj)
#   print(summary(lm(test$mean_Rn~0+pred_test$Rn_Adj)))
# }



h_data=as.h2o(valid_measurements)
m=h2o.randomForest(y="L_radon",
                   x=features,
                   ntrees = 300,
                   mtries = 5,
                   max_depth = 100,
                   min_rows = 3,
                   nbins_cats = 5,
                   nfolds = 10,
                   training_frame = h_data)

#m<-lmer(L_radon~temp+hpbl+Uranium+Rn_Potential+Slope+POP_SQMI+Month+Year+(1|ZIPCODE),data=ne_radon)
#cor(m@frame$L_radon,predict(m))
#0.3588672

#pred_data=expand.grid(Year=4:19,Month=1:12,ZIPCODE=unique(ne_radon$ZIPCODE))
pred_data=expand.grid(Year=1996:2018,Month=1:12,ZIPCODE=unique(ne_radon$ZIPCODE))
#pred_data$Year=as.factor(pred_data$Year)
#pred_data$Month=as.factor(pred_data$Month)
load(here::here("Data","Medium Data","Monthly_Mete.RData"))
load(here::here("Data","Medium Data","Rn_Geology.RData"))
#zip_mete_record$year=as.factor(zip_mete_record$year-1996)
#zip_mete_record$year=as.factor(zip_mete_record$year)
#zip_mete_record$month=as.factor(zip_mete_record$month)
pred_data=pred_data%>%left_join(zip_mete_record,by=c("ZIPCODE"="ZIP","Year"="year","Month"="month"))
pred_data=pred_data%>%left_join(zips_geo,by=c("ZIPCODE"="ZIP"))
#pred_data$Year=as.factor(pred_data$Year)
#pred_data$Month=as.factor(pred_data$Month)
pred_data=pred_data%>%dplyr::select(temp,hpbl,Uranium,Rn_Potential,Sur_42,Sur_43,Sur_45,Sur_81,Sur_82,Slope,Month,Year,ZIPCODE,POP_SQMI)
pred_data=pred_data%>%drop_na()
pred_data<-pred_data%>%mutate(Season= cut(x=Month,breaks=c(0,2,6,9,11,12),labels=c("Winter","Spring","Summer","Autumn","winter")))
pred_data$Season<-as.character(pred_data$Season)
pred_data[pred_data$Season=="winter","Season"]="Winter"
pred_data$Month=as.factor(pred_data$Month)
h_pred=as.h2o(pred_data)
pred_radon=h2o.predict(m,newdata=h_pred)
pred_radon=as.data.frame(pred_radon)
pred_data=cbind.data.frame(pred_data,pred_radon)
#pred_data$Rn_Adj=10^(pred_data$predict)

comp_data=valid_measurements%>%left_join(pred_data,by=c("ZIPCODE"="ZIPCODE","Year"="Year","Month"="Month"))
l=lm(L_radon~predict,data=comp_data)
comp_data$L_Rn_Adj=l$coefficients[1]+comp_data$predict*l$coefficients[2]
pred_data$L_Rn_Adj=l$coefficients[1]+pred_data$predict*l$coefficients[2]

comp_data$Rn_Adj=10^comp_data$L_Rn_Adj
pred_data$Rn_Adj=10^pred_data$L_Rn_Adj
l=lm(mean_Rn~Rn_Adj,data=comp_data)
comp_data$Rn_Adj=l$coefficients[1]+comp_data$Rn_Adj*l$coefficients[2]
pred_data$Rn_Adj=l$coefficients[1]+pred_data$Rn_Adj*l$coefficients[2]
#sqrt(mean((comp_data$Rn_Adj-comp_data$mean_Rn)^2,na.rm=T))
#1.431
#cor(comp_data$Rn_Adj,comp_data$mean_Rn,use="complete.obs")
#0.8226
library(ggplot2)
ggplot(data=comp_data,aes(x=mean_Rn,y=Rn_Adj))+
  geom_point(size=5,shape=1,stroke=2,color="navy")+
  geom_hline(yintercept = 0,size=2)+
  geom_vline(xintercept = 0,size=2)+
  geom_abline(intercept = 0,slope=1,size=3)+
  coord_fixed(xlim=c(0,20),ylim=c(0,20),ratio=1,clip = "on")+
  xlab("Monthly Average ZIP Code-specific Radon (# > 5, pCi/L)")+
  ylab("Predicted Monthly ZIP Code-specific Radon (pCi/L)")+
  theme_bw()+
  theme(
    axis.line = element_line(linetype = "solid"),
    axis.title = element_text(size=25),
    axis.text = element_text(size=20)
  )

ggplot(data=comp_data,aes(x=Rn_Adj,y=mean_Rn-Rn_Adj))+
  geom_point(size=5,shape=1,stroke=2,color="navy")+
  geom_hline(yintercept = 0,size=2)+
  xlab("Predicted Monthly ZIP Code-specific Radon (pCi/L)")+
  ylab("Residual of the Predicted Radon (pCi/L)")+
  coord_fixed(xlim=c(0,15),ylim=c(-10,10),ratio=0.5,clip = "on")+
  theme_bw()+
  theme(
    axis.line = element_line(linetype = "solid"),
    axis.title = element_text(size=25),
    axis.text = element_text(size=20)
  )
  

ggplot(data=comp_data,aes(x=mean_Rn,y=Rn_Adj))+
  stat_bin_hex(geom = "hex",colour="black", na.rm=TRUE,binwidth = 0.5) +
  geom_abline(intercept = 0,slope=1,size=2)+
  scale_fill_gradientn(colours=c("#552583","#FDB927"), 
                       name = "Frequency", 
                       na.value=NA)+
  guides(fill = guide_colourbar(direction = "horizontal",
                                barwidth = 24.5, barheight = 1,title.position = "top",label.vjust=1))+
         
  coord_fixed(xlim=c(0,20),ylim=c(0,20),ratio=1)+
  guides(fill = guide_colourbar(direction = "horizontal",barwidth = 20, barheight = 1,title.position = "top"))+
  xlab("Monthly Average ZIP Code-specific Radon (# > 5, pCi/L)")+
  ylab("Predicted Monthly ZIP Code-specific Radon (pCi/L)")+
  theme_bw()+
  theme(
    axis.line = element_line(linetype = "solid"),
    axis.title = element_text(size=25),
    axis.text = element_text(size=20),
    legend.position = c(0.7,0.1),
    legend.title = element_text(size=30),
    legend.text = element_text(size=25)
  )
#names(pred_data)[15]="L_Rn"
#pred_data$Rn=exp(pred_data$L_Rn)
#R^2=0.71219
#MAE= 0.487


#pred_Radon=predict(object=m,newdata=pred_data,re.form=~(1|ZIPCODE))
#pred_data=cbind.data.frame(pred_data,pred_Radon)
#pred_data$Rn=exp(pred_data$pred_Radon)
#summarise the original dateset and compare with the predicted 
#comp_data=valid_measurements%>%left_join(pred_data)%>%filter(!is.na(Rn))
#adj_model=lm(mean_Rn~Rn,data=comp_data)
#pred_data$Rn_Adj=coef(adj_model)[1]+coef(adj_model)[2]*pred_data$Rn

#comp_data=comp_data%>%left_join(pred_data%>%dplyr::select(Year,Month,ZIPCODE,Rn_Adj))
#temp=comp_data%>%group_by(Year,ZIPCODE)%>%summarise(obs_rn=mean(mean_Rn),
#                                                    prd_rn=mean(Rn_Adj))
#cor(comp_data$Rn_Adj,comp_data$mean_Rn)
#ggplot(data=temp)+
#  geom_point(aes(x=obs_rn,y=prd_rn))+
#  geom_abline(intercept = 0,slope = 1)+
#  coord_cartesian(xlim=c(0,25),ylim=c(0,25))+
#  theme_classic()
#0.53816
save(file=here::here("Data","Medium Data","Pred_Rn_Data_Mod.RData"),pred_data)
