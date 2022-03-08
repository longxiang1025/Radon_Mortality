#The objective of this script is to compare long- and short-term measurements, originally for an EHP
#research letter, then need to be recast into a full research letter
library(dplyr)
library(readr)
library(sf)
library(lubridate)
library(mgcv)
library(cowplot)
library(grid)
library(gridExtra)
library(ggplot2)
library(sp)

season_prop=function(start_date,end_date){
  #The objective of this function is to calcualte the proportion of long-term measurement in each season
  day_imp=lubridate::interval(start=start_date,end=end_date)
  day_imp=start_date+0:as.numeric(day_imp,"day")
  day_imp=yday(day_imp)
  day_imp=as.data.frame(day_imp)
  names(day_imp)="doy"
  day_imp$season=cut(day_imp$doy,
                     breaks=c(0,59,155,246,337,366),
                     labels = c("Winter","Spring","Summer","Autumn","Winter"))
  return(summary(as.factor(day_imp$season))/nrow(day_imp))
}

load("Long_and_Short.RData")
result_data=result_data%>%filter(Init_Measurement<40)
result_data$duration=as.numeric(result_data$Follow_End_Date-result_data$Follow_Start_Date)
result_data=result_data%>%filter(duration>89)
result_data=result_data%>%filter(Init_Measurement>0,Follow_Measurement>0)
result_data=result_data%>%filter(Diff_Days<32,duration<366)
result_data$log_Init=log(result_data$Init_Measurement)
result_data$log_Follow=log(result_data$Follow_Measurement)
result_data$Short_Month=lubridate::month(result_data$Init_End_Date)

result_data=result_data%>%left_join(month_season_table,by=c("Short_Month"="month"))
result_data$duration_centered=result_data$duration-200

result_season=mapply(FUN = season_prop,
                      start_date=result_data$Follow_Start_Date,
                      end_date=result_data$Follow_End_Date)
result_season=as.data.frame(t(result_season))
result_data=bind_cols(result_data,result_season)

m=lm(log_Follow~log_Init+duration_centered+Winter+Summer+Spring,data=result_data)
summary(m)

m=gam(log_Follow~log_Init+s(duration_centered)+Winter+Summer+Spring,data=result_data)
summary(m)

pred_grids=expand.grid(log_Init=log(seq(0.2,10,0.02)),duration=seq(90,365,1))
pred=predict.lm(m,pred_grids,interval = "confidence")
pred=cbind.data.frame(pred_grids,pred)
pred$pred_sd=(pred$upr-pred$lwr)/1.96*2
pred$p=pnorm(q=log(4),mean=pred$fit,sd=pred$pred_sd,lower.tail = F)


ggplot()+
  geom_point(data=pred%>%filter(duration%in%c(90,300)),
             aes(x=exp(log_Init),y=p,color=duration))

load(file="Merged_Measurements_201031.RData")
percent_grid=seq(0.4,10,0.1)
short_term_measurements=lab_data%>%filter(Method!="AT")

radon_count=list()
for(i in 2:length(percent_grid)){
  range=c(percent_grid[i-1],percent_grid[i])
  percent=mean(between(short_term_measurements$PCI.L,percent_grid[i-1],percent_grid[i]))
  
  short_prob=pred%>%filter(duration==90,log_Init>log(range[1]),log_Init<log(range[2]))
  short_prob=mean(short_prob$p)
  long_prob=pred%>%filter(duration==300,log_Init>log(range[1]),log_Init<log(range[2]))
  long_prob=mean(long_prob$p)
  radon_count[[i]]=cbind.data.frame(range[1],range[2],percent,short_prob,long_prob)
  names(radon_count[[i]])=c("Low_End","High_End","Percent","Short_Prob","Long_Prob")
}
radon_count=bind_rows(radon_count)
radon_count$Count=139684244*radon_count$Percent
radon_count$Diff_Count=radon_count$Count*(radon_count$Short_Prob-radon_count$Long_Prob)
radon_count$Savings=radon_count$Diff_Count*2000
#-----------Figure 1. Locations of Measurements-----------------
load(here::here("Data","GeoData","2015_Shapes.RData"))
zip_centroid=st_centroid(st_as_sf(zips))
zip_centroid=cbind.data.frame(zip_centroid$ZIP,st_coordinates(zip_centroid))
names(zip_centroid)=c("ZIPCODE","Longitude","Latitude")
result_data=result_data%>%left_join(zip_centroid)

result_data$Longitude=result_data$Longitude+runif(nrow(result_data),-0.05,0.05)
result_data$Latitude=result_data$Latitude+runif(nrow(result_data),-0.05,0.05)
result_data=result_data%>%filter(!is.na(Longitude))
coordinates(result_data)=~Longitude+Latitude
result_data=st_as_sf(result_data)
st_crs(result_data)=st_crs(zips)

load(here::here("Data","GeoData","Boundaries.RData"))
bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs="+proj=utm +zone=16 +datum=WGS84")
result_data=result_data%>%mutate(Duration_Category=as.factor((duration>120)+(duration>290)))%>%
  mutate(Gap_Category=as.factor((Diff_Days>30)+(Diff_Days>60)))%>%
  arrange((Diff_Days))
result_data=result_data%>%filter(duration>89,duration<450)

us_map=ggplot()+
  geom_sf(data=bound_sf%>%filter(STUSPS%in%c(state.abb[c(1,3:10,12:50)])),fill="white")+
  geom_sf(data=result_data%>%filter(State%in%c(state.abb[c(1,3:10,12:50)]),Duration_Category=="0"),
          aes(color="Early"),shape=24,size=1.75,stroke=0.03,fill=NA)+
  geom_sf(data=result_data%>%filter(State%in%c(state.abb[c(1,3:10,12:50)]),Duration_Category=="1"),
          aes(color="Medium"),shape=24,size=1.75,stroke=0.03,fill=NA)+
  geom_sf(data=result_data%>%filter(State%in%c(state.abb[c(1,3:10,12:50)]),Duration_Category=="2"),
          aes(color="Long"),shape=24,size=1.75,stroke=0.03,fill=NA)+
  scale_color_manual("Duration of the long-term measurement \n in the pair of measurements",
                    breaks = c("Early","Medium","Long"),
                    values = c("#B22234","darkgreen","#3C3B6E"),
                    labels=c("90-120 days","121-300 days",">300 days"),
                    guide=guide_legend(direction = "horizontal",
                                       title.position = "left",
                                       label.position = "bottom",
                                       keywidth = unit(0.65, "inch")))+
  coord_sf(crs="+proj=utm +zone=14 +datum=WGS84")+
  theme_bw()+
  theme(
    legend.direction = "vertical",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    axis.title = element_blank(),
    legend.title = element_text(size=10),
    legend.text = element_text(size=10),
    legend.position = "bottom"
    #legend.position=c(0.18,0.125)
  )
us_map

h_duration=ggplot()+
  geom_histogram(data=result_data,aes(x=duration,y=..density..,fill=Duration_Category),
                 binwidth = 4,color="black",alpha=0.5)+
  coord_cartesian(xlim = c(90,400))+
  scale_fill_manual(breaks = c("0","1","2"),
                    values = c("#B22234","darkgreen","#3C3B6E"))+
  xlab("Duration of long-term measurements (days)")+
  ylab("Probability")+
  theme_bw()+
  theme(
    axis.title = element_text(size=13),
    axis.text = element_text(size=12),
    legend.position = "none"
  )

h_duration

ratio=100
s_percent=result_data%>%group_by(30*as.integer(37*Follow_Measurement/30))%>%summarise(n_s=sum(Duration_Category=="0")/length(ID),
                                                                    n_m=sum(Duration_Category=="1")/length(ID),
                                                                    n_l=sum(Duration_Category=="2")/length(ID))
names(s_percent)[1]="x"
s_histogram=ggplot()+
  geom_histogram(data=result_data,aes(x=37*Init_Measurement,y=..density..),
                 binwidth = 10,color="black",fill="lightgray")+
  #geom_density(data=result_data,aes(x=37*Init_Measurement,y=..density..),size=1.5,n=512)+
  geom_path(data=s_percent,aes(x=x,y=n_s/100,color="short"),alpha=0.15)+
  geom_smooth(data=s_percent,aes(x=x,y=n_s/100,color="short"),se=F)+
  geom_path(data=s_percent,aes(x=x,y=n_l/100,color="long"),alpha=0.15)+
  geom_smooth(data=s_percent,aes(x=x,y=n_l/100,color="long"),se=F)+
  geom_path(data=s_percent,aes(x=x,y=n_m/100,color="medium"),alpha=0.15)+
  geom_smooth(data=s_percent,aes(x=x,y=n_m/100,color="medium"),se=F)+
  geom_vline(aes(xintercept=148),size=1.25,color="black",linetype="dashed")+
  scale_color_manual("Percent of Durations",
                     breaks = c("short","medium","long"),
                     values = c("#B22234","darkgreen","#3C3B6E"),
                     labels=c("90-120 days","121-300 days",">300 days"),
                     guide=guide_legend(direction = "vertical",
                                        title.position = "top",
                                        label.position = "right",
                                        keywidth = unit(0.15, "inch")))+
  scale_y_continuous(sec.axis = sec_axis(trans = ~.*ratio,name = "Percent of Durations"))+
  coord_cartesian(xlim=c(0,400),ylim = c(0,0.01))+
  xlab("Short-term Measurement (Bq/m3)")+
  ylab("Probability")+
  theme_bw()+
  theme(
    axis.title = element_text(size=13),
    axis.text = element_text(size=12),
    legend.position=c(0.65,0.8),
    legend.background = element_rect(color="black",fill="white",size=0.25)
  )
s_histogram


l_percent=result_data%>%group_by(30*as.integer(37*Follow_Measurement/30))%>%summarise(n_s=sum(Duration_Category=="0")/length(ID),
                                                                                      n_m=sum(Duration_Category=="1")/length(ID),
                                                                                      n_l=sum(Duration_Category=="2")/length(ID))
names(l_percent)[1]="x"
l_histogram=ggplot()+
  geom_histogram(data=result_data,aes(x=37*Follow_Measurement,y=..density..),
                 binwidth = 10,color="black",fill="lightgray")+
  #geom_density(data=result_data,aes(x=37*Follow_Measurement,y=..density..),size=1.5)+
  geom_path(data=l_percent,aes(x=x,y=n_s/100,color="short"),alpha=0.15)+
  geom_smooth(data=l_percent,aes(x=x,y=n_s/100,color="short"),se=F)+
  geom_path(data=l_percent,aes(x=x,y=n_l/100,color="long"),alpha=0.15)+
  geom_smooth(data=l_percent,aes(x=x,y=n_l/100,color="long"),se=F)+
  geom_path(data=l_percent,aes(x=x,y=n_m/100,color="medium"),alpha=0.15)+
  geom_smooth(data=l_percent,aes(x=x,y=n_m/100,color="medium"),se=F)+
  geom_vline(aes(xintercept=148),size=1.25,color="black",linetype="dashed")+
  coord_cartesian(xlim=c(0,400),ylim = c(0,0.01))+
  scale_y_continuous(sec.axis = sec_axis(trans = ~.*ratio,name = "Percent of Durations"))+
  scale_color_manual("Percent of Durations",
                     breaks = c("short","medium","long"),
                     values = c("#B22234","darkgreen","#3C3B6E"),
                     labels=c("90-120 days","121-300 days",">300 days"),
                     guide=guide_legend(direction = "vertical",
                                        title.position = "left",
                                        label.position = "bottom",
                                        keywidth = unit(0.25, "inch")))+
  xlab("Long-term Measurement (Bq/m3)")+
  ylab("Probability")+
  theme_bw()+
  theme(
    axis.title = element_text(size=13),
    axis.text = element_text(size=12),
    legend.position="none"
  )
l_histogram

rl=cowplot::plot_grid(s_histogram,l_histogram,nrow = 2)
ll=cowplot::plot_grid(us_map,h_duration,nrow=2,rel_heights = c(2,1))
f1=cowplot::plot_grid(ll,rl,nrow = 1,rel_widths = c(2,1))
f1
cowplot::save_plot("Figure_1.pdf",f1,base_width = 12,base_height = 9)

#-----------Figure 2. Two curves------------------
result_data$Init_Integer=as.integer(result_data$Init_Measurement)
measurement_90=result_data%>%filter(duration<120,Diff_Days<30)
measurement_mid=result_data%>%filter(duration>120,duration<290,Diff_Days<30)
measurement_360=result_data%>%filter(duration>290,duration<450,Diff_Days<30)

t_s=measurement_90%>%group_by(as.integer(Init_Measurement))%>%summarise(mean(Follow_Measurement>4),n=length(Follow_Measurement))
t_l=measurement_360%>%group_by(as.integer(Init_Measurement))%>%summarise(mean(Follow_Measurement>4),n=length(Follow_Measurement))
t_m=measurement_mid%>%group_by(as.integer(Init_Measurement))%>%summarise(mean(Follow_Measurement>4),n=length(Follow_Measurement))

names(t_s)=c("Short","Positive_Rate","N")
names(t_l)=c("Short","Positive_Rate","N")
names(t_m)=c("Short","Positive_Rate","N")

pred_vis=pred%>%filter(duration%in%c(90,300))
pred_vis$duration=as.factor(pred_vis$duration)

ratio=14

f2=ggplot()+
  geom_boxplot(data=measurement_90%>%filter(Init_Measurement<10),
               aes(x=Init_Integer+0.5-0.09,y=Follow_Measurement,group=Init_Integer,fill="short_bar"),
               width=0.15,outlier.size = 0.5,alpha=0.5)+
  geom_boxplot(data=measurement_360%>%filter(Init_Measurement<10),
               aes(x=Init_Integer+0.5+0.09,y=Follow_Measurement,group=Init_Integer,fill="long_bar"),
               width=0.15,outlier.size = 0.5,alpha=0.5)+
  geom_hline(aes(yintercept=4),linetype="dashed")+
  geom_path(data=pred_vis%>%filter(duration=="90"),
            aes(x=exp(log_Init),y=p*ratio,color="short_line"),
            size=2.5)+
  geom_path(data=pred_vis%>%filter(duration=="300"),
            aes(x=exp(log_Init),y=p*ratio,color="long_line"),
            size=2.5)+
  scale_fill_manual("Duration of Long-term Measurements",
                    breaks = c("short_bar","long_bar"),
                    values=c("#f4a582","#92c5de"),
                    labels = c("Duration in [90,120] days",
                               "Duration in [300,400] days")
                    )+
  scale_color_manual("Predicted Likelihood of > 148 Bq/m3",
                     breaks = c("short_line","long_line"),
                     values = c("#ca0020","#0571b0"),
                     labels=c("Duration = 90 days",
                              "Duration = 360 days")
                     )+
  scale_x_continuous(breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5),
                     labels = c("0 ~ 37 \n (0-1 pCi/L)",
                                "37 ~ 74 \n (1-2 pCi/L)",
                                "74 ~ 111 \n (2-3 pCi/L)",
                                "111 ~ 148 \n (3-4 pCi/L)",
                                "148 ~ 185 \n (4-5 pCi/L)",
                                "185 ~ 222 \n (5-6 pCi/L)",
                                "222 ~ 259 \n (6-7 pCi/L)",
                                "259 ~ 296 \n (7-8 pCi/L)",
                                "296 ~ 333 \n (8-9 pCi/L)",
                                "334 ~ 370 \n(9-10 pCi/L)"))+
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14),
                     labels = c ("(0  pCi/L)   0",
                                 "(2  pCi/L)  74",
                                 "(4  pCi/L) 148",
                                 "(6  pCi/L) 222",
                                 "(8  pCi/L) 296",
                                 "(10 pCi/L) 370",
                                 "(12 pCi/L) 444",
                                 "(14 pCi/L) 518"),
                     sec.axis = sec_axis(trans = ~./ratio,name = "Predicted Probability of >148 Bq/m3 Long-term Measurements"))+
  coord_cartesian(xlim = c(0,10),ylim = c(-0.25,15),expand = F)+
  xlab("Short-term Measurement (Bq/m3)")+
  ylab("Observed Long-term Radon Measurement (mBq/m3)")+
  theme_bw()+
  theme(
    axis.text = element_text(size=12),
    axis.title = element_text(size = 13),
    legend.position = c(0.2,0.85),
    legend.text = element_text(size=12),
    legend.title = element_text(size=13),
    legend.background = element_rect(color = "black",fill="white",size=0.5)
  )
ggsave("Figure_2.pdf",f2,height = 9,width = 12,units = "in")
