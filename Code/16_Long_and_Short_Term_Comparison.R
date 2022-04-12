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
library(EnvStats)

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

lm_eqn <- function(df){
  m <- lm(Init_Measurement~Follow_Measurement,df);
  eq <- substitute(~~italic(r)^2~"="~r2, 
                   list(r2 = format((summary(m))$r.squared, digits = 2)))
  as.character(as.expression(eq));
}
r2_est<-function(df,n){
  result=list()
  for(i in 1:n){
    set.seed(i+1230)
    r_data=sample(1:nrow(df),nrow(df),replace = T)
    r_data=df[r_data,]
    m<-lm(Follow_Measurement~Init_Measurement+I(Init_Method=="LS")+I(Floor=="Basement")+duration_centered+I(Winter>=0.25)+I(Spring>=0.25)+I(Summer>=0.25)+I(Autumn>=0.25),
          data=r_data)
    r=(summary(m))$r.square
    result[[i]]=r
  }
  result=unlist(result)
  return(result)

}
#Main Analysis-------------------
load("Long_and_Short.RData")
result_data=result_data%>%filter(Init_Measurement<30)
result_data$duration=as.numeric(result_data$Follow_End_Date-result_data$Follow_Start_Date)
result_data=result_data%>%filter(duration>89,duration<366)
result_data=result_data%>%filter(Init_Measurement>0,Follow_Measurement>0)
result_data$log_Init=log(result_data$Init_Measurement)
result_data$log_Follow=log(result_data$Follow_Measurement)
result_data$Short_Month=lubridate::month(result_data$Init_End_Date)
result_data$duration_centered=result_data$duration-200

result_season=mapply(FUN = season_prop,
                      start_date=result_data$Follow_Start_Date,
                      end_date=result_data$Follow_End_Date)
result_season=as.data.frame(t(result_season))
result_data=bind_cols(result_data,result_season)

#2358 pairs of measurements
m0=lm(Follow_Measurement~Init_Measurement+I(Init_Method=="LS")+I(Floor=="Basement")+duration_centered+I(Winter>=0.25)+I(Spring>=0.25)+I(Summer>=0.25)+I(Autumn>=0.25),
      data=result_data%>%filter(Diff_Days<=180,Diff_Days>=0))
summary(m0)

m0_re=r2_est(df=result_data%>%filter(Diff_Days<=180,Diff_Days>=0),
             n=1000)
quantile(m0_re,c(0.025,0.975))

#690 pairs of measurements
m1=lm(Follow_Measurement~Init_Measurement+I(Init_Method=="LS")+I(Floor=="Basement")+duration_centered+I(Winter>=0.25)+I(Spring>=0.25)+I(Summer>=0.25)+I(Autumn>=0.25),
     data=result_data%>%filter(Diff_Days<=7,Diff_Days>=0))
summary(m1)

m1_re=r2_est(df=result_data%>%filter(Diff_Days<=7,Diff_Days>=0),
          n=1000)
quantile(m1_re,c(0.025,0.975))

#241 pairs of measurements
m2=lm(Follow_Measurement~Init_Measurement+I(Init_Method=="LS")+I(Floor=="Basement")+duration_centered+I(Winter>=0.25)+I(Spring>=0.25)+I(Summer>=0.25)+I(Autumn>=0.25),
      data=result_data%>%filter(Diff_Days<=14,Diff_Days>=7))
summary(m2)
m2_re=r2_est(df=result_data%>%filter(Diff_Days<=14,Diff_Days>=7),
             n=1000)
quantile(m2_re,c(0.025,0.975))

#387 pairs of measurements
m3=lm(Follow_Measurement~Init_Measurement+I(Init_Method=="LS")+I(Floor=="Basement")+duration_centered+I(Winter>=0.25)+I(Spring>=0.25)+I(Summer>=0.25)+I(Autumn>=0.25),
      data=result_data%>%filter(Diff_Days<=28,Diff_Days>=14))
summary(m3)
m3_re=r2_est(df=result_data%>%filter(Diff_Days<=28,Diff_Days>=14),
             n=1000)
quantile(m3_re,c(0.025,0.975))

#420 pairs of measurements
m4=lm(Follow_Measurement~Init_Measurement+I(Init_Method=="LS")+I(Floor=="Basement")+duration_centered+I(Winter>=0.25)+I(Spring>=0.25)+I(Summer>=0.25)+I(Autumn>=0.25),
      data=result_data%>%filter(Diff_Days<=60,Diff_Days>=28))
summary(m4)
m4_re=r2_est(df=result_data%>%filter(Diff_Days<=60,Diff_Days>=28),
             n=1000)
quantile(m4_re,c(0.025,0.975))

#455 pairs of measurements
m5=lm(Follow_Measurement~Init_Measurement+I(Init_Method=="LS")+I(Floor=="Basement")+duration_centered+I(Winter>=0.25)+I(Spring>=0.25)+I(Summer>=0.25)+I(Autumn>=0.25),
      data=result_data%>%filter(Diff_Days<=120,Diff_Days>=60))
summary(m5)
m5_re=r2_est(df=result_data%>%filter(Diff_Days<=120,Diff_Days>=60),
             n=1000)
quantile(m5_re,c(0.025,0.975))

#267 pairs of measurements
m6=lm(Follow_Measurement~Init_Measurement+I(Init_Method=="LS")+I(Floor=="Basement")+duration_centered+I(Winter>=0.25)+I(Spring>=0.25)+I(Summer>=0.25)+I(Autumn>=0.25),
      data=result_data%>%filter(Diff_Days<=180,Diff_Days>=120))
summary(m6)
m6_re=r2_est(df=result_data%>%filter(Diff_Days<=180,Diff_Days>=120),
             n=1000)
quantile(m6_re,c(0.025,0.975))

#Supplementary Analysis (Restrict to one-year measurement)----------
short_season=mapply(FUN = season_prop,
                     start_date=result_data$Init_Start_Date,
                     end_date=result_data$Init_Start_Date)
short_season=t(short_season)
short_season=as.data.frame(short_season)
names(short_season)=paste0("S_",names(short_season))
result_data=cbind.data.frame(result_data,short_season)

para_table=expand.grid(diff_cat=c(1,2),
                       month_cat=c(1,2,3,4))
diff_spec=rbind(c(1,0,28),
                c(2,28,180))
diff_spec=as.data.frame(diff_spec)
names(diff_spec)=c("diff_cat","low_l","up_l")
para_table=para_table%>%left_join(diff_spec)

result=list()

for( i in 1:nrow(para_table)){
  low_l=para_table[i,"low_l"]
  up_l=para_table[i,"up_l"]
  s=para_table[i,"month_cat"]
  if(s==1){
    s_list=c(12,1,2)
  }else if(s==2){
    s_list=c(3,4,5)
  }else if(s==3){
    s_list=c(6,7,8)
  }else if(s==4){
    s_list=c(9,10,11)
  }
  m=lm(Follow_Measurement~Init_Measurement+I(Floor=="Basement")+I(Init_Method=="LS")+duration,
       data=result_data%>%filter(duration>=330,duration<=370,Diff_Days>=low_l,Diff_Days<=up_l,
                                 (month(Init_Start_Date))%in%s_list))
  r2=(summary(m))$r.squared
  n=nrow(m$model)
  re=cbind.data.frame(para_table[i,],r2,n,t(coef(m)))
  result[[i]]=re
}
result=bind_rows(result)
ggplot(data=result)+geom_path(aes(x=diff_cat,y=r2,group=month_cat,color=month_cat))

#Analysis to generate two curves (inactive)--------------
pred_grids=expand.grid(log_Init=log(seq(0.2,10,0.02)),duration=seq(90,365,1))
pred=predict.lm(m,pred_grids,interval = "confidence")
pred=cbind.data.frame(pred_grids,pred)
pred$pred_sd=(pred$upr-pred$lwr)/1.96*2
pred$p=pnorm(q=log(4),mean=pred$fit,sd=pred$pred_sd,lower.tail = F)


ggplot()+
  geom_point(data=pred%>%filter(duration%in%c(90,300)),
             aes(x=exp(log_Init),y=p,color=duration))

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
#Figure 1. Locations of Measurements-----------------
load("Long_and_Short.RData")
result_data=result_data%>%filter(Init_Measurement<30)
result_data$duration=as.numeric(result_data$Follow_End_Date-result_data$Follow_Start_Date)
result_data=result_data%>%filter(duration>89,duration<366)
result_data=result_data%>%filter(Diff_Days<=180)
result_data=result_data%>%filter(Init_Measurement>0,Follow_Measurement>0)
result_data$log_Init=log(result_data$Init_Measurement)
result_data$log_Follow=log(result_data$Follow_Measurement)
result_data$Short_Month=lubridate::month(result_data$Init_End_Date)
result_data$duration_centered=result_data$duration-200

load(here::here("Data","GeoData","2015_Shapes.RData"))
sf::sf_use_s2(FALSE)
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
result_data=result_data%>%mutate(Gap_Category=as.factor((Diff_Days>=7)+(Diff_Days>=28)))%>%
  arrange(desc(-Diff_Days))

us_map=ggplot()+
  geom_sf(data=bound_sf%>%filter(STUSPS%in%c(state.abb[c(1,3:10,12:50)])),fill="white")+
  geom_sf(data=result_data%>%filter(State%in%c(state.abb[c(1,3:10,12:50)]),Gap_Category=="2"),
          aes(color="Long"),shape=24,size=1.75,stroke=0.03,fill=NA)+
  geom_sf(data=result_data%>%filter(State%in%c(state.abb[c(1,3:10,12:50)]),Gap_Category=="1"),
          aes(color="Medium"),shape=24,size=1.75,stroke=0.03,fill=NA)+
  geom_sf(data=result_data%>%filter(State%in%c(state.abb[c(1,3:10,12:50)]),Gap_Category=="0"),
          aes(color="Early"),shape=24,size=1.75,stroke=0.03,fill=NA)+
  scale_color_manual("Temporal difference between \n the long- and short-term measurements",
                    breaks = c("Early","Medium","Long"),
                    values = c("#B22234","darkgreen","#3C3B6E"),
                    labels=c("< 1 Week","1 Week-1 Month","1 Month-Half year"),
                    guide=guide_legend(direction = "horizontal",
                                       title.position = "left",
                                       label.position = "bottom",
                                       keywidth = unit(1, "inch")))+
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

h_duration=ggplot()+
  geom_histogram(data=result_data,aes(x=duration,y=..density..,fill=Gap_Category),
                 binwidth = 4,color="black",alpha=0.75)+
  coord_cartesian(xlim = c(85,370),ylim = c(0,0.12),clip = "on",expand = F)+
  scale_fill_manual(breaks = c("0","1","2"),
                    values = c("#B22234","darkgreen","#3C3B6E"))+
  xlab("Duration of long-term measurements (days)")+
  ylab("Probability")+
  theme_bw()+
  theme(
    axis.title = element_text(size=11),
    axis.text = element_text(size=10),
    legend.position = "none"
  )

h_duration=cowplot::plot_grid(NULL,h_duration,NULL,nrow=1,rel_widths = c(1,10,1))

fig1=cowplot::plot_grid(us_map,h_duration,nrow=2,rel_heights = c(2,1))

ggsave("Fig1.pdf",width = 9,height = 8,plot=fig1)


#Figure 2. the histogram and density curve of short- and long-term measurements----------
load(file="Merged_Measurements_201031.RData")
short_term_measurements=lab_data%>%filter(Method!="AT")
long_term_measurements=lab_data%>%filter(Method=="AT")
s_histogram=ggplot()+
  geom_histogram(data=short_term_measurements%>%filter(PCI.L<35,PCI.L>0),aes(x=37*PCI.L,y=..density..,fill="All"),
                 binwidth = 10,color="gray",alpha=0.33,size=0.15)+
  geom_histogram(data=result_data,aes(x=37*Init_Measurement,y=..density..,fill="Paired"),
                 binwidth = 10,color="gray",alpha=0.33,size=0.15)+
  geom_density(data=short_term_measurements%>%filter(PCI.L<35,PCI.L>0),aes(x=37*PCI.L,color="All"),
               size=1.25)+
  geom_density(data=result_data,aes(x=37*Init_Measurement,color="Paired"),
               size=1.25)+
  geom_vline(aes(xintercept=148),size=1.25,color="black",linetype="dashed")+
  scale_fill_manual(NULL,
                    breaks = c("All","Paired"),
                    values = c("#3C3B6E","#B22234"),
                    labels=c("All Short-term Measurements","Paired Short-term Measurements"),
                    guide=guide_legend(direction = "vertical",
                                       title.position = "top",
                                       label.position = "right",
                                       keywidth = unit(0.15, "inch"),
                                       keyheight = unit(0.15,"inch")))+
  scale_color_manual(NULL,
                     breaks = c("All","Paired"),
                     values = c("#3C3B6E","#B22234"),
                     labels=c("All Short-term Measurements","Paired Short-term Measurements"),
                     guide=guide_legend(direction = "vertical",
                                        title.position = "top",
                                        label.position = "right",
                                        keywidth = unit(0.25, "inch"),
                                        keyheight = unit(0.15,"inch")))+
  coord_cartesian(xlim=c(0,1000),ylim = c(0,0.01))+
  xlab(expression('Radon Concentrations (Bq/m'^3*')'))+
  ylab("Probability")+
  theme_bw()+
  theme(
    axis.title = element_text(size=12),
    axis.text = element_text(size=11),
    legend.title = element_text(size=12),
    legend.text = element_text(size=11),
    legend.box.margin =  margin(0.2,0.2,0.2,0.2,"in"),
    legend.position=c(0.6,0.8),
    legend.background = element_rect(color="black",fill="white",size=0.25)
  )
s_histogram


l_histogram=ggplot()+
  geom_histogram(data=long_term_measurements%>%filter(PCI.L<35),aes(x=37*PCI.L,y=..density..,fill="All"),
                 binwidth = 10,color="gray",alpha=0.33,size=0.15)+
  geom_histogram(data=result_data,aes(x=37*Follow_Measurement,y=..density..,fill="Paired"),
                 binwidth = 10,color="gray",alpha=0.33,size=0.15)+
  geom_density(data=long_term_measurements%>%filter(PCI.L<35),aes(x=37*PCI.L,color="All"),
               size=1.25)+
  geom_density(data=result_data,aes(x=37*Follow_Measurement,color="Paired"),
               size=1.25)+
  geom_vline(aes(xintercept=148),size=1.25,color="black",linetype="dashed")+
  scale_fill_manual(NULL,
                     breaks = c("All","Paired"),
                     values = c("#3C3B6E","#B22234"),
                     labels=c("All Long-term Measurements","Paired Long-term Measurements"),
                     guide=guide_legend(direction = "vertical",
                                        title.position = "top",
                                        label.position = "right",
                                        keywidth = unit(0.15, "inch"),
                                        keyheight = unit(0.15,"inch")))+
  scale_color_manual(NULL,
                     breaks = c("All","Paired"),
                     values = c("#3C3B6E","#B22234"),
                     labels=c("All Long-term Measurements","Paired Long-term Measurements"),
                     guide=guide_legend(direction = "vertical",
                                        title.position = "top",
                                        label.position = "right",
                                        keywidth = unit(0.25, "inch"),
                                        keyheight = unit(0.15,"inch")))+
  coord_cartesian(xlim=c(0,1000),ylim = c(0,0.01))+
  xlab(expression('Radon Concentrations (Bq/m'^3*')'))+
  ylab("Probability")+
  theme_bw()+
  theme(
    axis.title = element_text(size=12),
    axis.text = element_text(size=11),
    legend.title = element_text(size=12),
    legend.text = element_text(size=11),
    legend.box.margin =  margin(0.2,0.2,0.2,0.2,"in"),
    legend.box.spacing = unit(0.2,"in"),
    legend.position=c(0.6,0.8),
    legend.background = element_rect(color="black",fill="white",size=0.25)
  )
l_histogram

fig2=cowplot::plot_grid(s_histogram,l_histogram,nrow = 1,labels = c("A","B"),label_x = 0.85,label_y = 0.95)
ggsave("Figure2.pdf",plot=fig2,width=9,height = 6)
#Figure3. The scatter plot as facet of difference days-----------
load("Long_and_Short.RData")
result_data=result_data%>%filter(Init_Measurement<30)
result_data$duration=as.numeric(result_data$Follow_End_Date-result_data$Follow_Start_Date)
result_data=result_data%>%filter(duration>89,duration<366)
result_data=result_data%>%filter(Diff_Days<=180)
result_data=result_data%>%filter(Init_Measurement>0,Follow_Measurement>0)
result_data$log_Init=log(result_data$Init_Measurement)
result_data$log_Follow=log(result_data$Follow_Measurement)
result_data$Short_Month=lubridate::month(result_data$Init_End_Date)
result_data$duration_centered=result_data$duration-200

make_fig3_panels=function(low_m,up_m,add_axis_title=T){
  #low_m=120
  #up_m=180
  title=paste0("Difference from ",low_m," to ",up_m," days")
  p=ggplot(data=result_data%>%filter(Diff_Days>=low_m,Diff_Days<=up_m))+
    geom_point(aes(x=37*Init_Measurement,y=37*Follow_Measurement))+
    geom_abline(aes(slope=1,intercept=0))+
    ggtitle(title)+
    geom_text(x = 100, y = 750,
              label = lm_eqn(df=result_data%>%filter(Diff_Days>=low_m,Diff_Days<=up_m)), parse = TRUE)+
    coord_fixed(ratio = 1,xlim=c(0,1000),ylim=c(0,1000),clip = "on")+
    theme_bw()+
    theme(axis.title = element_text(size = 11),
          axis.text = element_text(size=10),
          plot.title=element_text(size=11,hjust=0.5, vjust=0.5,margin=margin(t=40,b=10)),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  if(add_axis_title==F){
    p=p+xlab(" ")+ylab(" ")
  }else{
    p=p+xlab(expression('Short-term Radon (Bq/m'^3*')'))+
      ylab(expression('Long-term Radon (Bq/m'^3*')'))
  }
  return(p)
  
}
f3pa=make_fig3_panels(low_m = 0,up_m=7,add_axis_title = T)
f3pb=make_fig3_panels(low_m = 7,up_m=14,add_axis_title = T)
f3pc=make_fig3_panels(low_m = 14,up_m=28,add_axis_title = T)
f3pd=make_fig3_panels(low_m = 28,up_m=60,add_axis_title = T)
f3pe=make_fig3_panels(low_m = 60,up_m=120,add_axis_title = T)
f3pf=make_fig3_panels(low_m = 120,up_m=180,add_axis_title = T)
fig3=cowplot::plot_grid(f3pa,f3pb,f3pc,
                   f3pd,f3pe,f3pf,nrow=2,labels = c("A","B","C","D","E","F"))
cowplot::ggsave2("Figure3.pdf",plot=fig3,width = 9,height = 8)
#-----------Figure 2. Two curves (Inactive)------------------
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
