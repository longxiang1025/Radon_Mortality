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
library(stringr)

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
                   list(r2 = formatC((summary(m))$r.squared, digits = 2,format = "f")))
  as.character(as.expression(eq));
}
r2_est<-function(df,n){
  result=list()
  for(i in 1:n){
    set.seed(i+1230)
    r_data=sample(1:nrow(df),nrow(df),replace = T)
    r_data=df[r_data,]
    m<-lm(Follow_Measurement~Init_Measurement+I(Init_Method=="LS")+I(Floor=="Basement")+long_duration_centered+I(Winter>=0.25)+I(Spring>=0.25)+I(Summer>=0.25)+I(Autumn>=0.25),
          data=r_data)
    r=(summary(m))$r.square
    result[[i]]=r
  }
  result=unlist(result)
  return(result)

}
strat_model<-function(d_lb=0,d_ub=7,l_lb=90,l_ub=365,s_lb=2,s_ub=4,n=1000,n_digits=2){
  df=result_data%>%filter(long_duration>=l_lb,long_duration<l_ub,
                          Diff_Days>=d_lb,Diff_Days<d_ub,
                          short_duration>=s_lb,short_duration<=s_ub)
  d_range=paste0("[",d_lb,", ",d_ub,")")
  l_range=paste0("[",l_lb,", ",l_ub,")")
  m=lm(Follow_Measurement~Init_Measurement+I(Init_Method=="LS")+I(Floor=="Basement")+long_duration_centered+I(Winter>=0.25)+I(Spring>=0.25)+I(Summer>=0.25)+I(Autumn>=0.25),
        data=df)
  summ=summary(m)
  r2=summ$r.squared
  
  m_re=r2_est(df=df,
               n=n)
  ci=quantile(m_re,c(0.025,0.975))
  ci=formatC(ci,digits = n_digits,format = "f")
  ci=paste0(ci,collapse=", ")
  ci=paste0("(",ci,")")
  r2_ci=paste0(formatC(r2,digits = n_digits,format="f"),"\r\n",ci)
  
  n=nrow(m$model)
  
  coeffs=summ$coefficients[,1]
  coeff_l_ci=summ$coefficients[,1]-1.96*summ$coefficients[,2]
  coeff_u_ci=summ$coefficients[,1]+1.96*summ$coefficients[,2]
  
  conv_factors=c(1,1,1,1,100,1,1,1,1)
  ci=paste(formatC(coeff_l_ci*conv_factors,digits = n_digits,format = "f"),
            formatC(coeff_u_ci*conv_factors,digits = n_digits,format = "f"),sep = ", ")
  ci=paste0("(",ci,")")
  coeff_ci=paste0(formatC(coeffs*conv_factors,digits = n_digits,format = "f"),"\r\n",ci)
  
  output=cbind.data.frame(d_range,l_range,n,r2_ci,t(coeff_ci[c(2,5:9)]))
  return(output)
}

set_breakpoints=function(n_bpoints=50,width=5,df){
  min_c=min(df$Diff_Days)
  max_c=max(df$Diff_Days)
  df_middle=df%>%filter(Diff_Days>min_c,Diff_Days<max_c)
  b_points=quantile(df_middle$Diff_Days,seq(0,1,1/n_bpoints))
  b_points=as.integer(b_points)
  b_points=c(min_c,b_points,max_c)
  b_points=cbind(b_points[1:(length(b_points)-width)],
             b_points[(width+1):length(b_points)])
  return(b_points)
}

write_cell=function(df,col_name="Follow_Measurement",conversion=1){
  return(paste0(formatC(median(conversion*df[,col_name]),digits = 1,format = "f"),"/r/n(",
                formatC(quantile(conversion*df[,col_name],0.25),digits = 1,format = "f"),", ",
                formatC(quantile(conversion*df[,col_name],0.75),digits = 1,format = "f"),")"))
}

write_summary=function(d_lb=0,d_ub=7,l_lb=90,l_ub=365){
  df=result_data%>%filter(long_duration>=l_lb,long_duration<l_ub,
                          Diff_Days>=d_lb,Diff_Days<d_ub)
  diff_range=paste0("[",d_lb,", ",d_ub,")")
  length_range=paste0("[",l_lb,", ",l_ub,")")
  n=nrow(df)
  long=write_cell(df,col_name = "Follow_Measurement",conversion=37)
  short=write_cell(df,col_name = "Init_Measurement",conversion = 37)
  
  ls=paste0(formatC(100*mean(df$Init_Method=="LS"),digits = 1,format = "f"),"%")
  basement=paste0(formatC(100*mean(df$Floor=="Basement"),digits = 1,format = "f"),"%")
  
  long_duration=write_cell(df,col_name = "long_duration",conversion=1)
  winter=write_cell(df,col_name = "Winter",conversion = 100)
  spring=write_cell(df,col_name = "Spring",conversion = 100)
  summer=write_cell(df,col_name = "Summer",conversion = 100)
  autumn=write_cell(df,col_name = "Autumn",conversion = 100)
  
  re_row=c(diff_range,length_range,n,long,short,ls,basement,long_duration,winter,spring,summer,autumn)
  re_row=as.data.frame(t(re_row))
  return(re_row)
  }

#Descriptive Analysis-------------------
load("Long_and_Short.RData")
result_data=result_data%>%filter(Init_Measurement<30)
result_data$long_duration=as.numeric(result_data$Follow_End_Date-result_data$Follow_Start_Date)
result_data$short_duration=as.numeric(result_data$Init_End_Date-result_data$Init_Start_Date)
result_data=result_data%>%filter(long_duration>89,long_duration<366)
result_data=result_data%>%filter(Diff_Days<=180)
result_data=result_data%>%filter(short_duration>1,short_duration<5)
result_data$Short_Month=lubridate::month(result_data$Init_End_Date)
result_data$long_duration_centered=result_data$long_duration-200

multi_meas_ids=result_data%>%group_by(ID)%>%summarise(n=length(ID))
multi_meas_ids=multi_meas_ids[multi_meas_ids$n>3,]
result_data=result_data%>%filter(!ID%in%multi_meas_ids$ID)

result_season=mapply(FUN = season_prop,
                      start_date=result_data$Follow_Start_Date,
                      end_date=result_data$Follow_End_Date)
result_season=as.data.frame(t(result_season))
result_data=bind_cols(result_data,result_season)

r0=write_summary(d_lb = 0,d_ub = 180,l_lb = 90,l_ub = 360)

r1=write_summary(d_lb = 0,d_ub = 7,l_lb = 90,l_ub = 360)
r1.1=write_summary(d_lb = 0,d_ub = 7,l_lb = 90,l_ub = 120)
r1.2=write_summary(d_lb = 0,d_ub = 7,l_lb = 120,l_ub = 270)
r1.3=write_summary(d_lb = 0,d_ub = 7,l_lb = 270,l_ub = 366)

r2=write_summary(d_lb = 7,d_ub = 30,l_lb = 90,l_ub = 360)
r2.1=write_summary(d_lb = 7,d_ub = 30,l_lb = 90,l_ub = 120)
r2.2=write_summary(d_lb = 7,d_ub = 30,l_lb = 120,l_ub = 270)
r2.3=write_summary(d_lb = 7,d_ub = 30,l_lb = 270,l_ub = 366)

r3=write_summary(d_lb = 30,d_ub = 90,l_lb = 90,l_ub = 360)
r3.1=write_summary(d_lb = 30,d_ub = 90,l_lb = 90,l_ub = 120)
r3.2=write_summary(d_lb = 30,d_ub = 90,l_lb = 120,l_ub = 270)
r3.3=write_summary(d_lb = 30,d_ub = 90,l_lb = 270,l_ub = 366)

r4=write_summary(d_lb = 90,d_ub = 180,l_lb = 90,l_ub = 360)
r4.1=write_summary(d_lb = 90,d_ub = 180,l_lb = 90,l_ub = 120)
r4.2=write_summary(d_lb = 90,d_ub = 180,l_lb = 120,l_ub = 270)
r4.3=write_summary(d_lb = 90,d_ub = 180,l_lb = 270,l_ub = 366)

table1=rbind.data.frame(r0,
                        r1.1,r1.2,r1.3,
                        r2.1,r2.2,r2.3,
                        r3.1,r3.2,r3.3,
                        r4.1,r4.2,r4.3)
write_excel_csv(table1,"Table1.csv")


#Analysis Set 1--------------------
#Stratify the data into four stratifications and fit four models with the same formula
#In order to see whether the R2 changes across different stratifications
##Four stratification-specific models------------------------------
s0=strat_model(d_lb = 0,d_ub = 180,l_lb = 90,l_ub = 366,n_digits = 2)
s1=strat_model(d_lb = 0,d_ub = 7,l_lb = 90,l_ub = 366,n_digits = 2)
s2=strat_model(d_lb = 7,d_ub = 30,l_lb = 90,l_ub = 366,n_digits = 2)
s3=strat_model(d_lb = 30,d_ub = 90,l_lb = 90,l_ub = 366,n_digits = 2)
s4=strat_model(d_lb = 90,d_ub = 180,l_lb = 90,l_ub = 366,n_digits = 2)
##Make Table 2 with the results-------------------------------------
table2=rbind.data.frame(s1,s2,s3,s4,s0)
write_excel_csv(table2,"Table2.csv")
##Stratify the data into ~50 stratification defined by percentiles, and run models-----------
#Determine the breakpoints of each overlaping stratifications
d_breaks=set_breakpoints(n_bpoints = 50,width = 10,df=result_data)
#Use all the data
l_breaks=c(90,366)
a1_table=expand.grid(d=1:(nrow(d_breaks)),
                     l=1:(length(l_breaks)-1))
a1_results=list()
for(i in 1:nrow(a1_table)){
  d=a1_table[i,"d"]
  l=a1_table[i,"l"]
  d_lb=d_breaks[d,1]
  d_ub=d_breaks[d,2]
  l_lb=l_breaks[l]
  l_ub=l_breaks[l+1]
  #We didn't run the bootstrap for thousands of times until it comes to publication.
  s=strat_model(d_lb = d_lb,d_ub = d_ub,l_lb = l_lb,l_ub = l_ub,n=100,n_digits = 3)
  a1_results[[i]]=s
}
a1_results=bind_rows(a1_results)
a1_vis_data=a1_results[,1:4]
#a1_vis_data[,c("xl","xu")]=str_split(str_sub(a1_results$d_range,start=2,end=-2),pattern = ",",simplify = T)
#a1_vis_data$xl=as.numeric(a1_vis_data$xl)
#a1_vis_data$xu=as.numeric(a1_vis_data$xu)
#a1_vis_data$x=(a1_vis_data$xl+a1_vis_data$xu)/2
a1_vis_data$est=as.numeric(substr(a1_results$r2_ci,1,5))
a1_vis_data$lci=as.numeric(substr(a1_results$r2_ci,8,12))
a1_vis_data$uci=as.numeric(substr(a1_results$r2_ci,15,19))

fig4=ggplot(data=a1_vis_data)+
  geom_crossbar(aes(x=d_range,y=est,ymin=lci,ymax=uci),size=0.25,fatten = 5,width=0.85,fill="gray85")+
  labs(x="Temporal Difference between Short- and Long-term Measurement (Days)",
       y=expression('R'^2~'of the Model'))+
  scale_y_continuous(breaks = c(0.0,0.25,0.50,0.75,1.0))+
  coord_cartesian(ylim = c(0,1),clip="off",expand = T)+
  theme_bw()+
  theme(axis.text.x = element_text(size=8,angle = -90,vjust=0.5, hjust=0),
        axis.text.y = element_text(size=8),
        axis.title = element_text(size=9),
        panel.grid.minor = element_blank())

ggsave(file="Figure4.pdf",plot = fig4,width = 6,height = 4,device = cairo_pdf)

#Analysis Set 2------------------------------------
#Further divide each of the four stratifications into three sub-groups based on length of the
#long-term measurements, then refit the model with the same formula to see whether R2 varies 
#across each subgroups, then see whether the trend differs across groups

s1.1=strat_model(d_lb = 0,d_ub = 7,l_lb = 90,l_ub = 120,n_digits = 2)
s1.2=strat_model(d_lb = 0,d_ub = 7,l_lb = 120,l_ub = 270,n_digits = 2)
s1.3=strat_model(d_lb = 0,d_ub = 7,l_lb = 270,l_ub = 366,n_digits = 2)

s2.1=strat_model(d_lb = 7,d_ub = 30,l_lb = 90,l_ub = 120,n_digits = 2)
s2.2=strat_model(d_lb = 7,d_ub = 30,l_lb = 120,l_ub = 270,n_digits = 2)
s2.3=strat_model(d_lb = 7,d_ub = 30,l_lb = 270,l_ub = 366,n_digits = 2)

s3.1=strat_model(d_lb = 30,d_ub = 90,l_lb = 90,l_ub = 120,n_digits = 2)
s3.2=strat_model(d_lb = 30,d_ub = 90,l_lb = 120,l_ub = 270,n_digits = 2)
s3.3=strat_model(d_lb = 30,d_ub = 90,l_lb = 270,l_ub = 366,n_digits = 2)

s4.1=strat_model(d_lb = 90,d_ub = 180,l_lb = 90,l_ub = 120,n_digits = 2)
s4.2=strat_model(d_lb = 90,d_ub = 180,l_lb = 120,l_ub = 270,n_digits = 2)
s4.3=strat_model(d_lb = 90,d_ub = 180,l_lb = 270,l_ub = 366,n_digits = 2)

table3=rbind.data.frame(s1.1,s1.2,s1.3,
                        s2.1,s2.2,s2.3,
                        s3.1,s3.2,s3.3,
                        s4.1,s4.2,s4.3)
write_excel_csv(table3,"Table3.csv")


d_breaks=c(0,7,30,90,180)
l_breaks=c(90,120,270,366)
a2_table=expand.grid(d=1:(length(d_breaks)-1),
                     l=1:(length(l_breaks)-1))
a2_results=list()
for(i in 1:nrow(a2_table)){
  d=a2_table[i,"d"]
  l=a2_table[i,"l"]
  d_lb=d_breaks[d]
  d_ub=d_breaks[d+1]
  l_lb=l_breaks[l]
  l_ub=l_breaks[l+1]
  s=strat_model(d_lb = d_lb,d_ub = d_ub,l_lb = l_lb,l_ub = l_ub,n_digits = 2)
  a2_results[[i]]=s
}
a2_results=bind_rows(a2_results)

a2_vis_data=a2_results[,1:4]
a2_vis_data$est=as.numeric(substr(a2_results$r2_ci,1,4))
a2_vis_data$lci=as.numeric(substr(a2_results$r2_ci,7,10))
a2_vis_data$uci=as.numeric(substr(a2_results$r2_ci,13,16))

ggplot(data=a2_vis_data)+
  geom_crossbar(aes(x=l_range,y=est,ymin=lci,ymax=uci,fill=d_range),position = "dodge",fatten = 1)

d_breaks=set_breakpoints(n_bpoints = 50,width = 10,df=result_data)
l_breaks=c(90,120,270,366)
a3_table=expand.grid(d=1:(nrow(d_breaks)-1),
                     l=1:(length(l_breaks)-1))
a3_results=list()
for(i in 1:nrow(a3_table)){
  d=a3_table[i,"d"]
  l=a3_table[i,"l"]
  d_lb=d_breaks[d,1]
  d_ub=d_breaks[d,2]
  l_lb=l_breaks[l]
  l_ub=l_breaks[l+1]
  s=strat_model(d_lb = d_lb,d_ub = d_ub,l_lb = l_lb,l_ub = l_ub,n_digits = 3,n=1000)
  a3_results[[i]]=s
}
a3_results=bind_rows(a3_results)
a3_vis_data=a3_results[,1:4]
a3_vis_data[,c("xl","xu")]=str_split(str_sub(a3_results$d_range,start=2,end=-2),pattern = ",",simplify = T)
a3_vis_data$xl=as.numeric(a3_vis_data$xl)
a3_vis_data$xu=as.numeric(a3_vis_data$xu)
a3_vis_data$x=(a3_vis_data$xl+a3_vis_data$xu)/2
a3_vis_data$est=as.numeric(substr(a3_results$r2_ci,1,5))
a3_vis_data$lci=as.numeric(substr(a3_results$r2_ci,9,13))
a3_vis_data$uci=as.numeric(substr(a3_results$r2_ci,15,20))

f5=ggplot(data=a3_vis_data)+
  geom_crossbar(aes(x=d_range,y=est,ymin=lci,ymax=uci),size=0.25,fatten = 5,width=0.85,fill="gray85")+
  labs(x="Temporal Difference between Short- and Long-term Measurement (Days)",
       y="Correlation between the Observed and Predicted Long-term Measurements")+
  scale_y_continuous(breaks = c(0.25,0.50,0.75,1.0))+
  coord_cartesian(ylim = c(0,1),clip="on",expand = F)+
  theme_bw()+
  theme(axis.text.x = element_text(size=8,angle = -90,vjust=1, hjust=0),
        axis.text.y = element_text(size=8),
        axis.title = element_text(size=11),
        panel.grid.minor = element_blank())+
  facet_grid(l_range~.)

ggsave("Figure5.pdf",plot=f5,height = 9,width = 6,device = cairo_pdf)

#Figure 1. Locations of Measurements-----------------
load("Long_and_Short.RData")
result_data=result_data%>%filter(Init_Measurement<30)
result_data$long_duration=as.numeric(result_data$Follow_End_Date-result_data$Follow_Start_Date)
result_data$short_duration=as.numeric(result_data$Init_End_Date-result_data$Init_Start_Date)
result_data=result_data%>%filter(long_duration>89,long_duration<366)
result_data=result_data%>%filter(Diff_Days<=180)
result_data=result_data%>%filter(short_duration>1,short_duration<5)
result_data$Short_Month=lubridate::month(result_data$Init_End_Date)
result_data$long_duration_centered=result_data$long_duration-200

multi_meas_ids=result_data%>%group_by(ID)%>%summarise(n=length(ID))
multi_meas_ids=multi_meas_ids[multi_meas_ids$n>3,]
result_data=result_data%>%filter(!ID%in%multi_meas_ids$ID)
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
result_data=result_data%>%mutate(Gap_Category=as.factor((Diff_Days>=7)+(Diff_Days>=28)+(Diff_Days>=90)))%>%
  arrange(desc(-Diff_Days))
colors= c("#a6cee3","#1f78b4","#b2df8a","#33a02c")
us_map=ggplot()+
  geom_sf(data=bound_sf%>%filter(STUSPS%in%c(state.abb[c(1,3:10,12:50)])),fill="white")+
  geom_sf(data=result_data%>%filter(State%in%c(state.abb[c(1,3:10,12:50)]),Gap_Category=="3"),
          aes(color="Cat4"),shape=24,size=1.75,stroke=0.05,fill=NA)+
  geom_sf(data=result_data%>%filter(State%in%c(state.abb[c(1,3:10,12:50)]),Gap_Category=="2"),
          aes(color="Cat3"),shape=24,size=1.75,stroke=0.05,fill=NA)+
  geom_sf(data=result_data%>%filter(State%in%c(state.abb[c(1,3:10,12:50)]),Gap_Category=="1"),
          aes(color="Cat2"),shape=24,size=1.75,stroke=0.05,fill=NA)+
  geom_sf(data=result_data%>%filter(State%in%c(state.abb[c(1,3:10,12:50)]),Gap_Category=="0"),
          aes(color="Cat1"),shape=24,size=1.75,stroke=0.05,fill=NA)+
  scale_color_manual("Temporal difference between \n the long- and short-term measurements",
                     breaks = c("Cat1","Cat2","Cat3","Cat4"),
                     values =colors,
                     labels=c("Cat I","Cat II","Cat III","Cat IV"),
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
  geom_histogram(data=result_data,aes(x=long_duration,y=..density..,fill=Gap_Category),
                 binwidth = 4,color="black",alpha=0.75)+
  coord_cartesian(xlim = c(85,370),ylim = c(0,0.14),clip = "on",expand = F)+
  scale_fill_manual(breaks = c("0","1","2","3"),
                    values =colors)+
  geom_vline(aes(xintercept=c(90,120,270,366)))+
  geom_text(aes(x=105,y=0.13,label="Sub A"))+
  geom_text(aes(x=195,y=0.13,label="Sub B"))+
  geom_text(aes(x=318,y=0.13,label="Sub C"))+
  xlab("Duration of long-term measurements (days)")+
  ylab("Probability")+
  scale_x_continuous(breaks = c(90,120,270,365))+
  theme_bw()+
  theme(
    axis.title = element_text(size=11),
    axis.text = element_text(size=10),
    legend.position = "none"
  )

h_duration=cowplot::plot_grid(NULL,h_duration,NULL,nrow=1,rel_widths = c(1,10,1))

fig1=cowplot::plot_grid(us_map,h_duration,nrow=2,rel_heights = c(2,1))

ggsave("Fig1.pdf",width = 9,height = 8,plot=fig1)


#Figure 2. The scatter plot as facet of difference days-----------
load("Long_and_Short.RData")
result_data=result_data%>%filter(Init_Measurement<30)
result_data$long_duration=as.numeric(result_data$Follow_End_Date-result_data$Follow_Start_Date)
result_data$short_duration=as.numeric(result_data$Init_End_Date-result_data$Init_Start_Date)
result_data=result_data%>%filter(long_duration>89,long_duration<366)
result_data=result_data%>%filter(Diff_Days<=180)
result_data=result_data%>%filter(short_duration>1,short_duration<5)
result_data$Short_Month=lubridate::month(result_data$Init_End_Date)
result_data$long_duration_centered=result_data$long_duration-200

multi_meas_ids=result_data%>%group_by(ID)%>%summarise(n=length(ID))
multi_meas_ids=multi_meas_ids[multi_meas_ids$n>3,]
result_data=result_data%>%filter(!ID%in%multi_meas_ids$ID)

make_fig3_panels=function(d_lb,d_ub,l_lb,l_ub,title=NULL,add_x_axis_title=F,add_y_axis_title=F){
  #low_m=120
  #up_m=180
  df=result_data%>%filter(Diff_Days>=d_lb,Diff_Days<=d_ub,long_duration>=l_lb,long_duration<=l_ub)
  if(is.null(title)){
    title=paste0("Difference from ",low_m," to ",up_m," days")
  }
  p=ggplot(data=df)+
    geom_point(aes(x=37*Init_Measurement,y=37*Follow_Measurement),size=1,alpha=0.45)+
    geom_abline(aes(slope=1,intercept=0))+
    ggtitle(title)+
    geom_text(x=50,y=1000,hjust=0,
              label=paste0("Temporal difference [",d_lb,", ",d_ub,")"),
              fontface="plain",size=3)+
    geom_text(x=50,y=900,hjust=0,
              label=paste0("Long duration [",l_lb,", ",l_ub,")"),
              fontface="plain",size=3)+
    geom_text(x = 50, y = 800,hjust=0,
              label = lm_eqn(df=df), parse = TRUE,size=3)+
    coord_fixed(ratio = 1,xlim=c(0,1000),ylim=c(0,1000),clip = "on")+
    theme_bw()+
    theme(axis.title = element_text(size = 11),
          axis.text = element_text(size=10),
          plot.title=element_text(size=11,hjust=0.5, vjust=0.5,margin=margin(t=40,b=10)),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  if(add_x_axis_title==F){
    p=p+xlab(" ")
  }else{
    p=p+xlab(expression('Short-term Radon (Bq/m'^3*')'))
  }
  if(add_y_axis_title==F){
    p=p+ylab(" ")
  }else{
    p=p+ylab(expression('Long-term Radon (Bq/m'^3*')'))
  }
  return(p)
  
}
f3pa=make_fig3_panels(d_lb = 0,d_ub = 7,l_lb = 90,l_ub = 120,title = "Cat I: Sub A",add_x_axis_title = T,add_y_axis_title = T)
f3pb=make_fig3_panels(d_lb = 0,d_ub = 7,l_lb = 120,l_ub = 270,title = "Cat I: Sub B",add_x_axis_title = T,add_y_axis_title = T)
f3pc=make_fig3_panels(d_lb = 0,d_ub = 7,l_lb = 270,l_ub = 366,title = "Cat I: Sub C",add_x_axis_title = T,add_y_axis_title = T)

f3pd=make_fig3_panels(d_lb = 7,d_ub = 30,l_lb = 90,l_ub = 120,title = "Cat II: Sub A",add_x_axis_title = T,add_y_axis_title = T)
f3pe=make_fig3_panels(d_lb = 7,d_ub = 30,l_lb = 120,l_ub = 270,title = "Cat II: Sub B",add_x_axis_title = T,add_y_axis_title = T)
f3pf=make_fig3_panels(d_lb = 7,d_ub = 30,l_lb = 270,l_ub = 366,title = "Cat II: Sub C",add_x_axis_title = T,add_y_axis_title = T)

f3pg=make_fig3_panels(d_lb = 30,d_ub = 90,l_lb = 90,l_ub = 120,title = "Cat III: Sub A",add_x_axis_title = T,add_y_axis_title = T)
f3ph=make_fig3_panels(d_lb = 30,d_ub = 90,l_lb = 120,l_ub = 270,title = "Cat III: Sub B",add_x_axis_title = T,add_y_axis_title = T)
f3pi=make_fig3_panels(d_lb = 30,d_ub = 90,l_lb = 270,l_ub = 366,title = "Cat III: Sub C",add_x_axis_title = T,add_y_axis_title = T)

f3pj=make_fig3_panels(d_lb = 90,d_ub = 180,l_lb = 90,l_ub = 120,title = "Cat IV: Sub A",add_x_axis_title = T,add_y_axis_title = T)
f3pk=make_fig3_panels(d_lb = 90,d_ub = 180,l_lb = 120,l_ub = 270,title = "Cat IV: Sub B",add_x_axis_title = T,add_y_axis_title = T)
f3pl=make_fig3_panels(d_lb = 90,d_ub = 180,l_lb = 270,l_ub = 366,title = "Cat IV: Sub C",add_x_axis_title = T,add_y_axis_title = T)


fig3=cowplot::plot_grid(f3pa,f3pb,f3pc,
                        f3pd,f3pe,f3pf,
                        f3pg,f3ph,f3pi,
                        f3pj,f3pk,f3pl,
                        nrow=4,labels = c("A","B","C","D","E","F","G","H","I","J","K","L"))
cowplot::ggsave2("Figure3.pdf",plot=fig3,width = 9,height = 16)

#Figure 3. the histogram and density curve of short- and long-term measurements----------
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
ggsave("Figure3.pdf",plot=fig2,width=9,height = 6)
#Supplementary Analysis (Restrict to one-year measurement, Inactive)----------
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
