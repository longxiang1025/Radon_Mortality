library(dplyr)
library(readr)
library(sf)
library(lubridate)
library(cowplot)
library(grid)
library(gridExtra)
library(ggplot2)
library(EnvStats)

ma_lab_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_MA_190628.csv",header=T)
ma_lab_data$TestPostalCode=as.character(ma_lab_data$TestPostalCode)
pa_lab_data<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/HSPH_Export_PA_190628.csv",header=T)
pa_lab_data$TestPostalCode<-as.character(pa_lab_data$TestPostalCode)
load(here::here("Data","GeoData","2015_Shapes.RData"))

lab_data=bind_rows(ma_lab_data,pa_lab_data)
lab_data$ID=paste0(lab_data$Checksum_TestAddress,lab_data$TestPostalCode)

lab_data$StartDate=as.character(lab_data$StartDate)
lab_data$EndDate=as.character(lab_data$EndDate)

lab_data<-lab_data%>%mutate(StartDate=ifelse(StartDate=="00/00/0000",NA,StartDate))
lab_data<-lab_data%>%filter(!is.na(StartDate))
lab_data$StartDate<-as.Date(lab_data$StartDate,"%m/%d/%Y")

lab_data<-lab_data%>%mutate(EndDate=ifelse(EndDate=="00/00/0000",NA,EndDate))
lab_data<-lab_data%>%filter(!is.na(EndDate))
lab_data$EndDate<-as.Date(lab_data$EndDate,"%m/%d/%Y")

lab_data$PCI.L=as.numeric(as.character(lab_data$Result))
lab_data[is.na(lab_data$PCI.L),"PCI.L"]=0

t=lab_data%>%filter(TestState%in%state.abb)%>%filter(Method%in%c("AC","LS"))%>%group_by(TestState)%>%summarise(n=length(DeviceNumber))
t=lab_data%>%filter(TestState%in%state.abb)%>%filter(Method%in%c("AT"))%>%group_by(TestState)%>%summarise(n=length(DeviceNumber))

#Find side-by-side comparisons--------------
side_by_side=lab_data[duplicated(lab_data[,c("ID","StartDate","EndDate","Method","Floor")])|
                        duplicated(lab_data[,c("ID","StartDate","EndDate","Method","Floor")],fromLast = T),]
side_by_side_count=side_by_side%>%group_by(ID,StartDate,EndDate,Method,Floor)%>%summarise(n=length(StartDate))
side_by_side_count=side_by_side_count%>%filter(n<3)
side_by_side=side_by_side_count%>%left_join(side_by_side)
side_by_side$Side=rep(c("A","B"),nrow(side_by_side)/2)

basic_info=side_by_side_count[,c("ID","StartDate","EndDate","Method","Floor")]
side_by_side_data=basic_info%>%left_join(side_by_side[side_by_side$Side=="A",c("ID","StartDate","EndDate","Method","Floor","PCI.L")])
names(side_by_side_data)[length(names(side_by_side_data))]="Measure_A"
side_by_side_data=side_by_side_data%>%left_join(side_by_side[side_by_side$Side=="B",c("ID","StartDate","EndDate","Method","Floor","PCI.L")])
names(side_by_side_data)[length(names(side_by_side_data))]="Measure_B"

t=side_by_side_data%>%filter((Measure_A/2+Measure_B/2)<21)%>%group_by(Method,as.integer(Measure_A/2+Measure_B/2))%>%summarise(c=mean(abs(Measure_A-Measure_B)),n=length(Method))
names(t)=c("Method","Conc","Dif_Abs","n")
ggplot(data=t)+
  geom_point(aes(x=37*Conc,y=37*Dif_Abs,color=Method,size=Method,shape=Method,group=Method))+
  geom_smooth(aes(x=37*Conc,y=37*Dif_Abs,color=Method,group=Method),size=0.5)+
  scale_color_manual("",
                     breaks = c("AC","LS","AT"),
                     values = c("#002868","#002868","#BF0A30"),
                     labels=c("Charcol Canister","Liquid Scintillation","Alpha Tracker"))+
  scale_size_manual("",
                    breaks = c("AC","LS","AT"),
                    values = c(2,2,2),
                    labels=c("Charcol Canister","Liquid Scintillation","Alpha Tracker"))+
  scale_shape_manual("",
                     breaks = c("AC","LS","AT"),
                     values = c(4,0,1),
                     labels=c("Charcol Canister","Liquid Scintillation","Alpha Tracker"))+
  xlab("Average radon concentration (Bq/m3)")+
  ylab("Mean absolute difference in concentration (Bq/m3)")+
  theme_bw()+
  theme(legend.position = c(0.2,0.8),
        legend.background = element_rect(size=1),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12))
  
  
#Find long-term following short-term measurements------------------
measurment_types=lab_data%>%arrange(StartDate)%>%group_by(ID,Floor)%>%summarise(AC=sum("AC"==Method),
                                                     AT=sum("AT"==Method),
                                                     LS=sum("LS"==Method))
measurment_types=measurment_types%>%filter(AT>0&(AC>0|LS>0))
measurment_types=measurment_types%>%filter(AT<3,AC<3,LS<3)
follow_data=measurment_types%>%left_join(lab_data)
follow_data=follow_data%>%arrange(StartDate)%>%group_by(ID,Floor)%>%summarise(start_type=unique(Method)[1],
                                                                  follow_type=unique(Method)[2])
follow_data=follow_data%>%filter(follow_type=="AT")
start_data=follow_data%>%left_join(lab_data,by=c("ID"="ID","Floor"="Floor","start_type"="Method"))

second_data=follow_data%>%left_join(lab_data,by=c("ID"="ID","Floor"="Floor","follow_type"="Method"))
second_data=as.data.frame(second_data)
comparison_data=list()
l=1
for(i in 1:nrow(second_data)){
  ##loop through all follow-up long-term measurements, and find their matching prior short-term measurements
  second_measurements=second_data[i,]
  start_measurements=start_data%>%filter(ID==second_measurements$ID,Floor==second_measurements$Floor)
  start_measurements=start_measurements%>%filter(StartDate<=second_measurements$StartDate)
  
  if(nrow(start_measurements)==0){
    print(paste(i,"no prior short-term test"))
  }else if(nrow(start_measurements)>1){
    start_measurements$Interval=interval(start_measurements$StartDate,
                                         start_measurements$EndDate)
    if(!lubridate::int_overlaps(start_measurements$Interval[1],
                                start_measurements$Interval[2])){
      #select the measurements cloesest before AT sample
      start_measurements=start_measurements[2,]
      diff_days=start_measurements$StartDate-second_measurements$StartDate
      print(paste(i,"two no-overlapping prior records",as.numeric(diff_days)))
    }else{
      ###If the prior screening tests are overlapped,calculate the mean
      temp=mean(start_measurements$PCI.L)
      start_measurements=start_measurements[1,]
      start_measurements$PCI.L=temp
      diff_days=start_measurements$StartDate-second_measurements$StartDate
      print(paste(i,"two side-by-side prior records",as.numeric(diff_days)))
    }
  }else{
    ###If the prior screening tests have no overlap, pick the later one instead
    diff_days=start_measurements$StartDate-second_measurements$StartDate
    print(paste(i,"one prior record",as.numeric(diff_days)))
  }
  result=cbind.data.frame(start_measurements[,c("ID","Floor","start_type","PCI.L")],
                          second_measurements[,c("follow_type","PCI.L")],
                          start_measurements$EndDate-start_measurements$StartDate,
                          second_measurements$EndDate-second_measurements$StartDate,
                          diff_days)
  names(result)=c("ID","Floor","start_type","Start_Measurement","follow_type","Follow_Measurement","Start_Duration","Follow_Duration","Diff_Days")
  comparison_data[[l]]=result
  l=l+1
}

result_data=bind_rows(comparison_data)
result_data$Diff_Days=as.numeric(result_data$Diff_Days)

ggplot(data=result_data%>%filter(Diff_Days>(-30),Start_Measurement>0,Follow_Measurement>0))+
  geom_point(aes(x=log(Start_Measurement),y=log(Follow_Measurement)),fill="#BF0A30",shape=21)+
  geom_smooth(aes(x = log(Start_Measurement),y=log(Follow_Measurement)),formula = y~0+x)+
  geom_abline(aes(intercept=0,slope=1))+
  geom_hline(aes(yintercept=0))+
  geom_vline(aes(xintercept=0))+
  xlab("Log of Short-term Basement Radon Concentration pCi/L")+
  ylab("Log of Follow-up Long-term Basement Radon Concentration pCi/L")+
  theme_bw()+
  theme(axis.title = element_text(size=13),
        axis.text = element_text(size=12))

breaks=seq(0,20,0.2)
ggplot()+
  geom_histogram(data=lab_data%>%filter(Method%in%c("AC","LS")),aes(x=PCI.L,y=..density..,fill="All"),breaks=breaks,color="black",alpha=0.35)+
  geom_histogram(data=result_data%>%filter(Diff_Days<30),aes(x=Start_Measurement,y=..density..,fill="With Follow"),breaks=breaks,color="black",alpha=0.35)+
  geom_density(data=lab_data%>%filter(Method%in%c("AC","LS")),aes(x=PCI.L,y=..density..,color="All"),breaks=breaks,size=1.5)+
  geom_density(data=result_data%>%filter(Diff_Days<30),aes(x=Start_Measurement,y=..density..,color="With Follow"),breaks=breaks,size=1.5)+
  geom_vline(aes(xintercept=4),size=1.5)+
  xlab("Basement Radon Concentration pCi/L")+
  scale_fill_manual("",
                    breaks = c("All","With Follow"),
                    values = c("#002868","#BF0A30"),
                    labels=c("All Short-term Measurements",
                             "Short-term Measurements with a follow-up Long-term Measurements"))+
  scale_color_manual("",
                    breaks = c("All","With Follow"),
                    values = c("#002868","#BF0A30"),
                    labels=c("All Short-term Measurements",
                             "Short-term Measurements with a follow-up Long-term Measurements"))+
  xlim(c(0,20))+
  theme_bw()+
  theme(legend.position = c(0.6,0.8),
        legend.text = element_text(size=13),
        axis.title = element_text(size=13),
        axis.text = element_text(size=12))

t=result_data%>%filter(Diff_Days>(-30))
t%>%group_by(as.integer(as.numeric(Follow_Duration)/50))%>%summarise(c=cor(Start_Measurement,Follow_Measurement),
                                                                      n=length(Start_Measurement))


#Upstairs and basement comparison-----------------------
lab_pairs=lab_data%>%group_by(ID,StartDate,Method,TestPostalCode,TestState)%>%
  summarise(n_base=sum(Floor=="Basement"),
            n_above=sum(Floor%in%c("First","Second")))
lab_pairs=lab_pairs%>%filter(n_base>0,
                             n_above>0,
                             n_base<10)
lab_data=lab_pairs%>%left_join(lab_data)
lab_data=lab_data%>%filter(ID%in%lab_pairs$ID,
                           Floor%in%c("Basement","First","Second"))
lab_records=lab_data%>%group_by(ID,Method,TestPostalCode,TestState,StartDate,Floor=="Basement")%>%summarise(mean_radon=mean(PCI.L),
                                                                            mean_duration=mean(Hours))
names(lab_records)=c("ID","Method","ZIPCode","State","StartDate","Basement","Radon","Duration")
lab_records=bind_cols(lab_records%>%filter(Basement),
                     lab_records%>%filter(!Basement))
lab_records=lab_records[,c(1,2,3,4,5,7,15,16)]
names(lab_records)=c("ID","Method","ZIPCODE","State","StartDate","Basement","Aboveground","Duration")
lab_records$lab="MA"

zip_centroid=st_centroid(st_as_sf(zips))
zip_centroid=cbind.data.frame(zip_centroid$ZIP,st_coordinates(zip_centroid))
names(zip_centroid)=c("ZIPCODE","Longitude","Latitude")
lab_records=lab_records%>%left_join(zip_centroid)

lab_records$Longitude=lab_records$Longitude+runif(nrow(lab_records),-0.05,0.05)
lab_records$Latitude=lab_records$Latitude+runif(nrow(lab_records),-0.05,0.05)
lab_records=lab_records%>%filter(!is.na(Longitude))
coordinates(lab_records)=~Longitude+Latitude
lab_records=st_as_sf(lab_records)
st_crs(lab_records)=st_crs(bound_sf)
#Select states where over 100 measurements exists
States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
         "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")

NE_States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE")
MW_States=c("IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")

lab_records=lab_records%>%filter(State%in%States)
lab_records$Region="Northeast"
lab_records[lab_records$State%in%MW_States,"Region"]="Midwest"

t=lab_records%>%group_by(ID)%>%summarise(n=length(ID))
t=lab_records%>%filter(Method%in%c("AC","LS"),Basement>0,(Aboveground)>0)%>%
  group_by(year(StartDate),Region)%>%
  summarise(m=exp(mean(log(Aboveground/Basement),na.rm=T)),
            s=geoSD(Aboveground/Basement,na.rm=T),
            n=length(ID))

names(t)[1:5]=c("Year","Region","Mean_Ratio","SD","Count")
t$SD=t$SD/sqrt(t$Count)
t$geometry=NULL
t$Count=paste0("(",t$Count,")")
ggplot(data=t%>%filter(Year>2004,Year<2019))+
  geom_path(aes(x=Year,y=Mean_Ratio,color=Region))+
  geom_point(aes(x=Year,y=Mean_Ratio,color=Region,size=Region),position = position_dodge(width=0.3))+
  geom_errorbar(aes(x=Year,ymax=Mean_Ratio+SD,ymin=Mean_Ratio-SD,color=Region,group=Region),position = position_dodge(width = 0.3))+
  #geom_text(aes(x=Year,y=Mean_Ratio,label=Count),vjust=0.05,hjust=0.02)+
  geom_smooth(aes(x=Year,y=Mean_Ratio,color=Region),method = "lm",formula=y ~ splines::bs(x, 3),se=T,position = position_dodge(width=0.3))+
  scale_color_manual(breaks=c("Northeast","Midwest"),
                     values = c("#002868","#BF0A30"))+
  scale_size_manual(breaks=c("Northeast","Midwest"),
                    values = c(3,3))+
  xlim(c(2005,2018))+
  scale_x_continuous(breaks = seq(2005,2018,1))+
  ylim(c(0.3,0.95))+
  ylab("Annual Upstairs/Basement ratios")+
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.position = c(0.15,0.85),
        legend.text = element_text(size=13),
        legend.title = element_blank(),
        legend.key.width = unit(38,units = "points"))
  
library(ggplot2)
library(ggExtra)

p1 <- ggplot(data=lab_records%>%filter(Method%in%c("AC","LS"),Basement<25,Aboveground<25)) +
  geom_point(aes(x=37*Basement,y=37*Aboveground,color=Method,shape=Method,size=Method)) +
  geom_smooth(method = "lm",aes(x=37*Basement,y=37*Aboveground),formula = y~0+x,color="#BF0A30")+
  xlab("Basement Radon Concentration (Bq/m3)")+
  ylab("Upstairs Radon Concentration (Bq/m3)")+
  scale_color_manual("",
                     breaks = c("AC","LS","AT"),
                     values = c("#002868","#002868","#BF0A30"),
                     labels=c("Charcol Canister","Liquid Scintillation","Alpha Tracker"))+
  scale_size_manual("",
                    breaks = c("AC","LS","AT"),
                    values = c(2,2,1),
                    labels=c("Charcol Canister","Liquid Scintillation","Alpha Tracker"))+
  scale_shape_manual("",
                     breaks = c("AC","LS","AT"),
                     values = c(4,1,1),
                     labels=c("Charcol Canister","Liquid Scintillation","Alpha Tracker"))+
  coord_equal()+
  theme_bw()+
  theme(legend.position="none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size=13))+
  facet_grid(~Region)

#p1 <- ggMarginal(p1, type="histogram",color="#002868",size=6,binwidth=15,fill="white")

# g<-lab_records%>%filter(Method=="LS")%>%mutate(ratio=Aboveground/Basement)%>%
#       ggplot()+geom_boxplot(aes(x="",y=ratio),size=0.75,width=0.5)+
#       scale_y_continuous(limits = c(0,1),breaks = seq(0.0,1.0,0.2))+
#       ylab("Ratio Between Upstairs and Basements")+
#       theme_bw()+
#       theme(axis.title.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank(),
#           axis.text.x = element_text(size=12),
#           panel.grid = element_blank(),
#           strip.background = element_blank(),
#           strip.text.x = element_blank())+
#       coord_flip()+
#   facet_grid(~Region)
# 
# p1=plot_grid(p1,g,ncol=1,rel_heights = c(6,1))

p2 <- ggplot(data=lab_records%>%filter(Method=="AT",Basement<25,Aboveground<25)) +
  geom_point(aes(x=37*Basement,y=37*Aboveground),color="#BF0A30",shape=1,size=1) +
  geom_smooth(method = "lm",aes(x=37*Basement,y=37*Aboveground),formula = y~0+x,color="#002868")+
  xlab("Basement Radon Concentration (Bq/m3)")+
  ylab("Upstairs Radon Concentration (Bq/m3)")+
  coord_equal()+
  theme_bw()+
  theme(legend.position="none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size=13))+
  facet_grid(~Region)


# p2 <- ggMarginal(p2, type="histogram",color="#002868",size=6,binwidth=15,fill="white")
# 
# g<-lab_records%>%filter(Method=="AC")%>%mutate(ratio=Aboveground/Basement)%>%
#   ggplot()+geom_boxplot(aes(x="",y=ratio),size=0.75,width=0.5)+
#   scale_y_continuous(limits = c(0,1),breaks = seq(0.0,1.0,0.2))+
#   ylab("Ratio Between Upstairs and Basements")+
#   theme_bw()+
#   theme(axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_text(size=12),
#         panel.grid = element_blank())+
#   coord_flip()
# 
# p2=plot_grid(p2,g,ncol=1,rel_heights = c(6,1))

p3 <- ggplot(data=lab_records%>%filter(Method=="AC",Basement<25,Aboveground<25)) +
  geom_point(aes(x=37*Basement,y=37*Aboveground),color="#BF0A30",shape=1,size=1) +
  geom_smooth(method = "lm",aes(x=37*Basement,y=37*Aboveground),formula = y~0+x,color="#002868")+
  xlab("Basement Radon Concentration (Bq/m3)")+
  ylab("Upstairs Radon Concentration (Bq/m3)")+
  ggtitle("Radon Measurements by Alpha-Tracker")+
  theme_bw()+
  coord_equal()+
  theme(legend.position="none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size=13))+
  facet_grid(~Region)
  

# p3 <- ggMarginal(p3, type="histogram",color="#BF0A30",size=6,fill="white",binwidth=15)
# g<-lab_records%>%filter(Method=="AT")%>%mutate(ratio=Aboveground/Basement)%>%
#   ggplot()+geom_boxplot(aes(x="",y=ratio),size=0.75,width=0.5)+
#   scale_y_continuous(limits = c(0,1),breaks = seq(0.0,1.0,0.2))+
#   ylab("Ratio Between Upstairs and Basements")+
#   theme_bw()+
#   theme(axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_text(size=12),
#         panel.grid = element_blank())+
#   coord_flip()
# 
# p3=plot_grid(p3,g,ncol=1,rel_heights = c(6,1))
# r1=plot_grid(p1,p2,p3,nrow=1)

lab_records$L_Aboveground=log(37*lab_records$Aboveground)
lab_records$L_Basement=log(37*lab_records$Basement)

p4 <- ggplot(data=lab_records%>%filter(Method=="LS",Basement>0,Aboveground>0,
                                       Basement<100,Aboveground<100)) +
  geom_point(aes(x=L_Basement,y=L_Aboveground),color="#002868",shape=3,size=1) +
  coord_cartesian(xlim = c(2, 8),ylim = c(2,8))+
  xlab("Log-transformed Basement Radon")+
  ylab("Log-transfored Upstairs Radon")+
  ggtitle("Radon Measurements by Liquid Scintillator")+
  theme_bw()+
  theme(legend.position="none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        plot.title = element_text(hjust = 0.5))

p4 <- ggMarginal(p4, type="histogram",color="#002868",size=6,binwidth=0.2,fill="white")

p5 <- ggplot(data=lab_records%>%filter(Method=="AC",Basement>0,Aboveground>0,
                                       Basement<100,Aboveground<100)) +
  geom_point(aes(x=L_Basement,y=L_Aboveground),color="#002868",shape=4,size=1) +
  coord_cartesian(xlim = c(2, 8),ylim = c(2,8))+
  xlab("Log-transformed Basement Radon")+
  ylab("Log-transfored Upstairs Radon")+
  ggtitle("Radon Measurements by Liquid Scintillator")+
  theme_bw()+
  theme(legend.position="none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        plot.title = element_text(hjust = 0.5))

p5 <- ggMarginal(p5, type="histogram",color="#002868",size=6,binwidth=0.2,fill="white")

p6 <- ggplot(data=lab_records%>%filter(Method=="AT",Basement>0,Aboveground>0,
                                       Basement<100,Aboveground<100)) +
  geom_point(aes(x=L_Basement,y=L_Aboveground),color="#BF0A30",shape=1,size=1) +
  coord_cartesian(xlim = c(2, 8),ylim = c(2,8))+
  xlab("Log-transformed Basement Radon")+
  ylab("Log-transfored Upstairs Radon")+
  ggtitle("Radon Measurements by Liquid Scintillator")+
  theme_bw()+
  theme(legend.position="none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        plot.title = element_text(hjust = 0.5))

p6 <- ggMarginal(p6, type="histogram",color="#BF0A30",size=6,binwidth=0.2,fill="white")
p6
r2=plot_grid(p4,p5,p6,nrow=1)
p=plot_grid(r1,r2,nrow=2)

lab_records$State=factor(lab_records$State,levels = States)

ggplot()+
  geom_boxplot(data=lab_records%>%filter(Method=="AT")%>%mutate(ratio=Aboveground/Basement),
                        aes(x=State,y=ratio,fill="Long"),ymax=NA,ymin=NA,size=0.5,width=0.3,color="black",position = position_nudge(x=0.15),outlier.shape = NA,alpah=0.35)+
  geom_boxplot(data=lab_records%>%filter(Method!="AT")%>%mutate(ratio=Aboveground/Basement),
               aes(x=State,y=ratio,fill="Short"),ymax=NA,ymin=NA,size=0.5,width=0.3,color="black",position = position_nudge(x=-0.15),outlier.shape = NA,alpha=0.45)+
  scale_y_continuous(limits = c(0,1),breaks = seq(0.0,1.0,0.2))+
  scale_fill_manual(breaks = c("Long","Short"),
                    values = c("#BF0A30","#002868"),
                    labels=c("Long-term Measurement",
                             "Short-term Measurement"))+
  ylab("Ratio Between Upstairs and Basements")+
  theme_bw()+
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=12),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.25),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank())+
  coord_flip()


library(rnaturalearth)
library(rnaturalearthdata)

load(here::here("Data","GeoData","Boundaries.RData"))
load(here::here("Data","GeoData","Counties.RData"))
bound_sf<-st_as_sf(bound)
county_sf<-st_as_sf(county)
bound_sf=st_transform(bound_sf,crs="+proj=utm +zone=16 +datum=WGS84")
st_crs(lab_records)=4326
lab_records=st_transform(lab_records,crs="+proj=utm +zone=16 +datum=WGS84")
ne_zoom=ggplot()+
  geom_sf(data=bound_sf%>%filter(STUSPS%in%States),size=0.25,fill="white")+
  geom_sf(data=st_union(bound_sf%>%filter(STUSPS%in%States[1:11])),fill="gray25")+
          coord_sf(crs ="+proj=utm +zone=17 +datum=WGS84")+
          theme(legend.position = "none",
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_rect(size=1.25,fill = "aliceblue",color="black"),
                panel.grid = element_blank(),
                plot.margin = unit(c(0, 0, 0, 0), "cm"))
ne_zoom=ggplotGrob(ne_zoom)

mw_zoom=ggplot()+
  geom_sf(data=bound_sf%>%filter(STUSPS%in%States),size=0.25,fill="white")+
  geom_sf(data=st_union(bound_sf%>%filter(STUSPS%in%States[12:23])),fill="gray25")+
  coord_sf(crs ="+proj=utm +zone=14 +datum=WGS84")+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(size=1.25,fill = "aliceblue",color="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
mw_zoom=ggplotGrob(mw_zoom)

region_map=ggplot()+
            geom_sf(data=bound_sf,fill="white")+
            geom_sf(data=bound_sf%>%filter(STUSPS%in%States[1:11]),fill="gray90")+
            geom_sf(data=bound_sf%>%filter(STUSPS%in%States[12:23]),fill="gray80")+
            #geom_sf(data=lab_records%>%filter(Method!="AT"),aes(color=Method,size=Method,shape=Method))+
            geom_sf(data=lab_records%>%filter(Method%in%c("AT")),aes(color=Method,size=Method,shape=Method))+
            scale_color_manual("",
                               breaks = c("AC","LS","AT"),
                               values = c("#002868","#002868","#BF0A30"),
                               labels=c("Charcol Canister","Liquid Scintillation","Alpha Tracker"))+
            scale_size_manual("",
                              breaks = c("AC","LS","AT"),
                              values = c(1.5,1.5,2),
                              labels=c("Charcol Canister","Liquid Scintillation","Alpha Tracker"))+
            scale_shape_manual("",
                               breaks = c("AC","LS","AT"),
                               values = c(4,1,1),
                               labels=c("Charcol Canister","Liquid Scintillation","Alpha Tracker"))+
            coord_sf(crs ="+proj=utm +zone=16 +datum=WGS84",
                     xlim = c(st_bbox(lab_records)[1]-10000,st_bbox(lab_records)[3]+10000),
                     ylim = c(st_bbox(lab_records)[2]-30000,st_bbox(lab_records)[4]+100000))+
            theme(legend.position = c(0.8,0.75),
                  legend.background = element_rect(fill="white",color="black",size=0.25),
                  legend.text = element_text(size=11),
                  legend.key = element_rect(color="black",fill="white"),
                  legend.key.width =  unit(2,"lines"),
                  legend.key.height =  unit(1,"lines"),
                  legend.direction = "vertical",
                  legend.box.spacing = unit(5,"lines"),
                  legend.justification="bottom",
                  legend.title = element_blank(),
                  legend.margin = margin(4.5, 4.5, 4.5, 4.5),
                  axis.line = element_line(color="black",size=0.25),
                  panel.background = element_rect(color="black",fill="aliceblue",size=0.5),
                  panel.grid = element_line(size=0.25,color="black",linetype = "dashed"))
            # annotation_custom(grob = us_zoom,
            #    xmin=(st_bbox(lab_records)[3]+st_bbox(lab_records)[1])/2,
            #    xmax=(st_bbox(lab_records)[3]+st_bbox(lab_records)[1])/2 +1200000,
            #    ymin=(st_bbox(lab_records)[4]) -350000,
            #    ymax=(st_bbox(lab_records)[4]+150000))
fig1=plot_grid(region_map,p3,nrow = 2,rel_heights = c(1,1.125))
fig2=plot_grid(region_map,p1,nrow = 2,rel_heights = c(1,1.125))
fig3=plot_grid(region_map,p2,nrow = 2,rel_heights = c(1,1.125))


save_plot(here::here("Figure_1_Resub.pdf"),fig2)

zips_sf=st_as_sf(zips)
zips_sf$color=(zips_sf$POP_SQMI>5000)+(zips_sf$POP_SQMI>1000)+(zips_sf$POP_SQMI>200)
zips_sf$color=as.factor(zips_sf$color)
              
summary(lab_records$Method)
lab_records$L_Aboveground=log(lab_records$Aboveground)
lab_records$L_Basement=log(lab_records$Basement)

long=lab_records%>%filter(Method=="AT")%>%group_by(State)%>%summarise(n=length(Basement),
                                                                 above=37*median(Aboveground,na.rm=T),
                                                                 a_1st=37*quantile(Aboveground,0.25,na.rm=T),
                                                                 a_3rd=37*quantile(Aboveground,0.75,na.rm=T),
                                                                 base=37*median(Basement,na.rm=T),
                                                                 b_1st=37*quantile(Basement,0.25,na.rm=T),
                                                                 b_3rd=37*quantile(Basement,0.75,na.rm=T))
short=lab_records%>%filter(Method!="AT")%>%group_by(State)%>%summarise(n=length(Basement),
                                                                 above=37*median(Aboveground,na.rm=T),
                                                                 a_1st=37*quantile(Aboveground,0.25,na.rm=T),
                                                                 a_3rd=37*quantile(Aboveground,0.75,na.rm=T),
                                                                 base=37*median(Basement,na.rm=T),
                                                                 b_1st=37*quantile(Basement,0.25,na.rm=T),
                                                                 b_3rd=37*quantile(Basement,0.75,na.rm=T))

month_season_table=cbind.data.frame(month=seq(1:12),
                                    season=c("Winter","Spring","Spring","Spring","Summer","Summer",
                                             "Summer","Autumn","Autumn","Autumn","Winter","Winter"))
lab_records$Month=lubridate::month(lab_records$StartDate)
lab_records=lab_records%>%left_join(month_season_table,by=c("Month"="month"))

lab_records%>%filter(Aboveground>0,Basement>0)%>%
  group_by(Method)%>%summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                                  lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                                  ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                                  cor=cor(L_Aboveground,L_Basement),
                                  n=length(season))


season_state_ratio=lab_records%>%filter(Method!="AT",Aboveground>0,Basement>0,
                     State%in%c("MA","NH","IL","MN","PA","WI","MD","NY"))%>%
  group_by(season,State)%>%summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                                                                   lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                                                                   ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                                                                   cor=cor(L_Aboveground,L_Basement),
                                                                   n=length(season))%>%arrange(State)
season_state_ratio$season=factor(season_state_ratio$season,levels = c("Spring","Summer","Autumn","Winter"))
season_state_ratio=season_state_ratio%>%arrange(season)

g_ratio=season_state_ratio%>%group_by(season)%>%summarise(m_ratio=mean(ratio),
                                                          lb=mean(ratio)-1.96*sd(ratio),
                                                          ub=mean(ratio)+1.96*sd(ratio))

large_states=c("MA","NH","NY","PA","WI","IL","MD","MN")
st_colors=c("#1f78b4","#a6cee3","#b2df8a","#33a02c",
            "#fb9a99","#e31a1c","#fdbf6f","#ff7f00")

p=ggplot()+
  geom_boxplot(data=season_state_ratio,aes(x=season,y=ratio),position = position_nudge(x=0.1),
               width=0.25,alpha=0.7,size=1.5)+
  scale_fill_manual(breaks=large_states,
                     values=st_colors)+
  scale_color_manual(breaks = large_states,
                     values = st_colors)

for(i in 1:8){
  p=p+geom_errorbar(data=season_state_ratio%>%filter(State==large_states[i]),
                  aes(x=season,ymin=lb,ymax=ub,color=State),
                  position = position_nudge(x=0.025*(i-1)),
                  width=0.05)+
    geom_path(data=season_state_ratio%>%filter(State==large_states[i]),
              aes(x=season,y=ratio,color=State,group=i),size=0.25,position = position_nudge(x=0.025*(i-1)))+
    geom_point(data=season_state_ratio%>%filter(State==large_states[i]),
               aes(x=season,y=ratio,fill=State),size=3,shape=21,position = position_nudge(x=0.025*(i-1)))
}

p=p+
  scale_y_continuous(breaks = seq(0.0,1.2,0.1),
                     limits = c(0,1.2))+
  theme_bw()+
  ylab("Ratio Upstair and Basement Radon")+
  theme(
  legend.direction = "horizontal",
  legend.title = element_blank(),
  legend.position = c(0.3,0.85),
  legend.key.width = unit(3,"lines"),
  legend.background = element_rect(color="gray50"),
  legend.text = element_text(12),
  axis.title.x = element_blank(),
  axis.text.x = element_text(size=12),
  axis.text.y = element_text(size=12),
  axis.title.y = element_text(14)
)


tank=list()
for(i in 1:8){
  short_base_measurement=lab_records[lab_records$State==large_states[i]&lab_records$Method!="AT",]$Basement
  short_above_measurement=lab_records[lab_records$State==large_states[i]&lab_records$Method!="AT",]$Aboveground
  long_base_measurement=lab_records[lab_records$State==large_states[i]&lab_records$Method=="AT",]$Basement
  long_above_measurement=lab_records[lab_records$State==large_states[i]&lab_records$Method=="AT",]$Aboveground

  short_basement_quantiles=quantile(short_base_measurement,seq(0.05,0.95,0.01))
  short_above_quantiles=quantile(short_above_measurement,seq(0.05,0.95,0.01))
  long_basement_quantiles=quantile(long_base_measurement,seq(0.05,0.95,0.01))
  long_above_quantiles=quantile(long_above_measurement,seq(0.05,0.95,0.01))
  
  quantiles=cbind.data.frame(seq(5,95),
                             short_basement_quantiles,
                             short_above_quantiles,
                             long_basement_quantiles,
                             long_above_quantiles)
  names(quantiles)=c("tiles","short_basement","short_above","long_basement","long_above")
  quantiles$short_ratio=quantiles$short_above/quantiles$short_basement
  quantiles$long_ratio=quantiles$long_above/quantiles$long_basement
  
  g_short=ggplot(data=quantiles)+
    geom_path(aes(x=tiles,y=37*short_basement,color="base_short",linetype="base_short"),size=0.5)+
    geom_point(aes(x=tiles,y=37*short_basement,color="base_short",shape="base_short",size="base_short"))+
    geom_path(aes(x=tiles,y=37*short_above,color="above_short",linetype="above_short"),size=0.5)+
    geom_point(aes(x=tiles,y=37*short_above,color="above_short",shape="above_short",size="above_short"))+
    geom_path(data=quantiles[quantiles$short_above>0,],aes(x=tiles,y=925*short_ratio,color="sm_short",linetype="sm_short"),se=F,size=1)+
    geom_point(data=quantiles[quantiles$short_above>0,],aes(x=tiles,y=925*short_ratio,color="sm_short",shape="sm_short",size="sm_short"))+
    geom_hline(aes(yintercept= 925*median(short_above_measurement/short_base_measurement,na.rm=T)),size=0.5,linetype="longdash",color="black")+
    scale_color_manual("Legend",breaks = c("base_short","above_short","sm_short"),
                       values = c("#B22234","#B22234","black"),
                       labels=c("Short-term Basement Radon Measurement",
                                "Short-term Upstairs Radon Measurement",
                                "Correction Factor Of Short-term Measurement"))+
    scale_linetype_manual("Legend",breaks = c("base_short","above_short","sm_short"),
                          values = c("solid","solid","solid"),
                          labels=c("Short-term Basement Radon Measurement",
                                   "Short-term Upstairs Radon Measurement",
                                   "Correction Factor Of Short-term Measurement"))+
    scale_shape_manual("Legend",breaks = c("base_short","above_short","sm_short"),
                       values = c(1,2,0),
                       labels=c("Short-term Basement Radon Measurement",
                                "Short-term Upstairs Radon Measurement",
                                "Correction Factor Of Short-term Measurement"))+
    scale_size_manual("Legend",breaks=c("base_short","above_short","sm_short"),
                      values = c(2,2,2),
                      labels=c("Short-term Basement Radon Measurement",
                               "Short-term Upstairs Radon Measurement",
                               "Correction Factor Of Short-term Measurement"))+
    scale_y_continuous("Radon Measurements",
                       sec.axis = sec_axis(trans = ~./925,name=" "),
                       limits = c(0,925))+
    scale_x_continuous("Cummulative Probability %",
                       breaks = seq(10,90,10))+
    theme_bw()+
    theme(
      legend.title = element_blank(),
      legend.key.width = unit(0.6,"in"),
      legend.key.height = unit(0.2,"in"),
      legend.box.spacing = unit(0.15,"in"),
      legend.background = element_rect(fill="white",color="black"),
      legend.box.background = element_rect(fill="white"),
      legend.position = c(0.4,0.835),
      legend.text = element_text(size=10),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=12),
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
  
  g_long=ggplot(data=quantiles)+
    geom_path(aes(x=tiles,y=37*long_basement,color="base_long",linetype="base_long"),size=0.5)+
    geom_point(aes(x=tiles,y=37*long_basement,color="base_long",shape="base_long",size="base_long"))+
    geom_path(aes(x=tiles,y=37*long_above,color="above_long",linetype="above_long"),size=0.5)+
    geom_point(aes(x=tiles,y=37*long_above,color="above_long",shape="above_long",size="above_long"))+
    geom_path(data=quantiles[quantiles$long_above>0,],aes(x=tiles,y=925*long_ratio,color="sm_long",linetype="sm_long"),se=F,size=1)+
    geom_point(data=quantiles[quantiles$long_above>0,],aes(x=tiles,y=925*long_ratio,color="sm_long",shape="sm_long",size="sm_long"))+
    geom_hline(aes(yintercept= 925*median(long_above_measurement/long_base_measurement,na.rm=T)),size=0.5,linetype="longdash",color="black")+
    scale_color_manual("Legend",breaks = c("base_long","above_long","sm_long"),
                       values = c("#3C3B6E","#3C3B6E","black"),
                       labels=c("Long-term Basement Radon Measurement",
                                "Long-term Upstairs Radon Measurement",
                                "Correction Factor Of Long-term Measurement"))+
    scale_linetype_manual("Legend",breaks = c("base_long","above_long","sm_long"),
                          values = c("solid","solid","solid"),
                          labels=c("Long-term Basement Radon Measurement",
                                   "Long-term Upstairs Radon Measurement",
                                   "Correction Factor Of Long-term Measurement"))+
    scale_shape_manual("Legend",breaks = c("base_long","above_long","sm_long"),
                       values = c(1,2,0),
                       labels=c("Long-term Basement Radon Measurement",
                                "Long-term Upstairs Radon Measurement",
                                "Correction Factor Of Long-term Measurement"))+
    scale_size_manual("Legend",breaks=c("base_long","above_long","sm_long"),
                      values = c(2,2,2),
                      labels=c("Long-term Basement Radon Measurement",
                               "Long-term Upstairs Radon Measurement",
                               "Correction Factor Of Long-term Measurement"))+
    scale_y_continuous(" ",
                       sec.axis = sec_axis(trans = ~./925,name="Correction Factor"),
                       limits = c(0,925))+
    scale_x_continuous("Cummulative Probability %",
                       breaks = seq(10,90,10))+
    theme_bw()+
    theme(
      legend.title = element_blank(),
      legend.key.width = unit(0.6,"in"),
      legend.key.height = unit(0.2,"in"),
      legend.box.spacing = unit(0.15,"in"),
      legend.background = element_rect(fill="white",color="black"),
      legend.box.background = element_rect(fill="white"),
      legend.position = c(0.4,0.835),
      legend.text = element_text(size=10),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size=12),
      axis.text.y.left = element_blank(),
      axis.ticks.y.left = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
  
  plot=cowplot::plot_grid(g_short,g_long,ncol=2)
  #x.grob <- textGrob("Cumulative Probability %", 
  #                   gp=gpar(fontface="bold", col="black", fontsize=14))
  title<-textGrob(large_states[i],
                  gp=gpar(fontface="bold", col="black", fontsize=14))
  tank[[i]]=grid.arrange(arrangeGrob(plot,top = title))
}

g<-plot_grid(tank[[1]],tank[[5]],ncol=1)
x.grob <- textGrob("Cumulative Probability %", 
                   gp=gpar(fontface="bold", col="black", fontsize=14))
y.grob_left=textGrob("Radon Measurement Bq/m3", 
                     gp=gpar(fontface="bold", col="black", fontsize=14),
                     rot=90)
y.grob_right=textGrob("Ration Between Upstairs and Basement Radon",
                      gp=gpar(fontface="bold", col="black", fontsize=14),
                      rot=270)

g=grid.arrange(arrangeGrob(g,bottom = x.grob,left = y.grob_left,right=y.grob_right))

short_summary=lab_records%>%filter(Method!="AT",Aboveground>0,L_Basement>0)%>%
                            group_by(State)%>%
                            summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                                     lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                                     ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                                     cor=cor(L_Aboveground,L_Basement),
                                     n=length(Method))
long_summary=lab_records%>%filter(Method=="AT",Aboveground>0,L_Basement>0)%>%
  group_by(State)%>%
  summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
            lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
            ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
            cor=cor(L_Aboveground,L_Basement),
            n=length(Method))

table2=cbind.data.frame(short_summary,long_summary)
state_slopes=table2[,c(1,2,6,9,13)]
names(state_slopes)=c("State","Short_Ratio","Short_Count","Long_Ratio","Long_Count")
state_slopes=bound_sf%>%left_join(state_slopes,by=c("STUSPS"="State"))
state_slopes=state_slopes%>%filter(STUSPS%in%States)

long_few_list=state_slopes[state_slopes$Long_Count<65,"NAME"]
long_few_list$geometry=NULL

short_few_list=state_slopes[state_slopes$Short_Count<65,"NAME"]
short_few_list$geometry=NULL

pattern <- function(x, size, pattern) {
  ex = list(
    horizontal = c(1, 2),
    vertical = c(1, 4),
    left2right = c(2, 4),
    right2left = c(1, 3)
  )
  fillgrid = st_make_grid(x, cellsize = size)
  endsf = lapply(1:length(fillgrid), function(j)
    sf::st_linestring(sf::st_coordinates(fillgrid[j])[ex[[pattern]], 1:2]))
  endsf = sf::st_sfc(endsf, crs = sf::st_crs(x))
  endsf = sf::st_intersection(endsf, x)
  endsf = endsf[sf::st_geometry_type(endsf)
                %in% c("LINESTRING", "MULTILINESTRING")]
  endsf = sf::st_line_merge(sf::st_union(endsf))
  return(endsf)
}

long_pattern = 
  pattern(bound_sf%>%filter(NAME%in%long_few_list$NAME), 25000, "left2right")

short_pattern=
  pattern(bound_sf%>%filter(NAME%in%short_few_list$NAME), 25000, "left2right")

long_map=ggplot()+
          geom_sf(data=bound_sf,fill="gray90")+
          geom_sf(data=state_slopes,aes(fill=Long_Ratio))+
          geom_sf(data=st_union(bound_sf%>%filter(STUSPS%in%States)),fill=NA,size=1.25,color="black")+
          geom_sf(data=long_pattern,size=0.5,color="black")+
          scale_fill_gradient2(high = "#BF0A30",low="#002868",mid = "white",midpoint = 0.55,
                               limits=c(0.35,0.75))+
          coord_sf(crs ="+proj=utm +zone=16 +datum=WGS84",
                   xlim = c(st_bbox(state_slopes)[1]-10000,st_bbox(state_slopes)[3]+10000),
                   ylim = c(st_bbox(state_slopes)[2]-30000,st_bbox(state_slopes)[4]+100000))+
  theme(legend.position = "right",
        legend.background = element_rect(fill="white",color="black",size=0.25),
        legend.text = element_text(size=11),
        legend.key = element_rect(color="black",fill="white"),
        legend.key.width =  unit(2,"lines"),
        legend.key.height =  unit(1,"lines"),
        legend.direction = "vertical",
        legend.box.spacing = unit(5,"lines"),
        legend.justification="bottom",
        legend.margin = margin(4.5, 4.5, 4.5, 4.5),
        axis.line = element_line(color="black",size=0.25),
        panel.background = element_rect(color="black",fill="white",size=0.5),
        panel.grid = element_line(size=0.25,color="black",linetype = "dashed"))+
          guides(fill = guide_colourbar(barwidth = 1, barheight = 12))

short_map=ggplot()+
  geom_sf(data=bound_sf,fill="gray90")+
  geom_sf(data=state_slopes,aes(fill=Short_Ratio))+
  geom_sf(data=st_union(bound_sf%>%filter(STUSPS%in%States)),fill=NA,size=1.25,color="black")+
  geom_sf(data=short_pattern,size=0.5,color="black")+
  scale_fill_gradient2(high = "#BF0A30",low="#002868",mid = "white",midpoint = 0.55,
                       limits=c(0.35,0.75))+
  coord_sf(crs ="+proj=utm +zone=16 +datum=WGS84",
           xlim = c(st_bbox(state_slopes)[1]-10000,st_bbox(state_slopes)[3]+10000),
           ylim = c(st_bbox(state_slopes)[2]-30000,st_bbox(state_slopes)[4]+100000))+
  theme(legend.position = "right",
        legend.background = element_rect(fill="white",color="black",size=0.25),
        legend.text = element_text(size=11),
        legend.key = element_rect(color="black",fill="white"),
        legend.key.width =  unit(2,"lines"),
        legend.key.height =  unit(1,"lines"),
        legend.direction = "vertical",
        legend.box.spacing = unit(5,"lines"),
        legend.justification="bottom",
        legend.margin = margin(4.5, 4.5, 4.5, 4.5),
        axis.line = element_line(color="black",size=0.25),
        panel.background = element_rect(color="black",fill="white",size=0.5),
        panel.grid = element_line(size=0.25,color="black",linetype = "dashed"))+
  guides(fill = guide_colourbar(barwidth = 1, barheight = 12))

plot_grid(short_map,long_map,ncol=1)

#Add secondary information to do subgroup analysis
zips=zips@data[,c("ZIP","POP_SQMI")]
lab_records=lab_records%>%left_join(zips,by=c("ZIPCODE"="ZIP"))
pop_result=lab_records%>%filter(Method=="AT",Basement>0,Aboveground>0)%>%
                  group_by(POP_SQMI>5000,POP_SQMI>1000,POP_SQMI>200)%>%
                  summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                            lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                            ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                            cor=cor(L_Aboveground,L_Basement),
                            n=length(Method))
library(raster)
rn_potential=shapefile(here::here("Data","USGS_Rn","usagrp_polygon.shp"))
rn_potential=st_as_sf(rn_potential)
rn_potential=st_transform(rn_potential,st_crs(lab_records))

lab_records=st_join(lab_records,rn_potential)
rn_results=lab_records%>%filter(Method=="AT",Basement>0,Aboveground>0)%>%
              group_by(GRP)%>%
              summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                        lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                        ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                        cor=cor(L_Aboveground,L_Basement),
                        n=length(Method))

surf=shapefile(here::here("Data","USGS_DS_425_SHAPES","Surficial_materials.shp"))
surf=st_as_sf(surf)
surf=st_transform(surf,st_crs(lab_records))

lab_records=st_join(lab_records,surf)
surf_provinces=lab_records%>%filter(Method=="AT",Basement>0,Aboveground>0)%>%
  group_by(UNIT_NAME)%>%summarise(n=length(Method))
surf_provinces=surf_provinces%>%filter(n>97)

surf_results=lab_records%>%filter(Method=="AT",Basement>0,Aboveground>0,UNIT_NAME%in%surf_provinces$UNIT_NAME)%>%
                            group_by(UNIT_NAME)%>%
                            summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                                      lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                                      ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                                      cor=cor(L_Aboveground,L_Basement),
                                      n=length(Method))
surf_results=surf_results[,c("UNIT_NAME","ratio","lb","ub","cor","n")]
surf_results$geometry=NULL

surf=surf%>%left_join(long_results[,c("UNIT_NAME","ratio")])
surf_colors=long_results[,c("UNIT_NAME","ratio")]
surf_colors=surf_colors%>%arrange(ratio)
surf_colors$colors=terrain.colors(n=18)[1:14]
surf_colors$labels=paste0("(",formatC(surf_colors$ratio,digits = 2),")",surf_colors$UNIT_NAME)

library(stringr)
surf_colors$labels=str_wrap(surf_colors$labels,width = 40)

ggplot(data=surf%>%filter(UNIT_NAME%in%surf_provinces$UNIT_NAME))+
  geom_sf(data=bound_sf,fill="gray80")+
  geom_sf(aes(fill=UNIT_NAME),size=0.01,color="black")+
  geom_sf(data=bound_sf,fill=NA)+
  geom_sf(data=bound_sf%>%filter(!STUSPS%in%States),fill="gray80")+
  coord_sf(crs ="+proj=utm +zone=16 +datum=WGS84",
                                    xlim = c(st_bbox(state_slopes)[1],st_bbox(state_slopes)[3]),
                                    ylim = c(st_bbox(state_slopes)[2],st_bbox(state_slopes)[4]))+
  scale_fill_manual(breaks = surf_colors$UNIT_NAME,
                    values = surf_colors$colors,
                    labels=surf_colors$labels,
                    guide = guide_legend(ncol = 1))+
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill="white",color="black",size=0.25),
        legend.text = element_text(size=10),
        legend.key = element_rect(color="black",fill="white"),
        legend.key.width =  unit(1,"cm"),
        legend.key.height =  unit(0.85,"cm"),
        legend.direction = "vertical",
        legend.box.spacing = unit(5,"lines"),
        legend.position="right",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        axis.line = element_line(color="black",size=0.25),
        panel.background = element_rect(color="black",fill="white",size=0.5),
        panel.grid = element_line(size=0.25,color="black",linetype = "dashed"))

library(raster)
climate_Zone<-raster(here::here("Data","Beck_KG_V1","Beck_KG_V1_present_0p0083.tif"))
bound.sp<-spTransform(bound,CRSobj = crs(climate_Zone))
bound.sp<-bound.sp[bound.sp$STUSPS%in%States,]
climate_Zone<-crop(climate_Zone,bound.sp)

lab_records_sp=st_transform(lab_records,crs = crs(climate_Zone))
lab_records$climate_zone=extract(climate_Zone,lab_records_sp)
climate_results=lab_records%>%filter(Method=="AT",Basement>0,Aboveground>0,climate_zone%in%c(25,26,14))%>%
                              group_by(climate_zone)%>%
                              summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                                        lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                                        ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                                        cor=cor(L_Aboveground,L_Basement),
                                        n=length(Method))

climate_Zone=projectRaster(climate_Zone,crs="+proj=utm +zone=16 +datum=WGS84")
climate_Zone=mask(climate_Zone,as_Spatial(st_transform(bound_sf,crs="+proj=utm +zone=16 +datum=WGS84")))
climate_raster=cbind.data.frame(coordinates(climate_Zone),values(climate_Zone$Beck_KG_V1_present_0p0083))
names(climate_raster)=c("x","y","zone")
climate_raster$zone=as.factor(climate_raster$zone)
climate_raster=climate_raster%>%filter(zone%in%c("25","26","14"))
climate_map=ggplot()+
              geom_tile(data=climate_raster,aes(x=x,y=y,fill=zone))+
              scale_fill_manual(breaks = c("26","25","14"),
                                values = c("#3182bd","#9ecae1","#deebf7"),
                                na.value="gray80")+
              geom_sf(data=bound_sf,fill=NA)+
              geom_sf(data=bound_sf%>%filter(!STUSPS%in%States),fill="gray80")+
              coord_sf(crs ="+proj=utm +zone=16 +datum=WGS84",expand = F,
                       xlim = c(st_bbox(state_slopes)[1],st_bbox(state_slopes)[3]),
                       ylim = c(st_bbox(state_slopes)[2],st_bbox(state_slopes)[4]))+
              theme_bw()+
              theme(axis.title = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    legend.position = "none",
                    panel.background = element_rect(fill = "white"),
                    panel.grid = element_blank())

climate_labels=c("(Dfb)Cold, no dry season, warm summer",
                 "(Dfa)Cold, no dry season, hot summer",
                 "(Cfa)Temperate, no dry season, hot summer")
climate_labels=str_wrap(climate_labels,width = 30)
library(ggstance)
climate_bar=ggplot(data=climate_results)+
              geom_crossbarh(aes(y=as.factor(climate_zone),
                                 x=ratio,
                                 xmin=lb,
                                 xmax=ub,
                                 fill=as.factor(climate_zone)),
                             fatten = 1,
                             width=0.2,
                             color="black")+
              scale_y_discrete(position = "right",
                               breaks = c("26","25","14"),
                               labels=climate_labels)+
              scale_x_continuous(limits = c(0.38,0.8),
                                 breaks = seq(0.4,0.8,0.05))+
              scale_fill_manual(breaks = c("26","25","14"),
                                 values = c("#3182bd","#9ecae1","#deebf7"),
                                 na.value="gray80")+
              coord_fixed(ratio = 0.2,ylim=c(0.5,3.5),expand = F)+
              theme_bw()+
              theme(axis.title = element_blank(),
                    legend.position = "none",
                    axis.text.y = element_text(size=12),
                    axis.text.x = element_blank(),
                    aspect.ratio = 1/2)

climate_row=plot_grid(climate_map,climate_bar,nrow = 1,rel_widths = c(1,3))

rn_map=ggplot()+
  geom_sf(data=rn_potential,aes(fill=as.factor(GRP)),lwd=0,size=0,color=NA)+
  scale_fill_manual(breaks = c("3","2","1"),
                    values = c("#fee8c8","#fdbb84","#e34a33"),
                    na.value="gray80")+
  geom_sf(data=bound_sf,fill=NA)+
  geom_sf(data=bound_sf%>%filter(!STUSPS%in%States),fill="gray80")+
  coord_sf(crs ="+proj=utm +zone=16 +datum=WGS84",expand = F,
           xlim = c(st_bbox(state_slopes)[1],st_bbox(state_slopes)[3]),
           ylim = c(st_bbox(state_slopes)[2],st_bbox(state_slopes)[4]))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank())
rn_labels=c("High Geological Radon Potential (>4 pCi/L)",
            "Medium Geological Radon Potential (>2 pCi/L)",
            "Low Geological Radon Potential (<2 pCi/L)")
rn_labels=str_wrap(rn_labels,width = 30)

rn_bar=ggplot(data=rn_results%>%filter(!is.na(GRP)))+
          geom_crossbarh(aes(y=as.factor(GRP),
                             x=ratio,
                             xmin=lb,
                             xmax=ub,
                             fill=as.factor(GRP)),
                         fatten = 1,
                         width=0.2,
                         color="black")+
          scale_y_discrete(position = "right",
                           breaks = c("1","2","3"),
                           labels=rn_labels)+
          scale_x_continuous(limits = c(0.38,0.8),
                             breaks = seq(0.4,0.8,0.05))+
          scale_fill_manual(breaks = c("3","2","1"),
                            values = c("#fee8c8","#fdbb84","#e34a33"),
                            na.value="gray80")+
          coord_fixed(ratio = 0.2,expand = F,ylim=c(0.5,3.5))+
          theme_bw()+
          theme(axis.title = element_blank(),
                legend.position = "none",
                axis.text.y = element_text(size=12),
                axis.text.x = element_blank(),
                aspect.ratio = 1/2)

rn_row=plot_grid(rn_map,rn_bar,nrow = 1,rel_widths = c(1,3))

surf_results=lab_records%>%filter(Method=="AT",Basement>0,Aboveground>0,UNIT_NAME%in%surf_provinces$UNIT_NAME)%>%
  group_by(UNIT_CODE,UNIT_NAME)%>%
  summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
            lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
            ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
            cor=cor(L_Aboveground,L_Basement),
            n=length(Method))
surf_results=surf_results[,c("UNIT_NAME","UNIT_CODE","ratio","lb","ub","cor","n")]
surf_breaks=c("411","412","421","422","431","451")
surf_labels=c("Glacial till sediments, mostly clayey, thin",
              "Glacial till sediments, mostly clayey, thick",
              "Glacial till sediments, mostly silty, thin",
              "Glacial till sediments, mostly silty, thick",
              "Glacial till sediments, mostly sandy, thin",
              "Glaciofluvial ice-contact sediments, mostly sand and gravel, thin")
surf_labels=str_wrap(surf_labels,width = 30)
surf_colors=c("#01665e","#5ab4ac","#c7eae5","#f6e8c3","#d8b365","#8c510a")
surf_map=ggplot()+
  geom_sf(data=surf%>%filter(UNIT_CODE%in%surf_breaks),
          aes(fill=as.factor(UNIT_CODE)),lwd=0,size=0,color=NA)+
  scale_fill_manual(breaks = surf_breaks,
                    values = surf_colors,
                    na.value="gray80")+
  geom_sf(data=bound_sf,fill=NA)+
  geom_sf(data=bound_sf%>%filter(!STUSPS%in%States),fill="gray80")+
  coord_sf(crs ="+proj=utm +zone=16 +datum=WGS84",expand = F,
           xlim = c(st_bbox(state_slopes)[1],st_bbox(state_slopes)[3]),
           ylim = c(st_bbox(state_slopes)[2],st_bbox(state_slopes)[4]))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank())

surf_bars=ggplot(data=surf_results%>%filter(UNIT_CODE%in%surf_breaks))+
  geom_crossbarh(aes(y=as.factor(UNIT_CODE),
                     x=ratio,
                     xmin=lb,
                     xmax=ub,
                     fill=as.factor(UNIT_CODE)),
                 fatten = 1,
                 width=0.2,
                 color="black")+
  scale_y_discrete(position = "right",
                   breaks = surf_breaks,
                   labels= surf_labels)+
  scale_x_continuous(limits = c(0.38,0.8),
                     breaks = seq(0.4,0.8,0.05))+
  scale_fill_manual(breaks = surf_breaks,
                    values = surf_colors,
                    na.value="gray80")+
  coord_fixed(ratio = 0.2,ylim=c(0.5,6.5),expand = F)+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(size=12),
        axis.text.x = element_blank(),
        aspect.ratio = 1/1)

pop_dens_map=ggplot()+
  geom_sf(data=zips_sf,aes(fill=color),lwd = 0,color=NA,size=0)+
  geom_sf(data=bound_sf%>%filter(!STUSPS%in%States),fill="gray80")+
  geom_sf(data=bound_sf,fill=NA)+
  scale_fill_manual("Pop Density",
                    breaks = c("0","1","2","3"),
                    values = c("#f2f0f7","#cbc9e2","#9e9ac8","#6a51a3"),
                    labels=c("<200","200-1000","1000-5000",">5000"))+
  coord_sf(crs ="+proj=utm +zone=16 +datum=WGS84",expand = F,
           xlim = c(st_bbox(state_slopes)[1],st_bbox(state_slopes)[3]),
           ylim = c(st_bbox(state_slopes)[2],st_bbox(state_slopes)[4]))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank())

pop_labels=c("(Rural) Population Density <200 people/sq mile",
             "(Outskirt) Population Density <1000 people/sq mile",
             "(Suburban) Population Density <5000 people/sq mile",
             "(Urban) Population Density >5000 people/sq mile")
pop_labels=str_wrap(pop_labels,width = 30)
pop_result$Class=as.factor(pop_result$`POP_SQMI > 200`+
                          pop_result$`POP_SQMI > 1000`+
                          pop_result$`POP_SQMI > 5000`)
pop_bars=ggplot(data=pop_result)+
  geom_crossbarh(aes(y=Class,
                     x=ratio,
                     xmin=lb,
                     xmax=ub,
                     fill=Class),
                 fatten = 1,
                 width=0.2,
                 color="black")+
  scale_y_discrete(position = "right",
                   breaks = c("0","1","2","3"),
                   labels= pop_labels)+
  scale_x_continuous(limits = c(0.38,0.8),
                     breaks = seq(0.4,0.8,0.05))+
  scale_fill_manual(breaks =  c("0","1","2","3"),
                    values =  c("#f2f0f7","#cbc9e2","#9e9ac8","#6a51a3"),
                    na.value="gray80")+
  coord_fixed(ratio = 0.2,ylim=c(0.5,4.5),expand = F)+
  theme_bw()+
  theme(axis.title = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(size=12),
        axis.text.x = element_blank(),
        aspect.ratio = 1/1.5)

pop_row=plot_grid(pop_dens_map,pop_bars,rel_widths = c(1,2),nrow=1)
rn_row=plot_grid(rn_map,rn_bar,rel_widths = c(1,2),nrow=1)
surf_row=plot_grid(surf_map,surf_bars,rel_widths = c(1,2),nrow=1)
climate_row=plot_grid(climate_map,climate_bar,rel_widths = c(1,2),nrow=1)

l_col=plot_grid(pop_dens_map,rn_map,surf_map,climate_map,nrow=4)
r_col=plot_grid(pop_bars,rn_bar,surf_bars,climate_bar,nrow=4,
                rel_heights = c(4.5,3.5,6.5,3.5),
                align = "v",
                axis = "l")
r_col
p=plot_grid(pop_row,
          rn_row,
          surf_row,
          climate_row,
          nrow=4,rel_heights = c(4.5,3.5,6.5,3.5),
          align = "v",
          axis = "l")
ggsave2(here::here("Figures","Subgroup_Analysis.pdf"),p,
       width = 9,height = 9,units = "in",
       device = cairo_pdf)

lab_records$UNIT_CODE=as.factor(lab_records$UNIT_CODE)
lab_records$Granular=1*(grepl("gravel",(lab_records$UNIT_NAME)))+
  2*(grepl("sand",(lab_records$UNIT_NAME)))+
  4*(grepl("silt",(lab_records$UNIT_NAME)))+
  5*(grepl("clay",(lab_records$UNIT_NAME)))
lab_records[lab_records$Granular==3,"Granular"]=1
lab_records$Granular=as.factor(lab_records$Granular)
lab_records$UNIT_THICK=as.factor(lab_records$UNIT_THICK)
lab_records$GEOL_AGE=as.factor(lab_records$GEOL_AGE)

lab_records$GRP=as.factor(lab_records$GRP)
lab_records$climate_zone=as.factor(lab_records$climate_zone)
lab_records$Pop_cat=as.factor((lab_records$POP_SQMI>5000)+(lab_records$POP_SQMI>1000)+(lab_records$POP_SQMI>200))
lab_records$L_diff=lab_records$L_Aboveground-lab_records$L_Basement
m=gam(L_diff~Pop_cat+GRP+climate_zone+Granular+UNIT_THICK+GEOL_AGE,
   data=lab_records%>%filter(Method=="AT",
                             Basement>0,Aboveground>0,Basement<50,Aboveground<50,
                             climate_zone%in%c("14","25","26"),
                             Granular%in%c("1","2","4","5"),
                             UNIT_THICK%in%c(">100 feet","<100 feet","Discontinuous, or patchy in distribution"),
                             GEOL_AGE%in%c("Holocene to Tertiary","late Wisconsinan to Illinoian","late Wisconsinan to pre-Illinoian")))

lab_records%>%filter(season=="Summer",Method!="AT",climate_zone%in%c("14","25","26"))%>%group_by(climate_zone)%>%summarise(median(Aboveground/Basement,na.rm=T))
lab_records%>%filter(Basement>0,Aboveground>0,Basement<25)%>%ggplot()+geom_point(aes(x=Basement,y=exp(L_diff)))+ylim(c(0,1))

breaks=37*seq(0,35,0.2)

ggplot()+
  geom_histogram(data=lab_data%>%filter(Floor=="Basement",PCI.L>0),aes(x=37*PCI.L,y=..density..,fill="All"),color="black",alpha=0.35,bins=70)+
  geom_histogram(data=lab_records%>%filter(Basement>0),aes(x=37*Basement,y=..density..,fill="Paired"),color="black",alpha=0.35,bins=70)+
  geom_density(data=lab_data%>%filter(Floor=="Basement",PCI.L>0),aes(x=37*PCI.L,y=..density..,color="All"),breaks=breaks,size=1)+
  geom_density(data=lab_records%>%filter(Basement>0),aes(x=37*Basement,y=..density..,color="Paired"),breaks=breaks,size=1)+
  xlab("Basement Radon Concentration Bq/m3")+
  scale_fill_manual("",
                    breaks = c("All","Paired"),
                    values = c("#002868","#BF0A30"),
                    labels=c("All short-term measurements in basement",
                             "Short-term Measurements with a paired upstairs measurements"))+
  scale_color_manual("",
                    breaks = c("All","Paired"),
                    values = c("#002868","#BF0A30"),
                    labels=c("All short-term measurements in basement",
                             "Short-term Measurements with a paired upstairs measurements"))+
  
  xlim(c(0,1000))+
  theme_bw()+
  theme(legend.position = c(0.5,0.75),
        legend.text = element_text(size = 13),
        axis.text = element_text(size=12),
        axis.title = element_text(size=13))

breaks=seq(0.1,2,0.01)
ggplot()+
  geom_histogram(data=lab_records%>%filter(State%in%NE_States,Basement>0),aes(x=Aboveground/Basement,y=..density..,fill="NE"),color="black",alpha=0.35,bins=70)+
  geom_histogram(data=lab_records%>%filter(State%in%MW_States,Basement>0),aes(x=Aboveground/Basement,y=..density..,fill="MW"),color="black",alpha=0.35,bins=70)+
  geom_density(data=lab_records%>%filter(State%in%NE_States,Basement>0),aes(x=Aboveground/Basement,y=..density..,color="NE"),breaks=breaks,size=1)+
  geom_density(data=lab_records%>%filter(State%in%MW_States,Basement>0),aes(x=Aboveground/Basement,y=..density..,color="MW"),breaks=breaks,size=1)+
  geom_vline(data=lab_records%>%filter(State%in%NE_States,Basement>0),aes(xintercept=median(Aboveground/Basement,na.rm=T),color="NE",linetype="NE"))+
  geom_vline(data=lab_records%>%filter(State%in%MW_States,Basement>0),aes(xintercept=median(Aboveground/Basement,na.rm=T),color="MW",linetype="MW"))+
  xlab("Upstairs/Basement Ratio")+
  ylab("Density")+
  scale_fill_manual("",
                    breaks = c("NE","MW"),
                    values = c("#002868","#BF0A30"),
                    labels=c("Northeast",
                             "Midwest"))+
  scale_color_manual("",
                     breaks = c("NE","MW"),
                     values = c("#002868","#BF0A30"),
                     labels=c("Northeast",
                              "Midwest"))+
  scale_linetype_manual("",
                        breaks = c("NE","MW"),
                        values = c("solid","solid"),
                        labels=c("Northeast",
                                 "Midwest"))+
  xlim(c(0,2))+
  theme_bw()+
  theme(legend.position = c(0.5,0.75),
        legend.text = element_text(size = 13),
        axis.text = element_text(size=12),
        axis.title = element_text(size=13))
