library(ggplot2)
library(grid)
library(raster)
library(dplyr)
library(here)
library(sf)


load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","Medium Data","GB_ZIPCODE.RData"))
load(here::here("Data","GeoData","Boundaries.RData"))
load(here::here("Data","GeoData","Counties.RData"))
load(here::here("Data","Medium Data","NE_Rn_Obs.RData"))
load(here::here("Data","Medium Data","Rn_Geology.RData"))
load(here::here("Data","Medium Data","ZIP_Housing.RData"))

bound_sf<-st_as_sf(bound)
county_sf<-st_as_sf(county)

zip_num<-ne_radon%>%group_by(ZIPCODE)%>%count()
zip_num=as.data.frame(zip_num)
gb_county=county_sf%>%dplyr::filter(GEOID%in%c("25021",
                                               "25023",
                                               "25025",
                                               "25009",
                                               "25017",
                                               "25005",
                                               "25027",
                                               "25001",
                                               "33001",
                                               "33011",
                                               "33013",
                                               "33015",
                                               "33017",
                                               "44001",
                                               "44003",
                                               "44005",
                                               "44007",
                                               "44009",
                                               "09015"))

gb_zip=st_intersection(gb_zip,gb_county)
county_sf=st_intersection(county_sf,bound_sf)
##---------------------Figure 1 Study Extent-----------------------------------
## of radon measurement map
zip_num=ne_radon%>%group_by(ZIPCODE)%>%summarise(nobs=length(ZIPCODE))
gb_zip=gb_zip%>%left_join(zip_num,by=c("ZIP"="ZIPCODE"))
gb_zip=gb_zip%>%left_join(zips_house[,c("ZIP","Housing_Units")],by=c("ZIP"="ZIP"))
gb_zip[is.na(gb_zip$nobs),"nobs"]=0
gb_zip[is.na(gb_zip$Housing_Units),"Housing_Units"]=0
gb_zip$ratio=gb_zip$nobs/gb_zip$Housing_Units
gb_zip[is.infinite(gb_zip$ratio),"ratio"]=0

crs <-"+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

bound_sf_trans=st_transform(bound_sf,crs)

g<-ggplot()  +
  geom_sf(data = bound_sf_trans, fill="gray85",color="gray45") +
  geom_sf(data=st_as_sfc(st_bbox(gb_county)),color="darkred",size=1,fill="red",alpha=0.5)+
  coord_sf(xlim=c(st_bbox(bound_sf_trans)[1],st_bbox(bound_sf_trans)[3]),
           ylim=c(st_bbox(bound_sf_trans)[2],st_bbox(bound_sf_trans)[4]),clip = "on")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(size=2),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
g=ggplotGrob(g)

ggplot()+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill="gray75",show.legend = F)+
  geom_sf(data=gb_zip,
          aes(size="Zip Code",color="Zip Code",linetype="Zip Code"),fill=NA,show.legend = T)+
  geom_sf(data=gb_zip%>%filter(ratio<0.5),aes(fill=ratio,size="Zip Code",color="Zip Code",linetype="Zip Code"))+
  geom_sf(data=gb_zip%>%filter(ratio>0.5),aes(size="Zip Code",color="Zip Code",linetype="Zip Code"),fill="#811d5e",show.legend = F)+
  geom_sf(data=county_sf%>%filter(STATEFP%in%c("25","09","23","33","44","50")),
          aes(size="County",color="County",linetype="County"),fill=NA,show.legend = T)+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill=NA,show.legend = T)+
  geom_sf(data=st_as_sfc(st_bbox(gb_county)),color="darkred",size=1,fill=NA,alpha=0.5,show.legend = F)+
  coord_sf(xlim=c(st_bbox(gb_zip)[1],st_bbox(gb_zip)[3]+1.5),ylim=c(st_bbox(gb_zip)[2],st_bbox(gb_zip)[4]),clip = "on")+
  scale_fill_gradientn("Percent of Units Measured",
                       colours = c("white","#fed800","#ff6f01","#fd2f24","#811d5e"),
                       limits=c(0,0.5),
                       breaks=c(0.05,0.1,0.2,0.48),
                       labels=c("5%","10%","20%","≥50%"),
                       na.value="white")+
  scale_color_manual("Boundaries",breaks=c("Zip Code","County","State"),values = c("black","black","grey20"))+
  scale_size_manual("Boundaries",breaks=c("Zip Code","County","State"),values = c(0.15,0.5,1))+
  scale_linetype_manual("Boundaries",breaks=c("Zip Code","County","State"),values=c("solid","solid","solid"))+
  guides(fill = guide_colourbar(direction = "horizontal",
                                barwidth = 12.5, barheight = 1,title.position = "top",label.vjust=1),
         color= guide_legend(label.position = "bottom",label.vjust=1,title.position="top"))+
  theme(
    panel.background = element_rect(fill="aliceblue"),
    panel.grid = element_line(size=1,linetype = "solid",color="aliceblue"),
    panel.border = element_rect(size=2,fill = NA),
    legend.position = c(0.8,0.01),
    legend.background = element_rect(fill="white",color="black"),
    legend.title = element_text(size=13),
    legend.text = element_text(size=11),
    legend.key.width =  unit(4,"lines"),
    legend.key.height =  unit(1,"lines"),
    legend.direction = "horizontal",
    legend.box="vertical",
    legend.box.spacing = unit(1,"lines"),
    legend.justification="bottom",
    legend.margin = margin(4.5, 4.5, 4.5, 4.5),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )+annotation_custom(grob = g,
                      xmin=st_bbox(gb_county)[3]+0.01,
                      xmax=st_bbox(gb_county)[3]+1.55,
                      ymin=st_bbox(gb_county)[4]-1,
                      ymax=st_bbox(gb_county)[4])

#predicted mean radon map
ggplot()+
  geom_sf(data=world,fill="gray80")+
  geom_sf(data=zips_sf,
          aes(size="ZCTA",color="ZCTA",linetype="ZCTA"),fill=NA,show.legend = T)+
  geom_sf(data=bound_sf,linetype="solid",size=2,color="black",fill="gray80")+
  geom_sf(data=zips_sf%>%filter(m_rn<4),aes(fill=m_rn))+
  geom_sf(data=county_sf%>%filter(STATEFP%in%c("25","09","23","33","44","50")),
          aes(size="County",color="County",linetype="County"),fill=NA,show.legend = T)+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill=NA,show.legend = T)+
  coord_sf(xlim=c(st_bbox(zips_sf)[1],st_bbox(zips_sf)[3]+0.25),ylim=c(st_bbox(zips_sf)[2],st_bbox(zips_sf)[4]),clip = "on")+
  scale_fill_gradientn("Average Basement Radon (pCi/L)",
                       colours = c("white","#fed800","#ff6f01","#fd2f24","#811d5e"),
                       limits=c(1,10),
                       breaks=c(1,4,6,8),
                       na.value="lightgray")+
  scale_color_manual("Boundaries",breaks=c("ZCTA","County","State"),values = c("black","black","grey20"))+
  scale_size_manual("Boundaries",breaks=c("ZCTA","County","State"),values = c(0.15,0.5,1))+
  scale_linetype_manual("Boundaries",breaks=c("ZCTA","County","State"),values=c("solid","solid","solid"))+
  guides(fill = guide_colourbar(direction = "horizontal",
                                barwidth = 24.5, barheight = 1,title.position = "top",label.vjust=1),
         color= guide_legend(label.position = "bottom",label.vjust=1))+
  theme(
    panel.background = element_rect(fill="aliceblue"),
    panel.grid = element_line(size=1,linetype = "solid",color="aliceblue"),
    panel.border = element_rect(size=2,fill = NA),
    legend.position = c(0.775,0.2),
    legend.background = element_rect(fill="white",color="black"),
    legend.title = element_text(size=20),
    legend.text = element_text(size=15),
    legend.key.width =  unit(7.5,"lines"),
    legend.key.height =  unit(3.5,"lines"),
    legend.direction = "horizontal",
    axis.text = element_blank()
  )

##---------------------Figure 2 County Specific Summary-------------------
ne_radon=ne_radon%>%filter(COUNTY%in%gb_county$NAME)
ne_radon$COUNTY=as.factor(as.character(ne_radon$COUNTY))
ne_radon$kit=paste0(ne_radon$Type,ne_radon$METHOD)
county_summary=ne_radon%>%group_by(COUNTY,STATE,kit)%>%summarise(gm=exp(mean(log(PCI.L))),
                                                             am=mean(PCI.L),
                                                             qn_1st=quantile(PCI.L,0.25),
                                                             qn_3rd=quantile(PCI.L,0.75),
                                                             count=length(PCI.L))
county_summary=county_summary%>%filter(count>100)
county_summary$label=paste0(county_summary$COUNTY,",",county_summary$STATE)
county_summary=county_summary%>%arrange(desc(STATE),count)
county_summary$label=factor(county_summary$label,levels=county_summary$label)

ggplot()+
  geom_errorbarh(data = county_summary%>%filter(kit=="AirChekAC"),aes(xmin=37*qn_1st,xmax=37*qn_3rd,y=label),height=0.3)+
  geom_point(data = county_summary%>%filter(kit=="AirChekAC"),aes(x=37*gm,y=label,color="AirChekAC",shape="AirChekAC"),size=4)+
  geom_text(data = county_summary%>%filter(kit=="AirChekAC"),aes(x=37*qn_3rd+50,y=label,label=paste0("(",count," obs)")))+
  geom_errorbarh(data = county_summary%>%filter(kit=="AccuStarLS"),aes(xmin=37*qn_1st,xmax=37*qn_3rd,y=label),position = position_nudge(y = -0.25),height=0.3)+
  geom_point(data = county_summary%>%filter(kit=="AccuStarLS"),aes(x=37*gm,y=label,color="AccuStarLS",shape="AccuStarLS"),size=4,position = position_nudge(y = -0.25))+
  geom_text(data = county_summary%>%filter(kit=="AccuStarLS"),aes(x=37*qn_3rd+50,y=label,label=paste0("(",count," obs)")),position = position_nudge(y = -0.25))+
  geom_errorbarh(data = county_summary%>%filter(kit=="AccuStarAC"),aes(xmin=37*qn_1st,xmax=37*qn_3rd,y=label),position = position_nudge(y = 0.25),height=0.3)+
  geom_point(data = county_summary%>%filter(kit=="AccuStarAC"),aes(x=37*gm,y=label,color="AccuStarAC",shape="AccuStarAC"),size=4,position = position_nudge(y = 0.25))+
  geom_text(data = county_summary%>%filter(kit=="AccuStarAC"),aes(x=37*qn_3rd+50,y=label,label=paste0("(",count," obs)")),position = position_nudge(y = 0.25))+
  scale_color_manual("Legend",breaks=c("AirChekAC","AccuStarAC","AccuStarLS"),
                     values=c("Navy","Navy","Navy"),
                     labels=c("AirChek","AccuStar PicoCan-400 ","Niton CLS-2"))+
  scale_shape_manual("Legend",breaks=c("AirChekAC","AccuStarAC","AccuStarLS"),
                     values=c(15,16,17),
                     labels=c("AirChek","AccuStar PicoCan-400 ","Niton CLS-2"))+
  xlim(0,500)+
  xlab("County-level Basement Rn Level (Bq/m3)")+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text = element_text(size=12),
    legend.position = c(0.9,0.8)
  )

##---------------------Figure 3 Scatter Plot----------------------------
library(MASS)
library(dplyr)
library(boot)
library(ggplot2)

cv_files=list.files(here::here("Data","Medium Data","Real_CV"),full.names = T)

cv_results=list()

for(f in 1:length(cv_files)){
  load(cv_files[f])
  cv_results[[f]]=test_set[,c("pred","gm_month","n_units","X","Y","fold","rowIndex")]
}

cv_results=bind_rows(cv_results)
cv_results=cv_results%>%group_by(rowIndex)%>%summarise(obs=mean(gm_month),
                                                       pred=mean(pred),
                                                       res=var(pred,na.rm=T),
                                                       n_units=mean(n_units),
                                                       X=mean(X),
                                                       Y=mean(Y))
cv_results$obs=37*exp(cv_results$obs)
cv_results$pred=37*exp(cv_results$pred)
m=rlm(obs~0+pred,weights = cv_results$n_units,data=cv_results)
ggplot(data=cv_results%>%filter(n_units>9))+
  geom_point(aes(x=obs,y=pred,size=n_units),fill="#B22234",shape=21,color="gray75")+
  geom_abline(intercept = 0,slope = m$coefficients[1],linetype="solid",color="#3C3B6E",size=1)+
  geom_abline(intercept = 0,slope = 1,linetype="dashed",size=0.75)+
  xlab("Observed Monthly ZCTA-level Radon Concentrations (Bq/m3)")+
  ylab("Estimated Monthly ZCTA-level Radon Concentrations (Bq/m3)")+
  scale_radius("Count of Short-term Measurements",
               breaks = c(10,25,75,150,200),
               labels = c(10,25,75,150,200),
               range = c(0.25,7.5),
               trans = "log")+
  coord_cartesian(xlim=c(30,250),ylim=c(30,250))+
  theme_bw()+
  theme(legend.position = c(0.3,0.8),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.box.background = element_rect(fill="white",size=1),
        axis.text = element_text(size=11),
        axis.title = element_text(size=12))

##---------------------Figure 4 Seasonal Distribution Plot--------------------------------------
#load all prediction maps
files=list.files(here::here("Data","Medium Data","Monthly Prediction"))
prediction_tank=list()
l=1
for(f in files){
  load(here::here("Data","Medium Data","Monthly Prediction",f))
  temp=gb_zip%>%left_join(monthly_prediction,by=c("ZIP"="ZIPCODE"))
  prediction_tank[[l]]=temp[,c("ZIP","Month","Year","G_Radon")]
  st_geometry(prediction_tank[[l]])=NULL
  l=l+1
}
prediction_tank=bind_rows(prediction_tank)
#attach season column to the data
trans<-cbind.data.frame(Month=1:12,
                        Season=c("Winter","Winter","Spring","Spring","Spring","Summer",
                                 "Summer","Summer","Autumn","Autumn","Autumn","Winter"))
prediction_tank=prediction_tank%>%left_join(trans)
#take the average of seasonal maps
season_level=prediction_tank%>%group_by(ZIP,Season)%>%summarise(m=37*mean(G_Radon))
season_level=season_level[!is.na(season_level$ZIP),]
spring_level=season_level[season_level$Season=="Spring",]
summer_level=season_level[season_level$Season=="Summer",]
autumn_level=season_level[season_level$Season=="Autumn",]
winter_level=season_level[season_level$Season=="Winter",]
#plot it 2*2
season_plot=function(level,season,legend.position="none"){
  plot_data=gb_zip%>%left_join(level)
  g<-ggplot()+
    geom_sf(data=bound_sf,
            aes(linetype="State",size="State",color="State"),fill="gray80",show.legend = F)+
    geom_sf(data=plot_data,aes(size="ZCTA",color="ZCTA",linetype="ZCTA",fill=m))+
    geom_sf(data=county_sf,
            aes(size="County",color="County",linetype="County"),fill=NA,show.legend = F)+
    geom_sf(data=bound_sf,
            aes(linetype="State",size="State",color="State"),fill=NA,show.legend = F)+
    coord_sf(xlim=c(st_bbox(gb_zip)[1],st_bbox(gb_zip)[3]),ylim=c(st_bbox(gb_zip)[2],st_bbox(gb_zip)[4]),clip = "on")+
    scale_fill_gradientn("Bq/m3",
                         colours = c("white","#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
                         values=c(0,0.45,0.5,0.65,1),
                         labels=c(0,50,100,125,150),
                         limits=c(0,175),
                         na.value="lightgray")+
    ggtitle(season)+
    scale_color_manual(NULL,breaks=c("ZCTA","County","State"),values = c("black","black","grey20"))+
    scale_size_manual(NULL,breaks=c("ZCTA","County","State"),values = c(0.15,0.5,1))+
    scale_linetype_manual(NULL,breaks=c("ZCTA","County","State"),values=c("solid","solid","solid"))+
    guides(fill = guide_colourbar(direction = "vertical",
                                  barwidth = 1, barheight = 8.5,title.position = "top",label.vjust=1),
           color= guide_legend(label.position = "bottom",label.vjust=1))+
    theme(
      panel.background = element_rect(fill="aliceblue"),
      panel.grid = element_line(size=1,linetype = "solid",color="aliceblue"),
      panel.border = element_rect(size=2,fill = NA),
      legend.position = legend.position,
      legend.background = element_rect(fill="white",color="black"),
      legend.title = element_text(size=13),
      legend.text = element_text(size=11),
      legend.key.width =  unit(2.5,"lines"),
      legend.key.height =  unit(1,"lines"),
      legend.direction = "vertical",
      legend.box="vertical",
      legend.margin = margin(1, 1, 1, 1),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  return(g)
}

spring_plot=season_plot(spring_level,season = "Spring",legend.position = c(0.5,0.5))
Annual_plot=season_plot(season_level,season = "Annual",legend.position = c(0.9,0.95))
summer_plot=season_plot(summer_level,season = "Summer",legend.position = "none")
autumn_plot=season_plot(autumn_level,season = "Autumn",legend.position = "none")
winter_plot=season_plot(winter_level,season = "Winter",legend.position = "none")

library(cowplot)
g<-cowplot::plot_grid(spring_plot,summer_plot,autumn_plot,winter_plot,
                      nrow = 2)
save_plot(file=here::here("Prediction_c.pdf"),plot=g,
          base_height=9,base_width=7,device = cairo_pdf)
##-------------------Plot the LBNL county radon prediction-----------------
lbnl<-read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/BerkeleyClean.csv")
lbnl$fips=as.character(formatC(lbnl$fips,width = 5,flag=0))
gb_county=st_intersection(gb_county,bound_sf)

plot_data=gb_county%>%left_join(lbnl,by=c("GEOID"="fips"))

g<-ggplot()+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill="gray80",show.legend = T)+
  geom_sf(data=plot_data,
          aes(size="County",color="County",linetype="County",fill=100*GMest),show.legend = T)+
  geom_sf(data=gb_zip,aes(size="ZCTA",color="ZCTA",linetype="ZCTA"),fill=NA)+
  geom_sf(data=county_sf,
          aes(size="County",color="County",linetype="County"),fill=NA,show.legend = F)+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill=NA,show.legend = F)+
  coord_sf(xlim=c(st_bbox(gb_zip)[1],st_bbox(gb_zip)[3]),ylim=c(st_bbox(gb_zip)[2],st_bbox(gb_zip)[4]),clip = "on")+
  scale_fill_gradientn("Bq/m3",
                       colours = c("white","#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
                       values=c(0,0.45,0.5,0.65,1),
                       labels=c(0,50,100,125,150),
                       limits=c(0,175),
                       na.value="lightgray")+
  scale_color_manual(NULL,breaks=c("ZCTA","County","State"),values = c("black","black","grey20"))+
  scale_size_manual(NULL,breaks=c("ZCTA","County","State"),values = c(0.15,0.5,1))+
  scale_linetype_manual(NULL,breaks=c("ZCTA","County","State"),values=c("solid","solid","solid"))+
  guides(fill = guide_colourbar(direction = "vertical",
                                barwidth = 1, barheight = 8.5,title.position = "top",label.vjust=1))+
  theme(
    panel.background = element_rect(fill="aliceblue"),
    panel.grid = element_line(size=1,linetype = "solid",color="aliceblue"),
    panel.border = element_rect(size=2,fill = NA),
    legend.position = c(0.5,0.5),
    legend.background = element_rect(fill="white",color="black"),
    legend.title = element_text(size=13),
    legend.text = element_text(size=11),
    legend.key.width =  unit(2.5,"lines"),
    legend.key.height =  unit(1,"lines"),
    legend.direction = "vertical",
    legend.box="vertical",
    legend.margin = margin(1, 1, 1, 1),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
#compare LBNL with our prediction
annual_level=prediction_tank%>%group_by(ZIP)%>%summarise(m=37*mean(G_Radon))
annual_plot=season_plot(spring_level,season = NULL,legend.position = "none")

g<-cowplot::plot_grid(annual_plot,g,nrow = 1,labels = c("A","B"))
save_plot(file=here::here("Prediction_Comparison.pdf"),plot=g,
          base_height=4.5,base_width=7,device = cairo_pdf)

#
load(here::here("Data","GeoData","FIPS_ZIPCODE_Crosswalk.RData"))
annual_level=annual_level%>%left_join(FIPS_ZIPCODE_TABLE,by=c("ZIP"="zips"))
fips_level=annual_level%>%group_by(fips)%>%summarise(m=mean(m,na.rm=T),
                                                     n=length(fips))
fips_pred=fips_level[fips_level$n>2,]
fips_pred=fips_pred%>%left_join(plot_data[,c("GEOID","GMest")],by=c("fips"="GEOID"))

fips_pred=fips_pred%>%group_by(fips)%>%summarise(pred=mean(m),
                                                 lbnl=mean(GMest,na.rm=T))
write.csv(fips_pred,file="fips_pred.csv")
##--------------------Plot the seasonal variation map-----------------------
temp=cbind.data.frame(spring_level[,c("ZIP","m"),],
                      summer_level[,"m"],
                      autumn_level[,"m"],
                      winter_level[,"m"])
names(temp)=c("ZIP","Spring","Summer","Autumn","Winter")
temp=temp[!is.na(temp$Spring),]
temp_sd=apply(temp[,2:5],1,FUN="sd")
temp=cbind.data.frame(temp$ZIP,temp_sd)
names(temp)=c("ZIP","Sd")

temp_ratio=temp$Winter/temp$Summer
temp=cbind.data.frame(temp$ZIP,temp_ratio)
names(temp)=c("ZIP","ratio")

plot_data=gb_zip%>%left_join(temp)
g<-ggplot()+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill="gray80",show.legend = F)+
  geom_sf(data=plot_data,aes(size="ZCTA",color="ZCTA",linetype="ZCTA",fill=ratio))+
  geom_sf(data=county_sf,
          aes(size="County",color="County",linetype="County"),fill=NA,show.legend = F)+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill=NA,show.legend = F)+
  coord_sf(xlim=c(st_bbox(gb_zip)[1],st_bbox(gb_zip)[3]),ylim=c(st_bbox(gb_zip)[2],st_bbox(gb_zip)[4]),clip = "on")+
  scale_fill_gradientn("Bq/m3",
                       colours = c("white","#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
                       #values=c(0,0.3,0.4,0.5,0.65,1),
                       limits=c(0.9,1.5),
                       na.value="lightgray")+
  scale_color_manual(NULL,breaks=c("ZCTA","County","State"),values = c("black","black","grey20"))+
  scale_size_manual(NULL,breaks=c("ZCTA","County","State"),values = c(0.15,0.5,1))+
  scale_linetype_manual(NULL,breaks=c("ZCTA","County","State"),values=c("solid","solid","solid"))+
  guides(fill = guide_colourbar(direction = "vertical",
                                barwidth = 1, barheight = 8.5,title.position = "top",label.vjust=1))+
  theme(
    panel.background = element_rect(fill="aliceblue"),
    panel.grid = element_line(size=1,linetype = "solid",color="aliceblue"),
    panel.border = element_rect(size=2,fill = NA),
    legend.position = c(0.8,0.8),
    legend.background = element_rect(fill="white",color="black"),
    legend.title = element_text(size=13),
    legend.text = element_text(size=11),
    legend.key.width =  unit(2.5,"lines"),
    legend.key.height =  unit(1,"lines"),
    legend.direction = "vertical",
    legend.box="vertical",
    legend.margin = margin(1, 1, 1, 1),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

##------------------------------Calculate R2 between LBNL and our prediction----------
load(here::here("Data","GeoData","FIPS_ZIPCODE_Crosswalk.RData"))

temp_m=apply(temp[,2:5], 1, FUN="mean")
temp$Mean=temp_m
temp=temp%>%left_join(FIPS_ZIPCODE_TABLE,by=c("ZIP"="zips"))
temp_ct=temp%>%group_by(fips)%>%summarise(m_rn=mean(Mean))
temp_ct=temp_ct%>%left_join(lbnl,by=c("fips"="fips"))
write.csv(temp_ct,file="corr.csv")

##------------------------------Plot Uranium concentration------------------------------
plot_data=gb_zip%>%left_join(zip_geo,by=c("ZIP"="ZIP"))
g<-ggplot()+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill="gray80",show.legend = F)+
  geom_sf(data=plot_data,aes(size="ZCTA",color="ZCTA",linetype="ZCTA",fill=Sur_43))+
  geom_sf(data=county_sf,
          aes(size="County",color="County",linetype="County"),fill=NA,show.legend = F)+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill=NA,show.legend = F)+
  coord_sf(xlim=c(st_bbox(gb_zip)[1],st_bbox(gb_zip)[3]),ylim=c(st_bbox(gb_zip)[2],st_bbox(gb_zip)[4]),clip = "on")+
  scale_fill_gradientn("Unitless",
                       colours = c("white","#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
                      values=c(0,0.4,0.6,0.8,1),
                       labels=c(0,0.4,0.6,0.8,1),
                       limits=c(0,1),
                       na.value="lightgray")+
  ggtitle("Radon Potential")+
  scale_color_manual(NULL,breaks=c("ZCTA","County","State"),values = c("black","black","grey20"))+
  scale_size_manual(NULL,breaks=c("ZCTA","County","State"),values = c(0.15,0.5,1))+
  scale_linetype_manual(NULL,breaks=c("ZCTA","County","State"),values=c("solid","solid","solid"))+
  guides(fill = guide_colourbar(direction = "vertical",
                                barwidth = 1, barheight = 8.5,title.position = "top",label.vjust=1),
         color= guide_legend(label.position = "bottom",label.vjust=1))+
  theme(
    panel.background = element_rect(fill="aliceblue"),
    panel.grid = element_line(size=1,linetype = "solid",color="aliceblue"),
    panel.border = element_rect(size=2,fill = NA),
    legend.position = c(0.85,0.7),
    legend.background = element_rect(fill="white",color="black"),
    legend.title = element_text(size=13),
    legend.text = element_text(size=11),
    legend.key.width =  unit(2.5,"lines"),
    legend.key.height =  unit(1,"lines"),
    legend.direction = "vertical",
    legend.box="vertical",
    legend.margin = margin(1, 1, 1, 1),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
ggsave(file=here::here("Radon Potential.pdf"),plot=g,
          height=9,width=7,device = cairo_pdf)

##---------------plot long-term season variation-------------------------
annual_season=prediction_tank%>%group_by(Year,Season)%>%summarise(m=mean(37*G_Radon,na.rm=T),
                                                                  sd=sd(37*G_Radon,na.rm=T))
annual_season=annual_season[!is.na(annual_season$m),]

ggplot()+
  geom_path(data=annual_season[annual_season$Season=="Spring",],aes(x=Year+0.15,y=m,color=Season),size=0.5)+
  geom_errorbar(data=annual_season[annual_season$Season=="Spring",],aes(x=Year+0.15,ymin=m-1.96*sd,ymax=m+1.96*sd,color=Season),size=0.5,width=0.25)+
  geom_point(data=annual_season[annual_season$Season=="Spring",],aes(x=Year+0.15,y=m,color=Season),size=2.5)+
  geom_path(data=annual_season[annual_season$Season=="Summer",],aes(x=Year,y=m,color=Season),size=0.5)+
  geom_errorbar(data=annual_season[annual_season$Season=="Summer",],aes(x=Year,ymin=m-1.96*sd,ymax=m+1.96*sd,color=Season),size=0.5,width=0.25)+
  geom_point(data=annual_season[annual_season$Season=="Summer",],aes(x=Year,y=m,color=Season),size=2.5)+
  geom_path(data=annual_season[annual_season$Season=="Autumn",],aes(x=Year+0.3,y=m,color=Season),size=0.5)+
  geom_errorbar(data=annual_season[annual_season$Season=="Autumn",],aes(x=Year+0.3,ymin=m-1.96*sd,ymax=m+1.96*sd,color=Season),size=0.5,width=0.25)+
  geom_point(data=annual_season[annual_season$Season=="Autumn",],aes(x=Year+0.3,y=m,color=Season),size=2.5)+
  geom_path(data=annual_season[annual_season$Season=="Winter",],aes(x=Year+0.45,y=m,color=Season),size=0.5)+
  geom_errorbar(data=annual_season[annual_season$Season=="Winter",],aes(x=Year+0.45,ymin=m-1.96*sd,ymax=m+1.96*sd,color=Season),size=0.5,width=0.25)+
  geom_point(data=annual_season[annual_season$Season=="Winter",],aes(x=Year+0.45,y=m,color=Season),size=2.5)+
  
  #geom_smooth(aes(x=Year,y=m,color=Season),
  #            method = "gam",formula = y ~ poly(x-2004, 4),se=F)+
  scale_color_manual(breaks=c("Spring","Summer","Autumn","Winter"),
                     values = c("#00FF7F","#00755E","#FFCC33","#00BFFF"))+
  xlab("Year")+
  ylab("Average Radon Concentrations (Bq/m3)")+
  theme_bw()
