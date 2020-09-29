library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(here)
library(rnaturalearth)
library(rnaturalearthdata)

#Basic Information
load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","GeoData","Boundaries.RData"))
load(here::here("Data","GeoData","Counties.RData"))

world <- ne_countries(scale = "medium", returnclass = "sf")

load(here::here("Data","Medium Data","NE_Rn_Obs.RData"))
load(here::here("Data","Medium Data","ZIP_Housing.RData"))

zips_sf<-st_as_sf(zips)
zips_sf<-zips_sf%>%filter(STATE%in%c("MA","NH","CT","ME","VT","RI"))
bound_sf<-st_as_sf(bound)
county_sf<-st_as_sf(county)
county_sf=county_sf%>%filter(STATEFP%in%c("09","23","25","33","44","50"))
## Count the number of observations per zipcode and per county
zip_rn_info=ne_radon%>%group_by(ZIPCODE)%>%summarise(obs_count=length(Year),
                                                      obs_amean=mean(PCI.L),
                                                      obs_gmean=exp(mean(log(PCI.L))),
                                                      q1_rn=quantile(PCI.L,0.25),
                                                      q3_rn=quantile(PCI.L,0.75))
zip_unit_count=zips_house[,c("ZIP","Housing_Units")]

zips_sf=zips_sf%>%left_join(zip_unit_count)%>%left_join(zip_rn_info,by=c("ZIP"="ZIPCODE"))
zips_sf[is.na(zips_sf$Housing_Units),"Housing_Units"]=0
zips_sf$obs_ratio=zips_sf$obs_count/zips_sf$Housing_Units
zips_sf[zips_sf$Housing_Units==0,"obs_ratio"]=0
zips_sf[is.na(zips_sf$obs_ratio),"obs_ratio"]=0
##-------------Figure 1A, Study Extent-------------------------------------
g<-ggplot()+
  geom_sf(data=world,fill="gray80",color=NA)+
  geom_sf(data=zips_sf,
          aes(size="Zip Code",color="Zip Code",linetype="Zip Code"),fill=NA,show.legend = T)+
  geom_sf(data=zips_sf%>%filter(obs_ratio<0.3),aes(fill=obs_ratio,size="Zip Code",color="Zip Code",linetype="Zip Code"))+
  geom_sf(data=zips_sf%>%filter(obs_ratio>0.3),aes(size="Zip Code",color="Zip Code",linetype="Zip Code"),fill="#811d5e",show.legend = F)+
  geom_sf(data=county_sf%>%filter(STATEFP%in%c("25","09","23","33","44","50")),
          aes(size="County",color="County",linetype="County"),fill=NA,show.legend = T)+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill=NA,show.legend = T)+
  coord_sf(xlim=c(st_bbox(zips_sf)[1],st_bbox(zips_sf)[3]+0.25),ylim=c(st_bbox(zips_sf)[2],st_bbox(zips_sf)[4]),clip = "on")+
  scale_fill_gradientn("Percent of Units Measured",
                       colours = c("white","#fed800","#ff6f01","#fd2f24","#811d5e"),
                       limits=c(0,0.3),
                       breaks=c(0.05,0.1,0.2,0.28),
                       labels=c("5%","10%","20%","≥30%"),
                       na.value="white")+
  scale_color_manual(NULL,breaks=c("Zip Code","County","State"),values = c("black","black","grey20"))+
  scale_size_manual(NULL,breaks=c("Zip Code","County","State"),values = c(0.15,0.5,1))+
  scale_linetype_manual(NULL,breaks=c("Zip Code","County","State"),values=c("solid","solid","solid"))+
  guides(fill = guide_colourbar(direction = "horizontal",
                                barwidth = 10.5, barheight = 1,title.position = "top",label.vjust=1),
         color= guide_legend(label.position = "bottom",label.vjust=1))+
  theme(
    panel.background = element_rect(fill="aliceblue"),
    panel.grid = element_line(size=1,linetype = "solid",color="aliceblue"),
    panel.border = element_rect(size=2,fill = NA),
    legend.position = c(0.775,0.2),
    legend.background = element_rect(fill="white",color="black"),
    legend.title = element_text(size=15),
    legend.text = element_text(size=12),
    legend.key.width =  unit(3.5,"lines"),
    legend.key.height =  unit(1.5,"lines"),
    legend.direction = "horizontal",
    axis.text = element_blank()
  )

##-------------Figure 1B, County-level summary plot of radon--------------------
county_rn_info=ne_radon%>%group_by(COUNTY,STATE)%>%summarise(obs_count=length(Year),
                                                             obs_amean=mean(PCI.L),
                                                             obs_gmean=exp(mean(log(PCI.L))),
                                                             q1_rn=quantile(PCI.L,0.25),
                                                             q3_rn=quantile(PCI.L,0.75))
county_rn_info=county_rn_info%>%filter(obs_count>100)

county_rn_info=county_rn_info%>%filter(COUNTY!="",COUNTY!="\\N")
county_rn_info$label=paste0(county_rn_info$COUNTY,",",county_rn_info$STATE)
county_rn_info=county_rn_info%>%arrange(desc(STATE),obs_count)
county_rn_info$label=factor(county_rn_info$label,levels=county_rn_info$label)

ggplot(data = county_rn_info)+
  geom_errorbarh(aes(xmin=37*q1_rn,xmax=37*q3_rn,y=label))+
  geom_point(aes(x=37*obs_gmean,y=label,color="GM",shape="GM"),size=4)+
  geom_point(aes(x=37*obs_amean,y=label,color="AM",shape="AM"),size=4)+
  geom_text(aes(x=37*q3_rn+40,y=label,label=paste0("(",obs_count," obs)")))+
  scale_color_manual("Legend",breaks=c("AM","GM"),
                     values=c("Navy","Navy"),
                     labels=c("Arithmetic Mean","Geometric Mean"))+
  scale_shape_manual("Legend",breaks=c("AM","GM"),
                     values=c(0,15),
                     labels=c("Arithmetic Mean","Geometric Mean"))+
  xlim(0,500)+
  xlab("County-level Basement Rn Levl (Bq/m3)")+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text = element_text(size=12),
    legend.position = c(0.8,0.9)
  )
