library(ggplot2)
library(raster)
library(dplyr)
library(here)
library(sf)

load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","Medium Data","Pred_Rn_Data.RData"))
load(here::here("Data","GeoData","Boundaries.RData"))
load(here::here("Data","GeoData","Counties.RData"))
load(here::here("Data","Medium Data","NE_Rn.RData"))
load(here::here("Data","Medium Data","Rn_Geology.RData"))

zips_sf<-st_as_sf(zips)
bound_sf<-st_as_sf(bound)
county_sf<-st_as_sf(county)

zip_num<-ne_radon%>%group_by(ZIPCODE)%>%count()
zip_num=as.data.frame(zip_num)
zip_rn=pred_data%>%group_by(ZIPCODE)%>%summarise(m_rn=mean(Rn_Adj))

zips_sf=zips_sf%>%left_join(zip_num,by=c("ZIP"="ZIPCODE"))
zips_sf=zips_sf%>%filter(STATE%in%c("MA","NH","CT","RI","ME","VT"))
zips_sf=zips_sf%>%left_join(zip_rn,by=c("ZIP"="ZIPCODE"))



## of radon measurement map
ggplot()+
  geom_sf(data=zips_sf)+
  geom_sf(data=zips_sf%>%filter(n>1,n<1000),aes(fill=n))+
  geom_sf(data=bound_sf,fill=NA)+
  geom_sf(data=county_sf%>%filter(STATEFP=="25"),fill=NA)+
  coord_sf(xlim=c(st_bbox(zips_sf)[1],st_bbox(zips_sf)[3]),ylim=c(st_bbox(zips_sf)[2],st_bbox(zips_sf)[4]),clip = "on")+
  scale_fill_continuous("Num of Measurements",type = "viridis")+
  theme(
    panel.background = element_blank(),
    panel.grid = element_line(size=1,linetype = "solid"),
    panel.border = element_rect(size=2,fill = NA),
    legend.position = c(0.8,0.3)
  )
#predicted mean radon map
ggplot()+
  geom_sf(data=world,fill="gray80")+
  geom_sf(data=zips_sf,
          aes(size="Zip Code",color="Zip Code",linetype="Zip Code"),fill=NA,show.legend = T)+
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
  scale_color_manual(NULL,breaks=c("Zip Code","County","State"),values = c("black","black","gray"))+
  scale_size_manual(NULL,breaks=c("Zip Code","County","State"),values = c(1.5,2,1))+
  scale_linetype_manual(NULL,breaks=c("Zip Code","County","State"),values=c("dashed","solid","solid"))+
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
#
library(rnaturalearth)
library(sf)
library(ggplot2)
library(dplyr)
world <- ne_countries(scale = 'medium', returnclass = 'sf')
ocean50 <- ne_download(scale = 50, type = 'ocean', category = 'physical',returnclass = 'sf')
ortho <- "+proj=ortho +lat_0=48 +lon_0=-72 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"


ggplot() + 
  geom_sf(data=ocean, fill = 'aliceblue') + # the globe "horizon" - you can ignore
  geom_sf(data=world) + # the visible continents
  geom_sf(data=bound_sf%>%filter(STATEFP=="25"),fill="red",color=NA)+
  coord_sf(crs = ortho) +
  theme(
    panel.grid.major = element_line(
      color = gray(.5), linetype = 'solid', size = 0.5),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
  )
  
