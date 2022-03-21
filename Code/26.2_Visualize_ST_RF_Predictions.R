library(ggplot2)

States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
         "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")
folder="/n/koutrakis_lab/lab/Radon_Mortality/Data/State_Year/"
Year=2015

files=list.files(path=folder,pattern =as.character(Year))
state_list=list()
l=1
for(f in files){
  load(paste0(folder,f))
  state_list[[l]]=pred_list
  l=l+1
}
state_pred=bind_rows(state_list)

library(sf)
library(scales)
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "

load(here::here("Data","GeoData","Boundaries.RData"))
load(here::here("Data","GeoData","2015_Shapes.RData"))
zips=st_as_sf(zips)
zips=zips%>%filter(STATE%in%States)
zips=st_transform(zips,crs=prjstring)
annual_pred=state_pred%>%filter(Basement==1)%>%group_by(ZIPS)%>%summarise(mean_rn=mean(Local_Pred))

zips=zips%>%left_join(annual_pred,by=c("ZIP"="ZIPS"))

bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs=prjstring)

ggplot(data=zips)+
  geom_sf(data=bound_sf%>%filter(STUSPS%in%States),fill="white")+
  geom_sf(aes(fill=mean_rn),color="gray",size=0.15)+
  scale_fill_stepsn(expression('Annual Average (pCi/L)'),
                    breaks = seq(0,8,0.4),
                    values = seq(0,1,0.05),
                    limits=c(0,8),
                    colors = rev(RColorBrewer::brewer.pal(11,"PuBuGn")),
                    na.value = "red",
                    guide = guide_colorsteps(direction = "horizontal",
                                             title.position = "top",
                                             label.position = "bottom",
                                             barwidth = unit(4, "inch"),
                                             barheight=unit(0.1, "inch")))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title = element_blank())
