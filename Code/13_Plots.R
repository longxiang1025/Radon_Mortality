library(ggplot2)
library(sf)
library(dplyr)

load(here::here("Data","Above_Basement_Comparison.RData"))

load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","Medium Data","GB_ZIPCODE.RData"))
load(here::here("Data","GeoData","Boundaries.RData"))
load(here::here("Data","GeoData","Counties.RData"))

bound_sf<-st_as_sf(bound)
county_sf<-st_as_sf(county)

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
county_sf=st_intersection(county_sf,bound_sf)
gb_zip=st_intersection(gb_zip,gb_county)
#zip_center=gb_zip%>%st_centroid()
#zip_center=st_intersection(zip_center,county_sf)

#zip_center=cbind.data.frame(zip_center$ZIP,
#                            st_coordinates(zip_center))
#names(zip_center)=c("ZIP","Longitude","Latitude")
#zip_center=zip_center%>%group_by(ZIP)%>%summarise(Longitude=mean(Longitude),
#                                                  Latitude=mean(Latitude))

#gb_records=gb_records%>%left_join(zip_center,by=c("ZIPCODE"="ZIP"))
#gb_records$Longitude=gb_records$Longitude+rnorm(nrow(gb_records),mean=0,sd=0.035)
#gb_records$Latitude=gb_records$Latitude+rnorm(nrow(gb_records),mean=0,sd=0.035)

#gb_records=gb_records[!is.na(gb_records$Longitude),]
#gb_records$Type=paste0(gb_records$lab,"_",gb_records$METHOD)
coordinates(gb_records)=~Longitude+Latitude
gb_records=st_as_sf(gb_records)
st_crs(gb_records)=st_crs(bound_sf)
gb_records=gb_records%>%arrange(desc(Duration))

study_extent=st_union(gb_zip)
gb_records=st_intersection(gb_records,study_extent)

corner_points=expand.grid(x=c(-71.2,-71.1),
                          y=c(42.4,42.5))
corner_points=corner_points[c(1,2,4,3),]

radnets<-cbind.data.frame(lat=c(43.2081,43.0718,42.3601,42.2626,41.8240),
                          lon=c(-71.5376,-70.7626,-71.0589,-71.8023,-71.4128))

boston_zoom<-ggplot()+
  xlim(c(-71.2,-71.1))+
  ylim(c(42.4,42.5))+
  geom_sf(data=county_sf%>%filter(STATEFP%in%c("25","09","23","33","44","50")),
          aes(fill="County",linetype="County",size="County",color="County"),show.legend = F)+
  geom_sf(data=gb_zip,
          aes(fill="ZIP",linetype="ZIP",size="ZIP",color="ZIP"),show.legend = F)+
  geom_sf_text(data=gb_zip,
               aes(label = NAME), colour = "black",size=3)+
  geom_sf(data=bound_sf,
          aes(fill="State",linetype="State",size="State",color="State"),show.legend = F)+
  geom_sf(data=gb_records%>%filter(Type!="MA_AT"),
          aes(color=Type,size=Type,shape=Type),show.legend = F)+
  geom_sf(data=gb_records%>%filter(Type=="MA_AT"),
          aes(color=Type,size=Type,shape=Type),show.legend = F)+
  scale_linetype_manual("Legend",breaks = c("State","ZIP","County"),
                        values = c("solid","solid","solid"))+
  scale_fill_manual("Legend",breaks = c("State","ZIP","County"),
                    values = c(NA,"gray90","gray95"))+
  scale_size_manual("Legend",breaks = c("State","ZIP","County","NC_AC","MA_LS","MA_AC","MA_AT"),
                    values = c(1,0.35,0.55,2.5,2.5,2.5,4))+
  scale_shape_manual("Legend",breaks = c("NC_AC","MA_LS","MA_AC","MA_AT"),
                     values = c(7,10,13,17))+
  scale_color_manual("Measurements",breaks = c("State","ZIP","County","NC_AC","MA_LS","MA_AC","MA_AT"),
                     values = c("Black","Black","Gray50","#2c7bb6","#2c7bb6","#2c7bb6","#d7191c"),
                     labels=c("State","ZIP Code","County","Air Chek","Niton","Accustar","Alpha-Track"),
                     guide = guide_legend(override.aes = list(linetype = c(rep("solid",3),rep("blank",4)), 
                                                              shape = c(rep(NA,3),7,10,13,17),
                                                              color=c("black","black","gray50","#2c7bb6","#2c7bb6","#2c7bb6","#d7191c"))))+
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
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )
boston_zoom
boston_zoom=ggplotGrob(boston_zoom)

crs <-"+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

bound_sf_trans=st_transform(bound_sf,crs)

us_east<-ggplot()  +
  geom_sf(data = bound_sf_trans, fill="gray85",color="gray45") +
  geom_sf(data=st_as_sfc(st_bbox(study_extent)),color="darkred",size=1,fill="red",alpha=0.5)+
  coord_sf(xlim=c(1000000,st_bbox(bound_sf_trans)[3]),
           ylim=c(st_bbox(bound_sf_trans)[2],st_bbox(bound_sf_trans)[4]),clip = "on")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(size=2),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
us_east=ggplotGrob(us_east)

gb_map=ggplot()+
  xlim(c(st_bbox(study_extent)[1],st_bbox(study_extent)[3]+1.25))+
  ylim(st_bbox(gb_zip)[2]-0.175,st_bbox(gb_zip)[4])+
  geom_sf(data=bound_sf,
          aes(fill="State",linetype="State",size="State",color="State"),show.legend = "polygon")+
  geom_sf(data=study_extent,fill="gray90",color="black",size=1)+
  geom_sf(data=county_sf%>%filter(STATEFP%in%c("25","09","23","33","44","50")),
          aes(fill="County",linetype="County",size="County",color="County"),show.legend = "polygon")+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill=NA,show.legend = "polygon")+
  geom_sf(data=study_extent,fill=NA,color="black",size=1.5)+
  geom_sf(data=gb_records%>%filter(Type!="MA_AT"),
          aes(color=Type,size=Type,shape=Type),show.legend = "point")+
  geom_sf(data=gb_records%>%filter(Type=="MA_AT"),
          aes(color=Type,size=Type,shape=Type),show.legend = "point")+
  geom_point(data=radnets,aes(x=lon,y=lat,size="Rad",color="Rad",shape="Rad"))+
  geom_polygon(data=corner_points,aes(x=x,y=y),fill=NA,color="black",size=1)+
  geom_sf(data=st_as_sfc(st_bbox(study_extent)),color="darkred",size=1.5,fill=NA,show.legend = F)+
  scale_linetype_manual("Legend",breaks = c("State","County"),
                        values = c("twodash","twodash"))+
  scale_fill_manual("Legend",breaks = c("State","County"),
                    values = c("gray95",NA))+
  scale_size_manual("Legend",breaks = c("State","County","NC_AC","MA_LS","MA_AC","MA_AT","Rad"),
                    values = c(0.75,0.5,0.75,0.75,0.75,2,3))+
  scale_shape_manual("Legend",breaks = c("NC_AC","MA_LS","MA_AC","MA_AT","Rad"),
                     values = c(7,10,13,17,16))+
  scale_color_manual(NULL,breaks = c("State","County","NC_AC","MA_LS","MA_AC","MA_AT","Rad"),
                     values = c("Black","Gray50","#2c7bb6","#2c7bb6","#2c7bb6","#d7191c","#ffffbf"),
                     labels=c("State","County","Air Chek","Niton","Accustar","Alpha-Track","RadNet Monitor"),
                     guide = guide_legend(ncol=4,override.aes = list(linetype = c(rep("twodash",2),rep("blank",5)), 
                                                                     shape = c(rep(NA,2),7,10,13,17,19),
                                                                     size=c(1,0.55,2,2,2,3,3),
                                                                     color=c("black","gray50","#2c7bb6","#2c7bb6","#2c7bb6","#d7191c","#ffffbf"))))+
  theme(
    panel.background = element_rect(fill="aliceblue"),
    panel.grid = element_line(size=1,linetype = "solid",color="aliceblue"),
    panel.border = element_rect(size=2,fill = NA),
    legend.position = c(0.5,0.01),
    legend.background = element_rect(fill="white",color="black"),
    legend.title = element_text(size=13),
    legend.text = element_text(size=11),
    legend.key.width =  unit(4,"lines"),
    legend.key.height =  unit(1,"lines"),
    legend.direction = "horizontal",
    legend.box="vertical",
    legend.box.spacing = unit(5,"lines"),
    legend.justification="bottom",
    legend.margin = margin(4.5, 4.5, 4.5, 4.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )+annotation_custom(grob = us_east,
                      xmin=st_bbox(study_extent)[3]-0.1,
                      xmax=st_bbox(study_extent)[3]+1.45,
                      ymin=st_bbox(study_extent)[4]-1.325,
                      ymax=st_bbox(study_extent)[4])+
  annotation_custom(grob = boston_zoom,
                    xmin=st_bbox(study_extent)[3]-0.1,
                    xmax=st_bbox(study_extent)[3]+1.45,
                    ymin = st_bbox(study_extent)[2],
                    ymax=st_bbox(study_extent)[2]+1.25)

gb_map

library(ggplot2)
load(here::here("Data","Above_Basement_Comparison.RData"))

short_basement_quantiles=quantile(gb_records[gb_records$Duration<100,"Basement"]$Basement,seq(0.05,0.95,0.01))
short_above_quantiles=quantile(gb_records[gb_records$Duration<100,"Aboveground"]$Aboveground,seq(0.05,0.95,0.01))
long_basement_quantiles=quantile(gb_records[gb_records$Duration>100,"Basement"]$Basement,seq(0.05,0.95,0.01))
long_above_quantiles=quantile(gb_records[gb_records$Duration>100,"Aboveground"]$Aboveground,seq(0.05,0.95,0.01))

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
  geom_hline(aes(yintercept= 925*0.417),size=0.5,linetype="longdash",color="black")+
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
    legend.key.height = unit(0.6,"in"),
    legend.box.spacing = unit(0.5,"in"),
    legend.background = element_rect(fill="white",color="black"),
    legend.box.background = element_rect(fill="white"),
    legend.position = c(0.45,0.8),
    legend.text = element_text(size=10),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size=12)
  )

g_long=ggplot(data=quantiles)+
  geom_path(aes(x=tiles,y=37*long_basement,color="base_long",linetype="base_long"),size=0.5)+
  geom_point(aes(x=tiles,y=37*long_basement,color="base_long",shape="base_long",size="base_long"))+
  geom_path(aes(x=tiles,y=37*long_above,color="above_long",linetype="above_long"),size=0.5)+
  geom_point(aes(x=tiles,y=37*long_above,color="above_long",shape="above_long",size="above_long"))+
  geom_path(data=quantiles[quantiles$long_above>0,],aes(x=tiles,y=925*long_ratio,color="sm_long",linetype="sm_long"),se=F,size=1)+
  geom_point(data=quantiles[quantiles$long_above>0,],aes(x=tiles,y=925*long_ratio,color="sm_long",shape="sm_long",size="sm_long"))+
  geom_hline(aes(yintercept= 925*0.417),size=0.5,linetype="longdash",color="black")+
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
    legend.key.height = unit(0.6,"in"),
    legend.box.spacing = unit(0.5,"in"),
    legend.background = element_rect(fill="white",color="black"),
    legend.box.background = element_rect(fill="white"),
    legend.position = c(0.45,0.8),
    legend.text = element_text(size=10),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size=12)
  )

library(cowplot)
cowplot::plot_grid(g_short,g_long,ncol=2)

ggplot(data=quantiles)+
  geom_point(aes(x=short_basement,y=short_above),color="red")+
  geom_point(aes(x=long_basement,y=long_above),color="blue")+
  geom_abline(aes(intercept=0,slope=0.45))
