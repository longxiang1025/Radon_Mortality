library(dplyr)
library(boot)

#files=list.files("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF_10000_2_5000/",recursive = T)
files=list.files("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",recursive = T)

test_result=list()
l=1
for( f in files){
  #load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF_10000_2_5000/",f))
  load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",f))
  test_result[[l]]=test
  l=l+1
}
test_result=bind_rows(test_result)
corr(test_result[test_result$Basement==1,c("local_pred","Mean_Conc")],test_result[test_result$Basement==1,"N"])
corr(test_result[test_result$Basement==0,c("local_pred","Mean_Conc")],test_result[test_result$Basement==0,"N"])

month_trend=test_result%>%group_by(Month,Basement)%>%summarise(c=corr(cbind.data.frame(local_pred,Mean_Conc),N))
annual_trend=test_result%>%group_by(Year,Basement)%>%summarise(c=corr(cbind.data.frame(local_pred,Mean_Conc),N))

library(ggplot2)
library(sf)
library(scales)
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "

load(here::here("Data","GeoData","Boundaries.RData"))
bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs=prjstring)
ggplot(data=test_result)+
  geom_sf(data=bound_sf,fill="white")+
  stat_summary_hex(aes(x=X,y=Y,z=(local_pred-Mean_Conc)),
                   color="gray",size=0.25,binwidth = 50000,
                   fun = ~weighted.mean(.x,.w=N)*(ifelse(length(.x)>2,1,NA)))+
  scale_fill_stepsn(expression('Mean Residual (pCi/L)'),
                    breaks = seq(-2,2,0.4),
                    values = seq(0,1,0.1),
                    limits=c(-2,2),
                    colors = rev(RColorBrewer::brewer.pal(11,"RdBu")),
                    na.value = "red",
                    guide = guide_colorsteps(direction = "horizontal",
                                             title.position = "top",
                                             label.position = "bottom",
                                             barwidth = unit(4, "inch"),
                                             barheight=unit(0.1, "inch")))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title = element_blank())

ggplot(data=test_result)+
  geom_sf(data=bound_sf,fill="white")+
  stat_summary_hex(aes(x=X,y=Y,z=abs(local_pred-Mean_Conc)),
                   color="gray",size=0.25,binwidth = 50000,
                   fun = ~weighted.mean(.x,.w=N)*(ifelse(length(.x)>2,1,NA)))+
  scale_fill_stepsn(expression('Mean Absolute Error (pCi/L)'),
                    breaks = seq(0,4,0.4),
                    values = seq(0,1,0.1),
                    limits=c(0,4),
                    colors = (RColorBrewer::brewer.pal(11,"Reds")),
                    na.value = "red",
                    guide = guide_colorsteps(direction = "horizontal",
                                             title.position = "top",
                                             label.position = "bottom",
                                             barwidth = unit(4, "inch"),
                                             barheight=unit(0.1, "inch")))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank())
