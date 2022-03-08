library(dplyr)
library(boot)

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
##-------------------Load original dataset------------------------------
load(here::here("Data","Medium Data","Valid_Rn_Measurement.RData"))
training_data=radon_month_obs
training_data$timestamp=12*(training_data$Year-1990)+training_data$Month
training_data=training_data%>%filter(Year>2004,Year<2019)
training_data=training_data%>%filter(n_units>9)
training_data$dist2fault=as.numeric(training_data$dist2fault)
training_data$gm_month=log(training_data$gm_month)
training_data=as.data.frame(training_data)
training_data[as.integer(substr(training_data$ZIPCODE,1,3))<28,"STATE"]="MA"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>28&as.integer(substr(training_data$ZIPCODE,1,3))<30,"STATE"]="RI"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>29&as.integer(substr(training_data$ZIPCODE,1,3))<39,"STATE"]="NH"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>38&as.integer(substr(training_data$ZIPCODE,1,3))<50,"STATE"]="ME"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>50&as.integer(substr(training_data$ZIPCODE,1,3))<60,"STATE"]="VT"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>59&as.integer(substr(training_data$ZIPCODE,1,3))<70,"STATE"]="CT"
training_data[is.na(training_data$STATE),"STATE"]="RI"
training_data=na.omit(training_data)
##-------------------Investigate the spatial trend of error---------------
training_data$x=cv_results$X
training_data$y=cv_results$Y
training_data$residual=cv_results$pred-cv_results$obs

zip_residual=training_data%>%group_by(ZIPCODE)%>%summarise(mean_residual=mean(residual))
load(here::here("Data","Medium Data","GB_ZIPCODE.RData"))
gb_zip_residual=gb_zip%>%dplyr::select(ZIP)%>%left_join(zip_residual,by=c("ZIP"="ZIPCODE"))
gb_zip$residual=gb_zip_residual$mean_residual

gb_zip$residual_cat=cut(gb_zip$residual,c(-2,-0.7,-0.4,-0.22,0.22,0.4,0.7,2))
library(ggplot2)
library(sf)
ggplot()+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill="gray80",show.legend = F)+
  geom_sf(data=gb_zip,aes(fill=residual))+
  geom_sf(data=county_sf,
          aes(size="County",color="County",linetype="County"),fill=NA,show.legend = F)+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill=NA,show.legend = F)+
  coord_sf(xlim=c(st_bbox(gb_zip)[1],st_bbox(gb_zip)[3]+0.25),ylim=c(st_bbox(gb_zip)[2]-0.35,st_bbox(gb_zip)[4]),clip = "on")+
  scale_fill_fermenter("Residual Ratio",
                       breaks = c(-1.5,-0.7,-0.4,-0.22,-0.1,0.1,0.22,0.4,0.7,1.5),
                       type="div",palette = "RdYlBu",guide = "coloursteps") +
  guides(fill = guide_coloursteps(direction = "horizontal",
                                barwidth = 20, barheight = 1.5,title.position = "top",label.vjust=1))+
  scale_color_manual(NULL,breaks=c("ZCTA","County","State"),values = c("black","black","grey20"))+
  scale_size_manual(NULL,breaks=c("ZCTA","County","State"),values = c(0.15,0.5,1))+
  scale_linetype_manual(NULL,breaks=c("ZCTA","County","State"),values=c("solid","solid","solid"))+
  theme(
    panel.background = element_rect(fill="aliceblue"),
    panel.grid = element_line(size=1,linetype = "solid",color="aliceblue"),
    panel.border = element_rect(size=2,fill = NA),
    legend.position = c(0.5,0.05),
    legend.background = element_rect(fill="white",color="black"),
    legend.title = element_text(size=13),
    legend.text = element_text(size=11),
    legend.key.width =  unit(2.5,"lines"),
    legend.key.height =  unit(1,"lines"),
    legend.direction = "horizontal",
    legend.box="horizontal",
    legend.justification="bottom",
    legend.margin = margin(1, 1, 1, 1),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
