sect_id=as.numeric(Sys.getenv("Sim"))

lamda=1e-5
bandwidth=1000

m=1+sect_id%%12
y=2005+as.integer(sect_id/12)
t=12*(y-1990)+m

library(caret)
library(dplyr)
library(GWmodel)
ti.distv=function (focal.t, obs.tv, units = "auto") 
{
  n <- length(obs.tv)
  dist.tv <- c()
  for (t in obs.tv) {
    if (focal.t >= t) 
      dist.tv <- c(dist.tv, ti.dist(t, focal.t, units = units))
    else 
      dist.tv <- c(dist.tv, abs(ti.dist(t, focal.t, units = units)))
  }
  dist.tv
}
assignInNamespace("ti.distv",ti.distv,"GWmodel")

source(here::here("Code","00_GTWR_NNLS_Function.R"))

#Create the prediction bases
load(here::here("Data","Medium Data","Selected_Base_Models.RData"))
load(here::here("Data","Medium Data","GB_ZIPCODE.RData"))

load(here::here("Data","Medium Data","Lag_Rn.RData"))
load(here::here("Data","Medium Data","zipcode_coords.RData"))
load(here::here("Data","Medium Data","Monthly_Mete.RData"))
load(here::here("Data","Medium Data","Rn_Geology.RData"))
load(here::here("Data","Medium Data","ZIP_Housing.RData"))

zipcode_rn_lag=zipcode_rn_lag%>%filter(Timestamp==t)
zip_mete_record=zip_mete_record%>%filter(month==m,year==y)

pred_base=zip_coord%>%filter(ZIPCODE%in%gb_zip$ZIP)%>%
  left_join(zip_geo,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zip_mete_record,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zips_house,by=c("ZIPCODE"="ZIP"))%>%
  left_join(zipcode_rn_lag,by=c("ZIPCODE"="ZIPCODE"))

names(pred_base)[names(pred_base)=="year.x"]="Year"
names(pred_base)[names(pred_base)=="month"]="Month"
names(pred_base)[names(pred_base)=="Timestamp"]="timestamp"
pred_base$dist2fault=as.numeric(pred_base$dist2fault)

pred_base[is.na(pred_base$Slope),"Slope"]=0
pred_base[is.na(pred_base$soilw),"soilw"]=400
pred_base[is.na(pred_base$soilm),"soilm"]=500
pred_base=na.omit(pred_base)

pred_bases=list()
for(i in 1:length(base_models)){
  pred_bases[[i]]=predict(base_models[[i]],pred_base)
}
pred_bases=do.call(cbind,pred_bases)
pred_bases=as.data.frame(pred_bases)
names(pred_bases)=paste0("M",1:13,"_Pred")
pred_bases[,4:6]=100*pred_bases[,4:6]
#calculate distance matrix between monthly level and trainning dataset
load(here::here("Data","Medium Data","Ensemble_Training_Data.RData"))


dist_matrix=st.dist(dp.locat = as.matrix(pred_base[,c("X","Y")]),
                    rp.locat = as.matrix(m_preds[,c("X","Y")]),
                    obs.tv =pred_base$timestamp,
                    reg.tv =m_preds$timestamp,
                    lamda = lamda)
monthly_pred=as.data.frame(matrix(nrow=nrow(pred_bases),ncol=3))
names(monthly_pred)=c("R1_Pred","R2_Pred","R3_Pred")
for(r in 1:3){
  bases=paste0("M",c(1:11,13),"_R",r,"_CV_Pred")
  names(pred_bases)[c(1:11,13)]=bases
  gtwr_pred=gtwr_s(obs=m_preds,
                   pred = pred_bases,
                   bases = bases,
                   kernel = "gaussian",
                   dis.matrix = dist_matrix,
                   bw=bandwidth)
  coefs=gtwr_pred[,2:14]
  pred=(cbind.data.frame(1,pred_bases[,bases]))*coefs
  pred=rowSums(pred)
  monthly_pred[,paste0("R",r,"_Pred")]=pred
}

monthly_pred$Pred=rowMeans(monthly_pred)
monthly_prediction=cbind.data.frame(pred_base$ZIPCODE,m,y,exp(monthly_pred$Pred))
names(monthly_prediction)=c("ZIPCODE","Month","Year","G_Radon")
save(file=here::here("Data","Medium Data","Monthly Prediction",paste0(y,"_",m,".RData")),monthly_prediction)
#Optional Plot
library(sf)
projstring="+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"
load(here::here("Data","Medium Data","GB_ZIPCODE.RData"))
load(here::here("Data","GeoData","2015_Shapes.RData"))
load(here::here("Data","GeoData","Boundaries.RData"))
load(here::here("Data","GeoData","Counties.RData"))

gb_zip=gb_zip%>%left_join(monthly_prediction,by=c("ZIP"="ZIPCODE"))
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
gb_zip=st_intersection(gb_zip,gb_county)
county_sf=st_intersection(county_sf,bound_sf)

month_text=month.abb[m]

g<-ggplot()+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill="gray80",show.legend = T)+
  geom_sf(data=gb_zip,aes(size="ZCTA",color="ZCTA",linetype="ZCTA",fill=G_Radon))+
  geom_sf(data=county_sf,
          aes(size="County",color="County",linetype="County"),fill=NA,show.legend = T)+
  geom_sf(data=bound_sf,
          aes(linetype="State",size="State",color="State"),fill=NA,show.legend = T)+
  coord_sf(xlim=c(st_bbox(gb_zip)[1],st_bbox(gb_zip)[3]+0.25),ylim=c(st_bbox(gb_zip)[2]-0.35,st_bbox(gb_zip)[4]),clip = "on")+
  scale_fill_gradientn("Indoor Radon (Bq/m3)",
                       colours = c("white","#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"),
                       limits=c(0,7),
                       breaks=c(1,2,3,4,5,7),
                       na.value="lightgray")+
  ggtitle(paste0(y,"-",month_text))+
  scale_color_manual(NULL,breaks=c("ZCTA","County","State"),values = c("black","black","grey20"))+
  scale_size_manual(NULL,breaks=c("ZCTA","County","State"),values = c(0.15,0.5,1))+
  scale_linetype_manual(NULL,breaks=c("ZCTA","County","State"),values=c("solid","solid","solid"))+
  guides(fill = guide_colourbar(direction = "horizontal",
                                barwidth = 15, barheight = 1.5,title.position = "top",label.vjust=1),
         color= guide_legend(label.position = "bottom",label.vjust=1))+
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

ggsave(file=here::here("Figures","Monthly_Prediction",paste0(y,"_",m,".jpg")),
       g,width=4,height =7,units = "in")
