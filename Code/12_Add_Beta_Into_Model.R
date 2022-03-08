library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(psych)

load(here::here("Data","Beta_Measurements.RData"))
load(here::here("Data","Medium Data","GB_ZIPCODE.RData"))
load(here::here("Data","Above_Basement_Comparison.RData"))

gb_records$EndDate=gb_records$StartDate+as.integer(gb_records$Duration/24)
gb_records$Measure_Interval=interval(start = gb_records$StartDate,end=gb_records$EndDate)
gb_records$beta=0
gb_records=gb_records%>%filter(Basement<100,Aboveground<100)
gb_records$L_Basement=log(gb_records$Basement)
gb_records$L_Aboveground=log(gb_records$Aboveground)
gb_records=gb_records%>%filter(Basement>0.2,Aboveground>0.2)
temp=gb_zip[,c("ZIP","POP_SQMI")]
temp=as.data.frame(temp)
temp=temp[,c("ZIP","POP_SQMI")]
temp=temp%>%distinct()
gb_records=gb_records%>%left_join(temp,
                                  by=c("ZIPCODE"="ZIP"))

month_season_table=cbind.data.frame(month=seq(1:12),
                                    season=c("Winter","Spring","Spring","Spring","Summer","Summer",
                                            "Summer","Autumn","Autumn","Autumn","Winter","Winter"))
gb_records$Month=lubridate::month(gb_records$StartDate)
gb_records=gb_records%>%left_join(month_season_table,by=c("Month"="month"))
gb_records$Type=paste0(gb_records$lab,"_",gb_records$METHOD)
gb_records%>%group_by(Type)%>%summarise(mean_base=37*median(Basement),
                                        f_q_base=37*quantile(Basement,0.25),
                                        t_q_base=37*quantile(Basement,0.75),
                                        mean_above=37*median(Aboveground),
                                        f_q_above=37*quantile(Aboveground,0.25),
                                        t_q_above=37*quantile(Aboveground,0.75),
                                        n_build=length(Type))

gb_records%>%group_by(Type)%>%summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                                        lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                                        ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                                        cor=cor(L_Aboveground,L_Basement))

gb_records%>%group_by(season)%>%
  filter(Duration<100)%>%summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                                        lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                                        ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                                        cor=cor(L_Aboveground,L_Basement),
                                        n=length(season))
gb_records%>%group_by((POP_SQMI>5000)+(POP_SQMI>1000))%>%
  summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                                                   lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                                                   ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                                                   cor=cor(L_Aboveground,L_Basement),
                                                   n=length(POP_SQMI))

gb_records%>%group_by(season)%>%filter(Duration<100)%>%
  summarise(n=length(ZIPCODE),
            gm_base=geometric.mean(Basement),
            fq_base=quantile(Basement,0.25),
            tq_base=quantile(Basement,0.75),
            gm_above=geometric.mean(Aboveground),
            fq_above=quantile(Aboveground,0.25),
            tq_above=quantile(Aboveground,0.75))




radnets<-cbind.data.frame(lat=c(43.2081,43.0718,42.3601,42.2626,41.8240),
                          lon=c(-71.5376,-70.7626,-71.0589,-71.8023,-71.4128),
                          city=c("CONCORD, NH","PORTSMOUTH, NH","BOSTON, MA","WORCESTER, MA","PROVIDENCE, RI"))

for(c in radnets$city){
  city_coords=radnets[radnets$city==c,]
  city_beta=beta_measurements%>%filter(Location==c)
  city_beta$end_date=city_beta[c(2:nrow(city_beta),nrow(city_beta)),"starting_date"]
  city_beta$Beta_interval=interval(start = city_beta$starting_date,
                                   end=city_beta$end_date)
  city_records=gb_records%>%filter(abs(Longitude-city_coords$lon)<0.35,
                                   abs(Latitude-city_coords$lat)<0.3,
                                   Duration<100)
  for(i in 1:nrow(city_records)){
    intersect=city_beta%>%filter(lubridate::int_overlaps(city_records[i,]$Measure_Interval,
                                                         city_beta$Beta_interval))
    if(nrow(intersect)>0){
      city_records[i,"beta"]=log(mean(intersect$Result))
    }
  }
  city_records=city_records[city_records$beta!=0,]
  if(nrow(city_records)>5){
  comp=cor.test(city_records$L_Aboveground,city_records$beta)
  print(paste(c,comp$estimate,comp$p.value))
  }
}
  

surface=shapefile(here::here("Data","New_England_Surface_Materials","New_England_Surface_Materials.shp"))
surface=spTransform(surface,proj4string(bound))
gb_geo=extract(surface,gb_records[,c("Longitude","Latitude")])
gb_geo=gb_geo$UNIT_NAME
gb_records$unit=gb_geo

gb_records%>%filter(unit%in%c("Glacial till sediments, mostly sandy, thin",
                              "Glaciofluvial ice-contact sediments, mostly sand and gravel, thin",
                              "Proglacial sediments, mostly coarse-grained, thick",
                              "Proglacial sediments, mostly coarse-grained, thin",
                              "Proglacial sediments, mostly fine grained, thin"))%>%
  group_by(unit)%>%summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                                        lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                                        ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                                        cor=cor(L_Aboveground,L_Basement),
                             n=length(unit))

percentile_base=quantile(gb_records$Basement,seq(0,1,0.01))
percentile_above=quantile(gb_records$Aboveground,seq(0,1,0.01))
vis_data=cbind.data.frame(q=0:100,
                          base_percentile=percentile_base,
                          above_percentile=percentile_above)
vis_data$ratio=vis_data$above_percentile/vis_data$base_percentile
  
ggplot(data = vis_data[5:95,])+
  geom_point(aes(x=q,y=base_percentile))+
  geom_point(aes(x=q,y=base_percentile/2))+
  geom_point(aes(x=q,y=above_percentile))

zipcode_list=gb_records%>%group_by(ZIPCODE)%>%summarise(n=length(ZIPCODE))
zipcode_list=zipcode_list[zipcode_list$n>10,]

temp=gb_records%>%filter(ZIPCODE%in%zipcode_list$ZIPCODE)%>%group_by(ZIPCODE)%>%summarise(ratio=exp(-diff(t.test(L_Aboveground,L_Basement)$estimate)),
                                                                                     lb=exp(t.test(L_Aboveground,L_Basement)$conf.int[1]),
                                                                                     ub=exp(t.test(L_Aboveground,L_Basement)$conf.int[2]),
                                                                                     cor=cor(L_Aboveground,L_Basement),
                                                                                     n=length(unit))
temp=gb_zip%>%left_join(temp,by=c("ZIP"="ZIPCODE"))
temp=temp[!is.na(temp$ratio),]
cor.test(temp$ratio,temp$POP_SQMI)
ggplot(data=temp)+geom_sf(aes(fill=ratio))
