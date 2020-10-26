library(sf)
library(dplyr)
library(RANN)

load(here::here("Data","Medium Data","Valid_Rn_Measurement.RData"))
projstring="+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"
#Try the cutoff as 5 first, if needed, we can use smaller number
training_data=radon_month_obs
training_data=training_data%>%filter(n_units>2)
training_data$dist2fault=as.numeric(training_data$dist2fault)
training_data$gm_month=log(training_data$gm_month)
training_data=as.data.frame(training_data)
training_data=na.omit(training_data)
training_data$timestamp=12*(training_data$Year-1990)+training_data$Month

load(here::here("Data","GeoData","2015_Shapes.RData"))
zip_ne=zips[zips$STATE%in%c("MA","NH","CT","RI","VT","ME"),]
zip_ne=st_as_sf(zip_ne)
zip_ne=st_transform(zip_ne,crs=projstring)
zip_centroid=st_centroid(zip_ne)
zip_coord=cbind.data.frame(zip_centroid$ZIP,st_coordinates(zip_centroid))
names(zip_coord)=c("ZIPCODE","X","Y")
save(file=here::here("Data","Medium Data","zipcode_coords.RData"),zip_coord)

training_data=training_data%>%left_join(zip_coord,by=c("ZIPCODE"="ZIPCODE"))

#For each month & year, select the zipcode-level radon in nearest 20 zipcodes in the past/forward two months

t_range=c(4,3,2)
r_range=c(10,15,20)

lag_stack=list()
for(i in 1:3){
  lag_tank=list()
  l=1
  for(t in 181:348){
    m_training=training_data%>%filter(timestamp>(t-t_range[i]),timestamp<(t+t_range[i]))
    print(nrow(m_training))
    for(z in 1:nrow(zip_ne)){
      src=zip_coord[z,]
      #Exclude itself
      ngbs=m_training%>%filter(!(ZIPCODE==src$ZIPCODE&timestamp==t))
      if(nrow(ngbs)<(r_range[i])){
        nk=nrow(ngbs)
      }else{
        nk=r_range[i]
      }
      #
      links=RANN::nn2(query=src[,c("X","Y")],
                      data = ngbs[,c("X","Y")],k=nk)
      index=t(links$nn.idx)
      ngb_zipcode=ngbs[index,]
      
      radon_obs=ngb_zipcode$gm_month
      radon_num=ngb_zipcode$n_units
      radon_dist=t(links$nn.dists)
      radon_dist=radon_dist+5000*abs(ngb_zipcode$timestamp-t)
      #calculate weighted average
      re<-cbind.data.frame(t,
                           src$ZIPCODE,
                           stats::weighted.mean(x=radon_obs,w=radon_num/radon_dist))
      names(re)=c("timestamp","zipcode","rn_lag")
      lag_tank[[l]]=re
      l=l+1
    }
    print(paste("Finish Time Stamp ",t))
  }
  lag_stack[[i]]=bind_rows(lag_tank)
}

zipcode_rn_lag=bind_cols(lag_stack)
zipcode_rn_lag=zipcode_rn_lag[,c(1:3,6,9)]
names(zipcode_rn_lag)=c("Timestamp","ZIPCODE","Rn_Lag_1","Rn_Lag_2","Rn_Lag_3")
save(file=here::here("Data","Medium Data","Lag_Rn.RData"),zipcode_rn_lag)

