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

proj="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

load(here::here("Data","Medium Data","Valid_Rn_Measurement.RData"))
#Try the cutoff as 5 first, if needed, we can use smaller number
training_data=radon_month_obs
training_data=training_data%>%filter(n_units>4)
training_data$dist2fault=as.numeric(training_data$dist2fault)
training_data$gm_month=log(training_data$gm_month)
training_data=as.data.frame(training_data)
training_data=na.omit(training_data)

#Attache long/lat of zips to training dataset
load(here::here("Data","GeoData","2015_Shapes.RData"))
zip_ne=zips[zips$STATE%in%c("MA","NH","CT","RI","VT","ME"),]
zip_ne=st_as_sf(zip_ne)
zip_ne=st_transform(zip_ne,crs=proj)
zip_centroid=st_centroid(zip_ne)
zip_coord=cbind.data.frame(zip_centroid$ZIP,st_coordinates(zip_centroid))
names(zip_coord)=c("ZIPCODE","X","Y")

training_data=training_data%>%left_join(zip_coord,by=c("ZIPCODE"="ZIPCODE"))
training_data$timestamp=12*(training_data$Year-1990)+training_data$Month

lamda=0.0001
bandwidth=200

dist_matrix=st.dist(dp.locat = as.matrix(training_data[,c("X","Y")]),
                    rp.locat = as.matrix(training_data[,c("X","Y")]),
                    obs.tv =training_data$timestamp,
                    reg.tv =training_data$timestamp,
                    lamda = lamda)

ens_m<-gtwr_s(obs=m_preds,
              pred=m_preds,
              bases = paste0("M",seq(1:13),"_CV_Pred"),
              bw=bandwidth,
              kernel = "gaussian",
              dis.matrix = dist_matrix)

pred_base=m_preds[,grep("Pred",names(m_preds))]
pred_base=cbind.data.frame(1,pred_base)
names(pred_base)[1]="Intercept"

coefs=ens_m[,2:15]
pred=pred_base*coefs
pred=rowSums(pred)

m_preds$ens_pred=pred
