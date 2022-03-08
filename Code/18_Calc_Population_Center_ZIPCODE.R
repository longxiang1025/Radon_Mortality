#The objective of this script is to extract the population center of each zipcode with a polygon
library(here)
library(sf)
library(raster)
library(dplyr)
#load basic data-------------
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#The first data file is borrowed from Fracking study, it consists of three data
#(1)the population density (1km) in 2015 from Columbia study
#(2)the shapes of zipcodes with polygons
#(3)the shapes of zipcodes without polygons
load(here::here("Data","GeoData","2015_Shapes.RData"))
zips=st_as_sf(zips)

#Use map algebra to caluclate the population center of each zipcode------------
#(1) xbar= sum(pop*x)/sum(pop)
#(2) ybar= sum(pop*y)/sum(pop)

pdm_sp=projectRaster(pdm_us,crs=prjstring,res=1000,method="ngb")
coords=coordinates(pdm_sp)
#create a raster with the x of the grid (pop density)
pdm_x=pdm_sp
values(pdm_x)=coords[,1]
names(pdm_x)="location_x"
#create a raster with the y of the grid (pop density)
pdm_y=pdm_sp
values(pdm_y)=coords[,2]
names(pdm_y)="location_y"
#create a raster of x * pop
pdm_x_pdm=pdm_sp*pdm_x
#create a raster of y * pop
pdm_y_pdm=pdm_sp*pdm_y
# use zipcode polygons to extract pdm_sp first, sum all pixels
zipcode_pdm=extract(pdm_sp,zips,fun=mean,cellnumbers=T,na.rm=T)
zipcode_pdm_x=extract(pdm_x_pdm,zips,fun=mean,na.rm=T)
zipcode_pdm_y=extract(pdm_y_pdm,zips,fun=mean,na.rm=T)
zipcode_pdm_xy=cbind.data.frame(zips$ZIP,zipcode_pdm,zipcode_pdm_x,zipcode_pdm_y)
names(zipcode_pdm_xy)=c("ZIPCODE","mean_pdm","mean_pdm_x","mean_pdm_y")
zipcode_pdm_xy$x=zipcode_pdm_xy$mean_pdm_x/zipcode_pdm_xy$mean_pdm
zipcode_pdm_xy$y=zipcode_pdm_xy$mean_pdm_y/zipcode_pdm_xy$mean_pdm
zipcode_pdm_xy=zipcode_pdm_xy[,c("ZIPCODE","x","y")]
zipcode_pdm_x=zipcode_pdm_xy%>%filter(!is.na(x))
save(file="ZIP_CODE_Pop_Center.RData",zipcode_pdm_xy)
