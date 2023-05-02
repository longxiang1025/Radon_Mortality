#The objective of this script is to fit local random forest models, instead of a giant national model
# An adaptive window will be used to guarantee that all sub-models are based on a sufficiently large
#training data set
i<-as.numeric(Sys.getenv("Sim"))
ntree=300
mtry=20
vis=FALSE

library(sf)
library(dplyr)
library(ggplot2)
library(proxy)
library(caret)
library(ranger)
library(boot)
library(mltools)

weight_summary<-function(data, lev = NULL, model = NULL){
  data=data[!is.na(data$pred),]
  data=data[!is.na(data$weights),]
  data$weights=data$weights/sum(data$weights)
  cor=corr(data[,c("obs","pred")],w=data[,"weights"])
  rmse=rmse(preds = data[,"pred"],actuals = data[,"obs"],weights = data[,"weights"])
  v=c(cor^2,rmse)
  names(v)=c("Rsquare","RMSE")
  return(v)
}

t_control=trainControl(method="none",
                       savePredictions = T,
                       verboseIter = TRUE,
                       summaryFunction = weight_summary)

load(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/National_Training/National_Training_",sample(1:50,1),".RData"))
features=names(training_data)[c(2:3,8:12,14:191)]
training_data$log_Rn=log(training_data$GMean)
target="log_Rn"
columns=features
training_data=as.data.frame(training_data)
#Here we only use the ZCTA-level mean with N>4 for evaluation purpose (CV). But we use all ZCTA-level obs as training dataset
evaluate_data=training_data%>%dplyr::filter(N>4)
#The evaluate dataset are from 7714 ZIPCodes
length(unique(evaluate_data$ZIPCODE))
evaluate_zip_list=unique(evaluate_data$ZIPCODE)
#Select the ZIPCode to work on
evaluate_zip=evaluate_zip_list[i]
zip_data=evaluate_data[evaluate_data$ZIPCODE==evaluate_zip,]
#(Step 1) Update the training data to remove the observations from the ZIPCode
training_data=training_data[training_data$ZIPCODE!=evaluate_zip,]
#(Step 2) Narrow down the training dataset to the closest 50k unqie observations
dists=dist(training_data[training_data$Obs_Type=="All",c("X","Y")],
           zip_data[1,c("X","Y")])
dist_cutoff=quantile(dists,50000/nrow(training_data))
print(dist_cutoff/1000)
#(Step 3) Calculate the weight of nearby observation as a joint function of N and distance
dists=dist(training_data[,c("X","Y")],zip_data[1,c("X","Y")])
local_training_data=training_data[which(dists<=dist_cutoff),]
dist_w=exp(-(dists[which(dists<=dist_cutoff)])^2/(2*(dist_cutoff/2)^2))/((dist_cutoff/2)*sqrt(2*pi))
dist_w=dist_w*(1/max(dist_w))
local_training_data$W=dist_w*sqrt(local_training_data$N)
## For visualizaiton only
if(vis==T){
  load(here::here("Data","GeoData","2015_Shapes.RData"))
  prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
  geoprjstring<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  zips_sf=st_as_sf(zips)
  local_training_vis=local_training_data%>%group_by(ZIPCODE)%>%summarise(Total_N=sum(N))
  local_zip_sf=zips_sf%>%filter(ZIP%in%local_training_data$ZIPCODE)
  local_zip_sf=local_zip_sf%>%left_join(local_training_vis,by=c("ZIP"="ZIPCODE"))
  
  local_pred_vis=zip_data[1,]
  coordinates(local_pred_vis)=~X+Y  
  proj4string(local_pred_vis)=prjstring
  local_pred_vis=st_as_sf(local_pred_vis)
  ggplot()+
    geom_sf(data=local_zip_sf,aes(fill=Total_N))+
    geom_sf(data=local_pred_vis,color="Red",shape=17,size=5)+
    geom_sf(data=local_pred_vis,color="Red",shape=25,size=5)
}

m=caret::train(
  y=local_training_data[,target],
  x=local_training_data[,columns],
  weights=sqrt(local_training_data$W),
  importance="permutation",
  local.importance=TRUE,
  metric="RMSE",
  method="ranger",
  num.trees=ntree,
  replace=T,
  keep.inbag=TRUE,
  num.threads = 1,
  trControl=t_control,
  verbose=T,
  seed=500,
  preProc = c("center", "scale"),
  tuneGrid=data.frame(.mtry=mtry,.splitrule="extratrees",.min.node.size=1)
)
var_Importance=(m$finalModel$variable.importance)
print(var_Importance[order(var_Importance,decreasing = T)])

results=cbind.data.frame(m$finalModel$predictions,local_training_data$log_Rn,local_training_data$N)
names(results)=c("Pred","Obs","N")
t=results%>%filter(!is.na(Pred))%>%group_by(N)%>%summarise(c=cor(Pred,Obs,use="complete.obs"),
                                                           r=rmse(Pred,Obs),
                                                           n=length(Pred))
t$ZIPCODE=zip_data[1,"ZIPCODE"]
print(t)
zip_data$Pred_log=predict(m,zip_data)

zip_importance=var_Importance
zip_prediction=zip_data%>%dplyr::select(State,ZIPCODE,month,year,N,Obs_Type,log_Rn,Pred_log)
zip_prediction$dist_cutoff=dist_cutoff

save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/National_Model_Evaluation/",zip_data[1,"ZIPCODE"],".RData"),
     var_Importance,zip_prediction,t)