#The objective of this script is to fit local random forest models, instead of a giant national model
# An adaptive window will be used to guarantee that all sub-models are based on a sufficiently large
#training data set
i<-as.numeric(Sys.getenv("Sim"))

vis=FALSE
overwrite=F
options(warn=-1)

library(sf)
library(dplyr)
library(ggplot2)
library(proxy)
library(caret)
library(ranger)
library(boot)
library(mltools)

#four tun-able parameters. The third one n_ngb determine the size of local training dataset, which
#indirectly govern he extent of local random forest. The fourth one proximity_weight determines whether
#to use proximity to calculate weights
ntree=c(200)
mtry=c(20)
n_ngb=c(10000,50000,100000)
proximity_weight=c(T,F)
parameter_table=expand.grid(ntree,mtry,n_ngb,proximity_weight)
names(parameter_table)=c("ntree","mtry","n_ngb","proximity_weight")
v=2

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
#nrow(unique(evaluate_data[,c("ZIPCODE","year","month")]))
evaluate_list=unique(evaluate_data[,c("ZIPCODE","year","month")])
#Select the ZIPCode to work on, projecting a array of 1:138449 to 1:10000
if(i==1){
  index_range=1:13
}else{
  index_range=0:13+14*(i-1)
}
for(j in index_range){
  evaluate_item=evaluate_list[j,]
  evaluate_zipcode=evaluate_item$ZIPCODE
  evaluate_month=evaluate_item$month
  evaluate_year=evaluate_item$year
  
  print(paste(Sys.time(),evaluate_zipcode,evaluate_month,evaluate_year,"Started"))
  
  item_data=evaluate_data%>%dplyr::filter(ZIPCODE==evaluate_zipcode,
                                          month==evaluate_month,
                                          year==evaluate_year)
  
  #(Step 1) Update the training data to remove the observations from the ZIPCode
  training_data=training_data%>%filter(ZIPCODE!=evaluate_zipcode|
                                         month!=evaluate_month|
                                         year!=evaluate_year)
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
  #(Step 2) Narrow down the training dataset to the closest n_ngb unqie observations
  dists=dist(training_data[,c("X","Y")],item_data[1,c("X","Y")])
  
  for( p in 1:nrow(parameter_table)){
    #iterate through all combinations of tun-able parameters
    #if the folder doesn't exist, create the folder
    sub_folder=paste0(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/National_Model_Evaluation_V",v,"/Sub_"),
                      "NT_",parameter_table[p,"ntree"],"_",
                      "MT_",parameter_table[p,"mtry"],"_",
                      "NG_",parameter_table[p,"n_ngb"],"_",
                      "P_",parameter_table[p,"proximity_weight"])
    if(!dir.exists(sub_folder)){
      dir.create(sub_folder,recursive = T)
    }
    #If the file doesn't exist, run the model
    file_name=paste0(sub_folder,"/",evaluate_zipcode,"_",evaluate_year,"_",evaluate_month,".RData")
    if((!file.exists(file_name))|(overwrite)){
      print(paste0("ZIPCode:",evaluate_zipcode," ",
                   "Month:",evaluate_month," ",
                   "Year:",evaluate_year," ",
                   "NTree:",parameter_table[p,"ntree"]," ",
                   "MTry:",parameter_table[p,"mtry"]," ",
                   "N_NGB:",parameter_table[p,"n_ngb"]," ",
                   "P_Weighted:",parameter_table[p,"proximity_weight"]))
      dist_cutoff=quantile(dists,parameter_table[p,"n_ngb"]/length(dists))
      print(dist_cutoff/1000)
      #(Step 3) Calculate the weight of nearby observation as a joint function of N and distance
      local_training_data=training_data[which(dists<=dist_cutoff),]
      if(parameter_table[p,"proximity_weight"]){
        dist_w=exp(-(dists[which(dists<=dist_cutoff)])^2/(2*(dist_cutoff/2)^2))/((dist_cutoff/2)*sqrt(2*pi))
        dist_w=dist_w*(1/max(dist_w))
        local_training_data$W=dist_w*sqrt(local_training_data$N)
      }else{
        local_training_data$W=sqrt(local_training_data$N)
      }
      m=caret::train(
        y=local_training_data[,target],
        x=local_training_data[,columns],
        weights=local_training_data$W,
        importance="permutation",
        local.importance=TRUE,
        metric="RMSE",
        method="ranger",
        num.trees=parameter_table[p,"ntree"],
        replace=T,
        keep.inbag=TRUE,
        num.threads = 1,
        trControl=t_control,
        verbose=F,
        seed=500,
        preProc = c("center", "scale"),
        tuneGrid=data.frame(.mtry=parameter_table[p,"mtry"],.splitrule="extratrees",.min.node.size=1)
      )
      var_Importance=(m$finalModel$variable.importance)
      #print(var_Importance[order(var_Importance,decreasing = T)])
      
      results=cbind.data.frame(m$finalModel$predictions,local_training_data$log_Rn,local_training_data$N)
      names(results)=c("Pred","Obs","N")
      t=results%>%filter(!is.na(Pred))%>%group_by(N)%>%summarise(c=cor(Pred,Obs,use="complete.obs"),
                                                                 r=rmse(Pred,Obs),
                                                                 n=length(Pred))
      t$ZIPCODE=evaluate_zipcode
      print(t[8:10,])
      zip_results=item_data
      zip_results$Pred_log=predict(m,item_data)
      
      zip_importance=var_Importance
      zip_prediction=zip_results%>%dplyr::select(State,ZIPCODE,month,year,N,Obs_Type,log_Rn,Pred_log)
      zip_prediction$dist_cutoff=dist_cutoff
      zip_prediction$ntree=parameter_table[p,"ntree"]
      zip_prediction$mtry=parameter_table[p,"mtry"]
      zip_prediction$n_ngb=parameter_table[p,"n_ngb"]
      zip_prediction$proximity=parameter_table[p,"proximity_weight"]
      save(file=file_name,
           var_Importance,zip_prediction,t)
    }else{
      #print(paste0(Sys.time()," ",file_name," Existed."))
    }
  }
  print(paste(Sys.time(),evaluate_zipcode,evaluate_month,evaluate_year,"Completed"))
}
