rnum<-as.numeric(Sys.getenv("Sim"))
timeline=T
#The objective of this script is to use the freshly fitted random forest model to predict local radon
library(sf)
library(dplyr)
library(caret)
library(boot)
library(mltools)

options(warn = -1)

#Prepare a table of all zipcode, year, month of year (No run every time)----------
# load(here::here("Data","GeoData","2015_Shapes.RData"))
# zips=st_as_sf(zips)
# zips$geometry=NULL
# 
# load(here::here("Data","GeoData","ZIP_CODE_Pop_Center.RData"))
# zipcode_pdm_xy=zipcode_pdm_xy%>%left_join(zips[,c("ZIP","NAME","STATE")],by=c("ZIPCODE"="ZIP"))
# 
# States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
#          "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")
# #There're 15,276 zipcodes in our study region
# zipcode_pdm_xy=zipcode_pdm_xy%>%filter(STATE%in%States)
# zipcode_list=zipcode_pdm_xy
# #The total number of combination is 3,666,240, meaning ~367 models to fit per run
# #But the whole combination is not saved, it's going to be generated every time
# #zipcode_month_year=expand.grid(ZIPS=zipcode_list$ZIPCODE,Month=1:12,Year=2001:2020)
# #Save multiple copies in scratch storage to accelerate parallel computing
# for(i in 1:50){
#   save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIPCode_List/ZIPCode_list_",i,".RData"),
#        zipcode_list)
# }
# load(paste0("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Scratch_Copies/Regional_Training_",random_num=sample(1:10,1),".RData"))
# all_data=training_data
# for(i in 1:50){
#   save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/Regional_Training/Regional_Training_",i,".RData"),
#        all_data)
# }
# load(file=here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ZIP_Spatial_Predictor_All.RData"))
#  for(i in 1:50){
#    save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIP_Geog/ZIP_Geog_",i,".RData"),
#         zipcode_geog)
#  }
# load(file=here::here("Data","Medium Data","NE_MW_Regional_Model_Data","Monthly_Beta.RData"))
# for(y in 2001:2020){
#   for( m in 1:12){
#     monthly_Beta=monthly_beta%>%filter(year==y,month==m)
#     save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIP_Beta/ZIP_Beta_",y,"_",m,".RData"),
#          monthly_Beta)
#   }
# }
# load(file=here::here("Data","Medium Data","NE_MW_Regional_Model_Data","Monthly_Metero.RData"))
# for(y in 2001:2020){
#   for( m in 1:12){
#     monthly_Metero=monthly_weather%>%filter(year==y,month==m)
#     save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIP_Metero/ZIP_Metero_",y,"_",m,".RData"),
#          monthly_Metero)
#   }
# }

#Load the necessary functions ahead-----------------------------------------------
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

control=trainControl(method="repeatedcv", number=5,repeats = 1,
                     savePredictions = F,returnResamp = "all",
                     verboseIter = FALSE,
                     summaryFunction = weight_summary)

fit_model_2=function(target,train_data,pred_data,return_imp=F){
  m=caret::train(
    y=train_data[,target],
    x=train_data[,features],
    weights=train_data$W,
    importance="impurity",
    metric="RMSE",
    method="ranger",
    num.trees=75,
    sample.fraction=0.65,
    replace=F,
    max.depth=35,
    num.threads = 1,
    trControl=control,
    verbose=F,
    seed=54321,
    preProc = c("center", "scale"),
    tuneGrid=data.frame(.mtry=65,.splitrule="extratrees",.min.node.size=5)
  )
  local_pred=predict(m,pred_data)
  if(return_imp){
    important_vars=varImp(m)[[1]]%>%dplyr::arrange(desc(Overall))
    important_vars=t(row.names(important_vars)[1:20])
    important_vars=as.data.frame(important_vars)
    names(important_vars)=paste0("Pred_",seq(1,20))
    important_vars=important_vars %>%
      mutate(across(everything(), as.character))
    test=cbind.data.frame(local_pred,pred[,target],R2=corr(m$pred[,c("obs","pred")],m$pred$weights)^2,important_vars)
    names(test)[1:2]=paste0(c("Pred_","Obs_"),target)
  }else{
    #pred=cbind.data.frame(local_pred,
    #                      pred[,target])
    local_pred=t(local_pred)
    names(local_pred)=paste0(c("Pred_"),target)
  }
  return(local_pred)
}

sample_Training=function(data,p=0.8,replace=F){
  x=1:nrow(data)
  sample_x=sample(x,size=p*length(x),replace = replace)
  return(data[sample_x,])
}

#Generate parameters of this run---------------------------------------------------
#The range is 0:9999
load(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIPCode_List/ZIPCode_list_",sample(1:50,1),".RData"))
load(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/Regional_Training/Regional_Training_",sample(1:50,1),".RData"))
training_data$geometry=NULL
zipcode_month_year=expand.grid(ZIPS=zipcode_list$ZIPCODE,Month=1:12,Year=2001:2020)
zipcode_month_year$ZIPS=as.character(zipcode_month_year$ZIPS)
zipcode_month_year=zipcode_month_year%>%arrange(as.numeric(ZIPS)+Month+Year)
load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIP_Geog/ZIP_Geog_",sample(1:250,1),".RData"))
if(timeline){
  print(paste0(rnum," Finish Load Spatial Data ",Sys.time()))
}
#The exchange between space and time, one month is equal to r/1000 km
r=150000
#the decay rate of Gaussian kernel
d=75000
#the number of nearest neighbors as training dataset
k=0

#Map rnum to 3,666,240
for(i in rnum+10000*(0:366)){
  row_record=zipcode_month_year[i,]
  ZIPCode=row_record$ZIPS
  State=zipcode_list[zipcode_list$ZIPCODE==row_record$ZIPS,"STATE"]
  Year=row_record$Year
  Month=row_record$Month
  if(file.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF_Pred/",State,"/",
                        Year,"/",
                        Month,"/",paste0(row_record$ZIPS,"_",Year,"_",Month),".RData"))){
    if(timeline){
      print(paste0(i," Existed"))
    }
  }else{
    row_record$Time=r*(12*(row_record$Year-2001)+row_record$Month)
    row_record$X=zipcode_list[zipcode_list$ZIPCODE==ZIPCode,]$x
    row_record$Y=zipcode_list[zipcode_list$ZIPCODE==ZIPCode,]$y
    load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIP_Metero/ZIP_Metero_",Year,"_",Month,".RData"))
    load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIP_Beta/ZIP_Beta_",Year,"_",Month,".RData"))
    if(timeline){
      print(paste0(i," Finish Load Temporal Data ",Sys.time()))
      }
    pred=row_record%>%left_join(zipcode_geog,by=c("ZIPS"="ZIPCODE"))%>%
      left_join(monthly_Beta,by=c("ZIPS"="ZIPCODE","Month"="month","Year"="year"))%>%
      left_join(monthly_weather,by=c("ZIPS"="ZIPCODE","Month"="month","Year"="year"))
    pred$Perc_AC=0.333
    pred$Perc_LS=0.333
    pred$Perc_AirChek=0.333
    pred=rbind.data.frame(pred,pred)
    pred$Per_Basement=c(1,0)
    if(sum(is.na(pred))>0){
      print(paste0(i," Skipped ",Sys.time())) 
      next
    }else{
      candidate=training_data%>%dplyr::filter((ZIPCODE!=row_record$ZIPS)|
                                           (Month!=row_record$Month)|
                                           (Year!=row_record$Year))
      candidate=candidate%>%filter(N>2)
      
      ngbs=nabor::knn(query=pred[1,c("X","Y")],
                      data = candidate[,c("X","Y")],
                      k=nrow(candidate))
      local_training_dist=t(ngbs$nn.dists)
      local_training_index=which(local_training_dist<r)
      local_training_index=ngbs$nn.idx[local_training_index]
      local_training=candidate[local_training_index,]
      local_training_dist=local_training_dist[local_training_dist<r,]
      #b=max(t(ngbs$nn.dists))/d
      b=d
      #dists=t(ngbs$nn.dists)
      dist_w=exp(-(local_training_dist)^2/(2*b^2))/(b*sqrt(2*pi))
      dist_w=dist_w*(1/max(dist_w))
      #Cap the weights to avoid some extraordinarily high numbers
      local_training$W=30*(local_training$N>=30)+(local_training$N<30)*local_training$N
      local_training$dists=as.numeric(local_training_dist)
      local_training$W=local_training$W*as.numeric(dist_w)
      local_training=local_training%>%arrange(desc(W))
      local_training=local_training%>%filter(W>1e-5)
      features=names(training_data)[c(3:4,13:18,21,23:88,91:103)]
      set.seed(500)
      Conc_container=list()
      for( l in 1:5){
        Conc=fit_model_2(target = "Mean_Log",
                         train_data = sample_Training(local_training,p=0.8,replace = F),
                         pred_data = pred,return_imp = F)
        Conc=as.data.frame(Conc)
        names(Conc)=c("Pred_Basement","Pred_Above")
        Conc_container[[l]]=Conc
      }
      Conc_container=bind_rows(Conc_container)
      if(timeline){
        print(paste0(i," Concentration Bootstrap Completed ",Sys.time())) 
      }
      Action_container=list()
      for( l in 1:5){
        Over_4=fit_model_2(target = "Over_4",
                         train_data = sample_Training(local_training,p=0.8,replace = F),
                         pred_data = pred,return_imp = F)
        Over_4=as.data.frame(Over_4)
        names(Over_4)=c("Pred_Basement_Over_4","Pred_Above_Over_4")
        Action_container[[l]]=Over_4
      }
      Action_container=bind_rows(Action_container)
      if(timeline){
        print(paste0(i," Action Level Bootstrap Completed ",Sys.time())) 
      }
      Conc=fit_model_2(target = "Mean_Log",train_data = local_training,
                          pred_data = pred,return_imp = F)
      
      Over_4=fit_model_2(target = "Over_4",train_data = local_training,
                          pred_data = pred,return_imp = F)
      Conc=exp(Conc)
      
      sf_pred=c(Conc, exp(apply(Conc_container,2,sd))-1,
             Over_4,apply(Action_container,2, sd))
      sf_pred=as.numeric(sf_pred)
      sf_pred=as.data.frame(t(sf_pred))
      names(sf_pred)=c("Conc_Basement","Conc_Above","SD_Basement","SD_Above",
                       "Base_Over_4","Above_Over_4","SD_Base_Over_4","SD_Above_Over_4")
      sf_pred$State=State
      sf_pred$ZIPCode=ZIPCode
      sf_pred$Year=Year
      sf_pred$Month=Month
      
      if(!dir.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF_Pred/",State,"/",
                            Year,"/",
                            Month,"/"))){
        dir.create(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF_Pred/",State,"/",
                          Year,"/",
                          Month,"/"),recursive = T)
      }
      save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF_Pred/",State,"/",
                       Year,"/",
                       Month,"/",ZIPCode,"_",Year,"_",Month,".RData"),
           sf_pred)
      if(timeline){
        print(paste0(i," Completed ",Sys.time())) 
      }
    }
  }
}
