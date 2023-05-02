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
fit_model=function(target,data,columns,pred_data,return_imp=F,boot=0,verbose=F,rseed=500){
  control=trainControl(method="cv",number = 5,
                       savePredictions = T,returnResamp = "all",
                       verboseIter = F,
                       summaryFunction = weight_summary)
  m=caret::train(
    y=data[,target],
    x=data[,columns],
    weights=data$W,
    importance="permutation",
    metric="RMSE",
    method="ranger",
    num.trees=100,
    sample.fraction=0.65,
    replace=F,
    num.threads = 1,
    trControl=control,
    verbose=F,
    seed=rseed,
    preProc = c("center", "scale"),
    tuneGrid=data.frame(.mtry=15,.splitrule="extratrees",.min.node.size=1)
  )
  if(verbose){
    print(paste("Initial Model Complete",Sys.time()))
  }
  var_Importance=(m$finalModel$variable.importance)
  n_imp_columns=sum(var_Importance>0.01)
  var_Importance_ordered=var_Importance[order(var_Importance,decreasing = T)]
  if(n_imp_columns>40){
    imp_columns=names(var_Importance_ordered)
  }else if (n_imp_columns>10){
    imp_columns=names(var_Importance_ordered[1:(2*n_imp_columns)])
  }else{
    imp_columns=names(var_Importance_ordered[1:10])
  }
  #imp_columns=rownames(imp_columns[1:30,])
  m1=caret::train(
    y=data[,target],
    x=data[,imp_columns],
    weights=data$W,
    importance="none",
    metric="RMSE",
    method="ranger",
    num.trees=100,
    sample.fraction=0.65,
    replace=F,
    seed=rseed,
    num.threads = 1,
    trControl=control,
    verbose=F,
    preProc = c("center", "scale"),
    tuneGrid=data.frame(.mtry=n_imp_columns,.splitrule="extratrees",.min.node.size=1)
  )
  local_pred=predict(m1,pred_data)
  if(verbose){
    print(paste("Model Only On Important Variables Complete",Sys.time(),format(local_pred,digits=3)))
  }
  if(return_imp){
    test=cbind.data.frame(t(local_pred),
                          R2=corr(m$pred[,c("obs","pred")],
                                  m$pred$weights)^2,
                          t(var_Importance))
    names(test)[1:2]=c("Pred_Log_Mean_Basement","Pred_Log_Mean_Aboveground")
    names(test)[4:91]=paste0("Imp_",features)
  }else{
    test=cbind.data.frame(local_pred,
                          Basement=c(1,0),
                          R2=corr(m$pred[,c("obs","pred")],
                                  m$pred$weights)^2)
    names(test)[1:2]=c("Pred_Log_Mean","Basement")
  }
  if(boot>0){
    boot_result=matrix(nrow=boot,ncol=2,data=0)
    for(b in 1:boot){
      set.seed(b+500)
      boot_data=data[sample(1:nrow(data),
                            size=nrow(data),replace = T),]
      m_boot=caret::train(
        y=boot_data[,target],
        x=boot_data[,imp_columns],
        weights=boot_data$W,
        importance="none",
        metric="RMSE",
        method="ranger",
        num.trees=100,
        sample.fraction=0.65,
        replace=F,
        seed=rseed,
        #max.depth=35,
        num.threads = 1,
        trControl=control,
        verbose=F,
        preProc = c("center", "scale"),
        tuneGrid=data.frame(.mtry=n_imp_columns,.splitrule="extratrees",.min.node.size=1)
      )
      #boot_result[b]=
      preds=predict(m_boot,pred_data)
      names(preds)=c("Log_Basement","Log_Aboveground")
      boot_result[b,]=preds
      if(verbose){
        print(paste(b,Sys.time(),format(boot_result[b,],digits=3)))
      }
    }
    test$Boot_sd_Basement=sd(boot_result[,1])
    test$Boot_sd_Aboveground=sd(boot_result[,2])
  }
  test$N_ngb=nrow(data)
  return(test)
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
      left_join(monthly_beta,by=c("ZIPS"="ZIPCODE","Month"="month","Year"="year"))%>%
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
      candidate=training_data%>%dplyr::filter((ZIPCODE!=pred[1,"ZIPS"])|
                                                (Month!=pred[1,"Month"])|
                                                (Year!=pred[1,"Year"]))
      candidate=candidate%>%filter(N>2)
      candidate=candidate%>%dplyr::filter(SD_Log>0.1)
      
      
      #candidate$Time=r*(12*(candidate$Year-2001)+candidate$Month)
      #candidate$Time=r*calculate_month_diff(x=pred$Month,y=candidate$Month)
      
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
      local_training$W=sqrt(local_training$N)/local_training$SD_Log
      local_training$dists=as.numeric(local_training_dist)
      local_training$W=local_training$W*as.numeric(dist_w)
      local_training=local_training%>%arrange(desc(W))
      local_training=local_training%>%filter(W>1e-5)
      features=names(training_data)[c(3:4,13:18,21,23:88,91:103)]
      set.seed(500)
      log_fit=fit_model(target = "Mean_Log",
                        data = local_training,
                        columns=features,
                        pred_data = pred,
                        return_imp = T,
                        boot=10,
                        verbose = F)
      if(timeline){
        print(paste0(i," Concentration Bootstrap Completed ",Sys.time())) 
      }
      log_fit$State=State
      log_fit$ZIPCode=ZIPCode
      log_fit$Year=Year
      log_fit$Month=Month
      
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
           log_fit)
      if(timeline){
        print(paste0(i," Completed ",Sys.time())) 
      }
    }
  }
}
