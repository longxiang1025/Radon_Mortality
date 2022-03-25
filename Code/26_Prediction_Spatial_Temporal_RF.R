#The objective of this script is to use the freshly fitted random forest model to predict local radon
library(sf)
library(dplyr)
library(caret)
library(boot)
library(mltools)

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

control=trainControl(method="repeatedcv", number=10,repeats = 1,
                     savePredictions = F,returnResamp = "all",
                     verboseIter = FALSE,
                     summaryFunction = weight_summary)

#Generate parameters of this run---------------------------------------------------
#The range is 0:9999
rnum<-as.numeric(Sys.getenv("Sim"))
load(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIPCode_List/ZIPCode_list_",sample(1:50,1),".RData"))
load(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/Regional_Training/Regional_Training_",sample(1:50,1),".RData"))
zipcode_month_year=expand.grid(ZIPS=zipcode_list$ZIPCODE,Month=1:12,Year=2001:2020)
zipcode_month_year$ZIPS=as.character(zipcode_month_year$ZIPS)
load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIP_Geog/ZIP_Geog_",sample(1:50,1),".RData"))
#The exchange between space and time, one month is equal to r/1000 km
r=5000
#the decay rate of Gaussian kernel
d=2
#the number of nearest neighbors as training dataset
k=5000

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
#    print(paste0(i," Existed"))
  }else{
    row_record$Time=r*(12*(row_record$Year-2001)+row_record$Month)
    row_record$X=zipcode_list[zipcode_list$ZIPCODE==ZIPCode,]$x
    row_record$Y=zipcode_list[zipcode_list$ZIPCODE==ZIPCode,]$y
    load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIP_Metero/ZIP_Metero_",Year,"_",Month,".RData"))
    load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/ZIP_Beta/ZIP_Beta_",Year,"_",Month,".RData"))
    pred=row_record%>%left_join(zipcode_geog,by=c("ZIPS"="ZIPCODE"))%>%
      left_join(monthly_Beta,by=c("ZIPS"="ZIPCODE","Month"="month","Year"="year"))%>%
      left_join(monthly_Metero,by=c("ZIPS"="ZIPCODE","Month"="month","Year"="year"))
    pred$Perc_AC=0.333
    pred$Perc_LS=0.333
    pred$Perc_AirChek=0.333
    pred=rbind.data.frame(pred,pred)
    pred$Basement=c(1,0)
    if(sum(is.na(pred))>0){
      print(paste0(i," Skipped ",Sys.time())) 
      next
    }else{
      candidate=all_data%>%dplyr::filter((ZIPCODE!=row_record$ZIPS)|
                                           (Month!=row_record$Month)|
                                           (Year!=row_record$Year))
      candidate$Time=r*(12*(candidate$Year-2001)+candidate$Month)
      
      ngbs=nabor::knn(query=row_record[1,c("X","Y","Time")],
                      data = candidate[,c("X","Y","Time")],
                      k=k)
      
      local_training=candidate[ngbs$nn.idx,]
      local_training$geometry=NULL
      local_training$Basement=as.numeric(local_training$Basement)
      features=names(local_training)[c(3:5,9:14,16:20,22:78,81:93)]
      
      b=max(t(ngbs$nn.dists))/d
      dists=t(ngbs$nn.dists)
      dist_w=exp(-(dists)^2/(2*b^2))/(b*sqrt(2*pi))
      dist_w=dist_w*(1/max(dist_w))
      set.seed(500)
      m=caret::train(
        y=local_training$Mean_Conc,
        x=local_training[,features],
        weights=local_training$N*dist_w,
        importance="impurity",
        metric="RMSE",
        method="ranger",
        num.trees=50,
        sample.fraction=0.55,
        replace=F,
        max.depth=15,
        trControl=control,
        preProc = c("center", "scale"),
        tuneGrid=data.frame(.mtry=35,.splitrule="extratrees",.min.node.size=10)
      )
      local_pred=predict(m,pred)
      pred$Local_Pred=local_pred
      pred=pred%>%dplyr::select(ZIPS,Month,Year,X,Y,Basement,Local_Pred)
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
           pred)
#      print(paste0(i," Completed ",Sys.time())) 
    }
  }
}
