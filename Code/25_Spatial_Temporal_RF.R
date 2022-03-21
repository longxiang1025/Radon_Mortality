rnum<-as.numeric(Sys.getenv("Sim"))

library(sf)
library(dplyr)
library(caret)
library(nabor)
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

load(paste0("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Scratch_Copies/Regional_Training_",random_num=sample(1:10,1),".RData"))
load(paste0(paste0("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Scratch_Copies/ZIPCODE_Time_Unique_",random_num=sample(1:10,1),".RData")))
all_data=training_data
all_data$geometry=NULL
all_data$Basement=as.numeric(all_data$Basement)
#Manually remove the collinear columns
features=names(all_data)[c(3:5,9:14,16:20,22:93)]

#zipcode_time_list=unique(all_data[,c("ZIPCODE","Month","Year","X","Y")])
#for(i in 1:10){
#  save(file = paste0("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Scratch_Copies/ZIPCODE_Time_Unique_",i,".RData"),zipcode_time_list)
#}

control=trainControl(method="repeatedcv", number=10,repeats = 3,
                     savePredictions = T,returnResamp = "all",
                     verboseIter = FALSE,
                     summaryFunction = weight_summary)
#The exchange between space and time, one month is equal to r/1000 km
r=5000
#the decay rate of Gaussian kernel
d=2
#the number of nearest neighbors as training dataset
k=5000

for(i in rnum+10000*(0:5)){
  if(file.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",
                        zipcode_time_list[i,"Year"],"/",
                        zipcode_time_list[i,"Month"],"/",i,".RData"))){
    print(paste0(i," Completed"))
  }else{
    pred=all_data%>%dplyr::filter(ZIPCODE==zipcode_time_list[i,"ZIPCODE"],
                                  Month==zipcode_time_list[i,"Month"],
                                  Year==zipcode_time_list[i,"Year"])
    pred$Time=r*(12*(pred$Year-2001)+pred$Month)
    
    candidate=all_data%>%dplyr::filter((ZIPCODE!=zipcode_time_list[i,"ZIPCODE"])|
                                         (Month!=zipcode_time_list[i,"Month"])|
                                         (Year!=zipcode_time_list[i,"Year"]))
    candidate$Time=r*(12*(candidate$Year-2001)+candidate$Month)
    
    ngbs=nabor::knn(query=pred[1,c("X","Y","Time")],
                    data = candidate[,c("X","Y","Time")],
                    k=k)
    
    local_training=candidate[ngbs$nn.idx,]
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
      num.trees=150,
      sample.fraction=0.55,
      replace=F,
      max.depth=25,
      trControl=control,
      preProc = c("center", "scale"),
      tuneGrid=data.frame(.mtry=45,.splitrule="extratrees",.min.node.size=10)
    )
    local_pred=predict(m,pred)
    important_vars=varImp(m)[[1]]%>%dplyr::arrange(desc(Overall))
    important_vars=t(row.names(important_vars)[1:10])
    important_vars=as.data.frame(important_vars)
    names(important_vars)=paste0("Pred_",seq(1,10))
    important_vars=important_vars %>%
      mutate(across(everything(), as.character))
        
    test=cbind.data.frame(local_pred,pred[,c("ZIPCODE","Month","Year","X","Y","N","Mean_Conc","SD_Conc","Basement")],k,r,d,
                          R2=corr(m$pred[,c("obs","pred")],m$pred$weights)^2,
                          important_vars)
    if(!dir.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",
                          zipcode_time_list[i,"Year"],"/",zipcode_time_list[i,"Month"],"/"))){
      dir.create(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",
                        zipcode_time_list[i,"Year"],"/",zipcode_time_list[i,"Month"],"/"),recursive = T)
    }
    save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",
                     zipcode_time_list[i,"Year"],"/",zipcode_time_list[i,"Month"],"/",i,".RData"),
         test)
    print(paste0(i," Completed"))
  }

}
