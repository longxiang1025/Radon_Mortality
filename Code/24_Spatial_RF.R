#The objective of this script is to test the effectiveness of the spatial random forest in radon modelling

library(sf)
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
all_data=training_data
all_data$geometry=NULL
all_data$Basement=as.numeric(all_data$Basement)
#Manually remove the collinear columns
features=names(all_data)[c(3:5,9:14,16:20,22:78,81:93)]

#for each zipcode, find the nearest 1000 record (Adaptive)
#there are a total of 4,565 unique zipcode in our training dataset
zipcode_list=unique(all_data[,c("ZIPCODE","X","Y")])

control=trainControl(method="repeatedcv", number=10,repeats = 3,
                     savePredictions = T,returnResamp = "all",
                     verboseIter = TRUE,
                     summaryFunction = weight_summary)

for( i in 1:nrow(zipcode_list)){
  pred=all_data[all_data$ZIPCODE==zipcode_list[i,"ZIPCODE"],]
  
  candidate=all_data[all_data$ZIPCODE!=zipcode_list[i,"ZIPCODE"],]
  
  ngbs=nabor::knn(query=pred[1,c("X","Y")],
                  data = candidate[,c("X","Y")],
                  k=10000)
  
  local_training=candidate[ngbs$nn.idx,]
  b=max(t(ngbs$nn.dists))/2
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
    num.trees=100,
    sample.fraction=0.55,
    replace=F,
    max.depth=10,
    trControl=control,
    #preProc = c("center", "scale"),
    tuneGrid=data.frame(.mtry=30,.splitrule="variance",.min.node.size=3)
  )
  local_pred=predict(m,pred)
  test=cbind.data.frame(local_pred,pred[,c("ZIPCODE","Month","Year","N","Mean_Conc","SD_Conc")])
  if(nrow(test)>2){
    r2=corr(test[,c("local_pred","Mean_Conc")],w=test$N)
    print(paste(zipcode_list[i,"ZIPCODE"],nrow(test),round(r2,3),sum(test$N),Sys.time()))
  }
}
