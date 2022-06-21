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

calculate_month_diff<-function(x,y){
  x=2*pi*x/12
  y=2*pi*y/12
  d=abs(atan2(sin(x-y), cos(x-y)))
  return(12*d/(2*pi))
}

load(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/Regional_Training/Regional_Training_",sample(1:50,1),".RData"))
training_data$geometry=NULL
training_data=training_data[!is.na(training_data$Electricity_Fuel),]
#Here we only use the ZCTA-level mean with N>0 for evaluation purpose (CV)
#But we use all ZCTA-level obs as training dataset
evaluate_data=training_data%>%filter(N>4)

#Manually remove the collinear columns
features=names(training_data)[c(3:4,9:14,17,19:84,87:99)]

control=trainControl(method="cv",number = 5,
                     savePredictions = T,returnResamp = "all",
                     verboseIter = FALSE,
                     summaryFunction = weight_summary)
#The exchange between space and time, one month is equal to r/1000 km
r=500000
#the decay rate of Gaussian kernel
d=3
#the number of nearest neighbors as training dataset
k=15000

folder=paste0("ST_RF_",r,"_",k,"_",d)
if(!dir.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder))){
  dir.create(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder))
}

for(i in rnum+10000*(0:6)){
  if(i >0 & i< nrow(evaluate_data)){
    if(file.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder,"/",
                          evaluate_data[i,"Year"],"/",
                          evaluate_data[i,"Month"],"/",
                          evaluate_data[i,"ZIPCODE"],".RData"))){
      print(paste0(i," Completed"))
    }else{
      pred=evaluate_data[i,]
      pred$Time=0
      
      candidate=training_data%>%dplyr::filter((ZIPCODE!=evaluate_data[i,"ZIPCODE"])|
                                                (Month!=evaluate_data[i,"Month"])|
                                                (Year!=evaluate_data[i,"Year"]))
      candidate=candidate%>%filter(N>2)
      
      #candidate$Time=r*(12*(candidate$Year-2001)+candidate$Month)
      candidate$Time=r*calculate_month_diff(x=pred$Month,y=candidate$Month)
        
      ngbs=nabor::knn(query=pred[1,c("X","Y","Time")],
                      data = candidate[,c("X","Y","Time")],
                      k=k)
      
      local_training=candidate[ngbs$nn.idx,]
      b=max(t(ngbs$nn.dists))/d
      dists=t(ngbs$nn.dists)
      dist_w=exp(-(dists)^2/(2*b^2))/(b*sqrt(2*pi))
      dist_w=dist_w*(1/max(dist_w))
      #Cap the weights to avoid some extraordinarily high numbers
      local_training$W=20*(local_training$N>=20)+(local_training$N<20)*local_training$N
      local_training$dists=as.numeric(dists)
      local_training$W=local_training$W*as.numeric(dist_w)
      local_training=local_training%>%arrange(desc(W))
      set.seed(500)
      #We don't care a lot about the local fitting
      m=caret::train(
        y=local_training$Mean_Conc,
        x=local_training[,features],
        weights=local_training$W,
        importance="impurity",
        metric="RMSE",
        method="ranger",
        num.trees=75,
        sample.fraction=0.65,
        replace=F,
        max.depth=35,
        trControl=control,
        preProc = c("center", "scale"),
        tuneGrid=data.frame(.mtry=65,.splitrule="extratrees",.min.node.size=5)
      )
      local_pred=predict(m,pred)
      important_vars=varImp(m)[[1]]%>%dplyr::arrange(desc(Overall))
      important_vars=t(row.names(important_vars)[1:20])
      important_vars=as.data.frame(important_vars)
      names(important_vars)=paste0("Pred_",seq(1,20))
      important_vars=important_vars %>%
        mutate(across(everything(), as.character))
      
      test=cbind.data.frame(local_pred,pred[,c("ZIPCODE","Month","Year","X","Y","N","Mean_Conc","SD_Conc","Per_LDL","Per_Basement")],k,r,d,
                            R2=corr(m$pred[,c("obs","pred")],m$pred$weights)^2,
                            important_vars)
      if(!dir.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder,"/",
                            evaluate_data[i,"Year"],"/",evaluate_data[i,"Month"],"/"))){
        dir.create(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder,"/",
                          evaluate_data[i,"Year"],"/",evaluate_data[i,"Month"],"/"),recursive = T)
      }
      save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder,"/",
                       evaluate_data[i,"Year"],"/",evaluate_data[i,"Month"],"/",
                       evaluate_data[i,"ZIPCODE"],".RData"),
           test)
      print(paste0(Sys.time()," ",i," Completed: Pred ",format(test$local_pred,digits=3),";Obs ",format(test$Mean_Conc,digits=3),"; N ",test$N))
    }
    
  }
}
