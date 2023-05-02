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
features=names(training_data)[c(3:4,13:18,21,23:88,91:103)]

control=trainControl(method="cv",number = 5,
                     savePredictions = T,returnResamp = "all",
                     verboseIter = FALSE,
                     summaryFunction = weight_summary)
#The exchange between space and time, one month is equal to r/1000 km
r=150000
#the decay rate of Gaussian kernel
d=75000
#the number of nearest neighbors as training dataset
#k=15000
k=0

folder=paste0("ST_RF_",r,"_",k,"_",d)
if(!dir.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder))){
  dir.create(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder))
}

fit_model=function(target,data,columns,pred_data,return_imp=F,boot=0,verbose=F,rseed=500){
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
  #imp_columns=ranger::importance_pvalues(m$finalModel)
  #imp_columns=imp_columns[order(imp_columns[,1],decreasing=TRUE),]
  #init_columns=rownames(imp_columns[imp_columns[,2]<0.01,])
  #n_imp_columns=length(init_columns)
  if(n_imp_columns>40){
    imp_columns=names(var_Importance_ordered)
  }else{
    imp_columns=names(var_Importance_ordered[1:(2*n_imp_columns)])
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
    #important_vars=varImp(m1)[[1]]%>%dplyr::arrange(desc(Overall))
    #important_vars=t(row.names(important_vars)[1:20])
    #important_vars=as.data.frame(important_vars)
    #names(important_vars)=paste0("Pred_",seq(1,20))
    #important_vars=important_vars %>%
    #  mutate(across(everything(), as.character))
    test=cbind.data.frame(local_pred,pred_data[,target],
                          R2=corr(m$pred[,c("obs","pred")],
                                  m$pred$weights)^2,
                          t(var_Importance))
    names(test)[1:2]=paste0(c("Pred_","Obs_"),target)
    names(test)[4:91]=paste0("Imp_",features)
  }else{
    test=cbind.data.frame(local_pred,pred_data[,target],
                          R2=corr(m$pred[,c("obs","pred")],m$pred$weights)^2)
    names(test)[1:2]=paste0(c("Pred_","Obs_"),target)
  }
  if(boot>0){
    boot_result=vector(mode="numeric",length=boot)
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
      boot_result[b]=predict(m_boot,pred_data)
      if(verbose){
        print(paste(b,Sys.time(),format(boot_result[b],digits=3)))
      }
    }
    test$Percent_2=quantile(boot_result,0.025)
    test$Percent_5=quantile(boot_result,0.05)
    test$Percent_25=quantile(boot_result,0.25)
    test$Median=quantile(boot_result,0.5)
    test$Percent_75=quantile(boot_result,0.75)
    test$Percent_95=quantile(boot_result,0.95)
    test$Percent_97=quantile(boot_result,0.975)
    test$Boot_sd=sd(boot_result)
  }
  test$N_ngb=nrow(data)
  return(test)
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
      #Cap the weights to avoid some extraordinarily high numbers
      #local_training$W=30*(local_training$N>=30)+(local_training$N<30)*local_training$N
      #Instead of capping the W, it should be divided by the standard deivation
      local_training$W=sqrt(local_training$N)/local_training$SD_Log
      local_training$dists=as.numeric(local_training_dist)
      local_training$W=local_training$W*as.numeric(dist_w)
      local_training=local_training%>%arrange(desc(W))
      local_training=local_training%>%filter(W>1e-5)
      if(nrow(local_training)>10){
        print(paste0(Sys.time()," ",i," N: ",pred$N," n: ",nrow(local_training))) 
        set.seed(500)
        #We don't care a lot about the local fitting
        log_fit=fit_model(target = "Mean_Log",
                          data = local_training,
                          columns=features,
                          pred_data = pred,
                          return_imp = T,
                          boot=10,
                          verbose = F)
        #R2_fit=fit_model(target = "Over_2",return_imp = F)
        #R4_fit=fit_model(target="Over_4",return_imp=F)
        
        test=cbind.data.frame(pred[,c("ZIPCODE","Month","Year","X","Y","N")],
                              log_fit,
                              k,r,d)
        
        if(!dir.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder,"/",
                              evaluate_data[i,"Year"],"/",evaluate_data[i,"Month"],"/"))){
          dir.create(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder,"/",
                            evaluate_data[i,"Year"],"/",evaluate_data[i,"Month"],"/"),recursive = T)
        }
        save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder,"/",
                         evaluate_data[i,"Year"],"/",evaluate_data[i,"Month"],"/",
                         evaluate_data[i,"ZIPCODE"],".RData"),
             test)
        print(paste0(Sys.time()," ",i," Completed: Pred ",format(exp(test$Pred_Mean_Log),digits=3),
                     ";Obs ",format(exp(test$Obs_Mean_Log),digits=3),"; N ",test$N," n ",nrow(local_training))) 
      }
    }
    
  }
}
