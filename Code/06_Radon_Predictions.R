sect_id<-as.numeric(Sys.getenv("Sim"))

library(dplyr)
library(tidyr)
library(here)
library(lubridate)
library(sf)
library(boot)
library(caret)

report_result<-function(m,obs,w,id,in_id){
  #m=m.rf
  cor_w=m$results$Rsquare
  cor_w2=m$results$Rsquare2
  cor_w3=m$results$RsquareH
  rmse_w=m$results$RMSE
  return(cbind.data.frame(id,in_id,cor_w,cor_w2,cor_w3,rmse_w))
}

weight_summary<-function(data, lev = NULL, model = NULL){
  #data=data[!is.na(data),]
  cor=corr(data[,c("obs","pred")],w=data[,"weights"])
  cor2=corr(data[,c("obs","pred")],w=data[,"weights"]^2)
  cor3=corr(data[,c("obs","pred")],w=sqrt(data[,"weights"]))
  rmse=sqrt(sum((data[,'obs']-data[,'pred'])^2*data[,'weights'])/sum(data[,'weights']))
  mae=sum(abs(data[,'obs']-data[,'pred'])*data[,'weights'])/sum(data[,'weights'])
  v=c(cor,cor2,cor3,rmse,mae)
  names(v)=c("Rsquare","Rsquare2","RsquareH","RMSE","MAE")
  return(v)
}
# #create a parameter table for model tuning
# #random forest model: ID=1
# p1=expand.grid(ID=1,num.trees=c(100,200,500,1000),max.depth = c(10,15,25,30),
#                mtry=c(20,40,60),min.nodes=c(1,2,5,10,20),splitrule=c("variance","extratrees"))
# #neural network model: ID=2
# p2=expand.grid(ID=2,size=seq(4,30,2),decay=c(1e-4,1e-3,1e-2,1e-1))
# #glmboost : ID=3
# p3=expand.grid(ID=3,mstop=c(50,100,200,500,750,1000,1250,1500,1750,2000,2500,3000,3500,4000,4500,5000))
# #GAM boost: ID=4
# p4=expand.grid(ID=4,mstop=c(50,100,200,500,750,1000,1250,1500,1750,2000,2500,3000,3500,4000,4500,5000))
# #Gradient Boosting Machine: ID=5
# p5=expand.grid(ID=5,n.trees=c(100,200,500,1000), interaction.depth=seq(5,25,5),
#               n.minobsinnode=c(3,5,10),shrinkage=c(0.01,0.05,0.1))

# para_list=list(p1,p2,p3,p4,p5)
# t_length=simplify2array(lapply(para_list,nrow))
# id=c(rep(1,t_length[1]),rep(2,t_length[2]),
#      rep(3,t_length[3]),rep(4,t_length[4]),rep(5,t_length[5]))
# in_id=c(seq(1,t_length[1]),seq(1,t_length[2]),
#         seq(1,t_length[3]),seq(1,t_length[4]),seq(1,t_length[5]))
# para_table=cbind.data.frame(id,in_id)
# #a total of 748 parameter sets are tested
 
# save(file=here::here("Data","Medium Data","ParaList.RData"),para_list)
# save(file=here::here("Data","Medium Data","ParaTable.RData"),para_table)

load(here::here("Data","Medium Data","ParaList.RData"))
load(here::here("Data","Medium Data","ParaTable.RData"))
id=para_table[sect_id,"id"]
in_id=para_table[sect_id,"in_id"]

load(here::here("Data","Medium Data","Valid_Rn_Measurement.RData"))
load(here::here("Data","Medium Data","Lag_Rn.RData"))
load(here::here("Data","Medium Data","zipcode_coords.RData"))

#Try the cutoff as 5 first, if needed, we can use smaller number
training_data=radon_month_obs
training_data$timestamp=12*(training_data$Year-1990)+training_data$Month
training_data=training_data%>%filter(Year>2004,Year<2019)
training_data=training_data%>%filter(n_units>4)
training_data$dist2fault=as.numeric(training_data$dist2fault)
training_data$gm_month=log(training_data$gm_month)
training_data=as.data.frame(training_data)
training_data=training_data%>%left_join(zip_coord,by=c("ZIPCODE"="ZIPCODE"))
training_data=training_data%>%left_join(zipcode_rn_lag,by=c("ZIPCODE"="zipcode",
                                                            "timestamp"="timestamp"))
training_data=na.omit(training_data)

features=names(training_data)[c(1:2,15:16,18:94)]
CVfolds <- 10
CVrepeats <- 3
#set.seed(4321)
#indexPreds <- createMultiFolds(training_data$gm_month, k= CVfolds, times=CVrepeats)
#save(file=here::here("Data","Month_CV_Folds.RData"),indexPreds)
load(here::here("Data","Month_CV_Folds.RData"))
set.seed(4321)
control=trainControl(method="repeatedcv", number=CVfolds,repeats = CVrepeats,
                     savePredictions = T,index = indexPreds,returnResamp = "all",
                     verboseIter = TRUE,
                     summaryFunction = weight_summary)
set.seed(4321)
if(id==1){
  #RF
  num.trees=para_list[[id]][in_id,"num.trees"]
  max.depth=para_list[[id]][in_id,"max.depth"]
  mtry=para_list[[id]][in_id,"mtry"]
  min.node.size=para_list[[id]][in_id,"min.nodes"]
  splitrule=para_list[[id]][in_id,"splitrule"]
  m=caret::train(
    y=training_data$gm_month,
    x=training_data[,features],
    weights=training_data$n_units,
    importance="impurity",
    method="ranger",
    metric="Rsquare",
    num.trees=num.trees,
    sample.fraction=0.5,
    replace=F,
    max.depth=max.depth,
    trControl=control,
    tuneGrid=data.frame(.mtry=mtry,.splitrule=splitrule,.min.node.size=min.node.size)
  )
}
if(id==2){
  #NN
  size=para_list[[id]][in_id,"size"]
  decay=para_list[[id]][in_id,"decay"]
  m=caret::train(
    y=training_data$gm_month,
    x=training_data[,features],
    weights=training_data$n_units,
    method="nnet",
    metric="Rsquare",
    rang = 1,
    #abstol = 1.0e-10,
    reltol=1.0e-8,
    maxit=50000,
    trControl=control,
    tuneGrid=data.frame(.size=size,.decay=decay)
  )
}
if(id==3){
  #glmboost
  mstop=para_list[[id]][in_id,"mstop"]
  prune=1
  m=caret::train(
    y=training_data$gm_month,
    x=training_data[,features],
    weights=training_data$n_units,
    metric="Rsquare",
    trControl=control,
    method="glmboost",
    tuneGrid=data.frame(
      .mstop=mstop,
      .prune=prune
    )
  )
}
if(id==4){
  #GAMBOOST
  mstop=para_list[[id]][in_id,"mstop"]
  prune=1
  m=caret::train(
    y=training_data$gm_month,
    x=training_data[,features[c(1:21,23:79)]],
    weights=training_data$n_units,
    metric="Rsquare",
    trControl=control,
    method="gamboost",
    tuneGrid=data.frame(
      .mstop=mstop, 
      .prune=prune
    )
  )
}
if(id==5){
  #GBM
  n.trees=para_list[[id]][in_id,"n.trees"]
  interaction.depth=para_list[[id]][in_id,"interaction.depth"]
  n.minobsinnode=para_list[[id]][in_id,"n.minobsinnode"]
  shrinkage=para_list[[id]][in_id,"shrinkage"]
  
  m=caret::train(
    y=training_data$gm_month,
    x=training_data[,features],
    weights=training_data$n_units,
    method="gbm",
    metric="Rsquare",
    trControl=control,
    tuneGrid=data.frame(.n.trees=n.trees,
                        .interaction.depth=interaction.depth,
                        .n.minobsinnode=n.minobsinnode,
                        .shrinkage=shrinkage)
  )
}

save(file=here::here("Data","Medium Data","Model_List",paste0(sect_id,"_Model.RData")),m)
