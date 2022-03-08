#(1)split the datasets into 10 non-overlap pieces.
#(2)For each nine pieces (training set), fit 284 base models to model the training dataset
#(3)Select the top models of each category
#(4)Use the selected models to make predictions on test set
#(5)Run a GTWR to create coefficient rasters, just based on training set
#(6)Use the generated coeffecient rasters (step 5), jointly with the results of step 4 to make predictions of test set
#(7)Evaluate the results of step 6
sect_id<-as.numeric(Sys.getenv("Sim"))

#fold (1-30)
fold_id=as.integer(sect_id/221)
mod_id=sect_id%%221+1

library(dplyr)
library(caret)
library(boot)


load(here::here("Data","Medium Data","Valid_Rn_Measurement.RData"))
load(here::here("Data","Medium Data","Lag_Rn.RData"))
load(here::here("Data","Medium Data","zipcode_coords.RData"))

load(here::here("Data","Month_CV_Folds.RData"))

##-----------------------Make Training and Test Dataset-----------------
training_data=radon_month_obs
training_data$timestamp=12*(training_data$Year-1990)+training_data$Month
training_data=training_data%>%filter(Year>2004,Year<2019)
training_data=training_data%>%filter(n_units>9)
training_data$dist2fault=as.numeric(training_data$dist2fault)
training_data$gm_month=log(training_data$gm_month)
training_data=as.data.frame(training_data)
training_data=training_data%>%left_join(zip_coord,by=c("ZIPCODE"="ZIPCODE"))
training_data=training_data%>%left_join(zipcode_rn_lag,by=c("ZIPCODE"="ZIPCODE",
                                                            "timestamp"="Timestamp"))
training_data[as.integer(substr(training_data$ZIPCODE,1,3))<28,"STATE"]="MA"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>28&as.integer(substr(training_data$ZIPCODE,1,3))<30,"STATE"]="RI"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>29&as.integer(substr(training_data$ZIPCODE,1,3))<39,"STATE"]="NH"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>38&as.integer(substr(training_data$ZIPCODE,1,3))<50,"STATE"]="ME"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>50&as.integer(substr(training_data$ZIPCODE,1,3))<60,"STATE"]="VT"
training_data[as.integer(substr(training_data$ZIPCODE,1,3))>59&as.integer(substr(training_data$ZIPCODE,1,3))<70,"STATE"]="CT"
training_data[is.na(training_data$STATE),"STATE"]="RI"
training_data=na.omit(training_data)

if(fold_id==0){
  training_set=training_data
  test_set=training_data
}else{
  training_set=training_data[indexPreds[[fold_id]],]
  test_set=training_data[!seq(1,nrow(training_data))%in%indexPreds[[fold_id]],]
}
##-----------------------Create Parameter Table--------------------------------
#random forest model: ID=1
#p1=expand.grid(ID=1,num.trees=c(20,50),max.depth = c(5,10,20),
#               mtry=c(30,45,60),min.nodes=c(1,2,5),splitrule=c("variance","extratrees"))
#neural network model: ID=2
#p2=expand.grid(ID=2,size=seq(4,12,2),decay=c(1e-4,1e-3,1e-2,1e-1))
#glmboost : ID=3
#p3=expand.grid(ID=3,mstop=c(50,500,750,1000,1250,1500))
#GAM boost: ID=4
#p4=expand.grid(ID=4,mstop=c(50,500,750,1000,1250,1500))
#Gradient Boosting Machine: ID=5
#p5=expand.grid(ID=5,n.trees=c(50,100,200), interaction.depth=c(5,15,25),
#               n.minobsinnode=c(1,3,5),shrinkage=c(0.01,0.05,0.1))

#para_list=list(p1,p2,p3,p4,p5)
#t_length=simplify2array(lapply(para_list,nrow))
#id=c(rep(1,t_length[1]),rep(2,t_length[2]),
#     rep(3,t_length[3]),rep(4,t_length[4]),rep(5,t_length[5]))
#in_id=c(seq(1,t_length[1]),seq(1,t_length[2]),
#        seq(1,t_length[3]),seq(1,t_length[4]),seq(1,t_length[5]))
#para_table=cbind.data.frame(id,in_id)
# #a total of 221 parameter sets are tested

#save(file=here::here("Data","Medium Data","ParaList.RData"),para_list)
#save(file=here::here("Data","Medium Data","ParaTable.RData"),para_table)
load(here::here("Data","Medium Data","ParaList.RData"))
load(here::here("Data","Medium Data","ParaTable.RData"))
##-----------------------Fit Base Models------------------------------------
weight_summary<-function(data, lev = NULL, model = NULL){
  data=data[!is.na(data$pred),]
  data=data[!is.na(data$weights),]
  data$weights=data$weights/sum(data$weights)
  cor=boot::corr(data[,c("obs","pred")],w=data[,"weights"])
  #cor2=corr(data[,c("obs","pred")],w=data[,"weights"]^2)
  #cor3=corr(data[,c("obs","pred")],w=sqrt(data[,"weights"]))
  v=c(cor)
  names(v)=c("Rsquare")
  return(v)
}
CVfolds <- 10
CVrepeats <- 3

control=trainControl(method="repeatedcv", number=CVfolds,repeats = CVrepeats,
                     savePredictions = T,returnResamp = "all",
                     verboseIter = TRUE,
                     summaryFunction = weight_summary)
features=names(training_data)[c(1:2,
                                20:28,
                                32:55,
                                58:107,
                                111:113)]
id=para_table[mod_id,"id"]
in_id=para_table[mod_id,"in_id"]

if(id==1){
  #RF
  num.trees=para_list[[id]][in_id,"num.trees"]
  max.depth=para_list[[id]][in_id,"max.depth"]
  mtry=para_list[[id]][in_id,"mtry"]
  min.node.size=para_list[[id]][in_id,"min.nodes"]
  splitrule=para_list[[id]][in_id,"splitrule"]
  m=caret::train(
    y=training_set$gm_month,
    x=training_set[,features],
    weights=training_set$n_units,
    importance="impurity",
    method="ranger",
    metric="Rsquare",
    num.trees=num.trees,
    sample.fraction=0.35,
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
  #bag=para_list[[id]][in_id,"bag"]
  m=caret::train(
    y=training_set$gm_month/100,
    x=training_set[,features],
    weights=training_set$n_units,
    method="pcaNNet",
    metric="Rsquare",
    rang = 1,
    #abstol = 1.0e-10,
    reltol=1.0e-8,
    maxit=5000,
    trControl=control,
    tuneGrid=data.frame(.size=size,.decay=decay)
  )
}
if(id==3){
  #glmboost
  mstop=para_list[[id]][in_id,"mstop"]
  prune=1
  m=caret::train(
    y=training_set$gm_month,
    x=training_set[,features],
    weights=training_set$n_units,
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
    y=training_set$gm_month,
    x=training_set[,features[c(1:24,26:29,31:88)]],
    weights=training_set$n_units,
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
    y=training_set$gm_month,
    x=training_set[,features],
    weights=training_set$n_units,
    method="gbm",
    metric="Rsquare",
    trControl=control,
    tuneGrid=data.frame(.n.trees=n.trees,
                        .interaction.depth=interaction.depth,
                        .n.minobsinnode=n.minobsinnode,
                        .shrinkage=shrinkage)
  )
}

train_pred=m$pred%>%group_by(rowIndex)%>%summarise(pred=mean(pred))
train_obs=m$pred%>%group_by(rowIndex)%>%summarise(obs=mean(obs))
if(fold_id>0){
  train_id=indexPreds[[fold_id]]
  test_pred=predict(m,test_set)
  test_obs=test_set$gm_month
  test_id=which(!seq(1,nrow(training_data))%in%indexPreds[[fold_id]])
  
  train_cv=cbind.data.frame(train_id,train_obs$obs,train_pred$pred,training_set$n_units)
  test_cv=cbind.data.frame(test_id,test_obs,test_pred,test_set$n_units)
  names(train_cv)=c("ID","Obs","Pred","Weight")
  names(test_cv)=c("ID","Obs","Pred","Weight")
  
  train_cv$Model=id
  train_cv$Para=in_id
  test_cv$Model=id
  test_cv$Para=in_id
  save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",fold_id,"_",mod_id,".RData"),
       train_cv,test_cv)
}else{
  train_fit=cbind.data.frame(train_obs$obs,train_pred$pred,training_set$n_units)
  names(train_fit)=c("Obs","Pred","Weight")
  train_fit$Model=id
  train_fit$Para=in_id
  save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",fold_id,"_",mod_id,".RData"),
       train_fit)
}

