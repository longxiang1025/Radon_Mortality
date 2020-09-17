library(caret)
library(caretEnsemble)
library(gbm)
library(reshape2)
library(mboost)
library(boot)
library(dplyr)
library(raster)
library(sf)

load(here::here("Data","Medium Data","Valid_Rn_Measurement.RData"))
#Try the cutoff as 5 first, if needed, we can use smaller number
training_data=radon_obs%>%filter(n_units>4)
training_data=training_data%>%filter(month_Rn>0)
training_data$L_radon=log10(training_data$month_Rn)
training_data=as.data.frame(training_data)
training_data=na.omit(training_data)

features=names(training_data)[c(1,13:31,33:86)]
CVfolds <- 10
CVrepeats <- 3

#set.seed(42)
#indexPreds <- createMultiFolds(training_data$month_Rn, k= CVfolds, times=CVrepeats)
#save(file=here::here("Data","CV_Folds.RData"),indexPreds)
load(here::here("Data","CV_Folds.RData"))
weight_summary<-function(data, lev = NULL, model = NULL){
  cor=corr(data[,c("obs","pred")],w=data[,"weights"])
  rmse=sqrt(sum((data[,'obs']-data[,'pred'])^2*data[,'weights'])/sum(data[,'weights']))
  mae=sum(abs(data[,'obs']-data[,'pred'])*data[,'weights'])/sum(data[,'weights'])
  v=c(cor,rmse,mae)
  names(v)=c("Rsquare","RMSE","MAE")
  return(v)
}

set.seed(4321)
control=trainControl(method="repeatedcv", number=CVfolds,repeats = CVrepeats,
                     savePredictions = T,index = indexPreds,returnResamp = "all",
                     verboseIter = TRUE,summaryFunction = weight_summary)
set.seed(4321)
##----------------Train individual models---------------------------------------
# m.rf=caret::train(
#   y=training_data$L_radon,
#   x=training_data[,features],
#   weights=training_data$n_units,
#   importance="impurity",
#   method="ranger",
#   metric="RMSE",
#   trControl=control,
#   tuneGrid=data.frame(.mtry=40,.splitrule="variance",.min.node.size=3)
# )
# save(file=here::here("Data","Medium Data","Model_List","m_rf.RData"),m.rf)
# 
# m.nn=m.nn=caret::train(
#   y=training_data$L_radon,
#   x=training_data[,features],
#   weights=training_data$n_units,
#   method="nnet",
#   metric="RMSE",
#   rang = 100,
#   abstol = 1.0e-8,
#   reltol=1.0e-10,
#   maxit=50000,
#   trControl=control,
#   tuneGrid=data.frame(.size=10,.decay=0.01)
# )
# save(file=here::here("Data","Medium Data","Model_List","m_nn.RData"),m.nn)
# 
# m.glm=caret::train(
#   y=training_data$L_radon,
#   x=training_data[,features],
#   weights=training_data$n_units,
#   metric="RMSE",
#   trControl=control,
#   method="glmboost",
#   tuneGrid=data.frame(
#     .mstop=1000,
#     .prune=1
#   )
# )
# save(file=here::here("Data","Medium Data","Model_List","m_glm.RData"),m.glm)
# 
# m.gamboost=caret::train(
#   y=training_data$L_radon,
#   x=training_data[,features],
#   weights=training_data$n_units,
#   metric="RMSE",
#   trControl=control,
#   method="gamboost",
#   tuneGrid=data.frame(
#     .mstop=10000,
#     .prune=1
#   )
# )
# save(file=here::here("Data","Medium Data","Model_List","m_gam.RData"),m.gamboost)
# 
# m.gbm=caret::train(
#   y=training_data$L_radon,
#   x=training_data[,features],
#   weights=training_data$n_units,
#   method="gbm",
#   trControl=control,
#   tuneGrid=data.frame(.n.trees=100,
#                       .interaction.depth=10,
#                       .n.minobsinnode=5,
#                       .shrinkage=0.1)
# )
# save(file=here::here("Data","Medium Data","Model_List","m_gbm.RData"),m.gbm)
# 
# m.cart=caret::train(
#   y=training_data$L_radon,
#   x=training_data[,features],
#   weights=training_data$n_units,
#   method="rpart2",
#   trControl=control,
#   tuneGrid=data.frame(.maxdepth=5)
# )
# save(file=here::here("Data","Medium Data","Model_List","m_cart.RData"),m.cart)
# 
# m.bglm=caret::train(
#   y=training_data$L_radon,
#   x=training_data[,features],
#   method="bayesglm",
#   preProcess = "scale",
#   trControl=control)
# 
# save(file=here::here("Data","Medium Data","Model_List","m_bglm.RData"),m.bglm)
##----------------Load individual models-------------------------------
model_files<-list.files(here::here("Data","Medium Data","Model_List"),full.names = T)
for( f in model_files){
  load(f)
}
##----------------Model Ensembling-------------------------------------
multimodel <- list(m1=m.rf, 
                   m2 = m.nn,
                   m3=m.glm,
                   m4=m.gbm,
                   m5=m.gamboost,
                   m6=m.cart,
                   m7=m.bglm)
class(multimodel) <- "caretList"
##It seems that caretEnsemble currently doesn't support case weight
##----------------Costumize Model Ensembling-----------------------------
##First extract the CV results of all models
ensemble_training=list()
for(m in 1:length(multimodel)){
  model=multimodel[[m]]
  type=paste0("M",m)
  pred=model$pred%>%dplyr::select(pred,obs,weights,rowIndex,Resample)
  pred$type=type
  ensemble_training[[m]]=pred
}
ensemble_training=dplyr::bind_rows(ensemble_training)
ensemble_training$fold=as.numeric(substr(ensemble_training$Resample,5,6))
ensemble_training$rep=substr(ensemble_training$Resample,8,11)
form=as.formula("obs~pred.M1+pred.M2+pred.M3+pred.M4+pred.M5+pred.M6+pred.M7")
cv_result=list()
l=1
#Simulate a repeated ten-fold process
for( it_rep in c("Rep1","Rep2","Rep3")){
  for(f in 1:10){
    it_train=ensemble_training%>%filter(rep==it_rep,fold!=f)
    it_test=ensemble_training%>%filter(rep==it_rep,fold==f)
    
    train_obs=it_train%>%group_by(rowIndex)%>%summarise(obs=mean(obs))
    train_wt=it_train%>%group_by(rowIndex)%>%summarise(weights=mean(weights))
    train_bases=reshape(data=it_train[,c("pred","type","rowIndex")],timevar = "type",idvar="rowIndex",direction = "wide")
    
    train_set=train_obs%>%left_join(train_wt)%>%left_join(train_bases)
    
    m=caret::train(
     y=train_set$obs,
     x=train_set[,c("pred.M1","pred.M2","pred.M3","pred.M4",
                        "pred.M5","pred.M6","pred.M7")],
     weights=train_set$weights,
     importance="impurity",
     method="ranger",
     metric="RMSE",
     tuneGrid=data.frame(.mtry=5,.splitrule="variance",.min.node.size=1)
    )
    
    #m=glm(data=train_set,formula = as.formula(form),weights = train_set$weights^2)
    
    test_obs=it_test%>%group_by(rowIndex)%>%summarise(obs=mean(obs))
    test_wt=it_test%>%group_by(rowIndex)%>%summarise(weights=mean(weights))
    test_bases=reshape(data=it_test[,c("pred","type","rowIndex")],timevar = "type",idvar="rowIndex",direction = "wide")
    test_set=test_obs%>%left_join(test_wt)%>%left_join(test_bases)
    
    pred=predict(m,test_set)
    cv_it=cbind.data.frame(pred,test_obs$obs,test_set$weights,test_obs$rowIndex)
    names(cv_it)=c("cv_pred","obs","weight","rowIndex")
    cv_result[[l]]=cv_it
    l=l+1
  }
}

cv_result=bind_rows(cv_result)
cv_result=cv_result%>%group_by(rowIndex)%>%summarise(obs=mean(obs),
                                                     pred=mean(cv_pred),
                                                     weight=mean(weight))

cv_result=cv_result%>%arrange(weight)
cv_result$ori_obs=10^(cv_result$obs)
cv_result$ori_pre=10^(cv_result$pred)

corr(cv_result[,c("ori_obs","ori_pre")],w=cv_result[,"weight"]^2)

corr(cv_result[,c("obs","pred")],w=cv_result[,"weight"]^2)

save(file=here::here("Data","Medium Data","CV_Results.RData"),cv_result)

cv_result=cv_result%>%arrange(rowIndex)
cv_result$ZIP_Code=training_data$ZIPCODE
cv_result$Season=training_data$Season
cv_result$State=substr(as.character(cv_result$ZIP_Code),1,2)

corr(cv_result[cv_result$State=="01",c("obs","pred")],
     w=cv_result[cv_result$State=="01","weight"]^2)

corr(cv_result[cv_result$Season=="Winter",c("obs","pred")],
     w=cv_result[cv_result$Season=="Winter","weight"]^2)


##----------------Make profile plot based on the cross validation results----------------- 
m_abline=lm(ori_obs~ori_pre,data = cv_result,weights = cv_result$weight)

ggplot(data=cv_result%>%filter(weight>4))+
  geom_point(aes(x=37*ori_obs,y=37*ori_pre,size=weight^1.5),fill="#d7191c",shape=21,stroke=0.5,color="grey75")+
  geom_abline(intercept = 0,slope=1,linetype="dotted")+
  geom_abline(intercept = 37*m_abline$coefficients[1],slope=m_abline$coefficients[2],linetype="solid",color="navy")+
  coord_fixed(ratio=1,xlim=c(0,600),ylim = c(0,600))+
  xlab("Observed Community-level Radon levels (Bq/m3)")+
  ylab("Predicted Community-level Radon levels (Bq/m3)")+
  scale_size_area("Num of Obs",breaks=c(33,1000,2828),labels=c(10,100,200),max_size = 8.5)+
  theme_bw()

##----------------Build the final model-------------------------------
ensemble_training=list()
for(m in 1:length(multimodel)){
  model=multimodel[[m]]
  type=paste0("M",m)
  pred=model$pred%>%
    dplyr::select(pred,obs,weights,rowIndex)%>%
    group_by(rowIndex)%>%
    summarise(pred=mean(pred),
              obs=mean(obs),
              weights=mean(weights))
  pred$type=type
  ensemble_training[[m]]=pred
}
ensemble_training=dplyr::bind_rows(ensemble_training)
ensemble_bases=cbind.data.frame(
  ensemble_training%>%group_by(rowIndex)%>%summarise(obs=mean(obs)),
  (ensemble_training%>%group_by(rowIndex)%>%summarise(weights=mean(weights)))[,2],
  (ensemble_training%>%filter(type=="M1"))[,2],
  (ensemble_training%>%filter(type=="M2"))[,2],
  (ensemble_training%>%filter(type=="M3"))[,2],
  (ensemble_training%>%filter(type=="M4"))[,2],
  (ensemble_training%>%filter(type=="M5"))[,2],
  (ensemble_training%>%filter(type=="M6"))[,2],
  (ensemble_training%>%filter(type=="M7"))[,2]
)
names(ensemble_bases)=c("rowIndex","obs","weights","M1.pred","M2.pred",
                        "M3.pred","M4.pred","M5.pred","M6.pred","M7.pred")

m=caret::train(
  y=ensemble_bases$obs,
  x=ensemble_bases[,c("M1.pred","M2.pred","M3.pred","M4.pred",
                 "M5.pred","M6.pred","M7.pred")],
  weights=ensemble_bases$weights,
  importance="impurity",
  method="ranger",
  metric="RMSE",
  trControl=control,
  tuneGrid=data.frame(.mtry=5,.splitrule="variance",.min.node.size=3)
)
save(file=here::here("Data","Medium Data","Ensembled_Model.RData"),m)
