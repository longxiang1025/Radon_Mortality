i<-as.numeric(Sys.getenv("Sim"))

library(sf)
library(dplyr)
library(caret)
library(nabor)
library(boot)
library(mltools)

parameter_table=expand.grid(ntree=c(100,300,500),
                            mtry=c(8,12,16,20,24))

print(paste0("N Tree=",parameter_table[i,"ntree"]))
print(paste0("M Try=",parameter_table[i,"mtry"]))

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

load(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/National_Training/National_Training_",sample(1:50,1),".RData"))
#Here we only use the ZCTA-level mean with N>0 for evaluation purpose (CV)
#But we use all ZCTA-level obs as training dataset
features=names(training_data)[c(2:3,8:12,14:191)]
p_control=trainControl(method="cv",number = 5,
                     savePredictions = T,
                     returnResamp = "all",
                     verboseIter = TRUE,
                     summaryFunction = weight_summary)

t_control=trainControl(method="none",
                       savePredictions = T,
                       verboseIter = TRUE,
                       summaryFunction = weight_summary)

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

#data=as.data.frame(training_data[sample(1:nrow(training_data),200000,replace=T),])
training_data$log_Rn=log(training_data$GMean)
target="log_Rn"
columns=features
training_data=as.data.frame(training_data)

m=caret::train(
  y=training_data[,target],
  x=training_data[,columns],
  weights=sqrt(training_data$N),
  importance="permutation",
  local.importance=TRUE,
  metric="RMSE",
  method="ranger",
  num.trees=parameter_table[i,"ntree"],
  replace=T,
  num.threads = 1,
  trControl=t_control,
  verbose=T,
  seed=500,
  preProc = c("center", "scale"),
  tuneGrid=data.frame(.mtry=parameter_table[i,"mtry"],.splitrule="extratrees",.min.node.size=1)
)
var_Importance=(m$finalModel$variable.importance)
print(var_Importance[order(var_Importance,decreasing = T)])

results=cbind.data.frame(m$finalModel$predictions,training_data$log_Rn,training_data$N)
names(results)=c("Pred","Obs","N")
t=results%>%filter(!is.na(Pred))%>%group_by(N)%>%summarise(c=cor(Pred,Obs,use="complete.obs"),
                                                           r=rmse(Pred,Obs),
                                                           n=length(Pred))
print(as.data.frame(t))
save(m,file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/National_MTRY_",
                   parameter_table[i,"mtry"],"_NTREE_",parameter_table[i,"ntree"],".RData"))
