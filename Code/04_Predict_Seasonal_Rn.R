sect_id<-as.numeric(Sys.getenv("Sim"))

library(dplyr)
library(tidyr)
library(here)
library(lubridate)
library(sf)
library(boot)
library(caret)

report_result<-function(m,obs,w,id,in_id){
  cv_pred=m$pred%>%group_by(rowIndex)%>%summarise(pred=mean(pred))%>%arrange(rowIndex)
  result=cbind.data.frame(obs,w,cv_pred)
  names(result)=c("obs","w","rowIndex","pred")
  cor_w=corr(result[,c("obs","pred")],w=result$w)
  rmse_w=sqrt(sum((result$obs-result$pred)^2*result$w)/sum(result$w))
  return(cbind.data.frame(id,in_id,cor_w,rmse_w))
}

#create a parameter table for model tuning
#random forest model: ID=1
#p1=expand.grid(ID=1,mtry=seq(10,50,5),min.nodes=seq(1,5))
#neural network model: ID=2
#p2=expand.grid(ID=2,size=seq(5,25,5),decay=c(1e-4,1e-3,1e-2,1e-1))
#multivariate additive regression splines : ID=3
#p3=expand.grid(ID=3,nprune=seq(20,40,5),degree=seq(2,5))
#GAM boost: ID=4
#p4=expand.grid(ID=4,mstop=c(1000,2000,5000,10000), prune=seq(1,5))
#Gradient Boosting Machine: ID=5
#p5=expand.grid(ID=5,n.trees=seq(100,2000,300), interaction.depth=seq(5,15,5),
#               n.minobsinnode=c(3,5,10),shrinkage=c(0.01,0.05,0.1))
#para_list=list(p1,p2,p3,p4,p5)
#t_length=simplify2array(lapply(para_list,nrow))
#id=c(0,rep(1,t_length[1]),rep(2,t_length[2]),
#     rep(3,t_length[3]),rep(4,t_length[4]),rep(5,t_length[5]))
#in_id=c(1,seq(1,t_length[1]),seq(1,t_length[2]),
#        seq(1,t_length[3]),seq(1,t_length[4]),seq(1,t_length[5]))
#para_table=cbind.data.frame(id,in_id)
#a total of 241 parameter sets are tested, 0 for Bayesglm
#Bayesian GLM, no need to prune

#tune_table=cbind.data.frame(para_table,cor_w=0,rmse_w=0)
#save(file=here::here("Data","Medium Data","Tune_Results.RData"),tune_table)
#save(file=here::here("Data","Medium Data","ParaList.RData"),para_list)
#save(file=here::here("Data","Medium Data","ParaTable.RData"),para_table)

load(here::here("Data","Medium Data","ParaList.RData"))
load(here::here("Data","Medium Data","ParaTable.RData"))
load(here::here("Data","Medium Data","Tune_Results.RData"))
load(here::here("Data","Medium Data","Monthly_Mete.RData"))
load(here::here("Data","Medium Data","Rn_Geology.RData"))
load(here::here("Data","Medium Data","NE_Season_Rn.RData"))
load(here::here("Data","Medium Data","ZIP_Housing.RData"))

id=para_table[sect_id,"id"]
in_id=para_table[sect_id,"in_id"]

zip_season_rn=zip_season

zip_mete_record=zip_mete_record%>%mutate(Season= cut(x=month,breaks=c(0,2,6,9,11,12),labels=c("Winter","Spring","Summer","Autumn","Winter")))
zip_season_mete=zip_mete_record%>%group_by(ZIP,year,Season)%>%summarise(
  uwnd=mean(uwnd),
  vwnd=mean(vwnd),
  temp=mean(temp),
  albedo=mean(albedo),
  hpbl=mean(hpbl),
  rhum=mean(rhum),
  snowc=mean(snowc),
  soilm=mean(soilm),
  pcp=mean(pcp),
  soilt=mean(soilt)
)


radon_obs=zip_season_rn%>%left_join(zip_geo,by=c("ZIPCODE"="ZIP"))
radon_obs=radon_obs%>%left_join(zips_house,by=c("ZIPCODE"="ZIP"))
radon_obs$Season=as.character(radon_obs$Season)
radon_obs[radon_obs$Season=="Autum","Season"]="Autumn"
radon_obs=radon_obs%>%left_join(zip_season_mete,by=c("Year"="year",
                                                     "Season"="Season",
                                                     "ZIPCODE"="ZIP"))
#valid_measurements=ne_radon%>%group_by(Year,Month,ZIPCODE)%>%summarise(mean_Rn=mean(PCI.L),var_Rn=sd(PCI.L),n_obs=n_distinct(FINGERPRINT))
#save(file=here::here("Data","Medium Data","Valid_Rn_Measurement.RData"),radon_obs)
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
indexPreds <- createMultiFolds(training_data$month_Rn, k= CVfolds, times=CVrepeats)
#save(file=here::here("Data","CV_Folds.RData"),indexPreds)
#load(here::here("Data","CV_Folds.RData"))

set.seed(4321)
control=trainControl(method="repeatedcv", number=CVfolds,repeats = CVrepeats,
                     savePredictions = T,index = indexPreds,returnResamp = "all",verboseIter = TRUE)
set.seed(4321)
if(id==1){
  #RF
  mtry=para_list[[id]][in_id,"mtry"]
  min.node.size=para_list[[id]][in_id,"min.nodes"]
  m.rf=caret::train(
    y=training_data$L_radon,
    x=training_data[,features],
    weights=training_data$n_units,
    importance="impurity",
    method="ranger",
    metric="RMSE",
    trControl=control,
    tuneGrid=data.frame(.mtry=mtry,.splitrule="variance",.min.node.size=min.node.size)
  )
  load(here::here("Data","Medium Data","Tune_Results.RData"))
  tune_table[tune_table$id==id&tune_table$in_id==in_id,]=report_result(m.rf,training_data$L_radon,w=training_data$n_units,
                id=id,in_id = in_id)
  print(report_result(m.earth,training_data$L_radon,w=training_data$n_units,
                      id=id,in_id = in_id))
  save(file=here::here("Data","Medium Data","Tune_Results.RData"),tune_table)
}
if(id==2){
  #NN
  size=para_list[[id]][in_id,"size"]
  decay=para_list[[id]][in_id,"decay"]
  m.nn=caret::train(
    y=training_data$L_radon,
    x=training_data[,features],
    weights=training_data$n_units,
    method="nnet",
    metric="RMSE",
    rang = 100,
    abstol = 1.0e-8,
    reltol=1.0e-10,
    maxit=50000,
    trControl=control,
    tuneGrid=data.frame(.size=size,.decay=decay)
  )
  load(here::here("Data","Medium Data","Tune_Results.RData"))
  tune_table[tune_table$id==id&tune_table$in_id==in_id,]=report_result(m.nn,training_data$L_radon,w=training_data$n_units,
                                                                       id=id,in_id = in_id)
  print(report_result(m.earth,training_data$L_radon,w=training_data$n_units,
                      id=id,in_id = in_id))
  save(file=here::here("Data","Medium Data","Tune_Results.RData"),tune_table)
}
if(id==3){
  #MARS
  nprune=para_list[[id]][in_id,"nprune"]
  degree=para_list[[id]][in_id,"degree"]
  m.earth=caret::train(
    y=training_data$L_radon,
    x=training_data[,features],
    weights=training_data$n_units,
    metric="RMSE",
    trControl=control,
    method="earth",
    tuneGrid=data.frame(
      .nprune=nprune,
      .degree=degree
    )
  )
  load(here::here("Data","Medium Data","Tune_Results.RData"))
  tune_table[tune_table$id==id&tune_table$in_id==in_id,]=report_result(m.earth,training_data$L_radon,w=training_data$n_units,
                                                                       id=id,in_id = in_id)
  print(report_result(m.earth,training_data$L_radon,w=training_data$n_units,
                      id=id,in_id = in_id))
  save(file=here::here("Data","Medium Data","Tune_Results.RData"),tune_table)
}
if(id==4){
  #GAMBOOST
  mstop=para_list[[id]][in_id,"mstop"]
  prune=para_list[[id]][in_id,"prune"]
  m.gamboost=caret::train(
    y=training_data$L_radon,
    x=training_data[,features],
    weights=training_data$n_units,
    metric="RMSE",
    trControl=control,
    method="gamboost",
    tuneGrid=data.frame(
      .mstop=mstop, 
      .prune=prune
    )
  )
  load(here::here("Data","Medium Data","Tune_Results.RData"))
  tune_table[tune_table$id==id&tune_table$in_id==in_id,]=report_result(m.gamboost,training_data$L_radon,w=training_data$n_units,
                                                                       id=id,in_id = in_id)
  print(report_result(m.earth,training_data$L_radon,w=training_data$n_units,
                      id=id,in_id = in_id))
  save(file=here::here("Data","Medium Data","Tune_Results.RData"),tune_table)
}
if(id==5){
  #GBM
  n.trees=para_list[[id]][in_id,"n.trees"]
  interaction.depth=para_list[[id]][in_id,"interaction.depth"]
  n.minobsinnode=para_list[[id]][in_id,"n.minobsinnode"]
  shrinkage=para_list[[id]][in_id,"shrinkage"]
  
  m.gbm=caret::train(
    y=training_data$L_radon,
    x=training_data[,features],
    weights=training_data$n_units,
    method="gbm",
    trControl=control,
    tuneGrid=data.frame(.n.trees=n.trees,
                        .interaction.depth=interaction.depth,
                        .n.minobsinnode=n.minobsinnode,
                        .shrinkage=shrinkage)
  )
  load(here::here("Data","Medium Data","Tune_Results.RData"))
  tune_table[tune_table$id==id&tune_table$in_id==in_id,]=report_result(m.gbm,training_data$L_radon,w=training_data$n_units,
                                                                       id=id,in_id = in_id)
  print(report_result(m.earth,training_data$L_radon,w=training_data$n_units,
                      id=id,in_id = in_id))
  save(file=here::here("Data","Medium Data","Tune_Results.RData"),tune_table)
}



