sect_id<-as.numeric(Sys.getenv("Sim"))

for(sect_id in 1:30){
  library(dplyr)
  library(boot)
  library(stringr)
  library(GWmodel)
  
  ti.distv=function (focal.t, obs.tv, units = "auto") 
  {
    n <- length(obs.tv)
    dist.tv <- c()
    for (t in obs.tv) {
      if (focal.t >= t) 
        dist.tv <- c(dist.tv, ti.dist(t, focal.t, units = units))
      else 
        dist.tv <- c(dist.tv, abs(ti.dist(t, focal.t, units = units)))
    }
    dist.tv
  }
  assignInNamespace("ti.distv",ti.distv,"GWmodel")
  
  source(here::here("Code","00_GTWR_NNLS_Function.R"))
  
  load(here::here("Data","Medium Data","Valid_Rn_Measurement.RData"))
  load(here::here("Data","Medium Data","Lag_Rn.RData"))
  load(here::here("Data","Medium Data","zipcode_coords.RData"))
  load(here::here("Data","Month_CV_Folds.RData"))
  base_cv_files=list.files("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV")
  base_sect_id=str_split(base_cv_files,"_",simplify = T)[,1]
  it_files=base_cv_files[base_sect_id==as.character(sect_id)]
  
  ##----------------------Load location/time information for this partition-------
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
  
  training_data$rowIndex=1:nrow(training_data)
  training_set=training_data[indexPreds[[sect_id]],]
  test_set=training_data[!seq(1,nrow(training_data))%in%indexPreds[[sect_id]],]
  ##----------------------Load base models-------------------
  base_summary=list()
  l=1
  for( f in it_files){
    load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",f))
    wr=corr(train_cv[,c("Obs","Pred")],w=train_cv$Weight)
    base_summary[[l]]=cbind.data.frame(train_cv$Model[1],train_cv$Para[1],wr,f)
    names(base_summary[[l]])=c("Model_ID","In_ID","R2","file")
    l=l+1
  }
  base_summary=bind_rows(base_summary)
  base_summary=base_summary%>%arrange(desc(R2))
  
  ##----------------------Select non-overlapping models-------
  model_list=list()
  select_preds=list()
  select_preds_test=list()
  l=1
  for( i in 1:nrow(base_summary)){
    add=T
    if(i==1){
      load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",base_summary[i,"file"]))
    }else{
      load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",base_summary[i,"file"]))
      r=cor(train_cv$Pred,train_cv$Obs)
      if(r>0.25 & (max(train_cv$Pred)<3)&(max(test_cv$Pred)<3)){
        for(j in 1:(l-1))
        {
          c=cor(train_cv$Pred,select_preds[,j])
          if(c>0.935){
            print(paste("Too close with",j))
            add=F
            break()
          }
        } 
      }else{
        add=F
      }
    }
    if(add){
      model_list[[l]]=train_cv[1,c("Model","Para")]
      if(train_cv[1,"Model"]==2){
        select_preds=bind_cols(select_preds,100*train_cv[,c("Pred")])
        select_preds_test=bind_cols(select_preds_test,100*test_cv[,c("Pred")])
      }else{
        select_preds=bind_cols(select_preds,train_cv[,c("Pred")])
        select_preds_test=bind_cols(select_preds_test,test_cv[,c("Pred")])
      }
      l=l+1 
    }
  }
  
  model_list=bind_rows(model_list)
  names(select_preds)=paste0("Pred_",(1:ncol(select_preds)))
  names(select_preds_test)=paste0("Pred_",(1:ncol(select_preds_test)))
  
  ##----------------------Use GTWR to do the ensemble----------
  load(here::here("Data","Medium Data","zipcode_coords.RData"))
  training_set=training_set%>%left_join(zip_coord)
  select_preds$x=training_set$X
  select_preds$y=training_set$Y
  select_preds$Month=training_set$Month
  select_preds$obs=training_set$gm_month
  select_preds$weights=as.numeric(training_set$n_units)
  select_preds=as.data.frame(select_preds)
  
  lamda=1e-5
  bandwidth=1000
  
  dist_matrix=st.dist(dp.locat = as.matrix(select_preds[,c("x","y")]),
                      rp.locat = as.matrix(select_preds[,c("x","y")]),
                      obs.tv =select_preds$Month,
                      reg.tv =select_preds$Month,
                      lamda = lamda)
  base_features=names(select_preds)[grepl("Pred",names(select_preds))]
  
  
  ens_m<-gtwr_s(obs=select_preds,
                pred=select_preds,
                bases = base_features,
                bw=bandwidth,
                kernel = "gaussian",
                dis.matrix = dist_matrix)
  coefs=ens_m[,2:(2+length(base_features))]
  
  pred_base=select_preds[,base_features]
  pred_base=cbind.data.frame(1,pred_base)
  names(pred_base)[1]="Intercept"
  
  cv_pred=pred_base*coefs
  cv_pred=rowSums(cv_pred)
  
  in_r2=corr(cbind.data.frame(cv_pred,select_preds$obs),w=select_preds$weights)
  print(paste(sect_id," the internal correlation is", in_r2))
  ##----------------------Use GTWR to do prediction on test set--------------
  test_set=test_set%>%left_join(zip_coord)
  select_preds_test$x=test_set$X
  select_preds_test$y=test_set$Y
  select_preds_test$Month=test_set$Month
  select_preds_test$obs=test_set$gm_month
  select_preds_test$weights=test_set$n_units
  
  dist_matrix=st.dist(dp.locat = as.matrix(select_preds_test[,c("x","y")]),
                      rp.locat = as.matrix(select_preds[,c("x","y")]),
                      obs.tv =select_preds_test$Month,
                      reg.tv =select_preds$Month,
                      lamda = lamda)
  base_features=names(select_preds_test)[grepl("Pred",names(select_preds_test))]
  
  
  ens_m<-gtwr_s(obs=select_preds,
                pred=select_preds_test,
                bases = base_features,
                bw=bandwidth,
                kernel = "gaussian",
                dis.matrix = dist_matrix)
  
  coefs=ens_m[,2:(2+length(base_features))]
  
  pred_base=select_preds_test[,base_features]
  pred_base=cbind.data.frame(1,pred_base)
  names(pred_base)[1]="Intercept"
  
  cv_pred=pred_base*coefs
  cv_pred=rowSums(cv_pred)
  ex_r2=corr(cbind.data.frame(cv_pred,select_preds_test$obs),w=select_preds_test$weights)
  print(paste(sect_id," the external correlation is", ex_r2))
  
  ##----------------------Save the results based on CV fold---------------
  test_set$fold=sect_id
  test_set$pred=cv_pred
  
  save(file=here::here("Data","Medium Data","Real_CV",paste0(sect_id,".RData")),
       test_set)
  
}
