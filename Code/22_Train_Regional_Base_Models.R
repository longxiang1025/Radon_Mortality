#The objective of this script is to fit one of the 505 base models in one of the 30 folds
#The input of this script is the Fold ID and the ID of its base model
#The output of this script is the [1] CV pred (repeat only once) of each record in the fold (90%) using the base model.
#A follow-up script (23_XXX) will compare the CV pred of these 300+ base models, pick the top different
# base models and use two parameters (lamda and N) to ensemble them.
#The other output of this script is the prediction of each record in the remaining 10% in the fold
#rum range from 0 to 5009 (10*501-1)
rum<-as.numeric(Sys.getenv("Sim"))
#setwd("/n/koutrakis_lab/lab/Radon_Mortality/")

library(caret)
library(dplyr)
library(here)
library(boot)
library(kknn)
library(pls)
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

load("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Regional_ParaList.RData")
load("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Regional_ParaTable.RData")
load(paste0("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Scratch_Copies/Regional_Training_",random_num=sample(1:10,1),".RData"))
load("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Regional_CV_Folds.RData")

all_data=training_data
all_data$geometry=NULL
all_data$Basement=as.numeric(all_data$Basement)
#Manually remove the collinear columns
features=names(all_data)[c(3:5,9:14,16:20,22:78,81:93)]
numeric_feature=features[c(1:9,11:84)]

n_base=nrow(para_table)

for(r in c(3*rum, 3*rum+1, 3*rum+2)){
  sect_id=1+r%%n_base
  Fold=1+as.integer(r/n_base)
  id=para_table[sect_id,"id"]
  in_id=para_table[sect_id,"in_id"]
  
  if(file.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",Fold,"/",r,"_",Fold,"_",id,"_",in_id,".RData"))){
    print(paste0(r," has been done!"))
  }else{
    #Split data into training (90%) and test (10%) at the begining--------
    training_data=all_data[indexPreds[[Fold]],]
    test_data=all_data[-indexPreds[[Fold]],]
    
    
    #Create the 30 (90% vs 10%) Folds to be used in all following analysis------------------------
    #set.seed(4321)
    #CVfolds <- 10
    #CVrepeats <- 3
    #indexPreds <- createMultiFolds(training_data$Mean_Conc, k= CVfolds, times=CVrepeats)
    #save(file=here::here("Data","Medium Data","Regional_CV_Folds.RData"),indexPreds)
    
    # #Create the parameter table for 300+ base models------------------------------------------------
    #random forest model: ID=1
    # p1=expand.grid(ID=1,num.trees=c(20,100,200),max.depth = c(5,20,50),
    #                mtry=c(10,30,60),min.nodes=c(1,5,10),splitrule=c("variance","extratrees"))
    # #neural network model: ID=2
    # p2=expand.grid(ID=2,size=seq(2,11,1),decay=c(1e-4,1e-3,1e-2,1e-1))
    # #GAM : ID=3
    # p3=list()
    # for(i in 1:100){
    #   p=cbind.data.frame(i,t(sample(c(1:length(numeric_feature)),20,replace = F)))
    #   p3[[i]]=p
    # }
    # p3=bind_rows(p3)
    # p3$ID=3
    # #Gradient Boosting Machine: ID=4
    # p4=expand.grid(ID=4,n.trees=c(50,100,500), interaction.depth=seq(5,20,40),
    #                n.minobsinnode=c(3,5,10),shrinkage=c(0.01,0.05,0.1))
    # #Robust linear regression
    # p5=list()
    # for(i in 1:100){
    #   p=cbind.data.frame(i,t(sample(c(1:length(numeric_feature)),30,replace = F)))
    #   p5[[i]]=p
    # }
    # p5=bind_rows(p5)
    # p5$ID=5
    # #KNN
    # p6=expand.grid(ID=6,kmax=2:9,distance=1:2,kernel=c("rectangular","triangular","cos","gaussian"))
    # #PCA
    # p7=expand.grid(ID=7,ncomp=3:10)
    # 
    # para_list=list(p1,p2,p3,p4,p5,p6,p7)
    # t_length=simplify2array(lapply(para_list,nrow))
    # id=c(rep(1,t_length[1]),rep(2,t_length[2]),
    #      rep(3,t_length[3]),rep(4,t_length[4]),rep(5,t_length[5]),rep(6,t_length[6]),rep(7,t_length[7]))
    # in_id=c(seq(1,t_length[1]),seq(1,t_length[2]),
    #         seq(1,t_length[3]),seq(1,t_length[4]),seq(1,t_length[5]),seq(1,t_length[6]),seq(1,t_length[7]))
    # para_table=cbind.data.frame(id,in_id)
    # 
    # #a total of 845 parameter sets are tested
    # save(file=here::here("Data","Medium Data","NE_MW_Regional_Model_Data,"Regional_ParaList.RData"),para_list)
    # save(file=here::here("Data","Medium Data","NE_MW_Regional_Model_Data,"Regional_ParaTable.RData"),para_table)

    #Set the general control parameters-------------------------------------
    set.seed(4321)
    control=trainControl(method="repeatedcv", number=10,repeats = 1,
                         savePredictions = T,returnResamp = "all",
                         verboseIter = TRUE,
                         summaryFunction = weight_summary)
    set.seed(4321)
    #Run five different models----------------------------------------------
    if(id==1){
      num.trees=para_list[[id]][in_id,"num.trees"]
      max.depth=para_list[[id]][in_id,"max.depth"]
      mtry=para_list[[id]][in_id,"mtry"]
      min.node.size=para_list[[id]][in_id,"min.nodes"]
      splitrule=para_list[[id]][in_id,"splitrule"]
      m=caret::train(
        y=training_data$Mean_Conc,
        x=training_data[,features],
        weights=training_data$N,
        importance="impurity",
        metric="RMSE",
        method="ranger",
        num.trees=num.trees,
        sample.fraction=0.35,
        replace=F,
        max.depth=max.depth,
        trControl=control,
        preProc = c("center", "scale"),
        tuneGrid=data.frame(.mtry=mtry,.splitrule=splitrule,.min.node.size=min.node.size)
      )
    }
    if(id==2){
      #NN
      size=para_list[[id]][in_id,"size"]
      decay=para_list[[id]][in_id,"decay"]
      m=caret::train(
        y=training_data$Mean_Conc/100,
        x=training_data[,features],
        weights=training_data$N,
        method="nnet",
        metric="RMSE",
        rang = 1,
        reltol=1.0e-8,
        maxit=5000,
        trControl=control,
        preProc = c("center", "scale"),
        tuneGrid=data.frame(.size=size,.decay=decay)
      )
    }
    if(id==3){
      #GAM
      m=caret::train(
        y=training_data$Mean_Conc,
        x=training_data[,numeric_feature[as.numeric(para_list[[3]][in_id,2:21])]],
        weights=training_data$N,
        replace=F,
        metric="RMSE",
        method="bam",
        trControl=control,
        preProc = c("center", "scale"),
        tuneGrid=data.frame(.select=T,.method="GCV.Cp")
      )
    }
    if(id==4){
      #GBM
      n.trees=para_list[[id]][in_id,"n.trees"]
      interaction.depth=para_list[[id]][in_id,"interaction.depth"]
      n.minobsinnode=para_list[[id]][in_id,"n.minobsinnode"]
      shrinkage=para_list[[id]][in_id,"shrinkage"]
      
      m=caret::train(
        y=training_data$Mean_Conc,
        x=training_data[,features],
        weights=training_data$N,
        method="gbm",
        metric="RMSE",
        trControl=control,
        preProc = c("center", "scale"),
        tuneGrid=data.frame(.n.trees=n.trees,
                            .interaction.depth=interaction.depth,
                            .n.minobsinnode=n.minobsinnode,
                            .shrinkage=shrinkage)
      )
    }
    if(id==5){
      #robust linear model
      m=caret::train(
        y=training_data$Mean_Conc,
        x=training_data[,numeric_feature[as.numeric(para_list[[5]][in_id,2:31])]],
        weights=training_data$N,
        method="rlm",
        metric="RMSE",
        trControl=control,
        preProc = c("center", "scale"),
        tuneGrid=data.frame(
          .intercept=0,
          .psi="psi.huber"
        )
      )
    }
    if(id==6){
      #knn
      kmax=para_list[[id]][in_id,"kmax"]
      distance=para_list[[id]][in_id,"distance"]
      kernel=para_list[[id]][in_id,"kernel"]

      training_data_knn=training_data%>%filter(N>9)
      training_data_knn$rowIndex=which(training_data$N>9)
      m=caret::train(
        y=training_data_knn$Mean_Conc,
        x=training_data_knn[,numeric_feature],
        weights=training_data_knn$N,
        method="kknn",
        metric="RMSE",
        trControl=control,
        preProc = c("center", "scale"),
        tuneGrid=data.frame(
          .kmax=kmax,
          .distance=distance,
          .kernel = kernel
        )
      )
      
      validation_data_knn=training_data%>%filter(N<10)
      validation_data_pred=predict(m,validation_data_knn)
      validation_data_pred=cbind.data.frame(validation_data_pred,
                                            validation_data_knn%>%dplyr::select("Mean_Conc","N"),
                                            as.numeric(which(training_data$N<10)))
      names(validation_data_pred)=c("pred","obs","weights","rowIndex")
      validation_data_pred$kmax=kmax
      validation_data_pred$distance=distance
      validation_data_pred$kernel=kernel
      validation_data_pred$Resample="ADD"
    }
    if(id==7){
      #pca
      ncomp=para_list[[id]][in_id,"ncomp"]
      m=caret::train(
        y=training_data$Mean_Conc,
        x=training_data[,numeric_feature],
        weights=training_data$N,
        method="pcr",
        metric="RMSE",
        trControl=control,
        preProc = c("center", "scale"),
        tuneGrid=data.frame(
          .ncomp=ncomp
        )
      )
    }
    
    #Extract the CV Prediction of the Base Model
    if(id==6){
      m$pred$rowIndex=training_data_knn$rowIndex[m$pred$rowIndex]
      cv_pred_base=bind_rows(m$pred,validation_data_pred)
      cv_pred_base=cv_pred_base%>%arrange(rowIndex)
    }else{
      cv_pred_base=m$pred%>%arrange(rowIndex)
    }
    
    #Use the base model to predict the test dataset-----------------------
    base_test=predict(m,test_data)
    base_test=cbind.data.frame(base_test,test_data[,c("ZIPCODE","Month","Year","N","Mean_Conc","SD_Conc")])
    base_test$id=id
    base_test$in_id=in_id
    
    #Save the model and the prediction of the base model
    if(!dir.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",Fold))){
      dir.create(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",Fold))
    }
    save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",Fold,"/",r,"_",Fold,"_",id,"_",in_id,".RData"),
         cv_pred_base,base_test) 
  }
  
}
