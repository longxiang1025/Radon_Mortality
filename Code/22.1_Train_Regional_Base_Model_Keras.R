#The objective of this script is to train advanced nerual network model with keras
#The implementation of keras-based function in caret is junk.
#There're 432 combinations of keras parameters, so the rnum is 0:(432*10-1)
rnum<-as.numeric(Sys.getenv("Sim"))

library(caret)
library(dplyr)
library(boot)
library(keras)
library(tensorflow)
library(tfdatasets)
use_condaenv('/n/home02/loli/.conda/envs/Radon')

load(paste0("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/Scratch_Copies/Regional_Training_",random_num=sample(1:5,1),".RData"))
load("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/Regional_CV_Folds.RData")

all_data=training_data
all_data$geometry=NULL
all_data$Basement=as.numeric(all_data$Basement)
features=names(all_data)[c(3:5,9:12,14:91)]
numeric_feature=features[c(1:7,9:85)]

# keras_layer_string=c("64_32_8","64_32_4","64_16_8","64_16_4","64_8_8","64_8_4","64_4_4","64_8","64_4","64",
#                      "32_32_8","32_32_4","32_16_8","32_16_4","32_8_8","32_8_4","32_4_4","32_8","32_4","32",
#                      "16_16_8","16_16_4","16_8_8","16_8_4","16_4_4","16_8","16_4","16",
#                      "8_8_8","8_8_4","8_4_4","8_8","8_4","8",
#                      "4_4","4")
# dropout=c(TRUE,FALSE)
# d_rate=c(0,0.2,0.3,0.4)
# 
# decay=c(TRUE,FALSE)
# lamda=c(0,0.01,0.0001)
# 
# keras_para_table=expand.grid(keras_layer_string,dropout,d_rate,decay,lamda)
# names(keras_para_table)=c("layer_string","dropout","d_rate","decay","lamda")
# keras_para_table=keras_para_table%>%dplyr::filter((dropout+(d_rate>0))%in%c(0,2))
# keras_para_table=keras_para_table%>%dplyr::filter((decay+(lamda>0))%in%c(0,2))
# # 
# save(file="/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/Regional_Keras_ParaTable.RData",keras_para_table)
load(file="/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/Regional_Keras_ParaTable.RData")

build_model <- function(layer_string="8_8_8",act_function="relu",dropout=T,decay=T,d_rate=0.3,lamda=0.01) {
  input <- layer_input_from_dataset(training_data[,features[-8]])
  units=stringr::str_split(layer_string,pattern = "_",simplify = T)
  units=as.numeric(units)
  output <- input %>% 
    layer_dense_features(dense_features(spec), dtype = tf$float32)
  for( u in units){
    if(decay==T){
        output <- output%>%layer_dense(units = u, activation = "relu",
                                       kernel_regularizer = keras::regularizer_l2(lamda))
    }else{
        output <- output%>%layer_dense(units = u, activation = "relu")
    }
    if(dropout==F){
      output<-output%>%layer_dropout(rate = d_rate)
    }

  }
  output<-output%>%layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_squared_error")
    )
}

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

n_base=nrow(keras_para_table)
for( r in c(3*rnum,3*rnum+1, 3*rnum+2)){
  sect_id=1+r%%n_base
  Fold=1+as.integer(r/n_base)
  if(file.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",Fold,"/",5050+r,"_",Fold,"_",8,"_",sect_id,".RData"))){
    print(paste0(r," has been done!"))
  }else{
    
    #I failed to find the builtin function in keras to extract the predictions on validation
    #So I have to manually do it
    #Fold=5
    training_data=all_data[indexPreds[[Fold]],]
    test_data=all_data[-indexPreds[[Fold]],]
    #Split the training dataset into 10 partitions (equal to "validation_split = 0.1")
    train_validation_split=createMultiFolds(y=training_data$Mean_Conc,k=10,times = 1)
    train_validation_result=list()
    
    layer_string=keras_para_table[sect_id,"layer_string"]
    dropout=keras_para_table[sect_id,"dropout"]
    d_rate=keras_para_table[sect_id,"d_rate"]
    decay=keras_para_table[sect_id,"decay"]
    lamda=keras_para_table[sect_id,"lamda"]
    
    for(i in 1:10){
      #For each iteration, train the model and predit on its own
      keras_training_data=training_data[train_validation_split[[i]],]
      keras_validation_data=training_data[-train_validation_split[[i]],]
      spec <- feature_spec(data=keras_training_data,x=features[-8],y="Mean_Conc") %>% 
        step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
        fit()
      model <- build_model(layer_string = layer_string,
                           dropout = dropout,
                           decay = decay,
                           lamda=lamda)
      model %>% fit(
        x = keras_training_data[,features[-8]],
        y = keras_training_data$Mean_Conc,
        sample_weight=keras_training_data$N,
        epochs = 100,
        validation_split = 0.1,
        verbose = 0,
        batch_size=1028)
      
      keras_validation_predict=model%>%predict(keras_validation_data[,features[-8]])
      rowIndex=1:nrow(training_data)
      rowIndex=rowIndex[-train_validation_split[[i]]]
      keras_validation_perf=cbind.data.frame(rowIndex,keras_validation_data$Mean_Conc,
                                             keras_validation_data$N,keras_validation_predict)
      names(keras_validation_perf)=c("rowIndex","obs","N","pred")
      print(corr(keras_validation_perf[,c("obs","pred")],w=keras_validation_perf$N))
      train_validation_result[[i]]=keras_validation_perf
    }
    train_validation_result=bind_rows(train_validation_result)
    #corr(train_validation_result[,c("obs","pred")],w=train_validation_result$N)
    cv_pred_base=train_validation_result%>%arrange(rowIndex)
    cv_pred_base$ZIPCODE=training_data$ZIPCODE
    cv_pred_base$Month=training_data$Month
    cv_pred_base$Year=training_data$Year
    cv_pred_base$id=8
    cv_pred_base$in_id=sect_id
    
    #Re-fit the model on the whole training dataset
    model %>% fit(
      x = training_data[,features[-8]],
      y = training_data$Mean_Conc,
      sample_weight=training_data$N,
      epochs = 100,
      validation_split = 0.1,
      verbose = 0,
      batch_size=1028)
    
    test_pred=model%>%predict(test_data[,features[-8]])
    base_test=cbind.data.frame(test_pred,test_data[,c("ZIPCODE","Month","Year","N","Mean_Conc","SD_Conc")])
    base_test$id=8
    base_test$in_id=sect_id
    in_id=sect_id
    
    #Save the model and the prediction of the base model
    if(!dir.exists(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",Fold))){
      dir.create(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",Fold))
    }
    save(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Radon_Base_CV/",Fold,"/",5050+r,"_",Fold,"_",8,"_",in_id,".RData"),
         cv_pred_base,base_test)  
  }
}

  
  