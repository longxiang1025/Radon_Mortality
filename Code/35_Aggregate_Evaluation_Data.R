#The objective of this script is to aggregate all the ZIPCode-level predictions and find the best 
#parameters for the national model
library(dplyr)
library(ggplot2)

path="/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/National_Model_Evaluation_V2/"
ntree=c(200)
mtry=c(20)
n_ngb=c(10000,50000,100000)
proximity_weight=c(T,F)
parameter_table=expand.grid(ntree,mtry,n_ngb,proximity_weight)
names(parameter_table)=c("ntree","mtry","n_ngb","proximity_weight")

all_estimations=list()

for( p in 1:nrow(parameter_table)){
  ntree=parameter_table[p,"ntree"]
  mtry=parameter_table[p,"mtry"]
  n_ngb=parameter_table[p,"n_ngb"]
  proximity=parameter_table[p,"proximity_weight"]
  
  sub_folder=paste0(path,paste0("Sub_NT_",ntree,"_MT_",mtry,"_NG_",n_ngb,"_P_",proximity,"/"))
  sub_estimations_files=list.files(sub_folder,
                                   recursive = T)
  sub_estimations=list()
  l=1
  for(f in sub_estimations_files){
    try({
      load(paste0(sub_folder,f))
      sub_estimations[[l]]=zip_prediction
    })
    l=l+1
    if(l%%1000==0){
      print(paste(Sys.time(),l))
    }
  }
  sub_estimations=dplyr::bind_rows(sub_estimations)
  all_estimations[[p]]=sub_estimations
}
all_estimations=dplyr::bind_rows(all_estimations)

save(file="National_Model_Data_All_Estimations_V2.RData",all_estimations)

load(file="National_Model_Data_All_Estimations_V1.RData")
t1=all_estimations%>%group_by(N,ntree,mtry,n_ngb,proximity)%>%summarise(cor(log_Rn,Pred_log),n=length(N))

load(file="National_Model_Data_All_Estimations_V2.RData")
t2=all_estimations%>%group_by(N,ntree,mtry,n_ngb,proximity)%>%summarise(cor(log_Rn,Pred_log),n=length(N))


all_estimations%>%filter(mtry==20,ntree==200,N<50)%>%
  group_by(N,n_ngb)%>%summarize(cor=cor(log_Rn,Pred_log))%>%
  ggplot()+geom_line(aes(x=N,y=cor,color=n_ngb,group=n_ngb))
