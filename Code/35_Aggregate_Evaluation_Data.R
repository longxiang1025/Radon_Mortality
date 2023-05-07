#The objective of this script is to aggregate all the ZIPCode-level predictions and find the best 
#parameters for the national model
library(dplyr)
library(ggplot2)

path="/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/National_Model_Evaluation/"
#In theory, there should be 7714*24=185136 files
all_estimations_files=list.files(path,
                                 recursive = T)
all_estimations=list()
l=1
for(f in all_estimations_files){
  load(paste0(path,f))
  all_estimations[[l]]=zip_prediction
  l=l+1
  if(l%%1000==0){
    print(paste(Sys.time(),l))
  }
}
all_estimations=dplyr::bind_rows(all_estimations)
save(file="National_Model_Data_All_Estimations.RData",all_estimations)
t=all_estimations%>%filter(N==9)%>%group_by(ntree,mtry,n_ngb,proximity)%>%summarise(cor(log_Rn,Pred_log))

all_estimations%>%filter(ntree==300,n_ngb==10000,N<30)%>%
  group_by(N,mtry)%>%summarize(cor=cor(log_Rn,Pred_log))%>%
  ggplot()+geom_line(aes(x=N,y=cor,color=mtry,group=mtry))
