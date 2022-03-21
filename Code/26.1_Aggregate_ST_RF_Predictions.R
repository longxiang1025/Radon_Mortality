#The objective of this script is to aggregate the predictions made by spatio-temporal random forest
# models. The script should only run on local computer and is designed to aggregate tiny files into 
# large files for each state and year.
library(dplyr)

i<-as.numeric(Sys.getenv("Sim"))

States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
         "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")
Years=2001:2020
State_Year=expand.grid(States,Years)
names(State_Year)=c("State","Year")

State=State_Year[i,"State"]
Year=State_Year[i,"Year"]

fold_path=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF_Pred/",State,"/",Year,"/")
files=list.files(fold_path,recursive = T)
pred_list=list()
l=1
for( f in files){
  load(paste0(fold_path,f))
  pred_list[[l]]=pred
  l=l+1
}
pred_list=bind_rows(pred_list)
save(file = paste0("/n/koutrakis_lab/lab/Radon_Mortality/Data/State_Year/",
                   paste0(State,"_",Year,".RData")),pred_list)


