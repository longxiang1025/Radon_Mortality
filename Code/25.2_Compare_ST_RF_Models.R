library(dplyr)
library(mltools)
library(lubridate)
library(EnvStats)

file_list=list.files("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/",
                     pattern = "ST_RF_Performance_",full.names = T)
load(file="Cleaned_Raw_Data_0610.RData")

container=list()
l=1
for(i in 1:length(file_list)){
  f=file_list[i]
  load(f)
  container[[i]]=test_result
}
container=bind_rows(container)
t=container%>%group_by(r,d,N>19,Per_Basement>0.75)%>%summarise(rmse(local_pred,Mean_Conc,N))

t=container%>%group_by(ZIPCODE,Month,Year,N)%>%summarise(v=var(local_pred-Mean_Conc))

container%>%group_by(r,d,N)%>%summarise(e=rmse(local_pred,Mean_Conc,N))%>%ggplot()+
  geom_line(aes(x=N,y=e,color=r,group=r))+
  xlim(c(0,40))
                                    