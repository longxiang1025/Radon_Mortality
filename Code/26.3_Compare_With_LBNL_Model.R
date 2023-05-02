#The objective of this script is to evaluate the correlation between our prediction and the LBNL model
library(dplyr)

#Load all predictions into a single dataframe
all_files=list.files("/n/koutrakis_lab/lab/Radon_Mortality/Data/State_Year/",full.names = T)
all_predictions=list()
l=1
for( f in all_files){
  load(f)
  all_predictions[[l]]=pred_list
  l=l+1
}
all_predictions=bind_rows(all_predictions)
average_predictions=all_predictions%>%group_by(ZIPCode)%>%summarise(avg_base=exp(mean(Pred_Log_Mean_Basement)),
                                                                    avg_above=exp(mean(Pred_Log_Mean_Aboveground)))
#Attach a crosswalk file to know the FIPS
load("/n/koutrakis_lab/lab/Group_Data/FIPS_ZIPCODE_Crosswalk.RData")
average_predictions=average_predictions%>%left_join(FIPS_ZIPCODE_TABLE,by=c("ZIPCode"="zips"))

#Re-aggregate average by FIPS
county_prediction=average_predictions%>%group_by(fips)%>%summarise(avg_base=mean(avg_base),
                                                                   avg_above=mean(avg_above))

#Attach the LBNL prediction
LBNL_prediction=read.csv("/n/koutrakis_lab/lab/Group_Data/Radon/BerkeleyClean.csv")
LBNL_prediction$fips=formatC(LBNL_prediction$fips,width = 5,flag=0)

#Generate analysis-ready data frame
county_prediction=county_prediction%>%left_join(LBNL_prediction%>%dplyr::select(fips,GMest),
                                                by=c("fips"))
cor(county_prediction$avg_above,county_prediction$GMest,use="complete.obs")
