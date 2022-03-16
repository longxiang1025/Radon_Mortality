library(dplyr)
library(boot)

files=list.files("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",recursive = T)
test_result=list()
l=1
for( f in files){
  load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",f))
  test_result[[l]]=test
  l=l+1
}
test_result=bind_rows(test_result)
corr(test_result[test_result$Basement==1,c("local_pred","Mean_Conc")],test_result[test_result$Basement==1,"N"])
corr(test_result[test_result$Basement==0,c("local_pred","Mean_Conc")],test_result[test_result$Basement==0,"N"])
