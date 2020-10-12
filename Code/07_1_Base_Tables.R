library(dplyr)

ma_month=ne_radon%>%filter(Type=="AccuStar")%>%
  group_by(ZIPCODE,Year)%>%
  summarise(ma_n=length(FINGERPRINT),ma_m=exp(mean(log(PCI.L))))

ma_month=ma_month%>%filter(ma_n>10)

nc_month=ne_radon%>%filter(Type=="AirChek")%>%
  group_by(ZIPCODE,Year)%>%
  summarise(nc_n=length(FINGERPRINT),nc_m=exp(mean(log(PCI.L))))

nc_month=nc_month%>%filter(nc_n>10)

comp=nc_month%>%left_join(ma_month)

comp=comp[!is.na(comp$ma_m),]
cor(comp$nc_m,comp$ma_m)

