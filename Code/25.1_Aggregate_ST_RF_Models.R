library(dplyr)
library(boot)

#files=list.files("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF_10000_2_5000/",recursive = T)
files=list.files("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",recursive = T)

test_result=list()
l=1
for( f in files){
  #load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF_10000_2_5000/",f))
  load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",f))
  test_result[[l]]=test
  l=l+1
  if(l%%1000==0){
    print(l)
  }
}
test_result=bind_rows(test_result)
corr(test_result[test_result$Basement==1,c("local_pred","Mean_Conc")],test_result[test_result$Basement==1,"N"])
corr(test_result[test_result$Basement==0,c("local_pred","Mean_Conc")],test_result[test_result$Basement==0,"N"])
save(file = here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ST_RF_Performance.RData"),test_result)

load(paste0("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Scratch_Copies/Regional_Training_",random_num=sample(1:10,1),".RData"))
zipcode_state_table=unique(training_data[,c("ZIPCODE","State")])

test_result=test_result%>%left_join(zipcode_state_table)
States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
         "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")
t=test_result%>%filter(Basement==1,State%in%States,N>9)%>%
    #group_by(State)%>%
    summarise(cor=corr(d=cbind.data.frame(local_pred,Mean_Conc),w=N),s=length(N),
              me=mean(local_pred-Mean_Conc),
              mae=mean(abs(local_pred-Mean_Conc)),
              mre=mean(abs(local_pred-Mean_Conc)/Mean_Conc))

month_trend=test_result%>%filter(State%in%States,N>9)%>%group_by(Month,Basement)%>%summarise(c=corr(cbind.data.frame(local_pred,Mean_Conc),N))
annual_trend=test_result%>%filter(State%in%States,N>9)%>%group_by(Year,Basement)%>%summarise(c=corr(cbind.data.frame(local_pred,Mean_Conc),N))

library(ggplot2)
library(sf)
library(scales)
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "

load(here::here("Data","GeoData","Boundaries.RData"))
bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs=prjstring)
ggplot(data=test_result)+
  geom_sf(data=bound_sf,fill="white")+
  stat_summary_hex(aes(x=X,y=Y,z=(local_pred-Mean_Conc)),
                   color="gray",size=0.15,binwidth = 35000,
                   fun = ~weighted.mean(.x,.w=N)*(ifelse(length(.x)>5,1,NA)))+
  scale_fill_stepsn(expression('Mean Residual (pCi/L)'),
                    breaks = seq(-2,2,0.4),
                    values = seq(0,1,0.1),
                    limits=c(-2,2),
                    colors = rev(RColorBrewer::brewer.pal(11,"RdBu")),
                    na.value = "red",
                    guide = guide_colorsteps(direction = "horizontal",
                                             title.position = "top",
                                             label.position = "bottom",
                                             barwidth = unit(4, "inch"),
                                             barheight=unit(0.1, "inch")))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title = element_blank())

ggplot(data=test_result)+
  geom_sf(data=bound_sf,fill="white")+
  stat_summary_hex(aes(x=X,y=Y,z=abs(local_pred-Mean_Conc)),
                   color="gray",size=0.25,binwidth = 50000,
                   fun = ~weighted.mean(.x,.w=N)*(ifelse(length(.x)>2,1,NA)))+
  scale_fill_stepsn(expression('Mean Absolute Error (pCi/L)'),
                    breaks = seq(0,4,0.4),
                    values = seq(0,1,0.1),
                    limits=c(0,4),
                    colors = (RColorBrewer::brewer.pal(11,"Reds")),
                    na.value = "red",
                    guide = guide_colorsteps(direction = "horizontal",
                                             title.position = "top",
                                             label.position = "bottom",
                                             barwidth = unit(4, "inch"),
                                             barheight=unit(0.1, "inch")))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank())

ggplot(data=test_result)+
  geom_sf(data=bound_sf,fill="white")+
  stat_summary_hex(aes(x=X,y=Y,z=abs(local_pred-Mean_Conc)/Mean_Conc),
                   color="gray",size=0.25,binwidth = 50000,
                   fun = ~exp(weighted.mean(log(.x),.w=N))*(ifelse(length(.x)>4,1,NA)))+
  scale_fill_stepsn(expression('Relative Error'),
                    breaks = seq(0,1,0.1),
                    values = seq(0,1,0.1),
                    limits=c(0,1),
                    colors = (RColorBrewer::brewer.pal(11,"Reds")),
                    na.value = "red",
                    guide = guide_colorsteps(direction = "horizontal",
                                             title.position = "top",
                                             label.position = "bottom",
                                             barwidth = unit(4, "inch"),
                                             barheight=unit(0.1, "inch")))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank())

ggplot(data=test_result)+
  geom_sf(data=bound_sf,fill="white")+
  stat_summary_hex(aes(x=X,y=Y,z=R2),
                   color="gray",size=0.25,binwidth = 50000,
                   fun = ~exp(weighted.mean(log(.x),.w=N))*(ifelse(length(.x)>4,1,NA)))+
  scale_fill_stepsn(expression('Local Fitting (R2)'),
                    breaks = seq(0,1,0.1),
                    values = seq(0,1,0.1),
                    limits=c(0,1),
                    colors = (RColorBrewer::brewer.pal(11,"PuBuGn")),
                    na.value = "red",
                    guide = guide_colorsteps(direction = "horizontal",
                                             title.position = "top",
                                             label.position = "bottom",
                                             barwidth = unit(4, "inch"),
                                             barheight=unit(0.1, "inch")))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank())

pre_int="RI"
temp_predictors=c("uwnd","vwnd","temp","albedo","hpbl","rhum","snowc",                     
                  "soilm","pcp","soilt","soilw","pres","mean_beta","Month","Year")
geological_predictors=c("Sediment_Type","AVG_AWC","AVG_OM","AVG_KSAT","AVG_KV",                    
                        "AVG_BD","AVG_FC","AVG_NO4","AVG_NO10","AVG_NO200","AVG_POR",                   
                        "AVG_KFACT","AVG_THK","Uranium","Elevation","Slope","TRI",
                        "TPI","Roughness","dist2fault","RI","X","Y")
detector_predictors=c("Basement","Per_AC","Perc_LS","Per_AirChek")
building_predictors=c("Housing_Units","Single_Family","Two_Units_Housing",         
                      "Three_Four_Units_Housing","Five_Nine_Units_Housing","Ten_Nineteen_Units_Housing",
                      "Over_Twenty_Units_Housing","Over_Fifty_Units_Housing","Units_After_2014",          
                      "Units_2010_2013","Units_2000_2009","Units_1990_1999","Units_1980_1989",
                      "Units_1970_1979","Units_1960_1969","Units_1950_1959","Units_1940_1949","Units_Before_1939",         
                      "One_Room_Unit","Two_Room_Unit","Three_Room_Unit","Four_Room_Unit","Five_Room_Unit","Six_Room_Unit",             
                      "Seven_Room_Unit","Eight_Room_Unit","Over_Nine_Room_Unit",
                      "No_Bedroom_Unit","One_Bedroom_Unit","Two_Bedroom_Unit",          
                      "Three_Bedroom_Unit","Four_Bedroom_Unit","Over_Five_Bedroom_Unit",    
                      "Gas_Fuel","Electricity_Fuel","Oil_Fuel",                  
                      "Coal_Fuel","Solar_Fuel","No_Fuel")

cate=building_predictors

ggplot(data=test_result)+
  geom_sf(data=bound_sf,fill="white")+
  stat_summary_hex(aes(x=X,y=Y,
                       z=((Pred_1%in%geological_predictors)+(Pred_2%in%geological_predictors)+
                            (Pred_3%in%geological_predictors)+ (Pred_4%in%geological_predictors)+
                            (Pred_5%in%geological_predictors))),
                   color="gray",size=0.1,binwidth = 25000,
                   fun = ~weighted.mean(.x,.w=N))+
  scale_fill_stepsn(expression('Local Fitting (R2)'),
                    breaks = seq(0,1,0.1),
                    values = seq(0,1,0.1),
                    limits=c(0,1),
                    colors = (RColorBrewer::brewer.pal(11,"PuBuGn")),
                    na.value = "red",
                    guide = guide_colorsteps(direction = "horizontal",
                                             title.position = "top",
                                             label.position = "bottom",
                                             barwidth = unit(4, "inch"),
                                             barheight=unit(0.1, "inch")))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank())
