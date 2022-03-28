#########################################
#The objective of this script is to aggregate the performance of each location-based random forest
#model. Based on the aggregated results, two multi-panel figures (Figure 3 and 4) are made. Figure 3
# shows the correlation (scatter plot). Figure 4(s) shows the spatial distribution of residuals, MAE and 
#importance of each category of predictors. Also, state-, month-, and year-related summary of results
# need to created as Tables.
##########################################
library(dplyr)
library(boot)
library(MASS)
library(ggplot2)

r2ww <- function(x){
  #The function to calculate weigthted R2 for rlm
   SSe <- sum((x$w*x$resid)^2); #the residual sum of squares is weighted
   observed <- x$resid+x$fitted;
   SSt <- sum((x$w*(observed-mean(observed)))^2); #the total sum of squares is weighted      
   value <- 1-SSe/SSt;
   return(value);
  }

lm_eqn <- function(df){
  m <- rlm(local_pred~Mean_Conc,weights =N, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(37*coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(r2ww(m)^2, digits = 2)))
  as.character(as.expression(eq));
}
pattern <- function(x, size, pattern) {
  ex = list(
    horizontal = c(1, 2),
    vertical = c(1, 4),
    left2right = c(2, 4),
    right2left = c(1, 3)
  )
  fillgrid = st_make_grid(x, cellsize = size)
  endsf = lapply(1:length(fillgrid), function(j)
    sf::st_linestring(sf::st_coordinates(fillgrid[j])[ex[[pattern]], 1:2]))
  endsf = sf::st_sfc(endsf, crs = sf::st_crs(x))
  endsf = sf::st_intersection(endsf, x)
  endsf = endsf[sf::st_geometry_type(endsf)
                %in% c("LINESTRING", "MULTILINESTRING")]
  endsf = sf::st_line_merge(sf::st_union(endsf))
  return(endsf)
}


#Load the validation files [No need to run every time]------------------------
#files=list.files("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF_10000_2_5000/",recursive = T)
files=list.files("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",recursive = T)

test_result=list()
l=1
for( f in files){
  #load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF_10000_2_5000/",f))
  size=file.size(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",f))
  if(size>0){
    load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/ST_RF/",f))
    test_result[[l]]=test
    l=l+1
    if(l%%1000==0){
      print(l)
    } 
  }
}
test_result=bind_rows(test_result)
corr(test_result[test_result$Basement==1,c("local_pred","Mean_Conc")],test_result[test_result$Basement==1,"N"])
corr(test_result[test_result$Basement==0,c("local_pred","Mean_Conc")],test_result[test_result$Basement==0,"N"])
save(file = here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ST_RF_Performance_5000_2_5000.RData"),test_result)

#Figure 3 the correlation between observed and predicted concentrations----------------
load(file = here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ST_RF_Performance_5000_2_5000.RData"))
load(paste0("/n/koutrakis_lab/lab/Radon_Mortality/Data/Medium Data/NE_MW_Regional_Model_Data/Scratch_Copies/Regional_Training_",random_num=sample(1:10,1),".RData"))
zipcode_state_table=unique(training_data[,c("ZIPCODE","State")])

test_result=test_result%>%left_join(zipcode_state_table)
States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
         "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")

test_result=test_result%>%arrange(N)
m_base=rlm(local_pred~Mean_Conc,weights=N,data=test_result%>%filter(N>9,Basement==1))
m_above=rlm(local_pred~Mean_Conc,weights=N,data=test_result%>%filter(N>9,Basement==0))

m_base_large=rlm(local_pred~Mean_Conc,weights=N,data=test_result%>%filter(N>19,Basement==1))
m_above_large=rlm(local_pred~Mean_Conc,weights=N,data=test_result%>%filter(N>19,Basement==0))

create_scatter=function(df,Basement=T,add_legend=T,
                        xlab_str=expression('Observed ZCTA-level Radon (Bq/m'^3*')'),
                        ylab_str=expression('Predicted ZCTA-level Radon (Bq/m'^3*')')){
  m=rlm(local_pred~Mean_Conc,weights=N,data=df)
  if(Basement){
    p_fill="#B22234"
    legend_title="Number of \nBasement\nMeasurements"
  }else{
    p_fill="#3C3B6E"
    legend_title="Number of \nAboveground\nMeasurements"
  }
  p=ggplot(data=df)+
    geom_point(aes(x=37*Mean_Conc,y=37*local_pred,size=N),fill=p_fill,shape=21,color="gray75")+
    geom_abline(intercept = 37*m$coefficients[1],
                slope = m$coefficients[2],linetype="solid",color="darkgreen",size=1)+
    geom_abline(intercept = 0,slope = 1,linetype="dashed",size=0.75)+
    xlab(xlab_str)+
    ylab(ylab_str)+
    geom_text(x = 350, y = 25, 
              label = lm_eqn(df=df), parse = TRUE)+
    scale_radius(name=legend_title,
                 breaks = c(10,50,100,250,500),
                 labels = c(10,50,100,250,500),
                 limits = c(10,500),
                 range = c(0.15,5),
                 trans = "log")+
    coord_fixed(ratio = 1,xlim=c(0,500),ylim=c(0,500))+
    theme_bw()+
    theme(legend.position = c(0.185,0.725),
          legend.title = element_text(size=11),
          legend.text = element_text(size=10),
          legend.box.background = element_rect(fill="white",size=1),
          axis.text = element_text(size=10),
          axis.title = element_text(size=11))
  if(add_legend==F){
    p=p+theme(legend.position = "none")
  }
  return(p)
}

p_base=create_scatter(df=test_result%>%filter(N>9,Basement==1),Basement = T,add_legend = F,xlab_str = " ",ylab_str = " ")
p_base_large=create_scatter(df=test_result%>%filter(N>29,Basement==1),Basement = T,add_legend = T)

p_above=create_scatter(df=test_result%>%filter(N>9,Basement==0),Basement = F,add_legend = F,xlab_str = " ",ylab_str = " ")
p_above_large=create_scatter(df=test_result%>%filter(N>19,Basement==0),Basement = F,add_legend = T,xlab_str = " ",ylab_str = " ")

fig3=cowplot::plot_grid(p_base,p_above,p_base_large,p_above_large,
                   nrow=2,labels = c("A","B","C","D"))
cowplot::save_plot("Fig3.pdf",base_height = 9,base_width = 9,plot = fig3)

t=test_result%>%filter(State%in%States,N>9)%>%
  #group_by(State)%>%
  group_by(Basement)%>%
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
us_bound=st_union(bound_sf)

extent_bound=bound_sf%>%filter(STUSPS%in%States)
extent_bound=st_union(extent_bound)
exclude_region=bound_sf%>%filter(!STUSPS%in%States)
exclude_region=st_union(exclude_region)
exclude_pattern = 
  pattern(exclude_region, 100000, "left2right")

common_theme=theme(panel.background = element_rect(fill = "white",color="white"),
                   axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   legend.background = element_rect(fill="gray94",color="black",size=0.25),
                   legend.title = element_text(size=12,angle =-90),
                   legend.text = element_text(size=10),
                   legend.text.align = 0,
                   legend.margin = margin(0.06,0.1,0.06,0.1,"in"),
                   legend.position = "right")
##Panel A of Figure 4 (Spatial Distribution of Mean Residual)---------
f4pa=ggplot(data=test_result)+
  geom_sf(data=bound_sf,fill="white")+
  stat_summary_hex(aes(x=X,y=Y,z=37*(local_pred-Mean_Conc)),
                   color="gray",size=0.15,binwidth = 50000,
                   fun = ~weighted.mean(.x,.w=N)*(ifelse(length(.x)>3,1,NA)))+
  geom_sf(data=bound_sf,fill=NA,color="black",size=0.25,alpha=0.5,show.legend = F)+
  geom_sf(data=us_bound,fill=NA,size=0.85)+
  geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
  geom_sf(data=exclude_pattern,size=0.35,color="black")+
  geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
  geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
  scale_fill_stepsn(expression('Mean Residual (Bq/m'^3*')'),
                    breaks = seq(-100,100,20),
                    values = seq(0,1,0.1),
                    labels=formatC(seq(-100,100,20),width = 4,flag=" "),
                    limits=c(-100,100),
                    colors = rev(RColorBrewer::brewer.pal(11,"RdBu")),
                    na.value = "red",
                    guide = guide_colorsteps(direction = "vertical",
                                             title.position = "right",
                                             label.position = "right",
                                             barwidth = unit(0.15, "inch"),
                                             barheight=unit(2.1, "inch")))+
  theme_bw()+
  common_theme+
  coord_sf(crs =prjstring,expand = F,clip = "on",
           xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
           ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))

f4pb=ggplot(data=test_result)+
  geom_sf(data=bound_sf,fill="white")+
  stat_summary_hex(aes(x=X,y=Y,z=37*abs(local_pred-Mean_Conc)),
                   color="gray",size=0.15,binwidth = 50000,
                   fun = ~weighted.mean(.x,.w=N)*(ifelse(length(.x)>3,1,NA)))+
  geom_sf(data=bound_sf,fill=NA,color="black",size=0.25,alpha=0.5,show.legend = F)+
  geom_sf(data=us_bound,fill=NA,size=0.85)+
  geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
  geom_sf(data=exclude_pattern,size=0.35,color="black")+
  geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
  geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
  scale_fill_stepsn(expression('Mean Absolute Residual (Bq/m'^3*')'),
                    breaks = seq(0,200,20),
                    values = seq(0,1,0.1),
                    labels=formatC(seq(0,200,20),width = 5,flag=" "),
                    limits=c(0,200),
                    colors = (RColorBrewer::brewer.pal(11,"Greens")),
                    na.value = "red",
                    guide = guide_colorsteps(direction = "vertical",
                                             title.position = "right",
                                             label.position = "right",
                                             barwidth = unit(0.15, "inch"),
                                             barheight=unit(2.1, "inch")))+
  theme_bw()+
  common_theme+
  coord_sf(crs =prjstring,expand = F,clip = "on",
           xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
           ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))

f4pc=ggplot(data=test_result)+
  geom_sf(data=bound_sf,fill="white")+
  stat_summary_hex(aes(x=X,y=Y,z=abs(local_pred-Mean_Conc)/Mean_Conc),
                   color="gray",size=0.25,binwidth = 50000,
                   fun = ~exp(weighted.mean(log(.x),.w=N))*(ifelse(length(.x)>3,1,NA)))+
  geom_sf(data=bound_sf,fill=NA,color="black",size=0.25,alpha=0.5,show.legend = F)+
  geom_sf(data=us_bound,fill=NA,size=0.85)+
  geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
  geom_sf(data=exclude_pattern,size=0.35,color="black")+
  geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
  geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
  scale_fill_stepsn(expression('Relative Residual'),
                    breaks = seq(0,1,0.1),
                    values = seq(0,1,0.1),
                    labels=formatC(seq(0,1,0.1),width = 8,flag=" "),
                    limits=c(0,1),
                    colors = (RColorBrewer::brewer.pal(11,"BuPu")),
                    na.value = "red",
                    guide = guide_colorsteps(direction = "vertical",
                                             title.position = "right",
                                             label.position = "right",
                                             barwidth = unit(0.15, "inch"),
                                             barheight=unit(2.1, "inch")))+
  theme_bw()+
  common_theme+
  coord_sf(crs =prjstring,expand = F,clip = "on",
           xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
           ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))

fig4=cowplot::plot_grid(f4pa,f4pb,f4pc,nrow = 3,labels = c("A","B","C"))
cowplot::save_plot(file="Figure4.pdf",base_height = 9,base_width = 6,plot = fig4)

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

create_panel_of_fig5=function(categories=building_predictors,add_legend=F){
  p=ggplot(data=test_result)+
    geom_sf(data=bound_sf,fill="white")+
    stat_summary_hex(aes(x=X,y=Y,
                         z=((Pred_1%in%categories)+(Pred_2%in%categories)+
                              (Pred_3%in%categories)+ (Pred_4%in%categories)+
                              (Pred_5%in%categories))),
                     color="gray",size=0.25,binwidth = 50000,
                     fun = ~weighted.mean(.x,.w=N))+
    geom_sf(data=bound_sf,fill=NA,color="black",size=0.25,alpha=0.5,show.legend = F)+
    geom_sf(data=us_bound,fill=NA,size=0.85)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    geom_sf(data=exclude_pattern,size=0.35,color="black")+
    geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    scale_fill_stepsn(expression('Likelihood of Being Key Predictor(s)'),
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
    common_theme+
    theme(legend.title = element_text(angle = 0),
          legend.box.margin = margin(0.1,0.1,0.1,0.1,"in"))+
    coord_sf(crs =prjstring,expand = F,clip = "on",
             xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
             ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))
  if(add_legend==F){
    p=p+theme(legend.position = "none")
  }
  return(p)
}
f5pa=create_panel_of_fig5(categories = geological_predictors,add_legend = F)
f5pb=create_panel_of_fig5(categories = detector_predictors,add_legend = F)
f5pc=create_panel_of_fig5(categories = temp_predictors,add=F)
f5pd=create_panel_of_fig5(categories = building_predictors,add=F)
legend=create_panel_of_fig5(categories = detector_predictors,add_legend = T)
legend=cowplot::get_legend(legend)

top=cowplot::plot_grid(f5pa,f5pb,f5pc,f5pd,nrow = 2,labels = c("A","B","C","D"),label_x = 0.9,label_y = 0.23)
fig5=cowplot::plot_grid(top,legend,nrow=2,rel_heights = c(8,1))
cowplot::save_plot(file="Figure5.pdf",plot=fig5,base_height = 6,base_width = 9)
