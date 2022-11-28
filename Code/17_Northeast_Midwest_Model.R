######################################################################3
#The objective of this script is to generate the training dataset for the random forest model
#Meanwhile, two figures (Fig 1 and 2) are made here to summarize the data.
#######################################################################
library(dplyr)
library(lubridate)
library(EnvStats)
library(sf)
library(ggplot2)
library(ggsn)
library(cowplot)
library(xlsx)

#Load constant parameters-------------------------------------------
States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
         "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")

Neighbour_States=c("WV","KY","VA","CO","WV","MT","OK","AR","TN","NC")

load(file="Merged_Measurements_220610.RData")

load(here::here("Data","GeoData","2015_Shapes.RData"))
sf::sf_use_s2(FALSE)
zips_sf=st_as_sf(zips)
zip_centroid_longlat=st_centroid(zips_sf)
zips_sf=st_transform(zips_sf,crs="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45")
zip_centroid=st_centroid(zips_sf)
zip_centroid=cbind.data.frame(zip_centroid$ZIP,st_coordinates(zip_centroid),st_coordinates(zip_centroid_longlat))
names(zip_centroid)=c("ZIPCODE","x","y","Longitude","Latitude")

load(here::here("Data","GeoData","Boundaries.RData"))
load("/n/koutrakis_lab/lab/Group_Data/Basic_Geodata/Canada_boundaries.RData")
bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45")
us_bound=st_union(bound_sf)

extent_bound=bound_sf%>%filter(STUSPS%in%States)
extent_bound=st_union(extent_bound)
exclude_region=bound_sf%>%filter(!STUSPS%in%States)
exclude_region=st_union(exclude_region)

common_theme=theme(panel.background = element_rect(fill = "aliceblue",color="aliceblue"),
                   axis.title = element_blank(),
                   axis.text = element_text(size=11),
                   #axis.ticks = element_blank(),
                   legend.background = element_rect(fill="white",color="black",size=0.25),
                   legend.title = element_text(size=14,angle = -90),
                   legend.text = element_text(size=12),
                   legend.margin = margin(0.06,0.1,0.06,0.1,"in"))

#Load defined functions--------------------------------------------
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

create_title=function(text,orientation="vertical"){
  if(orientation=="vertical"){
    pure_title <- ggdraw() + 
      draw_label(
        text,
        x = 0,
        hjust = 0.5,
        angle=90
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(2, 2, 2, 2,unit = "pt")
      ) 
  }else{
    pure_title <- ggdraw() + 
      draw_label(
        text,
        x = 0.5
        ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(2, 2, 2, 2,unit = "pt")
      ) 
  }
  return(pure_title)
}

#Create Training Dataset--------------------------------------------
lab_data[lab_data$Floor=="basement","Floor"]="Basement"
lab_data[lab_data$Floor=="first","Floor"]="First"
lab_data[lab_data$Floor=="second","Floor"]="Second"
lab_data[lab_data$Floor=="unknown","Floor"]="Unknown"

lab_data=lab_data%>%filter(Method!="AT",
                           TestState%in%c(States,Neighbour_States),
                           Floor%in%c("Basement","First","Second"))
##Remove the measurements, if duration of AirChek is over 7 days and durations of LS/AC is over 4 days
lab_data$duration=lab_data$EndDate-lab_data$StartDate
lab_data=lab_data%>%filter(!is.na(duration))
lab_data=lab_data%>%filter(duration>0,duration<8)
lab_data=lab_data%>%filter((Method=="AC"&duration<5)||
                           (Method=="LS"&duration<5)||
                           (Method=="LS"&duration<8))
##Remove the measurements, if there're too many measurements from the same building (commercial)--------------------
multiple_ids=lab_data%>%group_by(ID)%>%summarise(n=length(ID))
multiple_ids=multiple_ids%>%filter(n>5)
lab_data=lab_data%>%filter(!ID%in%multiple_ids$ID)
##Remove the measurements, if they use the Device with continuous KitNumber in the same ZIPCode and month of year----
lab_data$Month=month(lab_data$StartDate)
lab_data$Year=year(lab_data$StartDate)
lab_data=lab_data%>%arrange(TestPostalCode,Floor,DeviceNumber,StartDate)
lab_data$continuous_kitnumber=c(1000,diff(lab_data$DeviceNumber))
lab_data$continuous_kitnumber=abs(lab_data$continuous_kitnumber)<3
lab_data$overlap_day=c(100,diff(lab_data$StartDate))
lab_data$overlap_day=abs(lab_data$overlap_day)<4
lab_data$bulkorder=lab_data$continuous_kitnumber&lab_data$overlap_day
lab_data=lab_data%>%filter(!bulkorder)
#Take the average of collocated measurements
coloc_data=lab_data%>%group_by(ID,StartDate,EndDate,TestState,TestPostalCode,Floor,Method)%>%summarise(n=length(ID),Conc=mean(PCI.L))
##Only select the first measurement of the ID--------------------------------
coloc_data=coloc_data%>%filter(year(StartDate)<2021,
                               year(StartDate)>2000)

multiple_ids_date=coloc_data%>%group_by(ID)%>%summarise(First_Start_Date=min(StartDate),
                                                        n_Measure=length(unique(StartDate)))
coloc_data=coloc_data%>%left_join(multiple_ids_date)
coloc_data=coloc_data%>%filter(StartDate==First_Start_Date)
coloc_data=coloc_data%>%filter(!is.na(Conc))
#About 99.9 percentile. So we didn't delete lots of measurements
coloc_data=coloc_data%>%filter(Conc<100)

#Replace zero concentration with random number
set.seed(12345)
coloc_data[coloc_data$Conc==0&coloc_data$Method!="AirChek","Conc"]=
  runif(nrow(coloc_data[coloc_data$Conc==0&coloc_data$Method!="AirChek",]),min=0.01,max=0.4)
coloc_data[coloc_data$Conc==0&coloc_data$Method=="AirChek","Conc"]=
  runif(nrow(coloc_data[coloc_data$Conc==0&coloc_data$Method=="AirChek",]),min=0.01,max=0.1)
save(coloc_data,file="Cleaned_Raw_Data_0610.RData")

## [Table 1]Summarize the data in different stratifications----------------
write_t1_row=function(data=coloc_data){
  n=nrow(data)
  temp_t=data%>%group_by(Floor=="Basement")%>%summarise(n=length(Floor),
                                                        median=median(37*Conc),
                                                        q1=quantile(37*Conc,0.25),
                                                        q3=quantile(37*Conc,0.75))
  temp_t$text=paste0(formatC(temp_t$median, digits = 1,format = "f")," (",
                     formatC(temp_t$q1, digits = 1,format = "f"),", ",
                     formatC(temp_t$q3, digits = 1,format = "f"),")")
  temp_t$nice_num=formatC(temp_t$n, format="d", big.mark=",")
  
  row=t(c(temp_t[2,"n"],temp_t[2,"text"],temp_t[1,"n"],temp_t[1,"text"]))
}

r1=write_t1_row(data=coloc_data)
r2=write_t1_row(data=coloc_data%>%filter(TestState%in%c("MA","NH","VT","ME","CT","RI")))
r3=write_t1_row(data=coloc_data%>%filter(TestState%in%c("NY","PA","MD","NJ","DE")))
r4=write_t1_row(data=coloc_data%>%filter(TestState%in%c("IL","IN","MI","OH","WI")))
r5=write_t1_row(data=coloc_data%>%filter(TestState%in%c("IA","MN","MO","KS","NE","SD","ND")))
r6=write_t1_row(data=coloc_data%>%filter(TestState%in%Neighbour_States))

r7=write_t1_row(data=coloc_data%>%filter(lubridate::month(StartDate)%in%c(12,1,2)))
r8=write_t1_row(data=coloc_data%>%filter(lubridate::month(StartDate)%in%c(3,4,5)))
r9=write_t1_row(data=coloc_data%>%filter(lubridate::month(StartDate)%in%c(6,7,8)))
r10=write_t1_row(data=coloc_data%>%filter(lubridate::month(StartDate)%in%c(9,10,11)))

r11=write_t1_row(data=coloc_data%>%filter(lubridate::year(StartDate)%in%c(2001,2002,2003,2004,2005)))
r12=write_t1_row(data=coloc_data%>%filter(lubridate::year(StartDate)%in%c(2006,2007,2008,2009,2010)))
r13=write_t1_row(data=coloc_data%>%filter(lubridate::year(StartDate)%in%c(2011,2012,2013,2014,2015)))
r14=write_t1_row(data=coloc_data%>%filter(lubridate::year(StartDate)%in%c(2016,2017,2018,2019,2020)))

r15=write_t1_row(data=coloc_data%>%filter(Method=="LS"))
r16=write_t1_row(data=coloc_data%>%filter(Method=="AC"))
r17=write_t1_row(data=coloc_data%>%filter(Method=="AirChek"))

table1=rbind(r1,r2,r3,r4,r5,r6,
             r7,r8,r9,r10,r11,
             r12,r13,r14,r15,r16,r17)
write.xlsx(x=table1,file="Table1.xlsx")

## [Figure 1]Create a plot for the point-base data-------------
load(file="Cleaned_Raw_Data_0610.RData")
coloc_data_vis=coloc_data%>%left_join(zip_centroid,by=c("TestPostalCode"="ZIPCODE"))
coloc_data_vis=coloc_data_vis%>%filter(!is.na(x))
exclude_pattern = 
  pattern(exclude_region, 100000, "left2right")

creat_fig1=function(data=coloc_data_vis%>%filter(Floor=="Basement")){
  re_plot=ggplot(data=data)+
    geom_sf(data=ca_shp,fill="gray95",size=0.75,alpha=0.75)+
    geom_sf(data=bound_sf,fill="white")+
    stat_summary_hex(aes(x=x,y=y,z=Conc),color="gray",size=0.25,binwidth = 50000,fun = ~geoMean(.x)*(ifelse(length(.x)>5,1,NA)))+
    geom_sf(data=bound_sf,fill=NA,color="black",size=0.25,alpha=0.5,show.legend = F)+
    geom_sf(data=us_bound,fill=NA,size=0.85)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    geom_sf(data=exclude_pattern,size=0.35,color="black")+
    geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    scale_fill_stepsn(expression('Radon Concentraion (Bq/m'^3*')'),
                      breaks = seq(0,10,1),
                      values = seq(0,1,0.1),
                      limits=c(0,10),
                      labels=37*seq(0,10,1),
                      oob = scales::squish,
                      colors = (RColorBrewer::brewer.pal(11,"YlOrRd")),
                      guide = guide_colorsteps(direction = "vertical",
                                               title.position = "right",
                                               label.position = "right",
                                               barwidth = unit(0.1, "inch"),
                                               barheight=unit(4, "inch")))+
    coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
             xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
             ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))+
    theme_bw()+
    common_theme
  return(re_plot)
}

panel_a=creat_fig1(data=coloc_data_vis%>%filter(Floor=="Basement"))
panel_b=creat_fig1(data=coloc_data_vis%>%filter(Floor!="Basement"))
panel_a=panel_a+
  north(x.min = st_bbox(bound_sf%>%filter(STUSPS%in%States))[1],
        x.max = st_bbox(bound_sf%>%filter(STUSPS%in%States))[3],
        y.min = st_bbox(bound_sf%>%filter(STUSPS%in%States))[2],
        y.max = st_bbox(bound_sf%>%filter(STUSPS%in%States))[4],
        location="topleft")
panel_b=panel_b+
  ggsn::scalebar(data=bound_sf%>%filter(STUSPS%in%States),
                 transform = F,
                 dist = 250,
                 st.dist = 0.04,
                 st.size = 3,
                 dist_unit = "km",
                 st.bottom = F,
                 border.size = 0.5,
                 box.fill = c("black","white"),
                 location="bottomright")
fig1=cowplot::plot_grid(panel_a,panel_b,nrow = 2,labels = c("A","B"))
cowplot::save_plot("Fig1.pdf",base_height = 9,base_width = 9,plot=fig1)
## [Figure Supplement 1]Plot the percent of three types of detectors----------
load(file = "Cleaned_Raw_Data_0610.RData")
creat_fig_supp_1=function(data=coloc_data_vis,Type="AirChek"){
  data$vis_column=as.numeric(data$Method==Type)
  re_plot=ggplot(data=data)+
    geom_sf(data=ca_shp,fill="gray95",size=0.75,alpha=0.75)+
    geom_sf(data=bound_sf,fill="white")+
    stat_summary_hex(aes(x=x,y=y,z=vis_column),color="gray",size=0.25,binwidth = 50000,fun = ~mean(.x)*(ifelse(length(.x)>5,1,NA)))+
    geom_sf(data=bound_sf,fill=NA,color="black",size=0.25,alpha=0.5,show.legend = F)+
    geom_sf(data=us_bound,fill=NA,size=0.85)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    geom_sf(data=exclude_pattern,size=0.35,color="black")+
    geom_sf(data=exclude_region,size=0.65,fill="white",alpha=0.75)+
    geom_sf(data=extent_bound,fill=NA,color="Black",size=0.65)+
    scale_fill_stepsn(paste0("Percnet of ",Type),
                      breaks = seq(0,1,0.1),
                      values = seq(0,1,0.1),
                      limits=c(0,1),
                      oob = scales::squish,
                      colors = (RColorBrewer::brewer.pal(11,"PuBuGn")),
                      guide = guide_colorsteps(direction = "vertical",
                                               title.position = "right",
                                               label.position = "right",
                                               barwidth = unit(0.1, "inch"),
                                               barheight=unit(4, "inch")))+
    coord_sf(crs ="+proj=lcc +lon_0=-100 +lat_1=33 +lat_2=45",expand = F,clip = "on",
             xlim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[1]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[3]+80000),
             ylim = c(st_bbox(bound_sf%>%filter(STUSPS%in%States))[2]-25000,st_bbox(bound_sf%>%filter(STUSPS%in%States))[4]+25000))+
    theme_bw()+
    common_theme
  return(re_plot)
}
panel_s1_a=creat_fig_supp_1(data=coloc_data_vis,Type = "AirChek")
panel_s1_b=creat_fig_supp_1(data=coloc_data_vis,Type = "AC")
panel_s1_c=creat_fig_supp_1(data=coloc_data_vis,Type = "LS")
fig_s1=cowplot::plot_grid(panel_s1_a,panel_s1_b,panel_s1_c,nrow=3)
## [Figure 2]Plot the seasonal trend in radon concentrations----------------
load(file="Cleaned_Raw_Data_0610.RData")
monthly_mean=coloc_data%>%group_by(Floor=="Basement",month(StartDate),year(StartDate))%>%
  summarise(m=geoMean(Conc),lb=quantile(Conc,0.25),ub=quantile(Conc,0.75))
month_name_table=cbind.data.frame(1:12,month.abb)
names(month_name_table)=c("Month","month.abb")
names(monthly_mean)=c("Floor","Month","Year","Gm","Lb","Ub")  
monthly_mean=monthly_mean%>%left_join(month_name_table)
monthly_mean=monthly_mean%>%arrange(Year,Month)

season_rects=data.frame(xstart = c(1,seq(3,241,3)), xend = c(seq(3,241,3),241), 
                    col = c("Winter",rep(c("Spring","Summer","Autumn","Winter"),20)))
year_rects=data.frame(xstart = seq(1,241,12), 
                      xend = c(seq(13,241,12),241))
year_annotate=data.frame(x=seq(6.5,240,12),
                         y=4.75,
                         label=2001:2020)

fig2=ggplot(data=monthly_mean)+
        geom_rect(data=season_rects, aes(ymin=0, ymax=5, xmin=xstart,
                                  xmax=xend, fill=col), alpha =0.5)+
        geom_rect(data=year_rects, aes(ymin=0, ymax=5, xmin=xstart,
                                  xmax=xend),color="gray50",size=0.15,alpha=0)+
        geom_line(aes(x=12*(Year-2001)+Month,y=Gm,color=Floor),size=0.25)+
        geom_smooth(aes(x=12*(Year-2001)+Month,y=Gm,color=Floor),
                    method = "gam",formula = y~s(x,k=100),size=0.5)+
        geom_point(aes(x=12*(Year-2001)+Month,y=Gm,color=Floor),size=1.5,shape=21)+
        scale_x_continuous(breaks = year_annotate$x,
                           labels = year_annotate$label,
                           minor_breaks = NULL)+
        scale_y_continuous(breaks = 0:4,
                           labels = 37*(0:4))+
        scale_color_manual("Building Floor",
                           breaks = c(TRUE,FALSE),
                           values = c("#BF0A30","#002868"),
                           labels=c("Basement","Aboveground"))+
        scale_fill_manual("Season",
                          breaks = c("Spring","Summer","Autumn","Winter"),
                          values = c("#FEF8FA","#D6F1C6","#F9CC87","#C9F1FD"))+
        coord_cartesian(xlim = c(1,241),ylim = c(0,5),expand = F,clip = "on")+
        ylab(expression(Geometric~Mean~of~Radon~Concentration~(Bq/m^3)))+
        theme_bw()+
        theme(axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size=8),
              axis.ticks.length.x = unit(0,"in"),
              axis.title.x = element_blank(),
              panel.grid = element_blank(),
              legend.background = element_rect(color="gray75"),
              legend.title = element_text(size = 9),
              legend.text = element_text(size=8),
              legend.box.margin = margin(1,1,1,1,unit = "pt"))
ggsave("Fig2.pdf",plot = fig2,width = 9,height = 5,units = "in",device = cairo_pdf)
##Calculate the monthly geoMean of ZIP Codes----------------------
load(file="Cleaned_Raw_Data_0610.RData")
monthly_summ=coloc_data%>%group_by(TestPostalCode,TestState,
                                   month(StartDate),year(StartDate))%>%
                          summarise(n=length(ID),
                                    m_Conc=geoMean(Conc),
                                    sd_Conc=geoSD(Conc),
                                    m_log=mean(log(Conc)),
                                    var_log=sd(log(Conc)),
                                    over_2=mean(Conc>=2),
                                    over_4=mean(Conc>=4),
                                    perc_Imp=mean(((Conc<0.1)&(Method=="AirChek"))||((Conc<0.4)&(Method!="AirChek"))),
                                    perc_Basment=mean(Floor=="Basement"),
                                    perc_AC=mean(Method=="AC"),
                                    perc_LS=mean(Method=="LS"),
                                    per_Chek=mean(Method=="AirChek"))
names(monthly_summ)=c("ZIPCODE","State","Month","Year","N","Mean_Conc","SD_Conc","Mean_Log","SD_Log",
                      "Over_2","Over_4","Per_LDL","Per_Basement","Perc_AC","Perc_LS","Perc_AirChek")

##Select monthly/ZIP Code with over 5 measurements
#training_summ=monthly_summ%>%filter(N>4)
training_summ=monthly_summ
save(training_summ,file = "Regional_Training_Without_Predictors_220621.RData")

#Connecting Training Dataset with Predictors------------
load(file="Regional_Training.RData")
#The actual working process should be binding predictors to all months & zipcodes
#Then attach some of them to the training dataset, the rest of them can be used to 
#make the final prediction. So, script named 18(for spatial only) and 19 (time and space)
#will be created.