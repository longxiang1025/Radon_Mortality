library(ggplot2)
library(dplyr)
library(cowplot)

make_plot=function(zips_vis,title="Basement",include_legend=T){
  p=ggplot(data=zips_vis)+
    geom_sf(data=bound_sf%>%filter(STUSPS%in%States),fill="white")+
    geom_sf(aes(fill=mean_rn),color="black",size=0.05)+
    scale_fill_stepsn(expression('Monthly Average (pCi/L)'),
                      breaks = seq(0,10,1),
                      values = seq(0,1,0.1),
                      limits=c(0,10),
                      colors = (RColorBrewer::brewer.pal(11,"YlOrRd")),
                      na.value = "gray",
                      guide = guide_colorsteps(direction = "horizontal",
                                               title.position = "top",
                                               label.position = "bottom",
                                               barwidth = unit(4, "inch"),
                                               barheight=unit(0.1, "inch")))+
    theme_bw()+
    ggtitle(title)+
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "aliceblue",color="aliceblue"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_rect(fill="white",color="black",size=0.25),
          legend.title = element_text(size=14),
          legend.text = element_text(size=12),
          legend.margin = margin(0.02,0.02,0.02,0.02,"in"),
          plot.title=element_text(hjust=0.5, vjust=0.5,margin=margin(t=40,b=-30)))
  if(include_legend==F){
    p=p+theme(legend.position = "none")
  }
  return(p)
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
        plot.margin = margin(2, 2, 2, 10,unit = "pt")
      ) 
  }else{
    pure_title <- ggdraw() + 
      draw_label(
        text,
        x = 0,
        hjust = 0
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(2, 2, 2, 10,unit = "pt")
      ) 
  }
  return(pure_title)
}

States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
         "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")
folder="/n/koutrakis_lab/lab/Radon_Mortality/Data/State_Year/"
for(Year in 2001:2020){
  files=list.files(path=folder,pattern =as.character(Year))
  state_list=list()
  l=1
  for(f in files){
    load(paste0(folder,f))
    state_list[[l]]=pred_list
    l=l+1
  }
  state_pred=bind_rows(state_list)
  
  library(sf)
  library(scales)
  prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "
  
  load(here::here("Data","GeoData","Boundaries.RData"))
  load(here::here("Data","GeoData","2015_Shapes.RData"))
  zips=st_as_sf(zips)
  zips=zips%>%filter(STATE%in%States)
  zips=st_transform(zips,crs=prjstring)
  bound_sf<-st_as_sf(bound)
  bound_sf=st_transform(bound_sf,crs=prjstring)
  for(m in 1:12){
    title=create_title(paste0(month.abb[m]," ",Year),"horizontal")
    
    month_basement=state_pred%>%
      filter(Month==m)%>%
      group_by(ZIPCode)%>%summarise(mean_rn=mean(exp(Pred_Log_Mean_Basement)))
    zips_basement=zips%>%left_join(month_basement,by=c("ZIP"="ZIPCode"))
    p1=make_plot(zips_vis = zips_basement,title="Basement",include_legend = F)
    
    month_aboveground=state_pred%>%
      filter(Month==m)%>%
      group_by(ZIPCode)%>%summarise(mean_rn=mean(exp(Pred_Log_Mean_Aboveground)))
    zips_aboveground=zips%>%left_join(month_aboveground,by=c("ZIP"="ZIPCode"))
    p2=make_plot(zips_vis = zips_aboveground,title="Aboveground",include_legend = F)
    
    bar=make_plot(zips_vis = zips_aboveground,title="Aboveground",include_legend = T)
    bar=get_legend(bar)
    
    top_row=title
    mid_row=cowplot::plot_grid(p1,p2,nrow = 1)
    bot_row=plot_grid(NULL,bar,nrow = 1,rel_widths = c(2,80))
    
    fig=cowplot::plot_grid(top_row,mid_row,bot_row,nrow = 3,rel_heights = c(1,20,5))
    cowplot::save_plot(fig,base_height = 6,base_width = 12,
                       filename = paste0("/n/koutrakis_lab/lab/Radon_Mortality/Figures/Paper_Regional_Radon_Model/Month_Maps/Figure_",Year,"_",m,".jpeg"))
    
  }
  
}

#Show the seasonal variation in radon concentrations-------------------
library(sf)
States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
         "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")
prjstring<-"+proj=aea +lat_1=20 +lat_2=60 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs "

load(here::here("Data","GeoData","Boundaries.RData"))
load(here::here("Data","GeoData","2015_Shapes.RData"))
zips=st_as_sf(zips)
zips=zips%>%filter(STATE%in%States)
zips=st_transform(zips,crs=prjstring)
bound_sf<-st_as_sf(bound)
bound_sf=st_transform(bound_sf,crs=prjstring)

folder="/n/koutrakis_lab/lab/Radon_Mortality/Data/State_Year/"
files=list.files(path=folder)
all_data=list()
l=1
for( f in files){
  load(paste0(folder,f))
  all_data[[l]]=pred_list
  l=l+1
  if(l%%50==0){
    print(Sys.time())
  }
}
all_data=bind_rows(all_data)

#Show the spatial variation in concentration and sd---------------------------

create_fig6=function(months, years, title=NULL, select_basement=T,plot_sd=F,add_legend=T){
  if(select_basement){
    p_data=all_data%>%dplyr::filter(Month%in%months,Year%in%years)%>%
      dplyr::group_by(ZIPCode)%>%dplyr::summarise(mean_Rn=mean(exp(Pred_Log_Mean_Basement)))
    sd_data=all_data%>%dplyr::filter(Month%in%months,Year%in%years)%>%
      dplyr::group_by(ZIPCode)%>%dplyr::summarise(mean_Sd=mean((exp(Boot_sd_Basement)-1)))
  }else{
    p_data=all_data%>%dplyr::filter(Month%in%months,Year%in%years)%>%
      dplyr::group_by(ZIPCode)%>%dplyr::summarise(mean_Rn=mean(exp(Pred_Log_Mean_Aboveground)))
    sd_data=all_data%>%dplyr::filter(Month%in%months,Year%in%years)%>%
      dplyr::group_by(ZIPCode)%>%dplyr::summarise(mean_Sd=mean((exp(Boot_sd_Aboveground)-1)))
  }
  zips_vis=zips%>%left_join(p_data,by=c("ZIP"="ZIPCode"))%>%
    left_join(sd_data,by=c("ZIP"="ZIPCode"))
  
  if(is.null(title)){
    if(select_basement){
      title=paste0(month.abb[months],"-",years," [Basement]")
    }else{
      title=paste0(month.abb[months],"-",years," [Aboveground]")
    }
  }
  if(!plot_sd){
    p=ggplot(data=zips_vis)+
    geom_sf(aes(fill=mean_Rn),color=NA,lwd = 0)+
    geom_sf(data=bound_sf%>%filter(STUSPS%in%States),fill=NA,size=0.25)+
    scale_fill_stepsn(expression('Monthly Average Radon (Bq/m'^3*')'),
                      breaks = seq(0,10,1),
                      values = seq(0,1,0.1),
                      limits=c(-0.0001,10.0001),
                      colors = (RColorBrewer::brewer.pal(11,"YlOrRd")),
                      labels=as.character(37*seq(0,10,1)),
                      na.value = "gray",
                      guide = guide_colorsteps(direction = "horizontal",
                                               title.position = "top",
                                               label.position = "bottom",
                                               barwidth = unit(4, "inch"),
                                               barheight=unit(0.1, "inch")))+
    theme_bw()+
    ggtitle(title)+
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "white",color="white"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_rect(fill="white",color="black",size=0.25),
          legend.title = element_text(size=12),
          legend.text = element_text(size=10),
          legend.margin = margin(0.02,0.02,0.02,0.02,"in"),
          plot.title=element_text(size=12,hjust=0.5, vjust=0.5,margin=margin(t=40,b=-15)))
  }
  else{
    p=ggplot(data=zips_vis)+
      geom_sf(aes(fill=mean_Sd),color=NA,lwd = 0)+
      geom_sf(data=bound_sf%>%filter(STUSPS%in%States),fill=NA,size=0.25)+
      scale_fill_stepsn(expression('Relative Standard Deviation of the Prediction'),
                        breaks = c(0,0.05,0.1,0.15,0.2,0.3,0.5,1,1.5),
                        #values = seq(from=0,to=1,length.out=11),
                        values = scales::rescale(c(0,0.05,0.1,0.15,0.2,0.3,0.5,1,1.5)),
                        limits=c(-0.0001,2.0001),
                        colors = (RColorBrewer::brewer.pal(11,"PuBu")),
                        labels=paste0(100*c(0,0.05,0.1,0.15,0.2,0.3,0.5,1,1.5),"%"),
                        na.value = "gray",
                        guide = guide_colorsteps(direction = "horizontal",
                                                 title.position = "top",
                                                 label.position = "bottom",
                                                 barwidth = unit(4, "inch"),
                                                 barheight=unit(0.1, "inch")))+
      theme_bw()+
      ggtitle(title)+
      theme(legend.position = "bottom",
            panel.background = element_rect(fill = "white",color="white"),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.background = element_rect(fill="white",color="black",size=0.25),
            legend.title = element_text(size=12),
            legend.text = element_text(size=10),
            legend.text.align = 0.5,
            legend.margin = margin(0.02,0.02,0.02,0.02,"in"),
            plot.title=element_text(size=12,hjust=0.5, vjust=0.5,margin=margin(t=40,b=-15)))
  }
  if(add_legend==F){
    p=p+theme(legend.position = "none")
  }
  return(p)
}

f6pa=create_fig6(months = seq(1,12),
                 years = 2000:2020,select_basement = T,title = " ",
                 add_legend = F,plot_sd = F )
f6pb=create_fig6(months = seq(1,12),
                 years = 2000:2020,select_basement = T,title = " ",
                 add_legend = F,plot_sd = T )
f6pc=create_fig6(months = seq(1,12),
                 years = 2000:2020,select_basement = F,title = " ",
                 add_legend = F,plot_sd=F )
f6pd=create_fig6(months = seq(1,12),
                 years = 2000:2020,select_basement = F,title = " ",
                 add_legend = F,plot_sd=T )

basement_title=create_title("Basement")
aboveground_title=create_title("Aboveground")

conc_legend=create_fig6(months = c(12,1,2),years = 2000:2020,select_basement = F,title = "Winter & Aboveground",add_legend = T )
conc_legend=cowplot::get_legend(conc_legend)

sd_legend=create_fig6(months = c(12,1,2),years = 2000:2020,select_basement = F,title = "Winter & Aboveground",add_legend = T ,plot_sd = T)
sd_legend=cowplot::get_legend(sd_legend)

f6_up_left=cowplot::plot_grid(basement_title,aboveground_title,nrow=2)
f6_up_right=cowplot::plot_grid(f6pa,f6pb,f6pc,f6pd,labels = c("A","B","C","D"),label_x = 0.035,label_y = 0.825)

f6_bottom=cowplot::plot_grid(conc_legend,sd_legend,nrow=1)
f6_bottom=cowplot::plot_grid(NULL,f6_bottom,rel_widths = c(1,15))

f6_up=cowplot::plot_grid(f6_up_left,f6_up_right,nrow=1,rel_widths = c(1,15))

f6=plot_grid(f6_up,f6_bottom,nrow = 2,rel_heights = c(7,1))
cowplot::save_plot(filename = "Figure6.pdf",base_height = 6.5,base_width = 9,plot=f6)

#Show the seasonal variation in floor-dependent concentrations----------------------------------------
f7pa=create_fig6(months = c(12,1,2),
                 years = 2000:2020,select_basement = T,title = "Winter & Basement",
                 add_legend = F,plot_sd = F )
f7pb=create_fig6(months = c(6,7,8),
                 years = 2000:2020,select_basement = T,title = "Summer & Basement",
                 add_legend = F,plot_sd = F )
f7pc=create_fig6(months = c(12,1,2),
                 years = 2000:2020,select_basement = F,title = "Winter & Aboveground",
                 add_legend = F,plot_sd = F )
f7pd=create_fig6(months = c(6,7,8),
                 years = 2000:2020,select_basement = F,title = "Summer & Aboveground",
                 add_legend = F,plot_sd = F )
conc_legend=create_fig6(months = c(12,1,2),years = 2000:2020,select_basement = F,title = "Winter & Aboveground",add_legend = T )
conc_legend=cowplot::get_legend(conc_legend)
f7_up=cowplot::plot_grid(f7pa,f7pb,f7pc,f7pd,
                          labels = c("A","B","C","D"),nrow=2,
                          label_x = 0.065,label_y = 0.825)
f7=cowplot::plot_grid(f7_up,conc_legend,nrow = 2,rel_heights = c(7,1))
cowplot::save_plot(filename = "Figure7.pdf",base_height = 6.5,base_width = 9,plot=f7)
#Show the spatial variation in importance------------------------------------
create_fig8=function(title=NULL,var="RI",add_legend=T){
  p_data=all_data%>%dplyr::select(ZIPCode,paste0("Imp_",var))
  names(p_data)[2]="Importance"
  p_data=p_data%>%dplyr::group_by(ZIPCode)%>%dplyr::summarise(mean_Imp=mean(Importance))
  zips_vis=zips%>%left_join(p_data,by=c("ZIP"="ZIPCode"))
  marks= c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.5)
  p=ggplot(data=zips_vis)+
    geom_sf(aes(fill=mean_Imp),color=NA,lwd = 0)+
    geom_sf(data=bound_sf%>%filter(STUSPS%in%States),fill=NA,size=0.25)+
    scale_fill_stepsn(expression('Percent Increase in Mean Abosolute Error'),
                      breaks = marks,
                      values = scales::rescale(marks),
                      limits=c(-0.0001,0.501),
                      colors = (RColorBrewer::brewer.pal(8,"Greens")),
                      labels=paste0(100*marks,"%"),
                      na.value = "gray",
                      guide = guide_colorsteps(direction = "horizontal",
                                               title.position = "top",
                                               label.position = "bottom",
                                               barwidth = unit(4, "inch"),
                                               barheight=unit(0.1, "inch")))+
    theme_bw()+
    ggtitle(title)+
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "white",color="white"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_rect(fill="white",color="black",size=0.25),
          legend.title = element_text(size=12),
          legend.text = element_text(size=10),
          legend.text.align = 0.5,
          legend.margin = margin(0.02,0.02,0.02,0.02,"in"),
          plot.title=element_text(size=8,hjust=0.5, vjust=0.5,margin=margin(t=-15,b=-15)))
  if(add_legend==F){
    p=p+theme(legend.position = "none")
  }
  return(p)
}

f8pa=create_fig8(var="RI",title = "Importance of Radon Potential", add_legend = F)
f8pb=create_fig8(var="Year",title="Importance of Year",add_legend = F)
f8pc=create_fig8(var="temp",title="Importance of Temperature",add_legend = F)
f8pd=create_fig8(var="soilt",title = "Importance of Soil Temperature",add_legend = F)
f8pe=create_fig8(var="Grav_ISO",title="Importance of Gravity Anomaly",add_legend = F)
f8pf=create_fig8(var="Per_Basement",title = "Importance of Basement Percent",add_legend = F)
f8pg=create_fig8(var="dist2fault",title="Importance of Active Fault",add_legend = F)
f8ph=create_fig8(var="Elevation",title="Importance of Elevation",add_legend = F)
f8pi=create_fig8(var="pres",title="Importance of Barometric Pressure",add_legend = F)
f8pj=create_fig8(var="Gas_Fuel",title="Importance of Natural Gas",add_legend = F)
f8_up= cowplot::plot_grid(f8pa,f8pb,f8pc,f8pd,f8pe,f8pg,f8ph,f8pi,f8pj,
                   nrow=3,ncol = 3,
                   label_x = 0.85,label_y = 0.3,
                   labels = c("A","B","C","D","E","F","G","H","I"))
f8_legend=create_fig8(var="Month",title="Importance of Month",add_legend =T)
f8_legend=cowplot::get_legend(f8_legend)

f8=cowplot::plot_grid(f8_up,f8_legend,nrow = 2,rel_heights = c(9,1))
cowplot::save_plot(filename = "Figure8.pdf",base_height = 7,base_width = 9,plot=f8)



#Show the spatial variation in local fitting---------------
spatial_r2=all_data%>%group_by(ZIPCode)%>%summarise(r2=mean(R2))
data_vis=zips%>%left_join(spatial_r2,by=c("ZIP"="ZIPCode"))
f9 = ggplot(data = data_vis) +
  geom_sf(aes(fill = r2), color = NA, lwd = 0) +
  geom_sf(
    data = bound_sf %>% filter(STUSPS %in% States),
    fill = NA,
    size = 0.25
  ) +
  scale_fill_stepsn(
    expression('Local fitting (R' ^ 2 * ')'),
    breaks = seq(0, 0.6, 0.1),
    limits = c(-0.0001, 0.6),
    colors = (RColorBrewer::brewer.pal(6, "Purples")),
    #labels=as.character(37*seq(0,10,1)),
    na.value = "gray",
    guide = guide_colorsteps(
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      barwidth = unit(4, "inch"),
      barheight = unit(0.1, "inch")
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "white"),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill = "white", color = "black", size =
                                       0.25),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.margin = margin(0.02, 0.02, 0.02, 0.02, "in"),
    plot.title = element_text(
      size = 12,
      hjust = 0.5,
      vjust = 0.5,
      margin = margin(t = 40, b = -15)
    )
  )
ggsave(filename = "Figure9.pdf",width = 6,height = 4.5,plot=f9)