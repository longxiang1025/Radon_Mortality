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
    
    month_basement=state_pred%>%filter(Basement==1,Month==m)%>%group_by(ZIPS)%>%summarise(mean_rn=mean(Local_Pred))
    zips_basement=zips%>%left_join(month_basement,by=c("ZIP"="ZIPS"))
    p1=make_plot(zips_vis = zips_basement,title="Basement",include_legend = F)
    
    month_aboveground=state_pred%>%filter(Basement==0,Month==m)%>%group_by(ZIPS)%>%summarise(mean_rn=mean(Local_Pred))
    zips_aboveground=zips%>%left_join(month_aboveground,by=c("ZIP"="ZIPS"))
    p2=make_plot(zips_vis = zips_aboveground,title="Aboveground",include_legend = F)
    
    bar=make_plot(zips_vis = zips_aboveground,title="Aboveground",include_legend = T)
    bar=get_legend(bar)
    
    top_row=title
    mid_row=cowplot::plot_grid(p1,p2,nrow = 1)
    bot_row=plot_grid(NULL,bar,nrow = 1,rel_widths = c(2,80))
    
    fig=cowplot::plot_grid(top_row,mid_row,bot_row,nrow = 3,rel_heights = c(1,20,5))
    cowplot::save_plot(fig,base_height = 10,base_width = 12,
                       filename = paste0("/n/koutrakis_lab/lab/Radon_Mortality/Figures/Paper_Regional_Radon_Model/Month_Maps/Figure_",Year,"_",m,".jpeg"))
    
  }
  
}

