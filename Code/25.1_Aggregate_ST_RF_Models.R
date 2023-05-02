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
library(cowplot)
library(egg)

r2ww <- function(x){
  #The function to calculate weigthted R2 for rlm
   #SSe <- sum((x$w*x$resid)^2); #the residual sum of squares is weighted
   #observed <- x$resid+x$fitted;
   #SSt <- sum((x$w*(observed-mean(observed)))^2); #the total sum of squares is weighted      
   #value <- 1-SSe/SSt;
  value=corr(cbind(x$fitted.values,x$fitted.values+x$residuals),w=x$weights) 
  return(value);
  }

lm_eqn <- function(df){
  df$y=37*exp(df$Obs_Mean_Log)
  df$x=37*exp(df$Pred_Mean_Log)
  m <- rlm(y~x,weights =N, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2,nsmall=1),
                        b = format(unname(coef(m)[2]), digits = 2,nsmall=1),
                        r2 = format(r2ww(m)^2, digits = 2,nsmall=2)))
  as.character(as.expression(eq));
}

lm_eqn2<-function(df){
  df$y=df$Obs_Over_4
  df$x=df$Pred_Over_4
  m <- rlm(y~x,weights =N, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2,nsmall=1),
                        b = format(unname(coef(m)[2]), digits = 2,nsmall=1),
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

mae<-function(x,y,w){
  return(weighted.mean(abs(x-y),w))
}

#Load the validation files [No need to run every time]------------------------
r=150000
d=75000
k=0
folder=paste0("ST_RF_",r,"_",k,"_",d)

files=list.files(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder),recursive = T)
print(paste(Sys.time(),"A total of ",length(files),"Files"))
test_result=list()
l=1
for( f in files){
  size=file.size(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder,"/",f))
  if(size>0){
    load(paste0("/n/holyscratch01/koutrakis_lab/Users/loli/",folder,"/",f))
    test_result[[l]]=test
    l=l+1
    #if(l%%1000==0){
      print(paste(l,Sys.time()))
    #} 
  }
}
test_result=bind_rows(test_result)
test_result=test_result%>%filter(N>4)
test_result%>%group_by(N)%>%summarise(m=mae(exp(Obs_Mean_Log),exp(Pred_Mean_Log),N),
                                      n=length(Obs_Mean_Log))%>%ggplot()+geom_point(aes(x=N,y=m))+xlim(c(0,30))
test_result%>%group_by(as.integer(N/5))%>%summarise(m=mae(exp(Obs_Mean_Log),exp(Pred_Mean_Log),N),
                                                     n=length(Obs_Mean_Log))
test_result%>%group_by(N>9)%>%summarise(c=mltools::rmse(exp(Obs_Mean_Log),exp(Pred_Mean_Log),N))
test_result%>%group_by(N>19)%>%summarise(c=mltools::rmse(exp(Obs_Mean_Log),exp(Pred_Mean_Log),N))
test_result%>%group_by(N>29)%>%summarise(c=mltools::rmse(exp(Obs_Mean_Log),exp(Pred_Mean_Log),N))

corr(exp(test_result[,c("Obs_Mean_Log","Pred_Mean_Log")]),w=test_result$N)
save(file = here::here("Data","Medium Data","NE_MW_Regional_Model_Data",
                       paste0("ST_RF_Performance_",r,"_",k,"_",d,"_V6.RData")),test_result)

#Figure 3 the correlation between observed and predicted concentrations----------------
load(file = here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ST_RF_Performance_150000_0_75000_V6.RData"))
load(file=here::here("Data","Medium Data","NE_MW_Regional_Model_Data","Regional_Training_220621.RData"))
training_data$geometry=NULL
training_data=training_data[!is.na(training_data$Electricity_Fuel),]
#Here we only use the ZCTA-level mean with N>4 for evaluation purpose (CV)
#But we use all ZCTA-level obs as training dataset
evaluate_data=training_data%>%filter(N>4)
evaluate_data=evaluate_data%>%left_join(test_result,by=c("ZIPCODE","Month","Year","X","Y","N"))
zipcode_state_table=unique(training_data[,c("ZIPCODE","State")])

test_result=test_result%>%left_join(zipcode_state_table)
States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
         "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")
test_result=test_result%>%filter(State%in%States)
test_result=test_result%>%arrange(N)
#Figure 3 consists of two columns. Left column (Panel A) is the declining trend of MAE
#Right column (Panel B-D) consists of three scatter plots respectively for (N<10, N<20 and N>20)

#Create left column
trend_vis=test_result%>%filter(N>4)%>%group_by(N)%>%
  summarise(mae=mean(37*abs(exp(Pred_Mean_Log)-exp(Obs_Mean_Log))),
            sample_size=length(N))
## Bootstrap is needed to estimate the variance in mae with each category of sample size
boot_mae_Conc=function(x,sim_time=50){
  boot_tank=list()
  for(i in 1:sim_time){
    set.seed(12345+i)
    run=sample(1:nrow(x),2*nrow(x)/3)
    run=x[run,] 
    boot_tank[[i]]=mean(abs(37*exp(run$Obs_Mean_Log)-37*exp(run$Pred_Mean_Log)))
  }
  boot_tank=unlist(boot_tank)
  return(as.numeric(summary(boot_tank)))
}
sim_boot_conc=list()
for(n in 1:30){
  slice=test_result%>%filter(N==n)
  sim_boot_conc[[n]]=boot_mae_Conc(slice)
}
sim_boot_conc=sim_boot_conc[5:30]
sim_boot_conc=do.call("rbind",sim_boot_conc)
sim_boot_conc=as.data.frame(sim_boot_conc)
names(sim_boot_conc)=c("Min","Q1","Meidan","Mean","Q3","Max")
sim_boot_conc$N=5:30
sim_boot_conc$sample_size=unlist(trend_vis[1:26,"sample_size"])
## Create the figure in the left column
fig_3_a=ggplot(data=sim_boot_conc)+
  geom_errorbar(aes(x=N,ymin=Q1,ymax=Q3),width=1,color="#B22234")+
  geom_path(aes(x=N,y=Mean),linetype="dashed",color="#B22234")+
  geom_point(aes(x=N,y=Mean),size=0.85)+
  xlab("Sample Size")+
  ylab(expression('Mean Absolute Error (Bq/m'^3*')'))+
  scale_x_continuous(breaks = c(5,10,15,20,25,30))+
  coord_cartesian(xlim = c(5,30),ylim=c(20,45))+
  theme_bw()+
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size=10),
        panel.grid.minor = element_blank())
fig_3_left_c=cowplot::plot_grid(fig_3_a,labels = "A",label_x = 0.85,label_y = 0.1)
#Create the right column
create_fig3_scatter=function(df,add_legend=T,Basement=T,s_width=0.1,
                        xlab_str=expression('Observed ZCTA-level Radon (Bq/m'^3*')'),
                        ylab_str=expression('Predicted ZCTA-level Radon (Bq/m'^3*')')){
  df$y=37*exp(df$Obs_Mean_Log)
  df$x=37*exp(df$Pred_Mean_Log)
  m <- rlm(y~x,weights =N, df)
  if(Basement){
    p_fill="#B22234"
    legend_title="Sample Size"
  }else{
    p_fill="#3C3B6E"
    legend_title="Sample Size"
  }

  p=ggplot(data=df)+
    geom_point(aes(x=37*exp(Obs_Mean_Log),y=37*exp(Pred_Mean_Log),size=N),fill=p_fill,shape=21,color="gray75",stroke=s_width)+
    geom_abline(intercept = m$coefficients[1],
                slope = m$coefficients[2],linetype="solid",color="darkgreen",size=1)+
    geom_abline(intercept = 0,slope = 1,linetype="dashed",size=0.75)+
    xlab(xlab_str)+
    ylab(ylab_str)+
    geom_text(x = 200, y = 450, 
              label = lm_eqn(df=df), parse = TRUE)+
    scale_radius(name=legend_title,
                 breaks = c(5,10,20,50,100),
                 labels = c(5,10,20,50,100),
                 limits = c(5,100),
                 range = c(0.05,5))+
    coord_fixed(ratio = 1,xlim=c(0,500),ylim=c(0,500))+
    theme_bw()+
    theme(legend.position = "right",
          legend.title = element_text(size=10),
          legend.text = element_text(size=9),
          legend.box.background = element_rect(fill="white",size=1),
          axis.text = element_text(size=10),
          axis.title = element_text(size=11),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  if(add_legend==F){
    p=p+theme(legend.position = "none")
  }
  return(p)
}

p_small=create_fig3_scatter(df=test_result%>%filter(N>4,N<10),Basement = T,add_legend = F,xlab_str = " ",ylab_str = expression('Predicted Rn level (Bq/m'^3*')'))
p_medium=create_fig3_scatter(df=test_result%>%filter(N>9,N<15),Basement = T,add_legend = F,xlab_str = " ",ylab_str = expression('Predicted Rn level (Bq/m'^3*')'),
                             s_width =0.15)
p_large=create_fig3_scatter(df=test_result%>%filter(N>14),Basement = T,add_legend = F,
                            xlab_str = expression('Observed Rn level (Bq/m'^3*')'),
                            ylab_str = expression('Predicted Rn level (Bq/m'^3*')'),s_width=0.5)
x_p=435
y_p=35

fig_3_center_c=ggarrange(tag_facet(p_small+
                                    theme(axis.ticks.x = element_blank(),
                                          axis.title.x = element_blank(),
                                          axis.text.x = element_blank(),
                                          plot.margin = margin(t=1,b = 1))+
                                          facet_wrap(~"small"),
                                          tag_pool = "B",
                                          open = " ",
                                          close= " ",
                                          x=x_p,
                                          y=y_p),
                         tag_facet(p_medium+
                                     theme(axis.ticks.x = element_blank(),
                                           axis.title.x = element_blank(),
                                           axis.text.x = element_blank(),
                                           plot.margin = margin(b = 1))+
                                     facet_wrap(~"medium"),
                                     tag_pool = "C",
                                     open = " ",
                                     close= " ",
                                     x=x_p,
                                     y=y_p), 
                         tag_facet(p_large+
                                     theme(plot.margin = margin(t = 1))+
                                     facet_wrap(~"large"),
                                     tag_pool = "D",
                                     open = " ",
                                     close= " ",
                                     x=x_p,
                                     y=y_p),
                         ncol = 1)

#fig_3_center_c=cowplot::plot_grid(p_small,p_medium,p_large,ncol = 1,
#                                  labels=c("B","C","D"),axis = "l",align = "h",
#                                  label_x = 0.75,label_y = 0.25)

p_legend=create_fig3_scatter(df=test_result%>%filter(N>19),Basement = T,add_legend = T,
                             xlab_str = expression('Observed Rn level (Bq/m'^3*')'),
                             ylab_str = expression('Predicted Rn level (Bq/m'^3*')'),s_width=0.25)
p_legend=cowplot::get_legend(p_legend)
fig_3_right_c=plot_grid(NULL,p_legend,NULL,align="b",nrow = 1,rel_widths = c(0.5,5,0.5))

fig_3=cowplot::plot_grid(plotlist = list(fig_3_left_c,fig_3_center_c,fig_3_right_c),
                         nrow=1,align = "b",rel_widths = c(0.85,1,0.35))

title <- ggdraw() + 
  draw_label(
    bquote(atop(bold("Figure 4.")~"The sample size-specific differences/correlations between",
    "the observed and predicted ZCTA-level monthly radon concentrations.")),
    size=12,
    hjust = 0.5
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

fig_3=plot_grid(
  fig_3,
  title,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(1, 0.075)
)

ggsave("Fig3_Alternative.pdf",plot=fig_3,
       width=7,height = 9,units = "in")


fig_s_2=cowplot::plot_grid(p_small,p_medium,p_large,p_huge,nrow = 2,labels = c("A","B","C","D"))
cowplot::save_plot("Fig_S2.pdf",plot=fig_s_2,base_width=9, base_height=9,device = cairo_pdf)

p_base=create_scatter(df=test_result%>%filter(N>19,Per_Basement<0.75),Basement = T,add_legend = F,xlab_str = " ",ylab_str = " ")
p_base_large=create_scatter(df=test_result%>%filter(N>19,Per_Basement>0.75),Basement=T,add_legend = T)

p_above=create_scatter(df=test_result%>%filter(N>9,Per_Basement<0.5),Basement = F,add_legend = F,xlab_str = " ",ylab_str = " ")
p_above_large=create_scatter(df=test_result%>%filter(N>29,Per_Basement<0.5),Basement = F,add_legend = T,xlab_str = " ",ylab_str = " ")

fig3=cowplot::plot_grid(p_base,p_above,p_base_large,p_above_large,
                   nrow=2,labels = c("A","B","C","D"))
cowplot::save_plot("Fig3.pdf",base_height = 9,base_width = 9,plot = fig3)


#Figure 3 (Alternative) the trend of MAE against sample size----------------
load(file = here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ST_RF_Performance_150000_0_75000.RData"))


boot_mae_Perc=function(x,sim_time=50){
  boot_tank=list()
  for(i in 1:sim_time){
    set.seed(12345+i)
    run=sample(1:nrow(x),2*nrow(x)/3)
    run=x[run,] 
    boot_tank[[i]]=mean(abs(run$Obs_Over_4-run$Pred_Over_4))
  }
  boot_tank=unlist(boot_tank)
  return(as.numeric(summary(boot_tank)))
}



fig3_alternative_b=ggplot(data=sim_boot_perc)+
  geom_errorbar(aes(x=N,ymin=Q1,ymax=Q3),width=0.35,color="#3C3B6E")+
  geom_path(aes(x=N,y=Mean),linetype="dashed",color="#3C3B6E")+
  geom_point(aes(x=N,y=Mean),size=0.85)+
  xlab("Measurement Count Per ZIP Code and Month")+
  ylab(expression('Mean Absolute Error (Percent)'))+
  scale_x_continuous(breaks = c(5,10,15,20,25,30))+
  coord_cartesian(xlim = c(5,30),ylim=c(0,0.25))+
  theme_bw()+
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size=10),
        panel.grid.minor = element_blank())
fig3_alternative=cowplot::plot_grid(fig3_alternative_a,fig3_alternative_b,
                                    labels = c("A","B"),label_x = 0.85,label_y = 0.9)

ggsave("Fig3_Alternative.pdf",plot=fig3_alternative,
       width=9,height = 6,units = "in")

#Figure 4 the correlation between observed and predicted proportion> 4 pCi/L----
load(file = here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ST_RF_Performance_150000_0_75000_V4.RData"))
load(file=paste0("/n/holyscratch01/koutrakis_lab/Users/loli/Medium_Data/Regional_Training/Regional_Training_",sample(1:50,1),".RData"))
training_data$geometry=NULL
training_data=training_data[!is.na(training_data$Electricity_Fuel),]
#Here we only use the ZCTA-level mean with N>0 for evaluation purpose (CV)
#But we use all ZCTA-level obs as training dataset
evaluate_data=training_data%>%filter(N>4)
evaluate_data=evaluate_data%>%left_join(test_result,by=c("ZIPCODE","Month","Year","X","Y","N"))
zipcode_state_table=unique(training_data[,c("ZIPCODE","State")])

test_result=test_result%>%left_join(zipcode_state_table)
States=c("MA","NH","ME","VT","CT","RI","NY","PA","MD","NJ","DE",
         "IL","OH","MI","WI","IN","IA","MN","MO","KS","NE","SD","ND")

test_result=test_result%>%arrange(N)

create_f3_scatter_2=function(df,add_legend=T,Basement=T,
                             xlab_str=expression('Observed ZCTA-level Radon (Bq/m'^3*')'),
                             ylab_str=expression('Predicted ZCTA-level Radon (Bq/m'^3*')')){
  #To create a scatter plot in Figure 4 with a style similar with Figure 3
  df$y=37*exp(df$Obs_Mean_Log)
  df$x=37*exp(df$Pred_Mean_Log)
  m <- rlm(y~x,weights =N, df)
  if(Basement){
    p_fill="#B22234"
    legend_title="Number of\nMeasurements"
  }else{
    p_fill="#3C3B6E"
    legend_title="Number of \nMeasurements"
  }
  
  p=ggplot(data=df)+
    geom_point(aes(x=37*exp(Obs_Mean_Log),y=37*exp(Pred_Mean_Log),size=N),fill=p_fill,shape=21,color="gray75",stroke=0.01)+
    geom_abline(intercept = m$coefficients[1],
                slope = m$coefficients[2],linetype="solid",color="darkgreen",size=1)+
    geom_abline(intercept = 0,slope = 1,linetype="dashed",size=0.75)+
    xlab(xlab_str)+
    ylab(ylab_str)+
    geom_text(x = 350, y = 25, 
              label = lm_eqn(df=df), parse = TRUE)+
    scale_radius(name=legend_title,
                 breaks = c(10,20,50,100,250,500),
                 labels = c(10,20,50,100,250,500),
                 limits = c(10,500),
                 range = c(0.1,3),
                 trans = "log10")+
    coord_fixed(ratio = 1,xlim=c(0,500),ylim=c(0,500))+
    theme_bw()+
    theme(legend.position = c(0.215,0.8),
          legend.title = element_text(size=10),
          legend.text = element_text(size=9),
          legend.box.background = element_rect(fill="white",size=1),
          legend.key.height= unit(0.125,"inch"),
          axis.text = element_text(size=10),
          axis.title = element_text(size=11))
  if(add_legend==F){
    p=p+theme(legend.position = "none")
  }
  return(p)
}

create_f4_scatter=function(df,add_legend=T,
                          xlab_str=expression('Observed % over 148 Bq/m'^3),
                          ylab_str=expression('Predicted % over 148 Radon Bq/m'^3)){
  p_fill="#3C3B6E"
  legend_title="Number of\nMeasurements"
  df$y=df$Obs_Over_4
  df$x=df$Pred_Over_4
  m <- rlm(y~x,weights =N, df)
  
  p=ggplot(data=df)+
    geom_point(aes(x=Obs_Over_4,y=Pred_Over_4,size=N),fill=p_fill,shape=21,color="gray75",stroke=0.01)+
    geom_abline(intercept = m$coefficients[1],
                slope = m$coefficients[2],linetype="solid",color="darkgreen",size=1)+
    geom_abline(intercept = 0,slope = 1,linetype="dashed",size=0.75)+
    xlab(xlab_str)+
    ylab(ylab_str)+
    geom_text(x = 0.35, y = 0.85, 
              label = lm_eqn2(df=df), parse = TRUE)+
    scale_radius(name=legend_title,
                 breaks = c(10,20,50,100,250,500),
                 labels = c(10,20,50,100,250,500),
                 limits = c(10,500),
                 range = c(0.1,3),
                 trans = "log10")+
    coord_fixed(ratio = 1,xlim=c(0,1),ylim=c(0,1))+
    theme_bw()+
    theme(legend.position = c(0.825,0.175),
          legend.title = element_text(size=10),
          legend.text = element_text(size=9),
          legend.box.background = element_rect(fill="white",size=1),
          legend.key.height= unit(0.125,"inch"),
          axis.text = element_text(size=10),
          axis.title = element_text(size=11))
  if(add_legend==F){
    p=p+theme(legend.position = "none")
  }
  return(p)
}

#f4pa=create_f4_scatter(df=test_result%>%filter(N<10),add_legend = F)
#f4pb=create_f4_scatter(df=test_result%>%filter(N>9,N<20),add_legend = F)
f4pa=create_f3_scatter_2(df=test_result%>%filter(N>9),add_legend = T)
f4pb=create_f4_scatter(df=test_result%>%filter(N>9),xlab_str = "Observed Percent Over Action Level",
                       ylab_str = "Predicted Percent Over Action Level",add_legend = T)
#f4pd=create_f4_scatter(df=test_result%>%filter(N>29),add_legend = T)

fig_4=cowplot::plot_grid(f4pa,f4pb,nrow = 1,labels = c("A","B"),label_x = 0.135,label_y = 0.85)
cowplot::save_plot("Fig4.pdf",plot=fig_4,base_width=9, base_height=6,device = cairo_pdf)
#[Table 2] Calculate the overall CV results--------------- 
calculate_cv=function(data,cat,cutoff=c(4,14)){
  t_list=list()
  l=1
  for(c in cutoff){
    t=data%>%filter(N>c)%>%
      summarise(cor=corr(d=cbind.data.frame(37*exp(Pred_Mean_Log),
                                            37*exp(Obs_Mean_Log)),w=N)^2,
                s=length(N),
                me=37*weighted.mean(exp(Pred_Mean_Log)-exp(Obs_Mean_Log),w=N),
                mae=37*weighted.mean(abs(exp(Pred_Mean_Log)-exp(Obs_Mean_Log)),w=N),
                mre=weighted.mean(abs(Pred_Mean_Log-Obs_Mean_Log),w=N)) 
    t_list[[l]]=t
    l=l+1
  }
  t=cbind.data.frame(t_list)
  #t=cbind(t[2,2:6],t[1,2:6])
  t=cbind(t,cat)
  return(t)
}

NE_States=c("VT","NH","ME","MA","CT","RI")
MA_States=c("NY","PA","MD","NJ","DE")
CNE_States=c("IL","OH","MI","WI","IN")
CNW_States=c("IA","MN","MO","KS","NE","SD","ND")

all_cv=calculate_cv(data=test_result%>%filter(State%in%States),cat = "All")
ne_cv=calculate_cv(data=test_result%>%filter(State%in%NE_States),cat = "New England")
ma_cv=calculate_cv(data=test_result%>%filter(State%in%MA_States),cat="Mid Atlantic")
cne_cv=calculate_cv(data=test_result%>%filter(State%in%CNE_States),cat="East North Central")
cnw_cv=calculate_cv(data=test_result%>%filter(State%in%CNW_States),cat=" West North Central")
out_cv=calculate_cv(data=test_result%>%filter(!State%in%States),cat=" Outside")

winter_cv=calculate_cv(data=test_result%>%filter(State%in%States,Month%in%c(12,1,2)), cat="Winter")
spring_cv=calculate_cv(data=test_result%>%filter(State%in%States,Month%in%c(3,4,5)), cat="Spring")
summer_cv=calculate_cv(data=test_result%>%filter(State%in%States,Month%in%c(6,7,8)), cat="Summer")
autumn_cv=calculate_cv(data=test_result%>%filter(State%in%States,Month%in%c(9,10,11)), cat="Autumn")

y1_cv=calculate_cv(data=test_result%>%filter(State%in%States,Year>=2001,Year<=2005), cat= "2001-2005")
y2_cv=calculate_cv(data=test_result%>%filter(State%in%States,Year>=2006,Year<=2010), cat = "2006-2010")
y3_cv=calculate_cv(data=test_result%>%filter(State%in%States,Year>=2011,Year<=2015), cat= "2011-2015")
y4_cv=calculate_cv(data=test_result%>%filter(State%in%States,Year>=2015,Year<=2020), cat= "2016-2020")

table_2=rbind(all_cv,ne_cv,ma_cv,cne_cv,cnw_cv,out_cv,winter_cv,spring_cv,summer_cv,autumn_cv,y1_cv,y2_cv,y3_cv,y4_cv)
write.csv(table_2,file="Table2.csv")

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
