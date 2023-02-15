##########################################################################
#The objective of this script is to address the comments of EST
# 1. Create histograms of observed and predicted radon concentrations
# 2. Calculate the correlation bettwen observed and predicted concentration
#    on a log scale
##########################################################################
library(dplyr)
library(ggplot2)
library(cowplot)
library(MASS)
library(boot)

#Load all predicted concentrations 
all_files=list.files("/n/koutrakis_lab/lab/Radon_Mortality/Data/State_Year/",full.names = T)
all_predictions=list()
l=1
for( f in all_files){
  load(f)
  all_predictions[[l]]=pred_list
  l=l+1
}
all_predictions=bind_rows(all_predictions)
#Load all observed concentrations
load(file = here::here("Data","Medium Data","NE_MW_Regional_Model_Data","ST_RF_Performance_150000_0_75000.RData"))
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

test_result=test_result%>%arrange(N)
# Create three histograms, one for all observations (N>4), one for basement predictions, one for aboveground predictions
pa=ggplot2::ggplot(data=test_result%>%filter(N>4))+
  ggplot2::geom_histogram(aes(x=37*exp(Obs_Mean_Log),y=..density..),binwidth = 30,color="black",fill="white")+
  ggplot2::geom_density(aes(x=37*exp(Obs_Mean_Log),color="All Observed"),size=1.25)+
  theme_bw()+
  scale_color_manual(NULL,
                    breaks = c("All Observed"),
                    values = c("#3C3B6E"))+
  coord_cartesian(xlim = c(0,500),y=c(0,0.0175))+
  xlab("Observed Radon Concentrations (Bq/m3)")+
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank())
pa

pb=ggplot2::ggplot(data=all_predictions)+
  ggplot2::geom_histogram(aes(x=37*Conc_Basement,y=..density..),binwidth = 30,color="black",fill="white")+
  ggplot2::geom_density(aes(x=37*Conc_Basement,color="All Basement Prediction"),size=1.25)+
  theme_bw()+
  scale_color_manual(NULL,
                     breaks = c("All Basement Prediction"),
                     values = c("#B22234"))+
  coord_cartesian(xlim = c(0,500),y=c(0,0.0175))+
  xlab("Predicted Radon Concentrations in Basement (Bq/m3)")+
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank())
pb

pc=ggplot2::ggplot(data=all_predictions)+
  ggplot2::geom_histogram(aes(x=37*Conc_Above,y=..density..),binwidth = 30,color="black",fill="white")+
  ggplot2::geom_density(aes(x=37*Conc_Above,color="All Aboveground Prediction"),size=1.25)+
  theme_bw()+
  scale_color_manual(NULL,
                     breaks = c("All Aboveground Prediction"),
                     values = c("#486856"))+
  coord_cartesian(xlim = c(0,500),y=c(0,0.0175))+
  xlab("Predicted Radon Concentrations in Basement (Bq/m3)")+
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank())
pc
p=cowplot::plot_grid(pa,pb,pc,nrow=1,labels = c("A","B","C"))

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
  df$y=df$Obs_Mean_Log
  df$x=df$Pred_Mean_Log
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

create_f3_scatter_3=function(df,add_legend=T,Basement=T,
                             xlab_str=expression('Log Observed ZCTA-level Radon (Bq/m'^3*')'),
                             ylab_str=expression('Log Predicted ZCTA-level Radon (Bq/m'^3*')')){
  #To create a scatter plot in Figure 4 with a style similar with Figure 3
  df$y=df$Obs_Mean_Log
  df$x=df$Pred_Mean_Log
  m <- rlm(y~x,weights =N, df)
  if(Basement){
    p_fill="#B22234"
    legend_title="Number of\nMeasurements"
  }else{
    p_fill="#3C3B6E"
    legend_title="Number of \nMeasurements"
  }
  
  p=ggplot(data=df)+
    geom_point(aes(x=Obs_Mean_Log,y=Pred_Mean_Log,size=N),fill=p_fill,shape=21,color="gray75",stroke=0.01)+
    geom_abline(intercept = m$coefficients[1],
                slope = m$coefficients[2],linetype="solid",color="darkgreen",size=1)+
    geom_abline(intercept = 0,slope = 1,linetype="dashed",size=0.75)+
    xlab(xlab_str)+
    ylab(ylab_str)+
    geom_text(x = 1.5, y = -3, 
              label = lm_eqn(df=df), parse = TRUE)+
    scale_radius(name=legend_title,
                 breaks = c(5,10,20,50,100),
                 labels = c(5,10,20,50,100),
                 limits = c(5,100),
                 range = c(0.05,5))+
    coord_fixed(ratio = 1,xlim=c(-4,4),ylim=c(-4,4))+
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

f4sa=create_f3_scatter_3(df=test_result%>%filter(N<10),add_legend = T)
f4sb=create_f3_scatter_3(df=test_result%>%filter(N>9,N<15),add_legend = T)
f4sc=create_f3_scatter_3(df=test_result%>%filter(N>14),add_legend = T)
f4s=cowplot::plot_grid(f4sa,f4sb,f4sc,nrow=3,labels = c("A","B","C"))
ggsave("Fig4S_Alternative.pdf",plot=f4s,
       width=6,height = 12,units = "in")

#The obtecive of the following section is to visualize the trends in rural vs urban areas
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
  t=bind_rows(t_list)
  #t=cbind(t[2,2:6],t[1,2:6])
  t=cbind(t,cat)
  return(t)
}

test_result_pop=test_result%>%left_join(training_data%>%dplyr::select(ZIPCODE,Month,Year,popdensity))
medium_samples=test_result_pop%>%filter(N<21)

urban_cv=calculate_cv(data=test_result_pop%>%filter(State%in%States,popdensity>=2500),
                      cat = "Urban",
                      cutoff = 5:20)
urban_cv$N=5:20

rural_cv=calculate_cv(data=test_result_pop%>%filter(State%in%States,popdensity<2500),
                      cat = "Rural",
                      cutoff = 5:20)
rural_cv$N=5:20
cv_diff=bind_rows(urban_cv,rural_cv)

urban_row=calculate_cv(data=test_result_pop%>%filter(State%in%States,popdensity>=2500),
                      cat = "Urban",
                      cutoff = c(4,14))
rural_row=calculate_cv(data=test_result_pop%>%filter(State%in%States,popdensity<2500),
                       cat = "Rural",
                       cutoff = c(4,14))

pa=ggplot(data=cv_diff)+
  geom_line(aes(x=N,y=mae,color=cat),size=2)+
  geom_point(aes(x=N,y=mae,color=cat),size=4)+
  scale_color_manual("",
                     breaks = c("Rural","Urban"),
                     values = c("#B22234","#3C3B6E"),
                     labels=c("Non-urban Area (Density <2,500 people/sqmi)",
                              "Urban Ara (Density >2,500 people/sqmi)"))+
  theme_bw()+
  theme(legend.position = c(0.6,0.8),
        axis.title = element_text(size=13),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))+
  xlab("Sample Size")+
  ylab(expression('Mean Absolute Error (Bq/m'^3*')'))

pb=ggplot(data=cv_diff)+
  geom_line(aes(x=N,y=cor,color=cat),size=2)+
  geom_point(aes(x=N,y=cor,color=cat),size=4)+
  scale_color_manual("",
                     breaks = c("Rural","Urban"),
                     values = c("#B22234","#3C3B6E"),
                     labels=c("Non-urban Area (Density <2,500 people/sqmi)",
                              "Urban Ara (Density >2,500 people/sqmi)"))+
  theme_bw()+
  theme(legend.position = c(0.6,0.2),
        axis.title = element_text(size=13),
        axis.text = element_text(size = 12),
        legend.text = element_text(size=12))+
  xlab("Sample Size")+
  ylab(expression('Correlation'))
g=cowplot::plot_grid(pa,pb,nrow = 1,labels = c("A","B"))
g
