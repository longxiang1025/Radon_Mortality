gtwr_s<-function(obs,
                 pred,
                 bases,
                 dis.matrix,
                 bw,
                 kernel="gaussain"){
  gtwr_pred=matrix(nrow=nrow(pred),ncol=3+length(bases))
  coefs=paste0("b",0:length(bases))
  initials=list()
  for(l in 1:length(coefs)){
    initials[[l]]=0
  }
  names(initials)=coefs
  form=paste0(paste0(coefs[2:(1+length(bases))],"*",bases[1:length(bases)]),collapse = "+")
  form=paste0("obs~b0+",form)
  for(p in 1:nrow(pred)){
    rnk=rank(dis.matrix[,p],ties.method = "first")
    bw_st=dis.matrix[which(rnk==bw),p]
    indx=which((dis.matrix[,p]<bw_st)&(dis.matrix[,p]>0))
    train_set=obs[indx,]
    bw_st=bw_st/2
    wi=exp(-.5*(dis.matrix[indx,p]/bw_st)^2)
    wi=wi*train_set[,"weights"]
    wi=wi/max(wi)
    model_status=try({
      m=nls(as.formula(form),
          data=train_set,
          start = initials,
          control = list(maxiter = 50000, minFactor=1/2000, warnOnly=T),
          weights = wi,
          lower = rep(0,length(coefs)),
          upper=rep(1,length(coefs)),
          alg = "port")})
    if(class(model_status)=="try_error"){
      model_status=try({
        m=nls(as.formula(form),
              data=train_set,
              start = initials,
              control = list(maxiter = 50000, minFactor=1/2000, warnOnly=T),
              weights = wi,
              lower = rep(0,length(coefs)),
              upper=rep(1,length(coefs)),
              alg = "port")})
    }
    if(class(model_status)== "try-error"){
      gtwr_pred[p,]=c(p,rep(NA,3+length(bases)))
    }else{
      gtwr_pred[p,]=c(p,coef(m),sqrt(m$m$deviance()/119))
    }
    if(p%%100==0){
      print(paste0(p," out of ",nrow(pred)))
    }
  }
  gtwr_pred=as.data.frame(gtwr_pred)
  names(gtwr_pred)=c("ID","Intercept_Coef",sub("Pred","Coef",bases),"Residual")
  return(gtwr_pred)
}
