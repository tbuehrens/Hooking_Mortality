#prediction intervals GAM
gam_pred_ints_v2<-function(fit_gam,new_dat,quants,filter){
  beta <- coef(fit_gam)[!grepl(filter,names(coef(fit_gam)))]
  
  if(min(eigen(vcov(m1))$value) < 0){
    Vtest<-vcov(m1)
    V=bend(vcov(m1))$bent
    rownames(V)<-rownames(Vtest)
    colnames(V)<-colnames(Vtest)
  }else{
    V=vcov(m1)
  }
  #V <- vcov(fit_gam)[!grepl(filter,rownames(vcov(fit_gam))),!grepl(filter,colnames(vcov(fit_gam)))]
  V <- V[!grepl(filter,rownames(vcov(fit_gam))),!grepl(filter,colnames(vcov(fit_gam)))]
  
  num_beta_vecs <- 10000
  Cv <- chol(V)
  set.seed(1)
  nus <- rnorm(num_beta_vecs * length(beta))
  beta_sims <- beta + t(Cv) %*% matrix(nus, nrow = length(beta), ncol = num_beta_vecs)
  covar_sim <- predict(fit_gam, newdata = new_dat, type = "lpmatrix")
  covar_sim <- covar_sim<- covar_sim[,!grepl(filter,colnames(covar_sim))]
  linpred_sim <- covar_sim %*% beta_sims
  invlink <-function(x) ilogit(x)
  ilogit_sim <- invlink(linpred_sim)
  y_sim<-data.frame(ilogit_sim)%>%
    as_tibble()%>%
    rowid_to_column()%>%
    pivot_longer(!rowid)%>%
    group_by(rowid)%>%
    summarise(value=quantile(value,quants,na.rm=T),q=quants,.groups='drop')%>%
    pivot_wider(values_from = value,names_from = q)%>%
    ungroup() %>%
    dplyr::select(!rowid)
  results<-list(quants=y_sim,preds=ilogit_sim)
  return(results)
}
