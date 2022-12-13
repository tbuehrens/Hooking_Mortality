#functions to prepare brm outputs for marginaleffects, and marginaleffects outputs for tables and figures
#Author: Mark Roes
#Last modified: 10/6/22


ind.fun = function(x){
  ind = apply(x, 1, FUN = function(y) sum(ifelse(y == "Control", T, F)))
  out = cbind(x, ind)
  return(out)
}

covComb = function(dat=NULL, covars=NULL, type = c('observed','all')){
  
  if(length(covars) > 1){
    
    if(type == "observed"){
    level = dat %>%
      dplyr::select(all_of(covars)) %>%
      distinct() %>%
      droplevels()
  }
  
  if(type == 'all'){
    level = dat %>%
      dplyr::select(all_of(covars)) %>%
      apply(2,unique) %>%
      #as.tibble() %>%
      cross_df()
  }
      
    out = ind.fun(level) %>%
      filter(ind == 0 | ind == ncol(level)) %>%
      dplyr::select(-ind)
    
  }else{
    out = dat %>%
      dplyr::select(all_of(covars)) %>%
      apply(2,unique)
  }
  
return(out)
}



#Additional function to expand names
expandNames = function(data, covars = covars, sep = "_"){
  for(i in 1:length(covars)){
  data = data %>%
    mutate(!!covars[i] := word(name, sep = "_", i))
  }
  return(data)
}
