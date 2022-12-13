#Function to standardize variables using 2*SD, a la Gelman (2007)

stdGelman = function(x){
  out = (x-mean(x, na.rm = T))/ (sd(x, na.rm = T)*2)
    
  return(out)
}
