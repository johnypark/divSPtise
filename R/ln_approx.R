#' ln_approx
#' Warpper function of approx
#' @importFrom stats approx
ln_approx <-function(df,x.name,y.name,xout){

  if(missing(xout)){
    output<-approx(x=df[[x.name]],y=df[[y.name]])
    xout=0;
  }
  else {
    output<-approx(x=df[[x.name]],y=df[[y.name]], xout)
  }
  return(output)
}
