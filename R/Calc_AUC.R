#' function area under the curve
#' @import dplyr
#' @import magrittr
#'
#'
#'
#'
#'
#'
#'
#'


Key.Name="tag"
Data_Frame=cts


Calc_AUC<-function(cts,Key.List,date.min=NULL,date.max=NULL){
  ##add Key.Name
  ##add Out.Var =AUC_Rslt


  MinD<-cts[["date"]]%>%as.numeric%>%min
  MaxD<-cts[["date"]]%>%as.numeric%>%max

  if(is.null(date.min)) {
    integral_x_min=0;
  } else if ((date.max%>%as.Date%>%as.numeric)<MinD){
    integral_x_min=0;
  } else {
    integral_x_min<-date.min%>%as.Date%>%as.numeric-MinD
  }
  if (is.null(date.max)) {
    integral_x_max=(MaxD-MinD);
  } else if ((date.max%>%as.Date%>%as.numeric)>MaxD) {
    integral_x_max=(MaxD-MinD);
  } else {
    integral_x_max<-date.max%>%as.Date%>%as.numeric-MinD
  }

  AUC_Rslt <- data.frame(Key.List, NA)
  colnames(AUC_Rslt)=c(Key.Name,"AUC_Rslt")

  for(i in 1:length(Key.List)){
    fit <- (cts %>% group_by(tag) %>%
              filter(tag==AUC_Rslt$tag[i] ) %>% arrange(date) %>%
              select(date,sBranch) %>% mutate(date=as.numeric(date)-min(as.numeric(date))) %>%
              loess(sBranch~date, data=.))
    try(int.fun <- function(x) predict(fit, newdata=x))
    try(AUC_Rslt$AUC_Rslt[i] <- integrate(int.fun, integral_x_min, integral_x_max)$value)
    try(rm(fit))
  }


  return()

}

