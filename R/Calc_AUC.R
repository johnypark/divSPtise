#' function area under the curve
#' @import dplyr
#' @import magrittr
#' 071817
#'
#'
#'
#'
#'
#'
#'


Key.Name="tag"
Data_Frame=cts


Calc_AUC<-function(cts,Key.Name,Key.List,Foc.Var,Out.Var,date.min=NULL,date.max=NULL){
  ##add Key.Name
  ##add Foc.Var
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

  ##
  ##
  ##
  ##
  AUC_Rslt <- data.frame(Key.List, NA)
  colnames(AUC_Rslt)=c(Key.Name,Out.Var)









  for(i in 1:length(Key.List)){

    date.vec=sprintf("as.numeric(%s)-min(as.numeric(%s))","date","date")
    setNames(date.vec,"date")
    fit <- cts %>% group_by_(Key.Name) %>%
      filter_("%s==%s"%>%sprintf(Key.Name,AUC_Rslt[[Key.Name]][i])) %>%
      arrange(date) %>%
      select_("date",Foc.Var) %>%
      mutate_(.dots = setNames(date.vec,"date")) %$%
      loess(.[[Foc.Var]]~.[["date"]])

    try(int.fun <- function(x) predict(fit, newdata=x))
    try(AUC_Rslt[[Out.Var]][i] <- integrate(int.fun, integral_x_min, integral_x_max)$value)
    try(rm(fit))
  }


  return(AUC_Rslt)

}
