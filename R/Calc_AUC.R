#' function area under the curve
#' @import dplyr
#' @import magrittr
Calc_AUC<-function(cts,tagList,date.min=NULL,date.max=NULL){
  MinDate.DF<-cts[["date"]]%>%as.numeric%>%min
  if(is.null(date.min)) integral_x_min=0;
  else {
    integral_x_min<-date.min%>%as.Date%>%as.numeric-MinDate.DF
  }
  if(is.null(date.max)) integral_x_max=357;
  else {
    integral_x_max<-date.max%>%as.Date%>%as.numeric-MinDate.DF
  }


  sBranchAUC <- data.frame(tag=tagList, sBranchAUC=NA)

  for(i in 1:length(tagList)){
    fit <- (cts %>% group_by(tag) %>%
              filter(tag==sBranchAUC$tag[i] ) %>% arrange(date) %>%
              select(date,sBranch) %>% mutate(date=as.numeric(date)-min(as.numeric(date))) %>%
              loess(sBranch~date, data=.))
    try(int.fun <- function(x) predict(fit, newdata=x))
    try(sBranchAUC$sBranchAUC[i] <- integrate(int.fun, setValueLimit(integral_x_min,0,357), setValueLimit(integral_x_max,0,357))$value)
    try(rm(fit))
  }


  return()

}
