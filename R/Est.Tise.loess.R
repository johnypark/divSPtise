#' function estimated time series using loess method 071917

#' Dat_Frm: data frame of interest
#' Key.Name: Key
#' Key.List: List of Key variables to pass
#' Foc.Var: Variable of interest to get Area Under the Curve
#' Out.Var: Variable
#' @import dplyr
#' @import magrittr
#' @export

Est.Tise.loess<-function(Dat_Frm,Key.Name,Key.List,Foc.Var,date.min=NULL,date.max=NULL,...){
  ##time variable must be of name "date". Otherwise, change it to variable name: "date"
  MinD<-Dat_Frm[["date"]]%>%as.numeric%>%min
  MaxD<-Dat_Frm[["date"]]%>%as.numeric%>%max
  if(is.null(date.min)) {
    start_date=MinD;
  } else {
    start_date<-date.min%>%as.Date%>%as.numeric%>%setValueLimit(MinD,MaxD)
  }
  if(is.null(date.max)) {
    end_date=MaxD;
  } else{
    end_date<-date.max%>%as.Date%>%as.numeric%>%setValueLimit(MinD,MaxD)
  }
  #subset by given date first to create Dat_Frm subset--next goal



  Est_Rslt<-list() ##call list

  for(i in 1:length(Key.List)){

    date.vec=sprintf("as.numeric(%s)","date")
    #return("%s=='%s'"%>%sprintf(Key.Name,Key.List[i]))
    pred.focal<-Dat_Frm %>% group_by_(Key.Name) %>%
      filter_("%s=='%s'"%>%sprintf(Key.Name,Key.List[i])) %>%
      mutate_(.dots = setNames(date.vec,"date")) %>%
      group_by_("date") %$%
      loess(.[[Foc.Var]]~date,...) %>%
      predict(.,data.frame(date=c(start_date:end_date)),se = TRUE)
    #return(y)
    Est_Rslt[[i]]=data.frame(
      Key.List[i],
      pred.focal$fit%>%setValueLimit(0,100),
      pred.focal$se.fit,
      "date"=c(start_date:end_date)%>%as.Date(origin="1970-01-01")
    )
    colnames(Est_Rslt[[i]])=c(Key.Name,Foc.Var,"sd_%s"%>%sprintf(Foc.Var),"date")

    #try(int.fun <- function(x) predict(fit, newdata=x)%>%setValueLimit(0,100)) #function setValueLimit: set upper and lower bound for prediction
    #try(Est_Rslt[[Out.Var]][i] <- integrate(int.fun, start_date, end_date)$value)
    #try(rm(fit))

  }

  Est_Rslt=do.call("rbind",Est_Rslt)
  return(Est_Rslt)

}
