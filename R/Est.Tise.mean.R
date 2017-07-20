#' function estimated time series using monthly average method 071917

#' Dat_Frm: data frame of interest
#' Key.Name: Key
#' Key.List: List of Key variables to pass
#' Foc.Var: Variable of interest to get Area Under the Curve
#' Out.Var: Variable
#' @import dplyr
#' @import magrittr
#' @import lubridate
#' @export

Est.Tise.mean<-function(Dat_Frm,Key.Name,Key.List,Foc.Var,date.min=NULL,date.max=NULL){

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



  Est_Rslt<-list() ##call list

  for(i in 1:length(Key.List)){
Foc.mean.str<-"mean(%s)"%>%sprintf(Foc.Var)
Foc.mean.name<-"%s"%>%sprintf(Foc.Var)
Foc.sd.str<-"sd(%s)"%>%sprintf(Foc.Var)
Foc.sd.name<-"sd_%s"%>%sprintf(Foc.Var)

    df_bymonth<-Dat_Frm %>% group_by_(Key.Name) %>%
      filter_("%s=='%s'"%>%sprintf(Key.Name,Key.List[i]))%>%
      mutate(month = month(date), year= year(date)) %>%
      group_by(year, month, spcode) %>%
      summarise_(
                .dots= c(setNames(Foc.mean.str,Foc.mean.name),
                         setNames(Foc.sd.str,Foc.sd.name),
                        setNames("mean(date)","date")
                        )
                )

    Est_Rslt[[i]]<-df_bymonth

    #try(int.fun <- function(x) predict(fit, newdata=x)%>%setValueLimit(0,100)) #function setValueLimit: set upper and lower bound for prediction
    #try(Est_Rslt[[Out.Var]][i] <- integrate(int.fun, start_date, end_date)$value)
    #try(rm(fit))
  }

  Est_Rslt=do.call("rbind",Est_Rslt)
  return(Est_Rslt)

}
