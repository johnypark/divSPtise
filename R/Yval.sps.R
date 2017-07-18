
#' loess function wrapper for time series in date
#' @importFrom stats loess
#' @import dplyr
#' @import magrittr


Yval.sps<-function(Dat_Fr,sp_list,Est.Var,loess.span)
{
  ##Depends on sLeaves sBranch
  sp_list%<>% unique %>% as.character
  df.LIST<-list()
  start.date<-Dat_Fr[["date"]] %>% as.Date %>% unique %>% as.numeric %>% min
  end.date<-Dat_Fr[["date"]] %>% as.Date %>% unique %>% as.numeric %>% max
  date.period=seq(start.date,end.date)  ##Date variable: need to make this more flexible

  for (j in 1:length(sp_list)){

  pred.leaf<-Data_Frame %>%
    filter(spcode==sp_list[j]) %>%
    group_by(date) %$%
    loess(.[[Est.Var]] ~ .[["date"]]%>%as.numeric, span=loess.span) %>% predict(.,data.frame(date = date.period), se = TRUE)

#setNames function
#use: setNames(date.vec,"date")
  Var.Name1<-sprintf("Loess_%s",Est.Var)
  Var.Name2<-sprintf("Sd_%s",Est.Var)
  y.date=as.Date(date.period,origin = "1970-01-01")
  df.LIST[[j]]<- data.frame(
    "spcode"=sp_list[j],
    "date"=y.date,
    setNames(pred.leaf$fit%>%setValueLimit(0,100),Var.Name1),
    setNames(pred.leaf$se.fit,Var.Name2)
    )

}
  ####
  do.call("rbind",df.LIST) %>%return
}
