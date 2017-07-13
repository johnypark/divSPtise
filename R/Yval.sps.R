#' @importFrom stats loess
#'

Yval.sps<-function(Data_Frame,sp_list,loess.span)
{
  ##Depends on sLeaves sBranch
  sp_list%<>% unique %>% as.character
  df.LIST<-list()
  start.date<-cts[["date"]] %>% as.Date %>% unique %>% as.numeric %>% min
  end.date<-cts[["date"]] %>% as.Date %>% unique %>% as.numeric %>% max
  date.period=seq(start.date,end.date)  ##Date variable: need to make this more flexible

  for (j in 1:length(sp_list)){

  pred.leaf<-Data_Frame %>%
    filter(spcode==sp_list[j]) %>%
    group_by(date) %>%
    loess(sLeaves ~ as.numeric(date), data=., span=loess.span) %>% predict(.,data.frame(date = date.period), se = TRUE)


  y.date=as.Date(date.period,origin = "1970-01-01")
  df.LIST[[j]]<- data.frame("spcode"=sp_list[j],"date"=y.date,"Loess_Leaves"=setValueLimit(pred.leaf$fit,0,100),"Sd_Leaves"=pred.leaf$se.fit)

}
  ####
  do.call("rbind",df.LIST) %>%return
}
