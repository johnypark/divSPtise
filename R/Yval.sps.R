Yval.sps<-function(Data_Frame,sp_list)
{
  ##Depends on sLeaves sBranch
  sp_list%<>% unique %>% as.character
  df.LIST<-list()
  date.period=seq(16345,16702)  ##Date variable: need to make this more flexible

  for (j in 1:length(sp_list)){

  pred.leaf<-Data_Frame %>%
    filter(spcode==sp_list[j]) %>%
    group_by(date) %>%
    loess(sLeaves ~ as.numeric(date), data=., span=0.4) %>% predict(.,data.frame(date = date.period), se = TRUE)


  y.date=as.Date(date.period,origin = "1970-01-01")
  df.LIST[[j]]<- data.frame("spcode"=sp_list[j],"date"=y.date,"Mean_Leaves"=setValueLimit(pred.leaf$fit,0,100),"Sd_Leaves"=pred.leaf$se.fit)

}
  ####
  do.call("rbind",df.LIST) %>%return
}
