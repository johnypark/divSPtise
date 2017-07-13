Yval.sps<-function(Data_Frame,sp_list, var1, var2)
{
  ##Depends on sLeaves sBranch
  sp_list%<>% unique %>% as.character
  df.LIST<-list()

  for (j in 1:length(sp_list)){

  fit.example<-Data_Frame %>%
    filter(spcode==sp_list[j]) %>%
    group_by(date) %>%
    loess(sLeaves ~ as.numeric(date), data=., span=0.4)

  Y=predict(fit.example,data.frame(date = seq(16345, 16702)), se = TRUE) ##Date variable: need to make this more flexible

  y.spcode=rep(sp_list[i],358)
  y.date=as.Date(X,origin = "1970-01-01")
  y.Leaf=Y$fit*100
  k=length(sp_list)

  df.LIST[[j]]<- data.frame("spcode"=sp_list,"date"=y.date,"Leaf_ratio"=y.Leaf)
}
  ####
  do.call("rbind",df.LIST) %>%return
}
