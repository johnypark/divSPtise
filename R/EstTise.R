##wrapper around Yval.sps
##use Yval.sps as internal
EstTise<- function (Data_Frame,sp_list, Var_Interest=NULL, method=NULL){
  ##if Var_Interest is not one of colname in Data_Frame than error message
  #if (!Var_Interest%in%colnames(Data_Frame)){
  #  stop(paste(Var_Interest,"not a variable in",Data_Frame,sep=""))
  #}
  if(is.null(method)) method<-"mean"
  #if (!time.period%in%c("day","week","month","year")){
  #stop(paste("time.period must be one of the following: day, week, month, year. Current time.period= ",time.period,sep=""))
  #
  #}

  #[which(colnames(Data_Frame)==Var_Interest[number])] <- paste("group",number) something like this



  if (method=="mean"){ ##Depends on sLeaves, sBranch ##==> fix this to not depend on those variables

    df.LIST<-list()

    for (j in 1:length(sp_list)) {

      ##(2)-1##
      sbst<-Data_Frame %>% filter(spcode==sp_list[j])

      df_byweek=sbst%>%
        mutate(month = month(date), year= year(date), week=week(date)) %>%
        group_by(year, month,week, spcode) %>% summarise("Mean_Leaves"=mean(sLeaves),"Sd_Leaves"=sd(sLeaves), "Mean_Branch"=mean(sBranch), "Sd_Branch"=sd(sBranch),date=mean(date))

      df.LIST[[j]]<-df_byweek
    }

    df_0712 <- do.call("rbind", df.LIST)
    return(df_0712)
  }

  else if (method=="loess"){
    Yval.sps(Data_Frame,sp_list) %>% return

  }


}
