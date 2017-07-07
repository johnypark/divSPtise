#interpolation function __itplate__
## function adding itplateation to the dataframe

itplate<-function(df,Data_Type,IntrPol_Var,Time_Var=NULL,Prime_Key=NULL,Secnd_Key=NULL,Time_Full=NULL){
  ##depend
  ##on package magrittr
  ##on function : ln_approx
  #default: timepoints is from Time_Full: all the dates from UAV flights
  if(is.null(Prime_Key)) Prime_Key<-"tag"
  if(is.null(Secnd_Key)) Secnd_Key<-"spcode"
  if(is.null(Time_Var)) Time_Var<-"date"; df[[Time_Var]] %<>% as.Date
  if(is.null(Time_Full)) Time_Full<-df[[Time_Var]] %>% as.Date %>% unique
  df[["QC"]] <-Data_Type ##add new column QC to data frame
  df.array.KeyMatch <-df[[Prime_Key]]%>%unique
  df.itrpl.LIST=list()
  for (i in 1:length(df.array.KeyMatch)){
    sbst.df<-df %>% filter(tag==df.array.KeyMatch[i])
    sbst.df$Dcount<-sbst.df[[Time_Var]] %>% as.numeric
    index.MssngPnts<-(!(Time_Full)%in%sbst.df[[Time_Var]]) %>%which

    if (length(index.MssngPnts)>0){
      itrpl.leaf<- sbst.df %>% ln_approx("Dcount",IntrPol_Var, xout<-(Time_Full %>%as.numeric)[index.MssngPnts])
      df.itrpl<-data.frame(as.Date(itrpl.leaf[["x"]], origin="1970-1-1"),itrpl.leaf[["y"]], QC="interpolation")
      colnames(df.itrpl)[1:2]<-c(Time_Var,IntrPol_Var)


      df.itrpl[[Prime_Key]]<- df.array.KeyMatch[i] ##add new column tag to data frame
      df.itrpl[[Secnd_Key]] <-(sbst.df[[Secnd_Key]])[1]
      index.neg<-which(df.itrpl[[IntrPol_Var2]]<0)
      df.itrpl[index.neg,IntrPol_Var2]<-0
      df.itrpl.LIST[[i]]<-df.itrpl
    }
    else {}

  }
  sbst.df<-do.call("rbind",df.itrpl.LIST)
  return(sbst.df)
}
