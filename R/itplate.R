#interpolation function __itplate__
## function adding itplation of IntrPol_var to data fame, with two keys _Prime_key and Secnd_Key_ from Time_full range, find missing
## time points and fill data from missing time points, uses date time variable

itplate<-function(Dat_Frm,Data_Type,IntrPol_Var,Time_Var=NULL,Prime_Key=NULL,Secnd_Key=NULL,Time_Full=NULL){
  ##depend
  ##on package magrittr
  ##on function : ln_approx
  #default: timepoints is from Time_Full: all the dates from UAV flights
  if(is.null(Prime_Key)) Prime_Key<-"tag"
  if(is.null(Secnd_Key)) Secnd_Key<-"spcode"
  if(is.null(Time_Var)) Time_Var<-"date"
  if(is.null(Time_Full)) Time_Full<-Dat_Frm[[Time_Var]] %>% as.Date %>% unique
  Dat_Frm.index.na<-(Dat_Frm[[IntrPol_Var]]%>%is.na) # Get index for rows with NA values for variable of interest
  Dat_Frm %<>% .[!Dat_Frm.index.na,] # Remove row with NA values
  Dat_Frm[[Time_Var]] %<>% as.Date
  Dat_Frm[["QC"]] <-Data_Type ##add new column QC to data frame
  Dat_Frm.array.KeyMatch <-Dat_Frm[[Prime_Key]]%>%unique
  Dat_Frm %<>% filter(!is.na(Prime_Key))


  Dat_Frm.itrpl.LIST=list()
  for (i in 1:length(Dat_Frm.array.KeyMatch)){
    sbst.Dat_Frm<-Dat_Frm %>% filter(tag==Dat_Frm.array.KeyMatch[i])
    sbst.Dat_Frm$Dcount<-sbst.Dat_Frm[[Time_Var]] %>% as.numeric
    index.MssngPnts<-(!(Time_Full)%in%sbst.Dat_Frm[[Time_Var]]) %>%which

    if (length(index.MssngPnts)>0){
      itrpl.leaf<- sbst.Dat_Frm %>% ln_approx("Dcount",IntrPol_Var, xout<-(Time_Full %>%as.numeric)[index.MssngPnts])
      Dat_Frm.itrpl<-data.frame(as.Date(itrpl.leaf[["x"]], origin="1970-1-1"),itrpl.leaf[["y"]], QC="interpolation")
      colnames(Dat_Frm.itrpl)[1:2]<-c(Time_Var,IntrPol_Var)


      Dat_Frm.itrpl[[Prime_Key]]<- Dat_Frm.array.KeyMatch[i] ##add new column tag to data frame
      Dat_Frm.itrpl[[Secnd_Key]] <-(sbst.Dat_Frm[[Secnd_Key]])[1]
      index.neg<-which(Dat_Frm.itrpl[[IntrPol_Var]]<0)
      Dat_Frm.itrpl[index.neg,IntrPol_Var]<-0
      Dat_Frm.itrpl.LIST[[i]]<-Dat_Frm.itrpl
    }
    else {}

  }
  sbst.Dat_Frm<-do.call("rbind",Dat_Frm.itrpl.LIST)
  return(sbst.Dat_Frm)
}
