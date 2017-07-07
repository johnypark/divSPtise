##1. replace NA values of desingated colname with assigning rplvalue
RplNA_val <-function(df,colname,rplvalue){

  idx.col.na<-is.na(df[[colname]]) %>% which # get row index when column=NA
  df[idx.col.na,colname]<-rplvalue#fill flower=NA with rplvalue
  return(df) #new dataframe with new rplvalues
}
