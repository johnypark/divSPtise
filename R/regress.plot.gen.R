
#'============================================================================================================
#' function ..regress.plot.gen()
#'  this function is to generate "list" contain multiple plots with single x axis and multlie y varibes.
#'  output is "list"
#'  output can be recalled within multiplot function to generate figure
#'  for example, f<-regress.plot.gen(df,x_name,y_list); mutliplot(plotlist=f, cols=N) will give desirable plots
#' ============================================================================================================
#' @export

regress.plot.gen <- function(df_name,x_name,y_list,xlabname=NULL) {
  #df_name : dataframe name of interest
  #x_name : x axis of interest
  #y_list : list of variables to plot against x
  if(xlabname==NLL) xlabname="dry season deciduous"
  #(1) define a list to dump plots -figure of functional traits
  f.ft<-list()

  #(2) iternate through y_list
  for (j in 1:length(y_list)){

    ##(2)-1##
    sbst<-data.frame("x"=df_name[[x_name]], "y"=df_name[[y_list[j]]]) #for each y_var, generate subset of dataframe that only contains x_var and each y_var
    ## "deciduous"=df_name[["deciduous"]]
    #(2)-2# #Getting R^2 and p values
    Model<-lm(df_name[[y_list[j]]]~df_name[[x_name]])#using linear model function, get summary
    sm<-summary(Model)
    p.val2<-sm$coefficients[2,"Pr(>|t|)"]%>% round(2) #p-value: round up 2 digits

    #(2)-2-1 #If p value is lower than 0.01, add text p<0.01
    if (p.val2<0.01)
      p.val.text<-", p<0.01"
    else
      p.val.text<-paste(", p=",p.val2,sep="")

    #(2)-2-2 #add R square and p value to one string --f1.title
    f1.title<-paste("R^2=",sm$r.squared%>% round(2),p.val.text,sep="")

    newx <- seq(min(sbst$x,na.rm=TRUE), max(sbst$x,na.rm=TRUE), length.out=52)
    conf_interval <- predict(Model, newdata=data.frame("newx"=newx), interval="confidence", level = 0.95)
    sbst[["upperCI"]]=conf_interval[,2]
    sbst[["lowerCI"]]=conf_interval[,3]

    #(2)-3# # generate figure for each y_var,
    f1<-ggplot(sbst,aes(x=x,y=y))+geom_point()+geom_smooth(method="lm",se=FALSE) #points and regression line
    f1<-f1+xlab(xlabname)+ylab(y_list[j])+
      ylim(c(min(sbst$y,na.rm=TRUE),max(sbst$y,na.rm=TRUE)))#add y variable name
    f1<-f1+ggtitle(f1.title) #add R square and p value
    #f1<-f1+scale_color_manual(values=c("#ff9933", "#cccc00", "#996600", "#006600"),
    # labels=c("Brevi Deciduous", "Facultative Deciduous",
    # "Obligate Deciduous", "Evergreen"),NULL)
    f1<-f1+geom_line(aes(y=upperCI), color = "red", linetype = "dashed")+geom_line(aes(y=lowerCI), color = "red", linetype = "dashed")
    f1<-f1+theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    # legend.position=c(0.75,0.18), legend.background=element_rect(fill=alpha('white',0)))
    f.ft[[j]]<-f1 #dump each figure to each list member
  }
  #(3) Final result will be N member in list with N y variables
  return(f.ft)
}
