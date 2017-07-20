#' Calc_AUC area under the curve
#'
#'
#'
#'
#' @import magrittr
#' @import dplyr
#

Calc_AUC<-function(Dat_Frm,Key.Name,Foc.Var,time.Var,...){
  Dat_Frm %>% group_by(Key.Name) %>% summarise_()
    #duplicate or unique? if duplicate then keep




}
