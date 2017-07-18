#' Function for correcting values out of range
#' @export
#'
setValueLimit<-function(array, lowerlimit, upperlimit){
  array[array>upperlimit]=upperlimit
  array[array<lowerlimit]=lowerlimit

  return(array)
}
