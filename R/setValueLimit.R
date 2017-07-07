#Function for correcting values out of range

setValueLimit<-function(array, lowerlimit, upperlimit){
  array[array>upperlimit]=upperlimit
  array[array<lowerlimit]=lowerlimit

  return(array)
}
