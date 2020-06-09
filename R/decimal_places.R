# A handy function found at:
#https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r

#This is used to determine what the accuracy should be
decimal_places <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}
