## reading and pre-processing the daily sun spot data
## this is very rudimentary code written by C. Ritter
## 13/7/2016 to have a basis for discussion with
## Laure Lef?vre and Rainer von Sachs at the observatory


## -----------------------------------------------------
## functions
## the Anscombe transformation for variance stabilization
## the alpha parameter is set to 4.2 by default
# Anscombe0<-function(x,alpha=4.2,inverse=FALSE){
#   if(inverse){
#     return(((x/2*alpha)^2-3/8*alpha^2)/alpha)
#   } else {
#     return(2/alpha*sqrt(alpha*x+3/8*alpha^2))
#   }
# }
#
# ## smooth Anscombed data by a running
# ## mean over 91 days (one quarter)
# ## and transform back
# smoothit<-function(x,fitted,nmed=91,alpha=4.2){
#   x[is.na(x)]<-fitted[is.na(x)]
#   return(Anscombe0(rollmean(Anscombe0(x,alpha=alpha),
#                             k=nmed,na.pad=TRUE),inverse=TRUE))
# }
