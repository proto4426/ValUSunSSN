library(ValUSunSSN)
data("data.mat2.fin")
data("data.mat")

y <- sqrt(data.mat2.fin+1)


# Count number of avaliable observations by stations
id.notNA <- apply(y[,-ncol(y)], 2, function(x) which(!is.na(x)))
obs.notNA <- as.vector(as.numeric(lapply(id.notNA, FUN = length)))

select_col <- which(obs.notNA > 15e3) # Select stations with enough observations



y_nna <- apply(y, 2, function(x) which(!is.na(x)))

which_notNA <- function(x, matrix = F)  {
  apply(x, 2, function(y) which(!is.na(y), arr.ind = matrix))
}
y_nnaF <- which_notNA(y, matrix = T)



y_obs <- apply(y, 2, function(x)x[,])
y_obs <- matrix(y[!is.na(y)], ncol = ncol(y))

y_obs <- matrix(NA, ncol = ncol(y))
for(i in 1:length(y_nna)){
  y_obs[,i] <- y_na[[i]]
}


id.notNA <- apply(y_small, 2, function(x) which(!is.na(x)))
obs.notNA <- as.vector(as.numeric(lapply(id.notNA, FUN = length)))



# Take a smaller dataset to test the functions
str(y_small <- y[data.mat$decdate>1995, c(select_col,  4, 5)])



# Test the functions on the small dataset

z <- interpolsvd_em(y_small, nembed = 2, nsmo = 81,
                    ncomp = 4, niter = 30, displ = F)


z_s <- interpol_splines(y_small, nembed = 2, nsmo = 81,
                        ncomp = 4, niter = 30, displ = F)





y_obsToNA <- cvFromInterpolsvd(x = y, comp_max = 10,
                               niter = 30, min_keep_frac = 0.2)
save(y_obsToNA, file = "cv_all_splines.RData")
# 500 sec for y_small[7400,13] on smoothgauss
#

y_obsToNA$errorByComp  ;  y_obsToNA$CVerrorByComp

