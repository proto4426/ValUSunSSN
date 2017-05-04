library(ValUSunSSN)
data("data.mat2.fin")
data("data.mat")

#y <- sqrt(data.mat2.fin+1)
y <- data.mat2.fin


# Count number of avaliable observations by stations
id.notNA <- apply(y[,-ncol(y)], 2, function(x) which(!is.na(x)))
obs.notNA <- unlist(lapply(id.notNA, FUN = length))

# Select stations with enough observations (test)
select_col <- unname(which(obs.notNA > 15e3))


# Function to return the index of nonmissing cases
'which_notNA' <- function(x, matrix = F)  {
  apply(x, 2, function(y) which(!is.na(y), arr.ind = matrix))
}
y_nnaF <- which_notNA(y, matrix = T) # Example



# Take a smaller dataset to test the functions
str(y_small <- y[data.mat$decdate>1999, c(names(select_col))])


# Test the functions on the small dataset to estimate computation time
z <- interpolsvd_em(y_small, nembed = 2, nsmo = 81,
                    ncomp = 4, niter = 30, displ = F)
z_s <- interpol_splines(y_small, nembed = 2, nsmo = 81,
                        ncomp = 4, niter = 30, displ = F)



####  CV on all the dataset with the method of splines.

cv_all_splines <- cvFromInterpolsvd(x = y, comp_max = 10, method = "splines",
                                    niter = 30, min_keep_frac = 0.2)
# 500 sec for y_small[7400,13] with smoothgauss
# around 2h for whole dataset with Splines
save(cv_all_splines, file = "Data_hidden//cv_all_splines.RData")
# Save smaller object
cv_all_splines_err = cbind(cv_all_splines$errorByComp, cv_all_splines$CVerrorByComp)
save(cv_all_splines_err, file = "Data_hidden//cv_all_splines_err.RData")



### CV on the 5 stations with the most observed cases

selectedStations <- tail(obs.notNA[order(obs.notNA)], n = 5 )
y_selectedStations <- y[, names(selectedStations)]
#y_selectedStations <- y[9500:nrow(y), names(selectedStations)]

cv_5_splines <- cvFromInterpolsvd(x = y_selectedStations,
                                  comp_max = 7, method = "splines",
                                  niter = 30, min_keep_frac = 0.2, brow = F)
# 254 sec
cv_5_splines_err = cbind(cv_5_splines$errorByComp, cv_5_splines$CVerrorByComp)
save(cv_5_splines_err, file = "Data_hidden//cv_5_splines_err.RData")




library(ggplot2)
library(gridExtra)
library(ValUSunSSN)
g1 <- ggplot(data.frame("Number of components" = (1:10),
                        "errorByComp" = y_obsToNA$errorByComp),
             aes(x = Number.of.components, y = errorByComp )) +
  geom_line() + geom_point() + theme_piss()
g2 <- ggplot(data.frame("Number of components" = (1:10),
                        "CVerrorByComp" = y_obsToNA$CVerrorByComp),
             aes(x = Number.of.components, y = CVerrorByComp )) +
  geom_line() + geom_point() + theme_piss()
g2

grid.arrange(g1,g2)

###########" TEST

test_cvsplines <- cvFromInterpolsvd_2(x = y_selectedStations, comp_max = 6,
                                      method = "splines", niter = 30, n_folds = 5,
                                      min_keep_frac = 0.2, browser = T)



## REPEATED cross-validation to obtain more accurate (less variable) results ?

M <- 5  ;  comp_max = 6  ;   error_cv <- list()
t <- proc.time()
for(i in 1:M){
  rep_cv <- cvFromInterpolsvd(y_small, comp_max = comp_max, nembed = 2, nsmo = 81,
                              method = "splines",
                              niter = 5, min_keep_frac = 0.1, seed = i+123)
  error_cv[[i]] <-  rep_cv$CVerrorByComp
}
(proc.time()-t)[3]

# Compute the error mean from the repeated simulations
mean_cv_error <- colMeans(do.call(rbind, error_cv))

ggplot(data.frame(Number.of.components = 1:comp_max,
                  CVmeanError = mean_cv_error),
       aes(x = Number.of.components, y = CVmeanError)) +
  geom_line() + geom_point() + theme_piss()


### Do it in PARALLEL to save some time !

library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]-1) #not to overload the computer
registerDoParallel(cl)

t <- proc.time()
cvError_parallel <- foreach(i = 1:M, .combine=cbind,
                           .packages = c("ValUSunSSN", "ggplot2", "gridExtra")) %dopar% {
  rep_cv <- cvFromInterpolsvd(y_small, comp_max = comp_max, nembed = 2, nsmo = 81,
                              method = "splines",
                              niter = 5, min_keep_frac = 0.1, seed = i+123)
  rep_cv$CVerrorByComp
}
(proc.time()-t)[3]
mean_cv_error_par <- rowMeans(cvError_parallel)
stopCluster(cl)

ggplot(data.frame(Number.of.components = 1:comp_max,
                  CVmeanError = mean_cv_error_par),
       aes(x = Number.of.components, y = CVmeanError)) +
  geom_line() + geom_point() + theme_piss()
