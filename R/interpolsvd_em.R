
# ===============================================================
#' @export interpolsvd_em
#' @title interpolated SVD EM algorithm to fill missing values
#' @author Antoine Pissoort, \email{antoine.pissoort@@student.uclouvain.be}
#' @description
#' This main function fills gaps in monovariate or multivariate data
#' by SVD-imputation which is closely related to
#' expectation-maximization (EM) algorithm.
#' @param y a numeric data.frame or matrix of data with gaps
#' @param nembed  integer value controlling
#' embedding dimension (must be > 1 for monovariate data)
#' @param nsmo integer value controlling cutoff time  scale
#' in number of samples. Set it to 0 if only one single time scale is desired.
#' @param threshold1 numeric value controllingthe stop of the iterations after
#'  the relative energy change is < threshold
#' @param niter numeric value controlling the  maximum number of iterations
#' @param  ncomp  controls the number of significant components.
#' It has to be specified for running in automatic mode.
#' Default (0=) leads to manual selection during the algorithm
#' @param  displ boolean controlling the display of some information in
#' the console during the algorithm
#' @details
#'  The method decomposes the data into two time scales, which are processed
#' separately and then merged at the end. The cutoff time scale (nsmo) is
#' expressed in number of samples. A gaussian filter is used for filtering.
#' Monovariate data must be embedded first (nembed>1).
#' In the initial data set, gaps are supposed to be filled in with NA !!
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{y.filled}}{The same dataset as y but with gaps filled}
#'   \item{\code{w.distSVD}}{The distribution of the weights of the SVD}
#'   \item{\code{errorByComp}}{Numeric vector of length \code{niter} (??)
#'   containing the errors associated to each iterations( or comp?)}
#' }
#'
#' @examples
#'
#' # Take this for input, as advised in the test.m file
#' y <- sqrt(data.mat2.fin+1) # Select randomly here, for testing
#'
#' options(mc.cores=parallel::detectCores()) # all available cores
#'
#' z <- interpolsvd_em(y, nembed = 2, nsmo = 81, ncomp = 4,
#'                     niter = 30, displ = F)
#' # 193 sec for the whole dataset (with some stations discarded)

'interpolsvd_em' <-  function( y, nembed = 1, nsmo = 0, ncomp = 0,
                               threshold1 = 1e-5, niter = 30, displ = F){
  time <- proc.time() # measure time for computational issues

  if (nembed < 1)
    stop("Please choose nembed >1. If monovariate series, set default =1")
  if (nsmo < 1)
    stop("Please choose other cutoff. If 1 time scale is desired, set nsmo = 0")

  Emax <- 95  # max cumulative energy (%) for selecting nr of significant components

  # detect shape of input array and transpose in order to have more
  # rows than columns (faster)
  swap <- F # Transpose if too much columns, faster! transpose back in the end
  if ( ncol(y) > 2*nrow(y) ) {   y <- t(y) ;   swap <- T  }


  ## estimate average and standard deviation and standardise

  id.notNA <- apply(y, 2, function(x) which(!is.na(x)))
  obs.notNA <- as.vector(as.numeric(lapply(id.notNA, FUN = length)))

  # Anwsers if there is sufficient obs per station ?
  bad.obs <- which( obs.notNA <= 1 )
  # (From now,) we don't allow stations that have only one obs. Tune it !
  ave_y <- apply(y, 2, mean, na.rm = T )
  sd_y <- apply(y, 2, sd, na.rm = T ) # We remove NA's for this, so far
  y <- sweep(sweep(y, 2, ave_y, "-"), 2, sd_y, "/")
  # Control station that are more than 1 obs
  # And Fill values for station with all NA or with only 1 obs
  ave_y[bad.obs] <- mean(ave_y[-bad.obs]) ; sd_y[bad.obs] <- mean(sd_y[-bad.obs])
  # In matlab they replaced by 0 and 1 but it introduced errors.

  ## Perform some tests

  col.na <- apply(y, 2, function(x) all(is.na(x)))
  if ( any(col.na)  ) {
    cat("column(s)", colnames(y[col.na]), "have only missing values ! \n")
    stop('each column should have at least some valid values ')
    }
  if ( ncol(y)<2 & nembed<2 )
    stop(' embedding dimension must be >1 for (monovariate records ')
  if ( ncomp > ncol(y)*nembed )
    stop(paste('number of components cannot exceed ',ncol(y)*nembed))

  # embed records if necessary
  if(nembed>1)  x <- embedy(as.matrix(y), nembed, displ =  F)
  else   x <- y


  ## Weigh each record according to number of their # of Na's
  # Hence, larger weight is given to records with fewer gaps
  n.NA <- apply(x, 2, function(x) sum(is.na(x)))
  weight <- (nrow(x) - n.NA) / nrow(x)
  weight <- weight / max(weight)
  weight <- weight * weight

  x <- sweep(x, 2, weight, "*")

  # for display, choose the record that contains the largest # of gaps
  nNA <- sum(is.na(x))
  ind_gaps <- max(nNA)

  # first iteration: start by filling in gaps by linear interpolation
  # (Isn't it a bit 'poor' ??)

  xi <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
  ave_x <- rep(NA, ncol(x))
  xnew <- x
  for (i in 1:ncol(x)){
    w <- which(!is.na(x[,i]))
    ave_x[i] <- mean(x[w,i])   # see line 367 it is re-used
    xnew[,i] <- approx(c(0, w, nrow(x)+1), c(0, x[w,i], 0), (1:nrow(x)))$y
    # xnew with NA's replaced by (simple) linear interpolation

  }
  print(paste("total NA is : ", nNA))

  # subtract again the mean over the stations
  xnew <- sweep(xnew, 2, apply(xnew, 2, mean), "-")
  # Retrieve ind position of NA's for the imputations into the loop
  ind_Na <- which(is.na(x), arr.ind = T)

  # first estimate the dominant mode nr 1
  iter.count <- 0
  err <- numeric(length = niter) # store error assoc. to each # of comp.
  while ( iter.count < niter ){
    xfit <- rank_reduce(xnew, 1)  ;    xold <- xnew
    xnew[ind_Na] <- xfit[ind_Na]  # Now fit the NA's positions with the SVD approx.
    xnew <- sweep(xnew, 2, apply(xnew, 2, mean), "-")

    e <- xnew[ind_Na] - xold[ind_Na]
    err[iter.count+1]  <- sqrt( t(e) %*% e  / nNA) # no need here to create vector for err

    if ( err[iter.count+1]  < threshold1){ # If dominant mode is enough, we stop here
      cat(" iterations stopped at", iter.count, "for error =", err[iter.count+1])
      break
    }
    iter.count = iter.count + 1  
  }

    # ask for number of components if necessary
  if (ncomp < 1){
    svd <- svd(xnew)
    S <- svd$d  # do the SVD ; min(n,p) by default for both sing vec.
    U <- svd$u  ;   V <- svd$v   ;   Ak <- diag(S) # singuar values of the SVD
    E <- Ak %*% Ak
    E <- 100*E/sum(E)# fraction amount of energy for each SVD mode
    nE <- length(E)
    if (displ){
      ncomp2 <- readline(prompt = 'number of significant components ncomp  =')
      if ( any(ncomp2>nE) )  stop(paste(' ncomp must not exceed ', nE))
      else  ncomp2 <- 3
    }
    
    print(paste('using ',ncomp2,' components out of ',nE))
  } else {
    ncomp2 <- ncomp
    svd <- svd(xnew)
    S <- svd$d  ;   U <- svd$u  ;   V <- svd$v  ;   Ak <- diag(S)
  }
  print("main loop starts")
  if (nsmo > 1){
    for (k in 2:ncomp2){  # Now consider the other modes of the SVD until ncomp.
      iter.count <- 0
      while (iter.count < niter ){
        # No NA's are allowed trough (gaussian) smoothing function
        xlp <- smooth_gauss(xnew, nsmo)
        xhp <- xnew - xlp     ## Why doing these steps ?? (see above dec of fun)
        xlp <- rank_reduce(xlp, k)  ;     xhp <- rank_reduce(xhp, k)
        # After having applied the smoother for all stations,
        # we reduced the rank by SVD keeping k components.
        xold <- xnew
        xnew[ind_Na] <- xlp[ind_Na] + xhp[ind_Na]
        xnew <- sweep(xnew, 2, apply(xnew, 2, mean), "-") # average is over stations
        # and not over time ? check it
        e <- xnew[ind_Na] - xold[ind_Na]
        err[iter.count+1] <- sqrt( t(e) %*% e / nNA )   # Same as above : No need to alloc vector

        if (displ == T){
          print(paste('ncomp = ', k, '  iteration ', niter,' rel. error = ',
                      format(err,8)))
        }
        if (err[iter.count+1] < threshold1){
          cat(" iterations stopped at ", niter, "with error =",
              err[iter.count+1], "\n")
          break
        }
        niter = niter - 1
        cat("time after niter ", niter,  (proc.time() - time)[3], "sec", "\n")
      }
      if (displ == T ) cat('\n')
    }
  }else{
    for (k in 1:ncomp2){      iter.count <- 0
      while (iter.count < niter  ){
       xhp <- xnew  ;   xhp <- rank_reduce(xhp, k)
       xold <- xnew   ;    xnew[ind_Na] <- xhp[ind_Na]

       xnew <- sweep(xnew, 2, apply(xnew, 2, mean), "-")

       e <- xnew[ind_Na] - xold[ind_Na]
       err[iter.count+1] <- sqrt(t(e) %*% e/nNa)

       if (displ == T){
         cat('ncomp =  ', k,' iteration ', iter.count,
             '  rel. error = ',round(err,8))
       }
       if (err[iter.count+1] < threshold1) break
       niter = niter - 1
       cat("time after niter ", niter, "",  (proc.time() - time)[3], "sec", "\n")
     }
    }
   }
  # recompose the data by adding the mean
  for (i in 1:ncol(x)){ # As nr of columns is ~low, not important to vectorize
    w <- which(!is.na(x[,i]), arr.ind = T)
    xnew[,i] <- xnew[,i] / weight[i]
    xnew[,i] <- xnew[,i] - mean(xnew[w,i]) + ave_x[i]
  }

  # de-embed the data
  if (nembed > 1)  yf <- deembedy(xnew, ncol(y), 1, 0)
  else  yf <- xnew

  # restore mean and stdev
  for (i in 1:ncol(y)) {  # Number of cols (stations) is still low
    yf[,i] <- yf[,i] * sd_y[i] + ave_y[i]
  }
  #apply( yf, 2, function(x) x * sd_y + ave_y)

  if (swap)  yf <- t(yf)
  
  beepr::beep(sound = 8) # Little song to wake you up after this intense simulation ! 
  
  cat("Total time elapsed is", (proc.time() - time)[3], "sec")

  return(list(y.filled = yf,
              w.distSVD = Ak,
              errorByComp = err))
}

