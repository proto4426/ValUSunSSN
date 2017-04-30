# ===============================================================
#' @export interpol_CrossVal
#' @title  interpolsvd_em algorithm for cross-validation
#' @author Antoine Pissoort, \email{antoine.pissoort@@student.uclouvain.be}
#' @description
#' This present a "smoother" version of \code{\link{interpolsvd_em}} to apply the
#' cross-validation.
#' @seealso \code{\link{interpolsvd_em}} for information about the paramters and
#' the returned values which are the same.
#' @examples
#'
'interpol_CrossVal' <- function( y, nembed = 1, nsmo = 0, ncomp = 0,
                                threshold1 = 1e-5, niter = 30) {
  time <- proc.time() # measure time for computational issues

  if (nembed < 1)
    stop("Please choose nembed >1. If monovariate series, set default =1")
  if (nsmo < 1)
    stop("Please choose other cutoff. If 1 time scale is desired, set nsmo = 0")

  Emax <- 95  # max cumulative energy (%) for selecting nr of significant components

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

  # Standardize the matrix
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

  # for display : choose the record that contains the largest # of gaps
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


  ## first estimate the dominant mode nr 1
  iter.count <- 0

  # store error assoc. to each # of comp. and iter
  err <- matrix(NA, nrow = niter, ncol = ncomp)

   while ( iter.count < niter ) {
    xfit <- rank_reduce(xnew, 1)  ;    xold <- xnew
    xnew[ind_Na] <- xfit[ind_Na]  # Now fit the NA's positions with the SVD approx.
    xnew <- sweep(xnew, 2, apply(xnew, 2, mean), "-")

    e <- xnew[ind_Na] - xold[ind_Na]
    err[iter.count+1, 1]  <- sqrt( t(e) %*% e  / nNA) # no need here to create vector for err

    if ( err[iter.count+1, 1]  < threshold1){ # If dominant mode is enough, we stop here
      cat(" iterations stopped at", iter.count, "for error =", err[iter.count+1])
      break
    }
    iter.count = iter.count + 1
  }


  ncomp2 <- ncomp
  svd <- svd(xnew)
  S <- svd$d  ;   U <- svd$u  ;   V <- svd$v  ;   Ak <- diag(S)


  print("main loop starts")

  # To consider the case where ncomp si only 1
  if (ncomp2 != 1) {

  if (nsmo > 1){
    for (k in 2:ncomp2){  # Now consider the other modes of the SVD until ncomp.
      iter.count <- 0
      while (iter.count < niter ){

        xlp <- zoo::na.spline(xnew)
        #xlp <- smooth_gauss(xnew, nsmo)

        xhp <- xnew - xlp     ## Why doing these steps ?? (see above dec of fun)
        xlp <- rank_reduce(xlp, k)  ;     xhp <- rank_reduce(xhp, k)
        # After having applied the smoother for all stations,
        # we reduced the rank by SVD keeping k components.
        xold <- xnew
        xnew[ind_Na] <- xlp[ind_Na] + xhp[ind_Na]
        xnew <- sweep(xnew, 2, apply(xnew, 2, mean), "-") # average is over stations

        e <- xnew[ind_Na] - xold[ind_Na]
        err[iter.count+1, k] <- sqrt( t(e) %*% e / nNA )   # Same as above : No need to alloc vector


        if (err[iter.count+1, k] < threshold1){
          cat(" iterations stopped at ", iter.count, "with error =",
              err[iter.count+1, k], "\n")
          break
        }
        iter.count = iter.count+1
        cat("time after niter ", iter.count,  (proc.time() - time)[3], "sec", "\n")
      }
    }
  }
  else {
    for (k in 1:ncomp2){
      iter.count <- 0
    while (iter.count < niter ){
      xhp <- xnew  ;   xhp <- rank_reduce(xhp, k)
      xold <- xnew   ;    xnew[ind_Na] <- xhp[ind_Na]

      xnew <- sweep(xnew, 2, apply(xnew, 2, mean), "-")

      e <- xnew[ind_Na] - xold[ind_Na]
      err[iter.count+1, k] <- sqrt(t(e) %*% e/nNa)

      if (err[iter.count+1, k] < threshold1) break
      iter.count = iter.count+1
      cat("time after niter ", iter.count, "",  (proc.time() - time)[3], "sec", "\n")
    }
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

  # Little song to wake you up after this intense simulation !

  cat("Total time elapsed is", (proc.time() - time)[3], "sec")

  return(list(y.filled = yf,
              w.distSVD = Ak,
              errorByIt = err))
}




# ===============================================================
#' @export cvFromInterpolsvd
#' @title cross-validation from interpolsvd_em algorithm
#' @author Antoine Pissoort, \email{antoine.pissoort@@student.uclouvain.be}
#' @description
#' It aims to compare
#'
#' The three tuneable (hyper)parameters are :
#' \describe{
#' \item{\code{ncomp}}
#' \item{\code{nsmo}}
#' \item{\code{nembed}}
#' }
#' @param x
#' @param comp_max
#' @param niter
#' @param min_keep_frac real between 0 and 1 controlling
#' @param seed controls the seed of the random index sampling
#' @details
#'
#' @return A grid of 2 ggplot representing the error and the cross-validation
#' error with respect to the number of components retained from the SVD. And a
#' list containing the following elements:
#' \describe{
#'   \item{\code{y.filled}}{}
#'   \item{\code{errorByComp}}{}
#'   \item{\code{CVerrorByComp}}{}
#'   \item{\code{matrix_notNA}}{}
#' }
#' @examples
#' library(ValUSunSSN)
#' data("data.mat2.fin")
#' y <- sqrt(data.mat2.fin+1)
#'
#' y_obsToNA <- cvFromInterpolsvd(x = y, comp_max = 10,
#'                               niter = 30, min_keep_frac = 0.2)
'cvFromInterpolsvd' <- function(x, comp_max = 4, niter = 5,
                                min_keep_frac = 0.1, seed = 123){
  set.seed(seed)   ;    interpol_K <- list() ;   time <- proc.time()
  errorByComp <- CVerrorByComp <- numeric(length = comp_max)

  rownames(x) <- 1:nrow(x) # Ensure the rownames are OK for indexing


  # Retrieve index of the stations that are not missing
  notNA <- which_notNA(x, matrix = T)

  # Take the minimal number of non-minssing values for the stations
  num_keep_obs <- min(unlist(lapply(notNA,  function(x) length(x))))

  # Sample the index of the matrix to remove for the CV
  index_removed <- lapply(notNA,
                          function(x) sample(x, num_keep_obs * min_keep_frac))
  # Transform this in a matrix for convenience
  mat_notNA_rm <- matrix(unlist(index_removed), ncol = ncol(x),
                         dimnames = list(NULL, colnames(x)))
  # Split the columns to obtain a list of vectors (for each stations)
  index_station <- split(mat_notNA_rm,
                         rep(1:ncol(mat_notNA_rm), each = nrow(mat_notNA_rm)))

  # Compute the matrix with true values to compare later with the estimates
  # Then Remove values and finally compute the algorithm with this matrix
  x_true <- matrix(NA, nrow = nrow(mat_notNA_rm), ncol = ncol(x))
  x_remove <- x

  for(i in 1:ncol(x)){
    x_true[,i] <- x[index_station[[i]], i]

    # Place the new NA in the matrix
    x_remove[rownames(x) %in% mat_notNA_rm[,i] == T, i] <- NA
  }

  # browser()
  for (j in 1:comp_max) {
    interpol_K[[j]] <- interpol_CrossVal(x_remove, nembed = 2, nsmo = 81,
                                         ncomp = j, niter = niter)
    errorByComp[j] <- interpol_K[[j]]$errorByIt[niter, j]

    x_new <- interpol_K[[j]]$y.filled
    rownames(x_new) <- 1:nrow(x_new)

    # Allocate matrix of the same size to replace the estimated values
    x_estimate <- x_true
    for(i in 1:ncol(x)){
      x_estimate[,i] <- x_new[rownames(x_new) %in% mat_notNA_rm[,i] == T, i]
    }

    N <- nrow(x_estimate) * ncol(x_estimate)
    CVerrorByComp[j] <- sqrt( N^{-1} * sum((x_estimate - x_true)^2) )

    cat("\n", "CV iter number", j, "\n")
  }


  g1 <- ggplot(data.frame("Number of components" = (1:comp_max),
                          "errorByComp" = errorByComp),
               aes(x = Number.of.components, y = errorByComp )) +
    geom_line() + geom_point()
  g2 <- ggplot(data.frame("Number of components" = (1:comp_max),
                          "CVerrorByComp" = CVerrorByComp),
               aes(x = Number.of.components, y = CVerrorByComp )) +
    geom_line() + geom_point()

  grid.arrange(g1,g2)


  #  df <- data.frame("Number of components" = rep((1:comp_max), 2),
  #                   "errorByComp" = c(errorByComp, CVerrorByComp),
  #                   "CV" = c(rep("F", comp_max), rep("T", comp_max)))
  # print( ggplot(df, aes(x = Number.of.components, y = errorByComp, col = CV )) +
  #           geom_line() + geom_point() )

  cat("TOTAL time is", (proc.time() - time)[3], "sec")

  return(list(list_error = interpol_K, errorByComp = errorByComp,
              CVerrorByComp = CVerrorByComp, matrix_notNA = mat_notNA))
  #obs_remove = obs_remove))
}
