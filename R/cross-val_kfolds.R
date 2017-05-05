# ===============================================================
#' @export cvFromInterpolsvd_kfolds
#' @title  interpolsvd_em algorithm for K-fold cross-validation
#' @author Antoine Pissoort, \email{antoine.pissoort@@student.uclouvain.be}
#' @description
#' No more "raw" error values are  stored
#' @param comp_max Maximum number of component we want to test
#' @param n_folds integer denoting the number of folds we take and
#'  which also determines the fraction of values
#' we take for doing the cross-validation (='min_keep_frac' parameter
#' in the other version)
#' @return Vector of size comp_max containing the CV errors
#' @seealso \code{\link{interpolsvd_em}} for information about the paramters and
#' the returned values which are the same.
#' @examples
#'
'cvFromInterpolsvd_kfolds' <- function(x, comp_max = 4, nembed = 2, nsmo = 81,
                                  method = "splines", niter = 5, n_folds = 5,
                                  seed = 123, browser = F){
  if(browser) browser()  # Explore the function

  set.seed(seed)   ;   time <- proc.time()
  CVerrorByComp <- numeric(length = comp_max)

  rownames(x) <- 1:nrow(x) # Ensure the rownames are OK for indexing


  # Vectorize the matrix for computational issues
  flat_x <- c(as.matrix(x))
  flat_indices <- c(rownames(x))

  # Retireve indice of non-missing values and the number
  flat_indices_notNA <- which(!is.na(flat_x), arr.ind = F)
  n_notNA <- length(flat_indices_notNA)

  # Number n of values to which we will compute the CV
  n <- floor(n_notNA * n_folds^-1)

  # Random sampling
  flat_indices_notNA <- sample(flat_indices_notNA)


  print("Start main loop")

  # Choose the index for the different folds
  # index <- sample(nrow(mat_notNA_rm), nrow(mat_notNA_rm)/n_folds)
  # replicate(10,sapply(nrow(mat_notNA_rm),
  #                     function(x) sample(x,nrow(mat_notNA_rm)/n_folds)))

  ### Compute the Cross-validation Procedure
  for (j in 1:comp_max) {
     E <- 0
   for(k in 0:(n_folds-1)){

     cat("\n", "Fold number", k+1, "\n")

      test_x <- flat_x
      test_x[flat_indices_notNA[(k*n+1):((k+1)*n)]] <- NA

      test_xMat <- matrix(test_x, nrow = nrow(x), ncol = ncol(x))


      interpol_K <- interpol_CrossVal(sqrt(test_xMat+1), nembed = nembed,
                                           nsmo = nsmo, ncomp = j,
                                           niter = niter, method = method)
      test_xFilled <- interpol_K$y.filled
      test_xFilled <- (test_xFilled * test_xFilled) -1  # Inverse transform
      test_xFilled[test_xFilled<0] <- 0

      # Vectorize too, (for the comparisons)
      test_xFilled <- c(as.matrix(test_xFilled))




      N <- n / n_folds # Number of replaced gaps
      # Compute the error and sum it over the folds
      E <-  E + sqrt( N^-1 * sum( (test_xFilled[flat_indices_notNA[(k*n+1):((k+1)*n)]] -
                                   flat_x[flat_indices_notNA[(k*n+1):((k+1)*n)]] )^2) )

   }
     # Compute the cv Root Mean SE
    CVerrorByComp[j] <- E / n_folds
    cat("\n", "CV iter for component ", j, " has error ", CVerrorByComp[j],  "\n")

  }

 g <-  ggplot(data.frame("Number of components" = (1:comp_max),
                    "RMSE CV" = CVerrorByComp),
         aes(x = Number.of.components, y = CVerrorByComp )) +
    geom_line() + geom_point() + theme_piss()
 print(g)

  cat("TOTAL time is ", (proc.time() - time)[3], " sec")

  return(list(CVerrorByComp = CVerrorByComp))
}
