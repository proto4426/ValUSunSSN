## No more "raw" error values stored
'cvFromInterpolsvd_2' <- function(x, comp_max = 4, nembed = 2, nsmo = 81,
                                  method = "splines", niter = 5, n_folds = 5,
                                  min_keep_frac = 0.1, seed = 123, browser = F){
  if(browser) browser()  # Explore the function

  set.seed(seed)   ;   time <- proc.time()
  CVerrorByComp <- numeric(length = comp_max)

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
  x_removed <- x

  for(i in 1:ncol(x)){
    x_true[,i] <- x[index_station[[i]], i]

    # Place the new NA in the matrix
    x_removed[rownames(x) %in% mat_notNA_rm[,i] == T, i] <- NA
  }

  print("Start main loop")
  # Choose the index for the different folds
  index <- sample(nrow(mat_notNA_rm), nrow(mat_notNA_rm)/n_folds)
 replicate(10,sapply(nrow(mat_notNA_rm),
                     function(x) sample(x,nrow(mat_notNA_rm)/n_folds)))

  # Compute the Cross-validation Procedure
  for (j in 1:comp_max) {
    E <- 0

    for(k in 1:n_folds ){

      x_removed <- x_removed/n_folds

      interpol_K <- interpol_CrossVal(sqrt(x_removed+1), nembed = nembed,
                                           nsmo = nsmo, ncomp = j,
                                           niter = niter, method = method)
      x_new <- interpol_K$y.filled
      x_new <- (x_new * x_new) -1  # Inverse transform
      x_new[x_new<0] <- 0

      rownames(x_new) <- 1:nrow(x_new)

      # (Pre-)Allocate matrix of the same size to replace the estimated values
      x_estimate <- x_true

      # Allocate the estimated values in the matrix
      for(i in 1:ncol(x)){
        x_estimate[,i] <- x_new[rownames(x_new) %in% mat_notNA_rm[,i] == T, i]
      }

      N <- nrow(x_estimate) * ncol(x_estimate) # Number of replaced gaps
      E <-  E + sqrt( N^-1 * sum((x_estimate - x_true)^2) )

      cat("\n", "CV iter number", j, "with error", CVerrorByComp,  "\n")
    }
    CVerrorByComp[j] <- E / n_folds

  }

  ggplot(data.frame("Number of components" = (1:comp_max),
                    "RMSE CV" = CVerrorByComp),
         aes(x = Number.of.components, y = CVerrorByComp )) +
  geom_line() + geom_point() + theme_piss()


  cat("TOTAL time is ", (proc.time() - time)[3], " sec")

  return(list(list_error = interpol_K,  CVerrorByComp = CVerrorByComp))
}
