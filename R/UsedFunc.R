#' @export embedy
#' @title Embedding time-series
#' @description
#' Embeds a set of time series c(x1, x2,..., xm) into an D-dimensional
#' space by taking as state vectors the consecutive sequences: (written in Matlab's style)
#' z(k,:) = [x1(k) x1(k+T) ... x1(k+D*T-1) x2(k) x2(k+T) ...  xm(k+D*T-1)]
#' for k=1 to n-D*T+1,  where n is the length of x and T is the embedding delay
#' @param x input numeric matrix where records are in columns; n rows and m columns
#' @param D integer specifying the embedding dimension
#' @param T positive integer specifying the embedding delay. Default to 1
#' @param displ	boolean controlling the display of the size of the embedded matrix

#' @return \code{y} the embedded numeric matrix with n-D+1 rows and D*m columns
#'
#' @examples
#' y <- sqrt(data.mat+1)  # data.mat contains the stations in columns and time in rows
#' yytest <- matrix(y[!is.na(y)][1:200], ncol = 8)
#' embedy(yytest, 3 )
'embedy' <- function(x, D, T = 1, displ = F){
  T <- as.integer(T)  ;   x <- as.matrix (x) # in case input is a single ts
  if( T <= 0 )	stop(' T must be > 0 and integer')
  nrow.y <- nrow(x) - (D-1)*T
  if(nrow.y < 2 )  stop(paste('embedding dimension D must be < ', (nrow(x)-1)/T))
  if(displ) print(paste('size of embedded matrix is : ', nrow.y,' x ',D*ncol(x)))

  y <- matrix(NA, nrow.y,  ncol(x)*D)
  for (j in 1:ncol(x)){
    for (i in 1:D)
      y[ ,i+(j-1)*D] <- x[(1:nrow.y)+(i-1)*T, j]
  }
  as.matrix(y)
  #return(list(nrow.y = nrow(y), y = as.matrix(y)))
}


## ==============================================================
#' @export smooth_gauss
#' @title Gaussian smoothing
#' @description
#' Smooths data in x using either a gaussian window
#' which has a total width given by 2*ns+1 (each side contains ns data points)
#'
#' @param x		data matrix to smooth (smoothing is done columnwise)
#' @param ns		smoothing width of the window
#' @return \code{y.smt}	corresponding to the smoothed data
#' @examples
#' y <- sqrt(data.mat+1)  # data.mat contains the stations in columns and time in rows
#' y_smm <- smooth_gauss(y, 8)
'smooth_gauss' <- function(x, ns){
  ns <- ceiling(ns)
  ns <- min(floor(nrow(x)/2-1), ns) # ns must not exceed n/2
  if (ns <=1 ){
    y.smt <- x
    return(y)
  }
  # Compute point's weights inside the window. Points near the center
  # of the window are given more weight.
  w <- t(-ns:ns)/ns ;   w <- exp(-3 * w^2)    ;    w <- w/sum(w) #; browser()

  a <- rep(1, ns) %*% t(apply(x[1:ns,], 2, mean, na.rm = T));
  b <- rep(1, ns) %*% t(apply(x[(nrow(x)-ns):nrow(x), ], 2, mean, na.rm = T) )
  #a <- sweep(x[1:ns, ], 1, ave_y )
  xnew <- rbind(a, x, b) ;  y.smt <- xnew   # Keep same matrix's struct.
  for (i in 1:ncol(x)){  # do the
    #y.smt[,i] <- rollapply(xnew[,i], 2*ns+1, FUN = stats::filter, filter = w )
    y.smt[,i] <- stats::filter(xnew[,i], filter = w)
  }

  y.smt <- y.smt[(ns+1):(nrow(x)+ns), ]
  return(invisible(y.smt))
}




# ===============================================================
#' @export deembedy
#' @title deembeds m-dimensional space
#' @description
#' Deembeds an m-dimensional space which has been created
#' by \code{embedy()}.
#' @param y	the embedded numeric matrix with n rows and m columns
#' @param nset integer specifying the number of data sets used to build y
#' @param Td	integer corresponding to the embedding delay
#' @param displ	boolean controlling the display of the matrix
#' @return the averaged state vector with n+(m-1)*T rows and 1 column
#'
#' @examples
#' y <- sqrt(data.mat+1)  # data.mat contains the stations in columns and time in rows
#' # select 8-columns
#' yytest <- matrix(y[!is.na(y)][1:200], ncol = 8)
#' y.embed <- embedy(yytest, 3 )
#' deembedy(y.embed)
'deembedy' <- function(y, nset = 1, Td = 1, displ = F){
  m <- nset   # original size of unembedded matrix
  M <- ncol(y)/nset    # embedding dimension
  n <- nrow(y) + (M-1)*Td

  if ( !all.equal( M, round(M, 0)) ) stop('nset does not match size of data matrix')
  if (displ == T){
    print(paste('embedding dimension : ', M) )
    print(paste('length of array     : ', n) )
  }
  x <- dx <- matrix(NA, nrow = n, ncol = m)
  xx <- matrix(NA, nrow = n, ncol = M)
  for (j in 1:m){
    for (i in 1:M){
      a <- t(rep(0,(i-1)*Td))  ;  b <- y[ ,i+(j-1)*M]  ;  c <- t(rep(0,((M-i)*Td)))
      #print(length(a)) ; print(length(b))  ; print(length(c))
      xx[ ,i] <- c( a, b, c )
    }
    nor <- as.integer((1:M*Td)/T-0.5) + 1
    a <- apply(xx, 1, mean)  ;  b <- t( c(nor, M*rep(1,n-2*M*Td), rev(nor)) )
    #print(str(a))  ;  print(str(b))
    x[ ,j] <- a / b * M
  }
  #return(list(x,M))
  x
}

## =====================================================================
#' @export rank_reduce
#' @title Rank reduding
#' @description
#' \code{rank_reduce()} generates a low-rank version version of matrix X by computing
#' its SVD, and then then reconstructing X by keeping only its ncomp most important
#' singular values.
#'
#' @param X Numeric matrix
#' @param ncomp   number of significant components
#' @return The corresponding reduced rank version of X
#' @details
#' This function bypasses the (slower) full computation of the SVD by
#' estimating the singular vectors through diagonalisation of the covariance matrix of X
#' @examples
#' rank_reduce(dataSSN, ncomp = 10)
'rank_reduce' <- function(X, ncomp){
  #### swap matrix if # of columns > # of rows
  if (ncol(X) > nrow(X)){
    swap <- T  ;  X <- t(X)
  } else   swap <- F

  # Defining the matrices to store eigenvalues and eignevectors
  V <- D <- matrix(NA, nrow = nrow(X), ncol = ncol(X))
  M <- t(X) %*% X  # covariance matrix
  D <- eigen(M)$values # diagonalize M
  V <- eigen(M)$vector
  US <- X%*%V
  Xfit <- US[,1:ncomp] %*% t(V[,1:ncomp])

  if (swap == T)  Xfit <- t(Xfit)
  Xfit
}
# enables  the  fast  computation  of  the  SVD  and  related  methods,
## facilitated by randomized algorithms --> more efficient
## However, from now, profiling in the main function shows that its weight
## is  relatively minimal in the computational time compared


# ===============================================================
#' @export theme_piss
#' @title Homogeneous theme for ggplots
#' @author Antoine Pissoort, \email{antoine.pissoort@@student.uclouvain.be}
#' @description
#' Theme function for our builded \code{ggplots}.
#' Useful get coherent and similar colours, themes,... for all plots.
#' Also useful to decrease the number of lines of code.
#'
#' @param size_p Size of the plot's title.
#' @param size_c Size of the axis' title.
#' @param size_l Size of the legend's title.
#' @param theme ggplot's theme for the plot.
#' @param ... enables to add other theme's specifications for the plots, see
#' \url{http://ggplot2.tidyverse.org/reference/theme.html}
#'
#' @return A personalized ggplot2 theme object to add to every builded plots.
#' @details
#' This function is useful to decrease the amount of code for each ggplots
#' generated as this thesis will use exclusive \code{ggplot2} for the plots.
#' Values of other parameters such as colours could be changed inside the function.
#' @examples
#' df_uccle_vero <- data.frame(time = z1$Date[1:nrow(filledt.vero)], SSN_wnUC2 = filledt.vero$UC2, from = "Vero")
#'
#' df_uccle_c <- rbind.data.frame(df_uccle_vero, data.frame(time = z1$Date[1:nrow(df_uccle_vero)],
#'                                                         SSN_wnUC2 = zssn[,"wnUC2"][1:nrow(df_uccle_vero)], from = "`interpolEM`"))
#'
#' ggplot(df_uccle_c, aes(col = from)) +
#'   geom_point(aes(x = time, y = SSN_wnUC2), size = 0.15) +
#'   solar.cycle() +
#'   guides(colour = guide_legend(override.aes = list(size= 3))) +
#'   ggtitle("Filled missing values") +
#'   theme_piss()
"theme_piss" <-
  function(size_p = 18, size_c = 14, size_l = 12, theme = theme_bw(), ...){

    text <- function(size,...) element_text(size = size, colour = "#33666C",
                                            face="bold", ...)
    theme +
      theme(plot.title = text(size_p, hjust = 0.5),
            axis.title = text(size_c),
            legend.title = text(size_l),
      ) +
      theme(legend.background = element_rect(colour = "black"),
            legend.key = element_rect(fill = "white"), ...
      )
}




# ===============================================================
#' @export solar.cycle
#' @title Solar cycles on ggplots
#' @author Antoine Pissoort, \email{antoine.pissoort@@student.uclouvain.be}
#' @description
#' Function to plot vertical lines for adding to ggplots, which correspond to the solar cycles.
#' Dates found for the solar Cycles comes from
#'  \url{https://en.wikipedia.org/wiki/List_of_solar_cycles}
#' @param col choose color for the vertical lines
#' @param size choose size for the lines
#' @return vertical lines plotted on ggplots showing the solar cycles
#' @details
#' This function is useful to decrease the amount of code for each ggplots
#' generated as this thesis will use exclusive \code{ggplot2} for the plots.
#' Values of other parameters such as colours could be changed inside the function.
#' @examples
#' df_uccle_vero <- data.frame(time = z1$Date[1:nrow(filledt.vero)], SSN_wnUC2 = filledt.vero$UC2, from = "Vero")
#'
#' df_uccle_c <- rbind.data.frame(df_uccle_vero, data.frame(time = z1$Date[1:nrow(df_uccle_vero)],
#'                                                         SSN_wnUC2 = zssn[,"wnUC2"][1:nrow(df_uccle_vero)], from = "`interpolEM`"))
#'
#' ggplot(df_uccle_c, aes(col = from)) +
#'   geom_point(aes(x = time, y = SSN_wnUC2), size = 0.15) +
#'   solar.cycle() +
#'   guides(colour = guide_legend(override.aes = list(size= 3))) +
#'   ggtitle("Filled missing values") +
#'   theme_piss()
'solar.cycle' <- function( col = 3, size = 0.3) {
  list(
    geom_vline(xintercept = as.numeric(as.Date("1923-05-01")), col = col, size = size),
    geom_vline(xintercept = as.numeric(as.Date("1933-11-01")), col = col, size = size),
    geom_vline(xintercept = as.numeric(as.Date("1944-02-01")), col = col, size = size),
    geom_vline(xintercept = as.numeric(as.Date("1954-02-01")), col = col, size = size),
    geom_vline(xintercept = as.numeric(as.Date("1964-10-01")), col = col, size = size),
    geom_vline(xintercept = as.numeric(as.Date("1976-05-01")), col = col, size = size),
    geom_vline(xintercept = as.numeric(as.Date("1986-03-01")), col = col, size = size),
    geom_vline(xintercept = as.numeric(as.Date("1996-06-01")), col = col, size = size),
    geom_vline(xintercept = as.numeric(as.Date("2008-01-01")), col = col, size = size)
  )
}
