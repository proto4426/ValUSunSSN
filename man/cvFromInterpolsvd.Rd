% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cross-ValidationFuns.R
\name{cvFromInterpolsvd}
\alias{cvFromInterpolsvd}
\title{cross-validation from interpolsvd_em algorithm}
\usage{
cvFromInterpolsvd(x, comp_max = 4, nembed = 2, nsmo = 81,
  method = "splines", niter = 5, min_keep_frac = 0.1, seed = 123,
  brow = F)
}
\arguments{
\item{x}{the numeric matrix of SSN with days in rows and stations in columns for which we want
to compute the cross-validation based on inteprolsvd_em()}

\item{comp_max}{Maximum number of component we want to test}

\item{method}{Controls the method used for interpolation. Thus either "smooth_gauss"
or "splines" is allowed so far.}

\item{niter}{The number of iterations of the algorithm.}

\item{min_keep_frac}{Real between 0 and 1 controlling the % of the
station that has the highest number of NA. This will determine the number of fynthetic gaps}

\item{seed}{Controls the seed of the random index sampling pf the synthetic gaps}
}
\value{
A grid of 2 ggplot representing the error and the cross-validation
error with respect to the number of components retained from the SVD. And a
list containing the following elements:
\describe{
  \item{\code{y.filled}}{}
  \item{\code{errorByComp}}{}
  \item{\code{CVerrorByComp}}{}
}
}
\description{
It aims to compare

The three tuneable (hyper)parameters are :
\describe{
\item{\code{ncomp}}
\item{\code{nsmo}}
\item{\code{nembed}}
}
}
\details{

}
\examples{
library(ValUSunSSN)
data("data.mat2.fin")
y <- data.mat2.fin

y_obsToNA <- cvFromInterpolsvd(x = y, comp_max = 10,
                              niter = 30, min_keep_frac = 0.2)
}
\author{
Antoine Pissoort, \email{antoine.pissoort@student.uclouvain.be}
}
