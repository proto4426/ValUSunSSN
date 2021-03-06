% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UsedFunc.R
\name{theme_piss}
\alias{theme_piss}
\title{Homogeneous theme for ggplots}
\usage{
theme_piss(size_p = 18, size_c = 14, size_l = 12, theme = theme_bw(),
  ...)
}
\arguments{
\item{size_p}{Size of the plot's title.}

\item{size_c}{Size of the axis' title.}

\item{size_l}{Size of the legend's title.}

\item{theme}{ggplot's theme for the plot.}

\item{...}{enables to add other theme's specifications for the plots, see
\url{http://ggplot2.tidyverse.org/reference/theme.html}}
}
\value{
A personalized ggplot2 theme object to add to every builded plots.
}
\description{
Theme function for our builded \code{ggplots}.
Useful get coherent and similar colours, themes,... for all plots.
Also useful to decrease the number of lines of code.
}
\details{
This function is useful to decrease the amount of code for each ggplots
generated as this thesis will use exclusive \code{ggplot2} for the plots.
Values of other parameters such as colours could be changed inside the function.
}
\examples{
df_uccle_vero <- data.frame(time = z1$Date[1:nrow(filledt.vero)], SSN_wnUC2 = filledt.vero$UC2, from = "Vero")

df_uccle_c <- rbind.data.frame(df_uccle_vero, data.frame(time = z1$Date[1:nrow(df_uccle_vero)],
                                                        SSN_wnUC2 = zssn[,"wnUC2"][1:nrow(df_uccle_vero)], from = "`interpolEM`"))

ggplot(df_uccle_c, aes(col = from)) +
  geom_point(aes(x = time, y = SSN_wnUC2), size = 0.15) +
  solar.cycle() +
  guides(colour = guide_legend(override.aes = list(size= 3))) +
  ggtitle("Filled missing values") +
  theme_piss()
}
\author{
Antoine Pissoort, \email{antoine.pissoort@student.uclouvain.be}
}
