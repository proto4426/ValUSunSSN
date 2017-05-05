library(microbenchmark)
library(profvis)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(reshape2)
library(plotly)
library(parallel)

setwd("/home/piss/PissoortRepo/ValUSunSSN/Data_hidden")
#source('smooth.gaussR_LOAD.R')  # Re-created Gaussian smoother
library(ValUSunSSN)

data.chris <- read.csv("Alldata.csv")

data.chris$Date <- as.Date(paste(data.chris$year,
                                 data.chris$month, data.chris$day,sep = "-"))
data.chris[data.chris == -1] <- NA

data.chris <- data.chris[, colnames(data.chris) %in% c("Ng", "Ns") == F]
# We do it only for SSN here

data.mat <- spread(data.chris, key="station", value="SSN")
data.mat <- data.mat[order(data.mat$decdate),]

data.mat2 <- data.mat[,-(1:5)] # Get matrix of stations in columns only.


#load('/home/piss/PissoortRepo/ValUSunSSN/data/Filled/DataSSN81.RData')


#################### Tests  #######################
###################################################


## First, we discard stations that have coverage <5% for reliability issues
# If keep these values, the results have incorrect values
nNa.sum <- apply(data.mat2, 2, function(x) sum(!is.na(x)))
is_less5_cov <- unname(nNa.sum) < .05 * nrow(data.mat2)
data.mat2.fin <- data.mat2[, !is_less5_cov] # 7 stations are discarded !!

# Take this for input, as advised in the test.m file
y <- sqrt(data.mat2.fin+1) # Select randomly here, for testing

options(mc.cores=parallel::detectCores()) # all available cores


y <- y#[data.mat$decdate>1981 & data.mat$decdate<2015,]  # Here, change year to get the subset you want

# 81 days which captures solar rotation and the evolution of active regions)
# the more adjacent time steps are used in the reconstruction --> stronger smoothing in time.
z <- interpolsvd_em(y, nembed = 2, nsmo = 81, ncomp = 4,
                    niter = 30, displ = F)
# 572 sec for the whole dataset (with some stations discarded)
# V2 : 222 (182 without first loop) sec with modified version and data from 1981.
z <- z$y.filled
z_final60 = z*z - 1
z_final60[z_final60<0] <- 0




#==============================================================================
save(z_final60, file = "dataSSN60.RData")

#save(list = c('data.mat', 'data.mat2.fin', 'z_final'), file = "dataSSN_all.data" )
save(data.mat, file = 'data.mat.RData')
save(data.mat2.fin, file = 'data.mat2.fin.RData')
SSN_filled_all <- z_final
save(SSN_filled_all, file = 'SSN_filled_all.RData')


silsoSSN <- read.csv("SSN_silso_daily.csv", sep=";" )
silsoSSN <- silsoSSN[silsoSSN[,4]>1924 & silsoSSN[,4]<2015.26,]
silsoSSN <- silsoSSN[,-c(1, 2, 3, 6, 7, 8)]
colnames(silsoSSN) <- c("decdate", 'SSN')
save(silsoSSN, file = 'silsoSSN.RData')

ssn_chris <- read.csv('SSNfilledChris.csv')

save(ssn_chris, file = 'ssn_chris.RData')

colnames(data.mat2.fin)
load("dataSSN81.RData")

z_chris <- cbind.data.frame(z_final60,
            Date = data.mat[data.mat$decdate>1960 & data.mat$decdate<2015,]$Date)
colnames(z_chris) <- c( colnames(data.mat2.fin), "Date")

str(ssn_chris)

#======================================================================


# zssn <- interpolsvd_em(data.mat2.fin, nembed = 2, nsmo = 81, ncomp = 4,
#                        niter = 30, displ = F)
# zssn <- zssn$y.filled
## And with Splines smoothing ?
z_splines <- interpol_splines(y, nembed = 2, nsmo = 8, ncomp = 4,
                            niter = 30, displ = F)
# 80 sec for the whole dataset
z_splines <- z_splines$y.filled
z_splines = z_splines*z_splines - 1
z_splines[z_splines<0] <- 0
ssn_splines <- z_splines
save(ssn_splines, file = "ssn_plines.RData")

zs <- interpolsvd_em(y, nembed = 2, nsmo = 8, ncomp = 4,
                     niter = 5, displ = T, method.smooth = zoo::na.spline )$y.filled
# For the whole series ??
z.sp <- interpolsvd_em(y, nembed = 2, nsmo = 8, ncomp = 4,
                       niter = 5, displ = T)
# With 5 iterations (..), it is possible in "only" 43 sec !!!

microbenchmark(interpolsvd_em(y, nembed = 2, nsmo = 8, ncomp = 4, niter = 2, displ = T ),
               interpolsvd_em(y, nembed = 2, nsmo = 8, ncomp = 4,
                              niter = 2, displ = T, method.smooth = zoo::na.spline ),
               times = 10L)
# nearly 20 times faster ! Error behaviour seem well too




#### Visualize some results  : Comparisons with raw data

diff.proc <- y - z

# Create data.frame
y <- data.mat2.fin  ;  z <- SSN_filled_all

### Create the  Plot To Put in the Project ....

rownames(y) <- rownames(z) <- 1:nrow(y)
y1 <- cbind(as.data.frame(y), Date = data.mat$Date, x = "raw")
z1 <- cbind(as.data.frame(z), Date = data.mat$Date, x = "processed")
colnames(z1) <-  colnames(y1)
zz <- rbind(y1,z1)

zbis <- cbind(y1, UC2_proc = z1$wnUC2)

cols <- c("Raw" = 'red', "Filled" = 'blue')

ggplot(zbis) + geom_point(aes(x = Date, y = UC2_proc, col = "Filled"), size = .13) +
  geom_point(aes(x = Date, y = wnUC2, col = "Raw"), size = .11) +
  labs(y = 'wnUC2', title = ' interpolsvd_em() Filling Method for Uccle2') +
  solar.cycle(col = "#33666C") +
  annotate(geom = "text", label = "Solar Cycles",
           x = as.Date("1933-05-01"),
           y = 700, col = "#33666C" , size = 5, fontface = "italic") +
  geom_segment(aes(x = as.Date("1933-01-01"), xend = as.Date("1933-11-01"), y = 720, yend = 795),
               arrow = arrow(length = unit(0.5, "cm")), col = "#33666C") +
  scale_colour_manual(name = "Data", values = cols) +
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  theme_piss(legend.position = c(.938, .752))




ggplot(zz, aes( x = Date, y = wnUC2, col = x)) + geom_line()




colnames(zz) <- c(colnames(data.mat2.fin), "Date", "x")
'ggstatio.comp' <- function(datag, V, plotly = F ) {
  g <- ggplot(datag, aes_string(x = 'Date', y = V, col = 'x')) + geom_point( size = .4) +
    theme(plot.title = element_text(size=15, hjust=0.5,colour = "#33666C",
                                    face="bold"),
          axis.title = element_text(face = "bold", size=20,
                                    colour = "#33666C"),
          legend.title = element_text(colour="#33666C",
                                      size=12, face="bold")) +
    scale_colour_brewer(name=expression(underline(bold("data: "))),
                        palette = "Set1") +
    guides(colour = guide_legend(override.aes = list(size= 2.5))) +
    theme_bw()
  if (plotly) ggplotly(g)
  else g
}

for (i in 1:(ncol(zz)-2)){
  g <- ggstatio.comp(zz, colnames(zz)[i], plotly=F )
  assign(paste0("g", i), g)
}
grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2)
subplot(g1, hide_legend(g2), hide_legend(g3), hide_legend(g4),
        hide_legend(g5), shareX = T, shareY= F, which_layout = 1, nrows = 3)
# quite less volatility, more constant pattern, .... after processing
# discrepancies can occur but must retain our attention !


g <- ggplot(datag) + geom_line(aes( x = Date, y = ) size = .4) +
  theme(plot.title = element_text(size=15, hjust=0.5,colour = "#33666C",
                                  face="bold"),
        axis.title = element_text(face = "bold", size=20,
                                  colour = "#33666C"),
        legend.title = element_text(colour="#33666C",
                                    size=12, face="bold")) +
  scale_colour_brewer(name=expression(underline(bold("data: "))),
                      palette = "Set1") +
  guides(colour = guide_legend(override.aes = list(size= 2.5))) +
  theme_bw()


# z1 <- cbind(as.data.frame(z), Date = data.mat$Date )
# colnames(z1) <- c(colnames(data.mat2.fin), "Date")
# zz.full <- melt(z1, id.vars = "Date" )
# dimnames(zz.full)[[2]][2] <- "Station"
# dimnames(zz.full)[[2]][3] <- "SSN"
#
# g <- ggplot(zz.full) + geom_line(aes( x = Date, y = SSN, col = Station), size = .4) +
#   scale_color_hue(l = 40, c = 200) +
#   theme_piss()
# # scale_colour_brewer(name=expression(underline(bold("data: "))),
# #                     palette = "Set1") +
# #guides(colour = guide_legend(override.aes = list(size= 2.5)))
#
#
# ggplotly(g)




rownames(y) <- rownames(zssn) <- 1:nrow(y)
#y1 <- cbind(as.data.frame(data.mat2.fin), Date = data.mat[data.mat$decdate>1981,]$Date, x = "Raw")
y1 <- cbind(as.data.frame(data.mat2.fin), Date = data.mat$Date, x = "Raw")
#z1 <- cbind(as.data.frame(zssn), Date = data.mat[data.mat$decdate>1981,]$Date, x = "Filled")
colnames(y1) <- colnames(z1) <- c(colnames(data.mat2.fin), "Date", "x")

zz <- rbind(y1,z1)

g <- ggplot(zz, aes(x = Date, y =  wnA3, col = x ) ) + geom_point( size = .4) + theme_piss() +
  scale_colour_brewer(name="data : ",
                      palette = "Set1")
#ggplotly(g,)





##### Compariong complete raw dataset with the splines smoothing method :

diff.proc <- y - z
#stats::heatmap(as.matrix(diff.proc))   # Can be computationnally huge.
# But nice to visualize where are the differences

# GGPLOTs
colnames(z.sp) <- colnames(data.mat2)
tot.splinesDF <- cbind( rbind.data.frame(data.mat2, z.sp),
                        x = c(rep("raw", nrow(data.mat2)), rep("splines", nrow(z.sp))),
                        Date = data.mat$Date)
for (i in 1:(ncol(tot.splinesDF)-2)){
  g <- ggstatio.comp(tot.splinesDF, 'colnames(tot.splinesDF)[i]' )
  assign(paste0("g", i), g)
}
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 2)




#### Comparisons of the results between Gaussian and splines smoothing
zs <- ssn_splines  ;   z <- SSN_filled_all

mat.diff <- z - zs
stats::heatmap(mat.diff, Rowv = F, Colv = F)

## Plots
# Create data.frame for ggplot inputs
colnames(z) <- colnames(zs) <- colnames(data.mat2.fin)
z.d <- cbind.data.frame(z, met = "gauss")
zs.d <-  cbind.data.frame(zs, met = "splines")
z.df <- rbind(z.d, zs.d)
z.df$t <- rep(1:nrow(z.d), 2)

gg.compsmoot <- function (station){
  ggplot(z.df, aes_string(x = 't', y = station, col = 'met')) + geom_point(size = .2)
}
library(ggplot2)
for (i in 1:(ncol(z.df)-2)) {
  g <- gg.compsmoot( colnames(z.df)[i] )
  assign(paste0("g", i), g)
}

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 2)
# points are quite similars.... !!!




#====================================================
#################### Compare with Matlab from Caro

load('/home/piss/PissoortRepo/ValUSunSSN/data/Filled/DataSSN_OLD.RData')   # Take full dataset
filled_caro<-read.csv('/home/piss/PissoortRepo/ValUSunSSN/data/filledMATLAB_caro.csv',
                      header = F)
colnamescsv <-read.csv('/home/piss/PissoortRepo/ValUSunSSN/data/colnames.csv',
                       header = F)

colnames(filled_caro) <- colnamescsv[,2]
filled_caro <- data.mat2.fin[, colnames(filled_caro)]

zssn <- z_final
colnames(zssn) <-  colnames(data.mat2.fin)
# data.mat2.fin is the original dataset (from alldata.csv) with 52 stations

diffwith_caro <- as.matrix(zssn - filled_caro) # case-by-case differneces between
# my filled data (zssn) with R and this from matlab
sum((diffwith_caro)^2, na.rm = T)

#stats::heatmap(diff_caro, Rowv = F, Colv = F)

## Check if the cases without NA are the same
filled_caro[is.na(data.mat2.fin)] <- NA
sum( (as.matrix(filled_caro) - as.matrix(data.mat2.fin))^2, na.rm = T)
## Lots of differences between the observed values..... ?
#sum(filled_caro == data.mat2.fin, na.rm = T)


sum(is.na(filled_caro))
sum(is.na(data.mat2.fin))
# is.na(filled_caro) == is.na(data.mat2.fin)

## Visualize residuals, averaged over all stations
df_res_caro <- cbind.data.frame(diffwith_caro,
                                mean_squaredres = apply(diffwith_caro^2, 1, mean))

myPalette <- colorRampPalette(rev(brewer.pal(4, "Spectral")))
df_res_caro <- cbind(df_res_caro, time = data.mat$decdate)

ggplot(df_res_caro, aes(x = time, y = mean_squaredres, col = mean_squaredres)) +
  geom_point( size = 0.35) + theme_piss() + solar.cycle() +
  scale_colour_gradientn(colours = myPalette(10), limits=c(-150, 250))



df_uccle_chris <- data.frame(time = filled.chris2$times,
                             wnUC2 = filled.chris2$wnUC2_d.txt, from = "Chris")
df_uccle_c <- rbind.data.frame(df_uccle_chris,
                               data.frame(time = filled.chris2$times,
                                          wnUC2 = zssn$wnUC2[1:nrow(df_uccle_chris)],
                                          from = "`interpolEM`"))






#####  Time comparisons #################
#########################################
# (see Caro's Python code)

# Count number of avaliable observations by stations
id.notNA <- apply(data.mat2.fin[,-ncol(data.mat2.fin)], 2,
                  function(x) which(!is.na(x)))
obs.notNA <- unlist(lapply(id.notNA, FUN = length))

# Select stations with enough observations (test)
selected_cols <- unname(which(obs.notNA > 17e3))
selected_cols <- selected_cols[-1]
colnames(data.mat2.fin)[selected_cols]

tmp <- data.mat2.fin[1e4:nrow(data.mat2.fin), selected_cols]
timeTrial <- c()
current <- 8000
for(i in 1:27){
  currentData <- tmp[1:current,]
  t <- proc.time()
  x <- sqrt(currentData+1)
  # z <- interpolsvd_em(y, nembed = 2, nsmo = 81, ncomp = 4,
  #                     niter = 30, displ = F)
  z <- interpol_CrossVal(sqrt(x+1), nembed = 2, nsmo = 81,
                            ncomp = 4, niter = 30)
  timeTrial[i] = (proc.time()-t)[3]
  current = current + 200
}

time_pyt <- read.table("C:/Users/Piss/Documents/LINUX/PissoortRepo/ValUSunSSN/Data_hiddenpythonTimeComplexity.txt")
time_pyt <- t(time_pyt)
time_pyt <- rev(time_pyt)
time_pyt <- time_pyt[4:30]  # Error in allocation in python


plot(1:27, timeTrial, type = "l")
lines(1:27, time_pyt[4:30], type = "l")

index <- seq(18000, 23200, by = 200)

df <- data.frame(index = index, time_py = time_pyt, time_R = timeTrial)

cols <- c("Python" = "red", "R" = "blue")

ggplot(df) + geom_line(aes(x = index, y = time_py, col = "Python")) +
  geom_line(aes(x = index, y = time_R, col = "R")) +
  geom_point(aes(x = index, y = time_py, col = "Python")) +
  geom_point(aes(x = index, y = time_R, col = "R")) +
  geom_segment(aes(x = 22400, xend = 23450,
                   y = 12 , yend = 13),
                col = "black", size = 2) +
  geom_segment(aes(x = 22400, xend = 23350,
                   y = 13 , yend = 12),
               col = "black", size = 2) +
  labs(y = "Computation time (seconds) ", x = "Number of observations",
       title = "Language's computation time for interpolsvd_em() ") +
  scale_colour_manual(name = "Language", values = cols) +
  guides(colour = guide_legend(override.aes = list(size = 2))) +
  geom_vline(xintercept = 22400, linetype = "dashed", col = "blue") +
  theme_piss(legend.position = c(.938, .132))





###############################################################

## Splines : 2nd attempt
z_spline <- interpolsvd_em2(data.mat2.fin[data.mat$decdate>= 1981,], nembed = 2, nsmo = 8, ncomp = 4,
                            niter = 15, displ = T, method.smooth = zoo::na.spline )

z_spline <- z_spline$y.filled

z_ssn1 <- interpolsvd_em2(data.mat2.fin[data.mat$decdate>= 1981,], nembed = 2, nsmo = 8, ncomp = 4,
                          niter = 15, displ = T, method.smooth = smooth.gauss, param.smooth = nsmo )






####### Write files

load('/home/piss/PissoortRepo/ValUSunSSN/Data/DataSSN.RData')
rownames(zssn) <- data.mat$decdate
zssn <- as.data.frame(zssn)
colnames(zssn) <- colnames(data.mat2.fin)
write.csv(zssn, "SSNfilled_interpolsvd_full_R.csv")

load('/home/piss/PissoortRepo/ValUSunSSN/Data/DataSSN60.RData')
rownames(zssn) <- data.mat[data.mat$decdate>1960,]$decdate
zssn <- as.data.frame(zssn)
colnames(zssn) <- colnames(data.mat2.fin)
write.csv(zssn, "SSNfilled_interpolsvd_>60_R.csv")

load('/home/piss/PissoortRepo/ValUSunSSN/Data/DataSSN81.RData')
rownames(zssn) <- data.mat[data.mat$decdate>1981,]$decdate
zssn <- as.data.frame(zssn)
colnames(zssn) <- colnames(data.mat2.fin)
write.csv(zssn, "SSNfilled_interpolsvd_>81_R.csv")



##### Try to implement the computationally heavy methods (e.g., smoothing) with cpp
library(Rcpp)
