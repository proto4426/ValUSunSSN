# ValUSunSSN
R package for project Val-U-Sun for the consulting course LSTAT2390 at UCL

## R code to install the package from GitHub
1. `devtools::install_github("proto4426/ValUSunSSN")`
2. `library(ValUSunSSN)`

## R code to install the package from a local repository:
1. `install.packages("path-to-ValUSunSSN", repos = NULL, type="source")`
2. `library(ValUSunSSN)`


## First visualisation 

After having loaded the package in your environement, you can already run

`# Be sure to have plotly and ggplot2 already installed`

`runExample('stations') # Comparison of stations and methods by SSN`

`runExample('residuals') # Comparison of stations and methods by residuals`

to have a first idea of the filling method for the SSN done with `interposvd_em()`, method coming from 

*Dudok de Wit,T. (2011), A method for filling gaps in solar irradiance and solar proxy data, Astronomy & Astrophysics, 533*
http://adsabs.harvard.edu/abs/2011A%26A...533A..29D

This app must be improved, and we will also try to add other methods in it. 
