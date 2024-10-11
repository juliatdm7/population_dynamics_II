#Biodiversity Under Pressure - Population Dynamics II Practical 2         11/10/2024


#Matrix Population Model Practical


#Before starting, as we're gonna use popbio package for our matrix population modelling, we first need to install it and load it.

rm(list=ls(all=TRUE))

my_packages <- c('ggplot2', 'popbio')
new_packages <- my_packages[!(my_packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

library(ggplot2)
library(popbio)


