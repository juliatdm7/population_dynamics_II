#Biodiversity Under Pressure - Population Dynamics II Practical           8/10/2024

#Before staring the practical, we need to download some packages if we haven't already
my_packages <- c('dplyr', 'tidyr', 'marked', 'ggplot2', 'R2ucare')
new_packages <- my_packages[!(my_packages %in% installed.packages()
                              [,'Package'])]
if(length(new_packages)) install.packages(new_packages)


library(dplyr)
library(tidyr)
library(marked)
library(ggplot2)
library(R2ucare)
