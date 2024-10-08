#Biodiversity Under Pressure - Population Dynamics II Practical           8/10/2024


#Capture-Mark-Recapture Tryout
n1 <- 20
n2 <- 10
R <- 6
P <- (n1 * n2)/R
P_est <- c(13.33333, 23.33333, 16.66667, 16.66667, 10, 16.66667, 16.66667, 13.33333, 20, 10)
mean_P <-mean(P_est)

#This is used frequently to estimate survival rates. 


#Capture-Mark-Recapture practical

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

#1. Generating capture histories

longdata <- read.table("data/sparrowrecap.txt", header = TRUE, sep = '\t')

head(longdata)

dim(longdata) #2378 capture observations across different islands in the house sparrow meta-population. 
#Data is in long format: each individual appears as many times as it was captured and where/when it was captured each time

length(unique(longdata$id)) #number of unique individuals in the long data format.

table(longdata$sex) #observations in relation to sex. Same number of males and females.

table(longdata$year) #captures from 1998-2007

table(longdata$island) #at 4 different island locations
#We need to convert this data from long format to wide format so that the package "mark" can analyse it.

temp <- longdata[,1:2] #take the first two columns, id and year and put into a temporary dataframe

temp$detect <- 1 #add column for detection (all 1s because these represent captures)

temp <- temp %>%
  # remove duplicates, which may occur when individuals are caught multiple times in an sampling event
  distinct() %>%
  # spread out data. The fill = 0 adds rows for combinations of id and year where individuals were not observed
  spread(year, detect, fill = 0) %>% 
  # for every individual....
  group_by(id) %>%
  # paste together 0's and 1's using unite()
  # here we are pasting the strings together from the second column (first capture event)
  # to the last capture event ("tail(names(.),1)")
  # use sep="" so there are no characters separating 0's and 1's
  unite("ch", 2:tail(names(.),1), sep = "")

sparrow <- as.data.frame(temp) # new dataframe called sparrow

head(sparrow)

sparrow$island <- longdata$island[match(sparrow$id, longdata$id)] 
# this creates a new column called island in the sparrow df...
# using the entry from the island column in the longdata df... 
# where id in the sparrow df matches the id in the longdata df

sparrow$sex <- as.factor(longdata$sex[match(sparrow$id, longdata$id)])

sparrow <- droplevels(subset(sparrow, select = -id)) # remove id column so capture histories appear in first column

head(sparrow)


#2. 
