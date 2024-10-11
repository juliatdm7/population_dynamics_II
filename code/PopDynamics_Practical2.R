#Biodiversity Under Pressure - Population Dynamics II Practical 2         11/10/2024


#Matrix Population Model Practical


#Before starting, as we're gonna use popbio package for our matrix population modelling, we first need to install it and load it.

rm(list=ls(all=TRUE))

my_packages <- c('ggplot2', 'popbio')
new_packages <- my_packages[!(my_packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)

library(ggplot2)
library(popbio)


#1. Parameterising your Matrix Population Model (MPM)

##a) Stage-specific survival
##Survival rates where estimated using the data from the previous Practical using a Cormack–Jolly–Seber model in package "marked"
##Time-varying detection probabilities were used when estimating the stage-specific survival rates, which were as follows:
  S_j <- 0.463 #Juvenile survival from t --> t+1 (95% CI 0.404–0.524)
  S_y <- 0.510 #Yearling survival from t --> t+1 (95% CI 0.445–0.574)
  S_a <- 0.559 #Adult survival from t --> t+1 (95% CI 0.499–0.618)
  
  #First, let's enter the values into a dataframe 
  survival <- data.frame(Stage=factor(c('Juvenile','Yearling','Adult'), levels=c('Juvenile','Yearling','Adult')), Estimate=c(S_j, S_y, S_a), lcl=c(0.404, 0.445, 0.499), ucl=c(0.524, 0.574, 0.618)) #We also include the lower and higher intervals of our Confidence Intervals
  
  #Then, let's plot by stage
  ggplot(survival, aes(Stage, Estimate, ymin=lcl, ymax=ucl)) + 
    geom_errorbar(width=0.2) + geom_point() + ylim(0,1)
  
  #QUESTION: Which stage has the lowest survival rate? Is this what you would expect?
  #Juveniles have the lowest survival rate. Juveniles are very vulnerable to predation, nest abandonement, environmental adversities... and so it's expectable.
  
##b) Per capita reproduction
##As we're only going to consider the female segment of our population, we need to estimate only the number of female offspring produced by each female between censuses.
##This information can be gathered from the nest visitation data for Gjeroy for 1998-2012:
  nestdata <- read.table("data/gjeroynest.txt", header = TRUE, sep = '\t') 
  head(nestdata) #clutchno indicates whether it was the first, second or third etc. clutch laid in that nest in that breeding season. hatchingsuc indicates whether any live chicks were found for that clutch (yes = 1; no = 0). chickno indicates the number of chicks counted on the final visit prior to fledging.
  
  #Let's now estimate per capita reproduction
  #We can calculate the average Hatching success as well as well as the average Fledging nr
  HatchingSuc <- mean(nestdata$hatchingsuc)
  FledglingNo <- mean(nestdata$chickno[nestdata$hatchingsuc == 1])
  
  #For the number of clutches, we first create a new dataframe called "nests" which has one row for each unique nest. 
  #We add a column called numberofclutches which takes the maximum value of clutchno for each unique value of nestid. We then take the mean of these values to be the average number of clutches.
  nests <- data.frame(nestid = sort(unique(nestdata$nestid)), numberofclutches=tapply(nestdata$clutchno, nestdata$nestid, max))  
  ClutchNo <- mean(nests$numberofclutches)
  
  #We can use these as estimates of Clutch Number, Hatching Success and Fledgling Number to calculate per capita reproduction.
  ##(Note! The mean number of chicks prior to fledging is an upwardly biased estimate of the number of fledglings, since not all will likely fledge the nest successfully).
  
  R <- (ClutchNo * HatchingSuc * FledglingNo) / 2 #We multiply the average number of clutches by the probability of hatching success by the average number of fledlings (when hatching is successful). We then divide by two as we're focusing only on the female section of our population and that section is around half of our population
  #Our estimated per capita reproduction rate is 1.590824
  
#2. Deterministic Population Background
  
#Now that we have figured out our transition rates, we can build our Matrix Population Model
#We need a 3x3 matrix with the fertility transitions along the top row, and the survival transitions on the subsequent rows.

#Relation between our estimates of survival rates and the transition probabilities of the matrix:

  # Juvenile to Juvenile: S_j * R
  # Yearling to Juvenile: S_y * R
  # Adult to Juvenile: S_a * R
  # Juvenile to Yearling: S_j
  # Yearling to Yearling: 0 
  # Adult to Yearling: 0 
  # Juvenile to Adult: 0
  # Yearling to Adult: S_y
  # Adult to Adult: S_a
  
#Let's put the transition probabilities into a vector 
sparrowMPM <- c(S_j * R, S_y * R, S_a * R, S_j, 0, 0, 0, S_y, S_a)
names <- c("Juveniles", "Yearlings", "Adults")
names <-list(names, names)

#We can transform this vector into a matrix:
sparrowMPM <- matrix(sparrowMPM, nrow=3, ncol=3, byrow=T, dimnames = names)

#We can now use the popbio package to do some analyses of our deterministic MPM. For example, we can look at the population growth rate, lambda.
lambda(sparrowMPM) #Our lambda is 1.267068.

#What does this lambda tell us about our population? We know that a lambda = 2 implies an exponential growth in our population and that when lambda > 1, the population is experimenting a positive growth. 


