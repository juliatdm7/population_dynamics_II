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

#Let's project our population over 15 years
t <- 15
#We'll start with 50 juveniles, 20 yearlings and 30 adults
n0 <- c(50,20,30)

##a)Projected dynamics 
    projection <- pop.projection(sparrowMPM, n0, iterations = t)
    projected <- data.frame(time=1:15, N=projection$pop.sizes)

    #Plot projected pop size over time
    ggplot(projected, aes(time, N)) + 
      geom_line() + ylim(0,1500) + ylab('Projected N')

##b)Observed dynamics
    #We can now compare our model to estimated values:
    popest <- read.table("data/popest.txt", header = TRUE, sep = '\t')
    head(popest)
    #Let's plot N over time
    ggplot(popest, aes(year, N)) + 
      geom_line() + ylim(0,200) + ylab('Observed N')
    #How does this population trajectory compare with our estimate of lambda?
    #It grows, but it's not on constant growth and it's also not exponential growth, so it's definitely different.
    
##c)Stable stage distribution and reproductive value
    stages <- c('Juv','Yr','Ad')
    colnames(sparrowMPM) <- stages
    rownames(sparrowMPM) <- stages
    
    stable.stage(sparrowMPM) #Here, we're using the "popbio" to look at the the stable stage distribution of our population, that is the long-term average relative abundance of the different stage classes.
    
    reproductive.value(sparrowMPM) #Here, we're looking at the reproductive values of the different stage classes, that is the expected contribution of each individual in that stage class to future reproduction.
    
    #We could potentially compare the stable stage distribution with what we observed based on the recapture data to tell us something about the performance of our model, but we won’t do this today.
    
##d)Perturbation analysis
    #We can also use popbio to calculate sensitivities and elasticities of the different vital rates. 
    #These tell us about the relative importance of each vital rate (or matrix transition) in determining the population growth rate, lambda.
    #Sensitivities estimate the change in lambda for an absolute change in a vital rate.
    #Elasticities tell us about the effect of a proportional change.
    #Let's list the vital rates
    sparrow.param <- list(Phi.juv = S_j, Phi.yr = S_y, Phi.ad = S_a, R = R)
    
    #Give the matrix equation 
    sparrow.equation <- expression(Phi.juv * R, Phi.yr * R, Phi.ad * R, Phi.juv, 0, 0, 0, Phi.yr, Phi.ad)
    
    #Run the sensitivity analysis
    sens <- vitalsens(sparrow.equation, sparrow.param)
    sens
    
    #Plot elasticity of the vital rates 
    sens$vitalrate <- factor(c('Phi.juv', 'Phi.yr', 'Phi.ad', 'R'), levels = c('Phi.juv', 'Phi.yr', 'Phi.ad', 'R'))
    ggplot(sens, aes(vitalrate, elasticity)) + 
      geom_bar(stat = 'identity') 
    
    #Which vital rates are most important for population growth? Is this similar to the orca example that we saw in the lecture? Is this what you would expect based on the life-history of the species?
    #Juvenile Survival and Reproduction Rates are the most important rates for population growth, which differs from the orca example we saw in the lecture in which it was Adult Survival Rate that mattered the most.
    #These values are expected as Passerines tend to reach maturity the first year after they're born (in comparison to orcas, which reach sexual maturity between 7-16 years old (and they reach adulthood between 10 and 13 years old). 
    #Also, in each reproductive event, orcas have one offspring and don't reproduce until around five years later, while some passerines can go through more than one reproductive event per year and lay (way) more than one egg.
    #Passerines also tend to have shorter lives in comparison, so, putting all of this together, it does make sense that while Adult Survival is more important for population growth in orcas (they live longer and each reproductive event results in few offspring), for passerine species juvenile survival and reproduction rate are more importan (juvenile survival is a limiting factor, and although for not as long, each individual produces much more offspring per reproductive event).