###
###
###
###
###
### THIS IS THE RAW VERSION OF MY UTILITY MODEL: 
### WORKS SUPER WELL
###
###
###
###
###
###
###
library(tidyverse)
library(actuar)
library(rstan)

alpha=2
beta=4

constant=200
betaVals=rbeta(1000,alpha,beta)#*constant


dataList=list(x=betaVals,
              nData=length(betaVals)
              )

genInitList <- function() {
  list(
    alpha=10,
    beta=50,
    end=300# find a reasonable way to regurize this parameter. INITIAL VALUE ALWAYS NEEDS TO BE HIGHER THAN ANY VALUE FOR X!!!!!!!!
  )
}

  GenBeta=stan("B_Model_Code/Beta_Wouter.stan",
     data=dataList,
     pars=c("alpha","beta","end","y_pred"),
     init=genInitList,
     chains=3,
     cores=3
    )
