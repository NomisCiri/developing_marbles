# Ciranka 2023.
# this script subsets the Marble data and fits different learning models
# written to take command line input about models and agegroups

library(tidyverse)
library(rstan)

bashInput <- commandArgs(trailingOnly=TRUE)#c(1, 8)
####### Subsetting and Model Determination Block
LearningModelPath <- c(
  "./B_Model_Code/social_update_unc_only.stan",
  "./B_Model_Code/social_update_rs_ur.stan",
  "./B_Model_Code/social_update_rs.stan",
  "./B_Model_Code/social_update_ur.stan",
  "./B_Model_Code/social_update_rew_sens.stan",
  "./B_Model_Code/social_update_rew_sens_ur.stan",
  "./B_Model_Code/social_trembl.stan",
  "./B_Model_Code/no_social.stan",
  "./B_Model_Code/social_util_rs.stan"
)

#for (models in 1:2){
models<-as.numeric(bashInput[1])
models<-9
for (agegroups in 1:5){
  
  
  MarbleData <- read.csv("./A_RawData/TidyMarbleNew.csv")
  
  Agegroups <- c("0", "1", "2", "3","4")
  Agegroups[agegroups]
  
  MarbleData <- MarbleData %>% mutate(Agegroup = case_when(
    (age < 13) ~ "0",
    (age >= 13 & age < 16) ~ "1",
    (age >= 16 & age < 19) ~ "2",
    (age >= 19 & age < 22) ~ "3",
    (age >= 22) ~ "4"
  )) 
  
  # recode stuff
  
  MarbleData$Agegroup <- as.factor(MarbleData$Agegroup)
  MarbleData$DFE1DFD0 <- as.factor(MarbleData$DFE1DFD0)
  MarbleData$Social1Ind0 <- as.factor(MarbleData$Social1Ind0)
  MarbleData$PercentBlueEstimate <- round(as.numeric(MarbleData$PercentBlueEstimate))
  MarbleData$PercentBlueShownRel <- round(as.numeric(MarbleData$PercentBlueShownRel), 2)
  MarbleData$PercentBlueShownRel <- as.factor(as.character(MarbleData$PercentBlueShownRel))
  MarbleData$HowSure <- as.numeric(MarbleData$HowSure)
  
  # those subs break something... maybe not anymore
  MarbleData <- MarbleData %>%group_by(subject)%>%
    mutate(max_t=n())%>%
    ungroup()%>%
    filter(max_t==144)%>%
    filter(Agegroup == Agegroups[agegroups]) #%>% # &subject!=20) subject!=153 & subject!=158
  #filter(subject != 20 & subject != 153 & subject != 158 & subject != 85 & subject != 101) # Bin_Update_Data<-Bin_Update_Data[Bin_Update_Data$typeRA=="1",]# keep only The Risk trails.
  
  Bin_Update_Data <- MarbleData %>% arrange(Agegroup) # i order it first so i can make sure that
  # make it fit with my stan file.
  Bin_Update_Data <- Bin_Update_Data %>% dplyr::mutate(
    OtherChoseRisk = case_when(
      OtherChoseRisk == "NULL" ~ 1, # i dont need this but i use it as index in stan so it needs a value
      OtherChoseRisk == "1" ~ 2, # risky choices are coded as 2 in my stan code
      OtherChoseRisk == "0" ~ 1, # safe choices are coded as 1 in my stan code
      TRUE ~ 0 # keep the rest.
    )
  ) # end PeerChoice.
  
  
  # now check how many participants we have.
  Bin_Update_Data$subject <- as.numeric(Bin_Update_Data$subject)
  # change colname of subject into subjID
  numSubjs <- length(unique(Bin_Update_Data$subject)) # Total Number of Subs
  subjList <- unique(Bin_Update_Data$subject) ######
  Sequence_Length <- unique(Bin_Update_Data[Bin_Update_Data$TotalNShown < 99, ]$TotalNShown)
  Sequence_Length <- 9
  ####### number of trials and see which group he is in for each subject######
  Tsubj <- as.vector(rep(0, numSubjs))
  
  for (sIdx in 1:numSubjs) {
    curSubj <- subjList[sIdx]
    Tsubj[sIdx] <- length(Bin_Update_Data[Bin_Update_Data$subject == curSubj, ]$subject) # How many entries per Subject?
  }
  maxTrials <- max(Tsubj)
  
  p_gamble <- array(0, c(numSubjs, maxTrials))
  choice <- array(0, c(numSubjs, maxTrials))
  social_trial <- array(0, c(numSubjs, maxTrials))
  risky_payoff <- array(0, c(numSubjs, maxTrials))
  
  social_info_risk2_safe1 <- array(1, c(numSubjs, maxTrials)) # just make this ones
  social_info_risky <- array(0, c(numSubjs, maxTrials))
  social_info_safe <- array(0, c(numSubjs, maxTrials))
  
  risk1_Unc2 <- array(0, c(numSubjs, maxTrials))
  
  # specs for the uncertain trails
  Sucess <- array(0, c(numSubjs, maxTrials, Sequence_Length))
  Fail <- array(0, c(numSubjs, maxTrials, Sequence_Length))
  Blue_Sum_All <- array(0, c(numSubjs, maxTrials))
  Red_Sum_All <- array(0, c(numSubjs, maxTrials))
  # Agegroup<- array(0, c(numSubjs, maxTrials))
  p_gamble_est <- array(0, c(numSubjs, maxTrials))
  
  # generate the data Lists to be passed to stan
  # concatenate different groups in the third dimension.
  gambleList <- unique(MarbleData$probGamble)
  for (i in 1:numSubjs) {
    curSubj <- subjList[i]
    useTrials <- Tsubj[i]
    
    # make dummy variables
    social_trial[i, 1:useTrials] <- as.numeric(Bin_Update_Data[Bin_Update_Data$subject == curSubj, ]$Social