#this is a ugly resudual from 4 years ago that works but i hope to never see it again. 
make_data_list_recov<-function(MarbleData,agegroups){
  ##
  ##This is used to fit the model on binomial draws taken from the posterior predictives 
  ##of one specific model
  ##
  ##
  Agegroups <- c("0", "1", "2", "3","4")
  # Agegroups[agegroups]
  
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
  MarbleData <- MarbleData %>%group_by(participant)%>%
    mutate(max_t=n())%>%
    ungroup()%>%
    filter(max_t==144)%>%
    filter(Agegroup == Agegroups[agegroups]) #%>% # &participant!=20) participant!=153 & participant!=158
  #filter(participant != 20 & participant != 153 & participant != 158 & participant != 85 & participant != 101) # Bin_Update_Data<-Bin_Update_Data[Bin_Update_Data$typeRA=="1",]# keep only The Risk trails.
  
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
  Bin_Update_Data$participant <- as.numeric(Bin_Update_Data$participant)
  # change colname of participant into subjID
  numSubjs <- length(unique(Bin_Update_Data$participant)) # Total Number of Subs
  subjList <- unique(Bin_Update_Data$participant) ######
  Sequence_Length <- unique(Bin_Update_Data[Bin_Update_Data$TotalNShown < 99, ]$TotalNShown)
  Sequence_Length <- 9
  ####### number of trials and see which group he is in for each participant######
  Tsubj <- as.vector(rep(0, numSubjs))
  
  for (sIdx in 1:numSubjs) {
    curSubj <- subjList[sIdx]
    Tsubj[sIdx] <- length(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$participant) # How many entries per participant?
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
    social_trial[i, 1:useTrials] <- as.numeric(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$Social1Ind0) - 1
    social_info_risky[i, 1:useTrials] <- ifelse(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$OtherChoseRisk - 1, 1, 0)
    social_info_safe[i, 1:useTrials] <- ifelse(
      ifelse(as.numeric(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$OtherChoseRisk) == 1, 1, 0) & #true when safe was chosen
        as.numeric(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$Social1Ind0) - 1, 1, 0# true in social trials
    )
    
    # make indices
    risk1_Unc2[i, 1:useTrials] <- as.numeric(as.character(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$DFE1DFD0)) + 1
    social_info_risk2_safe1[i, 1:useTrials] <- Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$OtherChoseRisk
    
    # SIMULATE CHOICES FROM pp probabilites
    choice[i, 1:useTrials] <- as.numeric(rbernoulli(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$ChooseRisk))
    
    # experiment data
    p_gamble[i, 1:useTrials] <- Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$probGamble
    risky_payoff[i, 1:useTrials] <- Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$valueGamble
    p_gamble_est[i, 1:useTrials] <- as.double(as.character(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$PercentBlueEstimate)) / 100
    p_gamble_est[i, which(risk1_Unc2[i, 1:useTrials] == 1)] <- p_gamble[i, which(risk1_Unc2[i, 1:useTrials] == 1)] # put in correct ps for risk
    p_gamble_est[p_gamble_est > 0.99] <- 0.99
    p_gamble_est[is.na(p_gamble_est)] <- 0.5
  }
  p_gambleIndex <- p_gamble
  for (i in 1:length(gambleList)) {
    p_gambleIndex[p_gambleIndex == gambleList[i]] <- i
  }
  
  gambleList <- unique(MarbleData$probGamble)
  #print(gambleList)
  nGambles <- length(gambleList)
  
  #collect data in list for stan
  dataList <- list(
    N = numSubjs,
    T = maxTrials,
    t_subj = Tsubj,
    risky_payoff = risky_payoff,
    p_gamble_est = p_gamble_est,
    gambles_list = gambleList,
    n_gambles = length(gambleList),
    choice = choice,
    #dummy variables
    risk1_Unc2 = risk1_Unc2,
    social_info_risk2_safe1 = social_info_risk2_safe1, # social_info_risk2_safe1 is 0= solo ,1 = safe  ,3 = risky
    social_info_risky=social_info_risky,
    social_info_safe=social_info_safe,
    social_trial=social_trial
  )
  return(dataList)
}






#this is a ugly resudual from 4 years ago that works but i hope to never see it again. 
make_data_list<-function(MarbleData,agegroups){
  ####
  ####
  ###This is used for fitting the model on real choice data
  ###
  Agegroups <- c("0", "1", "2", "3","4")
  # Agegroups[agegroups]
  
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
  MarbleData <- MarbleData %>%group_by(participant)%>%
    mutate(max_t=n())%>%
    ungroup()%>%
    filter(max_t==144)%>%
    filter(Agegroup == Agegroups[agegroups]) #%>% # &participant!=20) participant!=153 & participant!=158
  #filter(participant != 20 & participant != 153 & participant != 158 & participant != 85 & participant != 101) # Bin_Update_Data<-Bin_Update_Data[Bin_Update_Data$typeRA=="1",]# keep only The Risk trails.
  
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
  Bin_Update_Data$participant <- as.numeric(Bin_Update_Data$participant)
  # change colname of participant into subjID
  numSubjs <- length(unique(Bin_Update_Data$participant)) # Total Number of Subs
  subjList <- unique(Bin_Update_Data$participant) ######
  Sequence_Length <- unique(Bin_Update_Data[Bin_Update_Data$TotalNShown < 99, ]$TotalNShown)
  Sequence_Length <- 9
  ####### number of trials and see which group he is in for each participant######
  Tsubj <- as.vector(rep(0, numSubjs))
  
  for (sIdx in 1:numSubjs) {
    curSubj <- subjList[sIdx]
    Tsubj[sIdx] <- length(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$participant) # How many entries per participant?
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
    social_trial[i, 1:useTrials] <- as.numeric(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$Social1Ind0) - 1
    social_info_risky[i, 1:useTrials] <- ifelse(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$OtherChoseRisk - 1, 1, 0)
    social_info_safe[i, 1:useTrials] <- ifelse(
      ifelse(as.numeric(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$OtherChoseRisk) == 1, 1, 0) & #true when safe was chosen
        as.numeric(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$Social1Ind0) - 1, 1, 0# true in social trials
    )
    
    # make indices
    risk1_Unc2[i, 1:useTrials] <- as.numeric(as.character(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$DFE1DFD0)) + 1
    social_info_risk2_safe1[i, 1:useTrials] <- Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$OtherChoseRisk
    
    # SIMULATE CHOICES FROM pp probabilites
    choice[i, 1:useTrials] <- Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$ChooseRisk
    
    # experiment data
    p_gamble[i, 1:useTrials] <- Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$probGamble
    risky_payoff[i, 1:useTrials] <- Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$valueGamble
    p_gamble_est[i, 1:useTrials] <- as.double(as.character(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$PercentBlueEstimate)) / 100
    p_gamble_est[i, which(risk1_Unc2[i, 1:useTrials] == 1)] <- p_gamble[i, which(risk1_Unc2[i, 1:useTrials] == 1)] # put in correct ps for risk
    p_gamble_est[p_gamble_est > 0.99] <- 0.99
    p_gamble_est[is.na(p_gamble_est)] <- 0.5
  }
  p_gambleIndex <- p_gamble
  for (i in 1:length(gambleList)) {
    p_gambleIndex[p_gambleIndex == gambleList[i]] <- i
  }
  
  gambleList <- unique(MarbleData$probGamble)
  #print(gambleList)
  nGambles <- length(gambleList)
  
  #collect data in list for stan
  dataList <- list(
    N = numSubjs,
    T = maxTrials,
    t_subj = Tsubj,
    risky_payoff = risky_payoff,
    p_gamble_est = p_gamble_est,
    gambles_list = gambleList,
    n_gambles = length(gambleList),
    choice = choice,
    #dummy variables
    risk1_Unc2 = risk1_Unc2,
    social_info_risk2_safe1 = social_info_risk2_safe1, # social_info_risk2_safe1 is 0= solo ,1 = safe  ,3 = risky
    social_info_risky=social_info_risky,
    social_info_safe=social_info_safe,
    social_trial=social_trial
  )
  return(dataList)
}
