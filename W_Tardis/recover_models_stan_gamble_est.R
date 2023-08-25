# Ciranka 2023.
# this script subsets the Marble data and fits different learning models
# written to take command line input about models and agegroups

#library(tidyverse)
#library(rstan)
####################################################################
## FUTRUE INIT.
####################################################################

pacman::p_load("listenv","future","future.batchtools","debugme","batchtools","tidyverse","rstan","brms","containerit")
#make dockeriamge
my_dockerfile <- containerit::dockerfile(from = utils::sessionInfo())
containerit::write(dockerfile_object, file = tempfile(fileext = "_dockerfile"))

Sys.setenv(DEBUGME='batchtools')
#fit models on slurm
slurm_template_file = "./model_fitting_functions/future_files/slurm-lido3.tmpl"

#login to cluster
login<-tweak(cluster, workers = "ciranka@tardis.mpib-berlin.mpg.de")
#fill in template file with specs
sbatch <- tweak(batchtools_slurm, template = ".batchtools.slurm.tmpl",
                workers = 4,
                resources = list(job.name = 'test1',
                                 ncpus= 1, 
                                 memory = 1000,
                                 queue="short", 
                                 walltime=120
                )
)

# make future plan
## login node -> { cluster nodes } -> { multiple cores }
plan(list(
  login,#login node
  sbatch,# { cluster nodes }
  multicore#{ multiple cores } 
))

f_libs%<-% .libPaths()
print(f_libs)

#dat%>%filter(id==1, session==1)%>%fit_model()
# parallelize cv over participants.
x %<-% {
  tpid <- Sys.getpid()
  thost <- Sys.info()[["nodename"]]
  
  # transfer my local tmpl file
  y <- listenv()
  for (task in 1:4) {
    ## (b) This will be evaluated on a compute node on the cluster
    y[[task]] %<-% {
      mhost <- Sys.info()[["nodename"]]
      mpid <- Sys.getpid()
    }
    Reduce(rbind, y)
  }
}

x







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
gen_models<-as.numeric(bashInput[1])
gen_models<-1
agegroups <-4# only one age group; takes long enough already.
model_looper<-c(1,3,5,7)

for (models in model_looper){
  
  #read datafile of generative model
  MarbleData <- readRDS("./A_RawData/pp_list.rds")[[gen_models]]
  
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
    choice[i, 1:useTrials] <- as.numeric(rbernoulli(Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$pp))
    
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
  print(gambleList)
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
  
  
  
  #    Stan Magic down here               .
  # 
  #                    .
  #          /^\     .
  #     /\   "V"
  #    /__\   I      O  o
  #   //..\\  I     .
  #   \].`[/  I
  #   /l\/j\  (]    .  O
  #  /. ~~ ,\/I          .
  #  \\L__j^\/I       o
  #   \/--v}  I     o   .
  #   |    |  I   _________
  #   |    |  I c(`       ')o
  #   |    l  I   \.     ,/
  # _/j  L l\_!  _//^---^\\_
  if (models==1 | models==8){
    POI=c("rho","kappas","tau","log_lik","y_pred")
  } else if(models == 7){
    POI=c("rho","kappas","tau","log_lik","y_pred","trembl")
  }else if(models == 9){
    POI=c("rho","psis","tau","log_lik","y_pred")
  }else{
    POI=c("rho","psis","kappas","tau","log_lik","y_pred")
  }
  
  ModelFit3 <- stan(LearningModelPath[models],
                    data = dataList,
                    pars=POI,
                    iter = 2000,
                    warmup = 1000,
                    cores = 4,
                    chains = 4,
                    control = list(
                      adapt_delta = 0.99,
                      max_treedepth = 20
                    )
  )
  
  
  saveRDS(ModelFit3,file = paste0("C_ModelFits/model_recovery/model_est_",models,"_sim_",gen_models,".rds"))
  
}
#}

# 
# 
#                                  _____  _____
#                                 <     `/     |
#                                  >          (
#                                 |   _     _  |
#                                 |  |_) | |_) |
#                                 |  | \ | |   |
#                                 |            |
#                  ______.______%_|            |__________  _____
#                _/                                       \|     |
#               |                    OLD CODE     <
#               |_____.-._________              ____/|___________|
#                                 | * 20/10/18 |
#                                 | + 15/06/23 |
#                                 |            |
#                                 |            |
#                                 |   _        <
#                                 |__/         |
#                                  / `--.      |
#                                %|            |%
#                            |/.%%|          -< @%%%
#                            `\%`@|     v      |@@%@%%    - mfj
#                          .%%%@@@|%    |    % @@@%%@%%%%
#                     _.%%%%%%@@@@@@%%_/%\_%@@%%@@@@@@@%%%%%%
# 
# # ModelCompiled<-stan_model("./B_Model_Code/NonCentered_GenBeta_Linear_Social_RiskSafe.stan")
# # results=optimizing(ModelCompiled, data=dataList,init=genInitList)
# # 
# # # here i tear up the sequence. its saved as factor so i need to make it into a character string and split it up according to the seperator,
# # for (t in 1:useTrials) {
#   Blue <- strsplit(toString((Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$blue_marbles[t])), split = ",")
#   BlueSum <- 0
#   
#   # now i add up the number of sucesses
#   for (k in 1:length(Blue[[1]])) {
#     BlueSum <- BlueSum + as.numeric(Blue[[1]][k])
#   }
#   Blue_Sum_All[i, t] <- BlueSum
#   
#   # here i add up the number of failures
#   Red <- strsplit(toString((Bin_Update_Data[Bin_Update_Data$participant == curSubj, ]$red_marbles[t])), split = ",")
#   RedSum <- 0
#   
#   for (k in 1:length(Blue[[1]])) {
#     RedSum <- RedSum + as.numeric(Red[[1]][k])
#   }
#   Red_Sum_All[i, t] <- RedSum
#   
#   # this isfor sequential updating where binary sucesses and fails are stored in the thrid dimension of the matrix
#   if (RedSum != 99) {
#     Sucess[i, t, 1:Sequence_Length] <- c(array(1, c(BlueSum)), array(0, c(Sequence_Length - BlueSum)))
#     Fail[i, t, 1:Sequence_Length] <- c(array(1, c(RedSum)), array(0, c(Sequence_Length - RedSum)))
#   } else {
#     # if it was a risk trial, then either put in 99 or make sure that you sample from uniform.
#     Sucess[i, t, 1:Sequence_Length] <- 99
#     Red_Sum_All[i, t] <- 1
#     Fail[i, t, 1:Sequence_Length] <- 99
#     Blue_Sum_All[i, t] <- 1
#   }
# } # end spli
# }