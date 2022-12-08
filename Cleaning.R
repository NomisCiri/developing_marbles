#you really have to clean up.


# this script subsets the data and fits different learning models on the PBS cluster of the MPI

library(tidyverse)
library(rstan)

bashInput <- commandArgs(trailingOnly = TRUE)
bashInput=c(5)
####### Subsetting and Model Determination Block
MarbleData<-read.csv("A_RawData/TidyMarbleNew.csv")

LearningModelPath<-c("B_Model_Code/NonCentered_GenBeta.stan",
                     "B_Model_Code/NonCentered_GenBeta_Linear.stan",
                     "B_Model_Code/NonCentered_GenBeta_Linear_Social.stan",
                     "B_Model_Code/TryUpdate.stan")

Agegroups=c("0","1","2","3","4")
Agegroups[as.numeric(bashInput[1])]

MarbleData<-MarbleData%>%mutate(Agegroup=case_when(
  (age<13)~"0",
  (age>=13 & age<16)~"1",
  (age>=16 & age<20)~"2",
  (age>=20 & age<23)~"3",
  (age>=23)~"4"
)
)

hist(MarbleData$subject,breaks=length(unique(MarbleData$subject))*4)

MarbleData$Agegroup=as.factor(MarbleData$Agegroup)
MarbleData$DFE1DFD0=as.factor(MarbleData$DFE1DFD0)
MarbleData$Social1Ind0=as.factor(MarbleData$Social1Ind0)
MarbleData$PercentBlueEstimate=round(as.numeric(MarbleData$PercentBlueEstimate))
MarbleData$PercentBlueShownRel=round(as.numeric(MarbleData$PercentBlueShownRel),2)
MarbleData$PercentBlueShownRel=as.factor(as.character(MarbleData$PercentBlueShownRel))
MarbleData$HowSure=as.numeric(MarbleData$HowSure)

# Set Priors and get everything ready for stan
MarbleData<-MarbleData%>%filter(Agegroup==Agegroups[as.numeric(bashInput[1])])#Bin_Update_Data<-Bin_Update_Data[Bin_Update_Data$typeRA=="1",]# keep only The Risk trails.

Bin_Update_Data<-MarbleData%>%arrange(Agegroup)# i order it first so i can make sure that 
#make it fit with my stan file.
Bin_Update_Data<-Bin_Update_Data%>%dplyr::mutate(
  OtherChoseRisk = case_when(
    OtherChoseRisk=="NULL" ~ 2,# i dont need this but i restricted the numbers in stan between 1 and 3
    OtherChoseRisk=="1" ~ 3,# risky choices are coded as 3 in my stan code
    OtherChoseRisk=="0" ~ 1,# safe choices are coded as 1 in my stan code
    TRUE~0 # keep the rest.
  )
)#end PeerChoice.
#subset it to fit on only one agegroup. 
#Bin_Update_Data%>%filter(Agegroup==2)->Bin_Update_Data
#Bin_Update_Data%>%filter(DFE1DFD0==0)->Bin_Update_Data

# now check how many participants we have.
Bin_Update_Data$subject<-as.numeric(Bin_Update_Data$subject)
#change colname of subject into subjID
numSubjs<-length(unique(Bin_Update_Data$subject))#Total Number of Subs
subjList <- unique(Bin_Update_Data$subject)######
Sequence_Length <- unique(Bin_Update_Data[Bin_Update_Data$TotalNShown<99,]$TotalNShown)
Sequence_Length<-9
####### number of trials and see which group he is in for each subject######
Tsubj <- as.vector(rep(0, numSubjs)) 

for (sIdx in 1:numSubjs)  {
  curSubj     <- subjList[sIdx]
  Tsubj[sIdx] <- length(Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$subject)  # How many entries per Subject?
  #GSubj[sIdx] <- Simulations[[i]]$group
}
maxTrials <- max(Tsubj)

# Information for user continued
cat(" # of (max) trials per subject = ", maxTrials, "\n\n")


# for multiple subjects

condition    <- array(0, c(numSubjs, maxTrials))#Other Chose Risk?
p_gamble    <- array(0, c(numSubjs, maxTrials))
choice  <- array(0, c(numSubjs, maxTrials))

safe_payoff<- array(0, c(numSubjs, maxTrials))
risky_payoff<- array(0, c(numSubjs, maxTrials))

risk1Unc0  <- array(0, c(numSubjs, maxTrials))

#specs for the uncertain trails
Sucess  <- array(0, c(numSubjs, maxTrials,Sequence_Length))
Fail  <- array(0, c(numSubjs, maxTrials,Sequence_Length))
p_gamble_est<- array(0, c(numSubjs, maxTrials))
Blue_Sum_All<- array(0, c(numSubjs, maxTrials))
Red_Sum_All<- array(0, c(numSubjs, maxTrials))
#Agegroup<- array(0, c(numSubjs, maxTrials))

# generate the data Lists to be passed to stan
# concatenate different groups in the third dimension.
gambleList<-unique(MarbleData$probGamble)
for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  useTrials    <- Tsubj[i]
  condition[i, 1:useTrials]    <- Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$OtherChoseRisk
  p_gamble[i, 1:useTrials]    <- Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$probGamb
  choice[i, 1:useTrials]    <- Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$ChooseRisk
  
  risky_payoff[i, 1:useTrials]    <- Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$valueGamble
  safe_payoff[i, 1:useTrials]    <- 5
  
  risk1Unc0[i, 1:useTrials] <- as.numeric(as.character(Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$DFE1DFD0))
  p_gamble_est[i, 1:useTrials]<-as.double(as.character(Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$PercentBlueEstimate))/100
  p_gamble_est[p_gamble_est>0.99]=0.99
  #ambigLevel[i, 1:useTrials] <- Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$Ambiguity
  
  # here i tear up the sequence. its saved as factor so i need to make it into a character string and split it up according to the seperator,
  for (t in 1:useTrials){
    Blue<-strsplit(toString((Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$blue_marbles[t])),split=",")
    BlueSum<-0;
    
    # now i add up the number of sucesses
    for (k in 1:length(Blue[[1]])){
      BlueSum=BlueSum+as.numeric(Blue[[1]][k])
    }
    Blue_Sum_All[i, t]<-BlueSum
    
    #here i add up the number of failures
    Red<-strsplit(toString((Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$red_marbles[t])),split=",")
    RedSum<-0;
    
    for (k in 1:length(Blue[[1]])){
      RedSum=RedSum+as.numeric(Red[[1]][k])
    }
    Red_Sum_All[i, t]<-RedSum
    
    # this isfor sequential updating where binary sucesses and fails are stored in the thrid dimension of the matrix
    if(RedSum!=99){
      Sucess[i, t,1:Sequence_Length]<-c(array(1, c(BlueSum)), array(0, c(Sequence_Length-BlueSum)) )
      Fail[i, t,1:Sequence_Length]<-c(array(1, c(RedSum)), array(0, c(Sequence_Length-RedSum)) )
    }else{
      # if it was a risk trial, then either put in 99 or make sure that you sample from uniform.
      Sucess[i, t,1:Sequence_Length]<-99
      Red_Sum_All[i, t]<-1
      Fail[i, t,1:Sequence_Length]<-99
      Blue_Sum_All[i, t]<-1
    }
  }
}
p_gambleIndex=p_gamble
for (i in 1:length(gambleList)){
  p_gambleIndex[p_gambleIndex==gambleList[i]]=i
}

#cant be bigger than 1.
p_gamble_est[p_gamble_est>=1]<-0.99
# Specify the number of parameters and parameters of interest
#numPars <- 5

inits<-"fixed"
# priors
if (inits[1] != "random") {
  if (inits[1] == "fixed") {
    inits_fixed <- c(0.1, 1, 0.0 ,0.5, 0.5)
  } else {
    if (length(inits) == numPars) {
      inits_fixed <- inits
      # mu_ocu   =rep(inits_fixed[3],2)
    } else {
      stop("Check your inital values!")
    }
  }
  genInitList <- function() {
    list(
      #initial values.
      mu_rho   =1,
      mu_UncertaintyMulti =1,
      mu_UncertaintyMulti_Risk =1,
      #Unc_Beta=array(0.01,dim=c(numSubjs,6)),
      mu_alpha_add=1,
      sigma_rho= rep(1),
      sigma_UncertaintyMulti= rep(c(1)),
      sigma_UncertaintyMulti_Risk= rep(c(1)),
      sigma_alpha_add=0.5,
      #  sigma_beta_add=1.0,
      rho_p    = rep(0,numSubjs),
      UncertaintyMulti_p  = rep(0.5,numSubjs),
      UncertaintyMulti_Risk_p  = rep(0.5,numSubjs),
      alpha_add_p    = rep(0,numSubjs),
      alpha_shape    = array(10,dim=c(numSubjs,6)),
      beta_shape=array(10,dim=c(numSubjs,6))
    )
  }
} else {
  genInitList <- "random"
}



dataList <- list(
  N       = numSubjs,## number of subjects in each group.
  T       = maxTrials,
  #Seq     = Sequence_Length,
  Tsubj   = Tsubj,
  # numPars = numPars,
  safe_payoff    = safe_payoff,
  risky_payoff = risky_payoff,
  p_gamble_est= p_gamble_est,
  Sucess  = Blue_Sum_All,
  Fail    = Red_Sum_All,
  condition  = condition, # condition is 0= solo ,1 = safe  ,3 = risky
  p_gamble = p_gambleIndex,
  gambleList=gambleList,
  NGambles=length(gambleList),
  choice = choice,
  risk1Unc0=risk1Unc0
  # agegroup=agegroup
)


POI     <- c("alpha_add",
             "rho")

POI_Social     <- c("alpha_add",
                    "rho","UncertaintyMulti","UncertaintyMulti_Risk","log_lik","gamble_Pred","y_pred")
#POI=c("alpha","beta","rho")
ModelFit = stan(LearningModelPath[1],
                data   = dataList,
                pars   = POI,
                init   = genInitList,
                iter   = 2000,
                cores = 3,
                chains =3,
                control = list(adapt_delta = 0.90,
                               max_treedepth=20)
)


saveRDS(ModelFit,file=paste0("../C_ModelFits/Beta_Test_Age_4.rds"))


ModelFit2 = stan(LearningModelPath[2],
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 2000,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta = 0.90,
                                max_treedepth=20)
)
saveRDS(ModelFit2,file=paste0("../C_ModelFits/Beta_Test2_Age_4.rds"))



#gambleList=replicate(numSubjs,unique(MarbleData$probGamble))
gambleList<-unique(MarbleData$probGamble)

dataList <- list(
  N       = numSubjs,## number of subjects in each group.
  T       = maxTrials,
  #Seq     = Sequence_Length,
  Tsubj   = Tsubj,
  # numPars = numPars,
  safe_payoff    = safe_payoff,
  risky_payoff = risky_payoff,
  p_gamble_est= p_gamble_est,
  Sucess  = Blue_Sum_All,
  Fail    = Red_Sum_All,
  condition  = condition, # condition is 0= solo ,1 = safe  ,3 = risky
  p_gamble = p_gambleIndex,
  gambleList=gambleList,
  NGambles=length(gambleList),
  choice = choice,
  risk1Unc0=risk1Unc0
  # agegroup=agegroup
)
genInitList <- function() {
  list(
    #initial values.
    mu_rho   =1,
    UncertaintyMulti =rep(1,numSubjs),
    UncertaintyMulti_Risk =rep(1,numSubjs),
    #Unc_Beta=array(0.01,dim=c(numSubjs,6)),
    mu_alpha_add=1,
    sigma_rho= rep(1),
    sigma_alpha_add=0.5,
    #  sigma_beta_add=1.0,
    rho_p    = rep(0,numSubjs),
    alpha_add_p    = rep(0,numSubjs),
    alpha_shape    = array(10,dim=c(numSubjs,6)),
    beta_shape=array(10,dim=c(numSubjs,6))
  )
}


ModelFit3 = stan(LearningModelPath[3],
                 data   = dataList,
                 pars   = POI_Social,
                 init   = genInitList,
                 iter   = 1000,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta = 0.90,
                                max_treedepth=20)
)




saveRDS(ModelFit3,file=paste0("../C_ModelFits/Beta_Test3_Age_4.rds"))





#gambleList=replicate(numSubjs,unique(MarbleData$probGamble))
gambleList<-unique(MarbleData$probGamble)
gambleList=replicate(numSubjs,unique(MarbleData$probGamble))

dataList <- list(
  N       = numSubjs,## number of subjects in each group.
  T       = maxTrials,
  #Seq     = Sequence_Length,
  Tsubj   = Tsubj,
  # numPars = numPars,
  safe_payoff    = safe_payoff,
  risky_payoff = risky_payoff,
  p_gamble_est= p_gamble_est,
  Sucess  = Blue_Sum_All,
  Fail    = Red_Sum_All,
  condition  = condition, # condition is 0= solo ,1 = safe  ,3 = risky
  p_gamble = p_gambleIndex,
  gambleList=gambleList,
  NGambles=length(gambleList),
  choice = choice,
  risk1Unc0=risk1Unc0
)



ModelFit4 = stan(LearningModelPath[4],
                 data   = dataList,
                 #pars   = POI_Social,
                 init   = genInitList,
                 iter   = 2000,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta = 0.90,
                                max_treedepth=20)
)
