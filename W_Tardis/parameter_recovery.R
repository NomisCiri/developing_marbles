# this script subsets the data and fits different learning models on the PBS cluster of the MPI

library(tidyverse)
library(rstan)
setwd("./W_Tardis/")
source("SimulateMarbles.R")
bashInput <- commandArgs(trailingOnly = TRUE)
#bashInput<-c(2000,2)

RecoveryDF<-data.frame()
####### Subsetting and Model Determination Block
LearningModelPath<-c("../B_Model_Code/NonCentered_GenBeta.stan",
                     "../B_Model_Code/NonCentered_GenBeta_Linear.stan",
                     "../B_Model_Code/NonCentered_GenBeta_Linear_Social.stan",
                     "../B_Model_Code/NonCentered_GenBeta_Linear_Social_RiskSafe_multi.stan",
                     "../B_Model_Code/TryUpdate.stan",
                     "../B_Model_Code/workingModel.stan")# IT IS REALLY THE WORKING; BEST FITTING MODEL!!!!)

POI_Social     <- c("alpha_add",
                    "rho","UncertaintyMulti","UncertaintyMulti_Risk",
                    "log_lik","gamble_Pred","y_pred")

#for(i in 2:5){## bc sth is wrong with the tardis i do it over here. 
njobs<-as.numeric(bashInput[[1]])
job<-as.numeric(bashInput[[2]])
MarbleData<-read.csv("../A_RawData/TidyMarbleNew.csv")
Agegroups=c("0","1","2","3","4")

MarbleData<-MarbleData%>%mutate(Agegroup=case_when(
  (age<13)~"0",
  (age>=13 & age<16)~"1",
  (age>=16 & age<19)~"2",
  (age>=19 & age<22)~"3",
  (age>=22)~"3"))

#Params<-expand_grid(rho,kappa,alpha_add,UncertaintyMulti_Risk,UncertaintyMulti)

hist(MarbleData$subject,breaks=length(unique(MarbleData$subject))*4)

MarbleData$Agegroup=as.factor(MarbleData$Agegroup)
MarbleData$DFE1DFD0=as.factor(MarbleData$DFE1DFD0)
MarbleData$Social1Ind0=as.factor(MarbleData$Social1Ind0)
MarbleData$PercentBlueEstimate=round(as.numeric(MarbleData$PercentBlueEstimate))
MarbleData$PercentBlueShownRel=round(as.numeric(MarbleData$PercentBlueShownRel),2)
MarbleData$PercentBlueShownRel=as.factor(as.character(MarbleData$PercentBlueShownRel))
MarbleData$HowSure=as.numeric(MarbleData$HowSure)

# Set Priors and get everything ready for stan
MarbleData<-MarbleData%>%#&subject!=20) subject!=153 & subject!=158
  filter(subject==1 | subject== 50 | subject==100)%>%
  #filter(subject!=20 & subject!=153 & subject!=158 & subject!=85 & subject!=101)%>%
  filter((!is.na(PercentBlueEstimate) | DFE1DFD0==0))#Bin_Update_Data-Bin_Update_Data[Bin_Update_Data$typeRA=="1",]# keep only The Risk trails.
#MarbleData<-MarbleData%>%filter(Agegroup=="3")
#MarbleData<-do.call("rbind", replicate(20, MarbleData, simplify = FALSE))

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
  condition[i, 1:maxTrials]<-sample(c(1,3),size = 144,replace=T)# simulate social info
  p_gamble[i, 1:useTrials]    <- Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$probGamb
  choice[i, 1:useTrials]    <- Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$ChooseRisk
  
  risky_payoff[i, 1:useTrials]    <- Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$valueGamble
  safe_payoff[i, 1:useTrials]    <- 5
  
  risk1Unc0[i, 1:useTrials] <- as.numeric(as.character(Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$DFE1DFD0))
  p_gamble_est[i, 1:useTrials]<-as.double(as.character(Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$PercentBlueEstimate))/100
  p_gamble_est[i,which(risk1Unc0[i,1:useTrials]==0)]<-p_gamble[i,which(risk1Unc0[i,1:useTrials]==0)]# put in correct ps for risk
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
#gambleList=replicate(numSubjs,unique(MarbleData$probGamble))
gambleList<-unique(MarbleData$probGamble)
print(gambleList)
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
    UncertaintyMulti =rep(1.1,numSubjs),
    UncertaintyMulti_Risk =rep(0.5,numSubjs),
    
    UncertaintyMulti_safe =rep(1,numSubjs),
    UncertaintyMulti_Risk_safe =rep(0.5,numSubjs),
    UncertaintyMulti_risk =rep(0.5,numSubjs),
    UncertaintyMulti_Risk_risk =rep(1,numSubjs),
    #Unc_Beta=array(0.01,dim=c(numSubjs,6)),
    UncertaintyMulti_safe_mu =1,
    UncertaintyMulti_Risk_safe_mu =1,
    UncertaintyMulti_risk_mu =1,
    UncertaintyMulti_Risk_risk_mu =1,
    
    UncertaintyMulti_safe_sigma =1,
    UncertaintyMulti_Risk_safe_sigma =1,
    UncertaintyMulti_risk_sigma =1,
    UncertaintyMulti_Risk_risk_sigma =1,
    
    UncertaintyMulti_safe_p =rep(0,numSubjs),
    UncertaintyMulti_Risk_safe_p =rep(0,numSubjs),
    UncertaintyMulti_risk_p =rep(0,numSubjs),
    UncertaintyMulti_Risk_risk_p =rep(0,numSubjs),
    
    mu_alpha_add=-1,
    sigma_alpha_add=0.5,
    alpha_add_p    = rep(0,numSubjs),
    
    sigma_alpha_add_risk=0.5,
    mu_alpha_add_risk=0.1,
    alpha_add_risk_p    = rep(0,numSubjs),
    
    sigma_alpha_add_safe=0.5,
    mu_alpha_add_safe=0.1,
    alpha_add_safe_p    = rep(0,numSubjs),
    #  sigma_beta_add=1.0,
    mu_rho   =0,
    sigma_rho= rep(1),
    rho_p    = rep(0,numSubjs),
    tau= rep(0.7,numSubjs),
    kappa    = array(9,dim=c(numSubjs,length(gambleList)))
  )
  
}

##### set up paramters for recovery. 
set.seed(123)
rho <- sample(seq(0.1,2,length.out=10))
kappa<- sample(seq(1,5,length.out=10))
alpha_add<- sample(seq(0.1,2,length.out=10))
alpha_add_risk<- sample(seq(0.1,2,length.out=10))
alpha_add_safe<- sample(seq(0.1,2,length.out=10))


rho <- 0.5# effectively truns off the value.
kappa<- 2
alpha_add<- 1
#alpha_add_risk<- sample(seq(0.1,2,length.out=10))
#alpha_add_safe<- sample(seq(0.1,2,length.out=10))
UncertaintyMulti_Risk_risk<-1#sample(seq(0.1,4,length.out=10))
UncertaintyMulti_risk<-1#sample(seq(0.1,4,length.out=10))
UncertaintyMulti_Risk_safe<-1#sample(seq(0.1,4,length.out=10))
UncertaintyMulti_safe<-1#sample(seq(0.1,4,length.out=10))

#set parameter to recover & keep the rest as they are.
if(recoveryPar==1){
  rho <- sample(seq(0.1,2,length.out=10))
}else if(recoveryPar==2){
  kappa<- sample(seq(1,5,length.out=10))
}else if(recoveryPar==3){
  alpha_add<- sample(seq(0.1,2,length.out=10))
}else if(recoveryPar==4){
  UncertaintyMulti_Risk_risk<-seq(0.5,4,length.out=10)
}else if(recoveryPar==5){
  UncertaintyMulti_risk<-seq(0.5,4,length.out=10)
}else if(recoveryPar==6){
  UncertaintyMulti_Risk_safe<-seq(0.5,4,length.out=10)
}else if(recoveryPar==7){
  UncertaintyMulti_safe<-seq(0.5,4,length.out=10)
}

allParams=
  expand.grid("rho"=rho,"kappa"=kappa,"alpha_add"=alpha_add,"UncertaintyMulti_Risk_risk"=UncertaintyMulti_Risk_risk,"UncertaintyMulti_risk"=UncertaintyMulti_risk,
              "UncertaintyMulti_safe"=UncertaintyMulti_safe,"UncertaintyMulti_Risk_safe"=UncertaintyMulti_Risk_safe)

#choice=
njobs=1
OneJob<-length(allParams[,1])/njobs
idx=job*OneJob
RecoverySubset=allParams#[idx:(idx+OneJob),]
#length(RecoverySubset$rho)
nReps=100
RecoveryScatterAll<-list()
cnt=1
parsSimul<-c("rho","kappa","weight","Social_Risk_risk","Social_Risk_safe","Social_Unc_risk","Social_Unc_safe")

for (recoveryPar in 1:7){
  rho <- 0.5# effectively truns off the value.
  kappa<- 2
  alpha_add<- 1
  #alpha_add_risk<- sample(seq(0.1,2,length.out=10))
  #alpha_add_safe<- sample(seq(0.1,2,length.out=10))
  UncertaintyMulti_Risk_risk<-1#sample(seq(0.1,4,length.out=10))
  UncertaintyMulti_risk<-1#sample(seq(0.1,4,length.out=10))
  UncertaintyMulti_Risk_safe<-1#sample(seq(0.1,4,length.out=10))
  UncertaintyMulti_safe<-1#sample(seq(0.1,4,length.out=10))
  
  #set parameter to recover & keep the rest as they are.
  
  if(recoveryPar==1){
    rho <- sample(seq(0.1,2,length.out=10))
  }else if(recoveryPar==2){
    kappa<- sample(seq(1,5,length.out=10))
  }else if(recoveryPar==3){
    alpha_add<- sample(seq(0.1,2,length.out=10))
  }else if(recoveryPar==4){
    UncertaintyMulti_Risk_risk<-seq(0.5,4,length.out=10)
  }else if(recoveryPar==5){
    UncertaintyMulti_Risk_safe<-seq(0.5,4,length.out=10)
  }else if(recoveryPar==6){
    UncertaintyMulti_risk<-seq(0.5,4,length.out=10)
  }else if(recoveryPar==7){
    UncertaintyMulti_safe<-seq(0.5,4,length.out=10)
  }
  
  RecoverySubset=expand.grid("rho"=rho,
                             "kappa"=kappa,
                             "alpha_add"=alpha_add,
                             "UncertaintyMulti_Risk_risk"=UncertaintyMulti_Risk_risk,
                             "UncertaintyMulti_risk"=UncertaintyMulti_risk,
                             "UncertaintyMulti_safe"=UncertaintyMulti_safe,
                             "UncertaintyMulti_Risk_safe"=UncertaintyMulti_Risk_safe
  )
  
  for (reps in 1:nReps){
    RecoveryDF=NULL
    for (i in 1:length(RecoverySubset$rho)){
      print(i)
      # make social information. 
      #rho,kappa,alpha_add,UncertaintyMulti_Risk_risk,UncertaintyMulti_Risk_safe,UncertaintyMulti_safe,UncertaintyMulti_risk
      ypred=simulateMarbleModelFinal(dataList,
                                     RecoverySubset[i,]$rho,
                                     RecoverySubset[i,]$kappa,
                                     RecoverySubset[i,]$alpha_add,
                                     RecoverySubset[i,]$UncertaintyMulti_Risk_risk,
                                     RecoverySubset[i,]$UncertaintyMulti_Risk_safe,
                                     RecoverySubset[i,]$UncertaintyMulti_safe,
                                     RecoverySubset[i,]$UncertaintyMulti_risk,
                                     tau=0.7)
      dataList$choice<-ypred[[1]]
      dataList$p_gamble_est<-ypred[[2]]
      
      ######
      ###### Here i fit the model using a classical optimization algorithm
      ######
      ModelCompiled<-stan_model( "../B_Model_Code/NonCentered_GenBeta_Linear_Social_RiskSafe.stan")
      ModelFit3 = optimizing(ModelCompiled,
                             data   = dataList,
                             algorithm = "LBFGS",
                             verbose = TRUE,
                             init=genInitList()
      )
      #clear containers
      rho=array()
      kappa=array()
      alpha_add=array()
      UncertaintyMulti_Risk_risk=array()
      UncertaintyMulti_risk=array()
      UncertaintyMulti_Risk_safe=array()
      UncertaintyMulti_safe=array()
      
      #fill containers
      for(z in 1:dataList$N){
        rho=cbind(rho,ModelFit3$par[paste0("rho[",z,"]")])
        for(q in 1:dataList$NGambles){
          kappa=cbind(kappa,ModelFit3$par[paste0("kappa[",z,",",q,"]")])
        }
        alpha_add=cbind(alpha_add,ModelFit3$par[paste0("alpha_add[",z,"]")])
        UncertaintyMulti_Risk_risk=cbind(UncertaintyMulti_Risk_risk,ModelFit3$par[paste0("UncertaintyMulti_Risk_risk[",z,"]")])
        UncertaintyMulti_risk=cbind(UncertaintyMulti_risk,ModelFit3$par[paste0("UncertaintyMulti_risk[",z,"]")])
        
        UncertaintyMulti_Risk_safe=cbind(UncertaintyMulti_Risk_safe,ModelFit3$par[paste0("UncertaintyMulti_Risk_safe[",z,"]")])
        UncertaintyMulti_safe=cbind(UncertaintyMulti_safe,ModelFit3$par[paste0("UncertaintyMulti_safe[",z,"]")])
      }
      
      RecoveryDF<-rbind(
        RecoveryDF,data.frame(
          rho_Recov=mean(rho,na.rm = T),
          rho_Sim=RecoverySubset[i,]$rho,
          kappa_Recov=mean(kappa,na.rm = T),
          kappa_Sim=RecoverySubset[i,]$kappa,
          alpha_add_Recov=mean(alpha_add,na.rm = T),
          alpha_add_Sim=RecoverySubset[i,]$alpha_add,
          UncertaintyMulti_Risk_risk_Recov=mean(UncertaintyMulti_Risk_risk,na.rm = T),
          UncertaintyMulti_Risk_risk_Sim=RecoverySubset[i,]$UncertaintyMulti_Risk_risk,
          UncertaintyMulti_risk_Recov=mean(UncertaintyMulti_risk,na.rm = T),
          UncertaintyMulti_risk_Sim=RecoverySubset[i,]$UncertaintyMulti_risk,
          UncertaintyMulti_Risk_safe_Recov=mean(UncertaintyMulti_Risk_safe,na.rm = T),
          UncertaintyMulti_Risk_safe_Sim=RecoverySubset[i,]$UncertaintyMulti_Risk_safe,
          UncertaintyMulti_safe_Recov=mean(UncertaintyMulti_safe,na.rm = T),
          UncertaintyMulti_safe_Sim=RecoverySubset[i,]$UncertaintyMulti_safe,
          pair=i
        )%>%pivot_longer(names_to="Parameter",values_to="Estimate",cols = -pair,)
      )
      
      RecoveryDF%>%mutate(SimOrRec=case_when(
        grepl("_Sim",RecoveryDF$Parameter,fixed=TRUE)~"Sim",
        grepl("_Recov",RecoveryDF$Parameter,fixed=TRUE)~"Recov"
      ),
      Parameter2=case_when(
        grepl("rho",RecoveryDF$Parameter,fixed=TRUE)~"rho",
        grepl("kappa",RecoveryDF$Parameter,fixed=TRUE)~"kappa",
        grepl("alpha_add",RecoveryDF$Parameter,fixed=TRUE)~"weight",
        grepl("UncertaintyMulti_Risk_risk",RecoveryDF$Parameter,fixed=TRUE)~"Social_Risk_risk",
        grepl("UncertaintyMulti_Risk_safe",RecoveryDF$Parameter,fixed=TRUE)~"Social_Risk_safe",
        grepl("UncertaintyMulti_risk",RecoveryDF$Parameter,fixed=TRUE)~"Social_Unc_risk",
        grepl("UncertaintyMulti_safe",RecoveryDF$Parameter,fixed=TRUE)~"Social_Unc_safe"
      ),
      par_Varied=parsSimul[recoveryPar]
      )->RecoveryScatter
    }# end recovery
    
    RecoveryScatterAll[[cnt]]<-RecoveryScatter
    cnt=cnt+1
  }
}# end pars

# RecoveryScatter%>%pivot_wider(names_from = SimOrRec,values_from = Estimate,id_cols = c(pair,Parameter2))%>%
#    saveRDS(.,file=paste0("../F_ParameterRecovery/Iter3_",job))

saveRDS(RecoveryScatterAll%>%map_dfr(.,~{.x},.id="id"),file = "30_09_Recovery.rds")

ParametersLab=c(rho=expression(lambda),
                weight=expression(sigma*" unc"),
                kappa=expression(sigma*" risk"),
                Social_Unc_safe=expression(psi*"Safe unc"),
                Social_Risk_safe=expression(psi*"Safe risk"),
                Social_Risk_risk=expression(psi*"Risk unc"),
                Social_Unc_risk=expression(psi*"Risk risk")
)

ParametersLab=c(expression(lambda),
                expression(sigma*" unc"),
                expression(sigma*" risk"),
                expression(psi*"Safe unc"),
                expression(psi*"Safe risk"),
                expression(psi*"Risk unc"),
                expression(psi*"Risk risk")
)


RecoveryScatterAll%>%map_dfr(.,~{.x},.id="id")%>%
  filter(Parameter2==par_Varied)%>%
  mutate(Parameter3 = case_when(Parameter2=="rho"~"lambda",
                                Parameter2=="weight"~"sigma['unc']",
                                Parameter2=="kappa"~"sigma['risk']",
                                Parameter2=="Social_Unc_safe"~"psi['safe_unc']",
                                Parameter2=="Social_Risk_safe"~"psi['safe_risk']",
                                Parameter2=="Social_Risk_risk"~"psi['risky_risk']",
                                Parameter2=="Social_Unc_risk"~"psi['risk_unc']"
  )
  )%>%
  #mutate(Parameter2=factor(Parameter2),labels=ParametersLab)
  pivot_wider(names_from = SimOrRec,values_from = Estimate,id_cols = c("id","pair","Parameter2","Parameter3"))%>%
  ggplot(aes(x=Sim,y=Recov))+geom_point(alpha=0.01)+
  geom_abline(slope=1)+
  stat_summary(color="red")+
  stat_smooth(method="lm")+
  scale_y_continuous(name="Recovered Parameter")+
  scale_x_continuous(name="Simulation Parameter")+
  #ggtitle("Parameter Recovery")+
  facet_wrap(Parameter3~.,scales = "free",labeller = label_parsed)+
  theme_minimal(14)->recovery
# 


ggsave(recovery,filename = "../X_Figures/Recovery1.png",width = 5,height = 5)
