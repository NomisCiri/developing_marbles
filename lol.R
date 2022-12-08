get_data_List<-function(Agegroup){
  MarbleData<-read.csv("./A_RawData/TidyMarbleNew.csv")
  
MarbleData<-MarbleData%>%mutate(Agegroup=case_when(
  (age<13)~"0",
  (age>=13 & age<16)~"1",
  (age>=16 & age<19)~"2",
  (age>=19 & age<22)~"3",
  (age>=22)~"4"))%>%filter((probGamble!=0.675) | Social1Ind0==0)

QuestionnaireData<-read.csv("./A_RawData/Questionnaire_SumScore.csv")

#PDS Scoring based on Pompéia et al. 2019
QuestionnaireData%>%group_by(subject)%>%
  mutate(PDS3.x=case_when((as.character(PDS3.x)=="1" & sex=="Weiblich")~"3",TRUE~as.character(PDS3.x)))%>%mutate(
    SumPDS=(as.numeric(as.character(PDS1.x))+1)+(as.numeric(as.character(PDS2.x))+1)+(as.numeric(as.character(PDS3.x))+1)
  )%>%
  mutate(PubertyStage=case_when(
    (SumPDS <=3 & sex=="Maennlich")~"0",
    (SumPDS>=4 & SumPDS<6 & sex=="Maennlich")~"1",
    (SumPDS>=6 & SumPDS<9  & sex=="Maennlich")~"2",
    (SumPDS>=9 & SumPDS<12 & sex=="Maennlich")~"3",
    (SumPDS>=12 & sex=="Maennlich")~"4",
    (SumPDS==3 & sex=="Weiblich")~"0",
    (SumPDS>=4 & SumPDS<6 & sex=="Weiblich")~"1",
    (SumPDS>=6 & SumPDS<=8 & sex=="Weiblich")~"2",
    (SumPDS>=8 & sex=="Weiblich")~"3",
    (SumPDS==12 & sex=="Weiblich")~"4"
  ))%>%select(PubertyStage,subject)%>%
  right_join(MarbleData,by = "subject")->MarbleData

ggplot(MarbleData, aes(x=as.numeric(PubertyStage),color=sex))+
  geom_histogram(aes(y=..count../144))

hist(MarbleData$subject,breaks=length(unique(MarbleData$subject))*4)

MarbleData$Agegroup=as.factor(MarbleData$Agegroup)
MarbleData$DFE1DFD0=as.factor(MarbleData$DFE1DFD0)
MarbleData$Social1Ind0=as.factor(MarbleData$Social1Ind0)
MarbleData$PercentBlueEstimate=round(as.numeric(MarbleData$PercentBlueEstimate))
MarbleData$PercentBlueShownRel=round(as.numeric(MarbleData$PercentBlueShownRel),2)
MarbleData$PercentBlueShownRel=as.factor(as.character(MarbleData$PercentBlueShownRel))
MarbleData$HowSure=as.numeric(MarbleData$HowSure)

MarbleData[is.na(MarbleData$PercentBlueEstimate),]$PercentBlueEstimate=200
# Set PrioMarbleDatars and get everything ready for stan
MarbleData<-MarbleData#%>%filter(PubertyStage==Agegroup)#Bin_Update_Data<-Bin_Update_Data[Bin_Update_Data$typeRA=="1",]# keep only The Risk trails.

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
  p_gamble[i, 1:useTrials]    <- Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$probGamble
  choice[i, 1:useTrials]    <- Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$ChooseRisk
  
  risky_payoff[i, 1:useTrials]    <- Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$valueGamble
  safe_payoff[i, 1:useTrials]    <- 5
  
  risk1Unc0[i, 1:useTrials] <- as.numeric(as.character(Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$DFE1DFD0))
  p_gamble_est[i, 1:useTrials]<-as.double(as.character(Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$PercentBlueEstimate))/100
  #print(as.character(Bin_Update_Data[Bin_Update_Data$subject==curSubj,]$PercentBlueEstimate))
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
p_gamble_est[p_gamble_est>=1]<-0.5
# Specify the number of parameters and parameters of interest
#numPars <- 5
#gambleList=replicate(numSubjs,unique(MarbleData$probGamble))
gambleList<-unique(MarbleData$probGamble)
print(gambleList)
nGambles=length(gambleList)
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
    UncertaintyMulti =rep(1,numSubjs),
    UncertaintyMulti_Risk =rep(2,numSubjs),
    UncertaintyMulti_safe =rep(2,numSubjs),
    UncertaintyMulti_Risk_safe =rep(2,numSubjs),
    UncertaintyMulti_risk =rep(2,numSubjs),
    UncertaintyMulti_Risk_risk =rep(2,numSubjs),
    #Unc_Beta=array(0.01,dim=c(numSubjs,6)),
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
    kappa    = array(7,dim=c(numSubjs,nGambles))
  )
  
}
return(list(dataList,genInitList))
}






#do it for optim
FitMarbles<-function(MarbleData,Agegroups,i,ModelCompiled){
  library(rstan)
  listInits<-get_data_List(Agegroups[i])
  dataList=listInits[[1]]
  genInitList=listInits[[2]]
  
  results=optimizing(ModelCompiled, data=dataList,init=genInitList,verbose=T)
  
  MarbleData2<-MarbleData#%>%filter(PubertyStage==Agegroups[i])#Bin_Update_Data<-Bin_Update_Data[Bin_Update_Data$typeRA=="1",]# keep only The Risk trails.
  subs=unique(MarbleData2$subject)
  age=unique(MarbleData2$age)
  PubertyStage=unique(MarbleData2$PubertyStage)
  
  alpha_add=NULL
  rho=NULL
  UncertaintyMulti_Risk_risk=NULL
  UncertaintyMulti_Risk_safe=NULL
  UncertaintyMulti_risk=NULL
  UncertaintyMulti_safe=NULL
  for (subject in 1:NSubs){
    alpha_add[subject]=as.numeric(results$par[paste0("alpha_add[",subject,"]")])
    rho[subject]=as.numeric(results$par[paste0("rho[",subject,"]")])
    UncertaintyMulti_Risk_risk[subject]=as.numeric(results$par[paste0("UncertaintyMulti_Risk_risk[",subject,"]")])
    UncertaintyMulti_Risk_safe[subject]=as.numeric(results$par[paste0("UncertaintyMulti_Risk_safe[",subject,"]")])
    UncertaintyMulti_risk[subject]=as.numeric(results$par[paste0("UncertaintyMulti_risk[",subject,"]")])
    UncertaintyMulti_safe[subject]=as.numeric(results$par[paste0("UncertaintyMulti_safe[",subject,"]")])
  }
  
  newSummarized<-rbind(cbind(subs,age,PubertyStage,alpha_add,
                             rho,UncertaintyMulti_Risk_risk,
                             UncertaintyMulti_Risk_safe,
                             UncertaintyMulti_risk,
                             UncertaintyMulti_safe)%>%as_tibble()%>%
                         pivot_longer(cols=c("alpha_add",
                                             "rho",
                                             "UncertaintyMulti_Risk_risk",
                                             "UncertaintyMulti_risk",
                                             "UncertaintyMulti_Risk_safe",
                                             "UncertaintyMulti_safe"),
                                      names_to="parameter",values_to="posteriorMean")%>%mutate(subs=as.numeric(subs),age=as.numeric(age))
  )
  return(newSummarized)
}

library(doParallel)

newSummarizedall<-foreach(i = 1:5,.combine = 'rbind')%dopar% {
  return(FitMarbles(MarbleData,Agegroups,i,ModelCompiled))
}

newSummarizedall<-FitMarbles(MarbleData,Agegroups,i,ModelCompiled)

newSummarizedall$posteriorMean<-as.numeric(newSummarizedall$posteriorMean)
