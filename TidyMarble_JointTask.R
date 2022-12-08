#end# This script loads the messy Marble OCU data and makes it kind of tidy so that i can work
#with it further. The goal is to take the sql data which saves every trial seperatly and make it wider.
# i try tidyverse

#Makes Tidy Data from the now even more messy Marbledata but my coding skills get a little better at least.
library('tidyverse')
#library('here')
# this will load the marble date
directory<-here()
setwd(directory)
names<-c("startTime","rt","stimulus1","stimulus2","key_press","old_order","riskyKey","new_order","changed_Order","red_marbles","blue_marbles",
         "OtherChoseRisk","ChooseRisk","valueGamble","probGamble","Social1Ind0","payoff","cumulatedPayoff","valueSure","trialID","PercentBlueEstimate",
         "HowSure","test_part","sex","age","subject","time_elapsed","ProximityCrit","subjectNumber")

keeps<-c("startTime","rt","key_press","riskyKey","red_marbles","blue_marbles",
         "OtherChoseRisk","ChooseRisk","valueGamble","probGamble","Social1Ind0","payoff","cumulatedPayoff","valueSure","trialID","PercentBlueEstimate",
         "HowSure","test_part","sex","age","subject","subjectNumber")

MarbleData<-read.csv("A_RawData/MarbleOCU_Webdev.csv")
colnames(MarbleData)<-names
MarbleDataTibble<-as_tibble(MarbleData)
MarbleDataTibble$startTime<-as.Date(MarbleDataTibble$startTime)
MarbleDataTibble$subject=as.numeric(as.character(MarbleDataTibble$subject))

MarbleDataTibble[MarbleDataTibble$test_part=="SoloChoiceMarble",]$test_part="SoloChoice"

MarbleDataTibble%>%mutate(
  subject=case_when(
    (is.na(subject))~as.numeric(as.character(subjectNumber)),
    TRUE~subject
  )
)->MarbleDataTibble

#this is actually really cool
MarbleDataTibble[keeps] %>%                                               # check what i want to keep
  dplyr::filter(test_part=="SoloChoice" | test_part=="Solotrial" | test_part!="NULL") %>%
  dplyr::filter(subject %in% c(1:188))%>%# select only the parts I use for Dataanalysis
  dplyr::filter(rt!="NULL")-> tbl1 
# check what i want to keep

# i start with 3 bc i look back three items
for(i in 3:length(tbl1$rt)){
if(tbl1$PercentBlueEstimate[i-1]!="NULL" & tbl1$test_part[i]=="SoloChoice"){
  tbl1$PercentBlueEstimate[i]=tbl1$PercentBlueEstimate[i-1]
  }else if (tbl1$PercentBlueEstimate[i-2]!="NULL" & tbl1$test_part[i]=="SoloChoice"){
    tbl1$PercentBlueEstimate[i]=tbl1$PercentBlueEstimate[i-2]
  }
}
tbl1 %>%
  mutate(HowSure=lag(HowSure)) %>%
  na.omit() %>%dplyr::filter(test_part=="SoloChoice")->TidyMarbleSolo


# the same happens here for Social Choices
MarbleDataTibble[keeps] %>%                                               # check what i want to keep
  dplyr::filter(test_part=="SocialChoice" | test_part=="SocialTrial" | test_part!="NULL") %>%
  dplyr::filter(as.numeric(subject) %in% c(1:188))%>%# select only the parts I use for Dataanalysis
  dplyr::filter(rt!="NULL")-> tbl2                                # check what i want to keep

Subs<-unique(tbl2$subject)
#okay. this here must happen because jspsych saves the data from different trials in seperate rows
#in the following i just impute the NULLS that are listed in the Choice Row for Certainty reports as well as Frequency estimates.
TidyMarbleSocial<-tibble()

for(i in 3:length(tbl2$rt)){
  if(tbl2$PercentBlueEstimate[i-1]!="NULL" & tbl1$test_part[i]=="SocialChoice"){
    tbl2$PercentBlueEstimate[i]=tbl2$PercentBlueEstimate[i-1]
  }else if (tbl2$PercentBlueEstimate[i-2]!="NULL" & tbl2$test_part[i]=="SocialChoice"){
    tbl2$PercentBlueEstimate[i]=tbl2$PercentBlueEstimate[i-2]
  }
}
tbl2 %>%
  mutate(HowSure=lag(HowSure)) %>%
  na.omit() %>%dplyr::filter(test_part=="SocialChoice")->TidyMarbleSocial

TidyMarble<-dplyr::bind_rows(TidyMarbleSolo,TidyMarbleSocial)
##### ok ok. but now we have to filter out the pilot and see how this looks like.
TidyMarble$startTime=as.Date(TidyMarble$startTime)

TidyMarble<-TidyMarble[TidyMarble$startTime >= "2018-06-06",]


# i need three new coulms: number of marbles shown
# percent blue marbles shown, percent red marbles shown.
#first, i add an vector of nulls
#i define the additional columns before and add them afterwards.

TidyMarble$PercentBlueShownAbs<-NaN
TidyMarble$PercentRedShownAbs<-NaN
TidyMarble$PercentBlueShownRel<-NaN
TidyMarble$PercentRedShownRel<-NaN
TidyMarble$TotalNShown<-NaN
TidyMarble$Agegroup<-NaN

for(i in 1:length(TidyMarble$rt)){
  
  str_split(TidyMarble$blue_marbles[i],pattern=",")%>%
    unlist()%>%
    as.numeric()%>%
    sum()/45->PercentBlueShownAbs
  
  str_split(TidyMarble$red_marbles[i],pattern=",")%>%
    unlist()%>%
    as.numeric()%>%
    sum()/45->PercentRedShownAbs
  
  TidyMarble$PercentBlueShownAbs[i]<-as.numeric(PercentBlueShownAbs)
  TidyMarble$PercentRedShownAbs[i]<-as.numeric(PercentRedShownAbs)
  TidyMarble$TotalNShown[i]<-(PercentRedShownAbs*45)+(PercentBlueShownAbs*45)
  
  str_split(TidyMarble$blue_marbles[i],pattern=",")%>%
    unlist()%>%
    as.numeric()%>%
    sum()/TidyMarble$TotalNShown[i]->PercentBlueShownRel
  
  str_split(TidyMarble$red_marbles[i],pattern=",")%>%
    unlist()%>%
    as.numeric()%>%
    sum()/TidyMarble$TotalNShown[i]->PercentRedShownRel
  
  TidyMarble$PercentBlueShownRel[i]<-as.numeric(PercentBlueShownRel)
  TidyMarble$PercentRedShownRel[i]<-as.numeric(PercentRedShownRel)
  
  if(as.numeric(as.character(TidyMarble$age[i]))<18){
    TidyMarble$Agegroup[i]=1#are they adolescents?
    if(as.numeric(as.character(TidyMarble$age[i]))<12){
      TidyMarble$Agegroup[i]=0#are they kids?
    }#enddoubleif.
  }else TidyMarble$Agegroup[i]=2#ore adults?
}

#finally; make index for Decicions from Description and expierience
TidyMarble$DFE1DFD0<-1
TidyMarble[TidyMarble$red_marbles==99,]$DFE1DFD0<-0
Webdev<-TidyMarble








######
######
###### HERE I LOAD THE VLAB DATA
######
######

names<-c("startTime","rt","stimulus1","stimulus2","key_press","old_order","riskyKey","new_order","changed_Order","red_marbles","blue_marbles",
         "OtherChoseRisk","ChooseRisk","valueGamble","probGamble","Social1Ind0","payoff","cumulatedPayoff","valueSure","trialID","PercentBlueEstimate",
         "HowSure","test_part","sex","age","subject","time_elapsed","ProximityCrit","subjectNumber")

keeps<-c("startTime","rt","key_press","riskyKey","red_marbles","blue_marbles",
         "OtherChoseRisk","ChooseRisk","valueGamble","probGamble","Social1Ind0","payoff","cumulatedPayoff","valueSure","trialID","PercentBlueEstimate",
         "HowSure","test_part","sex","age","subject","subjectNumber")

MarbleData<-read.csv2("A_RawData/MarbleOCU_vlabNew.csv")
colnames(MarbleData)<-names
MarbleDataTibble<-as_tibble(MarbleData)
MarbleDataTibble$startTime<-as.Date(MarbleDataTibble$startTime)

MarbleDataTibble$test_part=as.character(MarbleDataTibble$test_part)
MarbleDataTibble$subject=as.numeric(as.character(MarbleDataTibble$subject))

MarbleDataTibble[MarbleDataTibble$test_part=="SoloChoiceMarble",]$test_part="SoloChoice"

MarbleDataTibble%>%mutate(
  subject=case_when(
    (is.na(subject))~as.numeric(as.character(subjectNumber)),
    TRUE~subject
  )
)->MarbleDataTibble
#this is actually really cool
MarbleDataTibble[keeps] %>%                                               # check what i want to keep
  dplyr::filter(test_part=="SoloChoice" | test_part=="Solotrial" | test_part!="NULL") %>%
  dplyr::filter(subject %in% c(1:188))%>%# select only the parts I use for Dataanalysis
  dplyr::filter(rt!="NULL")-> tbl1 
# check what i want to keep
Subs<-unique(tbl1$subject)




for(i in 3:length(tbl1$rt)){
  if(tbl1$PercentBlueEstimate[i-1]!="NULL" & tbl1$test_part[i]=="SoloChoice"){
    tbl1$PercentBlueEstimate[i]=tbl1$PercentBlueEstimate[i-1]
  }else if (tbl1$PercentBlueEstimate[i-2]!="NULL" & tbl1$test_part[i]=="SoloChoice"){
    tbl1$PercentBlueEstimate[i]=tbl1$PercentBlueEstimate[i-2]
  }
}
tbl1 %>%
  mutate(HowSure=lag(HowSure)) %>%
  na.omit() %>%dplyr::filter(test_part=="SoloChoice")->TidyMarbleSolo


# the same happens here for Social Choices
MarbleDataTibble[keeps] %>%                                               # check what i want to keep
  dplyr::filter(test_part=="SocialChoice" | test_part=="SocialTrial" | test_part!="NULL") %>%
  dplyr::filter(as.numeric(subject) %in% c(1:188))%>%# select only the parts I use for Dataanalysis
  dplyr::filter(rt!="NULL")-> tbl2                                # check what i want to keep

Subs<-unique(tbl2$subject)
#okay. this here must happen because jspsych saves the data from different trials in seperate rows
#in the following i just impute the NULLS that are listed in the Choice Row for Certainty reports as well as Frequency estimates.
TidyMarbleSocial<-tibble()


for(i in 3:length(tbl2$rt)){
  if(tbl2$PercentBlueEstimate[i-1]!="NULL" & tbl1$test_part[i]=="SocialChoice"){
    tbl2$PercentBlueEstimate[i]=tbl2$PercentBlueEstimate[i-1]
  }else if (tbl2$PercentBlueEstimate[i-2]!="NULL" & tbl2$test_part[i]=="SocialChoice"){
    tbl2$PercentBlueEstimate[i]=tbl2$PercentBlueEstimate[i-2]
  }
}
tbl2 %>%
  mutate(HowSure=lag(HowSure)) %>%
  na.omit() %>%dplyr::filter(test_part=="SocialChoice")->TidyMarbleSocial

TidyMarble<-dplyr::bind_rows(TidyMarbleSolo,TidyMarbleSocial)

# only keep these ones that started after the pilot
DidIFindIt<-TidyMarble[TidyMarble$startTime >= "2018-06-06",]

# check if everything is correct
hist(DidIFindIt$subject,breaks=length(unique(DidIFindIt$subject)))
Subz<-unique(DidIFindIt$subject)
for (i in 1:length(unique(DidIFindIt$subject))){
  print(length(DidIFindIt[DidIFindIt$subject==Subz[i],]$subject))
}

TidyMarble<-TidyMarble[TidyMarble$startTime >= "2018-06-06",]


# i need three new coulms: number of marbles shown
# percent blue marbles shown, percent red marbles shown.
#first, i add an vector of nulls
#i define the additional columns before and add them afterwards.

TidyMarble$PercentBlueShownAbs<-NaN
TidyMarble$PercentRedShownAbs<-NaN
TidyMarble$PercentBlueShownRel<-NaN
TidyMarble$PercentRedShownRel<-NaN
TidyMarble$TotalNShown<-NaN
TidyMarble$Agegroup<-NaN

for(i in 1:length(TidyMarble$rt)){
  
  str_split(TidyMarble$blue_marbles[i],pattern=",")%>%
    unlist()%>%
    as.numeric()%>%
    sum()/45->PercentBlueShownAbs
  
  str_split(TidyMarble$red_marbles[i],pattern=",")%>%
    unlist()%>%
    as.numeric()%>%
    sum()/45->PercentRedShownAbs
  
  TidyMarble$PercentBlueShownAbs[i]<-as.numeric(PercentBlueShownAbs)
  TidyMarble$PercentRedShownAbs[i]<-as.numeric(PercentRedShownAbs)
  TidyMarble$TotalNShown[i]<-(PercentRedShownAbs*45)+(PercentBlueShownAbs*45)
  
  str_split(TidyMarble$blue_marbles[i],pattern=",")%>%
    unlist()%>%
    as.numeric()%>%
    sum()/TidyMarble$TotalNShown[i]->PercentBlueShownRel
  
  str_split(TidyMarble$red_marbles[i],pattern=",")%>%
    unlist()%>%
    as.numeric()%>%
    sum()/TidyMarble$TotalNShown[i]->PercentRedShownRel
  
  TidyMarble$PercentBlueShownRel[i]<-as.numeric(PercentBlueShownRel)
  TidyMarble$PercentRedShownRel[i]<-as.numeric(PercentRedShownRel)
  
  if(as.numeric(as.character(TidyMarble$age[i]))<18){
    TidyMarble$Agegroup[i]=1#are they adolescents?
    if(as.numeric(as.character(TidyMarble$age[i]))<12){
      TidyMarble$Agegroup[i]=0#are they kids?
    }#enddoubleif.
  }else TidyMarble$Agegroup[i]=2#ore adults?
}

#finally; make index for Decicions from Description and expierience
TidyMarble$DFE1DFD0<-1
TidyMarble[TidyMarble$red_marbles==99,]$DFE1DFD0<-0

vLab<-TidyMarble


#Wow that was a fucking mess!
TidyMarble<-rbind(Webdev,vLab)


Supposed<-c(1:188)
subs<-sort(as.numeric(as.character(unique(TidyMarble$subject))))
missing<-(Supposed %in% subs)
missingSubIdx<-Supposed[!missing]
TidyMarble$age<-as.numeric(as.character(TidyMarble$age))
TidyMarble<-TidyMarble%>%filter(age>8)%>%select( colnames(TidyMarble)[colnames(TidyMarble)!="subjectNumber" & colnames(TidyMarble)!="trialID"] )



trials<-rep(1,length(subs))
for(i in 1:length(subs)){
  trials[i]<-length(TidyMarble[TidyMarble$subject==subs[i],]$subject)
}
hist(trials) 

TooManyTrials<-subs[trials>144]# ok these are all kids. Something must have went really wrong here. Anyway. I only keep the ones, older than 

for (i in 1:length(TooManyTrials)){
  print(unique(TidyMarble[TidyMarble$subject==TooManyTrials[i],]$age))
}

temp<-TidyMarble[TidyMarble$subject %in% TooManyTrials & TidyMarble$age<15,]
TidyMarble<-TidyMarble%>%filter(!(subject %in% TooManyTrials))%>%rbind(temp)%>%arrange(subject)
notEnoughTrials<-subs[trials<143]# ok these are all kids. Something must have went really wrong here. Anyway. I only keep the ones, older than 
TidyMarble<-TidyMarble%>%filter(!(subject %in% notEnoughTrials))

hist(TidyMarble$subject,breaks=length(unique(TidyMarble$subject)))

recollect=(Supposed %in% unique(TidyMarble$subject))
missing<-Supposed[!recollect]

write.csv(TidyMarble,file="TidyMarble.csv")
#down here i build sth for Lucas so we can connect datasets.
demographics<-c("sex","age","subject")
demographicsTBL<-TidyMarble[demographics]
Lucas<-unique(demographicsTBL)
save(Lucas,file="Demographics.RData")
