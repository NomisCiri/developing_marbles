Developping\_Marbles
================
Simon
16/01/2019

-   [Agenda](#agenda)
-   [Marble Data](#marble-data)
-   [look at the subjects.](#look-at-the-subjects.)
-   [Lets have a first Look at the Data](#lets-have-a-first-look-at-the-data)
    -   [Who Wins](#who-wins)
    -   [Risky Choice](#risky-choice)
-   [Description Expierence Gap](#description-expierence-gap)
-   [Within subject change.](#within-subject-change.)
    -   [How Good were they at Estimating?](#how-good-were-they-at-estimating)
    -   [Who is most Confident?](#who-is-most-confident)
        -   [Is this substantial?](#is-this-substantial)
    -   [How does the Estimation Accuracy Relate to Confidence?](#how-does-the-estimation-accuracy-relate-to-confidence)
        -   [Inverse U?](#inverse-u)
-   [Models](#models)
-   [Individual parameters](#individual-parameters)
    -   [The Learning Model](#the-learning-model)
    -   [The Value Model](#the-value-model)
    -   [The Choice Model](#the-choice-model)
-   [Parameter Estimates](#parameter-estimates)
-   [Learning Scaling: p](#learning-scaling-p)
-   [Reward Sensitvity](#reward-sensitvity)
-   [Stochastizity?](#stochastizity)
-   [OCU Risk Decisions from Description](#ocu-risk-decisions-from-description)
-   [OCU Risk Decisions from Experience](#ocu-risk-decisions-from-experience)
-   [OCU Safe Decisions from Description](#ocu-safe-decisions-from-description)
-   [OCU Safe Decisions from Experience](#ocu-safe-decisions-from-experience)
-   [one plot](#one-plot)
-   [Social Influence and Learning?](#social-influence-and-learning)

Agenda
======

TODO: Posterior predictives. \* I added a first model \* Fit a model not on the estimates but on the ditibutions as seen by the participants. How does this change estimates for rho? They will go up. \* no age correlations in any of the Parameters, which is wierd, I probably did not do it right \* We are still waiting for 16 participants, But maybe we can already start thinking about this a little more \* Miniconference.

Marble Data
===========

In this document we have a look at the Data from the Marble Task. We are supposed to have 50 participants pre, 50 post and 50 adolescents who performed a social and solo version of the experiment. They were asked to decide between safe and sure gamble options in order to recieve bonus points. Sometimes the outcome probabilites were described to them as marbles in a jar. On other occasions the outcome probabilites had to be inferred by the participants. This was possible through seeing samples from an urn. Participans always saw 9 pieces of information. Then we asked them for an estimate about the proportion of marbles in that urn. Subsequently we asked how sure there are about this estimate. Only then they made a choice. In the Social trials we showed participants the choices of another subject who completed a similar task before [here is the paper](http://www.tandfonline.com/doi/full/10.1080/87565641.2016.1158265). This made it possible to choose an adviosr with a ralistic choice function. Advisors were selected to make risky choices 20% more than the subject before. The raw data was pretty messy, because it was collected on two servers and every trial has an own column but doesnt save all information. I used [this script](RawData/TidyMarble_JointTask.R) to tidy up.

look at the subjects.
=====================

For some reason this Histogram bins the first subjects together but this is the numebr of trials competed by every subject. 144 as it should be (4\*36). But generally, this looks satisfying and as if i did not screw anything up when coding, saving and making the SQL Data Tidy.

``` r
MarbleData<-read.csv("TidyMarbleNew.csv")
hist(MarbleData$subject,breaks=length(unique(MarbleData$subject))*4)
```

![](Analyze_files/figure-markdown_github/cars-1.png)

And here i reformat the stuff i need.

``` r
MarbleData$Agegroup=as.factor(MarbleData$Agegroup)
MarbleData$DFE1DFD0=as.factor(MarbleData$DFE1DFD0)
MarbleData$Social1Ind0=as.factor(MarbleData$Social1Ind0)
MarbleData$PercentBlueEstimate=round(as.numeric(MarbleData$PercentBlueEstimate))
MarbleData$PercentBlueShownRel=round(as.numeric(MarbleData$PercentBlueShownRel),2)
MarbleData$PercentBlueShownRel=as.factor(as.character(MarbleData$PercentBlueShownRel))
MarbleData$HowSure=as.numeric(as.character(MarbleData$HowSure))
```

    ## Warning: NAs introduced by coercion

``` r
MarbleData$subject=as.character(MarbleData$subject)
```

Lets have a first Look at the Data
==================================

Who Wins
--------

Thats the Most intersting thing of all. WHO GETS THE MOST BONUS POINTS? Lets check it.

![](Analyze_files/figure-markdown_github/unnamed-chunk-2-1.png)![](Analyze_files/figure-markdown_github/unnamed-chunk-2-2.png)

    ## Saving 7 x 5 in image

lol. Adolescents. This is going to be interesting.

Risky Choice
------------

So lets start to look at the overall proportion of risky desicions, by conditions. We can see (as preregistered) that Adolescent impact of Advice depends less on the Uncertainty Condition than we thought it would. There seems to be a linear Decrease in Risk Taking for Both Condiitions. In "Decisons From Expierience" subjects make less risky choices.

![](Analyze_files/figure-markdown_github/unnamed-chunk-3-1.png)

    ## Warning: Ignoring unknown aesthetics: y

![](Analyze_files/figure-markdown_github/unnamed-chunk-3-2.png)

Description Expierence Gap
==========================

IS REVERSED! By using representative samples, we squised the description expiereince gap.

``` r
MarbleData%>%filter(Social1Ind0==0)%>%ggplot(aes(y=ChooseRisk,x=probGamble,group=DFE1DFD0,shape=Social1Ind0,linetype=Social1Ind0))+stat_summary(mapping=aes(color=DFE1DFD0,linetype=Social1Ind0),geom="line",fun.y = mean, size=3)+
 stat_summary(geom="pointrange",fun.y=mean,fun.ymin =function(x) mean(x)-(sd(x)/sqrt(length(x))),fun.ymax=function(x) mean(x)+(sd(x)/sqrt(length(x))))+
  #scale_x_discrete(name="Agegroup",breaks=c(0,1,2),labels=c("Kids","Adolescents","Adults"))+
  scale_color_discrete(name="Uncertainty Condition",breaks=c(0,1),labels=c("Risk","Uncertainty"))+
  geom_hline(mapping=aes(yintercept=0.45),linetype="dotdash")+
  geom_hline(mapping=aes(yintercept=0.5),linetype="dotted",alpha=0.4)+
  geom_hline(mapping=aes(yintercept=0.4),linetype="dotted",alpha=0.4)+
  geom_hline(mapping=aes(yintercept=0.55),linetype="dotted",alpha=0.2)+
  geom_hline(mapping=aes(yintercept=0.35),linetype="dotted",alpha=0.2)+
  geom_hline(mapping=aes(yintercept=0.6),linetype="dotted",alpha=0.1)+
  geom_hline(mapping=aes(yintercept=0.3),linetype="dotted",alpha=0.1)+
  facet_grid(.~Agegroup
             #labeller = labeller(Social1Ind0 = labels)
             #  OtherChoseRisk=labels2
  )+
  #coord_cartesian(ylim=c(0.3,0.6))+
  scale_y_continuous(name="p Risky Choice")+
  ggtitle("Risky Choice")
```

![](Analyze_files/figure-markdown_github/unnamed-chunk-4-1.png)

Within subject change.
======================

We have a within subject design. So the appropriate thing to do here is to look at within subject change between solo and social but also risk and uncertainty. When you´re Uncertain what to do you should shift your behavior more when you observe social information. In the Uncertain condition there seems to be a linear development from shifting less towards more. In Risky Choice, adolescents seem to be most stubborn.

    ## Automatically converting the following non-factors to factors: subject

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

    ## Adding missing grouping variables: `subject`

    ## Automatically converting the following non-factors to factors: subject

    ## Adding missing grouping variables: `subject`

    ## Automatically converting the following non-factors to factors: subject

    ## Adding missing grouping variables: `subject`

    ## Automatically converting the following non-factors to factors: subject

![](Analyze_files/figure-markdown_github/unnamed-chunk-5-1.png)

How Good were they at Estimating?
---------------------------------

In the Decisions from Expierience trials we asked Pariticpants about what they thing the outcome Ditribution really was. So now see if there are age differnces in estimating these distributions. For this i treat the real distributions as factors.

``` r
MarbleData%>%filter(DFE1DFD0=="1"&PercentBlueEstimate!="NULL")%>%ggplot()+
  #stat_summary(geom="bar",fun.y = "mean",position="dodge")+
  stat_summary(aes(x=PercentBlueShownRel,y=PercentBlueEstimate,color=Agegroup),geom="pointrange",fun.y=mean,fun.ymin =function(x) mean(x)-(sd(x)/sqrt(length(x))),fun.ymax=function(x) mean(x)+(sd(x)/sqrt(length(x))),position = position_dodge(0.9))+
  
  stat_summary(aes(y=as.numeric(as.character(PercentBlueShownRel))*100,x=PercentBlueShownRel),geom="pointrange",position = position_dodge(0.9),shape=3,color="red")+
  scale_shape_manual(name="Distribution as Seen by Participant")+
  scale_color_viridis_d(name="Agegroup",breaks=c("0","1","2"),labels=c("Pre Adolescence","Adolescence","Young Adults"))+
  scale_x_discrete(name="Actual Distribution")+
  scale_y_continuous(name="Participants Estimate")+
  #coord_cartesian(ylim=c(30,70))+
  ggtitle("Estimation Accuracy")
```

    ## No summary function supplied, defaulting to `mean_se()

![](Analyze_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
ggsave(filename="X_Figures/Accuracy.pdf")
```

    ## Saving 10 x 5 in image
    ## No summary function supplied, defaulting to `mean_se()

Turns out they were accurate and captured the trends. While estimating similarly well, Children & Adolescents tended to be overoptimistic in their estimation. This effect looks smaller with higher proportions of blue marbles. Participants also were all Optimistic for low probabilites and Pessimistic for high ones. Which is interesting because it aligns with what we have seen above and could explain the reversal of the description expierence gap here.

Who is most Confident?
----------------------

``` r
MarbleData%>%filter(DFE1DFD0=="1"&PercentBlueEstimate!="NULL")%>%
  ggplot(aes(x=Agegroup,y=HowSure,alpha=Agegroup))+
  #stat_summary(geom="bar",fun.y="mean")+
  stat_summary(geom="pointrange",fun.y=mean,fun.ymin =function(x) mean(x)-1.96*(sd(x)/sqrt(length(x))),fun.ymax=function(x) mean(x)+1.96*(sd(x)/sqrt(length(x))),position = position_dodge(0.9),alpha=0.7)+
  #geom_jitter(aes(x=as.numeric(Agegroup),y=HowSure))+
  scale_x_discrete(name="Agegroup",breaks=c(0,1,2),labels=c("Kids","Adolescents","Adults"))+
  scale_y_continuous(name="Confidence Rating")+
  guides(alpha=F)+
  coord_cartesian(ylim=c(55,65))+
  ggtitle("Confidence Rating")
```

    ## Warning: Removed 1086 rows containing non-finite values (stat_summary).

![](Analyze_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
ggsave(filename="X_Figures/Confidence.pdf")
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 1086 rows containing non-finite values (stat_summary).

##### Is this substantial?

Probably not. Adolescents report to be least confident with their estimation. But if we look at the whole spectrum all age groups actually do seem fairly similar to one another.

``` r
MarbleData%>%filter(DFE1DFD0=="1"&PercentBlueEstimate!="NULL")%>%
  ggplot(aes(y=HowSure,x=Agegroup,group=Agegroup))+
  geom_jitter(aes(color=PercentBlueShownRel),width = 0.1)+
  # stat_summary(fun.data = mean_cl_boot,geom="pointrange",size=1,color="red")+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = TRUE,alpha=0.1,fill="black")+
  geom_boxplot(aes(x = as.numeric(Agegroup)+0.2, y = HowSure),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK", nudge_x = 1, nudge_y = 2) +
  scale_color_viridis_d(name="Observed outcome Probability")+
  scale_x_discrete(name="Agegroup",breaks=c(0,1,2),labels=c("Kids","Adolescents","Adults"))+
  #geom_hline(yintercept = 150,linetype="dotdash")+
  scale_y_continuous(name="Confidence Rating")+
  ggtitle("Mean Confidence per Subject")+facet_grid(~PercentBlueShownRel)
```

    ## Warning: Ignoring unknown parameters: nudge_x, nudge_y

    ## Warning: Removed 1086 rows containing non-finite values (stat_ydensity).

    ## Warning: Removed 1086 rows containing non-finite values (stat_boxplot).

    ## Warning: Removed 1086 rows containing missing values (geom_point).

![](Analyze_files/figure-markdown_github/unnamed-chunk-8-1.png)

How does the Estimation Accuracy Relate to Confidence?
------------------------------------------------------

I compute the difference between the Actual Number shown and the Participants estimate as their Squared Error.

``` r
labels <- c(
  "0" = "Kids",
  "1" = "Adolescents",
  "2" = "Adults"
)

MarbleData%>%filter(PercentBlueEstimate!="NULL")%>%mutate(
  Error=abs((as.numeric(PercentBlueEstimate) - as.numeric(as.character(PercentBlueShownRel))*100))
)%>%ggplot(aes(x=Agegroup,y=as.numeric(Error)))+
  #stat_summary(geom="bar",fun.y = "mean",position="dodge")+
  #stat_smooth(method="loess")+
  stat_summary(geom="pointrange",fun.y=mean,fun.ymin =function(x) mean(x)-(sd(x)/sqrt(length(x))),fun.ymax=function(x) mean(x)+(sd(x)/sqrt(length(x))),position = position_dodge(0.9))+
  #facet_grid(.~Agegroup,
  #           labeller = labeller(Agegroup = labels)
  #)+
  scale_x_discrete(name=" Agegroup")+
  scale_y_continuous(name="Estimation Error")+
  ggtitle(" Estimation Error")#+coord_cartesian(ylim=c(10,30))
```

![](Analyze_files/figure-markdown_github/unnamed-chunk-9-1.png)

I cant make much of this inverted U shape. Does this really mean, that when they are super wrong, then they are most confident? This bias seems to be least present in Kids.

### Inverse U?

Maybe the confidence has got more to do with the particiapnts estimate per se than with their accuracy. Is this enough to say that adolescents are more calibrated?

``` r
labels <- c(
  "0" = "Kids",
  "1" = "Adolescents",
  "2" = "Adults"
)


MarbleData%>%filter(DFE1DFD0=="1"&PercentBlueEstimate!="NULL")%>%mutate(
  delta=abs(as.numeric(as.character(PercentBlueShownRel))*100-as.numeric(PercentBlueEstimate)) 
)%>%ggplot(aes(x=delta,y=(HowSure),color=Agegroup,group=Agegroup))+
  #geom_point()+
  stat_summary(geom="point",fun.y = "mean",position="dodge",alpha=0.3)+
  stat_smooth(method="lm")+
  scale_color_viridis_d(name="Agegroup",breaks=c("0","1","2"),labels=c("Pre Adol","Adol","Young Adult"),option = "D")+
  #stat_summary(geom="pointrange",fun.y=mean,fun.ymin =function(x) mean(x)-(sd(x)/sqrt(length(x))),fun.ymax=function(x) mean(x)+(sd(x)/sqrt(length(x))),position = position_dodge(0.9))+
  #facet_grid(.~Agegroup,
 #            labeller = labeller(Agegroup = labels)
 # )+
  scale_x_continuous(name="Estimation Error")+
  scale_y_continuous(name="Confidence")
```

    ## Warning: Removed 1086 rows containing non-finite values (stat_summary).

    ## Warning: Removed 1086 rows containing non-finite values (stat_smooth).

    ## Warning: Width not defined. Set with `position_dodge(width = ?)`

![](Analyze_files/figure-markdown_github/unnamed-chunk-10-1.png)

Models
======

I outsourced Modelfitting to the tardis Here i fit a model that assumes different hyperdistributions for age groups.

We can look at it indivdually later on but i think this can provide a good first look. First i need to concatanete the subject parameters. For this I need to retrieve the real Values at some point.

``` r
ModelParamsFull=list()
subCount=1
for (j in 1:length(unique(MarbleData$Agegroup))){
  
  fitSep<-readRDS(paste0("C_ModelFits/Marbles_Model_4Age_",j,".rds"))
  Parameters<-(rstan::extract(fitSep))
  library(tidyverse)
  
  #Bin_Update_Data<-Bin_Update_Data[Bin_Update_Data$typeRA=="1",]# keep only The Risk trails.
  Bin_Update_Data<-MarbleData%>%arrange(Agegroup)# i order it first so i can make sure that 
  Agegroups<-unique(Bin_Update_Data$Age.bins)# for indexing my agegroups.
  
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
  # now check how many participants we have.
  Bin_Update_Data$subject<-as.numeric(Bin_Update_Data$subject)
  #change colname of subject into subjID
  numSubjs<-length(unique(Bin_Update_Data$subject))#Total Number of Subs
  subjList <- unique(Bin_Update_Data$subject)######
  ageList<-Bin_Update_Data%>%group_by(subject)%>%dplyr::summarize(
    age=mean(age)
  )%>%select(age)%>%ungroup()
  
  ageList=as.vector(ageList$age)
  
  
  confList<-Bin_Update_Data%>%filter(HowSure!=102)%>%group_by(subject)%>%dplyr::summarise(
    Confidence=mean(HowSure)
  )
  confList=as.vector(confList$Confidence)
  
  
  datalist = list()
  
  for(i in 1:dim(Parameters$alpha_add)[2]){
    df=data.frame(
      alphaAdd=as.vector(Parameters$alpha_add[,i]),
    #  betaAdd=as.vector(Parameters$beta_add[,i]),
      rho=as.vector(Parameters$rho[,i]),
      tau=as.vector(Parameters$tau[,i]),
      ocusafeRisk=as.vector(Parameters$ocu_safe_Risk[,i]),
      ocusafeUncertainty=as.vector(Parameters$ocu_safe_Uncertainty[,i]),
      ocuriskRisk=as.vector(Parameters$ocu_risk_Risk[,i]),
      ocuriskUncertainty=as.vector(Parameters$ocu_risk_Uncertainty[,i]),
      subject=subjList[subCount],
      age=ageList[subCount],# Here i need to get some kind of dictionary.
      conf=confList[subCount]
    )
    subCount=subCount+1
    datalist[[i]] <- df 
  }
  ModelParamsFull[[j]] <- do.call(rbind, datalist)
}
big_data<-do.call(rbind, ModelParamsFull)
```

In what follows i look at the Whole parameter Distributions.

Individual parameters
=====================

After i collect the fits, we can actually nicely concatenate them.

``` r
PosteriorMean<-big_data%>%dplyr::group_by(subject,age,conf)%>%dplyr::summarise(
  MeanAlphaAdd=mean(alphaAdd),
 # MeanBetaAdd=mean(betaAdd),
  MeanRho=mean(rho),
  meanTau=mean(tau),
  meanOCURiskR=mean(ocuriskRisk),
  meanOCURiskUnc=mean(ocuriskUncertainty),
  meanOCUSafeR=mean(ocusafeRisk),
  meanOCUSafeUnc=mean(ocusafeUncertainty)
)%>%dplyr::mutate(
  #MeanBoth=(((MeanAlphaAdd+MeanBetaAdd)/2)),
  TotalInfluenceUnc=abs(meanOCUSafeUnc+meanOCURiskUnc)/2,
  TotalInfluenceRisk=abs(meanOCUSafeR+meanOCURiskR)/2,
  #MeanBoth=(9^((MeanAlphaAdd+MeanBetaAdd)/2)),
  Group=case_when(
    age<13~"1",
    (age>12 & age <19)~"2",
    age>=19~"3"
    #TRUE
  )
)
```

The Learning Model
------------------

In each uncertain trial, Subjects see 9 pieces of evidence. These can be either red meaning a loss and blue meaning a win. The number of observed losses is denoted by *β*, the number of observed wins is denoted by *β*. In uncertain Trails, we assume that subjects update their beliefs about the outcome distribution in a quasi Bayesian manner.

*p* ∼ *B*(*α*<sup>*p*</sup>, *β*<sup>*p*</sup>)
 We allow Failures and Sucesses to be subjectively over or underrepresented by raising them to the free parameter of p. If the
*p*
 value is smaller than 1 this means that as comparaed to bayes optimal; subjects put lower weight on the sequentially presented information.

The Value Model
---------------

The Utility model is an expected utility model. Here we use the probability estimate which we either obtained by sampling or is described to the participant and multiply it with the subjective utility of the option. Participants could choose between a safe option which always had the Value of 5 an A risky option which can be computed as the probability of occurance times the utility.

*E**U* = *p* \* *V*<sup>*ρ*</sup>

The Choice Model
----------------

The choice model takes the negative difference of these utilites *Δ**U**t* and translates it into a choice probability by the sigmoid transformation which scales the difference between utils up and down.

$$
p\_{ChooseRisk}=\\frac{1}{1+e^{- (EU\_{safe}-EU\_{risk})\*\\tau}}
$$

Parameter Estimates
===================

In what follows i Show yu the mean of the parameter Estiamtes. This all makes sense. In a nutshell. Younger participants underweight more -&gt; more uncertainty. Younger participants follow advice stronger, not only risky but also safe advice. There is no adolecent peak but always seemingly linear effects.

Learning Scaling: p
===================

A lot of values are close to 0. Maybe we should talk about the boundaries for this parameter. Even if the exponent is negative, it will only approximate 0, but never get negative in itself so mathematically it would still make sense but we would allow subjects to be EVEN MORE UNCERTAIN. I mean, thats reasonable?

``` r
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

``` r
#lm(MeanAlphaAdd~age,data=PosteriorMean)

ggplot(PosteriorMean,aes(x=Group,y=MeanAlphaAdd))+
  stat_summary(fun.data = "mean_se")+scale_x_discrete(name="Agegroup",breaks=c("1","2","3"),labels=c("Age 10-12","Age 13-18","Age >18"))+  geom_jitter(aes(color=Group))+
  scale_color_viridis_d(name="Agegroup")+
  #stat_summary(fun.data = "mean_se")+scale_x_discrete(name="Agegroup",breaks=c("1","2","3"),labels=c("Age 10-12","Age 13-18","Age >18"))+
  geom_hline(aes(yintercept=1),linetype="dotdash",alpha=0.2)->learn
print(learn)
```

![](Analyze_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
ggsave(file='X_Figures/Learning.pdf')
```

    ## Saving 10 x 10 in image

Reward Sensitvity
=================

``` r
#lm(meanTau~poly(age,2),data=PosteriorMean)

ggplot(PosteriorMean,aes(x=Group,y=MeanRho))+
  geom_jitter(aes(color=Group),alpha=0.3)+
  scale_color_viridis_d(name="Agegroup")+
  stat_summary(fun.data = "mean_se")+scale_x_discrete(name="Agegroup",breaks=c("1","2","3"),labels=c("Age 10-12","Age 13-18","Age >18"))+
  geom_hline(aes(yintercept=1),linetype="dotdash",alpha=0.2)
```

![](Analyze_files/figure-markdown_github/unnamed-chunk-14-1.png)

Stochastizity?
==============

``` r
#lm(meanTau~poly(age,2),data=PosteriorMean)

ggplot(PosteriorMean,aes(x=Group,y=meanTau))+
  geom_jitter(aes(color=Group),alpha=0.3)+
  scale_color_viridis_d(name="Agegroup")+
  stat_summary(fun.data = "mean_se")+scale_x_discrete(name="Agegroup",breaks=c("1","2","3"),labels=c("Age 10-12","Age 13-18","Age >18"))
```

![](Analyze_files/figure-markdown_github/unnamed-chunk-15-1.png)

OCU Risk Decisions from Description
===================================

``` r
#lm(meanOcuR~poly(age,2),data=PosteriorMean)

ggplot(PosteriorMean,aes(x=as.numeric(age),meanOCURiskR))+
  geom_jitter(aes(color=as.numeric(age)),alpha=0.3)+
  stat_summary(fun.data = "mean_se")+
 # scale_x_continuous(name="Agegroup",breaks=c("1","2","3"),labels=c("Age 10-12","Age 13-18","Age >18"))+
  #scale_color_viridis_d(name="Agegroup")+
  geom_smooth(method="lm")+
  geom_hline(aes(yintercept=0),linetype="dotdash",alpha=0.2)
```

![](Analyze_files/figure-markdown_github/unnamed-chunk-16-1.png)

OCU Risk Decisions from Experience
==================================

``` r
#lm(meanOcuR~poly(age,2),data=PosteriorMean)
ggplot(PosteriorMean,aes(x=age,meanOCURiskUnc))+
  geom_jitter(aes(color=age),alpha=0.3)+
  stat_summary(fun.data = "mean_se")+#scale_x_discrete(name="Agegroup",breaks=c("1","2","3"),labels=c("Age 10-12","Age 13-18","Age >18"))+
  #scale_color_viridis_d(name="Agegroup")+
  geom_smooth(method="lm")+geom_hline(aes(yintercept=0),linetype="dotdash",alpha=0.2)
```

![](Analyze_files/figure-markdown_github/unnamed-chunk-17-1.png)

OCU Safe Decisions from Description
===================================

``` r
#lm(meanOcuR~poly(age,2),data=PosteriorMean)

ggplot(PosteriorMean,aes(x=Group,meanOCUSafeR))+
  geom_jitter(aes(color=Group),alpha=0.3)+
  stat_summary(fun.data = "mean_se")+scale_x_discrete(name="Agegroup",breaks=c("1","2","3"),labels=c("Age 10-12","Age 13-18","Age >18"))+
  scale_color_viridis_d(name="Agegroup")+
  geom_smooth(method="lm")+geom_hline(aes(yintercept=0),linetype="dotdash",alpha=0.2)
```

![](Analyze_files/figure-markdown_github/unnamed-chunk-18-1.png)

OCU Safe Decisions from Experience
==================================

``` r
#lm(meanOcuR~poly(age,2),data=PosteriorMean)

ggplot(PosteriorMean,aes(x=Group,meanOCUSafeUnc))+
  geom_jitter(aes(color=Group),alpha=0.3)+
  stat_summary(fun.data = "mean_se")+scale_x_discrete(name="Agegroup",breaks=c("1","2","3"),labels=c("Age 10-12","Age 13-18","Age >18"))+
  scale_color_viridis_d(name="Agegroup")+
  geom_smooth(method="lm")+geom_hline(aes(yintercept=0),linetype="dotdash",alpha=0.2)
```

![](Analyze_files/figure-markdown_github/unnamed-chunk-19-1.png)

one plot
========

``` r
PosteriorMean%>%gather(key="RiskUnc",value="measurement", meanOCURiskR,meanOCURiskUnc)%>%
  ggplot(aes(y=measurement,x=RiskUnc,color=Group,group=Group))+
  geom_point(aes(group=Group),width=0.1,position=position_dodge(0.9),alpha=0.1)+
  stat_summary(fun.data = mean_cl_boot,position=position_dodge(0.9),color="black")+
  #scale_color_viridis_d(name="Agegroup")+
  geom_hline(yintercept = 0, linetype="dotdash", alpha=0.3)+
  ylab("Socail Influence")+
  scale_color_viridis_d(name="Agegroup",breaks=c("1","2","3"), labels=c("Pre-Adolescence","Adolescence","Post-Adoelscence"))+
  scale_x_discrete(name="Risk & Uncertainty", breaks=c("meanOCURiskR","meanOCURiskUnc"),labels=c("Risk","Uncertainty"))+
  ggtitle("Social Influence on Risky Decisions")->Risky
```

    ## Warning: Ignoring unknown parameters: width

``` r
  plot(Risky)
```

![](Analyze_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
  ggsave(file='X_Figures/OCURisk.pdf')
```

    ## Saving 10 x 5 in image

``` r
PosteriorMean%>%gather(key="RiskUnc",value="measurement", meanOCUSafeR,meanOCUSafeUnc)%>%
  ggplot(aes(y=measurement,x=RiskUnc,color=Group,group=Group))+
  geom_point(aes(group=Group),width=0.1,position=position_dodge(0.9),alpha=0.1)+
  stat_summary(fun.data = mean_cl_boot,position=position_dodge(0.9),color="black")+
  geom_hline(yintercept = 0, linetype="dotdash", alpha=0.3)+
  ylab("Socail Influence")+
  scale_x_discrete(name="Risk & Uncertainty", breaks=c("meanOCUSafeR","meanOCUSafeUnc"),labels=c("Risk","Uncertainty"))+
  scale_color_viridis_d(name="Agegroup",breaks=c("1","2","3"), labels=c("Pre-Adolescence","Adolescence","Post-Adoelscence"))+
  ggtitle("Social Influence on Safe Decisions")->Safe
```

    ## Warning: Ignoring unknown parameters: width

``` r
  plot(Safe)
```

![](Analyze_files/figure-markdown_github/unnamed-chunk-20-2.png)

``` r
  ggsave(file='X_Figures/OCUSafe.pdf')
```

    ## Saving 10 x 5 in image

``` r
#cowplot::plot_grid(Risky,Safe)
```

Social Influence and Learning?
==============================

The smaller alpha, the more uncertainty, the more social influence.

``` r
#lm(meanOcuR~poly(age,2),data=PosteriorMean)

ggplot(PosteriorMean,aes(x=TotalInfluenceUnc,y = MeanAlphaAdd))+
  geom_point(aes(color=Group),alpha=0.3)+
  stat_summary(fun.data = "mean_se")+#scale_x_continuous(name="SocialInfluence Uncertainty",breaks=c("1","2","3"),labels=c("Age 10-12","Age 13-18","Age >18"))+
  scale_color_viridis_d(name="Agegroup")+
  geom_smooth(method="lm")+geom_hline(aes(yintercept=1),linetype="dotdash",alpha=0.2)+
  ggtitle("Social Influence And Uncertainty")
```

    ## Warning: Removed 134 rows containing missing values (geom_pointrange).

![](Analyze_files/figure-markdown_github/unnamed-chunk-21-1.png)
