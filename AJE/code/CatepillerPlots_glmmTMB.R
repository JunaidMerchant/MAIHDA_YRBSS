rm(list = ls())

library(ggeffects)   # predictions and significance testing
library(insight)     # extracting random effects variances
library(datawizard)  # data wrangling and preparation
library(parameters)  # model summaries
library(performance) # model fit indices, ICC
library(glmmTMB)     # multilevel modelling
library(marginaleffects)
library(ggplot2)
library(Amelia)
library(gtsummary)
library(gt)
library(writexl)
library(flextable)
library(tidyr)
library(forcats)
library(ggtext)
library(sandwich)
library(emmeans)
library(pglm)
library(pdR)
library(lme4)
library(merTools)
library(labelled)
library(sjPlot)
library(Metrics)
library(tidyverse)

# load data and remove unnecessary variables
load("C:/Users/jmerch/Desktop/LGBTQ/MAIHDA/StataResults_May1/RnR/ReDo/suicide_MAIHDA_modeldata.RData")
rm(lastFuncGrad,lastFuncParam,i, OutDir,fe_m_2,fe_m_4,predictions1,Data)

# define out dir
OutDir="C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\RnR\\RnRround2\\"

# clean tmpData to only needed variables
tmpData=tmpData[,c("suicide","prepost2020", "gender_char", "sexorient_char", "race_char","sitecode","agenum")]

# create stratum variables
tmpData$stratum40=paste0(tmpData$gender_char,"-",tmpData$sexorient_char,"-",tmpData$race_char)
tmpData$stratum40 <- as.factor(tmpData$stratum40)

tmpData$stratum80=paste0(tmpData$prepost2020,"-",tmpData$gender_char,"-",tmpData$sexorient_char,"-",tmpData$race_char)
tmpData$stratum80 <- as.factor(tmpData$stratum80)

# Generate a new variabe which records stratum size
tmpData <- tmpData %>%
  group_by(stratum40) %>%
  mutate(strata40N = n())

tmpData <- tmpData %>%
  group_by(stratum80) %>%
  mutate(strata80N = n())




stratum80_level <- tmpData %>%
  group_by(stratum80,strata80N,prepost2020,gender_char,sexorient_char,race_char) %>%
  summarise(suicide=mean(suicide)*100)

stratum80_level <- stratum80_level[order(stratum80_level$suicide),]
stratum80_level$meanStratumRank=seq(1,80)

gc()


################################################################################
# predict m5 fixed effects
m5_predictions_FE = predict_response(m_5,c("prepost2020","race_char","sexorient_char","gender_char"),type = "fixed",interval="confidence")

m5predFeRe=m5_predictions_RE[,c(2,3,4,5,1,6,7,8)]

names(m5predFeRe)=c("PredRe","PredReSe","PredReLo","PredReHi","year","race","sexualorientation","gender")

m5predFeRe=cbind(m5_predictions_FE[,2:5],m5predFeRe)
names(m5predFeRe)[1:4]=c("PredFe","PredFeSe","PredFeLo","PredFeHi")

# create stratum variables
# m5predFeRe$stratum40=paste0(m5predFeRe$gender,"-",m5predFeRe$sexualorientation,"-",m5predFeRe$race)
# m5predFeRe$stratum40 <- as.factor(m5predFeRe$stratum40)

m5predFeRe$stratum80=paste0(m5predFeRe$year,"-",m5predFeRe$gender,"-",m5predFeRe$sexualorientation,"-",m5predFeRe$race)
m5predFeRe$stratum80 <- as.factor(m5predFeRe$stratum80)

# get predicted values on logit scale
for (x in 1:8){
  
  m5predFeRe[c(paste0(names(m5predFeRe)[x],"Logit"))]=logit(m5predFeRe[names(m5predFeRe)[x]])
  
}





# clean strata vars and remake
m5predFeRe$sexualorientation=gsub("Heterosexual","Hetero",m5predFeRe$sexualorientation)
m5predFeRe$sexualorientation=gsub("Othersexual","OtherQSexOrient",m5predFeRe$sexualorientation)
m5predFeRe$gender=gsub("Female","Girl",m5predFeRe$gender)
m5predFeRe$gender=gsub("Male","Boy",m5predFeRe$gender)


m5predFeRe$stratum1=paste0(m5predFeRe$race,"-",m5predFeRe$sexualorientation,"-",m5predFeRe$gender)
m5predFeRe$stratum=paste0(m5predFeRe$year,": ",m5predFeRe$race,"-",m5predFeRe$sexualorientation,"-",m5predFeRe$gender)

# m5predFeRe$stratum=gsub("2020-","2020:",m5predFeRe$stratum)
m5predFeRe$stratum=gsub("Homosexual-Girl","Lesbian-Girl",m5predFeRe$stratum)
m5predFeRe$stratum1=gsub("Homosexual-Girl","Lesbian-Girl",m5predFeRe$stratum1)
m5predFeRe$stratum=gsub("Homosexual-Boy","Gay-Boy",m5predFeRe$stratum)
m5predFeRe$stratum1=gsub("Homosexual-Boy","Gay-Boy",m5predFeRe$stratum1)


# multiply predicted probabilities to get percent
m5predFeRe[,1:8]=m5predFeRe[,1:8]*100



m5predFeRe=m5predFeRe[order(m5predFeRe$PredFe),]
m5predFeRe$PredFeRank=seq(1,80)
m5predFeRe$PredFeRank2=as.character(m5predFeRe$PredFeRank)


m5predFeRe=m5predFeRe[order(m5predFeRe$PredRe),]
m5predFeRe$PredReRank=seq(1,80)
m5predFeRe$PredReRank2=as.character(m5predFeRe$PredReRank)


MeanRE=mean(m5predFeRe$PredRe)
MeanFE=mean(m5predFeRe$PredFe)

for (x in 1:80){
  
  
  if ( (m5predFeRe$PredFeLo[x]>MeanFE & m5predFeRe$PredFeHi[x]>MeanFE) | (m5predFeRe$PredFeLo[x]<MeanFE & m5predFeRe$PredFeHi[x]<MeanFE) ){
    
    m5predFeRe$PredFeRank2[x]=paste0(m5predFeRe$PredFeRank2[x],".",m5predFeRe$stratum[x])
    
  }
  
  
  if ( (m5predFeRe$PredReLo[x]>MeanRE & m5predFeRe$PredReHi[x]>MeanRE) | (m5predFeRe$PredReLo[x]<MeanRE & m5predFeRe$PredReHi[x]<MeanRE) ){
    
    m5predFeRe$PredReRank2[x]=paste0(m5predFeRe$PredReRank2[x],".",m5predFeRe$stratum[x])
    
  }
  
}
  


m5predFeRe$PredReRank2=gsub("-2020","",m5predFeRe$PredReRank2)
m5predFeRe$PredReRank2=gsub("MultiOtherRace","MORace",m5predFeRe$PredReRank2)
m5predFeRe$PredReRank2=gsub("OtherQSexOrient","OQSex",m5predFeRe$PredReRank2)
m5predFeRe$PredReRank2=gsub("Bisexual","BiSex",m5predFeRe$PredReRank2)
m5predFeRe$PredReRank2=gsub("Lesbian","Lesb",m5predFeRe$PredReRank2)
m5predFeRe$PredReRank2=gsub("Hispanic","Hisp",m5predFeRe$PredReRank2)

m5predFeRe$PredFeRank2=gsub("-2020","",m5predFeRe$PredFeRank2)
m5predFeRe$PredFeRank2=gsub("MultiOtherRace","MORace",m5predFeRe$PredFeRank2)
m5predFeRe$PredFeRank2=gsub("OtherQSexOrient","OQSex",m5predFeRe$PredFeRank2)
m5predFeRe$PredFeRank2=gsub("Bisexual","BiSex",m5predFeRe$PredFeRank2)
m5predFeRe$PredFeRank2=gsub("Lesbian","Lesb",m5predFeRe$PredFeRank2)
m5predFeRe$PredFeRank2=gsub("Hispanic","Hisp",m5predFeRe$PredFeRank2)



m5predFeRe=m5predFeRe[order(m5predFeRe$PredFe),]
m5predFeRe$PredFeRank2=factor(m5predFeRe$PredFeRank2, 
                              labels=as.character(m5predFeRe$PredFeRank2),
                              levels=as.character(m5predFeRe$PredFeRank2))

m5predFeRe=m5predFeRe[order(m5predFeRe$PredRe),]
m5predFeRe$PredReRank2=factor(m5predFeRe$PredReRank2, 
                              labels=as.character(m5predFeRe$PredReRank2),
                              levels=as.character(m5predFeRe$PredReRank2))









# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(m5predFeRe, aes(x=PredReRank2, y=PredRe)) + theme_bw() +
  geom_hline(yintercept=MeanRE, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=PredReLo, ymax=PredReHi)) + 
  # scale_x_discrete("Ranked Stratum") +
  theme(axis.text.x = element_text(size = 8,face = "bold",angle = 90, vjust = .5, hjust=1)) +
  xlab("") +
  ylab("Predicted suicidal ideation % Random Effects")

ggsave(filename = "CatepillarPlot_M5_RE_PredProb.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(m5predFeRe, aes(x=PredFeRank2, y=PredFe)) + theme_bw() +
  geom_hline(yintercept=MeanFE, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=PredFeLo, ymax=PredFeHi)) + 
  # scale_x_discrete("Ranked Stratum") +
  theme(axis.text.x = element_text(size = 8,face = "bold",angle = 90, vjust = .5, hjust=1)) +
  xlab("") +
  ylab("Predicted suicidal ideation % Fixed Effects")

ggsave(filename = "CatepillarPlot_M5_FE_PredProb.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")



################################################################################




# create simulate file
stratumsim <- rbind(m5predFeRe, 
                    m5predFeRe[rep(1:nrow(m5predFeRe),999),])

stratumsim$m5predBetaSe <- (stratumsim$PredReLogit - stratumsim$PredReLoLogit)/1.96

# specify initial value of the random-number seed (for replication purposes)
set.seed(354612)

stratumsim$m5probRESim <- 100*invlogit(stratumsim$PredReLogit + 
                                         rnorm(80000, mean=0, sd=stratumsim$m5predBetaSe))

stratumsim$m5probFESim <- 100*invlogit(stratumsim$PredFeLogit)

stratumsim$m5SimDiff <- stratumsim$m5probRESim - stratumsim$m5probFESim

# sort the data by strata
stratumsim <- stratumsim[order(stratumsim$stratum),]




stratumsim2 <- stratumsim %>%
  group_by(stratum) %>%
  summarise(mean=mean(m5SimDiff), std=sd(m5SimDiff)) %>%
  mutate(predStratumrank=rank(mean)) %>%
  mutate(hi=(mean + 1.96*std)) %>%
  mutate(lo=(mean - 1.96*std))

stratumsim3 <- stratumsim %>%
  group_by(stratum) %>%
  summarise(meanRE=mean(m5probRESim), stdRE=sd(m5probRESim),
            meanFE=mean(m5probFESim), stdFE=sd(m5probFESim),
            meanDiff=mean(m5SimDiff), stdDiff=sd(m5SimDiff),) %>%
  mutate(ReRank=rank(meanRE)) %>%
  mutate(meanREhi=(meanRE + 1.96*stdRE)) %>%
  mutate(meanRElo=(meanRE - 1.96*stdRE)) %>%
  mutate(FeRank=rank(meanFE)) %>%
  mutate(meanFEhi=(meanFE + 1.96*stdFE)) %>%
  mutate(meanFElo=(meanFE - 1.96*stdFE)) %>%
  mutate(DiffRank=rank(meanDiff)) %>%
  mutate(meanDiffhi=(meanDiff + 1.96*stdDiff)) %>%
  mutate(meanDifflo=(meanDiff - 1.96*stdDiff))



stratumsim2 <- stratumsim2[order(stratumsim2$predStratumrank),]


stratumsim2$xlab=as.character(stratumsim2$predStratumrank)


for (x in 1:length(stratumsim2$predStratumrank)){
  

  
  if ( (stratumsim2$hi[x]>0 & stratumsim2$lo[x]>0) | (stratumsim2$hi[x]<0 & stratumsim2$lo[x]<0) ){
    
    stratumsim2$xlab[x]=paste0(as.character(stratumsim2$predStratumrank[x]),".",as.character(stratumsim2$stratum[x]))
  }
  
}



stratumsim2$xlab=gsub("-2020","",stratumsim2$xlab)
stratumsim2$xlab=gsub("MultiOtherRace","MORace",stratumsim2$xlab)
stratumsim2$xlab=gsub("OtherQSexOrient","OQSex",stratumsim2$xlab)
stratumsim2$xlab=gsub("Bisexual","BiSex",stratumsim2$xlab)
stratumsim2$xlab=gsub("Lesbian","Lesb",stratumsim2$xlab)
stratumsim2$xlab=gsub("Hispanic","Hisp",stratumsim2$xlab)


stratumsim2$xlab=factor(stratumsim2$xlab,labels= as.character(stratumsim2$xlab),levels= as.character(stratumsim2$xlab))


print(sum(grepl(".P",stratumsim2$xlab)))
print(sum(grepl("-",stratumsimPre$xlab)))
print(sum(grepl("-",stratumsimPost$xlab)))


# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(stratumsim2, aes(x=xlab, y=mean)) + theme_bw() +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=lo, ymax=hi)) + 
  scale_x_discrete("",labels= xlab) +
  theme(axis.text.x = element_text(size = 8, face = "bold",angle = 90, vjust = .5, hjust=1)) +
  xlab("") +
  ylab("Random-Fixed Effect Difference \n (interactions)")

ggsave(filename = "CatepillarPlot_M5_FeReDiff.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(stratumsimPre, aes(x=xlab, y=mean)) + theme_bw() +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=lo, ymax=hi)) + 
  scale_x_discrete("Stratum Rank",labels= xlab) +
  theme(axis.text.x = element_text(face = "bold",angle = 90, vjust = .5, hjust=1)) +
  xlab("Stratum Rank") +
  ylab("Difference in predicted suicidal ideation % due to interactions \n Full Model")

ggsave(filename = "CatepillarPlot_M5_Pre2020_FeReDiff.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")


# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(stratumsimPost, aes(x=xlab, y=mean)) + theme_bw() +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=lo, ymax=hi)) + 
  scale_x_discrete("Stratum Rank",labels= xlab) +
  theme(axis.text.x = element_text(face = "bold",angle = 90, vjust = .5, hjust=1)) +
  xlab("Stratum Rank") +
  ylab("Difference in predicted suicidal ideation % due to interactions \n Full Model")

ggsave(filename = "CatepillarPlot_M5_Post2020_FeReDiff.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")


stratum80_level=merge(stratum80_level,m5predFeRe,by="stratum80")
stratum80_level=merge(stratum80_level,stratumsim2,by="stratum")
names(stratum80_level)[32:37]=c("ReFePredDiff", "ReFePredDiffStd","ReFePredDiffStratumrank","ReFePredDiffHi","ReFePredDiffLo" ,"ReFePredDiffXlab"  )

write.csv(stratum80_level,file=paste0(OutDir,"Stratum80Level_M5_StratumMeansAndSimPredProb.csv"),row.names = FALSE)



# m5_predict_simulate=predict_response(m_5,c("prepost2020","race_char","sexorient_char","gender_char"),type = "simulate",interval="confidence")
m5_predict_stat=predict(m_5,tmpData[c("prepost2020","race_char","sexorient_char","gender_char")],
                        type="response",se.fit=TRUE)



stratum80_level=stratum80_level[order(stratum80_level$meanStratumRank),]
stratum80_level$Stratum80Rank=paste0(as.character(stratum80_level$meanStratumRank),".",stratum80_level$stratum)
stratum80_level$Stratum80Rank=factor(stratum80_level$Stratum80Rank)
stratum80_level$suicide_std=invlogit(stratum80_level$stratumsim_std)*100



# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(stratum80_level, aes(x=Stratum80Rank, y=suicide)) + theme_bw() +
  geom_hline(yintercept=mean(stratum80_level$suicide), color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=PredRE-SeRE, ymax=PredRE+SeRE)) + 
  scale_x_discrete("Ranked Stratum") +
  theme(axis.text.x = element_text(face = "bold",angle = 90, vjust = .5, hjust=1)) +
  xlab("Ranked Stratum") +
  ylab("Predicted suicidal ideation % Random Effects - Full Model")






m5predFeRe <- merge(m5predFeRe, stratumsim2, by="stratum")
names(m5predFeRe)[16:21]=c("stratumsim_mean","stratumsim_std","stratumsim_rank","stratumsim_hi","stratumsim_lo","stratumsim_xlab")

m5predFeRe <- merge(m5predFeRe, stratumsimPre, by="stratum1")
names(m5predFeRe)[22:27]=c("stratumsimPre_mean","stratumsimPre_std","stratumsimPre_rank","stratumsimPre_hi","stratumsimPre_lo","stratumsimPre_xlab")

m5predFeRe <- merge(m5predFeRe, stratumsimPost, by="stratum1")
names(m5predFeRe)[28:33]=c("stratumsimPost_mean","stratumsimPost_std","stratumsimPost_rank","stratumsimPost_hi","stratumsimPost_lo","stratumsimPost_xlab")


write.csv(m5predFeRe,file=paste0(OutDir,"StratumLevel_M5_SimPredictedProbabilities.csv"),row.names = FALSE)

rm(x)



m5predFeRe=m5predFeRe[order(m5predFeRe$PredRe),]
m5predFeRe$stratum=factor(m5predFeRe$stratum,labels= as.character(m5predFeRe$stratum),levels= as.character(m5predFeRe$stratum))

# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(m5predFeRe, aes(x=stratum, y=PredRE)) + theme_bw() +
  geom_hline(yintercept=mean(tmpData$suicide)*100, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=PredRE-SeRE, ymax=PredRE+SeRE)) + 
  scale_x_discrete("Ranked Stratum") +
  theme(axis.text.x = element_text(face = "bold",angle = 90, vjust = .5, hjust=1)) +
  xlab("Ranked Stratum") +
  ylab("Predicted suicidal ideation % Random Effects - Full Model")

ggsave(filename = "CatepillarPlot_M5_RE.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(m5predFeRe, aes(x=stratum, y=PredFE)) + theme_bw() +
  geom_hline(yintercept=mean(tmpData$suicide)*100, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=PredFE-SeFE, ymax=PredFE+SeFE)) + 
  scale_x_discrete("Ranked Stratum") +
  theme(axis.text.x = element_text(face = "bold",angle = 90, vjust = .5, hjust=1)) +
  xlab("Ranked Stratum") +
  ylab("Predicted suicidal ideation % Fixed Effects - Full Model")

ggsave(filename = "CatepillarPlot_M5_FE.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

# plot the caterpillar plot of the predicted stratum percentage differences
ggplot(m5predFeRe, aes(x=stratum, y=stratumsim_mean)) + theme_bw() +
  geom_hline(yintercept=0, color="red", linewidth=1) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=stratumsim_lo, ymax=stratumsim_hi)) + 
  scale_x_discrete("Ranked Stratum",labels=m5predFeRe$stratumsim_xlab) +
  theme(axis.text.x = element_text(face = "bold",angle = 90, vjust = .5, hjust=1)) +
  xlab("Ranked Stratum") +
  ylab("Predicted suicidal ideation % Random-Fixed Effects - Full Model")

ggsave(filename = "CatepillarPlot_M5_REminFE.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")





save.image(file="C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\RnR\\RnRround2\\MAIHDA_ModelDataAndPredictions.RData")


