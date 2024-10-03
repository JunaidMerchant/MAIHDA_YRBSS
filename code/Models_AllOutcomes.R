######################################################################################
# MODEL PLOTS
######################################################################################
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

# Read in data and make pre/post 2020 variable
Data = read.csv("C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_Mar21\\DataFiles\\YRBSS_wRaceLGBTSentACS_2017_2019_2021_Clean377362obs.csv")
Data$post2020=0
Data$post2020[Data$year==2021]=1
Data$post2020=factor(Data$post2020)

Data$prepost2020="Pre-2020"
Data$prepost2020[Data$year==2021]="Post-2020"
Data$prepost2020=factor(Data$prepost2020)
Data <- within(Data, prepost2020 <- relevel(prepost2020, ref = "Pre-2020"))

Data$gender_char=factor(Data$gender_char)
Data <- within(Data, gender_char <- relevel(gender_char, ref = "Male"))

Data$sexorient_char=factor(Data$sexorient_char)
Data <- within(Data, sexorient_char <- relevel(sexorient_char, ref = "Heterosexual"))

Data$race_char=factor(Data$race_char)
Data <- within(Data, race_char <- relevel(race_char, ref = "White"))


# make year factor varbleia
Data$Year=factor(Data$year)


# Define output directory
OutDir="C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\modelResultsR\\"

setwd(OutDir)

Outcomes=c("suicide","sad","bully","ebully")

OutcomeNum=c(80,79,77,78)

i=1

for (i in 1:length(Outcomes)){
  
  tmpData=Data[!is.na(Data[OutcomeNum[i]]),]
  
  sink(paste0(OutDir,Outcomes[i],"_maihda.txt"))
  
  cat("__________________________________\n")
  cat("__________________________________\n")
  cat(paste(Outcomes[i],"\n"))
  cat("__________________________________\n")
  cat("__________________________________\n")
  
  cat("\n")
  cat("__________________________________\n")
  cat("Null Model 1 \n")
  cat("__________________________________\n")
  
  # estimate null model
  m_null <- glmmTMB(as.formula(paste(Outcomes[i]," ~ 1 +  ( 1 | gender_char:sexorient_char:race_char)")) , data = tmpData, family=binomial(link='logit'))

  # summarize the model
  cat("---------\n summary: \n---------\n")
  print(summary(m_null))
  cat("\n \n \n")
  
  # get model parameters
  cat("------------------\n model parameters: \n------------------\n")
  print(model_parameters(m_null,exponentiate=TRUE))
  cat("\n \n \n")
  # intraclass correlation
  cat("-------------------------\n intra-class correlation: \n-------------------------\n")
  print(icc(m_null))
  cat("\n")
  
  cat("------------------\n variance \n------------------\n")
  v_null <- get_variance(m_null)
  cat(v_null$var.random)
  cat("\n \n \n")
  
  cat("\n")
  cat("__________________________________\n")
  cat("Model 2: fixed year \n")
  cat("__________________________________\n")
  
  # estimate null model
  m_2 <- glmmTMB(as.formula(paste(Outcomes[i]," ~ 1 +  prepost2020 + ( 1 | gender_char:sexorient_char:race_char)")) , data = tmpData, family=binomial(link='logit'))

  # summarize the model
  cat("---------\n summary: \n---------\n")
  print(summary(m_2))
  cat("\n \n \n")
  
  # get model parameters
  cat("------------------\n model parameters: \n------------------\n")
  print(model_parameters(m_2,exponentiate=TRUE))
  cat("\n \n \n")
  # intraclass correlation
  cat("-------------------------\n intra-class correlation: \n-------------------------\n")
  print(icc(m_2))
  cat("\n")
  
  cat("------------------\n variance & PCV \n------------------\n")
  # get variaance
  v_2 <- get_variance(m_2)
  cat(v_2$var.random)
  cat("\n")
  print(((v_null$var.random - v_2$var.random) / v_null$var.random)*100)
  cat("\n \n \n")
  
  
  cat("\n")
  cat("__________________________________\n")
  cat("Model 3: random year \n")
  cat("__________________________________\n")
  
  # estimate null model
  m_3 <- glmmTMB(as.formula(paste(Outcomes[i]," ~ 1 +  prepost2020 + ( prepost2020 | gender_char:sexorient_char:race_char)")) , data = tmpData, family=binomial(link='logit'))

  # summarize the model
  cat("---------\n summary: \n---------\n")
  print(summary(m_3))
  cat("\n \n \n")
  
  # get model parameters
  cat("------------------\n model parameters: \n------------------\n")
  print(model_parameters(m_3,exponentiate=TRUE))
  cat("\n \n \n")
  # intraclass correlation
  cat("-------------------------\n intra-class correlation: \n-------------------------\n")
  print(icc(m_3))
  cat("\n")
  
  cat("------------------\n variance & PCV \n------------------\n")
  # get variaance
  v_3 <- get_variance(m_3)
  cat(v_3$var.random)
  cat("\n")
  print(((v_null$var.random - v_3$var.random) / v_null$var.random)*100)
  cat("\n \n \n")
  
  cat("\n")
  cat("__________________________________\n")
  cat("Model 4: fixed main effects \n")
  cat("__________________________________\n")
  
  # estimate null model
  m_4 <- glmmTMB(as.formula(paste(Outcomes[i]," ~ 1 +  prepost2020 + gender_char + sexorient_char + race_char + ( 1 | gender_char:sexorient_char:race_char)")) , data = tmpData, family=binomial(link='logit'))
  
  # summarize the model
  cat("---------\n summary: \n---------\n")
  print(summary(m_4))
  cat("\n \n \n")
  
  # get model parameters
  cat("------------------\n model parameters: \n------------------\n")
  print(model_parameters(m_4,exponentiate=TRUE))
  cat("\n \n \n")
  # intraclass correlation
  cat("-------------------------\n intra-class correlation: \n-------------------------\n")
  print(icc(m_4))
  cat("\n")
  
  cat("------------------\n variance & PCV \n------------------\n")
  # get variaance
  v_4 <- get_variance(m_4)
  cat(v_4$var.random)
  cat("\n")
  print(((v_null$var.random - v_4$var.random) / v_null$var.random)*100)
  cat("\n \n \n")
  
  cat("\n")
  cat("__________________________________\n")
  cat("Model 5: fixed main effects + random year \n")
  cat("__________________________________\n")
  
  # estimate null model
  m_5 <- glmmTMB(as.formula(paste(Outcomes[i]," ~ 1 +  prepost2020 + gender_char + sexorient_char + race_char + ( prepost2020 | gender_char:sexorient_char:race_char)")) , data = tmpData, family=binomial(link='logit'))
   
  # summarize the model
  cat("---------\n summary: \n---------\n")
  print(summary(m_5))
  cat("\n \n \n")
  
  # get model parameters
  cat("------------------\n model parameters: \n------------------\n")
  print(model_parameters(m_5,exponentiate=TRUE))
  cat("\n \n \n")
  # intraclass correlation
  cat("-------------------------\n intra-class correlation: \n-------------------------\n")
  print(icc(m_5))
  cat("\n")
  
  cat("------------------\n variance & PCV \n------------------\n")
  # get variaance
  v_5 <- get_variance(m_5)
  cat(v_5$var.random)
  cat("\n")
  print(((v_null$var.random - v_5$var.random) / v_null$var.random)*100)
  cat("\n \n \n")
  
  print(test_predictions(m_5,"prepost2020",test = "pairwise", by="sexorient_char"))
  cat("\n")
  print(test_predictions(m_5,"prepost2020",test = "pairwise", by="race_char"))
  cat("\n")
  print(test_predictions(m_5,"prepost2020",test = "pairwise", by="gender_char"))
  cat("\n")
  print(test_predictions(m_5,"prepost2020",test = "pairwise", by=c("sexorient_char","gender_char")))
  cat("\n")
  print(test_predictions(m_5,"prepost2020",test = "pairwise", by=c("race_char","gender_char")))
  cat("\n")
  
  sink()
  
  predictions1 <- predict_response(m_5,c("prepost2020","race_char","sexorient_char","gender_char"),type = "random",interval="confidence")
  
  write.csv(predictions1,file=paste0(OutDir,Outcomes[i],"_MAIHDA_PredictedProbabilities.csv"),row.names = FALSE)
  
  
  plot(predictions1,colors = "metro",connect.lines=TRUE)[[1]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  # save plot
  ggsave(filename = paste0(Outcomes[i],"_Model5_PrePost2020_Male.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  plot(predictions1,colors = "metro",connect.lines=TRUE)[[2]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  ggsave(filename = paste0(Outcomes[i],"_Model5_PrePost2020_Female.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  
  
  
  save.image(file=paste0(OutDir,Outcomes[i],"_MAIHDA_modeldata.RData"))
  
  rm("m_null","m_2","m_3","m_4","m_5","tmpData","v_null","v_2","v_3","v_4","v_5","predictions1")
  
}

