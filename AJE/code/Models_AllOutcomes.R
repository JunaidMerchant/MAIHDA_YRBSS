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
library(pglm)
library(pdR)



# Read in data and make pre/post 2020 variable
Data = read.csv("C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_Mar21\\DataFiles\\YRBSS_wRaceLGBTSentACS_2017_2019_2021_Clean377362obs.csv")



# SITE CODE = STATES
Data$sitecode=factor(Data$sitecode)
Data$sitecode=factor(Data$sitecode)
Data <- within(Data, sitecode <- relevel(sitecode, ref = "MD"))

# CREATE PRE-POST-2020 BINARY VARIABLE
Data$post2020=0
Data$post2020[Data$year==2021]=1
Data$post2020=factor(Data$post2020)
Data$prepost2020="Pre-2020"
Data$prepost2020[Data$year==2021]="Post-2020"
Data$prepost2020=factor(Data$prepost2020)
Data <- within(Data, prepost2020 <- relevel(prepost2020, ref = "Pre-2020"))

# CREATE GENDER FACTOR VARIABLE
Data$gender_char=factor(Data$gender_char)
Data <- within(Data, gender_char <- relevel(gender_char, ref = "Male"))

# CREATE SEXUAL ORIENTATION FACTOR VARIABLE
Data$sexorient_char=factor(Data$sexorient_char)
Data <- within(Data, sexorient_char <- relevel(sexorient_char, ref = "Heterosexual"))

# CREATE RACE FACTOR VARIABLE
Data$race_char=factor(Data$race_char)
Data <- within(Data, race_char <- relevel(race_char, ref = "White"))



# make year factor varble
Data$Year=factor(Data$year)


# Define output directory
OutDir="C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\RnR\\ReDo\\"

setwd(OutDir)

# Outcomes=c("suicide","sad","bully","ebully")
Outcomes=c("suicide")
OutcomeNum=c(80)

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
  print(v_null)
  cat("\n \n \n")
  
  cat("\n")
  cat("__________________________________\n")
  cat("Model 2: fixed year \n")
  cat("__________________________________\n")
  
  # estimate  model
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
  # get variance
  v_2 <- get_variance(m_2)
  print(v_2)
  cat("\n")
  # print(((v_null$var.random - v_2$var.random) / v_null$var.random)*100)
  cat("\n \n \n")
  
  fe_m_2 = pglm("suicide ~ prepost2020",data=tmpData,family = binomial(link='logit'),model="pooling",index=c("stratum3_char"),method = "bfgs",na.action=na.omit,start=NULL)
  print(htest_pglm(RE=m_2,  FE=fe_m_2, re.method="glmmTMB"))
  
  
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
  print(v_3)
  cat("\n")
  # print(((v_null$var.random - v_3$var.random) / v_null$var.random)*100)
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
  print(v_4)
  cat("\n")
  # print(((v_null$var.random - v_4$var.random) / v_null$var.random)*100)
  cat("\n \n \n")
  fe_m_4 = pglm("suicide ~ prepost2020 + gender_char + sexorient_char + race_char",data=tmpData,family = binomial(link='logit'),model="pooling",index=c("stratum3_char"),method = "bfgs",na.action=na.omit,start=NULL)
  print(htest_pglm(RE=m_4,  FE=fe_m_4, re.method="glmmTMB"))
  
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
  print(v_5)
  cat("\n")
  cat("\n \n \n")
  
  m5_predictions_RE = predict_response(m_5,c("prepost2020","race_char","sexorient_char","gender_char"),type = "random",interval="confidence")
  write.csv(m5_predictions_RE,file=paste0(OutDir,Outcomes[i],"_M5_PredictedProbabilities.csv"),row.names = FALSE)
  
  plot(m5_predictions_RE,colors = "metro",connect_lines=TRUE)[[1]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  # save plot
  ggsave(filename = paste0(Outcomes[i],"_Model5_Male_PrePost2020.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  plot(m5_predictions_RE,colors = "metro",connect_lines=TRUE)[[2]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  # save plot
  ggsave(filename = paste0(Outcomes[i],"_Model5_Female_PrePost2020.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  
  
  cat("\n")
  cat("__________________________________\n")
  cat("Model 6: fixed main effects + state + random year \n")
  cat("__________________________________\n")
  
  # estimate null model
  m_6 <- glmmTMB(as.formula(paste(Outcomes[i]," ~ 1 +  prepost2020 + gender_char + sexorient_char + race_char + sitecode + ( prepost2020 | gender_char:sexorient_char:race_char)")) , data = tmpData, family=binomial(link='logit'))
  
  # summarize the model
  cat("---------\n summary: \n---------\n")
  print(summary(m_6))
  cat("\n \n \n")
  
  # get model parameters
  cat("------------------\n model parameters: \n------------------\n")
  mp_m6=model_parameters(m_6,exponentiate=TRUE)
  print(mp_m6)
  write.csv(mp_m6,file=paste0(OutDir,Outcomes[i],"_M6_ModelParameters.csv"),row.names = FALSE)
  
  cat("\n \n \n")
  # intraclass correlation
  cat("-------------------------\n intra-class correlation: \n-------------------------\n")
  print(icc(m_6))
  cat("\n")
  
  cat("------------------\n variance & PCV \n------------------\n")
  # get variaance
  v_6 <- get_variance(m_6)
  print(v_6)
  cat("\n")
  
  m6_predictions_RE = predict_response(m_6,c("prepost2020","race_char","sexorient_char","gender_char"),type = "random",interval="confidence")
  write.csv(m6_predictions_RE,file=paste0(OutDir,Outcomes[i],"_M6_PredictedProbabilities.csv"),row.names = FALSE)
  
  plot(m6_predictions_RE,colors = "metro",connect_lines=TRUE)[[1]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  # save plot
  ggsave(filename = paste0(Outcomes[i],"_Model6_Male_PrePost2020.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  plot(m6_predictions_RE,colors = "metro",connect_lines=TRUE)[[2]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  # save plot
  ggsave(filename = paste0(Outcomes[i],"_Model6_Female_PrePost2020.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  
  
  
  
  
  
  sink()
  
  
  
  save.image(file=paste0(OutDir,Outcomes[i],"_MAIHDA_modeldata.RData"))
  
  
  
  
  
  
  

  
  rm("m_null","m_2","m_3","m_4","m_5","tmpData","v_null","v_2","v_3","v_4","v_5","predictions1")
  
}














######################################################################################
# SUPPLEMENTAL WITH JUST 25 STATES W FULL DATA
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
library(pglm)
library(pdR)



# Read in data and make pre/post 2020 variable
Data = read.csv("C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_Mar21\\DataFiles\\YRBSS_wRaceLGBTSentACS_2017_2019_2021_Clean377362obs.csv")






# should exclude 30,597 obs !
DropStates=c("AL","CA","DE","IN","KS","MO","MS","NJ","SC","UT","VA")
DropStateIndx = !grepl( paste(DropStates,collapse="|"), Data$sitecode)
Data = Data[DropStateIndx,]

Data$sitecode=factor(Data$sitecode)



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
OutDir="C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\RnR\\ReDo\\"

setwd(OutDir)

# Outcomes=c("suicide","sad","bully","ebully")
Outcomes=c("suicide")
OutcomeNum=c(80)

i=1

for (i in 1:length(Outcomes)){
  
  tmpData=Data[!is.na(Data[OutcomeNum[i]]),]
  
  sink(paste0(OutDir,Outcomes[i],"_maihda_25states.txt"))
  
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
  print(v_null)
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
  print(v_2)
  cat("\n")
  # print(((v_null$var.random - v_2$var.random) / v_null$var.random)*100)
  cat("\n \n \n")
  
  fe_m_2 = pglm("suicide ~ prepost2020",data=tmpData,family = binomial(link='logit'),model="pooling",index=c("stratum3_char"),method = "bfgs",na.action=na.omit,start=NULL)
  print(htest_pglm(RE=m_2,  FE=fe_m_2, re.method="glmmTMB"))
  
  
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
  print(v_3)
  cat("\n")
  # print(((v_null$var.random - v_3$var.random) / v_null$var.random)*100)
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
  print(v_4)
  cat("\n")
  # print(((v_null$var.random - v_4$var.random) / v_null$var.random)*100)
  cat("\n \n \n")
  fe_m_4 = pglm("suicide ~ prepost2020 + gender_char + sexorient_char + race_char",data=tmpData,family = binomial(link='logit'),model="pooling",index=c("stratum3_char"),method = "bfgs",na.action=na.omit,start=NULL)
  print(htest_pglm(RE=m_4,  FE=fe_m_4, re.method="glmmTMB"))
  
  cat("\n")
  cat("__________________________________\n")
  cat("Model 5: fixed main effects + random year \n")
  cat("__________________________________\n")
  
  # estimate null model
  m_5 <- glmmTMB(as.formula(paste(Outcomes[i]," ~ 1 +  prepost2020 + gender_char + sexorient_char + race_char +  ( prepost2020 | gender_char:sexorient_char:race_char)")) , data = tmpData, family=binomial(link='logit'))
  
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
  print(v_5)
  cat("\n")
  m5_predictions_RE = predict_response(m_5,c("prepost2020","race_char","sexorient_char","gender_char"),type = "random",interval="confidence")
  write.csv(m5_predictions_RE,file=paste0(OutDir,Outcomes[i],"_M5_PredictedProbabilities_25states.csv"),row.names = FALSE)
  
  plot(m5_predictions_RE,colors = "metro",connect_lines=TRUE)[[1]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  # save plot
  ggsave(filename = paste0(Outcomes[i],"_Model5_Male_PrePost2020_25states.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  plot(m5_predictions_RE,colors = "metro",connect_lines=TRUE)[[2]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  # save plot
  ggsave(filename = paste0(Outcomes[i],"_Model5_Female_PrePost2020_25states.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  
  
  cat("\n")
  cat("__________________________________\n")
  cat("Model 6: fixed main effects + state + random year \n")
  cat("__________________________________\n")
  
  # estimate null model
  m_6 <- glmmTMB(as.formula(paste(Outcomes[i]," ~ 1 +  prepost2020 + gender_char + sexorient_char + race_char + sitecode + ( prepost2020 | gender_char:sexorient_char:race_char)")) , data = tmpData, family=binomial(link='logit'))
  
  # summarize the model
  cat("---------\n summary: \n---------\n")
  print(summary(m_6))
  cat("\n \n \n")
  
  # get model parameters
  cat("------------------\n model parameters: \n------------------\n")
  mp_m6=model_parameters(m_6,exponentiate=TRUE)
  print(mp_m6)
  write.csv(mp_m6,file=paste0(OutDir,Outcomes[i],"_M6_ModelParameters_25states.csv"),row.names = FALSE)
  
  cat("\n \n \n")
  # intraclass correlation
  cat("-------------------------\n intra-class correlation: \n-------------------------\n")
  print(icc(m_6))
  cat("\n")
  
  cat("------------------\n variance & PCV \n------------------\n")
  # get variaance
  v_6 <- get_variance(m_6)
  print(v_6)
  cat("\n")
  
  m6_predictions_RE = predict_response(m_6,c("prepost2020","race_char","sexorient_char","gender_char"),type = "random",interval="confidence")
  write.csv(m6_predictions_RE,file=paste0(OutDir,Outcomes[i],"_M6_PredictedProbabilities_25states.csv"),row.names = FALSE)
  
  plot(m6_predictions_RE,colors = "metro",connect_lines=TRUE)[[1]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  # save plot
  ggsave(filename = paste0(Outcomes[i],"_Model6_Male_PrePost2020_25states.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  plot(m6_predictions_RE,colors = "metro",connect_lines=TRUE)[[2]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  # save plot
  ggsave(filename = paste0(Outcomes[i],"_Model6_Female_PrePost2020_25states.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  
  
  cat("\n")
  cat("__________________________________\n")
  cat("Model 7: fixed main effects + state + agenum + random year \n")
  cat("__________________________________\n")
  
  # estimate null model
  m_7 <- glmmTMB(as.formula(paste(Outcomes[i]," ~ 1 +  prepost2020 + gender_char + sexorient_char + race_char + sitecode + agenum + ( prepost2020 | gender_char:sexorient_char:race_char)")) , data = tmpData, family=binomial(link='logit'))
  
  # summarize the model
  cat("---------\n summary: \n---------\n")
  print(summary(m_7))
  cat("\n \n \n")
  
  # get model parameters
  cat("------------------\n model parameters: \n------------------\n")
  print(model_parameters(m_7,exponentiate=TRUE))
  cat("\n \n \n")
  # intraclass correlation
  cat("-------------------------\n intra-class correlation: \n-------------------------\n")
  print(icc(m_7))
  cat("\n")
  
  cat("------------------\n variance & PCV \n------------------\n")
  # get variaance
  v_7 <- get_variance(m_7)
  print(v_7)
  cat("\n")
  
  
  
  
  
  
  sink()
  
  m7_predictions_RE = predict_response(m_7,c("prepost2020","race_char","sexorient_char","gender_char"),type = "random",interval="confidence")
  m7_predictions_FE = predict_response(m_7,c("prepost2020","race_char","sexorient_char","gender_char"),type = "fixed",interval="confidence")
  predictions1=m7_predictions_RE
  
  names(m7_predictions_FE)=paste0(names(m7_predictions_FE),"_FE")
  names(m7_predictions_RE)=paste0(names(m7_predictions_RE),"_RE")
  m7_predictions=cbind(m7_predictions_FE,m7_predictions_RE)
  
  m7_predictions$m7_pred_FE=m7_predictions$predicted_FE*100
  m7_predictions$m7_pred_RE=m7_predictions$predicted_RE*100
  
  write.csv(m7_predictions,file=paste0(OutDir,Outcomes[i],"_M7_PredictedProbabilities_25states.csv"),row.names = FALSE)
  
  save.image(file=paste0(OutDir,Outcomes[i],"_MAIHDA_modeldata_25states.RData"))
  
  
  plot(predictions1,colors = "metro",connect_lines=TRUE)[[1]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  # save plot
  ggsave(filename = paste0(Outcomes[i],"_Model7_Male_PrePost2020_25states.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  plot(predictions1,colors = "metro",connect_lines=TRUE)[[2]] + theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
    xlab("Year") + ylab("Predicted Suicidal Ideation (%)")
  
  # save plot
  ggsave(filename = paste0(Outcomes[i],"_Model7_Female_PrePost2020_25states.png"),path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")
  
  
  
  
  
  
  
  
  
  rm("m_null","m_2","m_3","m_4","m_5","tmpData","v_null","v_2","v_3","v_4","v_5","predictions1")
  
}