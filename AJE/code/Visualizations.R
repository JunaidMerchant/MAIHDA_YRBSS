# # install.packages("devtools")
# # devtools::install_github("gertstulp/ggplotgui")
# library("ggplotgui")
# library(ggeffects)   # predictions and significance testing
# library(insight)     # extracting random effects variances
# # library(datawizard)  # data wrangling and preparation
# library(parameters)  # model summaries
# library(performance) # model fit indices, ICC
# library(glmmTMB)     # multilevel modelling
# library(marginaleffects)
# library(ggplot2)
# library(Amelia)
# library(lattice)
# library(sjPlot) 
# library(tidyr)
# library(RColorBrewer)
# library(scales)
# library(gtools)
# library(ggpattern)
# 
# 
# # ggplot_shiny(StratumData)
# 
# ######################################################################################
# # TABLES
# ######################################################################################
# # https://reproducible-epi-workshop.louisahsmith.com/gtsummary
# # http://www.danieldsjoberg.com/gtsummary-weill-cornell-presentation/#8
# # https://www.danieldsjoberg.com/gtsummary/reference/  
# # https://www.danieldsjoberg.com/gtsummary/reference/tbl_stack.html
# 
# 
# rm(list = ls())
# 
# library(gtsummary)
# library(gt)
# library(writexl)
# library(flextable)
# 
# Data = read.csv("C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_Mar21\\DataFiles\\YRBSS_wRaceLGBTSentACS_2017_2019_2021_Clean377362obs.csv")
# 
# 
# Data$suicide_char[Data$suicide==1]="Yes"
# Data$suicide_char[Data$suicide==0]="No"
# 
# Data$sad_char[Data$sad==1]="Yes"
# Data$sad_char[Data$sad==0]="No"
# 
# Data$bully_char[Data$bully==1]="Yes"
# Data$bully_char[Data$bully==0]="No"
# 
# Data$ebully_char[Data$ebully==1]="Yes"
# Data$ebully_char[Data$ebully==0]="No"
# 
# 
# ######################
# ### Demographics Table
# 
# Demographic <- 
#   tbl_summary(
#     Data,
#     by = year,
#     include = c(gender_char,race_char,sexorient_char,
#                 suicide, sad, bully,ebully),
#     label = list(
#       gender_char ~ "Gender",
#       race_char ~ "Race",
#       sexorient_char ~ "Sexual Orientation",
#       suicide ~ "Suicidal Ideation (yes)",
#       sad ~ "Sadness (yes)",
#       bully ~ "Bullied (yes)",
#       ebully ~ "Online Bullied (yes)"
#     ),
#     missing_text = "missing") |> 
#   add_overall(col_label = "**Total**") |> 
#   bold_labels()   |> 
#   modify_footnote(update = everything() ~ NA) #  |> 
#   # modify_header(label = "**Variable**", p.value = "**P**")
# 
# Demographic
# 
# 
# as_gt(Demographic) |> gtsave(filename = "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\DemographicsTable.html")
# as_tibble(Demographic) %>% write_xlsx(., "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\DemographicsTable.xlsx")
# 
# as_flex_table(Demographic) %>% save_as_docx( path = "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\DemographicsTable.docx")
# 
# 
# ######################
# # State Counts by Year
# 
# 
# StateCounts <- 
#   tbl_summary(
#     Data,
#     by = year,
#     include = c(state),
#     label = list(state ~ "State"),
#     missing_text = "missing") |> 
#   add_overall(col_label = "**Total**") |> 
#   bold_labels() |> 
#   modify_footnote(update = everything() ~ NA)
# 
# 
# as_gt(StateCounts) |> gtsave(filename = "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\StateCountsTable.html")
# as_tibble(StateCounts) %>% write_xlsx(., "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\StateCountsTable.xlsx")
# 
# as_flex_table(StateCounts) %>% save_as_docx( path = "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\StateCountsTable.docx")
# 
# 
# 
# ##########################################
# # Intersectional Strata Cross Tabulations
# 
# StrataCrossTabsMale <-
#   Data[Data$gender_char=="Male",] %>%
#   tbl_cross(row = race_char, col = sexorient_char,
#             label = list(race_char ~ "Race", sexorient_char ~ "Sexual Orientation")) %>%
#   bold_labels() |> 
#     modify_footnote(update = everything() ~ NA)
# 
# StrataCrossTabsFemale <-
#   Data[Data$gender_char=="Female",] %>%
#   tbl_cross(row = race_char, col = sexorient_char,
#             label = list(race_char ~ "Race", sexorient_char ~ "Sexual Orientation")) %>%
#   bold_labels() |> 
#   modify_footnote(update = everything() ~ NA)
# 
# StrataCrossTabsAllYears <-
#   tbl_stack(list(StrataCrossTabsFemale, StrataCrossTabsMale), group_header = c("Female", "Male")) %>%
#   bold_labels()
# 
# StrataCrossTabsAllYears
# 
# as_gt(StrataCrossTabsAllYears) |> gtsave(filename = "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\StrataCrossTabsAllYears.html")
# as_tibble(StrataCrossTabsAllYears) %>% write_xlsx(., "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\StrataCrossTabsAllYears.xlsx")
# as_flex_table(StrataCrossTabsAllYears) %>% save_as_docx( path = "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\StrataCrossTabsAllYears.docx")
# 
# 
# ### 2017
# 
# for (y in unique(Data$year)){
#   
#   StrataCrossTabsMale <-
#     Data[(Data$gender_char=="Male" & Data$year==y),] %>%
#     tbl_cross(row = race_char, col = sexorient_char,
#               label = list(race_char ~ "Race", sexorient_char ~ "Sexual Orientation")) %>%
#     bold_labels() |> 
#     modify_footnote(update = everything() ~ NA)
#   
#   StrataCrossTabsFemale <-
#     Data[(Data$gender_char=="Female" & Data$year==y),] %>%
#     tbl_cross(row = race_char, col = sexorient_char,
#               label = list(race_char ~ "Race", sexorient_char ~ "Sexual Orientation")) %>%
#     bold_labels() |> 
#     modify_footnote(update = everything() ~ NA)
#   
#   StrataCrossTabsCombined <-
#     tbl_stack(list(StrataCrossTabsFemale, StrataCrossTabsMale), group_header = c("Female", "Male")) %>%
#     bold_labels()
#   
#   outfile=paste0("C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\StrataCrossTabs_", y)
#   
#   as_gt(StrataCrossTabsCombined) |> gtsave(filename = paste0(outfile,".html"))
#   as_tibble(StrataCrossTabsCombined) %>% write_xlsx(., paste0(outfile,".xlsx") )
#   as_flex_table(StrataCrossTabsCombined) %>% save_as_docx( path = paste0(outfile,".docx"))
#   
#   
# }
# 
# 
# StrataCrossTabsMale17 <-
#   Data[(Data$gender_char=="Male" & Data$year==2017),] %>%
#   tbl_cross(row = race_char, col = sexorient_char,
#             label = list(race_char ~ "Race", sexorient_char ~ "Sexual Orientation")) %>%
#   bold_labels() |> 
#   modify_footnote(update = everything() ~ NA)
# 
# StrataCrossTabsFemale17 <-
#   Data[(Data$gender_char=="Female" & Data$year==2017),] %>%
#   tbl_cross(row = race_char, col = sexorient_char,
#             label = list(race_char ~ "Race", sexorient_char ~ "Sexual Orientation")) %>%
#   bold_labels() |> 
#   modify_footnote(update = everything() ~ NA)
# 
# StrataCrossTabs2017 <-
#   tbl_stack(list(StrataCrossTabsFemale, StrataCrossTabsMale), group_header = c("Female", "Male")) %>%
#   bold_labels()
# 
# StrataCrossTabsAllYears
# 
# as_gt(StrataCrossTabs2017) |> gtsave(filename = "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\StrataCrossTabsAllYears.html")
# as_tibble(StrataCrossTabsAllYears) %>% write_xlsx(., "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\StrataCrossTabsAllYears.xlsx")
# as_flex_table(StrataCrossTabsAllYears) %>% save_as_docx( path = "C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\StrataCrossTabsAllYears.docx")


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

# Read in data and make pre/post 2020 variable
Data = read.csv("C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_Mar21\\DataFiles\\YRBSS_wRaceLGBTSentACS_2017_2019_2021_Clean377362obs.csv")
Data$post2020=0
Data$post2020[Data$year==2021]=1
Data$post2020=factor(Data$post2020)

Data$prepost2020="Pre-2020"
Data$prepost2020[Data$year==2021]="Post-2020"
Data$prepost2020=factor(Data$prepost2020)
Data <- within(Data, prepost2020 <- relevel(prepost2020, ref = "Pre-2020"))

# make year factor varbleia
Data$Year=factor(Data$year)

# make gender x race , and gender x sexorient variables
Data$GenderxRace=paste0(Data$gender_char,"-", Data$race_char)
Data$GenderxSexorient=paste0(Data$gender_char,"-", Data$sexorient_char)

# Make race minority binarized and sex minority binarized variables
Data$RaceMinBin[Data$race_char=="White"]="White"
Data$RaceMinBin[Data$race_char!="White"]="MinorityRace"

Data$SexMinBin[Data$sexorient_char=="Heterosexual"]="Hetero"
Data$SexMinBin[Data$sexorient_char!="Heterosexual"]="SexualMinority"

# Make RaceMinBin x Gender, and SexOrientMinBin x Gender variables
Data$GenderRaceMinBin=paste0(Data$gender_char,"-",Data$RaceMinBin)
Data$GenderSexMinBin=paste0(Data$gender_char,"-",Data$SexMinBin)

# Define output directory
OutDir="C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Visualizations\\"


##########################################
# Model 5 Pre/Post-2020
##########################################
mtest <- glmmTMB(as.formula("suicide ~ 1 + prepost2020 + gender_char + sexorient_char + race_char + ( prepost2020 | gender_char:sexorient_char:race_char)") , data = Data, family=binomial(link='logit'))



# summarize the model
print(summary(mtest))

# get model parameters
# print(model_parameters(mtest,exponentiate=TRUE))

predictions1 <- predict_response(mtest,c("prepost2020","race_char","sexorient_char","gender_char"),type = "random",interval="confidence")






# write.csv(predictions1,file="C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Tables\\Model5_PrePost2020_PredictedProbabilities.csv",row.names = FALSE)

predictions2=predictions1
predictions2$stratum=paste0(predictions2$group, "-",predictions2$facet, "-",predictions2$panel)
predictions2$GenderRace=paste0(predictions2$panel, "-",predictions2$group)
predictions2$GenderSexO=paste0(predictions2$panel, "-",predictions2$facet)
predictions2=predictions2[with(predictions2, order(stratum, x)),]
predictions2$percent=predictions2$predicted*100
predictions2$low=predictions2$conf.low*100
predictions2$hi=predictions2$conf.high*100

predictions3=predictions2[with(predictions2, order(x,stratum)),]
p3_pre=predictions3[predictions3$x=="Pre-2020",]
p3_post=predictions3[predictions3$x=="Post-2020",]

p3_post=p3_post[with(p3_pre, order(percent)),]
p3_pre=p3_pre[with(p3_pre, order(percent)),]
p3_pre$order=c(11:50)
p3_post$order=c(11:50)

p4=rbind(p3_pre[1,],p3_post[1,])
for (currow in 2:40){
  p4=rbind(p4,p3_pre[currow,],p3_post[currow,])
}
row.names(p4) <- NULL
p4$stratumorder=paste0(p4$order,":",p4$stratum)
p4$order2=c(11:90)


names(p4)[1]="Years"
names(p4)[6]="Race"
names(p4)[8]="Gender"
names(p4)[7]="SexualOrientation"

p4$SexualOrientationGender=paste0(p4$SexualOrientation, "-",p4$Gender)

p4$Race=gsub("MultiOtherRace","MultiOther",p4$Race)
p4$Stratum=gsub("MultiOtherRace","MultiOther",p4$stratum)
p4$stratumorder=gsub("MultiOtherRace","MultiOther",p4$stratumorder)

p4$Stratum=gsub("Heterosexual","Hetero",p4$stratum)
p4$Stratumorder=gsub("Heterosexual","Hetero",p4$stratumorder)
names(p4)[11]="GenderSexualOrientation"
p4$GenderSexualOrientation=gsub("Heterosexual","Hetero",p4$GenderSexualOrientation)
p4$GenderSexualOrientation=gsub("Othersexual","Other",p4$GenderSexualOrientation)
p4$GenderSexualOrientation=gsub("Homosexual","Gay/Lesbian",p4$GenderSexualOrientation)
p4$GenderSexualOrientation=gsub("-",": ",p4$GenderSexualOrientation)

p4$stratum2=p4$stratum

p4$stratum2=gsub("Male", "M", p4$stratum2)
p4$stratum2=gsub("Female", "F", p4$stratum2)
p4$stratum2=gsub("Heterosexual", "Het", p4$stratum2)
p4$stratum2=gsub("Homosexual", "Hom", p4$stratum2)
p4$stratum2=gsub("Bisexual", "Bi", p4$stratum2)
p4$stratum2=gsub("Othersexual", "OS", p4$stratum2)
p4$stratum2=gsub("MultiOtherRace", "Multi/Other Race", p4$stratum2)

p4$SexualOrientation=gsub("Homosexual", "Gay/Lesbian (Hom)", p4$SexualOrientation)
p4$SexualOrientation=gsub("Othersexual", "Other sexual (OS)", p4$SexualOrientation)
p4$SexualOrientation=gsub("Heterosexual", "Heterosexual (Het)", p4$SexualOrientation)
p4$SexualOrientation=gsub("Bisexual", "Bisexual (Bi)", p4$SexualOrientation)
p4$Gender=gsub("Male", "Male (M)", p4$Gender)
p4$Gender=gsub("Female", "Female (F)", p4$Gender)

p4$GenderSexualOrientation=gsub(":","-",p4$GenderSexualOrientation)
p4$GenderSexualOrientation=gsub("Gay/Lesbian","Homosexual",p4$GenderSexualOrientation)

p4$SexualOrientationGender=gsub("-"," ",p4$SexualOrientationGender)



# ggplot(data = p4,aes(x = stratumorder, y = percent, shape=Years,color=stratum)) + theme_classic()  +
#   geom_pointrange(aes(ymin = low, ymax = hi)) +
#   # geom_errorbar(aes(ymin=low, ymax=hi), width=.2) +
#   theme(axis.text.x = element_text(size = 10, color="black")) +
#   scale_x_discrete(labels=p4$stratum[seq(1,80,2)]) +
#   scale_shape_manual(values = c(15, 16)) +
#   xlab("Self-Reported suicidal ideation (%)") + 
#   ylab("") + coord_flip() # +
#   # scale_color_manual(values = c("gold","purple3","pink","lightblue","darkorange","blue","red4", "darkgreen")) 



ggplot(data = p4,aes(x = stratumorder, y = percent, shape=Years,color=SexualOrientationGender)) + theme_classic()  +
  geom_pointrange(aes(ymin = low, ymax = hi)) +
  # geom_errorbar(aes(ymin=low, ymax=hi), width=.2) +
  theme(axis.text.y = element_text(size = 10, color="black")) +
  scale_x_discrete(labels=p4$stratum[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  ylab("Self-Reported suicidal ideation (%)") + 
  xlab("") + coord_flip() +
  scale_color_manual(values = c("gold","purple3","pink","lightblue","darkorange","blue","red4", "darkgreen")) 

ggsave(filename = "StrataRanked_PredictedProb_Model5_GenderSexOrientColor_PrePost2020_Probabilities_SEbars.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")





ggplot(data = p4,aes(x = stratumorder, y = percent, shape=Years,color=SexualOrientation)) + theme_classic()  +
  geom_line(aes(group = stratum, linetype=Gender)) + geom_pointrange(aes(ymin = low, ymax = hi)) +
  # geom_errorbar(aes(ymin=low, ymax=hi), width=.2) +
  theme(axis.text.x = element_text(size = 10, color="black")) +
  scale_x_discrete(labels=p4$stratum[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  xlab("Self-Reported suicidal ideation (%)") + 
  ylab("") +
  scale_color_manual(values = c("gold","darkorange","darkgray", "darkgreen")) + coord_flip()



ggplot(data = p4,aes(x = stratumorder, y = percent, shape=Years,color=SexualOrientation)) + theme_classic()  +
  geom_line(aes(group = stratum, linetype=Gender)) + geom_point(size=3) +
  geom_errorbar(aes(ymin=low, ymax=hi), width=.2) +
  theme(axis.text.x = element_text(size = 10, color="black")) +
  scale_x_discrete(labels=p4$stratum2[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  xlab("Self-Reported suicidal ideation (%)") + 
  ylab("") +
  scale_color_manual(values = c("gold","darkorange","darkgray", "darkgreen")) + coord_flip()



ggplot(data = p4,aes(x = percent, y = stratumorder, shape=Years,color=SexualOrientation)) + theme_classic()  +
  geom_line(aes(group = stratum, linetype=Gender)) + geom_point(size=3) +
  geom_errorbar(aes(ymin=low, ymax=hi), width=.2, position=position_dodge(.9)) +
  theme(axis.text.y = element_text(size = 10, color="black")) +
  scale_y_discrete(labels=p4$stratum2[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  xlab("Self-Reported suicidal ideation (%)") + 
  ylab("") +
  scale_color_manual(values = c("gold","darkorange","darkgray", "darkgreen")) 


ggplot(data = p4,aes(x = percent, y = stratumorder, shape=Years,color=SexualOrientation)) + theme_classic()  +
  geom_line(aes(group = stratum, linetype=Gender)) + geom_point(size=3) +
  theme(axis.text.y = element_text(size = 10, color="black")) +
  scale_y_discrete(labels=p4$stratum2[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  xlab("Self-Reported suicidal ideation (%)") + 
  ylab("") +
  scale_color_manual(values = c("gold","darkorange","darkgray", "darkgreen"))

ggsave(filename = "StrataRanked_PredictedProb_Model5_SexOrientColor_PrePost2020_Probabilities_Verticlev2.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")


ggplot(data = p4,aes(x = percent, y = stratumorder, shape=Years,color=SexualOrientation)) + theme_classic()  +
  geom_line(aes(group = stratum, linetype=Gender)) + geom_point(size=3) +
  theme(axis.text.y = element_text(size = 10, color="black")) +
  scale_y_discrete(labels=p4$stratum2[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  xlab("Self-Reported suicidal ideation (%)") + 
  ylab("") +
  scale_color_manual(values = c("gold2", "darkred","blue","green4"))

ggsave(filename = "StrataRanked_PredictedProb_Model5_SexOrientColor_PrePost2020_Probabilities_Verticlev3.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")




ggplot(data = p4,aes(x = percent, y = stratumorder, shape=Years,color=SexualOrientation)) + theme_classic()  +
  geom_line(aes(group = stratum, linetype=Gender)) + geom_point(size=3) +
  theme(axis.text.y = element_text(size = 10, color="black")) +
  scale_y_discrete(labels=p4$stratum2[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  xlab("Self-Reported suicidal ideation (%)") + 
  ylab("") +
  scale_color_manual(values = c("purple2","magenta","blue3", "gold2"))

ggsave(filename = "StrataRanked_PredictedProb_Model5_SexOrientColor_PrePost2020_Probabilities_VerticlevBiSexualFlagColors.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

ggplot(data = p4,aes(x = percent, y = stratumorder, shape=Years,color=SexualOrientation)) + theme_classic()  +
  geom_line(aes(group = stratum, linetype=Gender)) + geom_point(size=3) +
  theme(axis.text.y = element_text(size = 10, color=RaceColors2)) +
  scale_y_discrete(labels=p4$stratum2[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  xlab("Self-Reported suicidal ideation (%)") + 
  ylab("") +
  scale_color_manual(values = c("purple2","magenta","blue3", "gold2"))

ggsave(filename = "StrataRanked_PredictedProb_Model5_SexOrientColor_PrePost2020_Probabilities_VerticlevBiSexualFlagColors_ColoredAxisLabels.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")



ggplot(data = p4,aes(x = percent, y = stratumorder, shape=Years,color=GenderSexualOrientation)) + theme_classic()  +
  geom_line(size=1.1,aes(group = stratum, linetype=Race)) + geom_point(size=3.5) +
  theme(axis.text.y = element_text(size = 10, color="black")) +
  scale_y_discrete(labels=p4$stratum[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  xlab("Self-Reported suicidal ideation (%)") + ylab("Ranked intersectional stratum (by pre-2020)") +
  scale_color_manual(values = c("red2", "gold","darkgrey","darkorange","green4","purple3","lightgrey","blue"))

ggsave(filename = "StrataRanked_PredictedProb_Model5_SexOrientColor_PrePost2020_Probabilities_VerticleNewColors.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")



ggplot(data = p4,aes(x = percent, y = stratumorder, shape=Years,color=SexualOrientation)) + theme_classic()  +
  geom_line(size=.9,aes(group = stratum, linetype=Gender)) + geom_point(size=3.5) +
  theme(axis.text.y = element_text(size = 10, color="black")) +
  scale_y_discrete(labels=p4$stratum[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  xlab("Self-Reported suicidal ideation (%)") + ylab("Ranked intersectional stratum (by pre-2020)") +
  scale_color_manual(values = c("darkgrey","red3", "gold2","blue"))

ggsave(filename = "StrataRanked_PredictedProb_Model5_SexOrientColor_PrePost2020_Probabilities_VerticleNewColors2.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")









ggplot(data = p4,aes(x = stratumorder, y = percent, shape=Years,color=Race)) + theme_classic()  +
  geom_line(aes(group = stratum, linetype=Gender)) + geom_point(size=2) +
  theme(axis.text.x = element_text(face="bold",size = 9,angle = 90, vjust = 1, hjust=1)) +
  scale_x_discrete(labels=p4$stratum[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  ylab("Self-Reported suicidal ideation (%)") + xlab("Ranked intersectional stratum (by pre-2020)") +
  scale_color_manual(values = c("darkgrey","darkred", "darkgreen","darkorange","blue"))

ggsave(filename = "StrataRanked_PredictedProb_Model5_PrePost2020_Probabilities.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")


ggplot(data = p4,aes(x = stratumorder, y = percent, shape=Years,color=SexualOrientation)) + theme_classic()  +
  geom_line(aes(group = stratum, linetype=Gender)) + geom_point(size=2) +
  theme(axis.text.x = element_text(face="bold",size = 9,angle = 90, vjust = 1, hjust=1)) +
  scale_x_discrete(labels=p4$stratum[seq(1,80,2)]) +
  scale_shape_manual(values = c(15, 16)) +
  ylab("Self-Reported suicidal ideation (%)") + xlab("Ranked intersectional stratum (by pre-2020)") +
  scale_color_manual(values = c("darkgrey","red", "gold","blue"))

ggsave(filename = "StrataRanked_PredictedProb_Model5_SexOrientColor_PrePost2020_Probabilities.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")



ggplot(data = StratumData[(StratumData$sexorient3==x),], aes(x=year, y=suicide, group=stratum3_char)) +
  geom_line(size=1.5,aes(linetype=gender,color=race)) + theme_classic()  + 
  # geom_errorbar(aes(ymin=lo_se, ymax=hi_se,color=race), width=.5,position=position_dodge(0.05)) +
  theme(axis.text.x = element_text( face="bold", size=15))+theme(axis.text.y = element_text( face="bold", size=15))+ 
  theme(plot.title = element_text(face="bold", size=22,hjust = 0.5)) + 
  scale_x_continuous(name="Year", breaks=seq(2017,2021,2)) +
  scale_y_continuous(name="Percent Suicidal Ideation", breaks=seq(0,75,5)) +
  scale_color_manual(values = c("darkred", "darkgreen","darkorange","darkgrey","blue")) +
  scale_linetype_manual(values=c("solid","dotdash"))+
  ylim(9, 55) + ylab("Self-Reported Suicidal Ideation (percent)") + ggtitle(x)



ggplot(data = predictions2[(predictions2$facet=="Homosexual"),],
       aes(x = x, y = percent,color=group, ,group=group, shape=panel)) + theme_classic()  +
  geom_line(aes(group = stratum,linetype=panel)) +  
  geom_point(size=4,position=position_dodge(0.2,preserve = "total")) +
  geom_errorbar(aes(ymin=low, ymax=hi,color=group), width=.5,position=position_dodge(0.2,preserve = "total"))


ggplot(data = predictions2[(predictions2$facet=="Homosexual"),],
       aes(x = x, y = percent,color=group, ,group=group, shape=panel)) + theme_classic()  +
  geom_line(aes(group = stratum,linetype=panel)) +  
  geom_pointrange(aes(ymin=low, ymax=hi,color=group), size=.9, position=position_dodge(0.25,preserve = "total"))

  
ggplot(data = predictions2[(predictions2$facet=="Homosexual" & predictions2$panel=="Female"),],
       aes(x = x, y = percent,color=group,group=group)) + theme_classic()  +
  geom_line(aes(group = stratum)) +  
  geom_pointrange(aes(ymin=low, ymax=hi,color=group), size=.9, position=position_dodge(0.2))

  


  
  
  
  
  
  
  
  
  
ggplot(data = predictions1[(predictions1$facet=="Homosexual"),],aes(x = x, y = predicted)) + 
  geom_point(size=3,aes(shape=panel,color = group)) + geom_line()
  geom_line(aes(group=group)) +
  
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high,color=group), width=.5,position=position_dodge(0.05))



ggplot(data = predictions1[(predictions1$facet=="Homosexual"),],aes(x = x, y = predicted, color = group)) + 
  geom_line(aes(linetype=panel,color=group)) +
  geom_point(aes(color=group, shape=panel)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high,color=group), width=.5,position=position_dodge(0.05))
  
  facet_wrap(~variable, scales = "free") +
  scale_x_discrete(labels = c("M1", "M2")) + 
  xlab("")

ggplot(data = StratumData[(StratumData$sexorient3==x),], aes(x=year, y=suicide, group=stratum3_char)) +
  geom_line(size=1.5,aes(linetype=gender,color=race)) + theme_classic()  + 
  # geom_errorbar(aes(ymin=lo_se, ymax=hi_se,color=race), width=.5,position=position_dodge(0.05)) +
  theme(axis.text.x = element_text( face="bold", size=15))+theme(axis.text.y = element_text( face="bold", size=15))+ 
  theme(plot.title = element_text(face="bold", size=22,hjust = 0.5)) + 
  scale_x_continuous(name="Year", breaks=seq(2017,2021,2)) +
  scale_y_continuous(name="Percent Suicidal Ideation", breaks=seq(0,75,5)) +
  scale_color_manual(values = c("darkred", "darkgreen","darkorange","darkgrey","blue")) +
  scale_linetype_manual(values=c("solid","dotdash"))+
  ylim(9, 55) + ylab("Self-Reported Suicidal Ideation (percent)") + ggtitle(x)



#####################
# replot without year
#####################
predictions1 <- ggpredict(
  mtest,
  c("gender_char","race_char","sexorient_char"),
  type = "random",
  ci = NA
)

predictions1 <- ggpredict(mtest,c("gender_char","race_char","sexorient_char"),type = "random",interval="confidence")

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE) + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
# ggsave(filename = "PredictedProb_Model3_PrePost2020_Male.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")


##########################################
# Model 3: PrePost2020 w Binary Race
##########################################

# Model 3: 3-Year Factor 
mtest <- glmmTMB(as.formula("suicide ~ 1 + prepost2020 + ( prepost2020 | gender_char:sexorient_char:RaceMinBin)") , data = Data, family=binomial(link='logit'))

# summarize the model
print(summary(mtest))

# get model parameters
print(model_parameters(mtest,exponentiate=TRUE))

# cross tab counts of strata: 
print(table(Data$RaceMinBin, Data$sexorient_char, Data$gender_char))

predictions1 <- ggpredict(
  mtest,
  c("prepost2020","sexorient_char","RaceMinBin","gender_char"),
  type = "random",interval="confidence"
)


plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[1]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
ggsave(filename = "PredictedProb_Model3_PrePost2020_Male_RaceMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[2]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

ggsave(filename = "PredictedProb_Model3_PrePost2020_Female_RaceMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

###############################################
# Model 3: PrePost2020 w Binary Race x Gender
###############################################


# Model 3: 3-Year Factor 
mtest <- glmmTMB(as.formula("suicide ~ 1 + prepost2020 + ( prepost2020 | GenderRaceMinBin:sexorient_char)") , data = Data, family=binomial(link='logit'))

# summarize the model
print(summary(mtest))

# get model parameters
print(model_parameters(mtest,exponentiate=TRUE))

# cross tab counts of strata: 
print(table(Data$RaceMinBin, Data$sexorient_char, Data$gender_char))

predictions1 <- ggpredict(
  mtest,
  c("prepost2020","GenderRaceMinBin","sexorient_char"),
  type = "random",interval="confidence"
)


plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE) + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  scale_color_manual(values = c( "blue","green3","darkred","gold")) +
xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
ggsave(filename = "PredictedProb_Model3_PrePost2020_GenderxRaceMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")


###############################################
# Model 3: PrePost2020 w Binary SexOrient x Gender
###############################################


mtest <- glmmTMB(as.formula("suicide ~ 1 + prepost2020 + ( prepost2020 | GenderxSexorient:race_char)") , data = Data, family=binomial(link='logit'))

# summarize the model
print(summary(mtest))

# get model parameters
print(model_parameters(mtest,exponentiate=TRUE))

# cross tab counts of strata: 
print(table(Data$race_char, Data$sexorient_char, Data$gender_char))

##
predictions1 <- ggpredict(
  mtest,
  c("prepost2020","GenderxSexorient","race_char"),
  type = "random",interval="confidence")

plot(predictions1,colors = "set2",dot.size=3,connect.lines=TRUE) + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  scale_color_manual(values = c( "darkgrey","skyblue","purple","blue",
                                 "gold","darkorange","red","darkred")) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
ggsave(filename = "PredictedProb_Model3_PrePost2020_GenderxSexMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")


##########################################
# Model 3: PrePost2020 w Binary Sex Orient
##########################################

# Model 3: 3-Year Factor 
mtest <- glmmTMB(as.formula("suicide ~ 1 + prepost2020 + ( prepost2020 | gender_char:SexMinBin:race_char)") , data = Data, family=binomial(link='logit'))

# summarize the model
print(summary(mtest))

# get model parameters
print(model_parameters(mtest,exponentiate=TRUE))

# cross tab counts of strata: 
print(table(Data$RaceMinBin, Data$sexorient_char, Data$gender_char))

predictions1 <- ggpredict(
  mtest,
  c("prepost2020","race_char","SexMinBin","gender_char"),
  type = "random",interval="confidence"
)


plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[1]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
ggsave(filename = "PredictedProb_Model3_PrePost2020_Male_SexMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[2]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

ggsave(filename = "PredictedProb_Model3_PrePost2020_Female_SexMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")




##########################################
# Model 3: 3-Year Factor 
##########################################

mtest <- glmmTMB(as.formula("suicide ~ 1 + Year + ( Year | gender_char:sexorient_char:race_char)") , data = Data, family=binomial(link='logit'))

performance::icc(mtest)

# summarize the model
print(summary(mtest))

# get model parameters
print(model_parameters(mtest,exponentiate=TRUE))

# cross tab counts of strata: 
print(table(Data$race_char, Data$sexorient_char, Data$gender_char))

predictions1 <- ggpredict(
  mtest,
  c("Year","race_char","sexorient_char","gender_char"),
  type = "random",
  ci = NA
)

Min=min(predictions1$predicted)
Max=max(predictions1$predicted)

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[1]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  # scale_y_continuous(name="Suicidal Ideation(%)", breaks=seq(5,55,10),limits=c(5,55)) +
  # scale_y_continuous(name="Suicidal Ideation(%)", breaks=seq(.05,.55,.1),limits=c(.05,.55)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
ggsave(filename = "PredictedProb_Model3_3Year_Male.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[2]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

ggsave(filename = "PredictedProb_Model3_3Year_Female.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")


##################
# Replot w SE bars
##################
predictions1 <- ggpredict(
  mtest,
  c("Year","race_char","sexorient_char","gender_char"),
  type = "random",interval="confidence")
write.csv(predictions1,file="C:\\Users\\jmerch\\Desktop\\LGBTQ\\MAIHDA\\StataResults_May1\\Tables\\Model3_3YearFactor_PredictedProbabilities.csv",row.names = FALSE)


Min=min(predictions1$predicted)
Max=max(predictions1$predicted)

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[1]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
ggsave(filename = "PredictedProb_Model3_3Year_Male_wSE.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="transparent")

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[2]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

ggsave(filename = "PredictedProb_Model3_3Year_Female_wSE.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="transparent")


##########################################
# Model 3: 3-Year Factor w Binary Race
##########################################

# Model 3: 3-Year Factor 
mtest <- glmmTMB(as.formula("suicide ~ 1 + Year + ( Year | gender_char:sexorient_char:RaceMinBin)") , data = Data, family=binomial(link='logit'))

# summarize the model
print(summary(mtest))

# get model parameters
print(model_parameters(mtest,exponentiate=TRUE))

# cross tab counts of strata: 
print(table(Data$RaceMinBin, Data$sexorient_char, Data$gender_char))

predictions1 <- ggpredict(
  mtest,
  c("Year","RaceMinBin","sexorient_char","gender_char"),
  type = "random",interval="confidence"
)

Min=min(predictions1$predicted)
Max=max(predictions1$predicted)

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[1]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
ggsave(filename = "PredictedProb_Model3_3Year_Male_RaceMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[2]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

ggsave(filename = "PredictedProb_Model3_3Year_Female_RaceMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")


###############################################
# Model 3: 3-Year Factor w Binary Race x Gender
###############################################


# Model 3: 3-Year Factor 
mtest <- glmmTMB(as.formula("suicide ~ 1 + Year + ( Year | GenderRaceMinBin:sexorient_char)") , data = Data, family=binomial(link='logit'))

# summarize the model
print(summary(mtest))

# get model parameters
print(model_parameters(mtest,exponentiate=TRUE))

# cross tab counts of strata: 
print(table(Data$RaceMinBin, Data$sexorient_char, Data$gender_char))

predictions1 <- ggpredict(
  mtest,
  c("Year","GenderRaceMinBin","sexorient_char"),
  type = "random",interval="confidence"
)


plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE) + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  scale_color_manual(values = c( "darkgreen","blue","gold","darkorange"))
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
ggsave(filename = "PredictedProb_Model3_3Year_GenderxRaceMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")


###############################################
# Model 3: 3-Year Factor w Binary sexual orient
###############################################

mtest <- glmmTMB(as.formula("suicide ~ 1 + Year + ( Year | gender_char:SexMinBin:race_char)") , data = Data, family=binomial(link='logit'))

# summarize the model
print(summary(mtest))

# get model parameters
print(model_parameters(mtest,exponentiate=TRUE))

# cross tab counts of strata: 
print(table(Data$race_char, Data$SexMinBin, Data$gender_char))

predictions1 <- ggpredict(
  mtest,
  c("Year","race_char","SexMinBin","gender_char"),
  type = "random", interval="confidence"
)

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[1]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  # scale_color_manual(values = c( "darkgreen","blue","gold","darkorange")) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
ggsave(filename = "PredictedProb_Model3_3Year_Male_SexMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[2]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
ggsave(filename = "PredictedProb_Model3_3Year_Female_SexMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")





###############################################
# Model 3: 3-Year Factor w Binary SexOrient x Gender
###############################################


mtest <- glmmTMB(as.formula("suicide ~ 1 + Year + ( Year | GenderxSexorient:race_char)") , data = Data, family=binomial(link='logit'))

# summarize the model
print(summary(mtest))

# get model parameters
print(model_parameters(mtest,exponentiate=TRUE))

# cross tab counts of strata: 
print(table(Data$race_char, Data$sexorient_char, Data$gender_char))

##
predictions1 <- ggpredict(
  mtest,
  c("Year","GenderxSexorient","race_char"),
  type = "random",interval="confidence")



plot(predictions1,colors = "set2",dot.size=3,connect.lines=TRUE) + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  scale_color_manual(values = c( "darkgrey","skyblue","purple","blue",
                                 "gold","darkorange","red","darkred")) +
xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
ggsave(filename = "PredictedProb_Model3_3Year_GenderxSexMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")






###############################################
# Model 3: 3-Year Factor + state
###############################################


mtest <- glmmTMB(as.formula("suicide ~ 1 + Year + state + ( Year | gender_char:sexorient_char:race_char)") , data = Data, family=binomial(link='logit'))
# get model parameters
print(model_parameters(mtest,exponentiate=TRUE))

predictions1 <- ggpredict(
  mtest,
  c("Year","race_char","sexorient_char","gender_char"),
  type = "random", interval="confidence"
)

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[1]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  # scale_color_manual(values = c( "darkgreen","blue","gold","darkorange")) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
# ggsave(filename = "PredictedProb_Model3_3Year_Male_SexMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[2]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
# ggsave(filename = "PredictedProb_Model3_3Year_Female_SexMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")


###############################################
# Model 3: 3-Year Factor + state random
###############################################


mtest <- glmmTMB(as.formula("suicide ~ 1 + Year + state + ( Year | gender_char:sexorient_char:race_char) + ( state | gender_char:sexorient_char:race_char)") , data = Data, family=binomial(link='logit'))
# get model parameters
print(model_parameters(mtest,exponentiate=TRUE))

predictions1 <- ggpredict(
  mtest,
  c("Year","race_char","sexorient_char","gender_char"),
  type = "random", interval="confidence"
)

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[1]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  # scale_color_manual(values = c( "darkgreen","blue","gold","darkorange")) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
# ggsave(filename = "PredictedProb_Model3_3Year_Male_SexMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")

plot(predictions1,colors = "metro",dot.size=3,connect.lines=TRUE)[[2]] + theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.x = element_text(face = "bold",size = 11,angle = 45, vjust = 1, hjust=1)) +
  xlab("Year") + ylab("Predicted Suicidal Ideation (%)")

# save plot
# ggsave(filename = "PredictedProb_Model3_3Year_Female_SexMinBin.png",path = OutDir, width = 8, height = 6, device='png', dpi=300,bg="white")



