## /AJE
**Project repository for paper under review at American Journal of Epidemiology**

###  Intersectional inequities in suicide ideation by race, sexual orientation, and gender among U.S. high school students pre and post 2020: An application of random effects intersectional MAIHDA

Quantifying intersectional health inequities and examining changes over time are foundational to social epidemiology. I-MAIHDA (intersectional multilevel analysis of individual heterogeneity and discriminatory accuracy) is a recent innovation that simplifies quantitative intersectional analyses while providing methodological improvements over conventional approaches. We illustrate the use of logistic I-MAIHDA with random effects to estimate intersectional inequities in suicidal ideation among U.S. high school students before and after 2020 by race, sexual orientation, and gender, using 2017-2021 data from the Youth Risk Behavior Surveillance System. The U.S. faces a youth mental health crisis made worse by the many disruptions of 2020, including the beginning of the COVID-19 pandemic. Before 2020, we found substantial inequities in ideation, ranging from 9.8-12.7% among heterosexual boys to over 50% among bisexual Multi-race/Other and White girls. We also found notable changes pre/post 2020. Strata at the lowest (heterosexual boys) and highest risk (bisexual girls) showed little change, while middle risk-ranked strata (Black Other/Questioning and lesbian girls, White Other/Questioning  boys and girls, and Multi-race/Other gay boys) reported large increases in ideation. Our findings suggest worsening teen mental health in the 2017-2021 period, particularly among racial and sexual orientation minorities. We illustrate the value of I-MAIHDA for understanding changes in intersectional health inequities.

![Main Figure](https://github.com/JunaidMerchant/MAIHDA_YRBSS/blob/main/Figure2_colorcodedY.png)


### /AJE/code

**Code for the analyses**

- `CleanAndMergeData.do`: Stata code for cleaning and merging data   
- `MELogisticModels.do`: Stata code for running mixed effects logistic I-MAIHDA models
- `Models_AllOutcomes.R`: R code for running mixed effects logistic I-MAIHDA models (used in Manuscript/Supplemental)
- `Visualizations.R`: R code for visualizations
- `HausmannTestReDo.R`: Custom R code for doing Hausmann test for models running using glmmTMB R package
- `CatepillerPlots_glmmTMB.R`: Custom R code for generating caterpillar plots for models running using glmmTMB R package

Analysis ready data files are too big to share on GitHub but can be requested.

### /AJE/results: model results and visualizations files


## /AJE/Shiny:

Intersectional Youth Mental Health Interactive [ [link](https://junaidmerchant.shinyapps.io/YRBSS_MAIHDA/) ] (preview image):

![Main Figure](https://github.com/JunaidMerchant/MAIHDA_YRBSS/blob/main/ShinyApp.png)
