library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)


df <- readr::read_csv("https://raw.githubusercontent.com/JunaidMerchant/MAIHDA_YRBSS/refs/heads/main/predict_probabilities/All7Outcomes_Strata40_wStateDivisions_reduced.csv")

# df$Year=factor(df$Year)

Questions=list("Suicidal Ideation: During the past 12 months, did you ever seriously consider attempting suicide? (YES/NO)",
               "Suicide Attempts: During the past 12 months, how many times did you actually attempt suicide?                (NOTE: transformed to binary by giving a YES for 1 or more suicide attempts; No for none.)",
               "Suicidal Planning: During the past 12 months, did you make a plan about how you would attempt suicide? (YES/NO)",
               "Sadness: During the past 12 months, did you ever feel so sad or hopeless almost every day for two weeks or more in a row that you stopped doing some usual activities? (YES/NO)",
               "Bullied: During the past 12 months, have you ever been bullied on school property? (YES/NO)",
               "Cyber Bullied: During the past 12 months, have you ever been electronically bullied (Count being bullied through texting,Instagram, Facebook, or other social media.)? (YES/NO)",
               "Raped: Have you ever been physically forced to have sexual intercourse when you did not want to? (YES/NO)")

names(Questions)=unique(df$outcome)


ui <- page_sidebar(
  # Application title
  titlePanel("Intersectional Teen Mental Health Inequities"),
  
  sidebar = sidebar(
    selectInput( 
      "Outcome", 
      "Select youth mental health outcome/exposure:", 
      choices = unique(df$outcome),
      selected = "Attempted Suicide"
    ),
    selectInput( 
      "division", 
      "Select geographic division:", 
      choices = unique(df$Geographic),
      selected = "4 Census Regions"
    ),
    checkboxGroupInput(
      "races", "Select Races:",
      choices = unique(df$Race), 
      selected = c("White","Black","Hispanic")
    ),
    checkboxGroupInput(
      "so", "Select Sexual Orientations:",
      choices = unique(df$Sexual_Orientation), 
      selected = c("Heterosexual","Homosexual","Bisexual")
    ),
    checkboxGroupInput(
      "gender", "Select Genders:",
      choices = unique(df$Gender), 
      selected = unique(df$Gender)
    ),
    selectInput(
      "years", "Select Years: 2015-2023 (dropdown menu)",
      choices = unique(df$Year), 
      selected = c(2017,2019,2021,2023),
      multiple = TRUE
    ),
    input_switch("x", "x-axis = Year (vs Division)", value = TRUE, width = NULL),
    input_switch("row", "Rows = Race (vs sex orient.)", value = TRUE, width = NULL),
    downloadButton('downloadData', 'Download Data')
  ),
  
  # Show a plot of the generated distribution
  htmlOutput('source'),
  plotOutput("line"),
  htmlOutput('foot')
  
  
)






server <- function(input, output, session) {
  
  subsetted <- reactive({
    req(input$races, input$so,input$gender, input$years,input$Outcome,input$division)
    
    df |> filter(Race %in% input$races) |> 
      filter(Sexual_Orientation %in% input$so) |>
      filter(Gender %in% input$gender) |>
      filter(Year %in% input$years) |>
      filter(outcome %in% input$Outcome) |>
      filter(Geographic %in% input$division) 
  })
  

  source <- observe({
    output$source=renderText(paste0("<p><b>Question from CDC's <a href='https://www.cdc.gov/yrbs/index.html'>Youth Risk Behavior Survey</a>:</b>"," ","<i>",Questions[[input$Outcome]],"</i></p>"))
  })
  
  output$foot=renderText("<p>* Predicted probabilities estimated using <a href='https://doi.org/10.1016/j.ssmph.2024.101664'>I-MAIHDA</a>. LGBTQ laws from <a href='https://www.lgbtmap.org/'>MAP</a>. Racial sentiment from <a href='https://doi.org/10.1097/EDE.0000000000001671'>BD4HE</a>.</p>")
  
  
  
  line <- observe({
    
    if (input$x & input$row){
      output$line <- renderPlot({
        
        p <- ggplot(subsetted(), aes(factor(Year), predicted,shape=Gender,color=Sexual_Orientation)) +
          theme_light() +
          facet_grid(vars(as.factor(Race)), vars(state)) +
          theme(strip.text.y = element_text(size = 10,face="bold",color="black")) +
          theme(strip.text.x = element_text(size = 10,face="bold",color="black")) +
          theme(strip.background =element_rect(fill="white")) +
          geom_line(aes(group=strata2, linetype=Gender)) +
          # geom_point(size=.8) +
          geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
          theme(axis.text.y=element_text(size=10,face="bold")) +
          theme(axis.text.x = element_text(size=10,face="bold",angle=90,vjust = 1,hjust = 1)) +
          scale_shape_manual(values = c(15, 16)) +
          scale_linetype_manual(values=c( "longdash","solid"))+
          xlab("Year") +
          ylab("% YES (predicted probability)") +
          scale_color_manual(values = c("red4","grey2","gold2","green4","skyblue3")) +
          ggtitle(input$Outcome) +
          theme(plot.title = element_text( size=16, face="bold",hjust=.5))
        
        
        
        p
      }, res = 100)
    } 
    if (!input$x & input$row){
      output$line <- renderPlot({
        
        p <- ggplot(subsetted(), aes(state, predicted,shape=Gender,color=Sexual_Orientation)) +
          theme_light() +
          facet_grid(vars(as.factor(Race)), vars(factor(Year))) +
          theme(strip.text.y = element_text(size = 10,face="bold",color="black")) +
          theme(strip.text.x = element_text(size = 10,face="bold",color="black")) +
          theme(strip.background =element_rect(fill="white")) +
          geom_line(aes(group=strata2, linetype=Gender)) +
          # geom_point(size=.8) +
          geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
          theme(axis.text.y=element_text(size=10,face="bold")) +
          theme(axis.text.x = element_text(size=10,face="bold",angle=45,vjust = 1,hjust = 1)) +
          scale_shape_manual(values = c(15, 16)) +
          scale_linetype_manual(values=c( "longdash","solid"))+
          xlab(input$division) +
          ylab("% YES (predicted probability)") +
          scale_color_manual(values = c("red4","grey2","gold2","green4","skyblue3")) +
          ggtitle(input$Outcome) +
          theme(plot.title = element_text( size=16, face="bold",hjust=.5))
        
        
        
        p
      }, res = 100)
      
    } 
    if (input$x & !input$row){
      output$line <- renderPlot({
        
        p <- ggplot(subsetted(), aes(factor(Year), predicted,shape=Gender,color=as.factor(Race))) +
          theme_light() +
          facet_grid(vars(Sexual_Orientation), vars(state)) +
          theme(strip.text.y = element_text(size = 10,face="bold",color="black")) +
          theme(strip.text.x = element_text(size = 10,face="bold",color="black")) +
          theme(strip.background =element_rect(fill="white")) +
          geom_line(aes(group=strata, linetype=Gender)) +
          # geom_point(size=.8) +
          geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
          theme(axis.text.y=element_text(size=10,face="bold")) +
          theme(axis.text.x = element_text(size=10,face="bold",angle=90,vjust = 1,hjust = 1)) +
          scale_shape_manual(values = c(15, 16)) +
          scale_linetype_manual(values=c( "longdash","solid"))+
          xlab("Year") +
          ylab("% YES (predicted probability)") +
          scale_color_manual(values = c("skyblue3","grey2","orange3","darkgreen","red4","gold3","purple4")) +
          ggtitle(input$Outcome) +
          theme(plot.title = element_text( size=16, face="bold",hjust=.5))
        
        
        
        p
      }, res = 100)
    } 
    if (!input$x & !input$row){
      output$line <- renderPlot({
        
        p <- ggplot(subsetted(), aes(state, predicted,shape=Gender,color=as.factor(Race))) +
          theme_light() +
          facet_grid(vars(Sexual_Orientation), vars(factor(Year))) +
          theme(strip.text.y = element_text(size = 10,face="bold",color="black")) +
          theme(strip.text.x = element_text(size = 10,face="bold",color="black")) +
          theme(strip.background =element_rect(fill="white")) +
          geom_line(aes(group=strata, linetype=Gender)) +
          # geom_point(size=.8) +
          geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
          theme(axis.text.y=element_text(size=10,face="bold")) +
          theme(axis.text.x = element_text(size=10,face="bold",angle=45,vjust = 1,hjust = 1)) +
          scale_shape_manual(values = c(15, 16)) +
          scale_linetype_manual(values=c( "longdash","solid"))+
          xlab(input$division) +
          ylab("% YES (predicted probability)") +
          scale_color_manual(values = c("skyblue3","grey2","orange3","darkgreen","red4","gold3","purple4")) +
          ggtitle(input$Outcome) +
          theme(plot.title = element_text( size=16, face="bold",hjust=.5))
        
        
        
        p
      }, res = 100)
    }
    
  })
  
}

shinyApp(ui, server)