library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(plotly)
library(reactable)

############################
# Read in data 
df <- readr::read_csv("https://raw.githubusercontent.com/JunaidMerchant/MAIHDA_YRBSS/refs/heads/main/Shiny/predict_probabilities/All7Outcomes_Strata40_wStateDivisions_reduced.csv")

############################
# Define YRBSS Questions for HTML
Questions=list("<b>1) Suicidal Ideation</b>: During the past 12 months, did you ever seriously consider attempting suicide? (YES/NO)",
               "<b>2) Suicide Attempts</b>: During the past 12 months, how many times did you actually attempt suicide?                (NOTE: transformed to binary by giving a YES for 1 or more suicide attempts; No for none.)",
               "<b>3) Suicidal Planning</b>: During the past 12 months, did you make a plan about how you would attempt suicide? (YES/NO)",
               "<b>4) Sadness</b>: During the past 12 months, did you ever feel so sad or hopeless almost every day for two weeks or more in a row that you stopped doing some usual activities? (YES/NO)",
               "<b>5) Bullied</b>: During the past 12 months, have you ever been bullied on school property? (YES/NO)",
               "<b>6) Cyber Bullied</b>: During the past 12 months, have you ever been electronically bullied (Count being bullied through texting,Instagram, Facebook, or other social media.)? (YES/NO)",
               "<b>7) Raped</b>: Have you ever been physically forced to have sexual intercourse when you did not want to? (YES/NO)")

names(Questions)=unique(df$outcome)

############################
# Define Details for HTML
Details=list("<i>* Predicted probabilities estimated using <b><a href='https://doi.org/10.1016/j.ssmph.2024.101664'>I-MAIHDA</a></b> (intersectional multilevel analysis of individual heterogeneity and discriminatory accuracy).</i>",   
             "  
             ",
             "<b>A) 2020 LGBTQ policy tally from <a href='https://www.lgbtmap.org/'>MAP</a>.</b>")

names(Details)=c(1,2,3)

############################
# Create Color and Vars DF for Plottings
SoColor=data.frame(So=c("Heterosexual","Bisexual","Homosexual","Other-Questioning","Minority (Not Hetero)"),Color=c("grey2","red4","gold2","green4","skyblue3"))
RaceColor=data.frame(Race=c("White","Asian","Black","Hispanic","Multi-Race","Other Race","Minority (Not White)"),Color=c("grey2","darkgreen","orange3","skyblue3","red4","gold3","purple4"))
GenderVars=data.frame(Gender=c("Boy","Girl"),Shape=c(15,16),Line=c("longdash","solid"))
Vars=list(X="Year",ColorVar="Sexual_Orientation",ColorVarLab="Sexuality",RowVar="Race",ColumnVar="state",Group="strata2",Angle=90)



############################
# Start Fluid UI
ui <- fluidPage(
  
  ############################
  # define side bar components
  page_sidebar(
    
    ############################
    # Application title
    titlePanel("Intersectional Teen Mental Health"),
    
    ############################
    # Side Bar Buttons/Functions
    sidebar = sidebar(
      selectInput( 
        "Outcome", 
        "Select youth mental health outcome/exposure:", 
        choices = unique(df$outcome),
        selected = "Attempted Suicide"
      ),
      
      ############################
      # Geographay
      selectInput( 
        "division", 
        "Select Geographic Division:", 
        choices = unique(df$Geographic),
        selected = "4 Census Regions"
      ),
      
      ############################
      # Gender
      checkboxGroupInput(
        "gender", "Select Genders:",
        choices = unique(df$Gender), 
        selected = unique(df$Gender)
      ),
      
      ############################
      # Race
      selectInput(
        "races", "Select Race/Ethnicities (7 options):",
        choices = unique(df$Race), 
        selected = c("White","Black","Hispanic"),
        multiple = TRUE
      ),
      
      ############################
      # Sexuality
      selectInput(
        "so", "Select Sexual Orientations (5 categories):",
        choices = unique(df$Sexual_Orientation), 
        selected = c("Heterosexual","Homosexual","Bisexual"),
        multiple = TRUE
      ),
      
      ############################
      # Year
      selectInput(
        "years", "Select Years: 2015-2023 (dropdown menu)",
        choices = unique(df$Year), 
        selected = c(2017,2019,2021,2023),
        multiple = TRUE
      ),
      
      ############################
      # Switch Buttons for x axis, facet row, and download button
      input_switch("x", "x-axis = Year (vs Division)", value = TRUE, width = NULL),
      input_switch("row", "Rows = Race (vs sex orient.)", value = TRUE, width = NULL),
      downloadButton('downloadData', 'Download Data')
    ),
    
    ############################
    ############################
    # Navigation Bar
      page_navbar(            
        nav_panel("Figure",
                  ########
                  # Static Plot
                  plotOutput(outputId = "line")
                  ),
        ########         
        # Interactive Plotly  Tab
        nav_panel("Interact",
                           # Show a plot of the generated distribution
                           plotlyOutput(outputId = "linely")
                  ),
        ########
        # Table Tab
        nav_panel("Table",
                  # Show a plot of the generated distribution
                  reactableOutput("table")
        ),
        
        ########
        # Details Tab
        nav_panel("Details",
                           # Show a plot of the generated distribution
                           htmlOutput('Title'),
                           htmlOutput('source'),
                           htmlOutput('foot'),
                          imageOutput("image1"),
                          htmlOutput('foot2'),
                          imageOutput("image2")


      )
    )
    
  )
  
)


############################
# Start Server
server <- function(input, output, session) {
  
  
  ############################
  # Render lgbt map image
  
  output$image1 <- renderImage( 
    { 
      list(src = "LGBTQ5_map_Fig.png", height = "75%") 
    }, 
    deleteFile = FALSE 
  )
  
  
  Foot2="<b>B) 2020 Negative Racial sentiment Quartiles from <a href='https://doi.org/10.1097/EDE.0000000000001671'>BD4HE</b>.</i>"
  output$foot2=renderText(paste("<p'>",Foot2,"</p>"))
  
  output$image2 <- renderImage( 
    { 
      list(src = "RaceSent_map_Fig.png", height = "75%") 
    }, 
    deleteFile = FALSE 
  )
  
  ############################
  # Subset Function for Figures
  subsetted <- reactive({
    req(input$races, input$so,input$gender, input$years,input$Outcome,input$division)
    
    df <- df |> filter(Race %in% input$races) |> 
      filter(Sexual_Orientation %in% input$so) |>
      filter(Gender %in% input$gender) |>
      filter(outcome %in% input$Outcome) |>
      filter(Geographic %in% input$division) |>
      filter(Year %in% input$years) 
    
    df$Year=factor(df$Year)
    df$state=as.factor(df$state)
    
    df$Race=as.factor(df$Race)
    df$Sexual_Orientation=as.factor(df$Sexual_Orientation)
    
    if ("White" %in% input$races){
      df <- within(df, Race <- relevel(Race, ref = "White"))
    }
    
    if ("Heterosexual" %in% input$so){
      df <- within(df, Sexual_Orientation <- relevel(Sexual_Orientation, ref = "Heterosexual"))
    }
    
    
  })
  
  
  ############################
  # Subset Function for Table
  subTable <- reactive({
    req(input$races, input$so,input$gender, input$years,input$Outcome,input$division)
    
    names(df)[10]="Division"
    names(df)[12]="Outcome"
    names(df)[2]="Percent"
    names(df)[3]="CI.low"
    names(df)[4]="CI.high"
    names(df)[6]="Sexuality"
    names(df)[11]="Geo.Factor"
    
    df <- df |> filter(Race %in% input$races) |> 
      filter(Sexuality %in% input$so) |>
      filter(Gender %in% input$gender) |>
      filter(Outcome %in% input$Outcome) |>
      filter(Geo.Factor %in% input$division) |>
      filter(Year %in% input$years) 
      
    df=df[,c(1,10,2:7,12,11)]
    
    
  })
  ############################
  # Render Title
  output$Title=renderText("<p><b>Question from CDC's <a href='https://www.cdc.gov/yrbs/index.html'>Youth Risk Behavior Survey</a>:</b></p>")
  
  ############################
  # Render Text for details
  source <- observe({
    output$source=renderText(paste0("<p'>",Questions,"</p>"))
  })
  
  ############################
  # Render Table
  output$table <- renderReactable({reactable(subTable())})
  
  ############################
  # Render Text for Details about IMAIHDA, LGBT Maps, Race Sent
  output$foot=renderText(paste("<p'>",Details,"</p>"))
  
  
  ############################
  # Render Race sent image

  ### GGPLOT ####
  line <- observe({

    GVar = GenderVars |> filter(Gender %in% input$gender)


    if (input$row){
      Colors = SoColor |> filter(So %in% input$so)
    } else {
      Colors = RaceColor |> filter(Race %in% input$races)
      Vars$ColorVar="Race"
      Vars$ColorVarLab="Race"
      Vars$Group="strata"
      Vars$RowVar="Sexual_Orientation"
    }

    if (!input$x){

      Vars$X="state"
      Vars$ColumnVar="Year"
      Vars$Angle=45

    }
    
      output$line <- renderPlot({
      p <- ggplot(subsetted(), aes( x=eval(parse(text=Vars$X)) , y=predicted, shape=Gender, color=eval(parse(text=Vars$ColorVar)) ) ) +
        theme_light() +
        facet_grid(vars( eval(parse(text=Vars$RowVar)) ), vars( eval(parse(text=Vars$ColumnVar)) )) +
        theme(strip.text.y = element_text(size = 10,face="bold",color="black")) +
        theme(strip.text.x = element_text(size = 10,face="bold",color="black")) +
        theme(strip.background =element_rect(fill="white")) +
        geom_line(aes(group= eval(parse(text=Vars$Group)) , linetype=Gender)) +
        geom_point(size=.8) +
        geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
        theme(axis.text.y=element_text(size=10,face="bold")) +
        theme(axis.text.x = element_text(size=10,face="bold",angle=Vars$Angle,vjust = 1,hjust = 1)) +
        scale_shape_manual(values = GVar$Shape) +
        scale_linetype_manual(values=GVar$Line)+
        xlab( Vars$X ) +
        ylab("% YES (predicted probability)") +
        scale_color_manual(values = Colors$Color) +
        ggtitle(input$Outcome) + 
        theme(legend.position="top") + 
        labs(color=Vars$ColorVarLab) +
        theme(plot.title = element_text( size=14, face="bold",hjust=.5))
      p
    }, res = 100)
    
})
  
  
  ### GGPLOTLY ####
  linely <- observe({
    # output$linely <-   observe({
    GVar = GenderVars |> filter(Gender %in% input$gender)

    if (input$row){
      Colors = SoColor |> filter(So %in% input$so)
    } else {
      Colors = RaceColor |> filter(Race %in% input$races)
      Vars$ColorVar="Race"
      Vars$ColorVarLab="Race"
      Vars$Group="strata"
      Vars$RowVar="Sexual_Orientation"
    }

    if (!input$x){

      Vars$X="state"
      Vars$ColumnVar="Year"
      Vars$Angle=45

    }
    
    output$linely <-renderPlotly({
      # eval(parse(text=Vars$ColorVar))
      p <- ggplot(subsetted(), aes( x=eval(parse(text=Vars$X)) , y=predicted, shape=Gender, color=eval(parse(text=Vars$ColorVar)) )) +
        theme_light() +
        facet_grid(vars( eval(parse(text=Vars$RowVar)) ), vars( eval(parse(text=Vars$ColumnVar)) )) +
        theme(strip.text.y = element_text(size = 10,face="bold",color="black")) +
        theme(strip.text.x = element_text(size = 10,face="bold",color="black")) +
        theme(strip.background =element_rect(fill="white")) +
        geom_line(aes(group = eval(parse(text=Vars$Group)) , linetype=Gender)) +
        geom_point(size=.8) +
        geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
        theme(axis.text.y=element_text(size=10,face="bold")) +
        theme(axis.text.x = element_text(size=10,face="bold",angle=Vars$Angle,vjust = 1,hjust = 1)) +
        scale_shape_manual(values = GVar$Shape) +
        scale_linetype_manual(values=GVar$Line)+
        xlab( Vars$X ) +
        ylab("% YES (predicted probability)") +
        scale_color_manual(values = Colors$Color) +
        ggtitle(input$Outcome,element_text( size=12,vjust=1)) + 
        theme(legend.position="top") + 
        labs(color=Vars$ColorVarLab) +
        guides(shape = "legend") + 
        guides(group="legend") + 
        guides(Sexual_Orientation = "legend") +
        guides(Gender = "legend") +
        guides(linetype ="none")   +
      theme(plot.title = element_text( size=12, face="bold",hjust=.5,vjust=0))
      
      # style(p,text=row(predicted))
      
      ggplotly(p,tooltip = c("predicted","conf.low","conf.high")) %>% 
        plotly::layout(legend=list(orientation='h',y=1.25))  %>%
        plotly::config(displaylogo = FALSE,modeBarButtonsToRemove = c("lasso2d","select2d") ) 
      
    }
    )
    
    
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(subsetted(), file,row.names = FALSE)
      
    })
  
}

shinyApp(ui, server)