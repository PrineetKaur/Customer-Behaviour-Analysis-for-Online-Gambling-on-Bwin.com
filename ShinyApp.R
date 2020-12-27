
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("DT")) install.packages("DT"); library("DT")
if(!require("plotly")) install.packages("plotly"); library("plotly")
if(!require("shinythemes")) install.packages("shinythemes"); library("shinythemes")
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("shinyWidgets")) install.packages("shinyWidgets"); library("shinyWidgets")
if(!require("fBasics")) install.packages("fBasics"); library("fBasics")
if(!require("foreign")) install.packages("foreign"); library("foreign")
if(!require("stringr")) install.packages("stringr"); library("stringr")
if(!require("rsconnect")) install.packages("rsconnect"); library("rsconnect")
#if(!require("devtools")) install.packages("devtools"); library("rsconnect")
#devtools::install_github("rstudio/rsconnect")


Datamart<- read.csv("Datamart.csv")
Ggender<-unique(Datamart$Gender)
Ggender<-Ggender[-3]
GApp<-unique(Datamart$ApplicationName)
ui <- fluidPage(
  #THEME OF THE PROJECT
  theme = shinytheme("superhero"),
  #TITLE OF THE PROJECT
  titlePanel(titlePanel("Gambling Project :")),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("gender","Select the gender:",Ggender,selected=Ggender),
      
      pickerInput("Application","Select the applications:",GApp,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE), selected=GApp),
      
      pickerInput("Country","Select the Country:",GCountry,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE),selected=GCountry)
      #Filter Segments
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  id= "tabsetpanel", 
                  
                  tabPanel(title = "User Information",
                           splitLayout(plotOutput("Gender_new"),plotOutput("Age_User"))),
                  
                  tabPanel(title = "Player Distribution by Language",
                           plotOutput("language")),
                  
                  tabPanel(title = "Player Distribution by Country",
                           plotOutput("country")),
                  
                  tabPanel(title="Player Distribution by Application",
                           plotOutput("distrib")),
                  
                  tabPanel(title = "Analysing Customer Loyalty Trends",
                           plotOutput("Seg")),
                  
                  tabPanel(title="Activation versus Loyalty",
                           plotOutput("ActivationVsLoyalty")),
                  
                  tabPanel(title = "Best Play Days per Product",
                           splitLayout(cellWidths = c("50%","50%"),plotOutput("poker"),plotOutput("games")),splitLayout(cellWidths = c("50%","50%"),plotOutput("casino"),plotOutput("sports"))),
                  
                  tabPanel(title='Best Play Months per Product',
                           splitLayout(cellWidths = c("50%","50%"),plotOutput("poker1"),plotOutput("games1")),splitLayout(cellWidths = c("50%","50%"),plotOutput("casino1"),plotOutput("sports1")))
                  
                  
                  
                  
                  
      )
    )
  )
)
server <- function(input, output){
  output$Seg <- renderPlot({ 
    
    data<-Datamart %>% dplyr::filter (!is.na(Segments),Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country)
    data <- within(data, 
                   Segments <- factor(Segments, 
                                      levels=names(sort(table(Segments), 
                                                        decreasing=TRUE))))
    ggplot(data,aes(x = Segments, fill = Segments)) +
      geom_bar() + 
      scale_fill_manual(values = c("#663300", "#CCCCCC", "#FFFFFF", "#FFCC33"))+ 
      labs(title = "Segments were derived as per Customer Loyalty", 
           subtitle = "(Bronze<Silver<Gold<Platinum)",
           x = "Segments", y = "Count of Customers") 
    
    
  })
  output$ActivationVsLoyalty<- renderPlot({
    data2<- Datamart %>% dplyr::filter (!is.na (Activation_Period), !is.na (Days_Ttl),Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country) %>% select (UserID, Activation_Period, Days_Ttl)
    ggplot(data2,aes(x = Days_Ttl, y = Activation_Period))+
      geom_tile() +
      xlab("Loyalty (Total no. of Days Played)") + 
      ylab("Time Delay from Registration and First Payment (Activation Period)")
  })
  output$Gender_new<- renderPlot({
    data3 <- Datamart %>% dplyr::filter (Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country)
    ggplot(data3,aes(x = Gender, fill = Gender)) +
      geom_bar() + ggtitle("Gender Distribution For Customers")+
      ylab("Count of Customers")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  })
  output$Age_User<- renderPlot({
    data4<- Datamart %>% dplyr::filter(Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country)
    ggplot(data4,aes(x = Age)) + ggtitle("Age Distribution For Customers")+
      geom_histogram(binwidth = 2,color="white", fill="black") + 
      ylab("Count of Customers")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  })
  output$poker<- renderPlot({
    data5<-Datamart %>% dplyr::filter (Most_Played_Day != 0,Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country) %>% select (UserID, Most_Played_Day)
    ggplot(data5,aes(x = Most_Played_Day, fill = Most_Played_Day)) +
      geom_bar() +
      coord_polar() +
      theme_void()+ ggtitle("Poker")+ theme(plot.title = element_text(hjust = 0.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  })
  
  output$games<- renderPlot({
    data6<-Datamart %>% dplyr::filter (Most_Played_Day_Ga != 0,Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country) %>% select (UserID, Most_Played_Day_Ga)
    ggplot(data6,aes(x = Most_Played_Day_Ga, fill = Most_Played_Day_Ga)) +
      geom_bar() +
      coord_polar() +
      theme_void()+ ggtitle("Games")+ theme(plot.title = element_text(hjust = 0.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  })
  output$casino<- renderPlot({
    data7<- Datamart %>%dplyr::filter (Most_Played_Day_Ca != 0,Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country) %>%select (UserID, Most_Played_Day_Ca)
    ggplot(data7,aes(x = Most_Played_Day_Ca, fill = Most_Played_Day_Ca)) +
      geom_bar() +
      coord_polar() +
      theme_void()+ ggtitle("Casino")+ theme(plot.title = element_text(hjust = 0.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
  })
  output$sports<-renderPlot({
    data8<-Datamart %>%dplyr::filter (Most_Played_Day_Sp != 0,Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country) %>%select (UserID, Most_Played_Day_Sp)
    ggplot(data8,aes(x = Most_Played_Day_Sp, fill = Most_Played_Day_Sp)) +
      geom_bar() +
      coord_polar() +
      theme_void() + ggtitle("Sports")+ theme(plot.title = element_text(hjust = 0.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  })
  output$poker1<- renderPlot({
    data9<-Datamart %>% dplyr::filter (Most_Played_Month != 0,Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country) %>% select (UserID, Most_Played_Month)
    ggplot(data9,aes(x = Most_Played_Month, fill = Most_Played_Month)) +
      geom_bar() +
      coord_polar() +
      theme_void()+ ggtitle("Poker")+ theme(plot.title = element_text(hjust = 0.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  })
  
  output$games1<- renderPlot({
    data10<-Datamart %>% dplyr::filter (Most_Played_Month_Ga != 0,Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country) %>% select (UserID, Most_Played_Month_Ga)
    ggplot(data10,aes(x = Most_Played_Month_Ga, fill = Most_Played_Month_Ga)) +
      geom_bar() +
      coord_polar() +
      theme_void()+ ggtitle("Games")+ theme(plot.title = element_text(hjust = 0.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  })
  output$casino1<- renderPlot({
    data11<- Datamart %>%dplyr::filter (Most_Played_Month_Ca != 0,Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country) %>%select (UserID, Most_Played_Month_Ca)
    ggplot(data11,aes(x = Most_Played_Month_Ca, fill = Most_Played_Month_Ca)) +
      geom_bar() +
      coord_polar() +
      theme_void()+ ggtitle("Casino")+ theme(plot.title = element_text(hjust = 0.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
  })
  output$sports1<-renderPlot({
    data12<-Datamart %>%dplyr::filter (Most_Played_Month_Sp != 0,Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country) %>%select (UserID, Most_Played_Month_Sp)
    ggplot(data12,aes(x = Most_Played_Month_Sp, fill = Most_Played_Month_Sp)) +
      geom_bar() +
      coord_polar() +
      theme_void() + ggtitle("Sports")+ theme(plot.title = element_text(hjust = 0.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
  })
  output$language<- renderPlot({
    data12<-Datamart %>%dplyr::filter (LanguageName!="NA",Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country)
    data13<-within(data12, 
                   LanguageName <- factor(LanguageName, 
                                          levels=names(sort(table(LanguageName), 
                                                            decreasing=TRUE))))
    ggplot(data13,aes(x = LanguageName, fill = LanguageName)) +
      geom_bar() +
      ylab("Count of Customers") + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ggtitle("Top Languages With Maximum Customers")+ theme(plot.title = element_text(hjust = 0.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
    
  })
  output$country<- renderPlot({
    data14 <-Datamart %>%dplyr::filter (CountryName!="NA",Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country)
    data15 <- within(data14, 
                     CountryName <- factor(CountryName, 
                                           levels=names(sort(table(CountryName), 
                                                             decreasing=TRUE))))
    data15<-head(data15,8)
    ggplot(data15, aes(x = CountryName, fill = CountryName)) +
      geom_bar() +
      ylab("Count of Customers") + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ggtitle("Top 5 Countries With Maximum Customers")+ theme(plot.title = element_text(hjust = 0.5))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
  })
  output$distrib <- renderPlot({
    data16<-Datamart %>%dplyr::filter (Gender %in% input$gender,ApplicationName%in%input$Application, CountryName%in%input$Country)
    data16<-within(data16, 
                   ApplicationName <- factor(ApplicationName, 
                                             levels=names(sort(table(ApplicationName), 
                                                               decreasing=TRUE))))
    ggplot(data16,aes(x = ApplicationName, fill = ApplicationName)) +
      geom_bar() +
      ylab("Count of Customers") + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))
    
    
    
  })
  
}

# Run shny app 
shinyApp(ui = ui, server = server)