#Load libraries
library(ISLR)
library(ggplot2)
library(glmnet)
library(caret)
library(faraway)
library(shinythemes)
library(psych)

myFile <- "C:/Users/karth/Desktop/New/Purdue/Courses/R for Analytics/Project/Final/Final.csv"
data1 <- read.table(file=myFile, header=T, sep=",")
myFile12 <- "C:/Users/karth/Desktop/New/Purdue/Courses/R for Analytics/Project/Final/Original.csv"
data12 <- read.table(file=myFile12, header=T, sep=",")
TOTALSCORE <- c(0,0,0,0,0,0)
YEAR <- c(0,0,0,0,0,0)
j=1
library(shiny)
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  tags$h4("University Rank Predictor", align = "center", style = "color:vanila"),
  selectInput(inputId = "num",
              label = "Select a University",
              data1[ , 2]),
  tableOutput("table"),
  textOutput("rank"),
  textOutput("score"),
  sidebarPanel(uiOutput("slider1"),
               uiOutput("slider2"),
               uiOutput("slider3"),
               uiOutput("slider4"),
               uiOutput("slider5"),
               uiOutput("slider6"),
               actionButton(inputId = "go",
                            label = "Update")),
  
  mainPanel(
    textOutput("percent"),
    textOutput("percent1"),
    textOutput("percent2"),
    textOutput("percent3"),
    textOutput("percent4"),
    textOutput("percent5"),
    textOutput("percent6"),
    textOutput("dot2"),
    plotOutput("hist"),
    textOutput("dot1")),
  textOutput("pred2"),
  uiOutput("pred1")
)
server <- function(input, output) {
  output$table=renderTable({data1[data1$University_Name==input$num,1:13]})
  data <- eventReactive(input$go, {
    a=(28.131+0.204*input$slider_a + 0.211*input$slider_b + 0.117*input$slider_c + 0.218*input$slider_d + 0.048*input$slider_e + 0.106*input$slider_f)
    if(a>100)
      a=100
    print(a)
  })
  
  beta <- eventReactive(input$go,
                        
                        { counter=1 
                        for(i in data1$Total_Score){
                          if((28.131+0.204*input$slider_a + 0.211*input$slider_b + 0.117*input$slider_c + 0.218*input$slider_d + 0.048*input$slider_e + 0.106*input$slider_f)<i)
                            counter <- counter+1
                        }
                        if ((28.131+0.204*input$slider_a + 0.211*input$slider_b + 0.117*input$slider_c + 0.218*input$slider_d + 0.048*input$slider_e + 0.106*input$slider_f)< data1[data1$University_Name == input$num,9]){
                          print(counter-1)
                        }else{
                          print(counter)  
                        }
                        
                        }
                        
  )
  
  
  
  
  
  output$slider1 <- renderUI({
    sliderInput("slider_a","Teaching Rating",value = data1[data1$University_Name == input$num,4],min=0,max=100,step=0.1)
  })
  output$slider2 <- renderUI({
    sliderInput("slider_b","Research Rating",value = data1[data1$University_Name == input$num,6],min=0,max=100,step=0.1)
  })
  output$slider3 <- renderUI({
    sliderInput("slider_c","Citations Rating",value = data1[data1$University_Name == input$num,7],min=0,max=100,step=0.1)
  })
  output$slider4 <- renderUI({
    sliderInput("slider_d","Industrial Income Rating",value = data1[data1$University_Name == input$num,8],min=0,max=100,step=0.1)
  })
  output$slider5 <- renderUI({
    sliderInput("slider_e","Placement Rating",value = data1[data1$University_Name == input$num,11],min=0,max=100,step=0.1)
  })
  output$slider6 <- renderUI({
    sliderInput("slider_f","Percentage of International Students",value = data1[data1$University_Name == input$num,12],min=0,max=100,step=0.1)
  })
  
  
  output$dot1 <- renderText({
    paste(".")
  })
  output$pred1 <- renderUI({
    paste("The Predicted Overall Score is =", data())
  })
  output$pred2 <- renderText({
    paste("The Predicted World Rank is =", beta())
  })
  
  output$rank <- renderText({
    paste("Current World Rank is", data1[data1$University_Name == input$num,1])
  })
  output$score <- renderText({
    paste("Current Overall Score is", data1[data1$University_Name == input$num,9])
  })
  output$hist <- renderPlot({
    for(i in 1:dim(data12)[1]){
      if(input$num==data12[i,2]){
        TOTALSCORE[j] <- data12[i,9]
        YEAR[j] <- data12[i,13]
      }
      j <- j+1
    }
    TOTALSCORE[7]=data()
    YEAR[7]=2017
    m <- data.frame(TOTALSCORE,YEAR)
    ggplot(data=m,aes(x=YEAR,y=TOTALSCORE))+geom_point(data=m,aes(x=YEAR,y=TOTALSCORE),color="red",size=5)+geom_line()+coord_cartesian(xlim = c(2010,2018),ylim=c(40,100))
    
  })
  output$percent <- renderText({
    paste("Dependence of Total Score on various factors is as shown:")
  })
  output$percent1 <- renderText({
    paste("22.57%   Teacher Rating ")
  })
  output$percent2 <- renderText({
    paste("23.34% Research Rating ")
  })
  output$percent3 <- renderText({
    paste("12.94% Citations Rating ")
  })
  output$percent4 <- renderText({
    paste("24.11% Industry Income Rating ")
  })
  output$percent5 <- renderText({
    paste("5.31% Placement Rating ")
  })
  output$percent6 <- renderText({
    paste("11.73% Percentage of international students ")
  })
  output$dot2 <- renderText({
    paste(".")
  })
}
shinyApp(ui = ui, server = server)