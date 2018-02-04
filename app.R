library(shiny)
library(DT)
library(titanic)
library(randomForest)

ui <- fluidPage(
  
  DT::dataTableOutput("mytable"),
  checkboxInput("checkbox" , label = "age", value = FALSE),
  checkboxInput("checkbox" , label = "class", value = FALSE),
  checkboxInput("checkbox" , label = "station", value = FALSE),
  actionButton("runRF", "Predict"),
  plotOutput("plotRF")
)

server <- function(input, output, session) {
  
  output$mytable = DT::renderDataTable({
    titanic_train
  })
  
  observeEvent(input$runRF, {
 
    #This should be filled with input, not working yet
    var = c("Pclass")
     
    fit <- randomForest(as.factor(Survived) ~ Pcalss, data = titanic_train, importance = TRUE, ntree=2000)
    prediction <- as.numeric(predict(fit, titanic_test))
    titanic_test$predicted <- prediction  
    
    output$plotRF <- renderPlot({
      hist(prediction)
    })
  })
}

shinyApp(ui, server)