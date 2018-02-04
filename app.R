library(shiny)
library(dplyr)
library(DT)
library(titanic)
library(randomForest)

ui <- fluidPage(
  
  DT::dataTableOutput("mytable"),
  checkboxGroupInput("myselection", "select columns", choices = c("Pclass", "Sex"), inline = TRUE),
  actionButton("runRF", "Predict"),
  plotOutput("outcome")

)
server <- function(input, output, session) {
  
  output$mytable = DT::renderDataTable({
    titanic_train
  })
  
  
  observeEvent(input$runRF, {
 
    titanic_train <- titanic_train[complete.cases(titanic_train),]
    titanic_train$Sex <- as.factor(titanic_train$Sex)
    
    sample <- sample.int(n = nrow(titanic_train), size = floor(.75*nrow(titanic_train)), replace = F)
    t_train <- titanic_train[sample,]
    t_test <- titanic_train[-sample,]
    
    fit <- randomForest(x = t_train[, "Sex", drop = FALSE],
     y = as.factor(t_train$Survived), importance = TRUE, ntree = 2000)
    prediction <- as.numeric(predict(fit, t_test))
    t_test$predicted <- prediction  
    t_test$correct <- as.numeric(ifelse(t_test$predicted == t_test$Survived, 1, 0))
    perc <- sum(t_test$correct)/nrow(t_test)
    
    df_graph = data.frame(id = "perc", acc = perc, n = 1)
    
    output$outcome <- renderPlot({
      as.character(perc)
      
      ggplot(df_graph) +
        geom_bar(aes(id, n), fill ="goldenrod2", stat ="identity", width = 0.5, alpha = 0.2) +
        geom_bar(aes(id, acc), fill ="black", stat ="identity", width = 0.2, alpha = 0.2) +
        ggtitle(paste0("The accuracy using this algorithm is: ", perc, " for ", nrow(t_test), "observations."))
        
    })
  })
}

shinyApp(ui, server)