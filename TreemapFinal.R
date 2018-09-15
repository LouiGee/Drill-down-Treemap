library(shiny)
library(readxl)
library(treemap)
library(htmltools)
library(htmlwidgets)
library(data.tree)
library(d3treeR)
library(lubridate)
#---TreemapCode---



#---UI---

ui <- fluidPage(
  
  titlePanel("Visualising number of orders recieved and their value by Country"),
  
  verticalLayout(dateInput("dinput",
                           "Select day:",
                            value = "2018-08-18",
                            min = "2018-08-18",
                            max = "2018-08-19",
                            format = "yyyy-mm-dd", 
                            autoclose = TRUE),
  
  textOutput('Orders')),
  

 
  d3tree2Output("au_tree_map")
  
)


#---Server---

server <- function(input,output) {
  
  dataset <- read.csv('Treemap_data.csv')
  dataset$Date <- as.Date(as.character(dataset$Date))
  
  dataset$BAL <- paste(dataset$BAL, "BAL", sep=":")
  dataset$SAL <- paste(dataset$SAL, "SAL", sep=":")
  dataset$SCH <- paste(dataset$SCH, "SCH", sep=":")
  
  field <- c("Country", "Value")
  dataset$countryvalue <- do.call("paste", c(dataset[field], sep = " ")
  )
  field <- c("BAL", "SAL", "SCH")
  dataset$countrybreakdown <- do.call("paste", c(dataset[field], sep = " ")
  )
  
  
  S <- reactive({subset(dataset, dataset$Date == as.Date(input$dinput))})

  output$au_tree_map <- renderD3tree2({
    
    
    d3tree(Treemap <- treemap(S(),
                              index=c("countryvalue","countrybreakdown"),
                              vSize="Orders",
                              vColor="Orders",
                              palette = "Blues",
                              type="value"),
           rootname = "Treemap")})
  
    
  output$Orders <- renderText({print(paste("Total Orders:", sum(S()$Orders)))})
  
}

shinyApp(ui = ui, server = server)
