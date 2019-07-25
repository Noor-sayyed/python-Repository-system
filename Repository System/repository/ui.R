library(ggplot2)
library(shiny)
library(jsonlite)
library(markdown)
repo <- read.csv("repository.csv", stringsAsFactors = FALSE)

sales <- read.csv("Sales.csv", stringsAsFactors = FALSE)
shinyUI(fluidPage(
  tags$head(tags$script(src = "message-handler.js")),
  navbarPage(
    "Repository",
    tabPanel("Home", sidebarLayout(
      sidebarPanel(
        selectInput("product", label = h5("Product:"), c("All", unique(
          as.character(repo$product)
        ))),
        selectInput("quantity", label = h5("Quantity"), c("All", unique(
          as.character(repo$quantity)
          
        ))),
        selectInput("totalprice", label = h5("Price:"), c("All", unique(
          as.character(repo$totalprice)
        )))
        
      ),
      mainPanel(tabsetPanel(
        type = "tabs",
        
        tabPanel("Table", DT::dataTableOutput(outputId =
                                                "table")),
        tabPanel(
          "Clusters",
          numericInput('clusters', 'Cluster count', 3,
                       min = 1, max = 9),
          plotOutput("clust")
        ),
        tabPanel("Charts", plotOutput("bar2"), hr(), plotOutput("bar1"))
      ))
    )),
    tabPanel("Add",
             sidebarLayout(
               sidebarPanel(
                 textInput("prod1",
                           "Product Name:",
                           "enter the name"),
                 
                 numericInput("size",
                              "size:",
                              0),
                 numericInput("quan1",
                              "quantity:",
                              0),
                 numericInput("price1",
                              "price:",
                              0),
                 
                 actionButton("updateddata", label = "Add Data")
                 
               ),
               mainPanel(DT::dataTableOutput(outputId = "table2"))
             )),
    
    tabPanel("Sales",
             mainPanel(
               tabsetPanel(
                 type = "tabs",
                 
                 tabPanel("SALES TABLE", DT::dataTableOutput(outputId = "prof")),
                 tabPanel(
                   "Linear regression prediction",
                   sidebarLayout(
                     sidebarPanel(
                       "PREDICTION::",
                       selectInput(
                         "predictproduct",
                         label = h5("Product name:"),
                         c("Select", unique(as.character(sales$productname)))
                       ),
                       numericInput(
                         "value",
                         "Enter the quantity",
                         100,
                         min = 100,
                         max = 90000
                       )
                     ),
                     mainPanel(
                       plotOutput("plot1"),
                       h4("PROFIT PREDICTION"),
                       textOutput("predictionvalue")
                     )
                   )
                 ),
                 tabPanel(
                   "GRAPHS",
                   plotOutput("graph"),
                   br(),
                   hr(),
                   hr(),
                   plotOutput("graph2")
                 ),
                 tabPanel("PIE CHART", plotOutput("pie2")),
                 tabPanel(
                   "CLUSTERS",
                   numericInput('cltrs', 'Cluster count', 3,
                                min = 1, max = 9),
                   plotOutput("clust2")
                 )
               )
             )),
  
    inverse = TRUE
  )
  # Application title
  
  
  # Sidebar with a slider input for number of bins
  
))
