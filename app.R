library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(GGally)
library(plotly)

# Load data
SGD_data_1 <- read.csv("SGD_S.csv")
SGD_data_2 <- read.csv("SGD_SV.csv")
ER_data_1 <- read.csv("ER_S.csv")
ER_data_2 <- read.csv("ER_SV.csv")
DT_data_1 <-read.csv("DT_S.csv")
DT_data_2 <- read.csv("DT_SV.csv")
ANN_data_1 <- read.csv("ANN_S.csv")
ANN_data_2 <- read.csv("ANN_SV.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cerulean"),

    # Application title
    titlePanel("Injury Risk Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Select the time frequency
            selectInput(inputId = "Algorithm", label = strong("Algorithms"), 
                        choices = c("Elastic-Net", "Decision Trees", "Stochastic Gradient Boosting", "Neural Netwoeks"),
                        selected = "Elastic-Net"),
            selectInput(inputId = "Variable", label = strong("Variables Types"), 
                        choices = c("Static Variables", "Static and Velocity Variables")
                        ), 
            actionButton("go", "Submit")
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("linePlot"),
           tableOutput("view")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Dataset selection
    sub_data <- eventReactive(input$go, {
        if(input$Algorithm == "Elastic-Net"){
            if(input$Variable == "Static Variables"){
                filter_data <- ER_data_1
            }
            else{
                filter_data <- ER_data_2
            }
        }
        if(input$Algorithm == "Decision Trees"){
            if(input$Variable == "Static Variables"){
                filter_data <- DT_data_1
            }
            else{
                filter_data <- DT_data_2
            }            
        }
        if(input$Algorithm == "Stochastic Gradient Boosting"){
            if(input$Variable == "Static Variables"){
                filter_data <- SGD_data_1
            }
            else{
                filter_data <- SGD_data_2
            }            
        }        
        if(input$Algorithm == "Neural Netwoeks"){
            if(input$Variable == "Static Variables"){
                filter_data <- ANN_data_1
            }
            else{
                filter_data <- ANN_data_2
            }            
        }
        
        return(filter_data)
        
    })

    output$linePlot <- renderPlot({
        ggplot(data = sub_data(), aes(time_frequency_month, RMSE, col = "red")) + 
            geom_line() + 
            geom_point(col = "black", size = 1.3) +
            ylim(0.2, 0.8) +
            theme_economist() + 
            theme(legend.position = "null", plot.title = element_text(size = 15,hjust = 0.5), axis.text.x = element_text(angle = 0))
    })
    
    output$view <- renderTable({
        sub_data()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)