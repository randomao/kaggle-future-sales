
### Script for final project
# Preliminaries
library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(data.table)
library(magrittr)
library(dplyr)

load('timeplot_data.Rdata')

### ======================================================================================= ###
# server
server <- function(input, output, session){

### ================================================================================== ###  
### Time series plots
  # Aggregate data using selected grouping
  plot1_data <- reactive({
    
    # Group shops/items by average sales over entire time period
    group_data <- timeplot_data[, lapply(.SD, mean, na.rm = TRUE)
                                , by = c(input$group_var1)
                                , .SDcols = c(input$group_var2)]
    names(group_data)[2] <- 'group_var'
    group_data[, group := ntile(group_var, input$num_groups)]
    
    # Join group names back onto raw data
    timeplot_data <- merge(timeplot_data, group_data, by = c(input$group_var1))
    
    if(input$disp_var == 'num_distinct'){
      if(input$group_var1 == 'shop_id'){
        agg_data <- timeplot_data[, .(num_distinct = length(unique(item_id)))
                                  , by = .(group, year_month)]
      }
      if(input$group_var1 == 'item_id'){
        agg_data <- timeplot_data[, .(num_distinct = length(unique(shop_id)))
                                  , by = .(group, year_month)]
      }
      
    }else{
      # Aggregate on the display variable, over months and groups
      agg_data <- timeplot_data[, lapply(.SD, mean, na.rm = TRUE)
                                , by = .(group, year_month)
                                , .SDcols = c(input$disp_var)]
    }
    
    names(agg_data)[3] <- 'disp_var'
    agg_data[, year_month1 := as.Date(paste(year_month, '01'), format = '%Y%m%d')]
    
    return(agg_data)
  })
  
  # Plot the displayed variable over time, grouped by grouping variables
  output$plot1 <- renderPlot({
    ggplot(data = plot1_data()
           , aes(x = year_month1, y = disp_var, colour = factor(group))) + 
      geom_line() +
      labs(x = 'Month', y = input$disp_var)
  })

### ================================================================================== ###    
### Correlation plots
  # Create a scatter plot using selected x and y axes
  plot2_data <- reactive({
    agg_data <- timeplot_data[, lapply(.SD, mean, na.rm = TRUE)
                              , by = c(input$group_var3)
                              , .SDcols = c(input$x_var, input$y_var)]
    return(agg_data)
  })
  
  
  output$plot2 <- renderPlot({
    ggplot(data = plot2_data()
           , aes_string(x = input$x_var, y = input$y_var)) +
      geom_point()
  })
  
  
}

### ======================================================================================= ###
# ui
ui <- fluidPage(
  themeSelector()
  
  , titlePanel(strong('Visualisation of Sales Data'))
  
  , navbarPage(title = 'Tabs'
               
               # Tab 1: Plot variables over time
               , tabPanel('Changes over Time'
                          
                          , sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = 'group_var1'
                                          , label = 'Select Variable to Group By'
                                          , choices = c('Shop' = 'shop_id'
                                                        , 'Item' ='item_id')
                                          , selected = 'shop_id')
                              
                              , selectInput(inputId = 'group_var2'
                                            , label = 'Select Variable to Group On'
                                            , choices = c('Volume' = 'volume'
                                                          , 'Average Price' = 'avg_price'
                                                          , 'Revenue' = 'revenue')
                                            , selected = 'volume')
                              
                              , selectInput(inputId = 'disp_var'
                                            , label = 'Select Variable to Display'
                                            , choices = c('Volume' = 'volume'
                                                          , 'Average Price' = 'avg_price'
                                                          , 'Revenue' = 'revenue'
                                                          , 'Number of Items/Shops' = 'num_distinct')
                                            , selected = 'volume')
                              
                              , sliderInput(inputId = 'num_groups'
                                            , label = 'Number of Groups'
                                            , value = 5
                                            , min = 1, max = 10, step = 1)
                            )
                            , mainPanel(
                              plotOutput(outputId = "plot1")
                            )
                          )
               )
               
               , tabPanel('Correlation between Variables'
                          , sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = 'x_var'
                                          , label = 'Select variable for x-axis'
                                          , choices = c('Volume' = 'volume'
                                                        , 'Average Price' = 'avg_price'
                                                        , 'Revenue' = 'revenue')
                                          , selected = 'volume')
                              
                              , selectInput(inputId = 'y_var'
                                            , label = 'Select variable for y-axis'
                                            , choices = c('Volume' = 'volume'
                                                          , 'Average Price' = 'avg_price'
                                                          , 'Revenue' = 'revenue')
                                            , selected = 'avg_price')
                              
                              , selectInput(inputId = 'group_var3'
                                            , label = 'Select variable to group by'
                                            , choices = c('Shop' = 'shop_id'
                                                          , 'Item' = 'item_id'
                                                          , 'Month' = 'year_month')
                                            , selected = 'shop_id')
                            )
                            , mainPanel(plotOutput(outputId = 'plot2'))
                          ))
  )
)

### ======================================================================================= ###
# Shiny App

shinyApp(ui = ui, server = server)