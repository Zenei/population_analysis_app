
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
population.ug <- read.csv("Uganda Statistics for the past 20 years.csv")
popn <- subset(population.ug, select = c("Year",
                                      "Population",
                                      "Births",
                                      "Deaths",
                                      "Net.Migrants",
                                      "Natural.Increase",
                                      "Population.Change"))

library(shiny)
library(shinythemes)
library(plotly)
library(plyr)
library(tseries)
library(forecast)
library(DT)

shinyServer(function(input, output) {
  
  
  ####### START PAGE #############################
  
  # display 10 rows initially
  output$ex1 <- DT::renderDataTable(
    DT::datatable(popn, options = list(pageLength = 25))
  )
  
  # -1 means no pagination; the 2nd element contains menu labels
  output$ex2 <- DT::renderDataTable(
    DT::datatable(
      popn, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  )
  
  # you can also use paging = FALSE to disable pagination
  output$ex3 <- DT::renderDataTable(
    DT::datatable(popn, options = list(paging = FALSE))
  )
  
  # turn off filtering (no searching boxes)
  output$ex4 <- DT::renderDataTable(
    DT::datatable(popn, options = list(searching = FALSE))
  )
  
  # write literal JS code in JS()
  output$ex5 <- DT::renderDataTable(DT::datatable(
    popn,
    options = list(rowCallback = DT::JS(
      'function(row, data) {
        // Bold cells for those >= 5 in the first column
        if (parseFloat(data[1]) >= 5.0)
          $("td:eq(1)", row).css("font-weight", "bold");
      }'
    ))
  ))
  
  ################## TREND #####################################
  output$trend <- renderPlot({
    population <- popn$Population/1000000
    population.timeseries <- ts(population,
                                start = c(1996),
                                end = c(2016),
                                frequency = 1)
    
    plot(population.timeseries,
         xlab ="Years",
         ylab ="Population in Millions",
         main ="Population Trend",
         xlim = c(1996,2016))
  })
  
  ###################  FORECAST ###################################
  
  output$predict <- renderPlot({
    population <- data.frame(popn$Population/1000000)
    population.timeseries <- ts(population,
                                start = c(1996),
                                end = c(2016),
                                frequency = 1)
    
    future.population <- forecast(population.timeseries)
    
    plot(future.population,
         xlab ="Years",
         ylab ="Population in Millions",
         main ="Population Forecast")
    
  })
  
  
  ################## RELATIONSHIP #################################
  
  output$rel <- renderPlot({
    # Considering the equation: a = coef1*x^-1.5+coef2 #
    # Assuming initial coefficients to be 2 and 3      #
    
    Births  <- as.vector(popn$Births, mode = "numeric")
    Deaths  <- as.vector(popn$Deaths, mode = "numeric")
    plot(Births,Deaths, col="red", cex = 1.3, pch=16)
    
    #Relationship model
    relation <- lm(Deaths~Births)
    
    #Take the assumed values and fit into the model
    #model <- nls(Deaths ~ coef1*(Births^-1.5) + coef2, 
    #               start = list(coef1 = 2, coef2 = 3))
    
    #plot(Births,Deaths,
    #abline(model,Biths),cex = 1.3, pch=16,
    #xlab = "Biths",
    #ylab = "Deaths")
    
    # Plot the chart with new data 
    #By fitting it to a prediction from 100 data points.
    new.data <- data.frame(Births = seq(min(Births), 
                                        max(Births), 
                                        len=100))
    lines(new.data$Births, 
          predict(relation, newdata = new.data))
    
    # Get the sum of the squared residuals.
    #print(sum(resid(model)^2))
    # Get the confidence intervals on the chosen values of the coefficients.
    #print(confint(model))
  })
  
  ###################### Relationship Model ###########################
  
  output$value <- renderPrint({
    Births  <- as.vector(popn$Births, mode = "numeric")
    Deaths  <- as.vector(popn$Deaths, mode = "numeric")
    
    #Relationship model
    relation <- lm(Deaths~Births)
    
    if(input$b > 0){
    predict(relation, newdata = data.frame(Births = input$b))
    }
    
  })
  
  
  ######################## GROWTH #################################
  
  output$grow <- renderPlotly({
    growth <- as.vector(popn$Population,mode="numeric")
    years  <- as.vector(popn$Year)
    data <- data.frame(growth,years)
    
    plot_ly(data, 
            x = ~years,
            y = ~growth,
            type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          mode = 'lines+markers',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
      layout(title = "Population Growth",
             xaxis = list(title = "Years"),
             yaxis = list (title = "Population"))
  })
  
  
  ########################## IMPACT ################################
  
  output$imp <- renderPlotly({
    impact <- as.vector(popn$Population.Change,mode="numeric")
    years  <- as.vector(popn$Year)
    data <- data.frame(impact,years)
    
    plot_ly(data, 
            x = ~years,
            y = ~impact,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = 'rgb(205, 12, 24)', width = 2)) %>%
      layout(title = "Population Changes over Time",
             xaxis = list(title = "Years"),
             yaxis = list (title = "Population Changes"))

  })

  
  ######################## OTHER PLOTS #############################
  ###### Trend $Births
  output$trend.births <- renderPlot({
    births.timeseries <- ts(popn$Births,
                                start = c(1996),
                                end = c(2016),
                                frequency = 1)
    
    plot(births.timeseries,
         xlab ="Years",
         ylab ="Births",
         main ="Births Trend",
         xlim = c(1996,2016))
  })
  ###### Projection $Births
  output$future.births <- renderPlot({
    births.timeseries <- ts(popn$Births,
                                start = c(1996),
                                end = c(2016),
                                frequency = 1)
    
    p.births <- forecast(births.timeseries)
    plot(p.births,
         xlab ="Years",
         ylab ="Births",
         main ="Births Forecast")
    
  })
  
  
  ###### Trend $Deaths
  output$trend.deaths <- renderPlot({
    deaths.timeseries <- ts(popn$Deaths,
                            start = c(1996),
                            end = c(2016),
                            frequency = 1)
    
    plot(deaths.timeseries,
         xlab ="Years",
         ylab ="Deaths",
         main ="Deaths Trend",
         xlim = c(1996,2016))
  })
  ###### Projection $Deaths
  output$future.deaths <- renderPlot({
    deaths.timeseries <- ts(popn$Deaths,
                            start = c(1996),
                            end = c(2016),
                            frequency = 1)
    
    p.deaths <- forecast(deaths.timeseries)
    plot(p.deaths,
         xlab ="Years",
         ylab ="Deaths",
         main ="Deaths Forecast")
    
  })
  
  
  ###### Trend $Net Migrants
  output$trend.migrants <- renderPlot({
    migrants.timeseries <- ts(popn$Net.Migrants*-1/1000,
                            start = c(1996),
                            end = c(2016),
                            frequency = 1)
    
    plot(migrants.timeseries,
         xlab ="Years",
         ylab ="Net Migrants in Thousands",
         main ="Net Migrants Trend",
         xlim = c(1996,2016))
  })
  ###### Projection $Net Migrants
  output$future.migrants <- renderPlot({
    migrants.timeseries <- ts(popn$Net.Migrants*-1/1000,
                            start = c(1996),
                            end = c(2016),
                            frequency = 1)
    
    p.migrants <- forecast(migrants.timeseries)
    plot(p.migrants,
         xlab ="Years",
         ylab =" Net Migrants in Thousands",
         main ="Net Migrants Forecast")
    
  })
  
  
  ###### Trend $Population Change
  output$trend.change <- renderPlot({
    change.timeseries <- ts(popn$Population.Change,
                              start = c(1996),
                              end = c(2016),
                              frequency = 1)
    
    plot(change.timeseries,
         xlab ="Years",
         ylab ="Population Change",
         main ="Populations Changes Trend",
         xlim = c(1996,2016))
  })
  ###### Projection $Population Change
  output$future.change <- renderPlot({
    change.timeseries <- ts(popn$Population.Change,
                              start = c(1996),
                              end = c(2016),
                              frequency = 1)
    
    p.change <- forecast(change.timeseries)
    plot(p.change,
         xlab ="Years",
         ylab ="Population Change",
         main ="Population Changes Forecast")
    
  })
  
  
  ###### Trend $Natural Increase
  output$trend.increase <- renderPlot({
    increase.timeseries <- ts(popn$Natural.Increase,
                              start = c(1996),
                              end = c(2016),
                              frequency = 1)
    
    plot(increase.timeseries,
         xlab ="Years",
         ylab ="Natural Increase",
         main ="Natural Increase Trend",
         xlim = c(1996,2016))
  })
  ###### Projection $Natural Increase
  output$future.increase <- renderPlot({
    increase.timeseries <- ts(popn$Natural.Increase,
                            start = c(1996),
                            end = c(2016),
                            frequency = 1)
    
    p.increase <- forecast(increase.timeseries)
    plot(p.increase,
         xlab ="Years",
         ylab ="Natural Increase",
         main ="Natural Increase Forecast")
    
  })
  
  
  ###### Bar Graphs $Births
  output$bar.births <- renderPlotly({
    births <- as.vector(popn$Births)
    years  <- as.vector(popn$Year)
    data <- data.frame(births,years)
    
    plot_ly(data, 
            x = ~years,
            y = ~births,
            type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          mode = 'lines+markers',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
      layout(title = "Change in Births over Time",
             xaxis = list(title = "Years"),
             yaxis = list (title = "Births"))

  })
  
  ###### Bar Graphs $Deaths
  output$bar.deaths <- renderPlotly({
    deaths <- as.vector(popn$Deaths)
    years  <- as.vector(popn$Year)
    data <- data.frame(deaths,years)
    
    plot_ly(data, 
            x = ~years,
            y = ~deaths,
            type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          mode = 'lines+markers',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
      layout(title = "Change in Deaths over Time",
             xaxis = list(title = "Years"),
             yaxis = list (title = "Deaths"))

  })
  
  ###### Bar Graphs $Net Migrants
  output$bar.migrants <- renderPlotly({
    migrants <- as.vector(popn$Net.Migrants*-1/1000)
    years  <- as.vector(popn$Year)
    data <- data.frame(migrants,years)
    
    plot_ly(data, 
            x = ~years,
            y = ~migrants,
            type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          mode = 'lines+markers',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
      layout(title = "Change in Net Number of Migrants over Time",
             xaxis = list(title = "Years"),
             yaxis = list (title = "Net Migrants in Thousands"))

  })
  
  ###### Bar Graphs $Deaths
  output$bar.increase <- renderPlotly({
    increase <- as.vector(popn$Natural.Increase)
    years  <- as.vector(popn$Year)
    data <- data.frame(increase,years)
    
    plot_ly(data, 
            x = ~years,
            y = ~increase,
            type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          mode = 'lines+markers',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>%
      layout(title = "Natural Increases in Population over Time",
             xaxis = list(title = "Years"),
             yaxis = list (title = "Natural Increase"))

  })



})
