
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(plotly)
library(plyr)
library(tseries)
library(forecast)
library(DT)

shinyUI(
navbarPage("",
  theme = shinytheme("cerulean"),
           
  tabPanel("Start",
     sidebarLayout(
       sidebarPanel(
         h5("INTRODUCTION"),
         hr(),
         p("Since 1996 to 2016 the population of Uganda has been gradually increasing as a result of the births and migrants in the country, using mathematically tabular values. This system forcuses on 
           analyzing the effect of births, deaths and migrants on the population of Uganda using graphical representation.")
       ),
       mainPanel(
         h1("Population Analysis System"),
         hr(),
         br(),
         h4("TABLE FOR ANALYZED POPULATION STATISTICS IN UGANDA FOR 21 YEARS"),

         tabsetPanel(
           tabPanel('Display length',     DT::dataTableOutput('ex1')),
           tabPanel('Length menu',        DT::dataTableOutput('ex2')),
           tabPanel('No pagination',      DT::dataTableOutput('ex3')),
           tabPanel('No filtering',       DT::dataTableOutput('ex4')),
           tabPanel('Function callback',  DT::dataTableOutput('ex5'))
         )
       )
     )


  ),
  
  tabPanel("Trend and Prediction",
     sidebarLayout(
       sidebarPanel(
         h5("Details"),
         hr(),
         p("After an analysis of the poplation for the past 21 years (1996 - 2016) we now use this data to make a future prediction on the population of Uganda for the coming 9 years.")
       ),
       mainPanel(
         h2("Trend of Population"),
         plotOutput("trend",   height = 350, width = 630),
         h2("Population Projections"),
         plotOutput("predict", height = 350, width = 630)
       )
     )
  ),
  
  tabPanel("Relationship Model",
     sidebarLayout(
       sidebarPanel(
         h5("Details"),
         hr(),
         p("Deaths affect the population negatively while the births vice versa this model will help us show how the two relate using a regression model."),
         numericInput("b", "Births",0),
         submitButton("Apply", icon("refresh"))
       ),
       mainPanel(
         h2("Relationship Model for Births and Deaths"),
         plotOutput("rel", height = 350, width = 630),
         br(),
         verbatimTextOutput("value")
       )
     )   
  ), 
  
  tabPanel("Growth and Impacts(Changes)",
     sidebarLayout(
       sidebarPanel(
         h5("Details"),
         hr(),
         p("Population growth has been largely because of the births over the past 21 years, this can be visualized using a bar graph. A line graph displaying the impact of births, deaths and migrants(population changes) on population.")
         
       ),
       mainPanel(
         h2("Population Growth"),
         plotlyOutput("grow", height = 350, width = 630),
         
         br(),
         
         h2("Impact of Births, Deaths and Migrations on Population"),
         plotlyOutput("imp",  height = 350, width = 630) 

       )
     )   
  ),

  tabPanel("Other Plots",
     sidebarLayout(
       sidebarPanel(
         h5("Details"),
         hr(),
         p("Here you can basically look at other data visualizations as a result of an indivdual analysis of the statistical data.")
        
       ),
       mainPanel(
         tabsetPanel(
           tabPanel('Births',             plotOutput("trend.births",    height = 350, width = 630),
                                          plotOutput("future.births",   height = 350, width = 630)),
           
           tabPanel('Deaths',             plotOutput("trend.deaths",    height = 350, width = 630),
                                          plotOutput("future.deaths",   height = 350, width = 630)),
           
           tabPanel('Net Migrants',       plotOutput("trend.migrants",  height = 350, width = 630),
                                          plotOutput("future.migrants", height = 350, width = 630)),
           
           tabPanel('Population Change',  plotOutput("trend.change",    height = 350, width = 630),
                                          plotOutput("future.change",   height = 350, width = 630)),
           
           tabPanel('Natural Increase',   plotOutput("trend.increase",  height = 350, width = 630),
                                          plotOutput("future.increase", height = 350, width = 630))
         ),
         
         tabsetPanel(
           tabPanel('Births',             plotlyOutput("bar.births",   height = 350, width = 630)),
           tabPanel('Deaths',             plotlyOutput("bar.deaths",   height = 350, width = 630)),
           tabPanel('Net Migrants',       plotlyOutput("bar.migrants", height = 350, width = 630)),
           tabPanel('Natural Increase',   plotlyOutput("bar.increase", height = 350, width = 630))
         )
       )
     )
  )
)
)