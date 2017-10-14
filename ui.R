library(shiny)
library(shape)
library(plyr)
library(ggplot2)

fluidPage(
  fluidRow(
    column(width = 2,
           h3("Data input:"),
           selectInput("Raining", "Raining?", choices = c('Yes'=1,'No'=0)),
           selectInput("Sprinkler", "Sprinkler On?", choices = c('Yes'=1,'No'=0)),
           selectInput("Grass", "Grass Wet?", choices = c('Yes'=1,'No'=0)),
           sliderInput("N",
                       "Number observations:",
                       min = 1,
                       max = 10,
                       value = 1,
                       step = 1),
           actionButton("drawS", "Add data")
    ),
    column(5, 
           fluidRow(
             column(5, plotOutput("plot1", height = 200)),#, div(style = "height:120px;")), 
             column(5, plotOutput("plot2", height = 200))#, div(style = "height:120px;"))
           ), 
           fluidRow(
             column(10, plotOutput("plot4", height = 250))#, div(style = "height:150px;"))
           ),
           fluidRow(
             column(10, plotOutput("plot3", height = 250))#, div(style = "height:250px;"))
           )
    ),
    column(2, 
           h3("Current data:"),
           tableOutput("table"),
           actionButton("reset", "Reset"),
           h3("Predictions:"),
           selectInput("whichP", "About sprinkler or rain?", choices = c('Sprinkler'='Sprinkler','Rain'='Rain')),        
           selectInput("GrassP", "When the grass is:", choices = c('Wet'='Wet','Dry'='Dry')),
           textOutput("textOut")
    )
  )
)