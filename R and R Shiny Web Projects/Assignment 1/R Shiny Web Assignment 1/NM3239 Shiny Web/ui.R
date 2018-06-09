
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shiny)
library(dplyr)
library(ggplot2)
library(dygraphs)
library(rCharts)
library(rsconnect)
library(shinythemes)
library(plotly)

shinyUI(fluidPage(theme = shinytheme("superhero"),
                  
                  tags$head(
                    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=VT323');
      
      h1 {
        font-family: 'VT323', serif;
        font-weight: 900;
        line-height: 1.5;
        color: #fff;
font: 20px;
      }

    "))
                  ),

  # Application title
  titlePanel(h1("Sunspots Are More Terrifying Than Ever")),
  h4("NM3239 Assignment 1 Shiny Web Application"),
  h4("Alicia Soh Su Xian (A0114334U)"),

  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      
      selectInput("var", 
        "1. Select the view for the dataset", 
        choices = c("Yearly", "Decennially", "Centennially"),
        selected = "Yearly"),
      
      checkboxInput(inputId = "stats",
                    label = strong("Show Statistics"),
                    value = FALSE),
      
      selectInput("type", 
                  "3. View different charts", 
                  choices = c("BarChart", "ScatterPlot", "LineChart", "LineChart&ScatterPlot"),
                  selected = "BarChart"
                  ),
      
      checkboxInput(inputId = "story",
                    label = strong("Show Story & Analysis"),
                    value = FALSE),
      
      checkboxInput(inputId = "ref",
                    label = strong("Show References"),
                    value = FALSE)
      
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput(outputId = "intro"),
      htmlOutput(outputId="q1"),
      plotlyOutput(outputId = "plot"),
      tableOutput(outputId = "statsno"),
      textOutput(outputId = "statsText"),
      htmlOutput(outputId="q2"),
      plotlyOutput(outputId = "differentcharts"),
      htmlOutput(outputId = "story"),
      htmlOutput(outputId = "ref")
      
    )
  )
))
