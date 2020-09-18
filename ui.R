#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(jsonlite)
library(httr)
library(stringr)
library(purrr)
library(lubridate)
library(tidyverse)
library(waiter)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    use_waiter(),
    # use_waitress(),
    # Application title
    titlePanel("Kāne‘ohe Bay Tide Predictions"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(
                "daterange",
                "Date Range",
                start = today(),
                end = today(),
                min = today() - years(5),
                max = today() + years(5)
            ),
            h5("hover over the graph"),
            h5("to see data points"),
            h5("(you might have to click on it)"),
            tags$small("clicking the 'autoscale' button will make the plot look bad")
        ),
        
        mainPanel(
            plotlyOutput("tidegraph"),
            tableOutput("tidetable"),
            tableOutput("moonphase")
        )
    )
))
