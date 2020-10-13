#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(tidyverse)
library(waiter)
library(lubridate)
library(plotly)

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
                start = today(tzone = "HST"),
                end = today(tzone = "HST"),
                min = today(tzone = "HST") - years(5),
                max = today(tzone = "HST") + years(5)
            ),
            # h5("hover over the graph"),
            # h5("to see data points"),
            # h5("(you might have to click on it)"),
            #tags$small("clicking the 'autoscale' button will make the plot look bad"),
            h5("On the graph:"),
            div("Moonrise looks like this: ",tags$img(src="img/first_quarter.png",style="width: 50px; height: 50px")),
            div("Moonset looks like this: ",tags$img(src="img/first_quarter_set.png",style="width: 50px; height: 25px")),
            div("Moon phases won't be perfect, or even particularly accurate, ok?")
        ),
        
        mainPanel(
            plotlyOutput("tidegraph"),
            tableOutput("tidetable"),
            tableOutput("moonphase")
        )
    )
))
