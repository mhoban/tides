#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
library(suncalc)

kbay_lat <- 21.433858
kbay_lon <- -157.788041

make_tib <- function(x) {
  tibble(
    time = ymd_hm(x[c(T,F)]),
    height = as.numeric(x[c(F,T)])
  )
}

get_predictions <- function(start,end) {
  url=str_c("https://tidesandcurrents.noaa.gov/api/datagetter?",
            "begin_date=",start,"&",
            "end_date=",end,
            "&station=1612480&product=predictions&datum=mllw&units=english&",
            "time_zone=lst&application=Tides_and_Currents&format=json")
  
  print(url)
  response <- GET(URLencode(url))
  doc <- content(response,as="parsed")
  return(
    doc$predictions %>%
      unlist() %>%
      make_tib()
  )
}

get_levels <- function(start,end) {
  url=str_c("https://tidesandcurrents.noaa.gov/api/datagetter?",
            "begin_date=",start,"&",
            "end_date=",end,
            "&station=1612480&product=water_level&datum=mllw&units=english&",
            "time_zone=lst&application=Tides_and_Currents&format=json")
  print(url)
  response <- GET(URLencode(url))
  doc <- content(response,as="parsed")
  return(
    doc$data %>%
      map(~.x[c("t","v")]) %>%
      unlist() %>%
      make_tib()
  )
}

get_suns <- function(dates,lat,lon) {
  suntimes <- getSunlightTimes(dates,lat=lat, lon=lon) %>%
    mutate(
      solarNoon = with_tz(solarNoon,"HST"),
      sunrise = with_tz(sunrise,"HST"),
      sunset = with_tz(sunset,"HST")
    ) %>%
    select(solarNoon, sunrise, sunset)
  days <- tibble(
    xstart = suntimes$sunrise,
    xend = suntimes$sunset,
    time="day"
  )
  nights <- c(rbind(suntimes$sunset,c(suntimes$sunrise[-1],max(suntimes$sunrise)+days(1))))
  nights <- with_tz(as_datetime(nights),"HST")
  nights <- tibble(xstart=nights[c(T,F)],xend=nights[c(F,T)],time="night")
  return(bind_rows(days,nights))
}


shinyServer(function(input, output) {
  
  w <- Waiter$new("tidegraph")
  
  tides <- reactive({
    sd <- format(input$daterange[1],"%Y%m%d")
    ed <- format(input$daterange[2],"%Y%m%d")
    w$show()
    on.exit({
      w$hide()
    })
    get_predictions(sd,ed)
  })
  
  waterlevel <- reactive({
    d1 <- input$daterange[1]
    d2 <- input$daterange[2]
    if ((d1 <= d2) & (d1 < now())) {
      sd <- format(d1,"%Y%m%d")
      ed <- format(d2,"%Y%m%d")
      get_levels(sd,ed)
    } else{
      tibble(time=as.POSIXct(NA),height=as.numeric(NA))
    }
  })
  
  extremes <- reactive({
    tides() %>% 
      distinct(height,.keep_all = T) %>%
      mutate(sign=c(0,diff(sign(diff(height))),0)) %>%
      filter(sign %in% c(2,-2)) %>%
      arrange(time) %>%
      mutate(
        type = as.factor(case_when(
          sign == -2 ~ "high",
          sign == 2 ~ "low"
        )),
        time = format(time,"%b %d, %Y %I:%M %Op")
        
      ) %>%
      select(type,time,height) %>%
      rename('Tide' = type, 'Date & time'=time, 'Predicted tide height'=height)
  })
  
  output$tidegraph <- renderPlotly({
    predictions <- tides()
    observations <- waterlevel()
    
    water <- predictions %>%
      full_join(observations,by="time",suffix=c("_predicted","_observed"))
    
    dates <- seq.Date(
      as_date(min(water$time,na.rm = T)-days(1)),
      as_date(max(water$time,na.rm = T)+days(1)),
      by=1
    )
    
    suntimes <- get_suns(dates,kbay_lat,kbay_lon)

    g <- ggplot() +
      ggtitle("Kāne‘ohe Bay tides (relative to mllw)") + 
      geom_rect(data = suntimes, aes(xmin = xstart, xmax = xend, ymin = -100, ymax = 100, fill = time), alpha = 0.4, show.legend = F) +
      scale_fill_manual(values=c("lightyellow","midnightblue")) +
      geom_line(
        data=water,
        aes(
          x=time,
          y=height_predicted, 
          group=1,
          color="predicted",
          text=str_c("time: ",with_tz(time,"HST"),"<br>","predicted height: ",height_predicted)
        )
      ) +
      #text=str_c("time: ",with_tz(time,"HST"),"<br>","height: ",height_predicted))
      geom_line(
        data=water,
        aes(
          x=time,
          y=height_observed, 
          group=1,
          color="observed", 
          text=str_c("time: ",with_tz(time,"HST"),"<br>","observed height: ",height_observed)
        )
      ) +
      scale_color_manual(name="Tide Height",values=c("predicted" = "black", "observed" = "red")) +
      coord_cartesian(ylim=range(c(0,water$height_observed,water$height_predicted),na.rm = T),xlim=range(water$time,na.rm = T)) 
    gg <- ggplotly(g,tooltip="text")
    gg %>%
      style(showlegend=FALSE, traces=1:2) %>%
      style(name="day",traces=1) %>%
      style(name="night",traces=2) %>%
      style(name="predicted",traces=3) %>%
      style(name="observed",traces=4)
  })
  
  output$tidetable <- renderTable(
    extremes(),
    striped = T,
    width = "100%"
  )
  
  output$moonphase <- renderTable({
    dates <- seq.Date(
      as_date(min(tides()$time,na.rm = T)-days(1)),
      as_date(max(tides()$time,na.rm = T)+days(1)),
      by=1
    )
    getMoonIllumination(dates,keep = "phase") %>%
      arrange(date) %>%
      mutate(date=format(date,"%m/%d/%Y"))
  })
})
