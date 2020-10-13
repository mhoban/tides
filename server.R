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
library(here)

kbay_lat <- 21.433858
kbay_lon <- -157.788041

make_tib <- function(x) {
  tibble(
    time = ymd_hm(x[c(T,F)],tz="HST"),
    # time = force_tz(ymd_hm(x[c(T,F)])),
    height = as.numeric(x[c(F,T)])
  )
}

get_predictions <- function(daterange) {
  dr <- range(daterange)
  url=str_c("https://tidesandcurrents.noaa.gov/api/datagetter?",
            "begin_date=",format(dr[1],"%Y%m%d"),"&",
            "end_date=",format(dr[2],"%Y%m%d"),
            "&station=1612480&product=predictions&datum=mllw&units=english&",
            "time_zone=lst&application=Tides_and_Currents&format=json")
  
  # print(url)
  response <- GET(URLencode(url))
  doc <- content(response,as="parsed")
  return(
    doc$predictions %>%
      unlist() %>%
      make_tib()
  )
}

get_levels <- function(daterange) {
  dr <- range(daterange)
  url=str_c("https://tidesandcurrents.noaa.gov/api/datagetter?",
            "begin_date=",format(dr[1],"%Y%m%d"),"&",
            "end_date=",format(dr[2],"%Y%m%d"),
            "&station=1612480&product=water_level&datum=mllw&units=english&",
            "time_zone=lst&application=Tides_and_Currents&format=json")
  # print(url)
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
  suntimes <- getSunlightTimes(dates,lat=lat, lon=lon, tz = "HST") %>%
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

get_moons <- function(dates,lat,lon) {

  getMoonTimes(dates,lat=lat,lon=lon,tz="HST") %>%
    select(rise,set) %>%
    pivot_longer(c("rise","set"), names_to="type", values_to="time") %>%
    arrange(time) %>%
    mutate(day = rep(seq(n()/2),each=2)) %>%
    pivot_wider(names_from = type, values_from = time,values_fn=mean) %>%
    drop_na() %>%
    select(-day) %>%
    mutate(date=as_date(rise)) %>%
    inner_join(getMoonIllumination(dates,keep=c("fraction","phase")),by="date") %>%
    mutate(
      moonphase = cut(
        phase,
        c(-Inf,0.125,0.25,0.375,0.5,0.625,0.75,0.875,Inf),
        labels=c('new','waxing_crescent','first_quarter','waxing_gibbous','full','waning_gibbous','last_quarter','waning_crescent')
      )
    )
    
  # getMoonTimes(dates,lat=lat,lon=lon,tz="HST") %>%
  #   inner_join(getMoonIllumination(dates,keep=c("fraction","phase")),by="date") %>%
  #   mutate(
  #     rise=rise,
  #     rise_forced = force_tz(rise,"HST"),
  #     rise_with = with_tz(rise,"HST"),
  #     set=set,
  #     set_forced = force_tz(set,"HST"),
  #     set_with = with_tz(set,"HST"),
  #     moonphase = cut(
  #       phase,
  #       c(-Inf,0.125,0.25,0.375,0.5,0.625,0.75,0.875,Inf),
  #       labels=c('new','waxing_crescent','first_quarter','waxing_gibbous','full','waning_gibbous','last_quarter','waning_crescent')
  #     )
  #   )
}

shinyServer(function(input, output) {
  
  w <- Waiter$new("tidegraph")
  w2 <- Waiter$new("tidetable")
  
  daterange <- reactive({
    seq.Date(as_date(input$daterange[1]),as_date(input$daterange[2]),by=1)
  }) 
  
  daterange2 <- reactive({
    seq.Date(as_date(input$daterange[1])-days(1),as_date(input$daterange[2])+days(2),by=1)
  }) 
  
  predictions <- reactive({
    get_predictions(daterange())
  })
  
  waterlevels <- reactive({
    get_levels(daterange())
  }) 
  
  suntimes <- reactive({
    get_suns(daterange2(),kbay_lat,kbay_lon)
  })
  
  moontimes <- reactive({
    moontimes <- get_moons(daterange2(),kbay_lat,kbay_lon)
    # print(moontimes)
    moontimes
  }) 
  
  # tides <- reactive({
  #   sd <- format(input$daterange[1],"%Y%m%d")
  #   ed <- format(input$daterange[2],"%Y%m%d")
  #   get_predictions(sd,ed)
  # })
  # 
  # waterlevel <- reactive({
  #   d1 <- 
  #   d2 <- input$daterange[2]
  #   if ((d1 <= d2) & (d1 < now())) {
  #     sd <- format(d1,"%Y%m%d")
  #     ed <- format(d2,"%Y%m%d")
  #     get_levels(sd,ed)
  #   } else{
  #     tibble(time=as.POSIXct(NA),height=as.numeric(NA))
  #   }
  # })
  # moontimes <- reactive({
  #   predictions <- tides()
  #   observations <- waterlevel()
  #   
  #   water <- predictions %>%
  #     full_join(observations,by="time",suffix=c("_predicted","_observed"))
  #   
  #   dates <- seq.Date(
  #     as_date(min(water$time,na.rm = T)-days(1)),
  #     as_date(max(water$time,na.rm = T)+days(1)),
  #     by=1
  #   )
  #   getMoonTimes(dates,lat=kbay_lat,lon=kbay_lon) %>%
  #     inner_join(getMoonIllumination(dates,keep=c("fraction","phase")),by="date") %>%
  #     mutate(
  #       moonphase = cut(
  #         phase,
  #         c(-Inf,0.125,0.25,0.375,0.5,0.625,0.75,0.875,Inf),
  #         labels=c('new.png','waxing_crescent.png','first_quarter.png','waxing_gibbous.png','full.png','waning_gibbous.png','last_quarter.png','waning_crescent.png')
  #       )  
  #     )
  # })
  
  extremes <- reactive({
    predictions() %>% 
      distinct(height,.keep_all = T) %>%
      mutate(sign=c(0,diff(sign(diff(height))),0)) %>%
      filter(sign %in% c(2,-2)) %>%
      arrange(time) %>%
      mutate(
        type = as.factor(case_when(
          sign == -2 ~ "high",
          sign == 2 ~ "low"
        )),
        time = format(time,"%a, %b %d, %Y %H:%M:%S")
        
      ) %>%
      select(type,time,height) %>%
      rename('Tide' = type, 'Date & time'=time, 'Predicted tide height (within the date range)'=height)
  })
  
  output$tidegraph <- renderPlotly({
    w$show()
    w2$show()
    on.exit({
      w$hide()
    })
    water <- predictions() %>%
      full_join(waterlevels(),by="time",suffix=c("_predicted","_observed"))
    suppressWarnings(
      g <- ggplot() +
        ggtitle("Kāne‘ohe Bay tides") + 
        geom_rect(data = suntimes(), aes(xmin = xstart, xmax = xend, ymin = -100, ymax = 100, fill = time), alpha = 0.4, show.legend = F) +
        scale_fill_manual(values=c("lightyellow","midnightblue")) +
        geom_line(
          data=water,
          aes(
            x=time,
            y=height_predicted, 
            group=1,
            color="predicted",
            text=str_c("time: ",format(time,"%a, %b %d, %Y %H:%M:%S"),"<br>","predicted height: ",height_predicted," ft")
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
            text=str_c("time: ",format(time,"%a, %b %d, %Y %H:%M:%S"),"<br>","observed height: ",height_observed," ft")
          )
        ) +
        scale_color_manual(name="Tide Height",values=c("predicted" = "black", "observed" = "red")) +
        coord_cartesian(ylim=range(c(0,water$height_observed,water$height_predicted),na.rm = T),xlim=range(water$time,na.rm = T)) +
        xlab("Time (HST)") + 
        ylab("Tide height (ft relative to mllw)")
    )
    ggp <- ggplotly(g,tooltip = "text")
    
    mr <- moontimes() %>% pmap(~{
      x <- list(...)
      list(
        source = base64enc::dataURI(file=here("www","img",str_c(x$moonphase,".png"))),
        xref = "x",
        yref = "paper",
        x= as.numeric(force_tz(x$rise,"HST"))-12500/2,
        y= 0.95,
        sizex = 12500,
        sizey = 0.1
      )
    })  
    ms <- moontimes() %>% pmap(~{
      x <- list(...)
      list(
        source = base64enc::dataURI(file=here("www","img",str_c(x$moonphase,"_set.png"))),
        xref = "x",
        yref = "paper",
        x= as.numeric(force_tz(x$set,"HST"))-12500/2,
        y= 0.95,
        sizex = 12500,
        sizey = 0.1
      )
    })  
    
    ggp %>% 
      layout(images=c(mr,ms)) %>%
      style(showlegend=FALSE, traces=1:2) %>%
      style(name="day",traces=1) %>%
      style(name="night",traces=2) %>%
      style(name="predicted",traces=3) %>%
      style(name="observed",traces=4)
  })
  
  output$tidetable <- renderTable(
    {
      on.exit({ w2$hide() })
      extremes() 
    },
    striped = T,
    width = "100%"
  )
})
