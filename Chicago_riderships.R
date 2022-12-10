# Change years period in facet wrap

library(tidyverse)
library(sf)
library(shiny)
library(spData)
library(scales)
library(cowplot)
library(plotly)
library(shinythemes)
library(rsconnect)
library(readr)
library(readxl)
library(viridis)
library(lubridate)
library(MASS)
library(RColorBrewer)
library(RSocrata)
library(tidyr)


# Bus data with API
bus_monthly_rides_by_route <- read.socrata("https://data.cityofchicago.org/resource/bynn-gwxy.csv")

# Shapefile
path_local <- "C:/Users/cuadr/OneDrive/Escritorio/CV and presentation/GitHub/R/Maps/Shapefiles/CTA_BusRoutes__2_"
chicago <- st_read(file.path(path_local, "CTA_BusRoutes.shp"))

# Adapting chicago shapefile and csv file to merge each other
chicago_new <- chicago %>% 
  mutate(ROUTE_NUM = ROUTE,
         ROUTE_NUM = recode(ROUTE_NUM, 
                            '111A' = '111.5',
                            '53A' = '53.5', 
                            '52A' = '52.5',
                            '62H' = '62.5',
                            '63W' = '63.5',
                            '54A' = '54.33',
                            '54B' = '54.66',
                            '55A' = '55.33',
                            '55N' = '55.66',
                            '49B' = '49.5',
                            'J14' = '14.5',
                            'X49' = '49.5',
                            'X9' = '9.5',
                            '81W' = '81.5',
                            '8A' = '8.5',
                            'X4' = '4.5',
                            '85A' = '85.5',
                            'X98' = '98.5'),
         ROUTE_NUM = as.numeric(ROUTE_NUM))

chicago_new_div <- chicago_new %>%
  mutate(bus_group = if_else(ROUTE_NUM <= 32, "Routes 1-32", 
                             if_else(ROUTE_NUM >= 33 & ROUTE_NUM <= 64, "Routes 33 - 64", 
                                     if_else(ROUTE_NUM >= 65 & ROUTE_NUM <= 96, "Routes 65 - 96", "Routes 97+"))),
         bus_group = as.character(bus_group))


cta_monthly <- bus_monthly_rides_by_route  %>%
  mutate(month = month(month_beginning),
         year = year(month_beginning)) 



# Creating data for interactive plots in Shiny
interactive_1 <- cta_monthly  %>%
  filter(month_beginning >= "2018-01-01") %>%
  mutate(year = as.character(year)) %>%
  group_by(routename, year) %>%
  summarize(rides = as.numeric(sum(monthtotal)))


difference <- cta_monthly %>%
  group_by(route, year) %>%
  summarize(Rides = as.numeric(sum(monthtotal)))

difference <- difference %>%
  group_by(route) %>%
  mutate(Percent = ((Rides - lag(Rides)) / Rides) * 100,
         diff_years = if_else(year == 2018, "2018-2019",
                              if_else(year == 2019, "2018-2019",
                                      if_else(year == 2020, "2019-2020",
                                              if_else(year == 2021, "2020-2021", 
                                                      if_else(year == 2022, "2021-2022", "no")))))) %>%
  filter(diff_years != "no") %>%
  group_by(year) %>%
  mutate(Percentile = ntile(Percent, 100)) %>%
  drop_na()


difference_2 <- difference %>%
  filter(year == 2020) %>%
  mutate(Decile = ntile(Percent, 10))

interactive_2 <- difference_2 %>%
  dplyr::select(route, Rides, Percent, Decile) %>%
  arrange(Decile)




# Static plot 1. 
fun_chicago <- function(data_rides, year_start, year_end, data_geom){
  
  rides = data_rides %>% 
    filter(year >= year_start & year <= year_end) %>%
    group_by(route, year) %>%
    summarize(Riderships = sum(monthtotal)) 
  
  geom_merged = data_geom %>%
    left_join(rides, by = c('ROUTE' = 'route')) %>%
    filter(!is.na(year))
  
  buses_map = ggplot(data = geom_merged, 
                     aes(geometry = geometry)) +
    geom_sf(aes(color = Riderships), size = 0.5) +
    labs(title = "Graph 1. Total riderships per year, Jan 2018 - Jun 2022",
         caption = "Source: Chicago Data Portal") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          legend.position = "bottom", 
          legend.text = element_text(colour="black", size= 7),
          panel.background = element_rect(fill = "gray70")) +
    facet_wrap(vars(year), strip.position = "top", nrow = 1) +
    scale_colour_distiller(palette = "YlOrRd", trans = "reverse") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  print(buses_map)
}

Static_1 <- fun_chicago(cta_monthly, 2018, 2022, chicago_new_div)
Static_1
save_plot("Static_1.png", Static_1)



# Static plot 2 
fun_chicago_change <- function(data_rides, year_start, year_end, data_geom){
  geom_merged = data_rides %>% 
    filter(year >= year_start & year <= year_end) %>%
    group_by(route, year) %>%
    left_join(data_geom, by = c('route' = 'ROUTE')) %>%
    filter(Percent > -200)
  buses_map = ggplot(data = geom_merged, 
                     aes(geometry = geometry)) +
    geom_sf(aes(color = Percent), size = 0.5) +
    labs(title = "Graph 2. Percent ridership change per year, 2018 - 2022",
         caption = "Source: Chicago Data Portal") +
    theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "bottom", 
        legend.text = element_text(colour="black", size= 7),
        panel.background = element_rect(fill = "gray70")) +
    facet_wrap(vars(diff_years), strip.position = "top", nrow = 1) +
    scale_colour_distiller(palette = "RdYlBu", trans = "reverse") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  print(buses_map)
}
Static_2 <- fun_chicago_change(difference, 2018, 2022, chicago_new_div)
Static_2
save_plot("Static_2.png", Static_2)



# Static plot 3
fun_chicago_change <- function(data_rides, year_start, year_end, data_geom){
  geom_merged = data_rides %>% 
    filter(year >= year_start & year <= year_end) %>%
    group_by(route, year) %>%
    left_join(data_geom, by = c('route' = 'ROUTE')) 
  buses_map = ggplot(data = geom_merged, 
                     aes(geometry = geometry)) +
    geom_sf(aes(color = Percentile), size = 0.5) +
    labs(title = "Graph 3. Ridership changes as percentile per year, 2018 - 2022",
         caption = "Source: Chicago Data Portal") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "bottom", 
        legend.text = element_text(colour="black", size= 7),
        panel.background = element_rect(fill = "gray70")) +
    facet_wrap(vars(diff_years), strip.position = "top", nrow = 1) +
    scale_colour_distiller(palette = "YlGnBu") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  print(buses_map)
}
Static_3 <- fun_chicago_change(difference, 2018, 2022, chicago_new_div)
Static_3
save_plot("Static_3.png", static_3)


# Creating a Shiny dashboard
ui <- fluidPage(theme = shinytheme("flatly"),
                fluidRow(
                  column(width = 8,
                         offset = 2,
                         align = "center",
                         tags$img(src = "https://www.appam.org/assets/1/6/harris3.png?62132",
                                  height = 100,
                                  width = 190)
                  )
                ), 
                fluidRow(
                  column(width = 8,
                         offset = 2,
                         align = "center", 
                         tags$h1("Covid-19 pandemic impact on Chicago's ridership"),
                         tags$hr()
                  )
                ),
                
                fluidRow(
                  column(width = 8,
                         offset = 2, 
                         align = "center",
                         tags$h4("Welcome! This Shiny dashboard shows basic information regarding the Covid-19 pandemic impact on Chicago's ridership between 2018 and 2022. To start, pick the route you are most interest in and evaluate how riderships changed in the last 5 years.")
                  ),
                  
                  fluidRow(
                    column(width = 8,
                           offset = 2,
                           align = "center",
                           selectInput(inputId = "routename",
                                       label = "Choose a route",
                                       choices = interactive_1$routename)
                    )
                  ),
                  
                  fluidRow(
                    column(width = 3,
                           offset = 2,
                           align = "center",
                           tableOutput("table")
                    ),
                    column(width = 5,
                           align = "center",
                           plotlyOutput("graph")  
                    )
                  ),
                  
                  fluidRow(
                    column(width = 8,
                           offset = 2, 
                           align = "center",
                           tags$h4("As you could appreciate, Covid-19 critically affected riderships during 2019 and 2020. Evaluate which routes received the highest impact by Covid-19 pandemic by deciles. Lowest deciles will group routes with critical negative impact.")
                    ),
                  
                  fluidRow(
                    column(width = 8,
                           offset = 2,
                           align = "center",
                           selectInput(inputId = "Decile",
                                       label = "Ridership change by deciles",
                                       choices = interactive_2$Decile)
                    )
                  ),
                  
                  fluidRow(
                    column(width = 3,
                           offset = 2,
                           align = "center",
                           tableOutput("table_2")
                    ),
                    column(width = 5,
                           offset = 1,
                           align = "center",
                           plotlyOutput("graph_2")  
                    )
                  ),
                  
                  fluidRow(
                    column(width = 8,
                           offset = 2, 
                           align = "center",
                           tags$h4("Please check the information below to see how the Covid-19 pandemic impacted the city with maps. Observe how ridership behavior changed in absolute terms, by percent or percentiles"),
                           tags$hr()
                    )
                  ),
                  
                  fluidRow(
                    column(width = 2,
                           offset = 2, 
                           align = "center", 
                           selectInput(inputId = "type",
                                       label = "Select a map",
                                       choices = c("Total riderships", "Percent change", "Percentile changes"))
                    ),
                    column(width = 7,
                           align = "center",
                           imageOutput(outputId = "type")
                    )
                  )
                )
))



server <- function(input, output) {
  
  df <- interactive_1
  
  data <- reactive({
    d <- filter(df, routename == input$routename)
    return(d)
  })
  
  output$graph <- renderPlotly({
    plt <- ggplot(data = data(), aes(x = year, y = rides)) +
      geom_col(aes(fill = year)) +
      scale_y_continuous(labels = label_comma()) +
      labs(title = "Riderships per year, 2018-2022",
           caption = "Source: Chicago Data Portal",
           x = "Year",
           y = "Riderships") +
      theme(plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            legend.position = 'none')
    ggplotly(plt)
  })
  
  output$table <- renderTable({data()})


  df_2 <- interactive_2
  data_2 <- reactive({
    d_2 <- filter(df_2, Decile == input$Decile)
    return(d_2)
  })

  
  output$graph_2 <- renderPlotly({
    plt <- ggplot(data = data_2(), aes(x = route, y = Rides)) +
      geom_col(aes(fill = route)) +
      scale_y_continuous(labels = label_comma()) +
      labs(title = "Ridership change by percentile, 2019-2020",
           caption = "Source: Chicago Data Portal",
           x = "Route number",
           y = "Riderships") +
      theme(plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            legend.position = 'none')
    ggplotly(plt)
  })
  
  output$table_2 <- renderTable({data_2()})
  

  output$type <- renderImage({ 
    if (input$type == "Total riderships") {
      src <- "Static_1.png"
    } else if (input$type == "Percent change") {
      src <- "Static_2.png"
    } else if (input$type == "Percentile changes") {
      src <- "Static_3.png"
    } 
    list(src = src,
         width = "100%")
  }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)
