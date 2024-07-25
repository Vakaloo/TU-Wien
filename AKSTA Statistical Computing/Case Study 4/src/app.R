#this is the path for the app
#NOTE!!!!
#THIS MUST BE MODIFIED
setwd("C:/Users/vaka1/Desktop/Case Study 4")

#install.packages("countrycode")

#Load the proper libraries and data set
load("data_cia.rda")
library(ggplot2)
library(plotly)
library(dplyr)
library(shiny)
library("countrycode")

#prepare the data
world_map <- map_data("world")

#get the ISO codes
world_map$iso2c <- countrycode::countrycode(sourcevar = world_map$region,
                                            origin = "country.name",
                                            destination = "iso2c", nomatch = NA)
#get an idea about the data
head(world_map)

#left join the world map data and data_cia based on ISO codes
world_map_data <- world_map %>%
  left_join(data_cia, by = c("iso2c"),
            relationship = "many-to-many") %>%
  rename("Country" = "country",
         "Median age" = "median_age",
         "Youth unemployment rate" = "youth_unempl_rate")

#rename some of the columns from the data
data_cia <- data_cia %>%
  select("country","Region.Name","Sub.region.Name",
         "Developed...Developing.Countries", "Population", 
         "median_age", "youth_unempl_rate") %>%
  rename("Country" = "country",
         "Region" = "Region.Name",
         "Subregion" = "Sub.region.Name",
         "Development status" = "Developed...Developing.Countries",
         "Median age" = "median_age",
         "Youth unemployment rate" = "youth_unempl_rate"
  )

#create the front end for the app
ui <- fluidPage(
  #title
  titlePanel("CIA World Factbook 2020"),
  #create the sidebar layout
  sidebarLayout(
    sidebarPanel(
      #select Median age or Youth unemployment rate
      selectInput("var",
                  "Select a variable:",
                  c("Median age","Youth unemployment rate"),
                  selected = "Median age"),
      #create the action button to view the raw data
      actionButton("view_raw_data", "View raw data"),
      #output of the data table
      dataTableOutput("rawdata")),
    mainPanel(
      #plot the interactive map
      "Map visualization",
      #style = "height: 800px; overflow-y: auto;",
      plotlyOutput("map", width = "100%",height = "800px")#width = "1000px", height = "800px"
    )
  )
)
server <- function(input, output) {
  #use the event reactive in order to view the raw data 
  #when we click the button "View raw data"
  #the data contains Median age or Youth unemployment rate
  #based on the selection from the front end
  data_table <- eventReactive(input$view_raw_data,{
    if (input$var == "Median age"){
      data_cia %>%
        select(-c("Youth unemployment rate"))
    }else{
      data_cia %>%
        select(-c("Median age")) 
    }
  })
  #create the table that goes to the output part of the UI
  #maximum number of shown rows is 15
  output$rawdata <- renderDataTable(
    data_table(),
    options = list(lengthMenu = c(5, 10, 15),
                   pageLength = 15
                   #scrollX = TRUE
                   #autoWidth = TRUE
                   )
  )
  #create the interactive map using plotly that goes to the output part of the UI
  output$map <- renderPlotly({
    ggplot(world_map_data, aes(x = long, y = lat, group = group, label = Country,
                               fill = .data[[input$var]])) +
      geom_polygon(color = "white") +
      scale_fill_viridis_c(name = input$var)+ 
      theme_bw()
  })
}

#run the app
shinyApp(ui, server) #options = list(launch.browser = TRUE)