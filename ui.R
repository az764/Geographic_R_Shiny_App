#load required libraries
library(shiny)
library(shinydashboard)
library(fmsb)
library(leaflet)
library(ggplot2)
library(dplyr)

#user interface

#navigation bar with title
navbarPage("Countries Analysis",
           
           #first tab (country comparison)
           tabPanel("Country Data Comparison",
                    #sidebar menu
                    sidebarPanel(width=3,
                                 
                                 #radar chart description
                                 paste0("Select up to three countries to compare. Click 'Go' to view visualisation and data table"),
                                 
                                 #line break
                                 br(),br(),
                                 
                                 #select countries using select input
                                 selectizeInput("countryselect","Select Countries: ",
                                                width = 250,choices = c(unique(countries$Country)),multiple = TRUE,options = list(maxItems = 3)),
                                 
                                 #action button to trigger visual
                                 actionButton("go","Go")
                                 ),
                    #main page area
                    mainPanel(
                      
                      #plot radar visual
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot",plotOutput("radarplot",width = "100%", height= "600px")),
                                  tabPanel("Table",tableOutput("table"))
                      )
                    )
           ),
           
           #second tab (largest buildings)
           tabPanel("Largest Buildings",
                    
                    #map description
                    paste0("The map visualises the largest buildings in the world. Please select a circle for more information about the building. 
                           A population time series chart will appear below the map once a circle is selected."),
                    
                    #leave a line break
                    br(),br(),
                    
                    #leaflet map output
                    leafletOutput("map",height="600px"),
                    
                    #leave a line break
                    br(),br(),
                    
                    #plot population time series chart
                    plotOutput("population_plot",width = "80%"),
                    
                    #leave a line break
                    br(),br()
           )
           
)
