#load libraries
library(RgoogleMaps)
library(leaflet)
library(tidyverse)
library(reshape)


#read csv data files
countries <- read.csv("data/countries of the world.csv")
buildings <- read.csv("data/tallest_completed_buildings.csv")
cities <- read.csv("data/The Most Largest Cities in the World 1950 - 2035.csv")



#filling in na population

#melt reshape the table from 'wide' to 'long'

cities<-melt(cities[,c(1,2,5:90)],id=c("Country","City"), direction ="long")

#remove 'X' from each year
cities$variable <- substr(cities$variable,2,5)

#fill in na values
cities <- cities%>%
  dplyr::group_by(Country,City, variable)%>%
  dplyr::summarise(sum=sum(value))%>%
  tidyr::fill(sum,.direction = "downup")

#remove special characters (Bogota and Sao Paulo have special character)
cities$City <- gsub('[^[:alnum:] ]', '', cities$City)

#replace New York to match format of buildings table
cities[cities$City == "New York",2] <- "New York City"
  
  
  
#prepare buildings table for map  
  
#get longitude and latitude of each city
buildings$coords<-lapply(buildings$CITY, RgoogleMaps::getGeoCode)
  
#unnest the nested list so that we have a column for longitude and latitude
buildings<- unnest_wider(buildings,coords)
  
#join the table with cities table to identify 2022 population
buildings<- left_join(buildings,cities[cities$variable=='2022',],by=c("CITY"="City"))
  
#format population
buildings$sum <- format(round(buildings$sum,-3),big.mark=",",scientific=FALSE)
  
#create popup visual
buildings$popup<- paste0(
  #building name
  "<b>",  buildings$NAME ,"</b>",
  "<br/>",
  #city
  "City: ",buildings$CITY,
  "<br/>",
  #rank
  "Rank: ",buildings$RANK,
  "<br/>",
  #completion year
  "Completion Year: ",buildings$COMPLETION,
  "<br/>",
  #height
  "Height: ",buildings$HEIGHT,
  "<br/>",
  #population
  "Population (2022) : ",buildings$sum
)
  
#define weight for visual (based on rank) to determine size of each map circle
buildings<- buildings%>% mutate(weight=case_when(
  RANK < 25 ~ 18,
  RANK >=26 & RANK <= 50 ~ 12,
  RANK >=51 & RANK <= 75 ~ 9,
  RANK >=76 & RANK <= 100 ~ 7
))%>%
#define opacity of each circle based on rank  
  mutate(opacity=case_when(
    RANK < 25 ~ 1,
    RANK >=26 & RANK <= 50 ~ 0.8,
    RANK >=51 & RANK <= 75 ~ 0.6,
    RANK >=76 & RANK <= 100 ~ 0.4
  ))


  

#prepare countries table for radar chart
  
#subset countries table to only columns of interest
countries<-countries[,c(1,2,3,5,8,9,10,16,17)]
  
#swap comma with decimal place and change class from character to numeric
countries[,-c(1,2)] <- apply(countries[,-c(1,2)], 2, function(y) as.numeric(gsub(",", ".", y)))
  
