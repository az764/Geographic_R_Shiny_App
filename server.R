#server logic
shinyServer(function(input, output) {
   
  #process data for radar chart visual
  country_processed <- eventReactive(input$go,{
    
    #error handle if no country has been selected
    if(is.null(input$countryselect))
    {
      #display error message 
      showNotification("Please select up to three countries to compare",type = "error")
      
      return(NULL)
    }
    
    #if countries have been selected then begin processing
    else{
      
      #define selectedcountry
      selectedcountry <- paste0(input$countryselect)
      
      #add max and min rows of each column - required for radar chart
      countries_radar<- rbind(
        c(Country="Max",Region="Max", apply(countries[, -c(1,2)], 2, function(y) max(y,na.rm = T) )),
        c(Country="Min",Region="Min", rep(0,7)),
        countries[countries$Country%in% c(selectedcountry),]
      )
      
      #change column names
      colnames(countries_radar) <- c("Country","Region","Population","Population Density (per sq mile)",
                                     "Infant mortality (per 1000 births)", "GDP ($ per capita)",
                                     "Literacy (%)", "Birth Rate (per 1000 people)", "Death Rate (per 1000 people)")
      
      #change class from character to numeric
      countries_radar[,-c(1,2)]<-as.data.frame(apply(countries_radar[,-c(1,2)],2,function(y) as.numeric(y)))
      
      #return table
      return(countries_radar)
    
    }
    
  })
  
  #render radar chart
  output$radarplot <- renderPlot({
    
    #requires country_processed before running
    req(country_processed())
    
    #define countries_radar
    countries_radar<-country_processed()
    
    #define colours for chart
    colours_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colours_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    
    #plot radar chart
    radarchart(countries_radar[,-c(1,2)],axistype=0, 
               #customise polygon
               pcol=colours_border, pfcol=colours_in , plwd=1 , plty=0.8,
               #customise the grid
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
               #customise labels
               vlcex=0.9
    )
    #add a legend
    legend(x=1.3, y=1.1, legend = countries_radar[-c(1,2),1], bty = "n", pch=20 , col=colours_in , text.col = "grey", cex=1, pt.cex=1)
    
  })
  
  #render table output on country comparison page
  output$table <- renderTable({
    
    #require country processed function
    req(country_processed())
    
    #define country_processed
    country_processed <- country_processed()
    
    #format population
    country_processed$Population <- format(round(country_processed$Population,-3),big.mark=",",scientific=FALSE)
    
    #output table (except first two rows as these only show max/min values)
    country_processed[-c(1,2),]
    
  })
  
  #render leaflet map
  output$map<- renderLeaflet({
    
    #plot map
    leaflet()%>%
      #add buildings
      addCircles(data=buildings, layerId =~NAME,lat = ~lat,lng= ~lon, popup= ~popup,opacity = ~opacity,weight = ~weight,radius = ~weight,label=~NAME,group="all")%>%
      #add tile
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  #change colour of circle on click
  observeEvent(input$map_shape_click,{
    
    #update leaflet map           
    leafletProxy("map")%>%
      #clear circles which were previously selected
      clearGroup("selected")%>%
      #add red circle
      addCircles(data=buildings[buildings$NAME %in% c(input$map_shape_click$id),] ,lat = ~lat,lng= ~lon, popup= ~popup,opacity = 1,weight =19,radius = 19,label=~NAME,group="selected",color = "Red")
      
  })
  
  #time series plot of population data
  output$population_plot <- renderPlot({
    
    #requires circle to be clicked to identify building selected
    req(input$map_shape_click)
    
    #identify what city the building is in
    cityselected <- buildings$CITY[buildings$NAME==input$map_shape_click$id]
    
    #filter population table to selected city
    cities_population <- cities[cities$City==cityselected,]
    
    #if number of rows is 0 and no population data is available
    if(nrow(cities_population) == 0){
      
      #display notification informing user of no data
      showNotification("No population data available for the city of the building selected",type = "error")
    }
    
    #else if data is available
    else{
      
      #generate time series plot
      ggplot(data=cities_population,aes(x=variable,y=(sum/1000000),group=1))+
        geom_line(colour="Red",lwd=1)+
        #add labels and formatting
        labs(title = paste0(cityselected," Population"),x="Year",y="Population (million)")+
        scale_x_discrete(breaks=seq(1950, 2035, 10))+
        theme(text = element_text(size=15))
    }
    
  })
  
})

