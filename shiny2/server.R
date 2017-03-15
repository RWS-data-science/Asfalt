library(shiny)
library(leaflet)
library(RColorBrewer)
#library(rgdal)
library(dplyr)
library(ggplot2)

server <- function(input, output, session) {
  
  load("r_int_wgs.RData")
  r.int.wgs<- r.int.wgs[order(-r.int.wgs$jr.tot.interventie),]
#r.int.wgs<- r.int.wgs[sample.int(nrow(r.int.wgs),100000),]
  dat <- reactive({
    r.int.wgs[which(r.int.wgs$jaar == input$jaar & r.int.wgs$dienst == input$dienst),]
  })
  

  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron"
      #  urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #  attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 6, lat = 52, zoom = 8)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    #pal <- colorpal()
    pal <- colorFactor(
      palette = "Spectral",
      dat()$jr.tot.interventie
    )
    
    leafletProxy("map",data=dat()) %>%
      clearShapes() %>%
      addCircles(lng=dat()$x,lat=dat()$y, radius = 20, weight=1,color =  ~pal(jr.tot.interventie),#"#777777",
                 fillColor = ~pal(jr.tot.interventie), fillOpacity = 0.7, popup = ~paste("ID:", WEG.BAAN.STROOK.VAN, "<br>",
                                                                                    "WEG:", WEG, "<br>",
                                                                                    "AANLEGDATUM:", AANLEGDATUM, "<br>",
                                                                                    "DIENST:", dienst, "<br>",
                                                                                    "Jaren tot interventie:",jr.tot.interventie,"<br>",
                                                                                    "Leeftijd:",leeftijd)
      )
  })
  
  datInBounds <- reactive({
    # if (is.null(input$map_bounds))
    #   return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    dat()[which(dat()$y >= latRng[1] & dat()$y <= latRng[2] & 
                      dat()$x >= lngRng[1] & dat()$x <= lngRng[2]),]
   #r.int.wgs[which(r.int.wgs$y >= latRng[1] & r.int.wgs$y <= latRng[2] & 
  #                   r.int.wgs$x >= lngRng[1] & r.int.wgs$x <= lngRng[2]),]
  })
  
  # Precalculate the breaks we'll need for the two histograms
 # centileBreaks <- hist(plot = FALSE, r.int.wgs$leeftijd, breaks = 20)$breaks
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(datInBounds()) == 0)
      return(NULL)
    
    ggplot(datInBounds(),aes(x=leeftijd))+geom_histogram(fill='#00DD00',binwidth = 1,col="black")+xlab("Leeftijd (j)")+
      ggtitle("Leeftijd wegvakken (zichtbaar)")+ylab("Aantal")+xlim(0,25)
    # hist(datInBounds()$leeftijd,
    #      breaks = centileBreaks,
    #      main = "Leeftijd wegvakken (zichtbaar)",
    #      xlab = "Leeftijd (j)",
    #      xlim = c(0,30),#range(allzips$centile),
    #      col = '#00DD00',
    #      border = 'white')
    })
#  Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = dat())

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    # if (input$legend) {
    pal <- colorFactor(
      palette = "Spectral",
      dat()$jr.tot.interventie)
     proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~jr.tot.interventie
       )
     #}
  })
}
