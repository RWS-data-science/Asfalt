library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                #             value = range(quakes$mag), step = 0.1
                
                #selectInput("colors", "Color Scheme",
                #            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                selectInput("jaar", "Jaar",
                            choices = c(2015:2010)
                ),
                selectInput("dienst", "Dienst",
                                        choices = c("MN","NN","ON","WN","WZ","ZD","ZN")
                ),
                plotOutput("histCentile", height = 200)
                # checkboxInput("legend", "Show legend", TRUE)
                
  )
)