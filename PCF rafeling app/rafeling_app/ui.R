ui <- dashboardPage(
  dashboardHeader(title = "Rafeling analysetool"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Analyse", tabName = "analyse", icon = icon("line-chart")),
      menuItem("Geoviewer", tabName = "geo", icon = icon("map-o")),
      menuItem("Predictive model", tabName = "ml", icon = icon("rocket"))
      
    )
    
    #a(br(),href="mailto:martijn.koole@rws.nl",icon("info-circle"),"Martijn Koole")
  ),
  
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "analyse",
              h3("Data-analyse rafelingsschade MJPV"),
              p("Deze tool geeft de gebruiker de mogelijkheid om zelf een aantal visualisaties te maken die extra inzicht kunnen geven
                in de relaties tussen een aantal variabelen en de leeftijd waarop een deklaag voor het eerst rafelingsschade heeft vertoond. 
                Hiervoor is data gebruikt van alle beschikbare MJPV planjaren voor rafeling (periode 2004 - 2015), 
                gecombineerd met beschikbare data uit andere RWS systemen of open data van bijvoorbeeld KNMI. 
                Vervolgens is voor alle hectometervakken de leeftijd bepaald waarop de eerste rafelingsschade
                is geconstateerd. Dit is het jaar waarin het planjaar voor rafeling voor het eerst <= 5 jaar was.",
                br(),
                "Daarnaast is er met behulp van Machine Learning een model ontwikkeld dat op basis van patronen in de verzamelde data 
                een verwachting kan geven van de leeftijd waarop een deklaag voor het eerst rafelingsschade zal vertonen. 
                De resultaten hiervan zijn te vinden in het menu 'Predictive model'."),
              fluidRow(
                tabBox(
                  #title = "First tabBox",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", #height = "250px",
                  tabPanel("Relaties",plotOutput("plot1",height = "600px" ),
                           p(br(),"Voor numerieke variabelen geeft de 'r' de Pearson correlatie coefficient weer. 
                             De blauwe lijn is het resultaat van een auto-regressie functie.")
                           ),
                  tabPanel("Aantallen","Aantal hectometervakken binnen selectie:",
                           checkboxInput("facet","Toon subplots",value = F),
                           plotOutput("plot2"))
                  
                  ),
                box(
                  title = "Selecteer variabelen",
                  selectInput("variabele", "Variabele:",choices = params ),
                  checkboxGroupInput("deklaag","Deklaagsoort:",deklaag,selected=c("ZOAB","ZOAB+","ZOABTW")),
                  selectInput("weg","Wegnummer:",choices=wegnummers, selected="Alle")
                )
                
              ),
              a(href="mailto:martijn.koole@rws.nl",icon("info-circle"),"Martijn Koole")
      ),
      
      # Second tab content
      tabItem(tabName = "geo",
              h3("Leaflet Geoviewer"),
              p("Geografische weergave van de selecties uit het 'Analyse' menu. 
                Toont de leeftijd van eerste rafelingsschade voor alle wegvakken waar dit tussen 2004 en 2015 is voorgekomen."),
              leafletOutput("map"),
              selectInput("weg2","Wegnummer:",choices=wegnummers, selected=1),
              selectInput("jaar","Jaar van eerste schade:",choices=c("Alle",2004:2015), selected="Alle"),
              p("Verkort de laadtijd door eerst een jaar te kiezen of een weg te selecteren. De filters van menu 'Analyse' zijn ook hier van toepassing."),
              a(href="mailto:martijn.koole@rws.nl",icon("info-circle"),"Martijn Koole")
              ),
      
      ##Third tab
      tabItem(tabName = "ml",
              h3("Voorspelling leeftijd van eerste schade m.b.v. Machine Learning"),
              p("Het regressiemodel voorspelt de leeftijd waarop er voor het eerst rafelingsschade zal optreden met ca. 50 variabelen als input. 
                Deze variabelen zijn verzameld uit andere datasets van RWS of uit Open Data. De meeste van de variabelen zijn ook terug te vinden in het menu 'Analyse'.
                De voorspelde leeftijd is het voorspelde jaar waarin het planjaar voor rafeling voor het eerst <= 5 jaar is. 
                In theorie kan een deklaag vanaf dat moment nog 5 jaar mee. Voor meer inzicht in het verloop van rafelingsschade van planjaar 5 t/m 0 is nog aanvullend onderzoek nodig.",
                br(),
                br(),
                "Voor onderstaande resultaten zijn alle wegvakken en aanvullende informatie verzameld voor het meest recente jaar in de gebruikte data, 2015. 
                Bij vergelijking van een aantal veel gebruikte Machine Learning algoritmes (zie ",a(href="https://github.com/RWS-data-science/Asfalt","Github")," voor de scripts), 
                gaf een ", a(href="http://xgboost.readthedocs.io/en/latest/model.html", "Xgboost model"), "de beste resultaten.
                Dit model is getraind en gevalideerd op alle data van 2004 - 2015 en scoort daarbij 
                na een '5-fold crossvaldidation' een R-squared van 0.74 en een RMSE van 2.03 op voorspelde leeftijd van eerste schade. Dat betekent in de praktijk dat de door
                het model voorspelde leeftijd er gemiddeld ongeveer 2 jaar naast zit. Desalniettemin kan dit toch al helpen om om de programmering te verbeteren. 
                Door toevoegen van extra informatie over bijvoorbeeld aanlegcondities en gebruikte materialen
                kan mogelijk een hogere nauwkeurigheid worden gehaald."),
              selectInput("weg3","Wegnummer:",choices=wegnummers, selected=12),
              numericInput("van","Hectometer van:",55,min=0,max=max(raf_fd$VAN),step = 0.1),
              numericInput("tot","Hectometer tot:",65,min=0,max=max(raf_fd$TOT),step=0.1),
              leafletOutput("map2"),
              dataTableOutput('table'),
              a(href="mailto:martijn.koole@rws.nl",icon("info-circle"),"Martijn Koole")
              
              )
      )
  )
)