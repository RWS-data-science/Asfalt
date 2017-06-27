library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)
library(leaflet)
library(RColorBrewer)
library(dplyr)
#library(caret)
library(DT)

load("data/raf_fd.RData")
#load("data/modelXgb.RData")
load("data/data_2015.RData")

deklaag<- sort(unique(raf_fd$DEKLAAGSOORT))
wegnummers<- c("Alle",sort(unique((raf_fd$WEG))))

params<- list("Deklaagsoort"="DEKLAAGSOORT",
              "District" = "district",
              "Bodemtype" = "bodem",
              "Bermtype" = "bermtype",
              "Strook" = "STROOK",
              "Boogstraal [m]"= "boogstraal.mean",
              "Verkanting [-]" = "verkanting.mean",
              "Stroefheid in jaar van eerste schade [?]" = "stroefheid",
              "Rijspoordiepte in jaar van eerste schade [mm]" = "dwars",
              "Langsvlakheid in jaar van eerste schade [mm]" = "langs",
              "Netwerkcategorie" = "cat",
              "Kunstwerk" = "kunstwerk",
              "Wegnummer"=  "WEG",
              "Afstand tot dichtsbijzijnde invoegstrook [m]" = "distance_invoeg",
              "Afstand tot dichtsbijzijnde uitvoegstrook [m]" = "distance_uitvoeg",
              "Gemiddelde verkeersintensiteit Alles (INWEVA) [vtg/etm]" = "gem_Inw_etm_AL",
              "Gemiddelde verkeersintensiteit L1 (INWEVA) [vtg/etm]" = "gem_Inw_etm_L1",
              "Gemiddelde verkeersintensiteit L2 (INWEVA) [vtg/etm]" = "gem_Inw_etm_L2",
              "Gemiddelde verkeersintensiteit L3 (INWEVA) [vtg/etm]" = "gem_Inw_etm_L3",
              "Min. temperatuur op datum van aanleg [C], dichtsbijzijnde KNMI station" = "aanleg.TN",
              "Max. temperatuur op datum van aanleg [C], dichtsbijzijnde KNMI station" = "aanleg.TX",
              "Max. windsnelheid op datum van aanleg [m/s], dichtsbijzijnde KNMI station" = "aanleg.FHX",
              "Neerslag op datum van aanleg [mm], dichtsbijzijnde KNMI station" = "aanleg.RH",
              "Cumulatief aantal vorst-dooi cycli over de levensduur [-]" = "freeze_count_1_cum",
              "Aantal bomen binnen 100m van de weg [-]" = "t_num100"
              
)


port <- Sys.getenv('PORT') 
print(port)

shiny::runApp('rafeling_app',host = '0.0.0.0', port = as.numeric(port))


