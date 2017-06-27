server <- function(input, output) {
  
  #create dataset based on filters
  data<- reactive({
    if(input$weg == "Alle" & input$jaar == "Alle"){
      raf_fd[which(raf_fd$DEKLAAGSOORT %in% input$deklaag),
             c(input$variabele,"leeftijd","x.wgs","y.wgs","WEG.BAAN.STROOK.VAN","WEG","jaar","AANLEGDATUM","DEKLAAGSOORT")]
      
    }
    else if (input$weg != "Alle" & input$jaar == "Alle"){
      raf_fd[which(raf_fd$DEKLAAGSOORT %in% input$deklaag&
                     raf_fd$WEG %in% input$weg),
             c(input$variabele,"leeftijd","x.wgs","y.wgs","WEG.BAAN.STROOK.VAN","WEG","jaar","AANLEGDATUM","DEKLAAGSOORT")]
      
    }
    else if (input$weg == "Alle" & input$jaar != "Alle"){
      raf_fd[which(raf_fd$DEKLAAGSOORT %in% input$deklaag&
                     raf_fd$jaar == input$jaar),
             c(input$variabele,"leeftijd","x.wgs","y.wgs","WEG.BAAN.STROOK.VAN","WEG","jaar","AANLEGDATUM","DEKLAAGSOORT")]
    }
    else {
      raf_fd[which(raf_fd$DEKLAAGSOORT %in% input$deklaag&
                     raf_fd$jaar == input$jaar& raf_fd$WEG %in% input$weg),
             c(input$variabele,"leeftijd","x.wgs","y.wgs","WEG.BAAN.STROOK.VAN","WEG","jaar","AANLEGDATUM","DEKLAAGSOORT")]
      
    }
    
    
  })
  ##get correct label for plotting
  lab<- reactive({
    names(params)[grep(as.character(input$variabele),params)]
  })
  
  ##dataset voor leaflet
  data2<- reactive({
    if(input$weg2 == "Alle" & input$jaar == "Alle"){
      raf_fd[which(raf_fd$DEKLAAGSOORT %in% input$deklaag),
             c(input$variabele,"leeftijd","x.wgs","y.wgs","WEG.BAAN.STROOK.VAN","WEG","jaar","AANLEGDATUM","DEKLAAGSOORT")]
      
    }
    else if (input$weg2 != "Alle" & input$jaar == "Alle"){
      raf_fd[which(raf_fd$DEKLAAGSOORT %in% input$deklaag&
                     raf_fd$WEG %in% input$weg2),
             c(input$variabele,"leeftijd","x.wgs","y.wgs","WEG.BAAN.STROOK.VAN","WEG","jaar","AANLEGDATUM","DEKLAAGSOORT")]
      
    }
    else if (input$weg2 == "Alle" & input$jaar != "Alle"){
      raf_fd[which(raf_fd$DEKLAAGSOORT %in% input$deklaag&
                     raf_fd$jaar == input$jaar),
             c(input$variabele,"leeftijd","x.wgs","y.wgs","WEG.BAAN.STROOK.VAN","WEG","jaar","AANLEGDATUM","DEKLAAGSOORT")]
    }
    else {
      raf_fd[which(raf_fd$DEKLAAGSOORT %in% input$deklaag&
                     raf_fd$jaar == input$jaar& raf_fd$WEG %in% input$weg2),
             c(input$variabele,"leeftijd","x.wgs","y.wgs","WEG.BAAN.STROOK.VAN","WEG","jaar","AANLEGDATUM","DEKLAAGSOORT")]
      
    }
    
    
  })
  ##Plot analysetab
  output$plot1 <- renderPlot({
    dat<-as.data.frame(data())
    la<- lab()
    colnames(dat)<- c("var","leeftijd","x.wgs","y.wgs","WEG.BAAN.STROOK.VAN","WEG","jaar","AANLEGDATUM")
    
    if(class(dat$var) %in% c("character","factor")){
      dat$var<- as.factor(dat$var)
      
      g1<- ggplot(dat,aes(x=var,y=leeftijd,fill=var))+geom_boxplot()+xlab("")+ylab("Leeftijd [j]")+ggtitle("Leeftijd eerste schade per categorie")+
        theme(  axis.text.x = element_text(angle=40, hjust=1),legend.position =  "none") 
      g2<- ggplot(dat,aes(x=var))+geom_histogram(stat="count")+ylab("Aantal")+ggtitle("Aantal hectometervakken in deze categorie")+xlab(la)+
        theme(  axis.text.x = element_text(angle=40, hjust=1),legend.position =  "none") 
      
      grid.arrange(g1,g2,heights=c(0.7,0.3))
    }
    else if(class(dat$var) %in% c("numeric","integer")){
      
      
      
      ggplot(dat,aes(x=var,y=leeftijd))+geom_jitter(alpha=0.2,color="orange")+xlab(la)+ylab("Leeftijd [j]")+stat_smooth()+xlim(min(dat$var,na.rm = T),quantile(dat$var,0.95,na.rm=T))+
        annotate("text", x = quantile(dat$var,0.95,na.rm=T), y = (max(dat$leeftijd,na.rm=T)-1), hjust=1,label = paste("r =",round(cor(dat$leeftijd,dat$var,use = "pairwise.complete.obs"),2)))
    }
  })
  
  output$plot2 <- renderPlot({
    dat<-as.data.frame(data())
    a<- ggplot(dat,aes(x=leeftijd))+geom_histogram(binwidth = 1,color="black",fill="cyan4",alpha=0.7)+
      xlab("Leeftijd [j]")+ylab("Aantal hectometervakken")+#ggtitle("Aantal hectometervakken in selectie")+
      theme(  axis.text.x = element_text(angle=40, hjust=1),legend.position =  "none") 
    if (input$facet == T){return(a+ facet_wrap(~DEKLAAGSOORT))}
    else {return(a)}
  })
  
  
  ######Leaflet tab######
  
  output$map <- renderLeaflet({
    
    dat2<-as.data.frame(data2())
    dat2<- dat2[complete.cases(dat2$x.wgs),]
    dat2<- dat2[order(-dat2$leeftijd),]
    #pal <- colorpal()
    pal <- colorNumeric(
      palette = "Spectral",
      c(0,15)
    )
    
    leaflet(dat2) %>%
      addProviderTiles("CartoDB.Positron"
      ) %>%
      clearShapes() %>%
      addCircles(lng=dat2$x.wgs, lat=dat2$y.wgs, radius = 20, weight=1,color =  ~pal(leeftijd),#"#777777",
                 fillColor = ~pal(leeftijd), fillOpacity = 0.7, popup = ~paste("Leeftijd eerste schade:",leeftijd, "<br>",
                                                                               "ID:", WEG.BAAN.STROOK.VAN, "<br>",
                                                                               "WEG:", WEG, "<br>",
                                                                               "DEKLAAGSOORT: ",DEKLAAGSOORT,"<br>",
                                                                               "AANLEGDATUM:", AANLEGDATUM, "<br>",
                                                                               "Jaar van eerste schade:",jaar
                 )
                 
                 
      ) %>%
      addLegend("bottomright", pal = pal, values = c(0,15),
                title = "Leeftijd eerste schade",
                labFormat = labelFormat(),
                opacity = 1
      )
    
    
  })
  
  #####Predictive model#####
  
  #output van beste ML model

  
  data3<- reactive({
    data_2015[which(data_2015$WEG == input$weg3 &
                      data_2015$VAN >= input$van &
                      data_2015$TOT <= input$tot &
                      ! data_2015$DEKLAAGSOORT %in% c("CEG-E")   ## Niet in trainset, dus model crasht
    ),]
    
  })
  
  
  
  output$table <- renderDataTable({
    dat3<-as.data.frame(data3())
    
    #dat3$pred<- round(predict(modelXgb,dat3,na.action=na.pass),1)
    dat3<- dat3[,c("WEG","VAN","TOT","BAAN","STROOK","DEKLAAGSOORT","AANLEGDATUM","pred")]
    colnames(dat3) = c("WEG","VAN","TOT","BAAN","STROOK","DEKLAAGSOORT","AANLEGDATUM","Voorspelde leeftijd [j]")
    dat3$`Jaar eerste schade`<- round(as.numeric(format(dat3$AANLEGDATUM, "%Y")) + dat3$`Voorspelde leeftijd [j]`,0)
    
    dat3 <- datatable(dat3,rownames = F) %>%
      formatStyle(c("Voorspelde leeftijd [j]","Jaar eerste schade"), backgroundColor = 'lightblue', fontWeight = 'bold')
    
    
    return(dat3)
  })
  
  output$map2 <- renderLeaflet({
    
    dat3<-as.data.frame(data3())
    dat3<- dat3[complete.cases(dat3$x.wgs),]
    #dat3$pred<- round(predict(modelXgb,dat3,na.action=na.pass),1)
    dat3$`Jaar eerste schade`<- round(as.numeric(format(dat3$AANLEGDATUM, "%Y")) + dat3$pred,0)
    
    #dat3<- dat3[order(-dat3$leeftijd),]
    #pal <- colorpal()
    pal <- colorNumeric(
      palette = "Spectral",
      c(0,15)
    )
    
    leaflet(dat3) %>%
      addProviderTiles("CartoDB.Positron"
      ) %>%
      clearShapes() %>%
      addCircles(lng=dat3$x.wgs, lat=dat3$y.wgs, radius = 20, weight=1,color =  ~pal(leeftijd),#"#777777",
                 fillColor = ~pal(pred), fillOpacity = 0.7, popup = ~paste("Voorspelling:",pred, "<br>",
                                                                           "Jaar eerste schade:",`Jaar eerste schade`,"<br>",
                                                                           "WEG:", WEG, "<br>",
                                                                           "BAAN:",BAAN,"<br>",
                                                                           "STROOK:",STROOK,"<br>",
                                                                           "DEKLAAGSOORT: ",DEKLAAGSOORT,"<br>",
                                                                           "AANLEGDATUM:", AANLEGDATUM #, "<br>",
                                                                           #"Jaar van eerste schade:",jaar
                 )
                 
                 
      ) %>%
      addLegend("bottomright", pal = pal, values = c(0,15),
                title = "Voorspelde leeftijd eerste schade [j]",
                labFormat = labelFormat(),
                opacity = 1
      )
    
    
  })
  
}

