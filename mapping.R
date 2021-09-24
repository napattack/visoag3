
library(shiny)
library(leaflet)
library(tidyverse)
library(htmlwidgets)
library(DT)
library(plyr)
library(rsconnect)


#loading our datasets
map.data<-read.csv("data/mapping.csv")
##i planed to make zoom funtion, but sofar not working, so i filter it out here. 
map.data<-map.data%>%filter(ZoomLevel=="3")
ui <- fluidPage(
  includeCSS("styles.css"),
  
  titlePanel("Bundesländer-Atlas Open Access (BAOA)"),
  
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    width = 2,
    helpText("Find your filter here"),
    checkboxGroupInput(inputId = "insType",label="Institution Type",
                       choices = list("Universität","Hochschule","ForschungsInstitution")),
    checkboxGroupInput(inputId = "bund",label="Bundesländer",
                       choices = list( "Baden-Württemberg", "Sachsen-Anhalt" ,"Berlin"                
                                       ,"Hamburg","Nordrhein-Westfalen" , "Hessen"                
                                       ,"Bayern","Niedersachsen"  , "Brandenburg"           
                                       , "Mecklenburg-Vorpommern" ,"Schleswig-Holstein"  ,"Thüringen"             
                                       ,"Sachsen" ,"Rheinland-Pfalz" , "Saarland"              
                                       ,"Bremen")),
    downloadButton("downloadData","Download")),
  mainPanel(
    tabsetPanel(
      id="tabs",
      tabPanel(
        title="Map",
        leafletOutput("mymap",width = "1200px",height = "700px")
        
      ),
      tabPanel(
        title="Map data",
        tableOutput("mytable")
      ),
      tabPanel(
        title="User guide",
        p("Der BAOA ist ein Tool zur Informationsvisualisierung und -exploration von institutioneller Offenheit deutscher(, öffentlich geförderter) Hochschulen, Universitäten und Forschungseinrichtungen. Er bietet Informationen auf den Ebenen “Deutschland”, der Bundesländer und der einzelnen Einrichtungen. Zum Zweck der Einordnung und Vergleichbarkeit wurde ein System der Bewertung der Offenheit entwickelt, der Monitor institutioneller Offenheit, welches basierend auf sechs Indikatoren (siehe Weitere Informationen) eine Bewertung der Offenheit zulässt und auf allen drei Ebenen abbildet."),
        br(),
        p("Mit unserer Kartenvisualisierung können Sie die Open-Access-Umsetzungsrate in 6 Dimensionen finden: Open-Access-Website, Open-Access-Beauftragte, Open-Access-Politik, Repositorium url, Berlin Erklärung, Open-Access 2020. Bisher haben wir nur zwei Filteroptionen, Institutionstyp und Bundesländer, die Sie standardmäßig mit UND verknüpfen können, innerhalb jedes Filters wird standardmäßig mit ODER verknüpft.Wir haben drei Panels auf dieser Website, einschließlich einer Kartenvisualisierung unseres OA-Indikators. Das Datenpanel, das die Daten in Tabellenform darstellt. Und die Benutzerführung, die Sie gerade lesen:D"),
        br(),
        strong("Data source"),
        p("Die Datensammlung des BAOA entsteht im Rahmen des Projekts open-access.network durch den Projektpartner Open-Access-Büro Berlin an der Universitätsbibliothek der Freien Universität Berlin. Allgemeine Informationen zum Projekt und dem Konzept des Bundesländer-Atlas können in der aktuellen Version 3.0 des Konzeptpapiers abgerufen werden (DOI: https://doi.org/10.5281/zenodo.4644125). An der Erstellung der Daten waren beteiligt: Maxi Kindling, Sophie Kobialka, Maike Neufend, Agnieszka Wenninger (Stand: 4.9.2021)"),
        br(),
        p("Der Quellcode ist verfügbar unter https://github.com/napattack/visoag3, contact: yiwang@hu-berlin.de"),
        strong("contributors"),
        p("Philipp","Lydia","Yi Wang")
      )
    )))



pal<-colorBin(palette = "OrRd",7,domain = map.data$OpenScore_institution)
server <- function( input, output){
  dataset<- reactive({
    if(is.null(input$insType)&is.null(input$bund)){
      dataset<-map.data
    }else if(is.null(input$insType)&!is.null(input$bund)){
      dataset<-map.data%>%
        filter(X%in%input$bund)
    }else if(is.null(input$bund)&!is.null(input$insType)){
      dataset<-map.data%>%
        filter(institutionType%in%c(input$insType))
    }
    else{
      dataset<-map.data%>%
        filter(institutionType%in%c(input$insType))%>%
        filter(X%in%input$bund)
    }
  })
  initial_lat =50.5647
  initial_lng = 9.68750
  initial_zoom = 5
  
  #make interactive app 
  output$mymap <- renderLeaflet(
    {
      leaflet()%>%
        addProviderTiles(provider="CartoDB.Positron")%>%
        # addTiles(tags$a(paste0("Map of OA level by",as.character(input$insType))),)%>%
        setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
        addCircleMarkers(
          data= dataset(),group=dataset()$ZoomLevel,
          lat =dataset()$lat,lng = dataset()$lon,radius =8,
          color = ~pal(OpenScore_institution),fillColor = ~pal(OpenScore_institution),opacity = 1,fillOpacity = .8,
          label =paste0(dataset()$Name.der.Institution,": ",as.character(dataset()$OpenScore_institution),"% ", as.character(dataset()$X),": ",as.character(dataset()$bundesländer_score),"% Germany:",as.character(dataset()$country_score),"%"),
          stroke = FALSE,
          popup=dataset()$additional.infos)%>%
      #  groupOptions("3", zoomLevels = 7:18)%>%
          addLegend("bottomright",
                  pal=pal,
                  values=paste0(as.character(~OpenScore_institution),"%"),
                  opacity=0.7 ,
                  title = "Rate institutioneller Offenheit")
      
     
      
      } )
  
  output$mytable<-renderTable({
    dataset()
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("BAOA",date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataset()[,c(1:5,15:26)], file,row.names = FALSE)
    }
  )
  
}


shinyApp(ui, server)
