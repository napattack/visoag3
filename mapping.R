
library(shiny)
library(leaflet)
library(tidyverse)
library(htmlwidgets)
library(DT)
library(plyr)
library(stringr)
getData('GADM', country='UKR', level=1)

#clean datatable and saved, you dont have to run this
#Score<-read.csv("data/cleanScore.csv")
#data<-read.csv("data/geo_data_woduplicateitem.csv")
#join<-left_join(Score,data)



#clean table, for no wikidata id 
#clean_join<-join%>%
  #filter(!is.na(Wikidata.ID))%>%
 # filter(Wikidata.ID!="Recherche fehlt")
#unique_join<-clean_join[order(clean_join$Wikidata.ID,-abs(clean_join$OpenScore_institution)),]
#unique_join<-unique_join%>%
 # filter(!is.na(item))

#length(unique(unique_join$Wikidata.ID))
#write.csv(unique_join,"data/mapping.csv",row.names = FALSE)



#map.data<-unique_join
geo_de<-read.csv("data/Geo_DE.csv",fileEncoding = "UTF-8-BOM")
geo_de$LON<-as.numeric((geo_de$lon))
geo_de$LAT<-as.numeric((geo_de$lat))


map.data<-read.csv("data/mapping.csv")
map.data$ZoomLevel<-c("3")

level2<-left_join(map.data,geo_de,by=c("X"="place"))
level2<-level2%>%
  filter(!is.na(LON))
level2<-as.data.frame(unique(level2[,c("X","LON","LAT","bundesländer_score","country_score")]))
colnames(level2)<-c("X","lon","lat","bundesländer_score","country_score")
data<-rbind.fill(map.data,level2)
data[is.na(data$ZoomLevel),33]<-"2"
data[data$ZoomLevel==2,16]<-data[data$ZoomLevel==2,17]
map.data<-data

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
        leafletOutput("mymap",width = "1400px",height = "700px")
      
      ),
      tabPanel(
        title="Map data",
        tableOutput("mytable")
      ),
      tabPanel(
        title="User guide",
        p("Der BAOA ist ein Tool zur Informationsvisualisierung und -exploration von institutioneller Offenheit deutscher(, öffentlich geförderter) Hochschulen, Universitäten und Forschungseinrichtungen. Er bietet Informationen auf den Ebenen “Deutschland”, der Bundesländer und der einzelnen Einrichtungen. Zum Zweck der Einordnung und Vergleichbarkeit wurde ein System der Bewertung der Offenheit entwickelt, der Monitor institutioneller Offenheit, welches basierend auf sechs Indikatoren (siehe Weitere Informationen) eine Bewertung der Offenheit zulässt und auf allen drei Ebenen abbildet."),
        br(),
        p("With our map visualisation, you could find the OA implemented rate in 6 dimentions, OA website, OA Beauftragte, OA Policy,Repositorium url, Berlin Erklärung, OA 2020"),
        p("Sofar we only have two fiters options, institution type and Bundesländer, you could combine them with AND operation defaultly, inside each filter is combining with OR defaultly."),
        p("we have three panels in this website, including map visualization of our OA indicator. Data panel, which present in table way. And User guide panel, which is you are reading right now:D"),
        br(),
        strong("Data source"),
        p("Die Datensammlung des BAOA entsteht im Rahmen des Projekts open-access.network durch den Projektpartner Open-Access-Büro Berlin an der Universitätsbibliothek der Freien Universität Berlin. Allgemeine Informationen zum Projekt und dem Konzept des Bundesländer-Atlas können in der aktuellen Version 3.0 des Konzeptpapiers abgerufen werden (DOI: https://doi.org/10.5281/zenodo.4644125). An der Erstellung der Daten waren beteiligt: Maxi Kindling, Sophie Kobialka, Maike Neufend, Agnieszka Wenninger (Stand: 4.9.2021)"),
        br(),
        p("Source code is available at https://github.com/napattack/visoag3, contact: yiwang@hu-berlin.de"),
        strong("contributors"),
        p("Philipp","Lydia","Yi Wang")
      )
      )))


pal<-colorBin(palette = "OrRd",7,domain = map.data$OpenScore_institution)
server <- function( input, output,session) {
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
  #make interactive app 
  output$mymap <- renderLeaflet(
    {
     leaflet()%>%
      addProviderTiles(provider="CartoDB.Positron")%>%
     # addTiles(tags$a(paste0("Map of OA level by",as.character(input$insType))),)%>%
      addCircleMarkers(
        data= dataset(),group=dataset()$ZoomLevel,
        lat =dataset()$lat,lng = dataset()$lon,radius =8,
        color = ~pal(OpenScore_institution),fillColor = ~pal(OpenScore_institution),opacity = 1,fillOpacity = .8,
        label = str_replace_na(paste0(dataset()$Name.der.Institution,": ",as.character(dataset()$OpenScore_institution),"% ", as.character(dataset()$X),": ",as.character(dataset()$bundesländer_score),"% Germany:",as.character(dataset()$country_score),"%"),replacement=""),
        stroke = FALSE,
        popup=dataset()$additional.infos)%>%
        
        groupOptions("3", zoomLevels = 6:18)%>%
      
        addLegend("bottomright",
                pal=pal,
                values=paste0(as.character(~OpenScore_institution),"%"),
                opacity=0.7 ,
                title = "Rate institutioneller Offenheit") } )
    
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
  
  