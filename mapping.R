library(shiny)
library(leaflet)
library(tidyverse)
library(htmlwidgets)


#Score<-read.csv("data/cleanScore.csv")
#data<-read.csv("data/geo_data_woduplicateitem.csv")
#join<-left_join(Score,data)

#write.csv(test,"data/join_score_wiki.csv",row.names = FALSE)

#clean table, for no wikidata id 
#clean_join<-join%>%
 # filter(!is.na(Wikidata.ID))%>%
#  filter(Wikidata.ID!="Recherche fehlt")
#unique_join<-clean_join[order(clean_join$Wikidata.ID,-abs(clean_join$OpenScore_institution)),]
#unique_join<-unique_join%>%
 # filter(!is.na(item))
#length(unique(unique_join$Wikidata.ID))
#write.csv(unique_join,"data/mapping.csv",row.names = FALSE)



#map.data<-unique_join

map.data<-read.csv("data/mapping.csv")

ui <- fluidPage(
  titlePanel("Bundesländer-Atlas Open Access (BAOA)"),
  hr(),
  br(),
  tags$a("Der BAOA versteht sich als Tool zur Informationsvisualisierung und -exploration."),
  br(),
  sidebarLayout(
    # Sidebar panel for inputs ----
  sidebarPanel(width = 2,
               br(),
               tags$a("Find your filters here"),
  checkboxGroupInput(inputId = "insType",label="Institution Type",
                       choices = list("Universität","Hochschule","ForschungsInstitution")),
  checkboxGroupInput(inputId = "bund",label="Bundesländer",
                     choices = list( "Baden-Württemberg", "Sachsen-Anhalt" ,"Berlin"                
                                     ,"Hamburg","Nordrhein-Westfalen" , "Hessen"                
                                     ,"Bayern","Niedersachsen"  , "Brandenburg"           
                                     , "Mecklenburg-Vorpommern" ,"Schleswig-Holstein"  ,"Thüringen"             
                                     ,"Sachsen" ,"Rheinland-Pfalz" , "Saarland"              
                                     ,"Bremen"))
  
  ),
  mainPanel(
  width = 10,
  leafletOutput("mymap"),
  tableOutput("mydata"),
  p(),
  hr(),
  br(),
  tags$a("Die Datensammlung des BAOA entsteht im Rahmen des Projekts open-access.network durch den Projektpartner Open-Access-Büro Berlin an der Universitätsbibliothek der Freien Universität Berlin. Allgemeine Informationen zum Projekt und dem Konzept des Bundesländer-Atlas können in der aktuellen Version 3.0 des Konzeptpapiers abgerufen werden (DOI: https://doi.org/10.5281/zenodo.4644125). An der Erstellung der Daten waren beteiligt: Maxi Kindling, Sophie Kobialka, Maike Neufend, Agnieszka Wenninger (Stand: 4.9.2021)")
    )
  )
)
pal<-colorBin(palette = "OrRd",9,domain = map.data$OpenScore_institution)
server <- function( input, output) {
  #make interactive app 
  output$mymap <- renderLeaflet({
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
     
    leaflet()%>%
      addProviderTiles(provider="CartoDB.Positron")%>%
     # addTiles(tags$a(paste0("Map of OA level by",as.character(input$insType))),)%>%
      addCircleMarkers(
        data= dataset,
        lat =dataset$lat,lng = dataset$lon,radius =~15*OpenScore_institution*0.01,
        color = ~pal(OpenScore_institution),fillColor = ~pal(OpenScore_institution),opacity = 1,fillOpacity = .8,
        label = paste0(dataset$Name.der.Institution,": ",as.character(dataset$OpenScore_institution),"% ", as.character(dataset$Ort),": ",as.character(dataset$bundesländer_score),"% Germany:",as.character(dataset$country_score),"%"),
        stroke = FALSE
        ,popup=dataset$additional.infos)%>%
        
      addLegend("bottomright",
                pal=pal,
                values=paste0(as.character(~OpenScore_institution),"%"),
                opacity=0.7 ,
                title = "Erfüllte OA-Indikatoren Rate") } )
      
}


shinyApp(ui, server)
  
  