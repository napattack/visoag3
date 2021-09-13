library(shiny)
library(leaflet)
library(tidyverse)
library(htmlwidgets)


Score<-read.csv("data/cleanScore.csv")
data<-read.csv("data/geo_data_woduplicateitem.csv")
join<-left_join(Score,data)

#write.csv(test,"data/join_score_wiki.csv",row.names = FALSE)

#clean table, for no wikidata id 
clean_join<-join%>%
  filter(!is.na(Wikidata.ID))%>%
  filter(Wikidata.ID!="Recherche fehlt")
unique_join<-clean_join[order(clean_join$Wikidata.ID,-abs(clean_join$OpenScore_institution)),]
unique_join<-unique_join%>%
  filter(!is.na(item))
length(unique(unique_join$Wikidata.ID))
#write.csv(unique_join,"data/mapping.csv",row.names = FALSE)



map.data<-unique_join
map.data<-map.data%>%
  mutate(add=paste)


ui <- fluidPage(
  titlePanel("Erfüllte OA-Indikatoren Rate"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  absolutePanel(id="controls",
                style="z-index:500;",
                class = "panel panel-default",
                draggable = TRUE),
  hr(),
  br(),
  tags$a("How we measure the OA level?\n"),
  br(),
  tags$a("we choose 6 indicators, OA Webseite of institution, OA-Beauftragte/r, OA Policy, Repositorium URL,Berliner Erklärung, OA2020."),
  checkboxGroupInput(inputId = "idicator",label="choose your OA indicator"),
  leafletOutput("mymap"),
  tableOutput("mydata"),
  p()
 
)
pal<-colorBin(palette = "OrRd",9,domain = map.data$OpenScore_institution)
server <- function( input, output) {
  #make interactive app 
  output$mymap <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(provider="CartoDB.Positron")%>%
      addCircleMarkers(
        data=map.data,lat =map.data$lat,lng = map.data$lon,radius =~10*OpenScore_institution*0.01,
        color = ~pal(OpenScore_institution),fillColor = ~pal(OpenScore_institution),opacity = 1,fillOpacity = .5,
        label = paste0(map.data$Name.der.Institution,": ",as.character(map.data$OpenScore_institution),"% ", as.character(map.data$Ort),": ",as.character(map.data$bundesländer_score),"% Germany:",as.character(map.data$country_score),"%"),
        popup=map.data$additional.infos
        
        
        )%>%
      addLegend("bottomright",
                pal=pal,
                values=paste0(as.character(map.data$OpenScore_institution),"%"),
                opacity=0.7 ,
                title = "Erfüllte OA-Indikatoren Rate"
                )
     
     
    
                        
  } )
  output$mydata<-renderDataTable(expr = map.data)
}


shinyApp(ui, server)
  
  