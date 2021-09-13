library(shiny)
library(leaflet)
library(tidyverse)
library(htmlwidgets)
#clean up geodata, somehow they have duplicated wikidata id
#data<-read.csv("data/geo_data.csv")
#data<-data%>%
 # filter(!is.na(item))
#data<-data[!duplicated(data$item),]
#write.csv(data,"data/geo_data_woduplicateitem.csv",row.names = FALSE)

#data$Wikidata.ID<-substr(data$item,start=1+unlist(gregexpr("/",data$item))[4],stop = nchar(data$item))

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
#write.csv(unique_join,"data/mapping.csv")
##get map germany, and additional info from original data



map.data<-unique_join
##input additional information as a pop up


#m<-leaflet()%>%addTiles()%>%
#  addCircleMarkers(data=map.data,lat =map.data$lat,lng = map.data$lon,radius =~10*OpenScore_institution*0.01) 

ui <- fluidPage(
  titlePanel("Erfüllte OA-Indikatoren Rate"),
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
server <- function( input, output, session) {
  #make interactive app 
  output$mymap <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(provider="CartoDB.Positron")%>%
      addCircleMarkers(
        data=map.data,lat =map.data$lat,lng = map.data$lon,radius =~10*OpenScore_institution*0.01,
        color = ~pal(OpenScore_institution),fillColor = ~pal(OpenScore_institution),opacity = 1,fillOpacity = .5,
        label = paste0(map.data$Name.der.Institution,": ",as.character(map.data$OpenScore_institution),"% ", as.character(map.data$Ort),": ",as.character(map.data$bundesländer_score),"% Germany:",as.character(map.data$country_score),"%"),
        options = markerOptions(minZoom=15,maxZoom=20)
        
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
  
  