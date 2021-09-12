library(shiny)
library(leaflet)
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
add.info<-institution[,c("Wikidata.ID","OA.Webseite.der.Institution","OA.Beauftragte.r.URL","X.Funktions..Emailadresse","OA.Policy.URL","OA.Leitlinie.URL","Repositorium.URL","OA.Verlag.URL")]
#clean up Reschercher fehlt, na,nein 

add.info<-replace_with_na(data=add.info,replace = list(add.info$OA.Webseite.der.Institution=="Recherche fehlt"))

add.info[add.info$OA.Webseite.der.Institution=="Recherche fehlt",2]<-" "

ggsave("plots/map.html")
#add.score<-read.csv("data/cleanScore.csv")

#Infos<-left_join(add.score%>%filter(!is.na(Wikidata.ID))%>%filter(Wikidata.ID!="Recherche fehlt"),add.info%>%filter(!is.na(Wikidata.ID))%>%filter(Wikidata.ID!="Recherche fehlt"),by=c("Wikidata.ID"="Wikidata.ID"))
#Infos<-Infos[order(Infos$Wikidata.ID,-abs(Infos$OpenScore_institution)),]
#Infos<-Infos[!duplicated(Infos$Wikidata.ID),]


additional_info_join<-left_join(unique_join,add.info,by=c("Wikidata.ID"="Wikidata.ID"))
colnames(unique_join)
map.data<-as.data.frame.list(unique_join[c( "lon","lat","OpenScore_institution")])
##input additional information as a pop up

download.file("https://github.com/iceweasel1/COVID-19-Germany/blob/master/germany_with_source.csv",destfile = 
                "data/Bundesländer_geo.csv")
library(vroom)
Bund_geo<-read.csv("data/Bund_geo.csv",encoding = "UTF-8")
Bund_geo<-Bund_geo[,c(3,4,5,6)]

#m<-leaflet()%>%addTiles()%>%
#  addCircleMarkers(data=map.data,lat =map.data$lat,lng = map.data$lon,radius =~10*OpenScore_institution*0.01) 

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("OpenAccess Visibility"),
 leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function( input, output, session) {
  
   points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet()%>%addTiles()%>%
      addCircleMarkers(data=map.data,lat =map.data$lat,lng = map.data$lon,radius =~10*OpenScore_institution*0.01,color="#BB3E03",label = map.data$OpenScore_institution) 
  })
}

shinyApp(ui, server)
  
  