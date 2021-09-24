
library("devtools")
library("WikidataQueryServiceR")
library("WikipediaR")
library("RCurl")
library("tidyverse")

data<-read.csv("data/cleanScore.csv")
#we filter only items with unique wikidata id
data<-data%>%
  filter(!is.na(Wikidata.ID))%>%
  filter(Wikidata.ID!="Recherche fehlt")

item<-data[,c(1)]
uniqueItem<-unique(item)#unique 701, 713 all id
query_results<-as.data.frame(matrix(nrow=1,ncol=5))
colnames(query_results)<-c( "item" , "name" ,"coord" ,"lat" ,  "lon" )
#225, 336 rechercher felht get rid of 262 350
#we want query every item in our wikidata id list
for (i in c(1:711)){
query<-paste0("SELECT DISTINCT ?item ?name ?coord ?lat ?lon
WHERE
{
 ?item wdt:P131* wd:",as.character(uniqueItem[i])," .
 ?item wdt:P625 ?coord .
 ?item p:P625 ?coordinate .
 ?coordinate psv:P625 ?coordinate_node .
 ?coordinate_node wikibase:geoLatitude ?lat .
 ?coordinate_node wikibase:geoLongitude ?lon .
 SERVICE wikibase:label {
 bd:serviceParam wikibase:language 'ca' .
 ?item rdfs:label ?name
 }
}")
#we binding the results in a dataframe and save 
query_result<-query_wikidata(query)
lapply(query_result,dim)
query_results<-rbind(query_results,query_result)

}



write_csv(query_results,"data/geo_data.csv",row.names = FALSE)
