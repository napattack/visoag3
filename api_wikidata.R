
library("devtools")
library("WikidataQueryServiceR")
library("WikipediaR")
library("RCurl")

data<-read.csv("data/cleanScore.csv")

data<-data%>%
  filter(!is.na(Wikidata.ID))
item<-data[,c(1)]
unique(item)#unique 701, 713 all id
query_results<-as.data.frame(matrix(nrow=1,ncol=5))
colnames(query_results)<-c( "item" , "name" ,"coord" ,"lat" ,  "lon" )
#225, 336 rechercher felht get rid of 
for (i in c(1:251,253:335,337:713)){
query<-paste0("SELECT DISTINCT ?item ?name ?coord ?lat ?lon
WHERE
{
 ?item wdt:P131* wd:",as.character(item[i])," .
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
query_result<-query_wikidata(query)
query_results<-rbind(query_results,query_result)

}


write.csv(query_results,"/data/geo_data.csv",row.names = FALSE)



1+1


query_results%>%
  filter(!is.na(item))




####
nrow(query_results)

query<-query_wikidata('SELECT DISTINCT ?item ?name ?coord ?lat ?lon
WHERE
{
 ?item wdt:P131* wd:Q315175.
 
 ?item wdt:P625 ?coord .
 ?item p:P625 ?coordinate .
 ?coordinate psv:P625 ?coordinate_node .
 ?coordinate_node wikibase:geoLatitude ?lat .
 ?coordinate_node wikibase:geoLongitude ?lon .
 SERVICE wikibase:label {
 bd:serviceParam wikibase:language "ca" .
 ?item rdfs:label ?name
 }
}
ORDER BY ASC (?name)')

lapply(query,dim)


library(httr)
library(xml2)
#install.packages("RWDataPlyr")
library(RWDataPlyr)
library(jsonlite)

url="http://www.wikidata.org/entity/"
ID<-test[,1]
URLs<-paste0(url,ID[1])
query<-GET(URLs)
str(query$content)
content_request <- httr::content(query, as="text")

library("XML")
wiki_xml <- read_xml(content_request)


