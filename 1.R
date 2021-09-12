

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


