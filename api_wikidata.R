##API WIKIdata 
library(httr)
library(xml2)
#install.packages("RWDataPlyr")
library(RWDataPlyr)
library(jsonlite)
data<-read.csv("data/cleanScore.csv")
wikiID<-data[,c(1)]
url="http://www.wikidata.org/entity/"

URLs<-paste0(url,wikiID[1])
query<-GET(URLs)
js<-jsonlite::toJSON(content(query))
js
rdf<-read.rdf(query)
