#futher development 
#additional city level?
download.file("https://github.com/iceweasel1/COVID-19-Germany/blob/master/germany_with_source.csv",destfile = 
                "data/Bundesländer_geo.csv")
library(vroom)
Bund_geo<-read.csv("data/Bund_geo.csv",encoding = "UTF-8")
Bund_geo<-Bund_geo[,c(3,4,5,6)]
install.packages("flexdashboard")
#Trägerschaft<-vroom("data/Bundesländer-Atlas_Institutionen_Trägerschaft_Hochschulen.csv",delim = ";")
