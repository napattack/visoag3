berlin.data<-map.data%>%filter(map.data$X=="Brandenburg")

map.data<-berlin.data
nrow(map.data%>%
       filter(map.data$bool_OA_Website=="TRUE"))/nrow(map.data)

nrow(map.data%>%
       filter(map.data$bool_OA.Policy=="TRUE"))/nrow(map.data)
nrow(map.data%>%
       filter(map.data$bool_OA_Beauftragte=="TRUE"))/nrow(map.data)


nrow(map.data%>%
       filter(map.data$bool_OA2020=="TRUE"))/nrow(map.data)
nrow(map.data%>%
       filter(map.data$bool_Berliner.Erklärung=="TRUE"))/nrow(map.data)

nrow(map.data%>%
       filter(map.data$bool_Repositorium.URL=="TRUE"))/nrow(map.data)
   
                
     