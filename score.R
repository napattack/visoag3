
library(tidyverse)
library(dplyr)

#general exploration
#input data 
institution<-read.delim("data/Bundesländer-Atlas - Institutionen.tsv",encoding = "UTF-8")
country<-read.delim("data/Bundesländer-Atlas - Länder.tsv",encoding = "UTF-8")
#general template for plotting
Template <- theme(axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.y = element_text(size = 12),
                  panel.background = element_blank())
#data cleaning and processing 
institution[institution$X==" Sachsen",1]<-"Sachsen"
institution<-institution%>%
  filter(Einrichtungsart!="Ressortforschung")
institution$institutionType<-ifelse(grepl("schung",institution$Einrichtungsart),"ForschungsInstitution",ifelse(grepl("Uni",institution$Einrichtungsart),"Universität","Hochschule"))

institution<-institution%>%
  mutate(bool_OA_Website=ifelse(is.na(OA.Webseite.der.Institution)|OA.Webseite.der.Institution=="Recherche fehlt","FALSE","TRUE"))%>%
  mutate(bool_OA_Beauftragte=ifelse(is.na(OA.Beauftragte.r)|OA.Beauftragte.r=="Recherche fehlt","FALSE","TRUE"))%>%
  mutate(bool_OA.Policy=ifelse(is.na(OA.Policy)|OA.Policy=="nein"|OA.Policy=="Recherche fehlt","FALSE","TRUE"))%>%
  mutate(bool_OA.Leitlinie=ifelse(is.na(OA.Leitlinie)|OA.Leitlinie=="Recherche fehlt","FALSE","TRUE"))%>%
  mutate(bool_OA.PL=ifelse(bool_OA.Leitlinie=="TRUE"|bool_OA.Policy=="TRUE","TRUE","FALSE"))%>%
  mutate(bool_Repositorium.URL=ifelse(is.na(Repositorium.URL)|Repositorium.URL=="Recherche fehlt","FALSE","TRUE"))%>%
  mutate(bool_Berliner.Erklärung=ifelse(is.na(Berliner.Erklärung)|Berliner.Erklärung=="nein","FALSE","TRUE"))%>%
  mutate(bool_OA2020=ifelse(is.na(OA2020)|OA2020=="nein","FALSE","TRUE"))
result<-data.frame(matrix(ncol=1,nrow=0))
#we calculate score of openess level by our 6 indicator here, and sum it up 100% as the we defined as fully openness
for(i in c(1:nrow(institution))){
  institution_score<-as.numeric(0)
  institution_score<-ifelse(institution$bool_OA_Website[i]=="TRUE",institution_score+1,institution_score+0)
  institution_score<-ifelse(institution$bool_OA_Beauftragte[i]=="TRUE",institution_score+1,institution_score+0)
  institution_score<-ifelse(institution$bool_OA.PL[i]=="TRUE",institution_score+1,institution_score+0)
  institution_score<-ifelse(institution$bool_Repositorium.URL[i]=="TRUE",institution_score+1,institution_score+0)
  institution_score<-ifelse(institution$bool_Berliner.Erklärung[i]=="TRUE",institution_score+1,institution_score+0)
  institution_score<-ifelse(institution$bool_OA2020[i]=="TRUE",institution_score+1,institution_score+0)
  result<-rbind(result,institution_score)
}

institution<-cbind(institution,result)
#transform formate into %
institution<-institution%>%
  mutate(OpenScore_institution=round(institution$X1/as.numeric(6)*100,2))
  
institution$score<-paste0(round(institution$X1/as.numeric(6)*100,2),"%")
#calculate the other level of openness
institution<-institution%>%group_by(X)%>%
  mutate(bundesländer_score=round(mean(OpenScore_institution),2))

institution<-institution%>%
  mutate(country_score=round(mean(institution$OpenScore_institution),2))



#plot the score by the bundesländer 
institution%>%
  group_by(X)%>%
  summarise(countryScore=mean(OpenScore_institution))%>%
  ggplot(aes(x=reorder(X,countryScore),y=countryScore))+
  geom_col(alpha = 0.8, width = 0.8)+
  geom_text(aes(x=reorder(X,countryScore),label=round(countryScore,2)))+
  Template+
  coord_flip()
ggsave("plots/score_bundesländer.png",dpi=500,width = 15,height=5)




  

##plot the score by the institution level
institution%>%
  group_by(Name.der.Institution,institutionType)%>%
  summarise(countryScore=mean(OpenScore_institution))%>%
  ggplot(aes(x=reorder(Name.der.Institution,countryScore),y=countryScore,fill=institutionType))+
  geom_col(alpha = 0.8, width = 0.8)+
  Template+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  coord_flip()
ggsave("plots/score_institution_by_institutionType.png",dpi=500,width = 15,height=5)

##plot the score by country
institution%>%
  group_by(institutionType)%>%
  summarise(countryScore=mean(OpenScore_institution))%>%
  ggplot(aes(x=institutionType,y=countryScore,fill=institutionType))+
  geom_col(alpha = 0.8, width = 0.8)+
  geom_text(aes(x=institutionType,y=countryScore,label=round(countryScore,2)))+
  Template+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  coord_flip()

ggsave("plots/score_country_byinstitutionType.png",dpi=500,width = 15,height=5)


institution$Einrichtungsart<-factor(institution$Einrichtungsart,unique(institution$Einrichtungsart))

institution%>%
  group_by(X,institutionType)%>%
  dplyr::summarise(Freq=n())%>%
  ggplot(aes(x=reorder(X,Freq),y=Freq,fill=institutionType))+
  geom_col(alpha = 0.8, width = 0.8)+
  Template+
  coord_flip()

ggsave("plots/Bundesländer_institutionType.png",dpi=500,width = 15,height=5)
###core by the type
institution%>%
  group_by(X,Einrichtungsart)%>%
  dplyr::summarise(Freq=n())%>%
  ggplot(aes(x=reorder(X,Freq),y=Freq,fill=Einrichtungsart))+
  geom_col(alpha = 0.8, width = 0.8)+
  Template+
  coord_flip()

ggsave("plots/c_institution_country_institutionType.png",dpi=500,width = 15,height=5)

colnames(institution)
#save clean data for other usage
clean<-institution[,c( "Wikidata.ID","X","Name.der.Institution", "Ort",
             "Einrichtungsart", "OA.Webseite.der.Institution" ,
             "OA.Beauftragte.r","OA.Policy",
             "OA.Leitlinie","Berliner.Erklärung", "OA2020" , 
             "bool_OA_Website"                             
                       ,"bool_OA_Beauftragte"                         
                       ,"bool_OA.Policy"                              
                       ,"bool_OA.Leitlinie"  
                       ,"bool_OA.PL"  
                       ,"bool_Repositorium.URL"                       
                       ,"bool_Berliner.Erklärung"                     
                       ,"bool_OA2020"                                 
                       ,"X1"                                          
                       ,"score"                                       
                       ,"OpenScore_institution",
                        "bundesländer_score",
                        "country_score"
                       ,"institutionType")]
write.csv(clean,"data/cleanScore.csv",row.names = FALSE)
test<-read.csv("data/cleanScore.csv")

test<-test%>%
  mutate(Score_web_de=nrow(test%>%filter(bool_OA_Website=="TRUE"))/nrow(test))%>%
  mutate(Score_Beauftragte_de=nrow(test%>%filter(bool_OA_Beauftragte=="TRUE"))/nrow(test))%>%
  mutate(Score_PL_de=nrow(test%>%filter(bool_OA.PL=="TRUE"))/nrow(test))%>%
  mutate(Score_Repositorium_de=nrow(test%>%filter(bool_Repositorium.URL=="TRUE"))/nrow(test))%>%
  mutate(Score_OA2020_de=nrow(test%>%filter(bool_OA2020=="TRUE"))/nrow(test))%>%
  mutate(Score_Berliner.Erklärung_de=nrow(test%>%filter(bool_Berliner.Erklärung=="TRUE"))/nrow(test))
  
p<-test[1,c(26,27,28,29,30,31)]
row.names(p)

  
  
test<-test%>%
  group_by(X)%>%
  mutate(Score_web_Bunde=nrow(test%>%filter(bool_OA_Website=="TRUE"))/nrow(test))
  
  
  
  
  mutate(Score_Beauftragte_Bunde=nrow(test%>%filter(bool_OA_Beauftragte=="TRUE"))/nrow(test))%>%
  mutate(Score_PL_Bunde=nrow(test%>%filter(bool_OA.PL=="TRUE"))/nrow(test))%>%
  mutate(Score_Repositorium_Bunde=nrow(test%>%filter(bool_Repositorium.URL=="TRUE"))/nrow(test))%>%
  mutate(Score_OA2020_Bunde=nrow(test%>%filter(bool_OA2020=="TRUE"))/nrow(test))%>%
  mutate(Score_Berliner.Erklärung_Bunde=nrow(test%>%filter(bool_Berliner.Erklärung=="TRUE"))/nrow(test))

  