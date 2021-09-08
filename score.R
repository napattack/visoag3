library(tidyverse)
library(dplyr)

#general exploration

institution<-read.delim("data/Bundesländer-Atlas - Institutionen.tsv",encoding = "UTF-8")
country<-read.delim("data/Bundesländer-Atlas - Länder.tsv",encoding = "UTF-8")
Template <- theme(axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.y = element_text(size = 12),
                  panel.background = element_blank())
institution[institution$X==" Sachsen",1]<-"Sachsen"
institution<-institution%>%
  mutate(bool_OA_Website=ifelse(is.na(OA.Webseite.der.Institution)|OA.Webseite.der.Institution=="Recherche fehlt","FALSE","TRUE"))%>%
  mutate(bool_OA_Beauftragte=ifelse(is.na(OA.Beauftragte.r)|OA.Beauftragte.r=="Recherche fehlt","FALSE","TRUE"))%>%
  mutate(bool_OA.Policy=ifelse(is.na(OA.Policy)|OA.Policy=="nein"|OA.Policy=="Recherche fehlt","FALSE","TRUE"))%>%
  mutate(bool_OA.Leitlinie=ifelse(is.na(OA.Leitlinie)|OA.Leitlinie=="Recherche fehlt","FALSE","TRUE"))%>%
  mutate(bool_Repositorium.URL=ifelse(is.na(Repositorium.URL)|Repositorium.URL=="Recherche fehlt","FALSE","TRUE"))%>%
  mutate(bool_Berliner.Erklärung=ifelse(is.na(Berliner.Erklärung)|Berliner.Erklärung=="nein","FALSE","TRUE"))%>%
  mutate(bool_OA2020=ifelse(is.na(OA2020)|OA2020=="nein","FALSE","TRUE"))
result<-data.frame(matrix(ncol=1,nrow=0))

for(i in c(1:nrow(institution))){
  institution_score<-as.numeric(0)
  institution_score<-ifelse(institution$bool_OA_Website[i]=="TRUE",institution_score+1,institution_score+0)
  institution_score<-ifelse(institution$bool_OA_Beauftragte[i]=="TRUE",institution_score+1,institution_score+0)
  institution_score<-ifelse(institution$bool_OA.Policy[i]=="TRUE",institution_score+1,institution_score+0)
  institution_score<-ifelse(institution$bool_OA.Leitlinie[i]=="TRUE",institution_score+1,institution_score+0)
  institution_score<-ifelse(institution$bool_Repositorium.URL[i]=="TRUE",institution_score+1,institution_score+0)
  institution_score<-ifelse(institution$bool_Berliner.Erklärung[i]=="TRUE",institution_score+1,institution_score+0)
  institution_score<-ifelse(institution$bool_OA2020[i]=="TRUE",institution_score+1,institution_score+0)
  result<-rbind(result,institution_score)
}

institution<-cbind(institution,result)
institution<-institution%>%
  mutate(OpenScore_institution=institution$X1/as.numeric(7)*100)
  
institution$score<-paste0(round(institution$X1/as.numeric(7)*100,2),"%")


#plot the score by the bundesländer 
institution%>%
  group_by(X)%>%
  summarise(countryScore=mean(OpenScore))%>%
  ggplot(aes(x=reorder(X,countryScore),y=countryScore))+
  geom_col(alpha = 0.8, width = 0.8)+
  Template+
  coord_flip()
ggsave("plots/score_country.png",dpi=500,width = 15,height=5)
##plot the score by the institution level
institution%>%
  group_by(Name.der.Institution)%>%
  summarise(countryScore=mean(OpenScore))%>%
  ggplot(aes(x=reorder(Name.der.Institution,countryScore),y=countryScore))+
  geom_col(alpha = 0.8, width = 0.8)+
  Template+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  coord_flip()
ggsave("plots/score_institution.png",dpi=500,width = 15,height=5)

##plot the score by country
institution%>%
  summarise(countryScore=mean(OpenScore))%>%
  ggplot(aes(x=0,y=countryScore))+
  geom_col(alpha = 0.8, width = 0.8)+
  geom_text(aes(x=0,y=countryScore,label=round(countryScore*100,2)))+
  Template+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  coord_flip()

ggsave("plots/score_country.png",dpi=500,width = 15,height=5)


institution$Einrichtungsart<-factor(institution$Einrichtungsart,unique(institution$Einrichtungsart))

institution%>%
  group_by(X,Einrichtungsart)%>%
  dplyr::summarise(Freq=n())%>%
  ggplot(aes(x=reorder(X,Freq),y=Freq,fill=Einrichtungsart))+
  geom_col(alpha = 0.8, width = 0.8)+
  Template+
  coord_flip()

ggsave("plots/c_institution_country_institutionType.png",dpi=500,width = 15,height=5)





for (i in 1:length(index)){
institution$index[i]<-factor(institution$index[i],unique(institution$index[i]))
  institution%>%
    group_by(X,index[i]))%>%
    dplyr::summarise(Freq=n())%>%
    ggplot(aes(x=reorder(X,Freq),y=Freq,fill=index[i])))+
    geom_col(alpha = 0.8, width = 0.8)+
    Template+
    coord_flip()
  name<-paste0("plots/c_institution_country_",institution$index[i],".png")
ggsave(name,dpi=500,width = 15,height=5)
}
  