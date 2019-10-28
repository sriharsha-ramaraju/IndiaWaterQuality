wq<-read.csv("IndiaAffectedWaterQualityAreas.csv",header=T,
             stringsAsFactors = FALSE)
dim(wq)
print("old variable names:")
names(wq)
names(wq)<-c("state","district","block","panchayat","village","habitation",
             "quality","year")
print("new variable names:")
names(wq)

#displaying the top 6 obs
head(wq)

library(dplyr)
library(ggplot2)

wq$year<-format(as.Date(wq$year, format="%d/%m/%Y"),"%Y")#taking only year
head(wq$year)
str(wq)

wq$state<-as.factor(wq$state)
wq$district<-as.factor(wq$district)
wq$block<-as.factor(wq$block)
wq$panchayat<-as.factor(wq$panchayat)
wq$village<-as.factor(wq$village)
wq$habitation<-as.factor(wq$habitation)
wq$quality<-as.factor(wq$quality)

wq$state<-ifelse(wq$state=="CHHATTISGARH","CHATTISGARH",
                       as.character(wq$state))


ggplot(wq,aes(x=quality))+geom_bar(fill="dodgerblue")+
  
  theme(axis.text.x=element_text(face="bold",size=18),
        axis.text.y = element_text(face="bold",size=18))+
  labs(y="Number Of Regions-Count",x="Chemicals")
  



ggplot(filter(wq,quality=="Fluoride"),aes(x=year,y=quality,fill=factor(state)))+
  geom_bar(stat="identity")+
  theme(legend.position="bottom")+
  theme(axis.text.x=element_text(face="bold",size=18),
        axis.text.y = element_text(face="bold",size=18))+
  labs(y="Number Of Regions-Count",x="Chemicals")

wq_ap<-wq %>% filter(state=="ANDHRA PRADESH")

ggplot(wq_ap,aes(x=quality,fill=year))+
  geom_bar(position = "dodge")+
  theme(axis.text.x=element_text(face="bold",size=18),
        axis.text.y = element_text(face="bold",size=18))+
  labs(y="Number Of Regions-Count",x="Chemicals")+
  ggtitle("Andhra Pradesh")


wq_tn<-wq %>% filter(state=="TAMIL NADU")

ggplot(wq_tn,aes(x=quality,fill=year))+
  geom_bar(position = "dodge")+
  theme(axis.text.x=element_text(face="bold",size=18),
        axis.text.y = element_text(face="bold",size=18))+
  labs(y="Number Of Regions-Count",x="Chemicals")+
  ggtitle("Tamil Nadu")

#now let's look at the statewise water quality 
#proportion

wq_states<-data.frame(table(wq$state,
                           wq$quality,
                           wq$year))


wq_states<-wq_states %>% arrange(desc(Freq))

p <- ggplot(data = wq_states, 
            aes(x = Var1, y = Freq, 
                group = Var2, fill = Var2))
p <- p + geom_bar(stat = "identity", 
                  width = 0.5, position = "dodge")
p <- p + theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 90))
p<- p+ xlab("States")
p <-p+ylab("Count")
p <- p+ggtitle("Water Quality in Indian-States")+
  scale_fill_brewer(palette="Set1")
p<-p+ theme(axis.text.x=element_text(face="bold",size=18),
            axis.text.y = element_text(face="bold",size=18))+
  labs(y="Number Of Regions-Count")
  
p

wq_states%>% ggplot(aes(x = Var3, y = Freq,fill=Var2))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=0.5))+
  facet_wrap(~ Var1)+
  labs(title="Water Quality in Indian-States",
       x="Year",y="Freq")+theme(axis.text.x=element_text(face="bold",size=18),
                                axis.text.y = element_text(face="bold",size=18))+
  labs(y="Number Of Regions-Count")
  

#lets take one of the top affected state and 
#let's look at the district wise water quality

wq_states_ex<-data.frame(table(wq$state,wq$quality,
                               wq$year,wq$district))


wq_states_exState<-wq_states_ex %>% filter(Var2=="Iron" & Var1=="BIHAR" & 
                                              Freq>50)%>%arrange(desc(Freq))

ggplot(wq_states_exState,aes(x=Var4,y=Freq,fill=Var3))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  labs(title="Bihar-Districts-Iron")+
  theme(axis.text.x=element_text(face="bold",size=18),
  axis.text.y = element_text(face="bold",size=18))+
  labs(y="Number Of Regions-Count")
  

rankq<-wq_states %>% 
  group_by(Var2) %>% 
  mutate(rank = rank(-Freq, ties.method = "first"))
head(rankq)

rankq %>% group_by(Var2)%>% filter(rank <15)%>% 
  ggplot( aes(x = Var1,y = rank,color=Var3)) + 
  geom_point(size=3,alpha=0.7) +
  facet_wrap(~Var2)+
  theme(axis.text.x=element_text(angle=90,hjust=0.5))+
  scale_color_manual(values=c("red","purple","blue","green"))+
  labs(title="Top 15 water Degradation states",
       x="State",y="Rank")+theme(axis.text.x=element_text(face="bold",size=16),
                                 axis.text.y = element_text(face="bold",size=18))



