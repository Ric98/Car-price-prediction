rm(list=ls())


setwd("C:/Users/Riccardo/Desktop/Thesis")

library(dplyr)
library(ggplot2)
library(corrplot)
library(readxl)
library(DataExplorer)
library(tidyverse)
library(openxlsx)

data_job<-read.xlsx("aug_train.xlsx")

View(data_job)

##1° step: rimozione colonna 1 e 2 perchè non utili al fine dell'analisi
data_job<-data_job[,-(1:2)]

##qualità dei dati: vediamo se ci sono NA

data_job<-na.omit(data_job)
View(data_job)
str(data_job)
##VEDIAMO SE CI SONO DUPLICATI


library(data.table)
library(moments)
duplicated(data_job)

data_set=unique(data_job)
View(data_set)
str(data_set)


mean(data_set$target)


##CERCHIAMO GLI OUTLIERS

boxplot(data_set$training_hours)


boxplot(data_set$training_hours)
hist(data_set$training_hours, main = "Training Hours")




##☼CONTIAMO LE OSSERVAZIONI ALL'INTERNO DI OGNI CATEGORIA
str(data_set)
count(data_set, vars=company_type)
count(data_set, vars=gender)### eliminare other 
count(data_set, vars=relevent_experience)
prop.table(table(data_set$target, data_set$relevent_experience))
count(data_set, vars=enrolled_university)
count(data_set, vars=education_level)
count(data_set, vars=major_discipline)
count(data_set, vars=experience)  ##da trasformare e raggruppare in gruppi 
count(data_set, vars=company_size) ## da trasformare e raggruppare in gruppi
count(data_set, vars=last_new_job) ## da trasformare in fattori da 1 a 5 
count(data_set, vars=target) ## da trasformare e raggruppare in gruppi



###prima di trasformare ritrosformo in character altrimenti mi da NA
##PULIAMO E SISTEMIAMO LE VARIABILI (trasformazioni, raggruppamenti ecc..)



data_set$target<- as.factor(data_set$target)
str(data_set)
#GENDER

data_set= data_set[data_set$gender!= "Other",] ##ABBIAMO CANCELLATO SU  CATEGORIA GENEDER LA CLASSE OTHER
count(data_set, vars=target)
data_set$gender[data_set$gender=="Male"]<-0
data_set$gender[data_set$gender=="Female"]<- 1

data_set$gender<-as.factor(data_set$gender)



##PIE CHART for gender
dev.new()
tab=data_set$gender%>%table()
percentages=tab%>%prop.table()%>%round(3)*100
txt=paste0(percentages,'%')
percentages

pie<- data.frame(
  Gender=c("Male ", "Female"), value=c(8038, 803))

pie <- pie %>% 
  arrange(desc(Gender)) %>%
  mutate(prop = value / sum(pie$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(pie, aes(x="", y=prop, fill=Gender)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  
  geom_text(aes(y = ypos, label = txt), color = "white", size=6) +
  scale_fill_brewer(palette="Dark2") 


#EXPERIENCE
### raggruppiamo ogni 5 gli anni di esperienza: 1=0-5; 2=6-10; 3=11-15; 4=16-20; 5= >20
count(data_set, vars=experience)

data_set$experience[data_set$experience=="<1"]<-0
data_set$experience[data_set$experience==">20"]<-21

data_set$experience<-as.integer(data_set$experience)

dev.new()


str(data_set$experience)

ggplot(data=data_set, aes(experience)) + 
  geom_bar(aes(fill=..count..))


##VEDIAMO COME LA MAGGIOR PARTE DEGLI INDIVIDUI HA 21 O PIù ANNI DI ESPERIENZA 
### Un motivo anche per il quale gli individui sono maggiormente 0 (no change) è dato dal fatto
#### che la maggior parte ha più di 21 anni di esperienza lavorativa


##TRASFORMIAMO COMPANY SIZE IN 6 LIVELLI: 1= 100; 2=500 3=1000 4=5000 5=10000 6= 10000+
count(data_set, vars=company_size)
data_set$company_size[data_set$company_size == "<10" | data_set$company_size == "50-99"] =1
data_set$company_size[data_set$company_size == "100-500"]  =2
data_set$company_size[data_set$company_size == "500-999" ] =3
data_set$company_size[data_set$company_size == "1000-4999" ] =4
data_set$company_size[data_set$company_size == "5000-9999" ] =5
data_set$company_size[data_set$company_size == "10000+" | data_set$company_size == "18172"] =6

a=count(data_set, vars=company_size)
data_set$company_size<-factor(data_set$company_size, ordered=F )
str(data_set$company_size)

dev.new()

pie10<- data.frame(
  Company_size=c("1-100",  "100-500", "500-999","1000-4999","5000-9999", "10000+"), value=c(a$n))

pie10<- pie10 %>% 
  arrange(desc(Company_size)) %>%
  mutate(prop = value / sum(pie10$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(pie10, aes(x="", y=prop, fill=Company_size)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ypos, label = paste0(round(value/sum(value)*100), "%")), color = "white", size=4) +
  scale_fill_brewer(palette="Dark2")



##TRASFORMIAMO last_new_job: 0 ; 1-3 ; 4>4
count(data_set, vars=last_new_job)

data_set$last_new_job[data_set$last_new_job== "never"] =0
data_set$last_new_job[data_set$last_new_job == "1"| data_set$last_new_job == "2"| data_set$last_new_job == "3"] =1
data_set$last_new_job[data_set$last_new_job == "4"| data_set$last_new_job == ">4"]= 2

data_set$last_new_job<-factor(data_set$last_new_job, ordered=T)

str(data_set$last_new_job)
b=count(data_set, vars=last_new_job)
dev.new()
tab5=data_set$last_new_job%>%table()

percentages5=tab5%>%prop.table()%>%round(3)*100

percentages5
txt5=paste0(percentages5,'%')

pie5<- data.frame(
  Last_new_job=c("never", "1-3", "4->4"), value=c(b$n))

pie5<- pie5 %>% 
  arrange(desc(Last_new_job)) %>%
  mutate(prop = value / sum(pie5$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(pie5, aes(x="", y=prop, fill=Last_new_job)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ypos, label =paste0(round(value/sum(value)*100), "%") ), color = "white", size=6) +
  scale_fill_brewer(palette="Dark2")

## laste new job = Difference in years between previous job and current job
 ## la maggior parte degli individui lavorano nell'attuale posizione lavorativa da circa 1-3 anni



##TRASFORMIAMO COMPANY_TYPE: 0=startup; 1= public sector; 2=Pvt Ltd; 3=other
c=count(data_set, vars=company_type)

data_set$company_type[data_set$company_type== "Early Stage Startup" | data_set$company_type=="Funded Startup" ] =0
data_set$company_type[data_set$company_type== "Public Sector"]=1

data_set$company_type[data_set$company_type== "Pvt Ltd"]=2
data_set$company_type[data_set$company_type== "Other" | data_set$company_type=="NGO" ] =3

pie3<- data.frame(
  Company_type=c("Startup", "Public Sector", "Private Company", "Other" ),
  value=c(c$n))

pie3<- pie3 %>% 
  arrange(desc(Company_type)) %>%
  mutate(prop = value / sum(pie3$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(pie3, aes(x="", y=prop, fill=Company_type)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ypos, label = paste0(round(value/sum(value)*100), "%")), color = "white", size=6) +
  scale_fill_brewer(palette="Dark2")


data_set$company_type<-as.factor(data_set$company_type)
##maggiori individui in settore privato




##TRASFORMO RELEVENT EXPERIENCE
d=count(data_set, vars=relevent_experience)

data_set$relevent_experience[data_set$relevent_experience== "Has relevent experience"]= 1
data_set$relevent_experience[data_set$relevent_experience== "No relevent experience"]= 0

data_set$relevent_experience<-as.factor(data_set$relevent_experience)

pie4<- data.frame(
  Relevant_experience=c("no relevant experience", "relevant experience"), value=c(d$n))

pie4<- pie4 %>% 
  arrange(desc(Relevant_experience)) %>%
  mutate(prop = value / sum(pie4$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(pie4, aes(x="", y=prop, fill=Relevant_experience)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ypos, label = paste0(round(value/sum(value)*100), "%")), color = "white", size=6) +
  scale_fill_brewer(palette="Dark2")


##89% hanno maggiore esperienza lavorative

##TRASFORMO ENROLLED UNIVERSITY
e=count(data_set, vars=enrolled_university)
e
data_set$enrolled_university[data_set$enrolled_university == "no_enrollment"] =0
data_set$enrolled_university[data_set$enrolled_university == "Part time course" |  data_set$enrolled_university =="Full time course"] =1

data_set$enrolled_university<-as.factor(data_set$enrolled_university)

pie15<- data.frame(
  Enrolled_univ=c("No enrollment", "Course" ),
  value=c(e$n))

pie15<- pie15 %>% 
  arrange(desc( Enrolled_univ)) %>%
  mutate(prop = value / sum(pie15$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(pie15, aes(x="", y=prop, fill= Enrolled_univ)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ypos, label = paste0(round(value/sum(value)*100), "%")), color = "white", size=6) +
  scale_fill_brewer(palette="Dark2")


### la maggior parte degli individui non fa nessun corso universitario mentre lavora


#TRASFORMIAMO EDUCATION_LEVEL
count(data_set, vars=education_level)
data_set$education_level[data_set$education_level== "Graduate"]=0
data_set$education_level[data_set$education_level== "Masters"]=1
data_set$education_level[data_set$education_level== "Phd"]=2

f=count(data_set, vars=education_level)

data_set$education_level<-as.factor(data_set$education_level)
dev.new()
pie14<- data.frame(
  Education_level=c("Graduate", "Masters","Phd"), value=c(f$n))

pie14<- pie14 %>% 
  arrange(desc(Education_level)) %>%
  mutate(prop = value / sum(pie14$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(pie14, aes(x="", y=prop, fill= Education_level)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ypos, label = paste0(round(value/sum(value)*100), "%")), color = "white", size=6) +
  scale_fill_brewer(palette="Dark2")


#TRASFORMIAMO MAJOR_DISCIPLINE
g=count(data_set, vars=major_discipline)
data_set$major_discipline[data_set$major_discipline== "Arts" | data_set$major_discipline== "Humanities"]=0
data_set$major_discipline[data_set$major_discipline== "Business Degree" | data_set$major_discipline== "STEM"]=1
data_set$major_discipline[data_set$major_discipline== "Other"] = 2
data_set$major_discipline[data_set$major_discipline== "No Major"] = 3

count(data_set, vars=major_discipline)

data_set$major_discipline<-as.factor(data_set$major_discipline)

#pie chart major discipline
dev.new()
pie30<- data.frame(
  Major_discipline=c("Arts&Humanities", "Business&STEM","Other","No Major"), value=c(g$n))

pie30<- pie30 %>% 
  arrange(desc(Major_discipline)) %>%
  mutate(prop = value / sum(pie30$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(pie30, aes(x="", y=prop, fill= Major_discipline)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ypos, label = paste0(round(value/sum(value)*100), "%")), color = "white", size=5) +
  scale_fill_brewer(palette="Dark2")



##PIE CHART FOR TARGET
## tasformo prima la variabile in factor: 
# •	target: 0 – Not looking for job change, 1 – Looking for a job change


h=count(data_set, vars= target)
dev.new()


pie25<- data.frame(
  Target=c("Not looking for job change", "Looking for a job change" ), value=c(h$n))

pie25<- pie25 %>% 
  arrange(desc(Target)) %>%
  mutate(prop = value / sum(pie25$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(pie25, aes(x="", y=prop, fill=Target)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="right") +
  geom_text(aes(y = ypos, label =paste0(round(value/sum(value)*100), "%")), color = "black", size=6) +
  scale_fill_brewer(palette="Blues")



###ISTOGRAMMI PER VARIABILI CONTINUE 

dev.new()
 summary(data_set$city_development_index)
library(stats)
str(data_set)

#TRAINING HOURS

library(graphics)

dev.new()
ggplot(data=data_set, aes(training_hours)) + 
  geom_bar(aes(fill=..count..))  +
  labs(title="Histogram", 
       subtitle="Training Hours",
       caption="Source: mpg",
       x="Training Hours",
       fill="Hours") 

 
##oppure altro istogramma


library(ggplot2)
theme_set(theme_classic())

# Plot density
g <- ggplot(data_set, aes(training_hours))
g + geom_density(aes(fill=training_hours), alpha=0.8, colour="red") + 
  labs(title="Density plot", 
       subtitle="Training Hours",
       caption="Source: mpg",
       x="Training Hours",
       fill="red")




###ISTOGRAMMA PER CITY DEV IND
ggplot(data=data_set, aes(city_development_index)) + 
  geom_histogram(aes(fill=..count..), binwidth = 0.02)



##CORRELATION
dev.new()

##faccio le correlazioni solo tra variabili numeriche
data_plot<-data_set[,c(1,7,11)]
plot_correlation(data_plot)

cor(data_set$city_development_index, data_set$experience, method = c("pearson"))
cor.test(data_set$city_development_index, data_set$experience, method=c("pearson"))
##non c'è una forte correlazione tra le variabili (accettabile) 


library(scatterplot3d)
scatterplot3d(data_set$city_development_index, data_set$experience)
##anche dallo scatterplot si può vedere come le variabili non siano correlate.
scatterplot3d(data_set$city_development_index, data_set$training_hours, col.grid="red")



###vediamo altri grafici

##GRAFICO TRA TARGET E TRAINING HOURS

dev.new()


ggplot(data_set, aes(training_hours, fill=target)) +
  geom_histogram(binwidth = 5, alpha = 0.5, 
                 position = "dodge")
summary(data_set$training_hours)

## dal grafico vediamo come la maggior parte delle osservazioni sono target=0 

##andiamo a vedere se c'è correlazione tra training e target 
## questo grafico mi fa vedere come in realtà le ore di training non siano cosi importanti per la decisione o meno di cambiare lavoro

library(ggpubr)
ggboxplot(data_set, x = "target", y = "training_hours", 
          color = "target", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Train Hour", xlab = "Target")


## NONOSTANTE il dataset sia sbilanciato (0>1) notiamo che per quanto riguarda 
## training hours seguono la stessa distribuzione con una mediana (q2) intorno le 60 ore
## non dovrebbe quindi essere una variabile discriminante


##andiamo a testare con ANOVA AD UNA VIA:H0: there is no difference
##H1: there is difference; C'è DIFFERENZA tra le ore di training?

t.test(training_hours~ target, data=data_set)


#p-value>0.05 Non posso rifiutare H0 e dico che la differenza tra le medie = 0
##cioè le ore di training non influenzano la scelta MA questo può essere  dovuto al campionamento sbilanciato? Forse no



##FACCIAMO LA STESSA COSA ANCHE PER INDICE DI SVILUPPO CITTà E TARGET
dev.new()
ggplot(data_set, aes(city_development_index, fill=target)) +
  geom_histogram(binwidth = 0.02)

ggboxplot(data_set, x = "target", y = "city_development_index", 
          color = "target", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "CDI", xlab = "Target")


### Dal grafico possiamo notare come la maggior parte degli individui che non vogliono cambiare lavoro
## appartengono ad una città con un indice di sviluppo molto elevato; questo potrebbe dar fastidio
## nel momento in cui si fa previsione in quanto gli individui provengono da città con un alto CDI


##testiamolo:
t.test(city_development_index~target, data=data_set)
##posso rifiutare l'ipotesi nulla e stabilire che la differenza tra le medie è statisticamente significativa, cioè diversa da 0

##box plot target e experience 

ggboxplot(data_set, x = "target", y = "experience", 
          color = "target", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Experience", xlab = "Target")

## vediamo come gli individui che scelgono di non cambiare lavoro hanno maggiori anni di esperienza
## rispetto a chi invece vorrebbe cambiare lavoro (a causa forse della maggiore età)



##GRAFICI TRA VARIABILI CATEGORICHE

## gender vs target

df <- data.frame(Target=data_set$target, Gender=data_set$gender)


ggplot(df, aes(Gender, ..count..)) + geom_bar(aes(fill = Target), position = "dodge")
##poco rilevante perchè il nostro dataset è composto principalmente da maschi


##target vs major discipline



dev.new()

df2 <- data.frame(Target=data_set$target, Discipline=data_set$major_discipline)
ggplot(df2, aes(Target, ..count..)) + geom_bar(aes(fill =  Discipline), position = "dodge") +scale_fill_brewer(palette="Dark2") 




### vediamo come la provenienza da ambito scientifico e/o economico incida fortemente su entrambe le scelte
## infatti il problema sta nel fatto (come visto dal pie chart) che il 90% del campione proviene
## da business o STEM
## variabile non discriminante


##target vs relevent experience

df3 <- data.frame(Target=data_set$target, Relevent_Exp=data_set$relevent_experience)
ggplot(df3, aes(Target, ..count..)) + geom_bar(aes(fill =  Relevent_Exp), position = "dodge") +scale_fill_brewer(palette="Dark2")


##chi ha esperienza lavorativa sembra cercare piu lavoro rispetto all'altra situazione 
## (nonostante campionamento sbilanciato rimane il fatto che cambia di più chi ha esperienza rilevante
## variabile probabilmente non discriminante


##target vs last new job

df4 <- data.frame(Target=data_set$target, last_new_job=data_set$last_new_job)
ggplot(df4, aes(Target, ..count..)) + geom_bar(aes(fill =  last_new_job), position = "dodge") +scale_fill_brewer(palette="Dark2")

## largamente predominante in entrambe le scelte la classe 1 (1-3 anni)
### variabile non discriminante : troppe 



##target vs enrolled university
df5 <- data.frame(Target=data_set$target, Enr_Univ=data_set$enrolled_university)
ggplot(df5, aes(Enr_Univ, ..count..)) + geom_bar(aes(fill =  Target), position = "dodge") +scale_fill_brewer(palette="Dark2")

###continuiamo a vedere lo sbilanciamento verso in questo caso lo 0 (no enrollment)


##target vs company size
df6 <- data.frame(Target=data_set$target, company_size=data_set$company_size)
ggplot(df6, aes(Target, ..count..)) + geom_bar(aes(fill =  company_size), position = "dodge") +scale_fill_brewer(palette="Dark2")


## possiamo notare come la dimensione della azienda si distribuisce piu o meno ugualmente
## eccetto una leggera differenza tra classe 6 (10000+) e classe 1(1-100) nel cambiare lavoro
## ciò può indicare una propensione al cambiare lavoro se si è in un azienda o piccola o molto grande

# la differenza non è evidente: potrebbe sembrare una variabile leggermente discriminante ma
## la differenza non è abbastanza significativa da poterla considerare discriminante


##target vs company type
df7 <- data.frame(Target=data_set$target, company_type=data_set$company_type)
ggplot(df7, aes(Target, ..count..)) + geom_bar(aes(fill =  company_type), position = "dodge") +scale_fill_brewer(palette="Dark2")

##come sopra vediamo che la distribuzione è troppo simile


##target vs education level


df8<-  data.frame(Target=data_set$target, education_level=data_set$education_level)
ggplot(df8, aes(education_level, ..count..)) + geom_bar(aes(fill =Target), position = "dodge") +scale_fill_brewer(palette="Dark2")
str(data_set)

##la maggior parte delle osservazione è graduate ma in ogni titolo di studio la scelta dominante 
## è sempre quella di non cambiare lavoro


library(MASS)
###COSTRUISCO GLM



### per la costruzione del miglior modello abbiamo utilizzato la regressione stepwise.
### prima ancora avevamo testato manualmente ed i risultati sono gli stessi
library(pscl)

##DIAMO IL NOME AL NOSTRO MODELLO estratto dalla stepwise regression



library(caret)

## SPLITTIAMO IL DATA SET
set.seed(1985)

## create index matrix

index<- createDataPartition(data_set$target,p=.8, list = F, times=1)

##create train_df and test_df

train_df <- data_set[index,]
test_df<- data_set[-index,]



train_model <- glm(target ~., data = train_df, family = "binomial") %>%
  stepAIC(trace = T)

### ABBIAMO FATTO REGR.STEPWISE PER scegliere le variabili e seguendo criterio AIC
## abbiamo appurato che il nostro modello è il seguente:
## target~ education_level +  training_hours + relevent_experience + company_size  + experience + city_development_index
summary(train_model)

##sulla base della stepwise regression sul data set di train scegliamo come modello di riferimento
## il modello  ugueale a quello sopra con 5 variabili + intercetta;
## proviamo tuttavia a togliere anche la variabile major discipline in quanto sembra non essere
## statisticamente significativa



##specify logistic regression model 
###TESTIAMO MODELLO SUL TRAINING 

modello<-glm(target~ experience + city_development_index + education_level+ relevent_experience + company_size + training_hours,
                   data=train_df, 
                family="binomial")
library(car)

checkmulticol<- lm(city_development_index~., data = train_df[,-12] )

summary(checkmulticol)
vif(checkmulticol)

coef_table<-table(exp(modello$coefficients))
namesvector= c("intercept", "experience", "city_development_index","education level 1", "education level 2" , "relevent_experience1","company_size2", "company_size3", "company_size4", "company_size5", "company_size6", "training hours")
importance= cbind(coef_table, namesvector)
View(importance[,-1])
View(coef_table)
str(data_job)
### interessante notare come il modello con  tutte le variabili e il modello con solo le variaibli significative siano uguali


 ### notare come in realtà il livello di educazione non è una variabile statisticamente significativa
### ma la teniamo per evitare errore di seconda specie
### Inoltre la varaibile city development index è quella che spiega la maggior parte di variabilità 
## in quanto è un indice che contiene al suo interno ulteriori variabili

##VARIABLE IMPORTANCE

varimportance=varImp(modello) ## vediamo come experience e city development index SONO QUELLE PIù IMPORTANTI A DESCRIVERE IL FENOMENO.

varimportance$Overall
### Vediamo errore di training per vedere se c'è overfitting.

pred_train_log <-ifelse(predict(modello, train_df, type="response")>0.5,1,0)
confusionMatrix(as.factor(pred_train_log), train_df$target, positive = "1")

tab_train_log= prop.table(table(pred_train_log, train_df$target)); tab_train_log
err_log_train= tab_train_log[1,2] + tab_train_log[2,1]
err_log_train


pred <-ifelse(predict(modello, test_df, type="response")>0.5,1,0)


confusionMatrix(as.factor(pred), test_df$target, positive = "1")
tab= prop.table(table(pred, test_df$target)); tab

err.logit = tab[1,2] + tab[2,1]
err.logit ## 

## il modello ha un error rate del 14% in test; nonostante un'ottima accuracy dovuta allla maggior presenza di 0
## il modello ha problemin nel predirre gli 1; questo è dovuto dalla mancanza di variabili omesse e da
## un forte sbilanciamento dei dati non solo in termini di variabile risposta, ma anche nello sbilanciamento
## di tutti i predittori.

##DOPO aver valutato anche l'errore di train notiamo che non ci sono problemi di overfitting
## cioè il modello non si adatta troppo ai dati.



pred_0 <-ifelse(predict(modello, test_df, type="response")>0.16,1,0)

confusionMatrix(as.factor(pred_0), test_df$target, positive = "1")
tab1= prop.table(table(pred_0, test_df$target)); tab1

err.logit1 = tab1[1,2] + tab1[2,1]
err.logit1 ## 


#con modello sbilanciato guarda la precision = pos pred value
## il modello è abbastanza accurato : 85% di accuracy MA può essere migliorato in quanto il data set è molto sbilanciato
## KAPPa misura "the agreement" tra predictions e valori veri; una volre= 1 indica perfetto "agreement";
## nel nostra caso è uguale 0.3; quindi non è proprio il massimo

#Il "NO information rate" è la percentuale più ampia delle classi osservate 
#(c'erano più dati di classe 2 rispetto alla classe 1 in questo set di test). 
#Viene anche calcolato un test di ipotesi per valutare se il tasso di accuratezza complessivo è maggiore del tasso della classe più grande.
  


library(broom)
log_df_int= tidy(modello)
log_df_int%>%
  mutate(term=reorder(term,estimate)) %>%
  ggplot( aes(term,estimate, fill=estimate)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low="blue", high="lightblue") +
  geom_hline(yintercept=0) +
  coord_flip()



library(leaps)



###ANDIAMO A VEDERE LE ROC

## Logit model
library(ROSE)
library(ROCR)
library(pROC)

probFull=predict(modello, test_df, type="response")
predictFull=prediction(probFull, test_df$target)
perfFull=performance(predictFull, measure = "tpr", x.measure = "fpr")
plot(perfFull, col="red")
##
dev.new()
probFull0=ifelse(predict(modello, test_df, type="response")>0.16,1,0)
probfulls=probFull0
predictFull0=prediction(probFull0, test_df$target)
perfFull0=performance(predictFull0, measure = "tpr", x.measure = "fpr")
plot(perfFull0, col="blue", add=T)

AUC1=performance(predictFull, measure="auc")
AUC0=performance(predictFull0, measure="auc")

cbind(AUC1@y.values, AUC0@y.values)


## il modello con un cutoff di 0.5 tuttavia si dimostra il migliore in termini di rapporto di
## sensitivy e 1 - specificity 


##PRECISION RECALL CURVE

predictFullpc=prediction(probFull, test_df$target)
PC1=performance(predictFullpc, measure = "ppv", x.measure = "tpr")

plot(PC1, col="red")
AUCPC=performance(predictFullpc, measure="auc")
AUCPC@y.values

###vediamo come la precision recall curve sia non soddisfacente (modello non bravo a predirre i veri positivi)

###ALBERI
library(rpart)
library(caTools)
library(tidyverse)
library(rpart.plot)
library(dplyr)
library(DataExplorer)
library(randomForest)
library(rattle)

library(tree)




tree<-rpart(target~., data=train_df, control=rpart.control(cp=0.0001), parms = list(split="gini")) 
#control= rpart(cp=0.0001.è un controllo sul complexity parameter); 0.0001 è solo un controllo per dire che non deve essere minore di 0.0001 (possiamo ometterlo)
## CP è alpha perchè usiamo la tecnica del cross validation per la convalida della configurazione dell'albero
## nel cross validation suddividiamo il train in k campioni e testiamo sul k-1 campione





plotcp(tree) 
printcp(tree)



bestcp<-tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]

bestcp

dev.new()
tree.pruned<- prune(tree, cp=bestcp) ##albero decisionale ottimale
tree.pruned


dev.new()
rpart.plot(tree.pruned, extra = 4)
plot(tree.pruned)    #- the predicted probability of change(0.09 e 0.59),
                     ## - the percentage of observations in the node (85% e 15%)
text(tree.pruned, cex=0.6, use.n=T, xpd=T)

prp(tree.pruned, faclen=0, cex=0.8, extra=1)

fancyRpartPlot(tree.pruned) ##0.91 sono gli 0 predetti giustamente; 0.09 quelli male;
                            ## 0.41 sono gli 1 predetti bene; 0.59 quelli predetti male

##♦VEDIAMO L'ALBERO SUL TRAIN PER VEDERE SE C'è OVERFITTING

predict_unseen_train<-predict(tree.pruned, train_df, type="class")
table_mat_train<-table(train_df$target, predict_unseen_train)
confusionMatrix(predict_unseen_train, train_df$target, positive="1")

tabtree_train= prop.table(table(predict_unseen_train, train_df$target)); tabtree_train

err.tree_train = tabtree_train[1,2] + tabtree_train[2,1]

err.tree_train

### VEDIAMO L'ALBERO SUL TEST


predict_unseen<-predict(tree.pruned, test_df, type="class")
table_mat<-table(test_df$target, predict_unseen)
confusionMatrix(predict_unseen, test_df$target, positive="1")

tabtree= prop.table(table(predict_unseen, test_df$target)); tabtree

err.tree = tabtree[1,2] + tabtree[2,1]
err.tree ## 

cbind(err.logit, err.tree)


###VEDIAMO CHE IN RELATà ALBERO SU TRAIN E TEST DANNO PIU O MENO GLI STESSI RISULTATI;
## IL MODELLO NON SI ADATTA AI DATI, MA RIMANE COMUNQUE UN MODELLO CHE CREA TROPPE
## MISCLASSIFICATIONS.


###come si puo vedere l'albero sceglie come best split un solo predittore; questo è dovuto dal fatto
## che la variabile city_dev_ind è la più discriminante ed ha il dominio su tutte le altre;
## infatti riesce da per se a classificare "bene" la variabile target

##tuttavia anche l'algoritmo tree model da risultati scadenti come il logit intorno ad un errore del 14%
## entrambi i modelli si rivelano essere non buoni per i soliti motivi


##RICORDA:
#la funzione random forest setta il sotto insieme di predittori per fare previsioni come n° predittori / 3

###RANDOM FOREST
library(randomForest)
set.seed(1)
model<- randomForest(formula=target~., data= train_df, ntree=500, proximity=T)
model
model$confusion
### vediamo come rf è bravo a classificare 0 ma molto male gli 1 (errore = 0.59)

##di default 500 alberi
##sceglie il predittore sulla base dell'importanza-< di default sono 3
#mi da 14.4% di OOB error
#poi mi dà confusion matrix

##tra tutti gli alberi cerchiamo quello che ci da un MSE minore

model$err.rate
head(model$err.rate)

which.min(model$err.rate[,1]) #min oob 
which.min(model$err.rate[,2]) #min 0
which.min(model$err.rate[,3]) #min 1
#vediamo come cambia albero ottimale in base a quello che vogliamo minimzzare

#plotto
dev.new()
plot(model)

#linea verde: OOB
#linea nera: non cambiano lavoro (0)
#linea rossa: cambiano lavoro (1)


#vediamo che anche all'aumentare del numero di alberi l'errore rimane costante.
#in realtà rimane costante fin quasi da subito, a sostegno della tesi precedente
#riguradnte il decision tree
#adding new tree doesn't help


dev.new()
varImpPlot(model) #ci plotta l'importanza dei predittori all'interno degli alberi
#l'asse delle x mostra la purezza del nodo per gli alberi di regressione su split che considerano questi regressori
## l'importanza Ã¨ sempre misurata con il concetto di impuritÃ
##vediamo che wind Ã¨ il predittore piu importante per la costruzione degli alberi
#( l'importanza si basa sul miglior predittore cioÃ¨ quello che se inserito su albero decisionale mi da impuritÃ  piÃ¹ alta)


###vediamo per il train
predict_unseen_trainrf<-predict(model, train_df, type="response")
table_mat<-table(train_df$target, predict_unseen_trainrf)
confusionMatrix(predict_unseen_trainrf, train_df$target, positive = "1")


tabrandomtrain= prop.table(table(predict_unseen_trainrf, train_df$target)); tabrandomtrain

err.randomtrain = tabrandomtrain[1,2] + tabrandomtrain[2,1]; err.randomtrain




predict_unseen<-predict(model, test_df, type="response")
table_mat<-table(test_df$target, predict_unseen)
confusionMatrix(predict_unseen, test_df$target, positive = "1")


tabrandom= prop.table(table(predict_unseen, test_df$target)); tabrandom

err.random = tabrandom[1,2] + tabrandom[2,1]; err.random


cbind(err.logit, err.tree, err.random)

###VEDIAMO ORA SE RIUSCIAMO A MIGLIORARE IL MODELLO 

##TUNE MTRY

#N.B: stepfactor= at ogni iterazione, mtry è gonfiato o sgonfiato di questo valore
##the (relative) improvement in OOB error must be by this much for the search to continue


t= tuneRF(train_df[,-12], train_df[,12],
          stepFactor = 0.5,
          plot=T,
          ntreeTry =200,
          trace=T,
          improve = 0.05)

### vediamo che OOB error migliora leggermente: proviamo a runnare ora il random forest con questi parametri

model1<- randomForest(formula=target~., data= train_df, ntree=200, mtry=3 , importance=T, proximity=T)

model1

##rivediamo i risultati su train e test

predict_unseen_trainrf1<-predict(model1, train_df, type="response")
table_mat1<-table(train_df$target, predict_unseen_trainrf1)
confusionMatrix(predict_unseen_trainrf1, train_df$target, positive = "1")


tabrandomtrain1= prop.table(table(predict_unseen_trainrf1, train_df$target)); tabrandomtrain1

err.randomtrain1 = tabrandomtrain1[1,2] + tabrandomtrain1[2,1]; err.randomtrain1


### vediamo come è leggermente migliorato

predict_unseen11<-predict(model1, test_df, type="response")
table_mat11<-table(test_df$target, predict_unseen11)
confusionMatrix(predict_unseen11, test_df$target, positive = "1")


tabrandom11= prop.table(table(predict_unseen11, test_df$target)); tabrandom11

err.random11 = tabrandom11[1,2] + tabrandom11[2,1]; err.random11


### il modello migliora ma c'è ancora overfitting:
## la soluzoine sarebbe quella di andare a cercare ulteriori migliori tuning parameter
## noi abbiamo solo calcolato mtry, ma per migliorare il random forest
## avremmo dovuto provare a migliorare ulteriori parametri come :
#nodesize - minimum size of terminal nodes;
#maxnodes - maximum number of terminal nodes



##ANDIAMO A VEDERE IL NUMERO DI NODI PER GLI ALBERI

hist(treesize(model1),
     main = "Numero di nodi per gli alberi",
     col="blue")


varImpPlot(model1)

## il grafico di sinistra mostra come il modello peggiora al venir meno di ogni variabile
## il grafico di destra mostra quanto pure i nodi sono
## alla fine dell'albero senza ciascuna variabile


varImpPlot(model1, sort= T,
           n.var = 10,
           main = "Top10 variabili importanti")

varUsed(model1) ## ci trova quale predittore sono utilizzati nel ranfom forest
## valori piu bassi sono quelle che hanno meno importanza.
## ci dice sulla base dell'importanza quanto volte questi predittori sono apparsi

#DEPENDENCE PLOT
## partial dependence plot ci da una rappresentazione grafica
## degli effetti marginali di ogni variabile sulla probabilità di classe

##vediamolo per city development index sulla classe 0 
install.packages("plotfunctions")

library(plotfunctions)
par(mfrow=c(1,2))
partialPlot(model1, train_df, city_development_index, "0")

##vediamo che quando city_dev_ind cresce tende a predirre meglio la classe 0
## questo ci rimanda al fatto che la maggior parte degli individui appartengono a quella classe

partialPlot(model1, train_df, city_development_index, "1")

## vediamo invece che per la classe 1, il modello prevede meglio quando city dev ind ha un valore piu basso

## questo mi porta a confermare che la scarsità del modello nel predirre gli 1
## è dato anche dal fatto che chi vuole cambiare lavoro (come visto già prima) sono individui che appartengono a città 
## dove il tasso di sviluppo è piu basso;
# ma poichè la maggior parte delle persone appartengono a città in cui
## il tasso di sviluppo è elevato, il modello ha dei problemi


## estraiamo ora il singolo albero dalla foresta

albero=getTree(model1, 1, labelVar = T) ## 1 indica albero numero 1
head(albero)

##left daughter the row where the left daughter node is; 0 if the node is terminal
#right daughter the row where the right daughter node is; 0 if the node is terminal

#split var which variable was used to split the node; 0 if the node is terminal

#split point where the best split is; see Details for categorical predictor

#status is the node terminal (-1) or not (1)

#prediction: the prediction for the node; 0 if the node is not terminal


set.seed(154)
##MULTI DIMENSIONAL SCALING PLOT OF PROXIMITY MATRIX
MDSplot(model1, train_df$target)


####TUNING PARAMETER MTRY
metric <- "Accuracy"
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(mtry=c(1:11))
set.seed(157)
tune <- train(target~., data=train_df, method = "rf", 
          metric=metric, tuneGrid=tunegrid, trControl=control)
print(tune)
summary(tune)
plot(tune)


## il muiglior albero è quello che usa 3 variabili ogni volta.
###•ALTRO MODO PER FARE TREE MODEL


tree.model=tree(target~., train_df)
plot(tree.model)
predas=predict(tree.model, test_df, type = "class")

mean(predas != test_df$target) ##misclassification error tree 14.37%


##FACCIAMO CV PER VEDERE DOVE TAGLIARE ALBERO
## l'albero ha già un solo split quindi non bisognerà tagliare
## è SOLO UNA CONFERMA

cv_tree=cv.tree(tree.model, FUN= prune.misclass)
names(cv_tree)

plot(cv_tree$size, cv_tree$dev, type="b")
## errore minimo a 2

pruned_model= prune.misclass(tree.model, best= 2)

plot(pruned_model)
text(pruned_model, pretty=0)

### vediamo che l'albero è sempre lo stesso!