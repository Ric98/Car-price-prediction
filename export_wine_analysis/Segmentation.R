rm(list=ls())
library(rstudioapi)
current_path<-getActiveDocumentContext()$path
print(getwd())

library(dplyr)
library(tidyverse)
library(readxl)
library(data.table)

file_2010<-read_excel("DATASET CLUSTER.xlsx", sheet = "2010")
file_2011<-read_excel("DATASET CLUSTER.xlsx", sheet = "2011")
file_2012<-read_excel("DATASET CLUSTER.xlsx", sheet = "2012")
file_2013<-read_excel("DATASET CLUSTER.xlsx", sheet = "2013")
file_2014<-read_excel("DATASET CLUSTER.xlsx", sheet = "2014")
file_2015<-read_excel("DATASET CLUSTER.xlsx", sheet = "2015")
file_2016<-read_excel("DATASET CLUSTER.xlsx", sheet = "2016")
file_2017<-read_excel("DATASET CLUSTER.xlsx", sheet = "2017")
file_2018<-read_excel("DATASET CLUSTER.xlsx", sheet = "2018")
file_2019<-read_excel("DATASET CLUSTER.xlsx", sheet = "2019")
file_2020<-read_excel("DATASET CLUSTER.xlsx", sheet = "2020")


library(datawizard)
z_2010<-scale(file_2010[,-c(1,4,6)])
z_2011<-scale(file_2011[,-c(1,4,6)])
z_2012<-scale(file_2012[,-c(1,4,6)])
z_2013<-scale(file_2013[,-c(1,4,6)])
z_2014<-scale(file_2014[,-c(1,4,6)])
z_2015<-scale(file_2015[,-c(1,4,6)])
z_2016<-scale(file_2016[,-c(1,4,6)])
z_2017<-scale(file_2017[,-c(1,4,6)])
z_2018<-scale(file_2018[,-c(1,4,6)])
z_2019<-scale(file_2019[,-c(1,4,6)])
z_2020<-scale(file_2020[,-c(1,4,6)])


library(ggcorrplot)
library(corrplot)

cor(z_2020) %>% corrplot(method = "color", type = "lower", tl.col = "black", tl.srt = 45,
           addCoef.col = TRUE, p.mat = cor.mtest(z_2010)$p, sig.level = 1, 
           title="Corrplot", mar=c(0,0,1,0))

cormat<-cor(z_2020)
mean(cormat)
mean(abs(cormat))

############## PAM ###############
library(cluster)
library(factoextra)
library(ggplot2)

# SCALE
fviz_nbclust(z_2020, pam, method ="silhouette")+theme_minimal()  
fviz_nbclust(z_2020, pam, method ="wss")+theme_minimal() 
pam_2010<-pam(z_2010, 4)
fviz_cluster(pam_2010)

############## K MEANS ++ ###############

library(ClusterR)
library(cluster)

k_2010<-kmeans(z_2010, centers=4, iter.max=100, nstart=25)
fviz_cluster(k_2010, z_2010, geom="text", frame.type="convex")
cluster_2010<-as.data.frame(k_2010$cluster)
row.names(cluster_2010)<-file_2010$PAESI

k_2011<-kmeans(z_2011, centers=4, iter.max=100, nstart=25)
fviz_cluster(k_2011, z_2011, geom="text", frame.type="convex")
cluster_2011<-as.data.frame(k_2011$cluster)
row.names(cluster_2011)<-file_2011$PAESI

k_2012<-kmeans(z_2012, centers=4, iter.max=100, nstart=25)
fviz_cluster(k_2012, z_2012, geom="text", frame.type="convex")
cluster_2012<-as.data.frame(k_2012$cluster)
row.names(cluster_2012)<-file_2012$PAESI

k_2013<-kmeans(z_2013, centers=4, iter.max=100, nstart=25)
fviz_cluster(k_2013, z_2013, geom="text", frame.type="convex")
cluster_2013<-as.data.frame(k_2013$cluster)
row.names(cluster_2013)<-file_2013$PAESI

k_2014<-kmeans(z_2014, centers=4, iter.max=100, nstart=25)
fviz_cluster(k_2014, z_2014, geom="text", frame.type="convex")
cluster_2014<-as.data.frame(k_2014$cluster)
row.names(cluster_2014)<-file_2014$PAESI

k_2015<-kmeans(z_2015, centers=4, iter.max=100, nstart=25)
fviz_cluster(k_2015, z_2015, geom="text", frame.type="convex")
cluster_2015<-as.data.frame(k_2015$cluster)
row.names(cluster_2015)<-file_2015$PAESI

k_2016<-kmeans(z_2016, centers=4, iter.max=100, nstart=25)
fviz_cluster(k_2016, z_2016, geom="text", frame.type="convex")
cluster_2016<-as.data.frame(k_2016$cluster)
row.names(cluster_2016)<-file_2016$PAESI

k_2017<-kmeans(z_2017, centers=4, iter.max=100, nstart=25)
fviz_cluster(k_2017, z_2017, geom="text", frame.type="convex")
cluster_2017<-as.data.frame(k_2017$cluster)
row.names(cluster_2017)<-file_2017$PAESI

k_2018<-kmeans(z_2018, centers=4, iter.max=100, nstart=25)
fviz_cluster(k_2018, z_2018, geom="text", frame.type="convex")
cluster_2018<-as.data.frame(k_2018$cluster)
row.names(cluster_2018)<-file_2018$PAESI

k_2019<-kmeans(z_2019, centers=4, iter.max=100, nstart=25)
fviz_cluster(k_2019, z_2019, geom="text", frame.type="convex")
cluster_2019<-as.data.frame(k_2019$cluster)
row.names(cluster_2019)<-file_2019$PAESI

##

k_2020<-kmeans(z_2020, centers=4, iter.max=100, nstart=25)

fviz_cluster(k_2020, z_2020, geom="text", frame.type="convex")
cluster_2020<-as.data.frame(k_2020$cluster)
row.names(cluster_2020)<-file_2020$PAESI


maj_voting_cluster<-as.data.frame(cbind(cluster_2010, cluster_2011, cluster_2012, cluster_2013, 
                    cluster_2014, cluster_2015, cluster_2016, cluster_2017, cluster_2018, 
                    cluster_2019, cluster_2020))
names<-rownames(maj_voting_cluster)
maj_voting<-cbind(names, maj_voting_cluster)
library(writexl)
write_xlsx(maj_voting, path="C:/Users/Riccardo/Desktop/maj_vot.xlsx")

## Gdp vs Popolazione
fviz_cluster(object = k_2020, data = z_2020, geom = "text",
             choose.vars = c("Gdp", "Popolazione"), frame.type = "convex")

## Gdp vs Densità
fviz_cluster(object = k_2020, data = z_2020, geom = "text",
             choose.vars = c("Gdp", "Densità"), frame.type = "convex")

## Popolazione vs Densità
fviz_cluster(object = k_2020, data = z_2020, geom = "text",
             choose.vars = c("Popolazione", "Densità"), frame.type = "convex")

################# STATS K MEANS ++

k_2020
k_2020$withinss
k_2020$tot.withinss
k_2020$betweenss
k_2020$size

k_2020$centers

mean_popolazione<-mean(file_2020$Popolazione)
sd_popolazione<-sd(file_2020$Popolazione)
mean_gdp<-mean(file_2020$Gdp)
sd_gdp<-sd(file_2020$Gdp)
mean_densità<-mean(file_2020$Densità)
sd_densità<-sd(file_2020$Densità)
#mean_growth<-mean(file_2020$Growth)
#sd_growth<-sd(file_2020$Growth)

pop_1 <- (k_2020$centers[1] * sd_popolazione) + mean_popolazione
pop_2 <- (k_2020$centers[2] * sd_popolazione) + mean_popolazione
pop_3 <- (k_2020$centers[3] * sd_popolazione) + mean_popolazione
pop_4 <- (k_2020$centers[4] * sd_popolazione) + mean_popolazione

gdp_1 <- (k_2020$centers[5] * sd_gdp) + mean_gdp
gdp_2 <- (k_2020$centers[6] * sd_gdp) + mean_gdp
gdp_3 <- (k_2020$centers[7] * sd_gdp) + mean_gdp
gdp_4 <- (k_2020$centers[8] * sd_gdp) + mean_gdp

dens_1 <- (k_2020$centers[9] * sd_densità) + mean_densità
dens_2 <- (k_2020$centers[10] * sd_densità) + mean_densità
dens_3 <- (k_2020$centers[11] * sd_densità) + mean_densità
dens_4 <- (k_2020$centers[12] *  sd_densità) + mean_densità

#growth_1 <- (k_2020$centers[13] * sd_growth) + mean_growth
#growth_2 <- (k_2020$centers[14] * sd_growth) + mean_growth
#growth_3 <- (k_2020$centers[15] * sd_growth) + mean_growth
#growth_4 <- (k_2020$centers[16] * sd_growth) + mean_growth

pop_cluster <- as.data.frame(rbind(pop_1, pop_2, pop_3, pop_4), row.names = F)
colnames(pop_cluster)<-"Popolazione"

gdp_cluster <- as.data.frame(rbind(gdp_1, gdp_2, gdp_3, gdp_4), row.names = F)
colnames(gdp_cluster)<-"Gdp"

dens_cluster <- as.data.frame(rbind(dens_1, dens_2, dens_3, dens_4), row.names = F)
colnames(dens_cluster)<-"Densità"

#growth_cluster <- as.data.frame(rbind(growth_1, growth_2, growth_3, growth_4), row.names = F)
#colnames(growth_cluster)<-"Growth"

cluster_FINALE <- as.data.frame(cbind(pop_cluster, gdp_cluster, dens_cluster))
#row.names(cluster_FINALE) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")
cluster_FINALE
k_2020$size


cluster_2020

library(factoextra)
sil <- silhouette(k_2020$cluster, dist(z_2020))
fviz_silhouette(sil)

z_2020_NOMALTA<-z_2020[-17,]
k_2020_NOMALTA<-kmeans(z_2020_NOMALTA, centers=3, iter.max=100, nstart=25)
fviz_cluster(k_2020_NOMALTA, z_2020_NOMALTA, geom="text", frame.type="convex")
cluster_2020_NOMALTA<-as.data.frame(k_2020_NOMALTA$cluster)
row.names(cluster_2020_NOMALTA)<-file_2020$PAESI[-17]
sil_NOMALTA <- silhouette(k_2020_NOMALTA$cluster, dist(z_2020_NOMALTA))
fviz_silhouette(sil_NOMALTA)

z_2020_NOMALTA_NOIRL<-z_2020[-c(14,17),]
k_2020_NOMALTA_NOIRL<-kmeans(z_2020_NOMALTA_NOIRL, centers=3, iter.max=100, nstart=25)
fviz_cluster(k_2020_NOMALTA_NOIRL, z_2020_NOMALTA_NOIRL, geom="text", frame.type="convex")
cluster_2020_NOMALTA_NOIRL<-as.data.frame(k_2020_NOMALTA_NOIRL$cluster)
row.names(cluster_2020_NOMALTA_NOIRL)<-file_2020$PAESI[-c(14,17)]
sil_NOMALTA_NOIRL <- silhouette(k_2020_NOMALTA_NOIRL$cluster, dist(z_2020_NOMALTA_NOIRL))
fviz_silhouette(sil_NOMALTA_NOIRL)

library(GGally)
library(plotly)

file_2020_NOMALTA <- file_2020[-17,-4]

k_2020_NOMALTA_3cluster<-kmeans(z_2020_NOMALTA, centers=3, iter.max=100, nstart=25)
file_2020_NOMALTA$cluster <- as.factor(k_2020_NOMALTA_3cluster$cluster)
p_NOMALTA<-ggparcoord(data=file_2020_NOMALTA, columns = c(2:5), groupColumn = "cluster", scale="std") + labs(x="Variables", y="Value (in std units)", title="Clustering")
ggplotly(p_NOMALTA)


file_2020_NOMALTA_NOIRL <- file_2020[-c(14,17),-4]

file_2020_NOMALTA_NOIRL$cluster <- as.factor(k_2020_NOMALTA_NOIRL$cluster)
p_NOMALTA_NOIRL<-ggparcoord(data=file_2020_NOMALTA_NOIRL, columns = c(2:5), groupColumn = "cluster", scale="std") + labs(x="Variables", y="Value (in std units)", title="Clustering")
ggplotly(p_NOMALTA_NOIRL)

