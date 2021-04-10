library(dplyr)
library(ggplot2)
library(cluster) 
library(factoextra)
library(corrplot)


data <- read.csv(file = 'Data_Cortex_Nuclear.csv', header = TRUE)
data = na.omit(data)
feature = subset(data, select = -c(class,MouseID,Genotype, Treatment, Behavior))
label = data$class
#feature$Treatment = as.numeric(feature$Treatment)
#feature$Genotype = as.numeric(feature$Genotype)
#feature$Behavior = as.numeric(feature$Behavior)
label = as.numeric(label)
feature
label
data

length(label)

library(factoextra)
pcclust=prcomp(feature,scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

dignm<-as.character(label)

pcclust$x[,1:2]

Classes = factor(label,labels = c("c-CS-s","c-CS-m","c-SC-s","c-SC-m","t-CS-s","t-CS-m","t-SC-s","t-SC-m"))
# Viz using ggplot
ggplot(data.frame(pcclust$x[,1:2]), aes(pcclust$x[,1], pcclust$x[,2], color = Classes)) + 
  xlab("PCA 1") +
  ylab("PCA 2") +
  geom_point(size = 3)



library(Rtsne)

#label = as.factor(data$UnderRisk)
Classes = factor(label,labels = c("c-CS-s","c-CS-m","c-SC-s","c-SC-m","t-CS-s","t-CS-m","t-SC-s","t-SC-m"))
colors = rainbow(length(unique(label)))
names(colors) = unique(label)

tsne_plot <- function(perpl=5,iterations=1000){
  set.seed(1) # for reproducibility
  tsne <- Rtsne(feature, dims = 2, check_duplicates = FALSE,perplexity=perpl, verbose=TRUE, max_iter = iterations)
  ggplot(data.frame(tsne$Y), aes(tsne$Y[,1], tsne$Y[,2], color = Classes)) +
    xlab("TSNE 1") +
    ylab("TSNE 2")+
    ggtitle("Perplexity = 15,Iterations = 1000")+
    geom_point(size = 4)
  
}
tsne_plot()



library(umapr)
library(tidyverse)


df <- as.matrix(feature)

label = as.factor(label)

Classes = factor(label,labels = c("c-CS-s","c-CS-m","c-SC-s","c-SC-m","t-CS-s","t-CS-m","t-SC-s","t-SC-m"))
# run UMAP algorithm
embedding <- umap(df,n_neighbors = 12,min_dist = 0.1, metric = "euclidean",random_state = as.integer(45))
#head(embedding)

embedding %>% 
  ggplot(aes(UMAP1, UMAP2, color = Classes)) +
  xlab("UMAP 1") +
  ylab("UMAP 2")+
  ggtitle("n_neighbors = 75,min_dist = 0.5,metric = manhattan")+
  geom_point(size = 4)

nrow(feature)
label




neighbors <- c(5,15,25,35,45,55,65,75,85,95,105,115,125,150,175,200,300,500)



neighbors %>% 
  map_df(~umap(as.matrix(feature), min_dist = 0.5, metric = "manhattan",n_neighbors = .x, random_state = as.integer(45)) %>% 
           mutate(pathology = Classes, Neighbor = .x)) %>% 
  mutate(Neighbor = as.integer(Neighbor)) %>% 
  ggplot(aes(UMAP1, UMAP2, color = pathology)) + 
  geom_point(size = 3) + 
  facet_wrap(~ Neighbor, scales = "free")


# Multi Dimensional Scaling

d <- dist(feature) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2, add = FALSE, x.ret = FALSE)

x <- fit$points[,1]
y <- fit$points[,2]
label = as.factor(label)

label = factor(label,labels = c("c-CS-s","c-CS-m","c-SC-s","c-SC-m","t-CS-s","t-CS-m","t-SC-s","t-SC-m"))

ggplot(data.frame(fit$points), aes(x, y, color = label)) + ggtitle("Multi Dimensional Scaling") + geom_point(size =3)

library(MASS)
fit <- isoMDS(d,k=2)

x <- fit$points[,1]
y <- fit$points[,2]

ggplot(data.frame(fit$points), aes(x, y, color = Classes)) + geom_point(size = 3)


library(MASS)
fit <- sammon(d)

x <- fit$points[,1]
y <- fit$points[,2]

ggplot(data.frame(fit$points), aes(x, y, color = label)) + geom_point()

##Silhoutte coefficient
df = data.frame(tsne$Y[,1],tsne$Y[,2])
#sil = silhouette(data$TenYearCHD, dist(df))
#print(mean(sil[,3]))
##correlation coefficient
#print(cor(embedding$UMAP1,embedding$UMAP2,  method = "pearson"))

## Connectivity
#install.packages("clValid")
#print(connectivity(distance = dist(df), data$DEATH_EVENT, neighbSize = 4,
#method = "euclidean"))
