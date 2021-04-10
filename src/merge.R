library(dplyr)
library(ggplot2)
library(cluster) 

library(factoextra)
library(corrplot)

# Preprocessing the data


data <- read.csv(file = 'data.csv', header = FALSE, row.names = 1)
head(data)
data = t(data)
ncol(data)
nrow(data)

data = as.data.frame(data)
data = dplyr::filter(data, pathology != 3)

data.train = subset(data, select = -c(pathology))
label = data$pathology
Classes = factor(label,labels = c("Healthy","BAV patients"))

evaluation = read.csv(file = 'evaluation_data.csv')
evaluation = subset(evaluation, select = -c(Timestamp))
colnames(evaluation)
evaluation[ evaluation == 0] <- 1
evaluation

#plot 1

library(umapr)
library(tidyverse)


df <- as.matrix(data.train)
label = as.factor(data$pathology)
Classes = factor(label,labels = c("Healthy","BAV patients"))
# run UMAP algorithm
embedding <- umap(df,n_neighbors = 56,min_dist = 0.5,metric = "euclidean",random_state = as.integer(45))
#head(embedding)

plot1.1 = embedding %>% 
  ggplot(aes(UMAP1, UMAP2, color = Classes)) +
  xlab("UMAP 1") +
  ylab("UMAP 2")+
  ggtitle("n_neighbors = 56,min_dist = 0.5")+
  geom_point(size = 4)



cohort_separability1 = as.factor(evaluation$UMAP..parameter.2...How.well.can.you.visually.separate.the.cohorts.)
plot1.2 = ggplot(evaluation, aes(x=UMAP..parameter.2...How.well.can.you.visually.separate.the.cohorts., fill=cohort_separability1)) +
  xlab("rating") +
  geom_bar()+
  theme(legend.position = "none")+scale_fill_manual(values = c("#0072B2","#56B4E9", "#CC79A7", "#D55E00", "#009E73"))


outlier_findability1 = as.factor(evaluation$UMAP..parameter.2....How.well.can.you.identify.outliers..)
plot1.3 = ggplot(evaluation, aes(x=UMAP..parameter.2....How.well.can.you.identify.outliers.., fill=outlier_findability1)) +
  xlab("rating") +
  geom_bar()+
  theme(legend.position = "none")+scale_fill_manual(values = c("#0072B2","#56B4E9", "#CC79A7", "#D55E00", "#009E73"))





#Plot 2

embedding <- umap(df,n_neighbors = 12,min_dist = 0.1,metric = "euclidean",random_state = as.integer(45))
#head(embedding)

plot2.1 = embedding %>% 
  ggplot(aes(UMAP1, UMAP2, color = Classes)) +
  xlab("UMAP 1") +
  ylab("UMAP 2")+
  ggtitle("n_neighbors = 12,min_dist = 0.1")+
  geom_point(size = 4)


cohort_separability2 = as.factor(evaluation$UMAP..parameter.3...How.well.can.you.visually.separate.the.cohorts.)
plot2.2 = ggplot(evaluation, aes(x=UMAP..parameter.3...How.well.can.you.visually.separate.the.cohorts., fill=cohort_separability2)) +
  xlab("rating") +
  geom_bar()+
  theme(legend.position = "none")+scale_fill_manual(values = c("#0072B2","#56B4E9", "#CC79A7", "#D55E00", "#009E73"))



outlier_findability2 = as.factor(evaluation$UMAP..parameter.3...How.well.can.you.identify.outliers..)
plot2.3 = ggplot(evaluation, aes(x=UMAP..parameter.3...How.well.can.you.identify.outliers.., fill=outlier_findability2)) +
  xlab("rating") +
  geom_bar()+
  theme(legend.position = "none")+scale_fill_manual(values = c("#0072B2","#56B4E9", "#CC79A7", "#D55E00", "#009E73"))




#Plot 3
library(Rtsne)
tsne_plot <- function(perpl=35,iterations=2000){
  set.seed(1) # for reproducibility
  tsne <- Rtsne(data.train, dims = 2, check_duplicates = FALSE,perplexity=perpl, verbose=TRUE, max_iter = iterations)
  ##Silhoutte coefficient
  df = data.frame(tsne$Y[,1],tsne$Y[,2])
  #sil = silhouette(data$pathology, dist(df))
  #print(mean(sil[,3]))
  ##correlation coefficient
  #print(cor(embedding$UMAP1,embedding$UMAP2,  method = "pearson"))
  
  ## Connectivity
  #install.packages("clValid")
  #print(connectivity(distance = dist(df), data$pathology, neighbSize = 4,
  #                  method = "euclidean"))
  g1 = ggplot(data.frame(tsne$Y), aes(tsne$Y[,1], tsne$Y[,2], color = Classes)) +
    xlab("TSNE 1") +
    ylab("TSNE 2")+
    ggtitle("Perplexity = 35,Iterations = 2000")+
    geom_point(size = 4)
  
}
plot3.1 = tsne_plot()

evaluation$TSNE..parameter.1...How.well.can.you.visually.separate.the.cohorts. <- factor(evaluation$TSNE..parameter.1...How.well.can.you.visually.separate.the.cohorts., levels = c("1", "2", "3","4","5"))

cohort_separability3 = as.factor(evaluation$TSNE..parameter.1...How.well.can.you.visually.separate.the.cohorts.)
plot3.2 = ggplot(evaluation, aes(x=TSNE..parameter.1...How.well.can.you.visually.separate.the.cohorts., fill=cohort_separability3)) +
  xlab("rating") +
  geom_bar()+
  scale_x_discrete(drop=FALSE)+
  theme(legend.position = "none")+scale_fill_manual(values = c("#56B4E9", "#CC79A7", "#D55E00", "#009E73"))
  

plot3.2

outlier_findability3 = as.factor(evaluation$TSNE..parameter.1...How.well.can.you.identify.outliers..)
plot3.3 = ggplot(evaluation, aes(x=TSNE..parameter.1...How.well.can.you.identify.outliers.., fill=outlier_findability3)) +
  xlab("rating") +
  geom_bar()+
  theme(legend.position = "none")+scale_fill_manual(values = c("#0072B2","#56B4E9", "#CC79A7", "#D55E00", "#009E73"))




#Plot 4
library(Rtsne)
tsne_plot <- function(perpl=15,iterations=1000){
  set.seed(1) # for reproducibility
  tsne <- Rtsne(data.train, dims = 2, check_duplicates = FALSE,perplexity=perpl, verbose=TRUE, max_iter = iterations)
  ##Silhoutte coefficient
  df = data.frame(tsne$Y[,1],tsne$Y[,2])
  #sil = silhouette(data$pathology, dist(df))
  #print(mean(sil[,3]))
  ##correlation coefficient
  #print(cor(embedding$UMAP1,embedding$UMAP2,  method = "pearson"))
  
  ## Connectivity
  #install.packages("clValid")
  #print(connectivity(distance = dist(df), data$pathology, neighbSize = 4,
  #                  method = "euclidean"))
  g1 = ggplot(data.frame(tsne$Y), aes(tsne$Y[,1], tsne$Y[,2], color = Classes)) +
    xlab("TSNE 1") +
    ylab("TSNE 2")+
    ggtitle("Perplexity = 15,Iterations = 1000")+
    geom_point(size = 4)
  
}
plot4.1 = tsne_plot()


cohort_separability4 = as.factor(evaluation$TSNE..parameter.2...How.well.can.you.visually.separate.the.cohorts.)
plot4.2 = ggplot(evaluation, aes(x=TSNE..parameter.2...How.well.can.you.visually.separate.the.cohorts., fill=cohort_separability4)) +
  xlab("rating") +
  geom_bar()+
  theme(legend.position = "none")+scale_fill_manual(values = c("#0072B2","#56B4E9", "#CC79A7", "#D55E00", "#009E73"))

evaluation$TSNE..parameter.2...How.well.can.you.identify.outliers.. <- factor(evaluation$TSNE..parameter.2...How.well.can.you.identify.outliers.., levels = c("1", "2", "3","4","5"))

outlier_findability4 = as.factor(evaluation$TSNE..parameter.2...How.well.can.you.identify.outliers..)
plot4.3 = ggplot(evaluation, aes(x=TSNE..parameter.2...How.well.can.you.identify.outliers.., fill=outlier_findability4)) +
  xlab("rating") +
  geom_bar()+
  scale_x_discrete(drop=FALSE)+
  theme(legend.position = "none")+scale_fill_manual(values = c("#56B4E9", "#CC79A7", "#D55E00", "#009E73"))




#plot 5

library(Rtsne)
tsne_plot <- function(perpl=5,iterations=500){
  set.seed(1) # for reproducibility
  tsne <- Rtsne(data.train, dims = 2, check_duplicates = FALSE,perplexity=perpl, verbose=TRUE, max_iter = iterations)
  ##Silhoutte coefficient
  df = data.frame(tsne$Y[,1],tsne$Y[,2])
  #sil = silhouette(data$pathology, dist(df))
  #print(mean(sil[,3]))
  ##correlation coefficient
  #print(cor(embedding$UMAP1,embedding$UMAP2,  method = "pearson"))
  
  ## Connectivity
  #install.packages("clValid")
  #print(connectivity(distance = dist(df), data$pathology, neighbSize = 4,
  #                  method = "euclidean"))
  g1 = ggplot(data.frame(tsne$Y), aes(tsne$Y[,1], tsne$Y[,2], color = Classes)) +
    xlab("TSNE 1") +
    ylab("TSNE 2")+
    ggtitle("Perplexity = 5,Iterations = 500")+
    geom_point(size = 4)
  
}
plot5.1 = tsne_plot()

x = c(1,2,3,4,5)
cohort_separability5 = as.factor(evaluation$TSNE..parameter.3...How.well.can.you.visually.separate.the.cohorts.)
plot5.2 = ggplot(evaluation, aes(x=TSNE..parameter.3...How.well.can.you.visually.separate.the.cohorts., fill=cohort_separability5)) +
  xlab("rating") +
  geom_bar()+
  theme(legend.position = "none")+scale_fill_brewer(palette="Dark2")+scale_fill_manual(values = c("#0072B2","#56B4E9", "#CC79A7", "#D55E00", "#009E73"))


outlier_findability5 = as.factor(evaluation$TSNE..parameter.3...How.well.can.you.identify.outliers..)
plot5.3 = ggplot(evaluation, aes(x=TSNE..parameter.3...How.well.can.you.identify.outliers.., fill=outlier_findability5)) +
  xlab("rating") +
  geom_bar()+
  theme(legend.position = "none")+scale_fill_brewer(palette="Dark2")+scale_fill_manual(values = c("#0072B2","#56B4E9", "#CC79A7", "#D55E00", "#009E73"))


#install.packages("ggpubr")
library(ggpubr)

ggarrange(plot2.1,plot2.2,plot2.3,plot3.1,plot3.2,plot3.3,plot4.1,plot4.2,plot4.3,plot5.1,plot5.2,plot5.3,plot1.1,plot1.2,plot1.3,
          labels = c("A", "B","C"),
          ncol = 3, nrow = 5)


plot1.1+theme(text = element_text(size = 18))

#y <- data.frame(howgood = c("Better","Better","Good","Good","Better"))
#y$howgood
#evaluation$TSNE..parameter.2...How.well.can.you.identify.outliers..
#evaluation$TSNE..parameter.2...How.well.can.you.identify.outliers.. <- factor(evaluation$TSNE..parameter.2...How.well.can.you.identify.outliers.., levels = c("1", "2", "3","4","5"))

#ggplot(y, aes(howgood)) +
#  geom_bar() +
#  scale_x_discrete(drop=FALSE)