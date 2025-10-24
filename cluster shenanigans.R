library(tidyverse)
library(rvest)
library(readxl)
library(factoextra)
Mountains <- read_excel("~/Mountains.xlsx",
sheet = "Alpine mountains over 3000 m hi")
Mountains$`Range [Note 4]`<- as.factor(Mountains$`Range [Note 4]`)
Mountains$`Region [Note 6]`<- as.factor(Mountains$`Region [Note 6]`)
Mountains$Country <- as.factor(Mountains$Country)
clustset <- Mountains[,c(2,3)]
clustdiag <- fviz_nbclust(clustset,FUNcluster=kmeans,method="wss")
set.seed(666)
model <- kmeans(clustset,4)
model2 <- kmeans(clustset,5)
clustplot2 <- fviz_cluster(model2,data=clustset)
clustplot <- fviz_cluster(model,data=clustset)



