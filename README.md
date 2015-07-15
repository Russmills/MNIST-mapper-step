---
title: "MNIST mapper step"
author: "Jiyi Jiang"
date: "July 15, 2015"
output: html_document
---

```{r}
library(TDAmapper)
#write.table(M, file = "kirsch_matrix.csv", append = FALSE, quote = TRUE, sep = ",",row.names=FALSE,col.names = FALSE)
set.seed(6)
ind <- sample(nrow(Q),size=6000,replace=FALSE)
labels <- train$y[ind]
Rtsne_input=Q[ind,]# number of random rows of 60000.
library(Rtsne)
Rtsne_result=Rtsne(Rtsne_input, dims = 3, initial_dims = 30, perplexity = 40,
        theta = 0.1, check_duplicates = TRUE, pca = TRUE, max_iter = 1000,
        verbose = FALSE, is_distance = FALSE)

Rtsne_result000 <- read.csv("darkenmatlabtsneoutput.csv",header = FALSE)

pc1 <- matrix(Rtsne_result$Y[,1])
pc2 <- matrix(Rtsne_result$Y[,2])
pc3 <- matrix(Rtsne_result$Y[,3])
cc1=gsub("1","gold",labels)
cc2=gsub("0","darkgreen",cc1)
colorlabels <- cc2

#plot3d(pc1, pc2, pc3)
library(plot3D)
library(rgl)
plot3d(pc1,pc2,pc3 )

pc1 <- Rtsne_result000$Y[,1]
pc1 <- Rtsne_result000[,1]
pc1 <- Rtsne_result000[,1]
pc2 <- Rtsne_result000[,2]
pc3 <- Rtsne_result000[,3]
plot3d(pc1,pc2,pc3)
```


```{r}
m2 <- mapper(
    distance_matrix = dist(data.frame(train$x[1:6000,]),
    filter_values = list( Rtsne_result000[,1], Rtsne_result000[,3] ),
    num_intervals = c(5,5),
    percent_overlap = 50,
    num_bins_when_clustering = 10)
    
    
m2 <- mapper(
    distance_matrix = dist(train$x[1:6000,]),
    filter_values = list(Rtsne_result000[,1], Rtsne_result000[,3]),
    num_intervals = c(5,5),
    percent_overlap = 50
    num_bins_when_clustering = 10)

g2 <- graph.adjacency(m2$adjacency, mode="undirected")
plot(g2, layout = layout.auto(g2) )
m2$points_in_vertex

library(igraph)
tkplot(g2)


##from Example 5
f <- Rtsne_result000
m2 <- mapper(dist(train$x[1:6000,]), f, c(5,5,5), c(30,30,30), 5)
g2 <- graph.adjacency(m2$adjacency, mode="undirected")
plot(g2, layout = layout.auto(g2) )
tkplot(g2)

# create data frames for vertices and edges with the right variable names 
MapperNodes <- mapperVertices(m2, 1:dim(f)[1] )
MapperLinks <- mapperEdges(m2)

# interactive plot
forceNetwork(Nodes = MapperNodes, Links = MapperLinks, 
            Source = "Linksource", Target = "Linktarget",
            Value = "Linkvalue", NodeID = "Nodename",
            Group = "Nodegroup", opacity = 0.8, 
            linkDistance = 10, charge = -400)    
```

```{r}
library(TDAmapper)
library(igraph)
library(tcltk)
Rtsne_result000 <- read.csv("darkenmatlabtsneoutput.csv",header = FALSE)
X <- data.frame(x = Rtsne_result000[,1],
                y = Rtsne_result000[,2],
                z = Rtsne_result000[,3])
m5 <- mapper(dist(X), f, c(3,3,3), c(30,30,30), 5)
g5 <- graph.adjacency(m5$adjacency, mode="undirected")
plot(g5, layout = layout.auto(g5) )
tkplot(g5)

f <-X 
# create data frames for vertices and edges with the right variable names 
MapperNodes <- mapperVertices(m5, 1:dim(f)[1] )
MapperLinks <- mapperEdges(m5)

# interactive plot
forceNetwork(Nodes = MapperNodes, Links = MapperLinks, 
            Source = "Linksource", Target = "Linktarget",
            Value = "Linkvalue", NodeID = "Nodename",
            Group = "Nodegroup", opacity = 0.8, 
            linkDistance = 10, charge = -400)    
```
