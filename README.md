---
title: "Mnist (HE)"
author: "Jiyi Jiang"
date: "July 15, 2015"
output: html_document
---

# Load MNIST data

```{r}
#setwd("C:/Users/sumloaner/Downloads")

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('train-images-idx3-ubyte')
  test <<- load_image_file('t10k-images-idx3-ubyte')
  
  train$y <<- load_label_file('train-labels-idx1-ubyte')
  test$y <<- load_label_file('t10k-labels-idx1-ubyte')  
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

load_mnist()
train$n
```

# Histogram equalization(background & noise & darken)
```{r}
library(IM)

unflatten <- function(arr784) {
    return(matrix(arr784, nrow=28)[,28:1])
}

flatten <- function(mat28x28) {
    return(as.vector(mat28x28[,28:1]))
}
Q <-matrix(0, nrow=6000, ncol=784)

for (i in 1:6000){
                digit_flat <- train$x[i,]

# check that unflatten and flatten are inverse functions
# digit_flat2 <- flatten(unflatten(digit_flat))
# sum(digit_flat - digit_flat2)
# show_digit(digit_flat)
# show_digit(digit_flat2)

digit_unflat <- unflatten(digit_flat)
digit_histeq <- histeq( digit_unflat )
digit_histeq_modified <- flatten(digit_histeq)

#digit_histeq_modified [ which(digit_histeq_modified  == min(digit_histeq_modified )) ] <- 0

          
          Average <- sum(digit_histeq_modified)/784
          b <- digit_histeq_modified < Average
          digit_histeq_modified[b] <- 0
          c <- digit_histeq_modified > Average
          digit_histeq_modified[c] <- max(digit_histeq_modified )
          Q[i,] <- digit_histeq_modified

Q[i,] <- digit_histeq_modified

                }



#show_digit(digit_flat)

show_digit(  digit_histeq_modified )
```


# Histogram equalization(background & noise)
```{r}
library(IM)

unflatten <- function(arr784) {
    return(matrix(arr784, nrow=28)[,28:1])
}

flatten <- function(mat28x28) {
    return(as.vector(mat28x28[,28:1]))
}
Q <-matrix(0, nrow=6000, ncol=784)

for (i in 1:6000){
                digit_flat <- train$x[i,]

# check that unflatten and flatten are inverse functions
# digit_flat2 <- flatten(unflatten(digit_flat))
# sum(digit_flat - digit_flat2)
# show_digit(digit_flat)
# show_digit(digit_flat2)

digit_unflat <- unflatten(digit_flat)
digit_histeq <- histeq( digit_unflat )
digit_histeq_modified <- flatten(digit_histeq)

#digit_histeq_modified [ which(digit_histeq_modified  == min(digit_histeq_modified )) ] <- 0

          
          Average <- sum(digit_histeq_modified)/784
          b <- digit_histeq_modified < Average
          digit_histeq_modified[b] <- 0
          #c <- digit_histeq_modified > Average
          #digit_histeq_modified[c] <- max(digit_histeq_modified )
          Q[i,] <- digit_histeq_modified

Q[i,] <- digit_histeq_modified

                }



#show_digit(digit_flat)

show_digit(  digit_histeq_modified )
```
# Run Rtsne on 2D

```{r}
set.seed(2) 
   #M <- train$x[1:6000,]    
#ind <- sample(nrow(train$x),size=6000,replace=FALSE)           
labels <- train$y[1:6000]
Rtsne_input=train$x[1:6000,]# number of random rows of 60000.
library(Rtsne)
Rtsne_result=Rtsne(Rtsne_input, dims = 2, initial_dims = 20, perplexity = 50,
        theta = 0.4, check_duplicates = TRUE, pca = TRUE, max_iter = 1000,
        verbose = FALSE, is_distance = FALSE)


pc1 <- matrix(Rtsne_result$Y[,1])
pc2 <- matrix(Rtsne_result$Y[,2])
pc3 <- matrix(Rtsne_result$Y[,3])
cc1=gsub("1","gold",labels)
cc2=gsub("0","darkgreen",cc1)
colorlabels <- cc2

#plot3d(pc1, pc2, pc3)
#library(plot3D)
library(igraph)
plot(Rtsne_result$Y, col=cc2)
```


# Run Rtsne ON 3D

```{r}

#write.table(M, file = "kirsch_matrix.csv", append = FALSE, quote = TRUE, sep = ",",row.names=FALSE,col.names = FALSE)
set.seed(6)
ind <- sample(nrow(Q),size=6000,replace=FALSE)
labels <- train$y[ind]
Rtsne_input=Q[ind,]# number of random rows of 60000.
library(Rtsne)
Rtsne_result=Rtsne(Rtsne_input, dims = 3, initial_dims = 30, perplexity = 40,
        theta = 0.1, check_duplicates = TRUE, pca = TRUE, max_iter = 1000,
        verbose = FALSE, is_distance = FALSE)

#Rtsne_result000 <- read.csv("darkenmatlabtsneoutput.csv",header = FALSE)

pc1 <- matrix(Rtsne_result$Y[,1])
pc2 <- matrix(Rtsne_result$Y[,2])
pc3 <- matrix(Rtsne_result$Y[,3])
cc1=gsub("1","gold",labels)
cc2=gsub("0","darkgreen",cc1)
colorlabels <- cc2

#plot3d(pc1, pc2, pc3)
library(plot3D)
library(rgl)
plot3d(pc1,pc2,pc3, )

pc1 <- Rtsne_result000$Y[,1]
pc1 <- Rtsne_result000[,1]
pc1 <- Rtsne_result000[,1]
pc2 <- Rtsne_result000[,2]
pc3 <- Rtsne_result000[,3]
plot3d(pc1,pc2,pc3)
```
# MNIST-mapper-step
---
title: "Mnist (HE)"
author: "Jiyi Jiang"
date: "July 15, 2015"
output: html_document
---

# Load MNIST data

```{r}
#setwd("C:/Users/sumloaner/Downloads")

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('train-images-idx3-ubyte')
  test <<- load_image_file('t10k-images-idx3-ubyte')
  
  train$y <<- load_label_file('train-labels-idx1-ubyte')
  test$y <<- load_label_file('t10k-labels-idx1-ubyte')  
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

load_mnist()
train$n
```

# Histogram equalization(background & noise & darken)
```{r}
library(IM)

unflatten <- function(arr784) {
    return(matrix(arr784, nrow=28)[,28:1])
}

flatten <- function(mat28x28) {
    return(as.vector(mat28x28[,28:1]))
}
Q <-matrix(0, nrow=6000, ncol=784)

for (i in 1:6000){
                digit_flat <- train$x[i,]

# check that unflatten and flatten are inverse functions
# digit_flat2 <- flatten(unflatten(digit_flat))
# sum(digit_flat - digit_flat2)
# show_digit(digit_flat)
# show_digit(digit_flat2)

digit_unflat <- unflatten(digit_flat)
digit_histeq <- histeq( digit_unflat )
digit_histeq_modified <- flatten(digit_histeq)

#digit_histeq_modified [ which(digit_histeq_modified  == min(digit_histeq_modified )) ] <- 0

          
          Average <- sum(digit_histeq_modified)/784
          b <- digit_histeq_modified < Average
          digit_histeq_modified[b] <- 0
          c <- digit_histeq_modified > Average
          digit_histeq_modified[c] <- max(digit_histeq_modified )
          Q[i,] <- digit_histeq_modified

Q[i,] <- digit_histeq_modified

                }



#show_digit(digit_flat)

show_digit(  digit_histeq_modified )
```


# Histogram equalization(background & noise)
```{r}
library(IM)

unflatten <- function(arr784) {
    return(matrix(arr784, nrow=28)[,28:1])
}

flatten <- function(mat28x28) {
    return(as.vector(mat28x28[,28:1]))
}
Q <-matrix(0, nrow=6000, ncol=784)

for (i in 1:6000){
                digit_flat <- train$x[i,]

# check that unflatten and flatten are inverse functions
# digit_flat2 <- flatten(unflatten(digit_flat))
# sum(digit_flat - digit_flat2)
# show_digit(digit_flat)
# show_digit(digit_flat2)

digit_unflat <- unflatten(digit_flat)
digit_histeq <- histeq( digit_unflat )
digit_histeq_modified <- flatten(digit_histeq)

#digit_histeq_modified [ which(digit_histeq_modified  == min(digit_histeq_modified )) ] <- 0

          
          Average <- sum(digit_histeq_modified)/784
          b <- digit_histeq_modified < Average
          digit_histeq_modified[b] <- 0
          #c <- digit_histeq_modified > Average
          #digit_histeq_modified[c] <- max(digit_histeq_modified )
          Q[i,] <- digit_histeq_modified

Q[i,] <- digit_histeq_modified

                }



#show_digit(digit_flat)

show_digit(  digit_histeq_modified )
```
# Run Rtsne on 2D

```{r}
set.seed(2) 
   #M <- train$x[1:6000,]    
#ind <- sample(nrow(train$x),size=6000,replace=FALSE)           
labels <- train$y[1:6000]
Rtsne_input=train$x[1:6000,]# number of random rows of 60000.
library(Rtsne)
Rtsne_result=Rtsne(Rtsne_input, dims = 2, initial_dims = 20, perplexity = 50,
        theta = 0.4, check_duplicates = TRUE, pca = TRUE, max_iter = 1000,
        verbose = FALSE, is_distance = FALSE)


pc1 <- matrix(Rtsne_result$Y[,1])
pc2 <- matrix(Rtsne_result$Y[,2])
pc3 <- matrix(Rtsne_result$Y[,3])
cc1=gsub("1","gold",labels)
cc2=gsub("0","darkgreen",cc1)
colorlabels <- cc2

#plot3d(pc1, pc2, pc3)
#library(plot3D)
library(igraph)
plot(Rtsne_result$Y, col=cc2)
```


# Run Rtsne ON 3D

```{r}

#write.table(M, file = "kirsch_matrix.csv", append = FALSE, quote = TRUE, sep = ",",row.names=FALSE,col.names = FALSE)
set.seed(6)
ind <- sample(nrow(Q),size=6000,replace=FALSE)
labels <- train$y[ind]
Rtsne_input=Q[ind,]# number of random rows of 60000.
library(Rtsne)
Rtsne_result=Rtsne(Rtsne_input, dims = 3, initial_dims = 30, perplexity = 40,
        theta = 0.1, check_duplicates = TRUE, pca = TRUE, max_iter = 1000,
        verbose = FALSE, is_distance = FALSE)

#Rtsne_result000 <- read.csv("darkenmatlabtsneoutput.csv",header = FALSE)

pc1 <- matrix(Rtsne_result$Y[,1])
pc2 <- matrix(Rtsne_result$Y[,2])
pc3 <- matrix(Rtsne_result$Y[,3])
cc1=gsub("1","gold",labels)
cc2=gsub("0","darkgreen",cc1)
colorlabels <- cc2

#plot3d(pc1, pc2, pc3)
library(plot3D)
library(rgl)
plot3d(pc1,pc2,pc3, )

pc1 <- Rtsne_result000$Y[,1]
pc1 <- Rtsne_result000[,1]
pc1 <- Rtsne_result000[,1]
pc2 <- Rtsne_result000[,2]
pc3 <- Rtsne_result000[,3]
plot3d(pc1,pc2,pc3)
```
