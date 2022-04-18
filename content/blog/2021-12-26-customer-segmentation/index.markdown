---
title: Customer Segmentation
layout: single-sidebar
date: '2021-12-26'
categories:
  - Machine Learning
  - Marketing
  - Business Analytics
tags:
  - R
  - Marketing
slug: segmentation-kmeans
subtitle: Using K-mean algorithm
summary: In this post, we perform a customer segmentation in R programming using k-means algorithm. We used a Shopping mall customers dataset to implement it.
lastmod: 2021-03-31
featured: yes
draft: no
image:
  placement: 1
  caption: '[Illustration from starline on Freepik](featured.jpg)'
  focal_point: Center
  preview_only: no
output:
  blogdown::html_page:
    toc: yes
    number_sections: no
    toc_depth: 1
links:
  - icon: github
    icon_pack: fab
    name: post materials
    url: https://github.com/hhousni/Customer_segmentation
---




## Introduction

My life is composed by different groups, my family, my friends, my co-workers, my clients. etc. Each segment has their needs and their expectations. To satisfy them, I set up different forms of communication and interaction.
Companies face the same challenges as we do. They communicate and interact with different customers. To succeed, they have to be able to identify groups of people that have similar characteristics among their customers to: 

- Better understand and target them
- Gain competitive advantage
- Determine new market opportunities 
- Focus on the most profitable segment
- Upsell and cross-sell other products and services

**Customer segmentation** is the practice of dividing customer into groups based on similar characteristics. In this post, we perform a customer segmentation in R programming using **k-mean** algorithm. We use a Shopping mall customers data set to implement it.

## Data Preparation and Exploration

**Data prep**

To perform a cluster analysis, the data set should be prepared as follows: 

- Columns should be the variables and Row the observations 
- The data set should not contain any missing values or blank. Otherwise, they should be removed or estimated 
- The data should be standardized in order to make the variables comparable. 

** Data Loading and prepration**


```r
# The Pacman package is used as package manager

if(!require("pacman")) install.packages("pacman")

# Load the packages

p_load(tidyverse,factoextra)

# Load the data set 

data <- read.csv("https://raw.githubusercontent.com/hhousni/Customer_segmentation/main/Mall_Customers.csv")
```




```r
str(data)
```

```
## 'data.frame':	200 obs. of  5 variables:
##  $ CustomerID            : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Gender                : chr  "Male" "Male" "Female" "Female" ...
##  $ Age                   : int  19 21 20 23 31 22 35 23 64 30 ...
##  $ Annual.Income..k..    : int  15 15 16 16 17 17 18 18 19 19 ...
##  $ Spending.Score..1.100.: int  39 81 6 77 40 76 6 94 3 72 ...
```

The data set contains 200 observations and 5 variables:

- CustomerID: The unique customer's ID
- Gender: customer's gender 
- Age: customer's age  
- Annual Income (k$): Customer's annual income 
- Spending Score: Score given to the customer based on the money spent and the customer's behavior.  


Checking, if there are missing values in the dataset. 

```r
#Find the number of NAs in each variable 
lapply(data,function(x) {length(which(is.na(x)))})
```

```
## $CustomerID
## [1] 0
## 
## $Gender
## [1] 0
## 
## $Age
## [1] 0
## 
## $Annual.Income..k..
## [1] 0
## 
## $Spending.Score..1.100.
## [1] 0
```

```r
#Find the number of blank in each variable
lapply(data, function(x) {length(which((x=="")))})
```

```
## $CustomerID
## [1] 0
## 
## $Gender
## [1] 0
## 
## $Age
## [1] 0
## 
## $Annual.Income..k..
## [1] 0
## 
## $Spending.Score..1.100.
## [1] 0
```

The data set does not contain missing values as both NAs and blank spaces.  

**Exploratory Analysis**


```r
head(data)
```

```
##   CustomerID Gender Age Annual.Income..k.. Spending.Score..1.100.
## 1          1   Male  19                 15                     39
## 2          2   Male  21                 15                     81
## 3          3 Female  20                 16                      6
## 4          4 Female  23                 16                     77
## 5          5 Female  31                 17                     40
## 6          6 Female  22                 17                     76
```

**Gender Visualization** 


```r
p_load(ggplot2)

ggplot(data, aes(Gender, fill=Gender)) +
  geom_bar(alpha=.4, col='black') + 
  ggtitle("Gender Comparision") + 
  xlab("Gender") + 
  ylab("Number of Customer")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />


Woman represent the majority of the Shopping mall customers. 




```r
summary(data$Age)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   18.00   28.75   36.00   38.85   49.00   70.00
```

```r
ggplot(data, aes(Age)) + geom_histogram(binwidth = 5, breaks=seq(15,70, by=5),
                                        col="black",
                                        fill=I('blue'),
                                        alpha=.2)  + 
  labs(title = "Age distribution", x="Age",y="Count")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

Customers age is between 18 to 70 years old. The average customer is 38.85 years old and the median is 36 years old. The majority of customer are between 20 to 50 years old. 



```r
summary(data$Annual.Income..k..)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   15.00   41.50   61.50   60.56   78.00  137.00
```

```r
ggplot(data, aes(Annual.Income..k..)) + geom_histogram(binwidth = 5, breaks=seq(10,140, by=10),
                                        col="black",
                                        fill=I('blue'),
                                        alpha=.2) +
  labs(title = "Annual Income Distribution", x="Annual Income Class",y="Frequency")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

In average, the customers earn 60.56 k(\$) per year, the minimum income is 15k(\$) per year and the maximum is 137 k(\$) per year. The major part of customers earn less than 90 k(\$) per year.  



```r
summary(data$Spending.Score..1.100.)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00   34.75   50.00   50.20   73.00   99.00
```

```r
ggplot(data, aes(Spending.Score..1.100.)) + geom_histogram(binwidth = 5, breaks=seq(0,100, by=10),
                                        col="black",
                                        fill=I('blue'),
                                        alpha=.2) +
  labs(title = "Spending Score distribution", x="Spending Score Class",y="Frequency")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />


## Implementation of k-means


**Algorithm description** 


**K-means clustering** is an unsupervised machine learning algorithm that splits unlabeled dataset into a number of pre-defined clusters (k) according to their properties. The goal of the k-means algorithm is to minimize the sum of distance between the data point and their corresponding cluster. 

k-means algorithm works as follow:

1. Decide the number of cluster (k)
2. Select a number of K centroids
3. Assign for each data point to their closer centroid by using distance measurement such as Euclidean or Manhattan distance.
4. For Every centroid, move the centroid to the average of the point assigned to that centroid.
5. Repeat step 3 and 4 until the centroids assignment no longer changes.

**To perform the k-means algorithm, the number of cluster has to be specified in advance. Different methods can be used to select the optimal cluster number**. 

*Elbow method* 

- This method runs k-means clustering for a range of values  (for instance, by varying k from 1 to 10 ) 
- For each value of K calculate the total within-cluster sum of square
- Plot the curve of total within-cluster sum of square 
- The location of a bend in the plot (knee) is an indicator of the number of clusters
    
*Silhouette method*

- The average silhouette method calculates the mean of silhouette observations for different k values. With the optimal number of k clusters, one can maximize the average silhouette over significant values for k clusters.
    
*Gap statistic*

- The gap statistic compares the total within intra-cluster variation for different values of k with their expected values under null reference distribution of the data. The estimate of the optimal clusters will be value that maximize the gap statistic. 

**Slecting the Optimum Number of clusters** 

*Elbow method* 


```r
p_load(purrr)
# function to calculate total intra-cluster sum of square
set.seed(123)
p_load(cluster,gridExtra,grid, factoextra)
fviz_nbclust(data[,3:5], kmeans, method = "wss") + geom_vline(xintercept = 4, linetype=2) + labs(subtitle = "Elbow method")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

The above graph seems to show a bend on the cluster number 4. We can conclude that 4 is the appropriate numbers of clusters. 

*Silhouette method*


```r
set.seed(123)
fviz_nbclust(data[,3:5], kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

According to the silhouette method, 6 is the optimal number of cluster.  

*Gap statistic method*


```r
set.seed(123)
fviz_nbclust(data[,3:5], kmeans, nstart=25, method = "gap_stat", nboot = 50) + labs(subtitle = "Gap statistic method")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

According to the Gap statistic method, 6 is the optimal number of cluster. 

**Two among the three different methods of selecting the optimal number of cluster concluded that 6 is the optimal number of cluster. To implement the K-mean algorithm we are going to use 6 clusters.**

## Results 


```r
set.seed(123)
result <- kmeans(data[,3:5],6,iter.max = 100, nstart = 50, algorithm="Lloyd")
result
```

```
## K-means clustering with 6 clusters of sizes 45, 21, 35, 39, 38, 22
## 
## Cluster means:
##        Age Annual.Income..k.. Spending.Score..1.100.
## 1 56.15556           53.37778               49.08889
## 2 44.14286           25.14286               19.52381
## 3 41.68571           88.22857               17.28571
## 4 32.69231           86.53846               82.12821
## 5 27.00000           56.65789               49.13158
## 6 25.27273           25.72727               79.36364
## 
## Clustering vector:
##   [1] 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2 6 2
##  [38] 6 2 6 1 6 1 5 2 6 1 5 5 5 1 5 5 1 1 1 1 1 5 1 1 5 1 1 1 5 1 1 5 5 1 1 1 1
##  [75] 1 5 1 5 5 1 1 5 1 1 5 1 1 5 5 1 1 5 1 5 5 5 1 5 1 5 5 1 1 5 1 5 1 1 1 1 1
## [112] 5 5 5 5 5 1 1 1 1 5 5 5 4 5 4 3 4 3 4 3 4 5 4 3 4 3 4 3 4 3 4 5 4 3 4 3 4
## [149] 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4 3
## [186] 4 3 4 3 4 3 4 3 4 3 4 3 4 3 4
## 
## Within cluster sum of squares by cluster:
## [1]  8062.133  7732.381 16690.857 13972.359  7742.895  4099.818
##  (between_SS / total_SS =  81.1 %)
## 
## Available components:
## 
## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
## [6] "betweenss"    "size"         "iter"         "ifault"
```

**Visualizing the clustering Results using Principle components analysis**


```r
pcclust <- prcomp(data[,3:5], scale=FALSE) 
summary(pcclust)
```

```
## Importance of components:
##                            PC1     PC2     PC3
## Standard deviation     26.4625 26.1597 12.9317
## Proportion of Variance  0.4512  0.4410  0.1078
## Cumulative Proportion   0.4512  0.8922  1.0000
```


```r
pcclust$rotation[,1:2]
```

```
##                               PC1        PC2
## Age                     0.1889742 -0.1309652
## Annual.Income..k..     -0.5886410 -0.8083757
## Spending.Score..1.100. -0.7859965  0.5739136
```

```r
set.seed(1)
ggplot(data,aes(x=Annual.Income..k.., y=Spending.Score..1.100.)) +
  geom_point(stat='identity', aes(color=as.factor(result$cluster))) +
  scale_color_discrete(name=' ',
                        breaks=c('1','2','3','4','5','6'),
                        labels=c('Cluster 1','Cluster 2','Cluster 3','Cluster 4','Cluster 5','Cluster 6')) +
  ggtitle('Segments of Mall Customers')
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />

The visualization of the clusters allows to observe five facts:

1. **Cluster 1 and 5** represent the medium customer in term of spending score and annual income. 

2. **Cluster 2** is regrouping the customers with low income and high spending score.

3. **Cluster 3** is regrouping the customers with low income and low spending score.

4. **Cluster 4** is regrouping the customers with high income and high spending score. 

5. **Cluster 6** is regrouping the customers with high income and low spending score.


## Conclusion 

In this post we wanted to perform a customer segmentation in R programming using k-mean algorithm. We used a Shopping mall customers data set to implement it. By using the function ***kmeans()*** from the stats package, we found that the Shopping mall customers can be split in 6 clusters. A principal component analysis allowed to have a better understanding of the clusters. We used the ***fviz_cluster()*** from the factoextra package to visualize the results. 
