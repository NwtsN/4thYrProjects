############ Q1.(i) ############
wine=read.csv("winequality-red.csv",stringsAsFactors = TRUE,sep = ";")
str(wine)
summary(wine)
head(wine)
wine$quality=as.factor(wine$quality)
dim(wine)

ggplot(wine) + 
  geom_bar(aes(x=quality)) + 
  xlab("Wine Quality")

require(patchwork)
require(GGally)

ggcorr(wine)
ggpairs(wine)
cor(wine)

# outlier numbers

outliers_list <- lapply(wine, function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- IQR(x)
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(x[x < lower_bound | x > upper_bound])
})

# To check the number of outliers for each variable
num_outliers=sapply(outliers_list, length)
round((num_outliers/1599)*100,digits = 2)

# correlation coefficients

cor_matrix=cor(wine)
par(mfrow=c(1,1),)
library(corrplot)
first_letters=c("FA","VA","CA","RS","C","FSD","TSD","D","pH","S","A","Q")
corrplot(cor_matrix, method = "square",tl.pos = "n")
text(x = seq_along(first_letters), y = 0.5, labels = first_letters, pos = 1, col = "black")
text(y = seq_along(rev(first_letters)), x=0.2, labels = rev(first_letters), col = "black")

# Correlation coefficients range from -1 to 1.
# A value of 1 indicates a perfect positive linear relationship between variables
# -1 indicates a perfect negative linear relationship
# 0 indicates no linear relationship.
# With respect to quality, if we class any factor with a correlation coefficient ≥|1/5|
# as something we could possibly subset by the factors:
# Volatile Acidity, Citric Acid, Sulphates and Alcohol
# Some other possible clusters based on this plot could be:
  #	Citric Acid Level related cluster
  #	Fixed Acidity Level related cluster
  #	pH Level related cluster
  #	Sulphate Level related cluster
  #	Density related cluster
  #	Quality related cluster
  #	Volatile Acidity related cluster
  #	Alcohol Level related cluster


# boxplots - VS quality

par(mfrow=c(3,4))
boxplot(residual.sugar~quality,data=wine,main="Residual Sugar VS Quality",ylab="Residual Sugar",xlab="Quality")
boxplot(fixed.acidity~quality,data=wine,main="Fixed Acidity VS Quality",ylab="Fixed Acidity",xlab="Quality")
boxplot(volatile.acidity~quality,data=wine,main="Volatile Acidity VS Quality",ylab="Volatile Acidity",xlab="Quality")
boxplot(citric.acid~quality,data=wine,main="Citric Acid VS Quality",ylab="Citric Acid",xlab="Quality")
boxplot(chlorides~quality,data=wine,main="Chlorides VS Quality",ylab="Chlorides",xlab="Quality")
boxplot(free.sulfur.dioxide~quality,data=wine,main="Free Sulphur Dioxide VS Quality",ylab="Free Sulphur Dioxide",xlab="Quality")
boxplot(total.sulfur.dioxide~quality,data=wine,main="Total Sulfur Dioxide VS Quality",ylab="Residual Sugar",xlab="Quality")
boxplot(density~quality,data=wine,main="Density VS Quality",ylab="Density",xlab="Quality")
boxplot(pH~quality,data=wine,main="pH Level VS Quality",ylab="pH",xlab="Quality")
boxplot(sulphates~quality,data=wine,main="Sulphates VS Quality",ylab="Sulphates",xlab="Quality")
boxplot(alcohol~quality,data=wine,main="Alcohol Level VS Quality",ylab="Alcohol",xlab="Quality")

# useful boxplots decided to be:

# volatile acidity vs quality
# citric acid vs quality
# sulphates  vs quality
# alcohol  vs quality

par(mfrow=c(2,2))
boxplot(volatile.acidity~quality,data=wine,main="Volatile Acidity VS Quality",ylab="Volatile Acidity",xlab="Quality")
boxplot(citric.acid~quality,data=wine,main="Citric Acid VS Quality",ylab="Citric Acid",xlab="Quality")
boxplot(sulphates~quality,data=wine,main="Sulphates VS Quality",ylab="Sulphates",xlab="Quality")
boxplot(alcohol~quality,data=wine,main="Alcohol Level VS Quality",ylab="Alcohol",xlab="Quality")






######### clustering#######

install.packages("GGally")
install.packages("cluster")
install.packages("factoextra")

library(GGally)
library(cluster)
library(factoextra)
library(tidyverse)

wine=read.csv("C:/Users/lpb20166/OneDrive - University of Strathclyde/Desktop/Year 4/Semester_1/MS416/IndividualAssignment/TemporaryFilesAndCSV/winequality-red.csv",stringsAsFactors = TRUE,sep = ";")
wine_subset=read.csv("C:/Users/lpb20166/OneDrive - University of Strathclyde/Desktop/Year 4/Semester_1/MS416/SPSS/Assignment/OutputFileAndCSV/wineQualityReds_subset.csv",stringsAsFactors = TRUE,sep = ",")
head(wine)
str(wine)
table(wine$quality)
summary(wine)

wine %>% 
  select(-quality) %>% 
  ggpairs()

?agnes

wine_agglomerative_single_euclidian_non_stand <- agnes(wine[,-12],metric = "euclidean",method = "single",stand = FALSE)
plot(wine_agglomerative_single_euclidian_non_stand,main = "Dendrogram - single linkage, non-standardised",which.plots = 2)

wine_agglomerative_avg_euclidian_non_stand <- agnes(wine[,-12],metric = "euclidean",method = "average",stand = FALSE)
plot(wine_agglomerative_avg_euclidian_non_stand,main = "Dendrogram - average linkage, non-standardised",which.plots = 2)

wine_agglomerative_complete_euclidian_non_stand <- agnes(wine[,-12],metric = "euclidean",method = "complete",stand = FALSE)
plot(wine_agglomerative_complete_euclidian_non_stand,main = "Dendrogram - complete linkage, non-standardised",which.plots = 2)

fviz_nbclust(wine[,-12], FUNcluster = hcut, method="gap_stat",nboot = 20,k.max=11,stand=TRUE, hc_func="agnes", hc_method="average", hc_metric="euclidean")
fviz_nbclust(wine[,-12], FUNcluster = hcut, method="silhouette",k.max=11,stand=TRUE, hc_func="agnes", hc_method="single", hc_metric="euclidean")
fviz_nbclust(wine[,-12], FUNcluster = hcut, method="wss",k.max=11, stand=TRUE, hc_func="agnes", hc_method="single", hc_metric="euclidean")

fviz_nbclust(wine[,-12], FUNcluster = kmeans, method="gap_stat",nboot = 100,k.max=11)
fviz_nbclust(wine[,-12], FUNcluster = kmeans, method="silhouette",k.max=11)
fviz_nbclust(wine[,-12], FUNcluster = kmeans, method="wss",k.max=11)

?kmeans

wine_agglomerative_complete_manhattan_stand <- agnes(wine[,-12],metric = "manhattan",method = "complete",stand = TRUE)
plot(wine_agglomerative_complete_manhattan_stand,main = "Dendrogram - complete linkage, standardised manhattan",which.plots = 2)
wine_agglomerative_complete_manhattan_stand
comp.man.stand.clust=cutree(wine_agglomerative_complete_manhattan_stand,k=6)
wine$clust.membership=as.factor(comp.man.stand.clust)
means <- aggregate(. ~ clust.membership, data = wine, FUN = mean)
medians <- aggregate(. ~ clust.membership, data = wine, FUN = median)
maxima <- aggregate(. ~ clust.membership, data = wine, FUN = max)
minima <- aggregate(. ~ clust.membership, data = wine, FUN = min)
variances <- aggregate(. ~ clust.membership, data = wine, FUN = var)
counts <- aggregate(. ~ clust.membership, data = wine, FUN = length)

wine_agglomerative_single_euclidian_stand <- agnes(wine[,-12],metric = "euclidean",method = "single",stand = TRUE)
wine_agglomerative_complete_euclidian_stand <- agnes(wine[,-12],metric = "euclidean",method = "complete",stand = TRUE)


p1=plot(wine_agglomerative_single_euclidian_stand,main = "Dendrogram - single linkage, standardised",which.plots = 2)
p2=plot(wine_agglomerative_avg_euclidian_stand,main = "Dendrogram - average linkage, standardised",which.plots = 2)
p3=plot(wine_agglomerative_complete_euclidian_stand,main = "Dendrogram - complete linkage, standardised",which.plots = 2)

k_2=hcut(wine[,-12], 
     k = 2, 
     hc_func = "agnes", 
     hc_method = "average", 
     hc_metric = "euclidean", 
     stand = TRUE)
k_3=hcut(wine[,-12], 
     k = 3, 
     hc_func = "agnes", 
     hc_method = "average", 
     hc_metric = "euclidean", 
     stand = TRUE)
k_4=hcut(wine[,-12], 
         k = 4, 
         hc_func = "agnes", 
         hc_method = "average", 
         hc_metric = "euclidean", 
         stand = TRUE)
k_5=hcut(wine[,-12], 
         k = 5, 
         hc_func = "agnes", 
         hc_method = "average", 
         hc_metric = "euclidean", 
         stand = TRUE)
k_6=hcut(wine[,-12], 
         k = 6, 
         hc_func = "agnes", 
         hc_method = "average", 
         hc_metric = "euclidean", 
         stand = TRUE)
p_1=fviz_cluster(k_21, data = wine[,-12], stand = TRUE, 
             main = "Cluster Plot - quality of having 2 clusters (avg linkage, euclidean, standardised)")
p_2=fviz_cluster(k_31, data = wine[,-12], stand = TRUE, 
             main = "Cluster Plot - quality of having 3 clusters (avg linkage, euclidean, standardised)")
p_3=fviz_cluster(k_41, data = wine[,-12], stand = TRUE, 
             main = "Cluster Plot - quality of having 4 clusters (avg linkage, euclidean, standardised)")
p_4=fviz_cluster(k_51, data = wine[,-12], stand = TRUE, 
             main = "Cluster Plot - quality of having 5 clusters (avg linkage, euclidean, standardised)")
p_5=fviz_cluster(k_61, data = wine[,-12], stand = TRUE, 
             main = "Cluster Plot - quality of having 6 clusters (avg linkage, euclidean, standardised)")
library(gridExtra)
grid.arrange(p_1,p_11, p_2, p_21,p_3, p_31, p_4, p_41,  p_5,  p_51, ncol = 3)
?fviz_cluster

fviz_nbclust(wine[,-12], 
             FUNcluster = hcut(wine[,-12], 
                               k = 3, 
                               hc_func = "agnes", 
                               hc_method = "average", 
                               hc_metric = "euclidean", 
                               stand = TRUE), 
             method="wss",k.max=11)
?fviz_nbclust
?hcut
