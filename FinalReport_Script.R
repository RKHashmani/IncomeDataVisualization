
## ---------------------------------------------------------------------------------------------------------------------------------------
# All the Libraries we need
library("knitr")
library(plyr)
library(dplyr)
library(ggbiplot)
library(smacof)
library(MASS)
library(cluster)
library(purrr)
library(dendextend)
library(factoextra)
# For k-medoid
library(cluster)
# SOM
library(kohonen)
# For Validation
library(fpc)
library(clValid)
library(mclust)
library(NbClust)
# For nonlinear projection
library(umap)
library(Rtsne)

## ----Train-peek,-----------------------------------------------------------------------------------------------------------------------
census_train <- read.csv("Data/census/adult.data.csv", header=TRUE, sep = ",")
census_test <- read.csv("Data/census/adult.test.csv", header=TRUE, sep = ",")

kable(census_train[0:8,0:7], align = "l", row.names = TRUE,
      caption = "The First 7 Columns.", booktabs = T)
kable(census_train[0:8,8:15], align = "l", row.names = TRUE,
      caption = "The Remaining 8 Columns.", booktabs = T)
## ---------------------------------------------------------------------------------------------------------------------------------------
samples_train <- nrow(census_train)
features_train <- ncol(census_train)

samples_test <- nrow(census_test)
features_test <- ncol(census_test)

sprintf(paste0("The total number of samples in the training set is %s and the number of ",
               "features is %s."), samples_train, features_train)
sprintf(paste0("The total number of samples in the test set is %s and the number of ",
               "features is %s."), samples_test, features_test)

## -----------------------------------------------------------------------------------------------------------------------------------
census_train %>% summarise_all(n_distinct)
census_train %>% group_by(income) %>% summarize(count=n())
census_test %>% summarise_all(n_distinct)
census_test %>% group_by(income) %>% summarize(count=n())

## ----combined-table, tidy=FALSE---------------------------------------------------------------------------------------------------------
census_total <- rbind(census_train, census_test)

sprintf(paste0("The number of samples in the total set is %s and the number of ",
               "features is %s."), samples_total, features_total)

census_total %>% summarise_all(n_distinct)

range <- data.frame(min=sapply(census_total,min),max=sapply(census_total,max))
kable(range, align = "l", row.names = TRUE,
      caption = "The Range of Values for all Features", booktabs = T)

# To see the unique classes in the final feature, "income".
vec <- as.vector(census_total['income'])
unique(vec)

## ----Data-matrix-orig-------------------------------------------------------------------------------------------------------------------
data_matrix_orig <- data.matrix(census_total)

kable(data_matrix_orig[0:8,0:7], align = "l", caption = "The First 7 Columns.", booktabs = T)
kable(data_matrix_orig[0:8,8:15], align = "l", caption = "The Remaining 8 Columns.", booktabs = T)

data_matrix <- data_matrix_orig[,-c(15)]

## ---------------------------------------------------------------------------------------------------------------------------------------
# Removing Missing Values
df_no_missing <- census_total[!(census_total$workclass==" ?" | 
                                  census_total$occupation==" ?" | 
                                  census_total$native.country ==" ?"),]
# Removing duplicate rows
data_frame_orig <- df_no_missing[!duplicated(df_no_missing), ]
data_frame <- data_frame_orig
# Removing 2 superfluous variables, Final Weight and Categorical Education
data_frame <- data_frame[,-c(3, 4)]
frame_no_outlier <- data_frame

## ---------------------------------------------------------------------------------------------------------------------------------------
# Interquartile Method for Outlier Removal
############ For Age

#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(frame_no_outlier$age, .25)
Q3 <- quantile(frame_no_outlier$age, .75)
IQR <- IQR(frame_no_outlier$age)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
frame_no_outlier <- subset(frame_no_outlier, frame_no_outlier$age>= (Q1 - 1.5*IQR) & frame_no_outlier$age<= (Q3 + 1.5*IQR))

############# For Education

#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(frame_no_outlier$education.num, .25)
Q3 <- quantile(frame_no_outlier$education.num, .75)
IQR <- IQR(frame_no_outlier$education.num)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
frame_no_outlier <- subset(frame_no_outlier, frame_no_outlier$education.num>= (Q1 - 1.5*IQR) & frame_no_outlier$education.num<= (Q3 + 1.5*IQR))

############# For capital gain

#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(frame_no_outlier$capital.gain, .25)
Q3 <- quantile(frame_no_outlier$capital.gain, .75)
IQR <- IQR(frame_no_outlier$capital.gain)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
frame_no_outlier <- subset(frame_no_outlier, frame_no_outlier$capital.gain>= (Q1 - 1.5*IQR) & frame_no_outlier$capital.gain<= (Q3 + 1.5*IQR))

############# For capital loss

#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(frame_no_outlier$capital.loss, .25)
Q3 <- quantile(frame_no_outlier$capital.loss, .75)
IQR <- IQR(frame_no_outlier$capital.loss)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
frame_no_outlier <- subset(frame_no_outlier, frame_no_outlier$capital.loss>= (Q1 - 1.5*IQR) & frame_no_outlier$capital.loss<= (Q3 + 1.5*IQR))

############# For hours per week

#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(frame_no_outlier$hours.per.week, .25)
Q3 <- quantile(frame_no_outlier$hours.per.week, .75)
IQR <- IQR(frame_no_outlier$hours.per.week)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
frame_no_outlier <- subset(frame_no_outlier, frame_no_outlier$hours.per.week>= (Q1 - 1.5*IQR) & frame_no_outlier$hours.per.week<= (Q3 + 1.5*IQR))

# Remove the zeroed columns of income gain/loss
data_full_clean <- frame_no_outlier[,-c(9, 10)]

### SAMPLING ###
clean_sample <- data_full_clean %>% group_by(income) %>% sample_n(2500)

## # To save the selected sample dataset, for reproducibility.
write.csv(temp ,'5000framev1.csv', row.names = FALSE, col.names = TRUE)

# To read the sampled data
clean_sample <- read.csv("data/5000framev1.csv")

sample <- scale(data.matrix(clean_sample))

## ----Boxplots---------------------------------------------------------------------------------------------------------------------------
# SCALED BOXPLOT
data_frame_box <- data_frame[,-c(13)]
data_full_clean_box <- data_full_clean[,-c(11)]
sample_box <- sample[,-c(11)]

par(mar= c(7, 4.1, 4.1, 2.1))
boxplot(scale(data.matrix(data_frame_box)), main = "Boxplot for the Original Dataset", las=2)
boxplot(scale(data.matrix(data_full_clean_box)), las=2, main = "Boxplot for the Original Dataset (Outliers Removed)")
boxplot(sample_box, las=2, main = "Boxplot for the Sampled Dataset (Outliers Removed)")
par(mar= c(5.1, 4.1, 4.1, 2.1))

## ---------------------------------------------------------------------------------------------------------------------------------------
# To Remove Class Labels
data_matrix_orig_mds <- sample
data_matrix_mds <- data_matrix_orig_mds[,-c(11)]
sample_number <- nrow(data_matrix_mds)
print(paste0("The number of samples selected is: ", sample_number))

## ----Data-prep-clus, cache=TRUE---------------------------------------------------------------------------------------------------------

#Same pre-processing
data_matrix_clus <- data_matrix_orig_mds[,-c(11)]
sample_number <- nrow(data_matrix_clus)
print(paste0("The number of samples selected is: ", sample_number))

# Setting Seed to allow reproducible results
set.seed(786) # Setting Seed

data_matrix_scaled <- data_matrix_clus # It's already scaled above.

### PCA FOR FULL DATASET ###

## ----cov-matrix, tidy=FALSE-------------------------------------------------------------------------------------------------------------
cov_matrix<- cov(data_matrix)

kable(cov_matrix[0:5,0:5], align = "l", row.names = TRUE,
      caption = "Covariance Matrix for the First 5 Features",
      booktabs = T)

eigen_info <- eigen(cov_matrix)
eigen_info

pca_data <- prcomp(data_matrix, center = TRUE, scale. = TRUE, retx = TRUE)
summary(pca_data)
pca_data # To print the full PCA data.

## ----screeplot, fig.cap="Scree Plot of the Principal Components."-----------------------------------------------------------------------
plot(pca_data$sdev ^2, xlab="Principal Component", ylab="Proportion of Variance",
     type='b', las=1, main = "Scree Plot")

## ----screeplot-norm, fig.cap="Scree Plot of the Principal Components, normalized."------------------------------------------------------
pca_data_sdev2 <- pca_data$sdev ^ 2 #Compute Variance
pca_data_normalized <- pca_data_sdev2/sum(pca_data_sdev2) #Compute Proportion of Variance
plot(pca_data_normalized,xlab="Principal Component", ylab="Proportion of Variance",
ylim=c(0,1), type='b', las=1, main = "Normalized Scree Plot")
axis(2,at=c(0.1, 0.3, 0.5, 0.7, 0.9), las=1) # To add additional ticks

## ----biplot, tidy=FALSE, fig.height=10, fig.width=10, fig.cap="Plotting all the datapoints on the first 2 Principal Components."--------

#I created a function, my_ggbiplot, to edit the color scheme of the ggbiplot:
source("my_functions.R")
pca_plot<-my_ggbiplot(pca_data, var.axes=TRUE, groups = data_matrix_orig[,c(15)])

# To make the arrows more visible
pca_plot$layers <- c(pca_plot$layers, pca_plot$layers[[1]])
pca_plot + 
  labs(color="Income", # Labeling the Legend
       title = "Plotting Datapoints on the First 2 Principal Components") +
  theme(plot.title = element_text(hjust = 0.5),
                     plot.caption=element_text(hjust = 0))

### PCA FOR SAMPLED DATA ###

## ----PCA-scree, tidy=FALSE, fig.cap="Scree Plot of the Principal Components using the sampled data, normalized."------------------------
pca_data <- prcomp(data_matrix_mds, center = TRUE, scale. = TRUE, retx = TRUE)

summary(pca_data)

# Plotting Normalized Scree Plot
pca_data_sdev2 <- pca_data$sdev ^ 2 #Compute Variance
pca_data_normalized <- pca_data_sdev2/sum(pca_data_sdev2) #Compute Proportion of Variance
plot(pca_data_normalized,xlab="Principal Component", ylab="Proportion of Variance",
ylim=c(0,1), type='b', las=1, main = "Normalized Scree Plot for the Sampled Data")
axis(2,at=c(0.1, 0.3, 0.5, 0.7, 0.9), las=1) # To add additional ticks

## ----PCA-sample, tidy=FALSE, fig.height=10, fig.width=10, fig.cap="Plot of the first 2 principal components using our sampled data."----
# Plotting the first 2 principal components
pca_plot<-my_ggbiplot(pca_data, var.axes=TRUE, groups = clean_sample[,c(11)])

# To make the arrows more visible
pca_plot$layers <- c(pca_plot$layers, pca_plot$layers[[1]])

pca_plot + 
  labs(color="Income", # Labeling the Legend
  title = "Plotting Samples on the First 2 Principal Components for the Sampled Data") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption=element_text(hjust = 0))


### MDS ###

dist_matrix <- dist(data_matrix_mds, method="euclidean")
dist_matrix_display <- as.matrix(dist_matrix)
kable(dist_matrix_display[0:8,0:8], align = "l", row.names = TRUE,
      caption = "Distance Matrix for the First 8 Features",
      booktabs = T)

## ----Classic----------------------------------------------------------------------------------------------------------------
classic_mds <- cmdscale(dist_matrix, k = 2)
## ----classic-MDS,  fig.cap="Plot of Classic Torgerson’s Metric MDS applied on the distance matrix."-------------------------------------
plot(classic_mds, main = "Classic Torgerson's MDS")

## ----Sammon-process, cache=TRUE---------------------------------------------------------------------------------------------------------
samm_mds_classic <- sammon(dist_matrix, y = classic_mds, k=2, niter = 50 )

## ----Sammon-classic, tidy=FALSE, fig.cap="Plot of Sammon's non-linear mapping using classic MDS as the initialization."-----------------
print(paste0("Stress when using Classic_mds as initialization: ",
             samm_mds_classic$stress))
plot(samm_mds_classic$points,
     xlab="Samm_mds[,1]",
     ylab="Samm_mds[,2]",
     main = paste0("Sammon's MDS Mapping (Classic MDS as Initialization)")
     )

## ----Sammon-loop,  cache=TRUE, tidy=FALSE-----------------------------------------------------------------------------------------------
min_stress_samm <- .Machine$double.xmax # Largest double value the computer can store
for(i in 1:10) {
  print(paste0("Iteration Number ", i,":"), i)
  rand_init <- matrix(rnorm(sample_number*2, 0, 100000), nrow = sample_number, ncol = 2)
  samm_mds <- sammon(dist_matrix, rand_init, k=2, niter = 50 )

  if (samm_mds$stress < min_stress_samm){
    best_samm_mds <- samm_mds
    min_stress_samm <- samm_mds$stress
    best_samm_i <- i
  }
}

## ----Sammon-random, tidy=FALSE, fig.cap="Plot of Sammon's non-linear mapping using the best found random initialization."---------------
print(paste0("Best Initialization's iteration number : ", best_samm_i))
print(paste0("Best Initialization's Stress: ", best_samm_mds$stress))
plot(best_samm_mds$points,
         xlab="Best Random initialization's Samm_mds[,1]",
         ylab="Best Random Initialization's Samm_mds[,2]",
         main = paste0("Sammon's MDS Mapping (Best Random Initialization)"))

## ----Kruskal-process, cache=TRUE--------------------------------------------------------------------------------------------------------
kruskal_mds_classic <- isoMDS(dist_matrix, y = classic_mds, k=2, maxit = 10 )

## ----Kruskal-classic, fig.cap="Plot of Kruskal's non-linear mapping using classic MDS as initialization."-------------------------------
print(paste0("Stress when using classic_mds as initialization: ",
             kruskal_mds_classic$stress))
plot(kruskal_mds_classic$points,
     xlab="kruskal_mds[,1]",
     ylab="kruskal_mds[,2]",
     main = paste0("Kruskal's MDS Mapping (Classic MDS as Initialization)")
     )

## ----Kruskal-loop, cache=TRUE, tidy=FALSE-----------------------------------------------------------------------------------------------
min_stress_kruskal <- .Machine$double.xmax # Largest double value the computer can store

for(i in 1:10) {
  print(paste0("Iteration Number ", i,":"), i)
  rand_init <- matrix(rnorm(sample_number*2, 0, 100000), nrow = sample_number, ncol = 2)
  kruskal_mds <- isoMDS(dist_matrix, rand_init, k=2, maxit = 10 )

  if (kruskal_mds$stress < min_stress_kruskal){
    best_kruskal_mds <- kruskal_mds
    min_stress_kruskal <- kruskal_mds$stress
    best_kruskal_i <- i
  }
}

## ----Kruskal-random, tidy=FALSE, fig.cap="Plot of Kruskal’s Non-metric MDS mapping using the best found random initialization."---------
print(paste0("Best Initialization's iteration number : ", best_kruskal_i))
print(paste0("Best Initialization's Stress: ", best_kruskal_mds$stress))
plot(best_kruskal_mds$points,
         xlab="Best Random initialization's kruskal_mds[,1]",
         ylab="Best Random Initialization's kruskal_mds[,2]",
         main = paste0("Kruskal's MDS Mapping (Best Random Initialization)"))

## ----Smacof-process, cache=TRUE---------------------------------------------------------------------------------------------------------
smacof_mds_classic <- smacofSym(dist_matrix, ndim = 2, init = "torgerson", itmax = 1000 )

## ----Smacof-classic, tidy=FALSE, fig.cap="Plot of Symmetric Smacof non-linear mapping using classic MDS as initialization. "------------
print(paste0("Stress when using torgerson's classic MDS as initialization: ",
             smacof_mds_classic$stress))
plot(smacof_mds_classic$conf,
     xlab="smacofSym_mds[,1]",
     ylab="smacofSym_mds[,2]",
     main = paste0("Symmetric Smacof MDS (Classic MDS as Initialization)")
     )


## ----Smacof-loop, cache=TRUE, tidy=FALSE------------------------------------------------------------------------------------------------
min_stress_smacof <- .Machine$double.xmax # Largest double value the computer can store
for(i in 1:10) {
  print(paste0("Iteration Number ", i,":"), i)
  smacof_mds <- smacofSym(dist_matrix, ndim = 2, init = "random", itmax = 1000 )

  if (smacof_mds$stress < min_stress_smacof){
    best_smacof_mds <- smacof_mds
    min_stress_smacof <- smacof_mds$stress
    best_smacof_i <- i
  }
}

## ----Smacof-random, tidy=FALSE, fig.cap="Plot of Symmetric Smacof MDS mapping using the best found random initialization."--------------
print(paste0("Best Initialization's iteration number : ", best_smacof_i))
print(paste0("Best Initialization's Stress: ", best_smacof_mds$stress))
plot(best_smacof_mds$conf,
         xlab="Best Random initialization's smacofSym_mds[,1]",
         ylab="Best Random Initialization's smacofSym_mds[,2]",
         main = paste0("Symmetric Smacof MDS Mapping (Best Random Initialization)"))

### FOR UMAP ###

dataLabels = clean_sample[,11] %>%
  unlist %>%
  factor(labels = c("<=50K",">50K"))

sample_UMAP <- data.matrix(clean_sample)
sample_UMAP <- sample_UMAP[,-c(11)]

## ----UMAP-Param-Training----------------------------------------------------------------------------------------------------

min_dist_1 = seq(0.001,0.2,0.05)
min_dist_2 = seq(0.1,0.5,0.05)
min_dist = c(min_dist_1, min_dist_2)
c = 0
for (n_neig in seq(5,50,5)) { # 10 Iterations
  for (min_distance in min_dist) { # 13 Iterations
    UMAP_Data = umap(sample_UMAP, n_neighbors = n_neig, min_dist = min_distance)
    df = data.frame(X = UMAP_Data[,1],
                    Y = UMAP_Data[,2],
                    Labels = dataLabels)
    plot = ggplot(data = df, aes(x = X ,y = Y, col = Labels)) +
      geom_point()
    c = c + 1
    # To save the multiple plots
    fileName = paste("ID",c,"N_N", n_neig, "M_D", min_distance,".png")
    png(fileName)
    print(plot)
    dev.off()
  }
}

## ----UMAP, cache=TRUE-------------------------------------------------------------------------------------------------------------------
UMAP_5000 = umap(sample_UMAP, n_neighbors = 30, min_dist = 0.15)
df = data.frame(X = UMAP_5000$layout[,1],
                Y = UMAP_5000$layout[,2],
                Labels = dataLabels)

## ----UMAP-Plot--------------------------------------------------------------------------------------------------------------------------
plot = ggplot(data = df, aes(x = X ,y = Y, col = Labels)) +
  geom_point() +
  ggtitle("UMAP Plot") +
  theme(plot.title = element_text(hjust = 0.5))
print (plot)

## ----TSNE-param-train, eval=FALSE-------------------------------------------------------------------------------------------------------
c = 0
for (per in seq(1,400,5)) { # 50 Iterations

    tSNE_Data = Rtsne(sample_UMAP,perplexity = per)
    df = data.frame(X = tSNE_Data$Y[,1],
                    Y = tSNE_Data$Y[,2],
                    Labels = dataLabels)
    plot = ggplot(data = df, aes(x = X ,y = Y, col = Labels)) +
      geom_point()
    c = c + 1
    # To save the multiple plots
    fileName = paste("ID",c,"Perplexity", per,".png")
    png(fileName)
    print(plot)
    dev.off()

}

## ----TSNE, cache=TRUE-------------------------------------------------------------------------------------------------------------------
tSNE_Data = Rtsne(sample_UMAP,perplexity = 81)
    df_tsne = data.frame(X = tSNE_Data$Y[,1],
                    Y = tSNE_Data$Y[,2],
                    Labels = dataLabels)

## ----TSNE-Plot--------------------------------------------------------------------------------------------------------------------------
plot_tsne = ggplot(data = df_tsne, aes(x = X ,y = Y, col = Labels)) +
  geom_point() +
  ggtitle("t-SNE Plot") +
  theme(plot.title = element_text(hjust = 0.5))
print (plot_tsne)

### For Hierarchical Clustering ###

dist_matrix_scaled <- dist(data_matrix_scaled, method = "euclidean")

## ----Hier-clus, cache=TRUE--------------------------------------------------------------------------------------------------------------
hier_aver <- agnes(dist_matrix_scaled, method = "average")
hier_sing <- agnes(dist_matrix_scaled, method = "single")
hier_comp <- agnes(dist_matrix_scaled, method = "complete")
hier_ward <- agnes(dist_matrix_scaled, method = "ward")
hier_weig <- agnes(dist_matrix_scaled, method = "weighted")
hier_gave <- agnes(dist_matrix_scaled, method = "gaverage")

m <- list( hier_aver, hier_sing, hier_comp, hier_ward, hier_weig, hier_gave)
names(m) <- c( "average", "single", "complete", "ward", "weighted", "gaverage")
ac <- function(x) {
  x$ac
}
print("The agglomerative coefficients for each of the 6 linkages are shown below:")
map_dbl(m, ac)

## ----Hier-link, fig.ncol = 2, out.width = "50%", fig.align = "center", fig.cap="Dendrogram of the 6 hierarchical clustering methods.", fig.subcap=c('Cluster Similarity: Group Average', 'Cluster Similarity: Single Link', 'Cluster Similarity: Complete Link', 'Cluster Similarity: Ward’s Method', 'Cluster Similarity: Weighted (UPGMA)', 'Cluster Similarity: Generalized Average (Flexible UPGMA)')----
pltree(hier_aver, cex = 0.6, hang = -1, main = 
         paste0("Dendrogram of AGNES (method = average)"))
pltree(hier_sing, cex = 0.6, hang = -1, main = 
         paste0("Dendrogram of AGNES (method = single)"))
pltree(hier_comp, cex = 0.6, hang = -1, main = 
         paste0("Dendrogram of AGNES (method = complete)"))
pltree(hier_ward, cex = 0.6, hang = -1, main = 
         paste0("Dendrogram of AGNES (method = ward)"))
pltree(hier_weig, cex = 0.6, hang = -1, main = 
         paste0("Dendrogram of AGNES (method = weighted)"))
pltree(hier_gave, cex = 0.6, hang = -1, main = 
         paste0("Dendrogram of AGNES (method = gaverage)"))

## ----Clus-Diana, cache=TRUE-------------------------------------------------------------------------------------------------------------
# For the divisive hierarchical clustering
hier_diana <- diana(dist_matrix_scaled)

## ----Hier-diana, fig.cap="Dendrogram of the Divisive Analysis clustering."--------------------------------------------------------------
# Divise coefficient
print(paste0("The divisive coefficient is: ", hier_diana$dc))

# Plotting
pltree(hier_diana, cex = 0.6, hang = -1, main = "Dendrogram of DIANA")

## ----Cut-Calc, cache=TRUE---------------------------------------------------------------------------------------------------------------
# Ward
cut_ward_2 <- cutree(hier_ward, k = 2)
cut_ward_5 <- cutree(hier_ward, k = 5)
#GAverage
cut_gave_2 <- cutree(hier_gave, k = 2)
cut_gave_5 <- cutree(hier_gave, k = 5)
#DIANA
cut_diana_2 <- cutree(hier_diana, k = 2)
cut_diana_5 <- cutree(hier_diana, k = 5)

## ----cache=TRUE-------------------------------------------------------------------------------------------------------------------------
# Ward
cluster_ward_2 <- aggregate(data_matrix_scaled,list(cluster=cut_ward_2),mean)
cluster_ward_2 <- cluster_ward_2[,-c(1)]

cluster_ward_5 <- aggregate(data_matrix_scaled,list(cluster=cut_ward_5),mean)
cluster_ward_5 <- cluster_ward_5[,-c(1)]

# GAverage
cluster_gave_2 <- aggregate(data_matrix_scaled,list(cluster=cut_gave_2),mean)
cluster_gave_2 <- cluster_gave_2[,-c(1)]

cluster_gave_5 <- aggregate(data_matrix_scaled,list(cluster=cut_gave_5),mean)
cluster_gave_5 <- cluster_gave_5[,-c(1)]

# DIANA
cluster_diana_2 <- aggregate(data_matrix_scaled,list(cluster=cut_diana_2),mean)
cluster_diana_2 <- cluster_diana_2[,-c(1)]

cluster_diana_5 <- aggregate(data_matrix_scaled,list(cluster=cut_diana_5),mean)
cluster_diana_5 <- cluster_diana_5[,-c(1)]

## ----dendro-colored, cache=TRUE, fig.ncol = 2, out.width = "50%", fig.align = "center", fig.cap="Dendrograms of the hierarchical clusterings using Ward's method, Generalized Average method, and Divisive Analysis (DIANA) clustering, colored to represent clusters of 2 (the original clustering) and 5 (predicted clustering from PCA).", fig.subcap=c('Ward\'s Method, Clustering = 2.', 'Ward\'s Method, Clustering = 5.', 'Generalized Average, Clustering = 2.', 'Generalized Average, Clustering = 5.', 'DIANA, Clustering = 2.', 'DIANA, Clustering = 5.')----
dend_obj_ward <- as.dendrogram(hier_ward)
dend_col_ward_2 <- color_branches(dend_obj_ward, k=2)
dend_col_ward_5 <- color_branches(dend_obj_ward, k=5)
plot(dend_col_ward_2, main = "Dendrogram (method = ward, k=2)")
plot(dend_col_ward_5, main = "Dendrogram (method = ward, k=5)")

dend_obj_gave <- as.dendrogram(hier_gave)
dend_col_gave_2 <- color_branches(dend_obj_gave, k=2)
dend_col_gave_5 <- color_branches(dend_obj_gave, k=5)
plot(dend_col_gave_2, main = "Dendrogram (method = gaverage, k=2)")
plot(dend_col_gave_5, main = "Dendrogram (method = gaverage, k=5)")

dend_obj_diana <- as.dendrogram(hier_diana)
dend_col_diana_2 <- color_branches(dend_obj_diana, k=2)
dend_col_diana_5 <- color_branches(dend_obj_diana, k=5)
plot(dend_col_diana_2, main = "Dendrogram (method = DIANA, k=2)")
plot(dend_col_diana_5, main = "Dendrogram (method = DIANA, k=5)")

### For k-means Clustering ###

## ----K-estimate, warning=FALSE, cache=TRUE----------------------------------------------------------------------------------------------
elbow_wss <- fviz_nbclust(data_matrix_scaled, kmeans, method = "wss", k.max=40)
elbow_silho <- fviz_nbclust(data_matrix_scaled, kmeans, method = "silhouette", k.max=40)

# To plot the k-estimating plots.
elbow_wss
elbow_silho

## ----K-means-clus, cache=TRUE, warning=FALSE--------------------------------------------------------------------------------------------
k_clus_2 <- kmeans(data_matrix_scaled, centers = 2, nstart = 100)
k_clus_4 <- kmeans(data_matrix_scaled, centers = 4, nstart = 100)
k_clus_5 <- kmeans(data_matrix_scaled, centers = 5, nstart = 100)
k_clus_12 <- kmeans(data_matrix_scaled, centers = 12, nstart = 100)

## ----K-means-clus-custom, cache=TRUE----------------------------------------------------------------------------------------------------
k_clus_ward_2 <- kmeans(data_matrix_scaled, centers = cluster_ward_2)
k_clus_ward_5 <- kmeans(data_matrix_scaled, centers = cluster_ward_5)
k_clus_gave_2 <- kmeans(data_matrix_scaled, centers = cluster_gave_2)
k_clus_gave_5 <- kmeans(data_matrix_scaled, centers = cluster_gave_5)
k_clus_diana_2 <- kmeans(data_matrix_scaled, centers = cluster_diana_2)
k_clus_diana_5 <- kmeans(data_matrix_scaled, centers = cluster_diana_5)

## ---------------------------------------------------------------------------------------------------------------------------------------
noquote("The size of the clusters for each of the k-means clusters are shown below:")
noquote("Randomly Initialized, k = 2:")
print(k_clus_2$size)
noquote("Randomly Initialized, k = 4:")
print(k_clus_4$size)
noquote("Randomly Initialized, k = 5:")
print(k_clus_5$size)
noquote("Randomly Initialized, k = 12:")
print(k_clus_12$size)

noquote("Ward's Method Initialized, k = 2:")
print(k_clus_ward_2$size)
noquote("Ward's Method Initialized, k = 5:")
print(k_clus_ward_5$size)
noquote("Generalized Averaged Method Initialized, k = 2:")
print(k_clus_gave_2$size)
noquote("Generalized Averaged Method Initialized, k = 5:")
print(k_clus_gave_5$size)
noquote("DIANA Method Initialized, k = 2:")
print(k_clus_diana_2$size)
noquote("DIANA Method Initialized, k = 5:")
print(k_clus_diana_5$size)

## ----k-random, cache=TRUE, fig.ncol = 2, out.width = "50%", fig.align = "center", fig.cap="Results of the k-means clustering for all 4 randomly initialized cluster options. A PCA was produced and the clusterings were projected onto the first 2 principal components in order to visualize them.", fig.subcap=c('k=2', 'k=4', 'k=5', 'k=12')----
fviz_cluster(k_clus_2, data = data_matrix_scaled)
fviz_cluster(k_clus_4, data = data_matrix_scaled)
fviz_cluster(k_clus_5, data = data_matrix_scaled)
fviz_cluster(k_clus_12, data = data_matrix_scaled)

## ----k-hierarchical, cache=TRUE, fig.ncol = 2, out.width = "50%", fig.align = "center", fig.cap="Results of the k-means clustering for all 6 hierarchical clustering-determined cluster options. A PCA was produced and the clusterings were projected onto the first 2 principal components in order to visualize them.", fig.subcap=c('Ward\'s Method, k=2', 'Ward\'s Method, k=5', 'Generalized Average Method, k=2', 'Generalized Average Method, k=5', 'DIANA Method, k=2', 'DIANA Method, k=5')----
fviz_cluster(k_clus_ward_2, data = data_matrix_scaled)
fviz_cluster(k_clus_ward_5, data = data_matrix_scaled)
fviz_cluster(k_clus_gave_2, data = data_matrix_scaled)
fviz_cluster(k_clus_gave_5, data = data_matrix_scaled)
fviz_cluster(k_clus_diana_2, data = data_matrix_scaled)
fviz_cluster(k_clus_diana_5, data = data_matrix_scaled)

### For k-medoid Clustering ###

## ----Clara, cache=TRUE------------------------------------------------------------------------------------------------------------------
clara_2 <- clara(data_matrix_clus, k=2, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = TRUE)
clara_3 <- clara(data_matrix_clus, k=3, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = TRUE)
clara_4 <- clara(data_matrix_clus, k=4, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = TRUE)
clara_5 <- clara(data_matrix_clus, k=5, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = TRUE)
clara_8 <- clara(data_matrix_clus, k=8, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = TRUE)
clara_12 <- clara(data_matrix_clus, k=12, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = TRUE)


## ----Clara-plots, cache=TRUE------------------------------------------------------------------------------------------------------------
print (clara_2)
fviz_cluster(clara_2, data = data_matrix_clus)
fviz_cluster(clara_3, data = data_matrix_clus)
fviz_cluster(clara_4, data = data_matrix_clus)
fviz_cluster(clara_5, data = data_matrix_clus)
fviz_cluster(clara_8, data = data_matrix_clus)
fviz_cluster(clara_12, data = data_matrix_clus)

### For k-means clustering on a Self-organizing Map ###

## ----SOM, cache=TRUE--------------------------------------------------------------------------------------------------------------------
data_matrix_s <- data_matrix_clus
som_grid <- somgrid(xdim = 20, ydim = 20, topo = "hexagonal")
som_model <- som(data_matrix_s, 
                 grid=som_grid, 
                 rlen=200, 
                 keep.data = TRUE
                )
som_codebook = getCodes(som_model) 

#Training progress for SOM
plot(som_model, type="changes")
#Node count plot
plot(som_model, type="count", main="Node Counts")
# U-matrix visualisation
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")

k_clus_som_2 <- kmeans(scale(som_codebook), centers = 2, nstart = 500)
k_clus_som_4 <- kmeans(scale(som_codebook), centers = 4, nstart = 500)
k_clus_som_5 <- kmeans(scale(som_codebook), centers = 5, nstart = 500)
k_clus_som_12 <- kmeans(scale(som_codebook), centers = 12, nstart = 500)

## ----SOM-Plots--------------------------------------------------------------------------------------------------------------------------
plot(som_model, type="mapping", bgcol = k_clus_som_2$cluster, main = "Self-Organizing Map, k = 2")
plot(som_model, type="mapping", bgcol = k_clus_som_4$cluster, main = "Self-Organizing Map, k = 4")
plot(som_model, type="mapping", bgcol = k_clus_som_5$cluster, main = "Self-Organizing Map, k = 5")
plot(som_model, type="mapping", bgcol = k_clus_som_12$cluster, main = "Self-Organizing Map, k = 12")

### For Validation ###

## ----K-means-boot, tidy=FALSE, cache=TRUE, results="hide"-------------------------------------------------------------------------------
k_clus_2_boot <- clusterboot(data_matrix_scaled, B=100, bootmethod="boot",
                        clustermethod=kmeansCBI,
                        krange=2, seed=786)

k_clus_4_boot <- clusterboot(data_matrix_scaled, B=100, bootmethod="boot",
                        clustermethod=kmeansCBI,
                        krange=4, seed=786)

k_clus_5_boot <- clusterboot(data_matrix_scaled, B=100, bootmethod="boot",
                        clustermethod=kmeansCBI,
                        krange=5, seed=786)

k_clus_12_boot <- clusterboot(data_matrix_scaled, B=100, bootmethod="boot",
                        clustermethod=kmeansCBI,
                        krange=12, seed=786)

## ----Clara-boot, cache=TRUE, results="hide"---------------------------------------------------------------------------------------------
clara_2_boot <- clusterboot(data_matrix_scaled, B=100, bootmethod="boot",
                        clustermethod=claraCBI,
                        k=2, seed=786)
clara_3_boot <- clusterboot(data_matrix_scaled, B=100, bootmethod="boot",
                        clustermethod=claraCBI,
                        k=3, seed=786)
clara_4_boot <- clusterboot(data_matrix_scaled, B=100, bootmethod="boot",
                        clustermethod=claraCBI,
                        k=4, seed=786)
clara_5_boot <- clusterboot(data_matrix_scaled, B=100, bootmethod="boot",
                        clustermethod=claraCBI,
                        k=5, seed=786)
clara_8_boot <- clusterboot(data_matrix_scaled, B=100, bootmethod="boot",
                        clustermethod=claraCBI,
                        k=8, seed=786)
clara_12_boot <- clusterboot(data_matrix_scaled, B=100, bootmethod="boot",
                        clustermethod=claraCBI,
                        k=12, seed=786)

# Printing the results of k-means and k-medoid Bootstrap Evaluation
print(k_clus_2_boot)
print(k_clus_4_boot)
print(k_clus_5_boot)
print(k_clus_12_boot)
print(clara_2_boot)
print(clara_4_boot)
print(clara_5_boot)
print(clara_12_boot)

## ----Multi-valid-test, tidy=FALSE, cache=TRUE, warning=FALSE----------------------------------------------------------------------------
multi_valid_test <- clValid(data_matrix_scaled, c(2, 3, 4, 5, 8, 12),
                      clMethods = c("agnes", "diana", "kmeans", "clara", "som" ),
                      validation = c("internal", "stability"),
                      maxitems = 50000,
                      method = "ward"
)
summary(multi_valid_test)

## ----Stability-plots, tidy=FALSE, fig.ncol = 2, out.width = "50%", fig.align = "center", fig.cap="Results of the stability validation tests for all the specified clustering methods and k values.", fig.subcap=c('')----
plot(multi_valid_test, measure=c("APN"), legend=FALSE, lwd = 3.5)
legend("bottomright", clusterMethods(multi_valid_test),col=1:5,
       lty=1:5, pch=paste(1:5), lwd = 3.5)

plot(multi_valid_test, measure=c("AD"), legend=FALSE, lwd = 3.5)
legend("bottomleft", clusterMethods(multi_valid_test), col=1:5,
       lty=1:5, pch=paste(1:5), lwd = 3.5)

plot(multi_valid_test, measure=c("ADM"), legend=FALSE, lwd = 3.5)
legend("bottomright", clusterMethods(multi_valid_test), col=1:5,
       lty=1:5, pch=paste(1:5), lwd = 3.5)

plot(multi_valid_test, measure=c("FOM"), legend=FALSE, lwd = 3.5)
legend("bottomleft", clusterMethods(multi_valid_test), col=1:5,
       lty=1:5, pch=paste(1:5), lwd = 3.5)

## ----Internal-plots, tidy=FALSE, fig.ncol = 2, out.width = "50%", fig.align = "center", fig.cap="Results of the Internal validation tests for all the specified clustering methods and k values.", fig.subcap=c('')----
plot(multi_valid_test, measure=c("Connectivity"), legend=FALSE, lwd = 3.5)
legend("topleft", clusterMethods(multi_valid_test), col=1:5, lty=1:5,
       pch=paste(1:5), lwd = 3.5)

plot(multi_valid_test, measure=c("Dunn"), legend=FALSE, lwd = 3.5)
legend("topright", clusterMethods(multi_valid_test), col=1:5, lty=1:5,
       pch=paste(1:5), lwd = 3.5)

plot(multi_valid_test, measure=c("Silhouette"), legend=FALSE, lwd = 3.5)
legend("topright", clusterMethods(multi_valid_test), col=1:5, lty=1:5,
       pch=paste(1:5), lwd = 3.5)

## ----Hier-sil-plots, tidy=FALSE, fig.ncol = 2, out.width = "50%", fig.align = "center", fig.cap="Silhouette plots of all the k = 2 hierarchical clusters we have generated.", fig.subcap=c('')----
# hierarchical
sil_heir_diana_2 <- silhouette(cut_diana_2, dist_matrix_scaled)
sil_heir_ward_2 <- silhouette(cut_ward_2, dist_matrix_scaled)
sil_heir_gave_2 <- silhouette(cut_gave_2, dist_matrix_scaled)

noquote("Cluster Sizes and Silhouette values for DIANA, Ward's Method, and ")
noquote("Generalized Average hierarchical clusterings, respectively:")

fviz_silhouette(sil_heir_diana_2, print.summary = TRUE,
                main ="Silhouette Plot (Hierarchical Clustering, DIANA, k = 2)")
fviz_silhouette(sil_heir_ward_2, print.summary = TRUE,
                main ="Silhouette Plot (Hierarchical Clustering, Ward's Method, k = 2)")
fviz_silhouette(sil_heir_gave_2, print.summary = TRUE,
                main ="Silhouette Plot (Hierarchical Clustering, Generalized Avg., k = 2)")

## ----K-means-sil-plots, tidy=FALSE, fig.ncol = 2, out.width = "50%", fig.align = "center", fig.cap="Silhouette plots of all the k = 2 k-means clusters we have generated.", fig.subcap=c('')----
# k-means
sil_k_rand_2 <- silhouette(k_clus_2$cluster, dist_matrix_scaled)
sil_k_ward_2 <- silhouette(k_clus_ward_2$cluster, dist_matrix_scaled)
sil_k_gave_2 <- silhouette(k_clus_gave_2$cluster, dist_matrix_scaled)
sil_k_diana_2 <- silhouette(k_clus_diana_2$cluster, dist_matrix_scaled)

noquote("Cluster Sizes and Silhouette values for the randomly initialized and Ward's")
noquote("Method, Generalized Average, and DIANA hierarchical clustering-initialized")
noquote("k-means clusters, respectively:")

fviz_silhouette(sil_k_rand_2, print.summary = TRUE,
                main ="Silhouette Plot (k-means Clustering, randomly initialized, k = 2)")
fviz_silhouette(sil_k_ward_2, print.summary = TRUE,
           main ="Silhouette Plot (k-means Clustering, Ward's Method initialized, k = 2)")
fviz_silhouette(sil_k_gave_2, print.summary = TRUE,
        main ="Silhouette Plot (k-means Clustering, Generalized Avg. initialized, k = 2)")
fviz_silhouette(sil_k_diana_2, print.summary = TRUE,
                main ="Silhouette Plot (k-means Clustering, DIANA initialized, k = 2)")

## ----K-means-12-sil-plot, tidy=FALSE, fig.align = "center", fig.cap="Silhouette plot for the randomly initialized k = 12 k-means cluster."----
sil_k_rand_12 <- silhouette(k_clus_12$cluster, dist_matrix_scaled)
noquote("Cluster Size and Silhouette value for the randomly initialized k-means cluster, k=12.")
fviz_silhouette(sil_k_rand_12, print.summary = TRUE,
               main ="Silhouette Plot (k-means Clustering, randomly initialized, k = 12)")

## ---------------------------------------------------------------------------------------------------------------------------------------
orig_cluster <- data.matrix(clean_sample)
orig_cluster <- orig_cluster[,c(11)]
sample_frame <- data.frame(sample)

## ---------------------------------------------------------------------------------------------------------------------------------------
noquote("Cluster Size Comparison:")
table(sample_frame$income, cut_diana_2)
ext_stats_hier_diana_2 <- cluster.stats(d=dist_matrix_scaled, orig_cluster, cut_diana_2)
randInd_hier_diana_2 <- ext_stats_hier_diana_2$corrected.rand
print(paste0("Rand Index for DIANA Hierarchical Clustering: ", randInd_hier_diana_2))

## ---- tidy=FALSE------------------------------------------------------------------------------------------------------------------------
noquote("Cluster Size Comparison:")
table(sample_frame$income, cut_ward_2)
ext_stats_hier_ward_2 <- cluster.stats(d=dist_matrix_scaled, orig_cluster, cut_ward_2)
randInd_hier_ward_2 <- ext_stats_hier_ward_2$corrected.rand
print(paste0("Rand Index for Ward's Method Hierarchical Clustering: ",
             randInd_hier_ward_2))

## ---- tidy=FALSE------------------------------------------------------------------------------------------------------------------------
noquote("Cluster Size Comparison:")
table(sample_frame$income, cut_gave_2)
ext_stats_hier_gave_2 <- cluster.stats(d=dist_matrix_scaled, orig_cluster, cut_gave_2)
randInd_hier_gave_2 <- ext_stats_hier_gave_2$corrected.rand
print(paste0("Rand Index for GAveraged Method Hierarchical Clustering: ",
             randInd_hier_gave_2))

## ---- tidy=FALSE------------------------------------------------------------------------------------------------------------------------
noquote("Cluster Size Comparison:")
table(sample_frame$income, k_clus_2$cluster)
ext_stats_k_rand_2 <- cluster.stats(d=dist_matrix_scaled, orig_cluster, k_clus_2$cluster)
randInd_k_rand_2 <- ext_stats_k_rand_2$corrected.rand
print(paste0("Rand Index for the randomly initialized k-means clustering: ",
             randInd_k_rand_2))

## ---- tidy=FALSE------------------------------------------------------------------------------------------------------------------------
noquote("Cluster Size Comparison:")
table(sample_frame$income, k_clus_ward_2$cluster)
ext_stats_k_ward_2 <- cluster.stats(d=dist_matrix_scaled, orig_cluster,
                                    k_clus_ward_2$cluster)
randInd_k_ward_2 <- ext_stats_k_ward_2$corrected.rand
print(paste0("Rand Index the Ward's Method-initialized k-means clustering: ",
             randInd_k_ward_2))

## ---- tidy=FALSE------------------------------------------------------------------------------------------------------------------------
noquote("Cluster Size Comparison:")
table(sample_frame$income, k_clus_gave_2$cluster)
ext_stats_k_gave_2 <- cluster.stats(d=dist_matrix_scaled, orig_cluster,
                                    k_clus_gave_2$cluster)
randInd_k_gave_2 <- ext_stats_k_gave_2$corrected.rand
print(paste0("Rand Index the GAveraged-initialized k-means clustering: ", randInd_k_gave_2))

## ---- tidy=FALSE------------------------------------------------------------------------------------------------------------------------
noquote("Cluster Size Comparison:")
table(sample_frame$income, k_clus_diana_2$cluster)
ext_stats_k_diana_2 <- cluster.stats(d=dist_matrix_scaled, orig_cluster,
                                     k_clus_diana_2$cluster)
randInd_k_diana_2 <- ext_stats_k_diana_2$corrected.rand
print(paste0("Rand Index the DIANA-initialized k-means clustering: ", randInd_k_diana_2))

## ---- tidy=FALSE------------------------------------------------------------------------------------------------------------------------
noquote("Cluster Size Comparison:")
table(sample_frame$income, clara_2$cluster)
ext_stats_k_medoid_2 <- cluster.stats(d=dist_matrix_scaled, orig_cluster,
                                     clara_2$cluster)
randInd_k_medoid_2 <- ext_stats_k_medoid_2$corrected.rand
print(paste0("Rand Index for k-medoid clustering: ", randInd_k_medoid_2))

## ---------------------------------------------------------------------------------------------------------------------------------------
print(paste0("Sammon's MDS Stress (Classic Initialized): ", samm_mds_classic$stress))
print(paste0("Kruskal's MDS Stress (Classic Initialized): ", kruskal_mds_classic$stress))
print(paste0("Symmetric Smacof MDS Stress (Classic Initialized): ", smacof_mds_classic$stress))

print(paste0("Best Sammon's MDS Stress (Randomly Initialized): ", best_samm_mds$stress))
print(paste0("Best Kruskal's MDS Stress (Randomly Initialized): ", best_kruskal_mds$stress))
print(paste0("Best Symmetric Smacof MDS Stress (Randomly Initialized): ", best_smacof_mds$stress))

print(paste0("Ward's Method agglomerative coefficient: ", hier_ward$ac))
print(paste0("Generalized Average agglomerative coefficient: ", hier_gave$ac))
print(paste0("DIANA clustering  divisive coefficient: ", hier_diana$dc))

### Extra Testing, based on Visual Inspections and Guesses

## ----K-8-hier, fig.ncol = 2, out.width = "50%", fig.align = "center", fig.cap="(a) Hierarchical Clustering using Ward's method and (b) its correspondingly initialized k-means clustering if we take k = 8. (c) k-means clustering for k=3, randomly initialized.", fig.subcap=c('Hierarchical Clustering, k=8', 'k-means Clustering (Hierarchical Clustering-Initialized), k=8', 'k-means Clustering (Randomly Initialized), k=3')----

dend_col_ward_8 <- color_branches(dend_obj_ward, k=8)
plot(dend_col_ward_8, main = "Dendrogram (method = ward, k=8)")

cut_ward_8 <- cutree(hier_ward, k = 8)
cluster_ward_8 = aggregate(data_matrix_scaled,list(cluster=cut_ward_8),mean)
cluster_ward_8 <- cluster_ward_8[,-c(1)]
k_clus_ward_8 <- kmeans(data_matrix_scaled, centers = cluster_ward_8)
noquote("Cluster Sizes for Ward's Method-Initialized k-means clustering, k = 8:")
print(k_clus_ward_8$size)
fviz_cluster(k_clus_ward_8, data = data_matrix_scaled)

k_clus_3 <- kmeans(data_matrix_scaled, centers = 3, nstart = 100)
fviz_cluster(k_clus_3, data = data_matrix_scaled)

### Main Modification to ggbiplot R Function. ###
my_ggbiplot <- function (pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE,
    obs.scale = 1 - scale, var.scale = scale, groups = NULL,
    ellipse = FALSE, ellipse.prob = 0.68, labels = NULL, labels.size = 3,
    alpha = 1, var.axes = TRUE, circle = FALSE, circle.prob = 0.69,
    varname.size = 3, varname.adjust = 1.5, varname.abbrev = FALSE,
    ...)
{
    ...

    if (var.axes) {
        if (circle) {
            theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi,
                length = 50))
            circle <- data.frame(xvar = r * cos(theta), yvar = r *
                sin(theta))
            g <- g + geom_path(data = circle, color = muted("white"),
                size = 1/2, alpha = 1/3)
        }
        g <- g + geom_segment(data = df.v, aes(x = 0, y = 0,
            xend = xvar, yend = yvar), arrow = arrow(length = unit(1/2,
            "picas")), color = muted("red"))
    }

    ...

    if (var.axes) {
        g <- g + geom_text(data = df.v, aes(label = varname,
            x = xvar, y = yvar, angle = angle, hjust = hjust),
            color = "red", size = 5)
    }
    return(g)
}

