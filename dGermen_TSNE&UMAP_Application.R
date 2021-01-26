# Setting working directory
setwd("/Users/macbookpro/Documents/GitHub/IncomeDataVisualization")

# Activating packages
library(caret) # Dummy variables
library(Rtsne)
library(uwot)

# To read pre-sampled data

sample_frame = read.csv("C:/Users/Deniz/Documents/GitHub/IncomeDataVisualization/Data/5000framev1.csv")

# Frame to Matrix
data_matrix_orig <- data.matrix(sample_frame)

# Extracting the labels
dataLabels = data_matrix_orig[,11] %>%
  unlist %>%
  factor(labels = c("<50K",">50K"))

# Removing the Labels
data_matrix <- data_matrix_orig[,-c(11)]
s = scale(data_matrix_OHE, center = FALSE, scale = TRUE)
sample_number <- nrow(data_matrix)
print(paste0("The number of samples selected is: ", sample_number))

## One hot encoding
dmy = dummyVars(" ~ .", data = sample_frame)
data5000_OHE = data.frame(predict(dmy, newdata = sample_frame))
data_matrix_OHE = data.frame(data5000_OHE)

write.csv(data5000_OHE,'5000frameOHE.csv', row.names = TRUE, col.names = TRUE)

# UMAP and t-SNE Application
## UMAP Application and Plotting
## n_neighbors 5-50 , 10-15 
## min_dist 0.001-0.5 , 0.1
## def config 15, 0.1
UMAP_5000 = umap(data_matrix)
df = data.frame(X = UMAP_5000[,1],
                Y = UMAP_5000[,2],
                Labels = dataLabels)

i = 10
UMAP_5000 = umap(data_matrix, n_neighbors = i, min_dist = 0.3)
df = data.frame(X = UMAP_5000$layout[,1],
                Y = UMAP_5000$layout[,2],
                Labels = dataLabels)

plot = ggplot(data = df, aes(x = X ,y = Y, col = Labels)) +
  geom_point()
name = paste("T_T ",i,"test.png")
png(name)
print(plot)
dev.off()

# Hyperparameter tuning
min_dist_1 = seq(0.001,0.2,0.01)
min_dist_2 = seq(0.1,0.5,0.05)
min_dist = c(min_dist_1, min_dist_2)

for (n_neig in seq(5,50,5)) { # 46 Iterations
  for (min_distance in min_dist) { # 29 Iterations
    UMAP_Data = umap(data_matrix, n_neighbors = n_neig, min_dist = min_distance)
    df = data.frame(X = UMAP_Data$layout[,1],
                    Y = UMAP_Data$layout[,2],
                    Labels = dataLabels)
    plot = ggplot(data = df, aes(x = X ,y = Y, col = Labels)) +
      geom_point()
    fileName = paste("N_N", n_neig, "M_D", min_distance)
    png(fileName)
    print(plot)
    dev.off()
  }
}

UMAP_5000_OHE = umap(data_matrix_OHE)
df = data.frame(X = UMAP_5000_OHE$layout[,1],
                Y = UMAP_5000_OHE$layout[,2],
                Labels = dataLabels)

ggplot(data = df, aes(x = X ,y = Y, col = Labels)) +
  geom_point()

## t-SNE Application p 1:400 ex 1:500
tSNE_5000 = Rtsne(data_matrix,perplexity = 5,exaggeration_factor = 120)
df = data.frame(X = tSNE_5000$Y[,1],
                Y = tSNE_5000$Y[,2],
                Labels = dataLabels)
ggplot(data = df, aes(x = X ,y = Y, col = Labels)) +
  geom_point()

tSNE_5000_OHE = Rtsne(data_matrix_OHE)
df = data.frame(X = tSNE_5000_OHE$Y[,1],
                Y = tSNE_5000_OHE$Y[,2],
                Labels = dataLabels)
ggplot(data = df, aes(x = X ,y = Y, col = Labels)) +
  geom_point()

# SOM Outlier Application
## SOM Application

SOM_5000 = som(as.numeric(data_matrix), xdim = 10, ydim = 10, 
               neigh = "gaussian", topol = "hexa")
som(as.numeric(data_matrix), xdim = 10, ydim = 10, init="linear", alpha=NULL, alphaType="inverse",
    neigh="gaussian", topol="rect", radius=NULL, rlen=NULL, err.radius=1,
    inv.alp.c=NULL)
s = as.numeric(data_matrix)







## DIRECTLY USABLE PLOTS
tSNE_Data = Rtsne(data_matrix,
                  perplexity = 30,
                  max_iter = 500, 
                  verbose = TRUE, 
                  exaggeration_factor = 12 )

df = data.frame(X = tSNE_Data$Y[,1],
                Y = tSNE_Data$Y[,2],
                Labels = dataLabels)
ggplot(data = df, aes(x = X ,y = Y, col = Labels)) +
  geom_point()

UMAP_Data = umap(data_matrix, n_neighbors = 13, min_dist = 0.1)
df = data.frame(X = UMAP_Data[,1],
                Y = UMAP_Data[,2],
                Labels = dataLabels)

ggplot(data = df, aes(x = X ,y = Y, col = Labels)) +
  geom_point()



# Custom Functions

## 1.5xIQR Rule
IQR_Outlier <- function(data, columnArray) {
  for (column in columnArray) {
    columnData = data[,column]
    
    Q3<-quantile(columnData,0.75)
    Q1<-quantile(columnData,0.25)
    IQR<-(Q3-Q1)
    left<- (Q1-(1.5*IQR))
    right<- (Q3+(1.5*IQR))
    
    c(columnData[columnData <left],columnData[columnData>right])
  }
  
  
}
