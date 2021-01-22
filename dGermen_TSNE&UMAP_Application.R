# Setting working directory
setwd("/Users/macbookpro/Documents/GitHub/IncomeDataVisualization")

# Activating packages
library(caret) # Dummy variables
library(umap)
library(Rtsne)
library(som)

# To read pre-sampled data

sample_frame = read.csv("Data/5000frame.csv")

# Frame to Matrix
data_matrix <- data.matrix(sample_frame)

# Extracting the labels
dataLabels = data_matrix_orig[,15] %>%
  unlist %>%
  factor(labels = c("<50K",">50K"))

# Removing the Labels
data_matrix <- data_matrix_orig[,-c(15)]
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
df = data.frame(X = UMAP_5000$layout[,1],
                Y = UMAP_5000$layout[,2],
                Labels = dataLabels)

ggplot(data = df, aes(x = X ,y = Y, col = Labels)) +
  geom_point()

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
