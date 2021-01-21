# Setting working directory
setwd("/Users/macbookpro/Documents/GitHub/IncomeDataVisualization")

# Activating packages
library(caret) # Dummy variables
library(umap)

# To read pre-sampled data

sample_frame = read.csv("Data/5000frame.csv")
data_matrix_orig_mds <- data.matrix(sample_frame)

data_matrix_mds <- data_matrix_orig_mds[,-c(15)]
sample_number <- nrow(data_matrix_mds)
print(paste0("The number of samples selected is: ", sample_number))

## One hot encoding
dmy = dummyVars(" ~ .", data = sample_frame)
data5000_OHE = data.frame(predict(dmy, newdata = sample_frame))

write.csv(data5000_OHE,'5000frameOHE.csv', row.names = TRUE, col.names = TRUE)

forest = isolation.forest(data_matrix_mds)
UMAP_5000 = umap(data_matrix_mds)
### Custom Functions

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