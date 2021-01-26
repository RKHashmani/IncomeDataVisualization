# Setting working directory
setwd("/Users/macbookpro/Documents/GitHub/IncomeDataVisualization")

library(som)

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

# Making all matrix to numeric
for (col in ncol(data_matrix)) {
  data_matrix[,col] = as.numeric(data_matrix[,col])
}

# SOM Application


som.init(data_matrix, xdim = 200, ydim = 200, init= "linear")
som_data = som(data_matrix, xdim = 200, ydim = 200, init = "linear",
               topol = "rect")

plot(som_data)

df = data.frame(X = som_data$code.sum$x,
                Y = som_data$code.sum$y,
                Cell_Representation_Count = som_data$code.sum$nobs)
ggplot(data = df, aes(x = X ,y = Y)) +
  geom_point(aes(size = Cell_Representation_Count))
