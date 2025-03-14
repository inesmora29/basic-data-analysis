library(readr)
diabetes <- read_csv("diabetes.csv")



head(diabetes)
dim(diabetes)

table(is.na(diabetes))
table(is.null(diabetes))

str(diabetes)

summary(diabetes)

diabetes_noNA <- na.omit(diabetes)
dim(diabetes_noNA)



diabetes$weight <- diabetes$weight * 0.453592 # From pounds to Kg
diabetes$height <- diabetes$height + 100 # This variable equals height - 100, we add the 100 back
diabetes$BMI <- with(diabetes, weight / ((height)/100)^2) # We can create a variable with the BMI for each sample

diabetes$waist <- diabetes$waist * 2.54 # From inches to cm
diabetes$hip <- diabetes$hip * 2.54 # From inches to cm
diabetes$waist_to_hip <- diabetes$waist / diabetes$hip # We can create a variable for hip to waist ratio

diabetes$bp.s <- ifelse(!is.na(diabetes$bp.1s) & !is.na(diabetes$bp.2s),
                        (diabetes$bp.1s + diabetes$bp.2s) / 2,
                        ifelse(!is.na(diabetes$bp.1s), diabetes$bp.1s,
                               diabetes$bp.2s))

diabetes$bp.d <- ifelse(!is.na(diabetes$bp.1d) & !is.na(diabetes$bp.2d),
                        (diabetes$bp.1d + diabetes$bp.2d) / 2,
                        ifelse(!is.na(diabetes$bp.1d), diabetes$bp.1d,
                               diabetes$bp.2d))

columns_to_delete <- c("bp.1s", "bp.2s", "bp.1d", "bp.2d", "time.ppn", "id", "gender", "frame", "location")

# We can now eliminate the columns "bp.1s", "bp.2s", "bp.1d" y "bp.2d", since we have incorporated the variables "bp.s" and "bp.d". We also remove other variables that we won't be using in our analysis
diabetes <- diabetes[, !(names(diabetes) %in% columns_to_delete)]


diabetes <- na.omit(diabetes)
dim(diabetes)

library(nortest)

check_normality <- function(data) {
  # Prepare an empty data frame to store results
  results <- data.frame(Variable = character(), Test = character(), Statistic = numeric(), p_value = numeric())
  
  for (col_name in colnames(data)) {
    if (is.numeric(data[[col_name]])) {
      
      # Shapiro-Wilk Test
      shapiro <- shapiro.test(data[[col_name]])
      results <- rbind(results, data.frame(Variable = col_name, 
                                           Test = "Shapiro-Wilk", 
                                           Statistic = shapiro$statistic, 
                                           p_value = shapiro$p.value))
      
      # Anderson-Darling Test
      anderson_darling <- ad.test(data[[col_name]])
      results <- rbind(results, data.frame(Variable = col_name, 
                                           Test = "Anderson-Darling", 
                                           Statistic = anderson_darling$statistic, 
                                           p_value = anderson_darling$p.value))
      
      # Plot Histogram and QQ plot for each variable
      par(mfrow = c(1, 2))
      hist(data[[col_name]], main = paste("Histogram of", col_name), col = "skyblue", breaks = 20)
      qqnorm(data[[col_name]], main = paste("QQ Plot of", col_name))
      qqline(data[[col_name]], col = "red")
    }
  }
  
  # Reset plot layout to single plot
  par(mfrow = c(1, 1))
  
  # Return the results data frame
  return(results)
}


normality_results <- check_normality(diabetes)


print(normality_results)


cor_matrices <- list(
  Spearman = cor(diabetes, use = "pairwise.complete.obs", method = "spearman"),
  Kendall = cor(diabetes, use = "pairwise.complete.obs", method = "kendall")
)



library(ggplot2)
library(reshape2)

plot_heatmap <- function(cor_matrix, title) {
  melted_cor <- melt(cor_matrix)
  ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = title, fill = "Correlation")
}

plot_heatmap(cor_matrices$Spearman, "Spearman Correlation Matrix")
plot_heatmap(cor_matrices$Kendall, "Kendall Correlation Matrix")


get_significance <- function(data, method = "kendall") {
  n <- ncol(data)
  sig_matrix <- matrix(NA, n, n)
  rownames(sig_matrix) <- colnames(data)
  colnames(sig_matrix) <- colnames(data)
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      test <- cor.test(data[[i]], data[[j]], method = method)
      sig_matrix[i, j] <- test$p.value
      sig_matrix[j, i] <- test$p.value
    }
  }
  return(sig_matrix)
}


get_significance(diabetes, method = "spearman")
get_significance(diabetes, method = "kendall")



get_significance(diabetes, method = "kendall")



#install.packages("factoextra")
library(factoextra)

# Performing PCA
pca_result <- prcomp(diabetes, scale = TRUE)

# Scree Plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Inspecting Loadings
pca_result$rotation

library(plotly)
library(dplyr)
# Extracting the loadings (arrows)
loadings <- as.data.frame(pca_result$rotation[, 1:3])
pca_scores <- as.data.frame(pca_result$x[, 1:3])

# Scale the loadings for better visualization
loadings$PC1 <- loadings$PC1 * max(abs(pca_scores$PC1))
loadings$PC2 <- loadings$PC2 * max(abs(pca_scores$PC2))
loadings$PC3 <- loadings$PC3 * max(abs(pca_scores$PC3))

# Create the 3D scatter plot with PCA scores
fig <- plot_ly() %>%
  add_trace(
    data = pca_scores,
    x = ~PC1,
    y = ~PC2,
    z = ~PC3,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(size = 3, color = 'orange'),
    name = 'Data Points'
  )

# Adding loadings as arrows, one by one
for (i in 1:nrow(loadings)) {
  fig <- fig %>%
    add_trace(
      x = c(0, loadings$PC1[i]),
      y = c(0, loadings$PC2[i]),
      z = c(0, loadings$PC3[i]),
      type = 'scatter3d',
      mode = 'lines+text',
      line = list(color = 'steelblue', width = 4),
      text = rownames(loadings)[i],
      textposition = 'top center',
      name = paste("Loading", rownames(loadings)[i])
    )
}

# Layout
fig <- fig %>%
  layout(
    title = "3D PCA Plot with Loadings",
    scene = list(
      xaxis = list(title = 'PC1'),
      yaxis = list(title = 'PC2'),
      zaxis = list(title = 'PC3')
    )
  )

fig



# Categorize stab.glu into Non-Diabetic, Prediabetic, and Diabetic

diabetes$DiabetesStatus <- cut(diabetes$stab.glu,
                           breaks = c(-Inf, 99, 125, Inf),
                           labels = c("Non-Diabetic", "Prediabetic", "Diabetic"))

# Extracting the relevant columns (standardizing them)
data_scaled <- scale(diabetes[, c('chol', 'stab.glu', 'hdl', 'ratio', 'glyhb', 'age', 'height', 'weight', 'waist', 'hip', 'BMI', 'waist_to_hip', 'bp.s', 'bp.d')])

# PCA
pca_res <- prcomp(data_scaled, scale. = TRUE)
pca_data <- as.data.frame(pca_res$x)
pca_data$DiabetesStatus <- diabetes$DiabetesStatus

# K-means clustering with 3 clusters
set.seed(123)
kmeans_res <- kmeans(pca_data[, c('PC1', 'PC2', 'PC3')], centers = 3)
pca_data$Cluster <- as.factor(kmeans_res$cluster)

# Interactive 3D Plot with Plotly (Diabetes Status)
plot1 <- plot_ly(pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~DiabetesStatus, colors = c('red', 'green', 'blue')) %>%
  add_markers() %>%
  layout(title = "3D PCA Plot with Diabetes Status",
         scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))

# Interactive 3D Plot with Plotly (K-means Clustering)
plot2 <- plot_ly(pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, colors = c('red', 'green', 'blue')) %>%
  add_markers() %>%
  layout(title = "3D K-means Clustering on PCA-Reduced Data",
         scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))

plot1
plot2



library(ggplot2)
library(dplyr)
library(tidyr)
library(factoextra)
library(cluster)
library(plotly)
library(caret)



# Split data into training and testing
set.seed(123)
train_index <- createDataPartition(pca_data$DiabetesStatus, p = 0.7, list = FALSE)
train_data <- pca_data[train_index, ]
test_data <- pca_data[-train_index, ]

# Define the models to train
models <- list(
  SVM = train(DiabetesStatus ~ PC1 + PC2 + PC3, data = train_data, method = 'svmRadial'),
  KNN = train(DiabetesStatus ~ PC1 + PC2 + PC3, data = train_data, method = 'knn', tuneLength = 5),
  DecisionTree = train(DiabetesStatus ~ PC1 + PC2 + PC3, data = train_data, method = 'rpart')
)

# Predict on test data & calculate performance metrics
results <- lapply(models, function(model) {
  predictions <- predict(model, test_data)
  cm <- confusionMatrix(predictions, test_data$DiabetesStatus)
  data.frame(Accuracy = cm$overall['Accuracy'])
})

# Combine results into a single data frame for comparison
results_df <- do.call(rbind, results)
results_df <- cbind(Model = names(models), results_df)
print(results_df)

# Plotly 3D Scatter Plot with Decision Boundaries (KNN Example)
grid <- expand.grid(PC1 = seq(min(pca_data$PC1), max(pca_data$PC1), length.out = 50),
                    PC2 = seq(min(pca_data$PC2), max(pca_data$PC2), length.out = 50),
                    PC3 = seq(min(pca_data$PC3), max(pca_data$PC3), length.out = 50))

predictions <- predict(models$KNN, grid)
grid$Prediction <- predictions

plot_ly() %>%
  add_markers(data = pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~DiabetesStatus, colors = c('red', 'green', 'blue'),
              marker = list(size = 3)) %>%
  add_markers(data = grid, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Prediction, opacity = 0.2, marker = list(size = 2)) %>%
  layout(title = '3D PCA Plot with Decision Boundaries',
         scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')))




