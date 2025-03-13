# Basic Data Analysis with R

## Project Overview
This project demonstrates a comprehensive analysis of a diabetes-related dataset using various data science techniques in R. The analysis involves:
- **Data Preprocessing**: Cleaning and preparing the data for analysis.
- **Principal Component Analysis (PCA)**: Reducing dimensionality and visualizing key relationships between variables.
- **Machine Learning Algorithms**: Implementation of classic algorithms such as K-Nearest Neighbors (KNN), Support Vector Machines (SVM), and Decision Trees for classification tasks.
- **Model Evaluation**: Assessing model performance using metrics like accuracy, precision, recall, and F1-score.

## Project Structure
```
|-- Basic_Data_Analysis_with_R.Rmd  # The main R Markdown file
|-- Basic_Data_Analysis_with_R.html # The generated HTML report
|-- README.md                       # This README file

```

## Installation
To reproduce the analysis, you need to have R and RStudio installed. You will also need the following R packages:

```r
install.packages(c("tidyverse", "factoextra", "plotly", "caret", "e1071", "rpart", "rmarkdown", "dplyr", "ggplot2", "nortest", "reshape2", "tidyr", "cluster"))
```

## Usage
1. Clone the repository:
   
```bash
git clone https://github.com/inesmora29/basic-data-analysis.git
```

2. Open `Basic_Data_Analysis_with_R.Rmd` in RStudio.

3. Install the necessary packages if you haven't already.

4. Knit the document to generate the HTML report.

## Results
- The correlation analysis shows how the variables are related to each other
- The PCA analysis shows the contribution of various variables to the principal components.
- Machine learning models are trained and evaluated for classification tasks, with accuracy displayed in the report.


