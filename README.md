# YouTube-Earnings-Classification-GDP

## Project Overview
This project analyzes the 2023 Global YouTube Statistics dataset, which focuses on classifying YouTube creators' earnings as high or low based on the GDP of their respective countries. 

## Data Source
The primary dataset for this project is the "Global YouTube Statistics 2023" from Kaggle, which provides comprehensive metrics related to YouTube channels. This dataset can be accessed at [Kaggle Dataset Link](https://www.kaggle.com/datasets/nelgiriyewithana/global-youtube-statistics-2023). A crucial aspect of our analysis is the incorporation of GDP data, sourced from the World Bank website. The specific GDP data used in this project can be found at [World Bank GDP Data](https://data.worldbank.org/indicator/NY.GDP.PCAP.CD).

## Data Preparation and Analysis
Data preparation is a critical step in this project, involving the cleaning and transformation of the `youtube_UTF_8.csv` dataset. This dataset includes various metrics related to YouTube channels. We remove columns that are not necessary for analysis, such as unique identifiers and highly correlated attributes, to focus on the most relevant features for classification.

## Shiny Application
An interactive Shiny application has been developed to provide dynamic visualization and exploration of the data and classification results. This application allows users to interact with the dataset, offering insights into the impact of various features and the classification outcomes in an engaging and informative way.

## Classification Methodology
The project employs a Decision Tree Classifier and compares its performance with another classification technique, such as logistic regression, Naïve Bayes, or KNN. This involves splitting the dataset into training and test sets, selecting appropriate features, and evaluating the models' performance using various statistical measures, including ROC plots and confusion matrices.

### Key Steps
- **Data Splitting**: Dividing the dataset into training and test sets for model training and evaluation.
- **Model Implementation**: Developing a Decision Tree Classifier and another comparative classification technique.
- **Attribute and Feature Selection**: Discussing the choice and impact of attributes and features on the models.
- **LIME Analysis**: Utilizing LIME to interpret key features for classification in specific test instances.
- **Model Evaluation**: Comparing models on training and test sets using statistical measures.

## Libraries and Tools Used
- dplyr
- ggplot2
- shiny
- shinyWidgets
- gridExtra
- tidyverse
- readxl
- rpart
- rpart.plot
- ROSE
- knitr
- LIME

## Additional Investigations
- **Clustering Analysis**: Applying clustering algorithms for additional insights.
- **Distance Measures and k Selection**: Investigating distance measures in clustering and the selection of the number of clusters.

