# Angela Jacinto (23778435) (50%): all tasks
# Benyapa Insawang (23890758) (50%): all tasks
#---------------------------
library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(gridExtra)
library(tidyverse)
library(readxl)
library(rpart)
library(rpart.plot)
library(ROSE)
library(shinythemes)
library(FactoMineR)
library(ROCit)
#---------------------------
plot_theme <- theme_minimal() + theme(axis.text.x = element_text(size = 16), 
                                      axis.text.y = element_text(size = 16), 
                                      axis.title = element_text(size = 18, face = "bold")) + 
  theme(legend.justification = "right", text = element_text(size = 16))
#---------------------------
# Data for Clustering
df <- read.csv('youtube_UTF_8.csv', na.strings = c('nan'))
df <- df[, -c(6, 9, 11:13, 15:17, 21:28)] # Remove redundant columns

# Extract the name of the country
country_name <- df$Country %>%
  table() %>%
  names()

# Extract the name of the country with the maximum frequency
country_mode <- country_name[which.max(table(df$Country))]

# Impute missing country with mode
df <- df %>%
  mutate(Country = ifelse(is.na(Country), country_mode, Country))

replace_na_with_median <- function(data, columns) {
  for (col in columns) {
    x <- data[[col]]
    non_na_values <- x[!is.na(x)]
    median_value <- median(non_na_values)
    is_bad <- ifelse(is.na(x), 1, 0) # isBAD take value 1 when NA is replaced by median
    data[[col]][is.na(data[[col]])] <- median_value
    data[[paste0(col, "_isBAD")]] <- is_bad
  }
  return(data)
}

columns_to_replace <- c("subscribers_for_last_30_days", "video_views_for_the_last_30_days")
df <- replace_na_with_median(df, columns_to_replace)

replace_missing_with_mapping <- function(data, level_mapping) {
  for (i in seq_len(nrow(level_mapping))) {
    category_level <- level_mapping$category_level[i]
    channel_level <- level_mapping$channel_level[i]
    
    data$channel_type[is.na(data$channel_type) & data$category == category_level] <- channel_level
    data$category[is.na(data$category) & data$channel_type == channel_level] <- category_level
  }
  return(data)
}

# Create mapping of similar level of category and channel_type
level_mapping <- data.frame(
  category_level = c("Pets & Animals", "Autos & Vehicles", "Comedy", "Education", "Entertainment", "Film & Animation", "Gaming", "Howto & Style", "Music", "News & Politics", "Nonprofits & Activism", "People & Blogs", "Sports", "Science & Technology"),
  channel_level = c("Animals", "Autos", "Comedy", "Education", "Entertainment", "Film", "Games", "Howto", "Music", "News", "Nonprofit", "People", "Sports", "Tech"))

df <- replace_missing_with_mapping(df, level_mapping)
df <- na.omit(df)

gdp <- read_excel("GDP.xls", na = "no data")
gdp <- as.data.frame(gdp) # Convert tibble to dataframe
gdp <- gdp[-1, ] # Remove an empty first row
new_column_names <- c("Country", as.character(1980:2028))
gdp <- gdp %>%
  rename_with(~new_column_names, .cols = everything()) # Rename columns

# Verify the consistency of country names between the YouTube data and GDP data
country_name %in% gdp$Country %>%
  sum() - length(country_name)
country_name[which(!country_name %in% gdp$Country)]

gdp <- gdp %>%
  mutate(Country = gsub("China, People's Republic of", "China", Country)) %>%
  mutate(Country = gsub("Korea, Republic of", "South Korea", Country)) %>%
  mutate(Country = gsub("Russian Federation", "Russia", Country)) %>%
  mutate(Country = gsub("Türkiye, Republic of", "Turkey", Country))
gdp[gdp$Country %in% levels(as.factor(df$Country)), c("Country", "2020", "2021", "2022", "2023")]

gdp2023 <- gdp[, c(1, 45)]
gdp2022 <- subset(gdp, Country == "Pakistan", select = c("Country", "2022"))
gdp2020 <- subset(gdp, Country == "Afghanistan", select = c("Country", "2020"))

df_gdp <- left_join(df, gdp2023, by = "Country")
df_gdp <- df_gdp %>%
  rename(GDP = `2023`) # Rename GDP column
df_gdp$GDP <- ifelse(df_gdp$Country == "Afghanistan", gdp2020[1, 2],
                     ifelse(df_gdp$Country == "Pakistan", gdp2022[1, 2], df_gdp$GDP))

worldbank <- read_excel("gdp_worldbank.xls")
worldbank <- worldbank[-c(1:2), ] %>%
  setNames(.[1, ]) # Make the first row as column names
worldbank <- worldbank[-1, ] %>%
  as.data.frame()

cuba_gdp <- subset(worldbank, `Country Name` == "Cuba", select = c(`Country Name`, `2020`))
df_gdp$GDP <- ifelse(df_gdp$Country == "Cuba", cuba_gdp[1, 2], df_gdp$GDP)
sum(is.na(df_gdp$GDP)) # All GDP is inserted
rm(gdp2020, gdp2022, gdp2023, cuba_gdp, gdp, worldbank) # Remove unused dataframe

df_gdp <- df_gdp %>%
  mutate(GDP = as.numeric(GDP)) %>%
  mutate(created_year = as.factor(created_year)) %>%
  mutate(subscribers_for_last_30_days_isBAD = as.factor(subscribers_for_last_30_days_isBAD)) %>%
  mutate(video_views_for_the_last_30_days_isBAD = as.factor(video_views_for_the_last_30_days_isBAD))

df_gdp$status <- ifelse(df_gdp$GDP < df_gdp$highest_yearly_earnings, 1, 0)

df_gdp.rose <- df_gdp[, -c(1:2, 8, 10, 15, 17)]

# Convert the Country column to a be numeric column
country_mapping <- c(
  "Afghanistan" = 1, "Andorra" = 2, "Argentina" = 3, "Australia" = 4,
  "Bangladesh" = 5, "Barbados" = 6, "Brazil" = 7, "Canada" = 8,
  "Chile" = 9, "China" = 10, "Colombia" = 11, "Cuba" = 12,
  "Ecuador" = 13, "Egypt" = 14, "El Salvador" = 15, "Finland" = 16,
  "France" = 17, "Germany" = 18, "India" = 19, "Indonesia" = 20,
  "Iraq" = 21, "Italy" = 22, "Japan" = 23, "Jordan" = 24,
  "Kuwait" = 25, "Latvia" = 26, "Malaysia" = 27, "Mexico" = 28,
  "Morocco" = 29, "Netherlands" = 30, "Pakistan" = 31, "Peru" = 32,
  "Philippines" = 33, "Russia" = 34, "Samoa" = 35, "Saudi Arabia" = 36,
  "Singapore" = 37, "South Korea" = 38, "Spain" = 39, "Sweden" = 40,
  "Switzerland" = 41, "Thailand" = 42, "Turkey" = 43, "Ukraine" = 44,
  "United Arab Emirates" = 45, "United Kingdom" = 46, "United States" = 47,
  "Venezuela" = 48, "Vietnam" = 49)
df_gdp.rose$Country <- country_mapping[df_gdp.rose$Country]
df_gdp.rose$Country <- as.factor(df_gdp.rose$Country)

# Convert the category column to a be numeric column
category_mapping <- c(
  "Autos & Vehicles" = 1, "Comedy" = 2, "Education" = 3, "Entertainment" = 4,
  "Film & Animation" = 5, "Gaming" = 6, "Howto & Style" = 7, "Movies" = 8,
  "Music" = 9, "News & Politics" = 10, "Nonprofits & Activism" = 11, "People & Blogs" = 12,
  "Pets & Animals" = 13, "Science & Technology" = 14, "Shows" = 15, "Sports" = 16,
  "Trailers" = 17, "Travel & Events" = 18)
df_gdp.rose$category <- category_mapping[df_gdp.rose$category]
df_gdp.rose$category <- as.factor(df_gdp.rose$category)

df.rose <- ROSE(status ~ ., data = df_gdp.rose, seed=123)$data

# a 90/10 split to form the training and test sets
set.seed(729375)
df.rose$rgroup <- runif(dim(df.rose)[1])
dTrainAll <- subset(df.rose, rgroup<=0.9)
dTest <- subset(df.rose, rgroup>0.9)
outcome <- c('status')
pos <- '1'
# names of columns that are categorical type and numerical type 
vars <- colnames(df.rose)[!(colnames(df.rose) %in% c("status", "rgroup"))]
catVars <- vars[sapply(dTrainAll[, vars], class) %in% c('factor', 'character')]
numericVars <- vars[sapply(dTrainAll[, vars], class) %in% c('numeric', 'integer')] 

# split dTrainAll into a training set and a validation (or calibration) set
useForCal <- rbinom(n=dim(dTrainAll)[1], size=1, prob=0.1)>0 
dCal <- subset(dTrainAll, useForCal)
dTrain <- subset(dTrainAll, !useForCal)
dTest <- dTest %>% filter(Country != 27)
#---------------------------
# Data for Clustering
df_gdp <- df_gdp %>%
  filter(!grepl("^ý.*ý$", Youtuber)) %>%
  mutate(Youtuber = gsub("ý", "", Youtuber)) %>%
  mutate(Youtuber = gsub("ï¿½", "", Youtuber))

# Convert the Country column to a be numeric column
country_mapping <- c(
  "Afghanistan" = 1, "Andorra" = 2, "Argentina" = 3, "Australia" = 4,
  "Bangladesh" = 5, "Barbados" = 6, "Brazil" = 7, "Canada" = 8,
  "Chile" = 9, "China" = 10, "Colombia" = 11, "Cuba" = 12,
  "Ecuador" = 13, "Egypt" = 14, "El Salvador" = 15, "Finland" = 16,
  "France" = 17, "Germany" = 18, "India" = 19, "Indonesia" = 20,
  "Iraq" = 21, "Italy" = 22, "Japan" = 23, "Jordan" = 24,
  "Kuwait" = 25, "Latvia" = 26, "Malaysia" = 27, "Mexico" = 28,
  "Morocco" = 29, "Netherlands" = 30, "Pakistan" = 31, "Peru" = 32,
  "Philippines" = 33, "Russia" = 34, "Samoa" = 35, "Saudi Arabia" = 36,
  "Singapore" = 37, "South Korea" = 38, "Spain" = 39, "Sweden" = 40,
  "Switzerland" = 41, "Thailand" = 42, "Turkey" = 43, "Ukraine" = 44,
  "United Arab Emirates" = 45, "United Kingdom" = 46, "United States" = 47,
  "Venezuela" = 48, "Vietnam" = 49)
df_gdp$Country <- country_mapping[df_gdp$Country]

# Convert the category column to a be numeric column
category_mapping <- c(
  "Autos & Vehicles" = 1, "Comedy" = 2, "Education" = 3, "Entertainment" = 4,
  "Film & Animation" = 5, "Gaming" = 6, "Howto & Style" = 7, "Movies" = 8,
  "Music" = 9, "News & Politics" = 10, "Nonprofits & Activism" = 11, "People & Blogs" = 12,
  "Pets & Animals" = 13, "Science & Technology" = 14, "Shows" = 15, "Sports" = 16,
  "Trailers" = 17, "Travel & Events" = 18)
df_gdp$category <- category_mapping[df_gdp$category]

df_gdp <- df_gdp %>%
  mutate(created_year = as.numeric(created_year)) %>%
  mutate(subscribers_for_last_30_days_isBAD = as.numeric(subscribers_for_last_30_days_isBAD)) %>%
  mutate(video_views_for_the_last_30_days_isBAD = as.numeric(video_views_for_the_last_30_days_isBAD))

df.clust <- df_gdp[, -c(1:2, 8, 15:16)] %>% # Exclude irreverent columns
  scale() %>% # Scale data
  as.data.frame()
#---------------------------
ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Data Visualization for Classification and Clustering"),
    tabsetPanel(
      tabPanel(
        "ROC of Single Variable Model",
        fluidRow(
          column(4, selectInput("selected_var", "Select Variable Type:", choices = c("Categorical", "Numerical"))
          ),
          column(4, conditionalPanel(
            condition = "input.selected_var == 'Categorical'",             
            selectInput("cat_var", "Select Feature:", choices = c("category", "Country", "created_year")),
          )),
          column(4, conditionalPanel(
            condition = "input.selected_var == 'Numerical'",             
            selectInput("num_var", "Select Feature:", choices = c("subscribers", "video.views", "uploads", "video_views_for_the_last_30_days", "subscribers_for_last_30_days")),
          )
        )),
        plotOutput("plot_ROC", width = "900px", height = "600px")
      ),
      
      tabPanel(
        "Logistic Regression Performance Evaluation",       
        HTML("<br>       
      <p>This panel shows you the performance of classification using logidtic regression on the test dataset.</p>            
           <br>"),       
        fluidRow(         
          column(4,                
                 selectInput("formula_vars1", "Select Variables for Formula:",
                             selected = colnames(df.rose)[2],                                   
                             choices = colnames(df.rose)[-c(11:12)], multiple = TRUE),         
          ),         
          div(id = "output-container", style = "text-align: center; position: relative;",
              verbatimTextOutput("glm_text"),             
          )       
        )     
      ),
    
    tabPanel(
      "Decision Tree Explorer",
      HTML("<br>
      <p>This panel shows you the decision tree plot and the performance of classification using decision tree on the test dataset.</p>
           <br>"),
      fluidRow(
        column(4,
               selectInput("formula_vars", "Select Variables for Formula:",
                                  selected = colnames(df.rose)[1],
                                  choices = colnames(df.rose)[-c(11:12)], multiple = TRUE),
        ),
        div(id = "output-container", style = "text-align: center; position: relative;",
            verbatimTextOutput("tree_text"),
            plotOutput("tree_plot")
        )
      )
    ),
    
    tabPanel(
      "Visualizing CH index and WSS",
      fluidRow(
        column(4,
               selectInput("selected_method", "Select Clustering Method:", 
                           choices = c("kmeans", "hclust"))
        ),
        column(4,
               conditionalPanel(
                 condition = "input.selected_method == 'hclust'",
                 selectInput("linkage", "Select Linkage:", choices = c("Complete", "Ward"))
               )
        ),
        column(4,
               selectInput("num", "Number of Clusters:", choices = 2:15)
        ),
        plotOutput("plot_CH", width = "900px", height = "600px")
      )
    ),
  
    tabPanel(
      "Visualizing Cluster by PCA",
      fluidRow(
        column(4,
               selectInput("selected_method.PCA", "Select Clustering Method:", 
                           choices = c("kmeans", "hclust"))
        ),
        column(4,
               selectInput("num.PCA", "Number of Clusters:", choices = 2:9)
        ),
        plotOutput("plot_PCA", width = "1200px", height = "700px")
      )
    ),
    
    tabPanel(
      "The Stats of Clusters",
      fluidRow(
        column(4,
               selectInput("selected_method.stat", "Select Clustering Method:", 
                           choices = c("kmeans", "hclust"))
        ),
        column(4,
               selectInput("num.stat", "Number of Clusters:", choices = 2:9)
        ),
        column(4,
               selectInput("stat", "Measures of Central Tendency.:", choices = c("mean", "median"))
        ),
        plotOutput("plot_stat", width = "900px", height = "600px")
      )
    )
  )
)

server <- function(input, output) {
  # Function to calculate predictions for categorical variables 
  mkPredC <- function(outCol, varCol, appCol) {
    pPos <- sum(outCol == pos) / length(outCol)
    naTab <- table(as.factor(outCol[is.na(varCol)]))
    pPosWna <- (naTab/sum(naTab))[pos]
    vTab <- table(as.factor(outCol), varCol)
    pPosWv <- (vTab[pos, ] + 1.0e-3*pPos) / (colSums(vTab) + 1.0e-3)
    pred <- pPosWv[appCol]
    pred[is.na(appCol)] <- pPosWna
    pred[is.na(pred)] <- pPos
    pred
  }
  
  # Function to calculate predictions for numerical variables 
  mkPredN <- function(outCol, varCol, appCol) {
    # compute the cuts
    cuts <- unique(
      quantile(varCol, probs=seq(0, 1, 0.1), na.rm=T))
    # discretize the numerical columns
    varC <- cut(varCol,cuts)
    appC <- cut(appCol,cuts)
    mkPredC(outCol,varC,appC)
  }
  
  # Function to generate ROC plot
  plot_roc <- function(predcol, outcol, colour_id=2, overlaid=F) {
    ROCit_obj <- rocit(score=predcol, class=outcol==pos)
    par(new=overlaid)
    plot(ROCit_obj, col = c(colour_id, 1),
         legend = FALSE, YIndex = FALSE, values = FALSE)
  }
  
  generateLogModel <- function(selected_vars) {
    if (!is.null(selected_vars)) {       
      formula_str <- paste("as.factor(status) ~", paste(selected_vars, collapse = " + "))       
      fV <- as.formula(formula_str)     
    } else {       
      fV <- as.formula("as.factor(status) ~ 1")     
    }          
    log_mod <- glm(fV, data=dTrain, family=binomial(link="logit"))     
    return(log_mod)   
  }
  
  performanceMeasures <- function(pred, truth, name = "model") {
    ctable <- table(truth = truth, pred = (pred > 0.5))
    accuracy <- sum(diag(ctable)) / sum(ctable)
    precision <- ctable[2, 2] / sum(ctable[, 2])
    recall <- ctable[2, 2] / sum(ctable[2, ])
    f1 <- 2 * precision * recall / (precision + recall)
    data.frame(model = name, precision = precision,
               recall = recall,
               f1 = f1, accuracy = accuracy)
  }
  # Function to generate performance measures
  pretty_perf_table <- function(model,training,test) {
    
    pred_train <- predict(model, newdata=dTrain)
    truth_train <- dTrain[, "status"]
    pred_test <- predict(model, newdata=dTest)
    truth_test <- dTest[, "status"]
    
    trainperf_tree <- performanceMeasures(
      pred_train, truth_train, "logistic, training")
    
    testperf_tree <- performanceMeasures(
      pred_test, truth_test, "logistic, test")
    
    perftable <- rbind(trainperf_tree, testperf_tree)
    return(perftable)
  }
  
  # Function to compute confusion matrix
  confusion_matrix <- function(ytrue, ypred) {
    ctable <- table(truth = ytrue, prediction = (ypred > 0.5))
  }
  
  # Function to compute AUC
  calculate_auc <- function(model, data, outcome_colname) {
    predictions <- predict(model, newdata=data, type="response")
    pred <- prediction(predictions, data[[outcome_colname]])
    perf <- performance(pred, "auc")
    auc <- as.numeric(perf@y.values[[1]])
    return(auc)
  }
  
  # Function to create Decision tree results
  calculate_accuracy <- function(T1) {
    diagonal_sum <- sum(diag(T1))
    total_sum <- sum(T1)
    accuracy <- diagonal_sum / total_sum
    return(accuracy)
  }
  
  generateDecisionTreePlot <- function(selected_vars) {
    if (!is.null(selected_vars)) {
      formula_str <- paste("as.factor(status) ~", paste(selected_vars, collapse = " + "))
      fV <- as.formula(formula_str)
    } else {
      fV <- as.formula("as.factor(status) ~ 1")
    }
    
    dt <- rpart(formula = fV, data = dTrain)
    return(dt)
  }
  
  # Function to return the squared Euclidean distance
  sqr_euDist <- function(x, y) {
    sum((x - y)^2)
  }
  
  # Function to calculate WSS of a cluster
  wss <- function(clustermat) {
    c0 <- colMeans(clustermat)
    sum(apply( clustermat, 1, FUN=function(row) {sqr_euDist(row, c0)} ))
  }
  
  # Function to calculate the total WSS
  wss_total <- function(scaled_df, labels) {
    wss.sum <- 0
    k <- length(unique(labels))
    for (i in 1:k) 
      wss.sum <- wss.sum + wss(subset(scaled_df, labels == i))
    wss.sum
  }
  
  # Function to calculate TSS
  tss <- function(scaled_df) {
    wss(scaled_df)
  }
  
  # Function to return the CH indices
  CH_index.c <- function(scaled_df, kmax, method="kmeans") {
    if (!(method %in% c("kmeans", "hclust"))) 
      stop("method must be one of c('kmeans', 'hclust')")
    
    npts <- nrow(scaled_df)
    wss.value <- numeric(kmax)
    wss.value[1] <- wss(scaled_df)
    
    if (method == "kmeans") {
      # kmeans
      for (k in 2:kmax) {
        set.seed(3064)
        clustering <- kmeans(scaled_df, k, nstart=10, iter.max=100)
        wss.value[k] <- clustering$tot.withinss
      } 
    } else {
      # hclust
      d <- dist(scaled_df, method="euclidean")
      pfit <- hclust(d, method="complete")
      for (k in 2:kmax) {
        labels <- cutree(pfit, k=k)
        wss.value[k] <- wss_total(scaled_df, labels)
      }
    }
    bss.value <- tss(scaled_df) - wss.value   
    B <- bss.value / (0:(kmax-1))             
    W <- wss.value / (npts - 1:kmax)          
    
    data.frame(k = 1:kmax, CH_index = B/W, WSS = wss.value)
  }
  
  # Function to return the CH indices
  CH_index.w <- function(scaled_df, kmax, method="kmeans") {
    if (!(method %in% c("kmeans", "hclust"))) 
      stop("method must be one of c('kmeans', 'hclust')")
    
    npts <- nrow(scaled_df)
    wss.value <- numeric(kmax)
    wss.value[1] <- wss(scaled_df)
    
    if (method == "kmeans") {
      # kmeans
      for (k in 2:kmax) {
        set.seed(3064)
        clustering <- kmeans(scaled_df, k, nstart=10, iter.max=100)
        wss.value[k] <- clustering$tot.withinss
      } 
    } else {
      # hclust
      d <- dist(scaled_df, method="euclidean")
      pfit <- hclust(d, method="ward.D2")
      for (k in 2:kmax) {
        labels <- cutree(pfit, k=k)
        wss.value[k] <- wss_total(scaled_df, labels)
      }
    }
    bss.value <- tss(scaled_df) - wss.value   
    B <- bss.value / (0:(kmax-1))             
    W <- wss.value / (npts - 1:kmax)          
    
    data.frame(k = 1:kmax, CH_index = B/W, WSS = wss.value)
  }
  
  princ <- prcomp(df.clust)
  nComp <- 2
  project2D <- as.data.frame(predict(princ, newdata=df.clust)[, 1:nComp])
  
  # Function to find convex hull
  find_convex_hull <- function(proj2Ddf, groups) {
    do.call(rbind, 
            lapply(unique(groups),
                   FUN = function(c) {
                     f <- subset(proj2Ddf, cluster==c); 
                     f[chull(f),]
                   }
            ) 
    )
  }
  
  # Function to generate k-means plot
  generate_kmeans_plot <- function(df, n) {
    fig_list <- list()
    kvalues <- seq(2, as.numeric(n))
    
    for (k in kvalues) {
      set.seed(3064)
      groups <- kmeans(df, k, nstart = 100, iter.max = 100)$cluster
      kmclust.project2D <- cbind(project2D, cluster = as.factor(groups), Youtuber = df_gdp$Youtuber)
      kmclust.hull <- find_convex_hull(kmclust.project2D, groups)
      
      fig <- ggplot(kmclust.project2D, aes(x = PC1, y = PC2)) +
        geom_point(aes(shape = cluster, color = cluster)) +
        geom_text(aes(label=Youtuber, color=cluster), hjust=0, vjust=1, size=3) + 
        geom_polygon(data = kmclust.hull, aes(group = cluster, fill = cluster), alpha = 0.4, linetype = 0) +
        labs(title = sprintf("k = %d", k)) + plot_theme
      
      fig_list[[paste0("fig", k)]] <- fig
    }
    
    num_plots_to_display <- length(kvalues)
    arranged_plots <- do.call("grid.arrange", fig_list[1:num_plots_to_display])
    
    return(arranged_plots)
  }
  
  # Function to generate hclust plot
  generate_hclust_plot <- function(df, n) {
    fig_list <- list()
    kvalues <- seq(2, as.numeric(n))
    
    for (k in kvalues) {
      d <- dist(df, method = "euclidean")
      pfit <- hclust(d, method = "ward.D2")
      groups <- cutree(pfit, k = k)
      hclust.project2D <- cbind(project2D, cluster = as.factor(groups), Youtuber = df_gdp$Youtuber)
      hclust.hull <- find_convex_hull(hclust.project2D, groups)
      
      fig <- ggplot(hclust.project2D, aes(x = PC1, y = PC2)) +
        geom_point(aes(shape = cluster, color = cluster)) +
        geom_text(aes(label=Youtuber, color=cluster), hjust=0, vjust=1, size=3) + 
        geom_polygon(data = hclust.hull, aes(group = cluster, fill = cluster), alpha = 0.4, linetype = 0) +
        labs(title = sprintf("k = %d", k)) + plot_theme
      
      fig_list[[paste0("fig", k)]] <- fig
    }
    
    num_plots_to_display <- length(kvalues)
    arranged_plots <- do.call("grid.arrange", fig_list[1:num_plots_to_display])
    
    return(arranged_plots)
  }
  
  # Function to generate cluster stats
  generate_cluster_stat <- function(df, method, num_clusters, stat) {
    if (method == "kmeans") {
      set.seed(3064)
      k <- kmeans(df, num_clusters, nstart = 100, iter.max = 100)
      df_with_clusters <- df %>% mutate(Cluster = k$cluster)
    } else {
      d <- dist(df, method = "euclidean")
      pfit <- hclust(d, method = "ward.D2")
      groups <- cutree(pfit, k = num_clusters)
      df_with_clusters <- df %>% mutate(Cluster = groups)
    }
    
    cluster_stat <- df_with_clusters %>% group_by(Cluster) %>% summarise_all(stat)
    
    ggplot(cluster_stat) +
      geom_line(aes(x = Cluster, y = subscribers, linetype = "Subscribers", color = "Subscribers")) +
      geom_line(aes(x = Cluster, y = video.views, linetype = "Video Views", color = "Video Views")) +
      geom_line(aes(x = Cluster, y = uploads, linetype = "Uploads", color = "Uploads")) +
      geom_line(aes(x = Cluster, y = video_views_for_the_last_30_days, linetype = "Video Views Last 30 Days", color = "Video Views Last 30 Days")) +
      geom_line(aes(x = Cluster, y = highest_yearly_earnings, linetype = "Highest Yearly Earnings", color = "Highest Yearly Earnings")) +
      geom_line(aes(x = Cluster, y = subscribers_for_last_30_days, linetype = "Subscribers Last 30 Days", color = "Subscribers Last 30 Days")) +
      scale_x_continuous(breaks = 1:num_clusters, labels = 1:num_clusters) +
      plot_theme +
      labs(title = "Mean of each feature by cluster", x = "cluster", y = "mean") +
      scale_color_manual(values = c("Subscribers" = "blue", "Video Views" = "green", "Uploads" = "red", "Video Views Last 30 Days" = "purple", "Highest Yearly Earnings" = "orange", "Subscribers Last 30 Days" = "black")) +
      scale_linetype_manual(values = c("Subscribers" = "solid", "Video Views" = "dashed", "Uploads" = "dotted", "Video Views Last 30 Days" = "longdash", "Highest Yearly Earnings" = "twodash", "Subscribers Last 30 Days" = "solid"))
  }
  
  glm_model <- reactive({     
    log_model <- generateLogModel(input$formula_vars1)     
    return(log_model)   
  })
  
  confusion_matrix_accuracy1 <- reactive({
    log_model <- glm_model()     
    perf <- pretty_perf_table(log_model, dTrain, dTest)
    p <- predict(log_model, newdata=dTest, type="response")
    cm <- confusion_matrix(dTest$status, p)
    auc <- calculate_auc(log_model, dTest, "status")
    result <- list(       
      performance = perf,       
      matrix = cm,
      aucScore = auc)     
    return(result)
  })
  
  output$glm_text <- renderPrint({
    req(confusion_matrix_accuracy1())     
    cm <- confusion_matrix_accuracy1()$matrix     
    perf <- confusion_matrix_accuracy1()$performance 
    auc <- confusion_matrix_accuracy1()$aucScore
    cat("Confusion Matrix: \n")     
    print(cm)     
    cat("Performance Measurement: \n")     
    print(perf)
    cat("AUC Score: \n")     
    print(round(auc,4))
  })
  
  decision_tree_model <- reactive({
    dt <- generateDecisionTreePlot(input$formula_vars)
    return(dt)
  })
  
  confusion_matrix_accuracy <- reactive({
    dt <- decision_tree_model()
    predicted_test <- predict(dt, newdata = dTest, type = "class")
    confusion_matrix <- table(dTest$status, predicted_test)
    accuracy <- calculate_accuracy(confusion_matrix)
    result <- list(
      confusion_matrix = confusion_matrix,
      accuracy = accuracy
    )
    return(result)
  })
  
  output$tree_plot <- renderPlot({
    req(decision_tree_model())
    rpart.plot(decision_tree_model())
  })
  
  output$tree_text <- renderPrint({
    req(confusion_matrix_accuracy())
    cm <- confusion_matrix_accuracy()$confusion_matrix
    accuracy <- confusion_matrix_accuracy()$accuracy
    cat("Confusion Matrix: \n")
    print(cm)
    cat("Accuracy: \n")
    print(round(accuracy, 4))
  })
  
  output$plot_CH <- renderPlot({
    if (input$selected_method == "kmeans") {
      crit.df <- CH_index.c(df.clust, as.numeric(input$num), method = "kmeans")
    } else if (input$selected_method == "hclust") {
      if (input$linkage == "Complete") {
        crit.df <- CH_index.c(df.clust, as.numeric(input$num), method = "hclust")
      } else if (input$linkage == "Ward") {
        crit.df <- CH_index.w(df.clust, as.numeric(input$num), method = "hclust")
      }
    }
    
    fig1 <- ggplot(crit.df, aes(x = k, y = CH_index)) +
      geom_point() + geom_line(colour = "red", size = 1) +
      scale_x_continuous(breaks = 1:15, labels = 1:15) +
      labs(y = "CH index") + plot_theme
    
    fig2 <- ggplot(crit.df, aes(x = k, y = WSS), color = "blue") +
      geom_point() + geom_line(colour = "blue", size = 1) +
      scale_x_continuous(breaks = 1:15, labels = 1:15) +
      plot_theme
    
    grid.arrange(fig1, fig2, nrow = 1)
  })
  
  output$plot_PCA <- renderPlot({
    if (input$selected_method.PCA == "kmeans") {
      generate_kmeans_plot(df.clust, input$num.PCA)
      
    } else if (input$selected_method.PCA == "hclust") {
      generate_hclust_plot(df.clust, input$num.PCA)
    }
  })
  
  output$plot_stat <- renderPlot({
    generate_cluster_stat(df.clust, input$selected_method.stat, input$num.stat, input$stat)
  })
  
  output$plot_ROC <- renderPlot({
    plot_roc(dTrain[, input$selected_var], dCal[, "status"])
      
  })
  
  output$plot_stat <- renderPlot({
    generate_cluster_stat(df.clust, input$selected_method.stat, input$num.stat, input$stat)
  })
  
  output$plot_ROC <- renderPlot({
    if(input$selected_var == 'Categorical'){
      p <- mkPredC(dTrain[,outcome], dTrain[,input$cat_var], dTest[,input$cat_var])
      plot_roc(p, dTest[,outcome])}
    else if (input$selected_var == 'Numerical'){
      p <- mkPredN(dTrain[,outcome], dTrain[,input$num_var], dTest[,input$num_var])
      plot_roc(p, dTest[,outcome])}
    
  })
  
}

shinyApp(ui, server)



# Ref
# https://shiny.posit.co/r/getstarted/build-an-app/customizing-ui/theming.html