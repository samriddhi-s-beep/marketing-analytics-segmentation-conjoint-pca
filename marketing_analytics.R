# =========================================================
# MARKETING ANALYTICS: SEGMENTATION, CONJOINT ANALYSIS & PCA
# Author: Samriddhi Soni
#
# 1. Customer segmentation (RFM, K-Means) and targeting (LDA)
# 2. Conjoint analysis of car attribute preferences
# 3. PCA perceptual map of car models
#
# Run from the project root so the data/ and outputs/ paths resolve.
# =========================================================

# ==================================================
# ========= 1. SEGMENTATION AND TARGETING ==========
# ==================================================

# ----------------------------------
# ======== LOAD LIBRARIES =========
# ----------------------------------

library(readxl)
library(MASS)
library(tidyverse)

set.seed(1)

# Load Dataset
customers <- read_excel("data/current_customers.xlsx")

# Basic Inspection
View(customers)
head(customers)
str(customers)
summary(customers)
dim(customers)

# Count missing values per column
colSums(is.na(customers))

# Convert "NA" strings in CustomerID into actual missing values
customers$CustomerID[customers$CustomerID == "NA"] <- NA

# Check missing values again
colSums(is.na(customers))

# Remove rows with missing descriptions
customers <- customers %>%
  filter(!is.na(Description))

# Convert InvoiceDate to proper datetime
customers$InvoiceDate <- as.POSIXct(
  customers$InvoiceDate,
  format = "%Y-%m-%dT%H:%M"
)

# Check structure again
str(customers)

# Dataset dimensions after cleaning
dim(customers)

# --------------------------------------
# ====== BASIC EXPLORATORY ANALYSIS ====
# --------------------------------------
# Summary statistics for numeric variable
summary(select(customers,
               Quantity,
               UnitPrice,
               household_size,
               income,
               age))

# HISTOGRAMS
hist(customers$UnitPrice,
     main = "Distribution of Unit Price",
     xlab = "Unit Price")

hist(customers$Quantity,
     main = "Distribution of Quantity",
     xlab = "Quantity")

hist(customers$income,
     main = "Distribution of Income",
     xlab = "Income")

# BOXPLOTS
boxplot(customers$Quantity,
        main = "Boxplot of Quantity")

boxplot(customers$UnitPrice,
        main = "Boxplot of Unit price")

# CORRELATION MATRIX
numeric_data <- customers %>%
  select(Quantity, UnitPrice, household_size,
         income, age, Work, Education)

cor(numeric_data)

# ----------------------------
# ==== TRANSACTION VALUES ====
# ----------------------------
# Total transaction value
customers$Sales <- customers$Quantity * customers$UnitPrice

# Summary of Sales
summary(customers$Sales)

# Check sales distribution
hist(customers$Sales,
     main = "Distribution of Sales",
     xlab = "Sales",
     col = "lightblue1",
     border = "white")

# Log-transformed histogram
hist(log(customers$Sales + 1),
     main = "Log Distribution of Sales",
     xlab = "Log Sales",
     col = "pink",
     border = "black")

# Boxplot
boxplot(customers$Sales,
        main = "Boxplot of Sales")

# ------------------------------
# === CUSTOMER-LEVEL DATASET ===
# ------------------------------
# Because segmentation should happen at customer level not transaction level

# Remove rows with missing CustomerID
customer_data <- customers %>%
  filter(!is.na(CustomerID))

# Create customer-level summary
customer_summary <- customer_data %>%
  group_by(CustomerID) %>%
  summarise(
    Frequency = n(),
    Monetary = sum(Sales),
    Avg_Quantity = mean(Quantity),
    Avg_Price = mean(UnitPrice),
    married = first(married),
    household_size = first(household_size),
    income = first(income),
    age = first(age),
    Work = first(Work),
    Education = first(Education)
  )

# Check dimensions
dim(customer_summary)

# Summary statistics
summary(customer_summary)

# -------------------------------------------
# ==== VISUALISE CUSTOMER LEVEL OUTLIERS ====
# -------------------------------------------

# Boxplots for customer-level variables

boxplot(customer_summary$Monetary,
        main = "Monetary Value Distribution")

boxplot(customer_summary$Frequency,
        main = "Frequency Distribution")

boxplot(customer_summary$Avg_Quantity,
        main = "Average Quantity Distribution")

# Histograms

hist(customer_summary$Monetary,
     main = "Monetary Histogram",
     xlab = "Monetary")

hist(log(customer_summary$Monetary + 1),
     main = "Log Monetary Histogram",
     xlab = "Log Monetary")

# --------------------------
# ==== RECENCY VARIABLE ====
# --------------------------

# Find latest purchase date in dataset
max_date <- max(customer_data$InvoiceDate)

# Create RFM table
rfm_data <- customer_data %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(max_date - max(InvoiceDate)),
    Frequency = n(),
    Monetary = sum(Sales)
  )

# Summary statistics
summary(rfm_data)

# ----------------------------------
# ==== INDEPENDENT RFM SORTING ====
# ----------------------------------

# Using quartiles

rfm_scores <- rfm_data %>%
  mutate(
    R_score = ntile(-Recency, 4),
    F_score = ntile(Frequency, 4),
    M_score = ntile(Monetary, 4)
  )

# Create combined RFM score
rfm_scores <- rfm_scores %>%
  mutate(
    RFM_Score = paste(R_score, F_score, M_score, sep = "")
  )

# Frequency table of RFM scores
table(rfm_scores$RFM_Score)


# ----------------------------------
# ==== SEQUENTIAL RFM SORTING ====
# ----------------------------------
rfm_sequential <- rfm_data %>%
  
  # Step 1: Sort by Recency
  mutate(R_group = ntile(-Recency, 4)) %>%
  
  # Step 2: Within Recency groups, sort by Frequency
  group_by(R_group) %>%
  mutate(F_group = ntile(Frequency, 4)) %>%
  
  # Step 3: Within R and F groups, sort by Monetary
  group_by(R_group, F_group) %>%
  mutate(M_group = ntile(Monetary, 4)) %>%
  
  ungroup()

# Create combined score
rfm_sequential <- rfm_sequential %>%
  mutate(
    Sequential_RFM = paste(R_group, F_group, M_group, sep = "")
  )

# View segment counts
table(rfm_sequential$Sequential_RFM)

# ---------------------------------
# ==== CUSTOMER SEGMENT LABELS ====
# ---------------------------------
rfm_scores <- rfm_scores %>%
  mutate(
    Segment = case_when(
      R_score >= 3 & F_score >= 3 & M_score >= 3 ~ "Champions",
      R_score >= 3 & F_score >= 2 ~ "Loyal Customers",
      R_score >= 3 & M_score >= 3 ~ "Big Spenders",
      R_score <= 2 & F_score >= 3 ~ "At Risk",
      R_score <= 2 & F_score <= 2 & M_score <= 2 ~ "Low Value",
      TRUE ~ "Potential Customers"
    )
  )

# Segment counts
table(rfm_scores$Segment)

# ----------------------------------------------
# ==== STANDARDIZE VARIABLES FOR CLUSTERING ====
# ----------------------------------------------
# Because without scaling, Monetary dominates clustering
  
# Select clustering variables
cluster_data <- rfm_data %>%
  select(Recency, Frequency, Monetary)

# Standardize variables
cluster_scaled <- scale(cluster_data)

# Check summary
summary(cluster_scaled)

# Check standard deviations
apply(cluster_scaled, 2, sd)

# ----------------------------------
# ==== HIERARCHICAL CLUSTERING ====
# ----------------------------------
# Compute distance matrix
dist_matrix <- dist(cluster_scaled)

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "complete")

# Plot dendrogram
plot(hc,
     main = "Hierarchical Clustering Dendrogram",
     xlab = "",
     sub = "")

# -----------------------
# ==== ELBOW METHOD ====
# -----------------------
# Use elbow method to identify optimal number of clusters
# based on within-cluster sum of squares (WCSS)
# Calculate WCSS

wcss <- numeric(10)

for(i in 1:10) {
  km <- kmeans(cluster_scaled,
               centers = i,
               nstart = 20)
  
  wcss[i] <- km$tot.withinss
}

# Plot elbow curve

plot(1:10, wcss,
     type = "b",
     pch = 19,
     frame = FALSE,
     xlab = "Number of Clusters",
     ylab = "WCSS",
     main = "Elbow Method")

# ----------------------------
# ==== K MEANS CLUSTERING ====
# ----------------------------

# Perform K-means clustering with multiple random starts
# to improve clustering stability and avoid local optima
# K-means clustering with 4 clusters
set.seed(1)

kmeans_4 <- kmeans(cluster_scaled,
                   centers = 4,
                   nstart = 20)

# View cluster assignments
table(kmeans_4$cluster)

# Cluster centers
kmeans_4$centers

# Add clusters to RFM data
rfm_clustered <- rfm_data %>%
  mutate(Cluster = as.factor(kmeans_4$cluster))

# K-means clustering with 3 clusters

set.seed(1)

kmeans_3 <- kmeans(cluster_scaled,
                   centers = 3,
                   nstart = 20)

# Cluster sizes
table(kmeans_3$cluster)

# Cluster centers
kmeans_3$centers

# ----------------------------------------
# ==== ADD CLUSTER LABELS & PROFILING ====
# ----------------------------------------
# Add 3-cluster solution to dataset

rfm_clustered <- rfm_data %>%
  mutate(Cluster = as.factor(kmeans_3$cluster))

# Cluster summary statistics
# Profile clusters using average RFM values
# to understand customer behavior patterns
cluster_profile <- rfm_clustered %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    Count = n()
  )

cluster_profile

# Although a 4-cluster solution was explored,
# the 3-cluster solution provided better managerial
# interpretability and more balanced segmentation

# ---------------------------------------
# ==== ADD MANAGERIAL CLUSTER LABELS ====
# ---------------------------------------
# Assign meaningful business oriented labels 
# to improve interpretation of customer segments
rfm_clustered <- rfm_clustered %>%
  mutate(
    Cluster_Name = case_when(
      Cluster == 1 ~ "Elite Customers",
      Cluster == 2 ~ "Disengaged Customers",
      Cluster == 3 ~ "Regular Customers"
    )
  )

# Check counts
table(rfm_clustered$Cluster_Name)

# --------------------------------
# ==== SEGMENT VISUALIZATIONS ====
# Boxplot: Monetary by Cluster----
boxplot(Monetary ~ Cluster,
        data = rfm_clustered,
        main = "Monetary Value by Cluster",
        xlab = "Cluster",
        ylab = "Monetary Value",
        col = "lightblue")

# Boxplot: Frequency by Cluster
boxplot(Frequency ~ Cluster,
        data = rfm_clustered,
        main = "Frequency by Cluster",
        xlab = "Cluster",
        ylab = "Frequency",
        col = "lightgreen")

# Boxplot: Recency by Cluster
boxplot(Recency ~ Cluster,
        data = rfm_clustered,
        main = "Recency by Cluster",
        xlab = "Cluster",
        ylab = "Recency",
        col = "lightpink")

# Scatterplot: Frequency vs Monetary
plot(rfm_clustered$Frequency,
     rfm_clustered$Monetary,
     col = rfm_clustered$Cluster,
     pch = 19,
     main = "Frequency vs Monetary by Cluster",
     xlab = "Frequency",
     ylab = "Monetary")

# --------------------------
# ==== ANOVA VALIDATION ====
# --------------------------
# Test whether cluster means differ 
# significantly across RFM variables

# Recency ANOVA
anova_recency <- aov(Recency ~ Cluster,
                     data = rfm_clustered)

summary(anova_recency)

# Frequency ANOVA
anova_frequency <- aov(Frequency ~ Cluster,
                       data = rfm_clustered)

summary(anova_frequency)

# Monetary ANOVA
anova_monetary <- aov(Monetary ~ Cluster,
                      data = rfm_clustered)

summary(anova_monetary)

# ------------------------------
# ==== PREPARE DATA FOR LDA ====
# ------------------------------
# Merge demographic variables with clustered data

lda_data <- customer_summary %>%
  select(CustomerID,
         married,
         household_size,
         income,
         age,
         Work,
         Education) %>%
  
  left_join(
    rfm_clustered %>%
      select(CustomerID, Cluster_Name),
    
    by = "CustomerID"
  )

# Check structure
str(lda_data)

# -------------------------------
# ==== DISCRIMINANT ANALYSIS ====
# -------------------------------
# Convert cluster labels to factor
lda_data$Cluster_Name <- as.factor(lda_data$Cluster_Name)

# Run LDA model

lda_model <- lda(
  Cluster_Name ~ married +
    household_size +
    income +
    age +
    Work +
    Education,
  
  data = lda_data
)

# Print model results
lda_model

# ----------------------------------
# ==== CONFUSION MATRIX ====
# ----------------------------------
# Evaluate classification performance
# by comparing actual and predicted segments

# Predict segment membership
lda_pred <- predict(lda_model)

# Predicted classes
predicted_clusters <- lda_pred$class

# Create confusion matrix
conf_matrix <- table(
  Actual = lda_data$Cluster_Name,
  Predicted = predicted_clusters
)

# Print confusion matrix
conf_matrix

# Classification accuracy
sum(diag(conf_matrix)) / sum(conf_matrix)

# -------------------------------------
# ==== LOAD PROSPECT CUSTOMER DATA ====
# -------------------------------------
prospects <- read_excel("data/prospect_customers.xlsx")

# Basic Inspection
str(prospects)
colSums(is.na(prospects))
dim(prospects)
summary(prospects)

# -------------------------------------
# ==== CLASSIFY PROSPECT CUSTOMERS ====
# -------------------------------------
# Classify prospect customers into predicted segments
# using the discriminant analysis model

# Predict prospect segments
prospect_predictions <- predict(
  lda_model,
  prospects
)

# Extract predicted classes
prospects$Predicted_Segment <- prospect_predictions$class

# Segment counts
table(prospects$Predicted_Segment)

# View updated dataset
View(prospects)

# =========================================================
# ================ 2. CONJOINT ANALYSIS ===================
# =========================================================

# ----------------------------
# ==== LOAD CONJOINT DATA ====
# ----------------------------
# Product profiles dataset
profiles <- read_excel("data/product_profiles.xlsx")

# Preference ratings dataset
preferences <- read_excel("data/conjoint_preferences.xlsx")

# Inspect datasets
str(profiles)
str(preferences)
dim(profiles)
dim(preferences)

# Missing values
colSums(is.na(profiles))
colSums(is.na(preferences))

# ----------------------------------
# ==== INSPECT ATTRIBUTE LEVELS ====
# ----------------------------------

# View unique values for each attribute

unique(profiles$Price)
unique(profiles$Brand)
unique(profiles$Horsepower)
unique(profiles$Color)
unique(profiles$Sunroof)

# -------------------------------
# ==== PREPARE CONJOINT DATA ====
# -------------------------------
# Convert categorical variables to factors

profiles$Price <- as.factor(profiles$Price)
profiles$Brand <- as.factor(profiles$Brand)
profiles$Horsepower <- as.factor(profiles$Horsepower)
profiles$Color <- as.factor(profiles$Color)
profiles$Sunroof <- as.factor(profiles$Sunroof)

# Check structure
str(profiles)

# -------------------------------
# ==== MEAN PREFERENCE SCORE ====
# -------------------------------
# Calculate average preference across respondents

profiles$Preference <- rowMeans(
  preferences[, 2:16]
)

# View updated dataset
View(profiles)

# Summary
summary(profiles$Preference)

# -----------------------------------
# ==== CONJOINT REGRESSION MODEL ====
# -----------------------------------
# Run conjoint regression

conjoint_model <- lm(
  Preference ~ Price +
    Brand +
    Horsepower +
    Color +
    Sunroof,
  
  data = profiles
)

# View model summary
summary(conjoint_model)

# ------------------------------
# ==== ATTRIBUTE IMPORTANCE ====
# ------------------------------
# Estimate customer utilities for automobile attributes
# Create utility ranges

price_range <- max(c(0,
                     coef(conjoint_model)[2],
                     coef(conjoint_model)[3],
                     coef(conjoint_model)[4])) -
  min(c(0,
        coef(conjoint_model)[2],
        coef(conjoint_model)[3],
        coef(conjoint_model)[4]))

brand_range <- max(c(0,
                     coef(conjoint_model)[5],
                     coef(conjoint_model)[6],
                     coef(conjoint_model)[7])) -
  min(c(0,
        coef(conjoint_model)[5],
        coef(conjoint_model)[6],
        coef(conjoint_model)[7]))

hp_range <- max(c(0,
                  coef(conjoint_model)[8],
                  coef(conjoint_model)[9])) -
  min(c(0,
        coef(conjoint_model)[8],
        coef(conjoint_model)[9]))

color_range <- max(c(0,
                     coef(conjoint_model)[10],
                     coef(conjoint_model)[11])) -
  min(c(0,
        coef(conjoint_model)[10],
        coef(conjoint_model)[11]))

sunroof_range <- max(c(0,
                       coef(conjoint_model)[12])) -
  min(c(0,
        coef(conjoint_model)[12]))

# Combine ranges
ranges <- c(price_range,
            brand_range,
            hp_range,
            color_range,
            sunroof_range)

# Calculate importance percentages
importance <- ranges / sum(ranges) * 100

# Create table
importance_table <- data.frame(
  Attribute = c("Price",
                "Brand",
                "Horsepower",
                "Color",
                "Sunroof"),
  
  Importance = importance
)

importance_table

# --------------------------------------------
# ==== ATTRIBUTE IMPORTANCE VISUALIZATION ====
# --------------------------------------------
barplot(
  importance_table$Importance,
  
  names.arg = importance_table$Attribute,
  
  main = "Attribute Importance in Customer Preferences",
  
  ylab = "Importance (%)",
  
  col = c("lightblue",
          "lightgreen",
          "lightpink",
          "lavender",
          "lightyellow")
)

# ---------------------------
# ==== PREDICT UTILITIES ====
# ---------------------------
# Predict utility scores for each product profile

profiles$Predicted_Utility <- predict(conjoint_model)

# View utilities
View(profiles)

# Sort profiles by predicted utility
profiles_ranked <- profiles %>%
  arrange(desc(Predicted_Utility))

# View ranked profiles
View(profiles_ranked)

# Top profiles
head(profiles_ranked)

# =========================================================
# ================= 3. PCA / PERCEPTUAL MAP ===============
# =========================================================

# --------------------------
# ==== LOAD PCA DATASET ====
# --------------------------
cars_pca <- read_excel("data/car_attributes.xlsx")

# Basic Inspection
View(cars_pca)
str(cars_pca)
dim(cars_pca)

# Missing values
colSums(is.na(cars_pca))

# --------------------------
# ==== PREPARE PCA DATA ====
# --------------------------
# Remove model names
cars_numeric <- cars_pca %>%
  select(-Model)

# Standardize variables
cars_scaled <- scale(cars_numeric)
summary(cars_scaled)

# Standard deviations
apply(cars_scaled, 2, sd)

# -----------------
# ==== RUN PCA ====
# -----------------
# PCA helps determine the hidden dimensions consumers use
# when evaluating automobiles
pca_model <- prcomp(
  cars_scaled,
  center = TRUE,
  scale. = TRUE
)

# PCA summary
summary(pca_model)

# Standard deviations
pca_model$sdev

# Rotation matrix (loadings)
pca_model$rotation

# PCA BIPLOT
biplot(
  pca_model,
  scale = 0,
  cex = 0.7
)

# -----------------------------
# === POWER BI EXPORT FILES ===
# -----------------------------
dir.create("outputs", showWarnings = FALSE)

# Customer-level cluster data
write.csv(rfm_clustered,
          "outputs/rfm_clustered.csv",
          row.names = FALSE)

# Cluster profile
write.csv(cluster_profile,
          "outputs/cluster_profile.csv",
          row.names = FALSE)

# Confusion matrix
write.csv(as.data.frame(conf_matrix),
          "outputs/confusion_matrix.csv",
          row.names = FALSE)

# Prospect classification
write.csv(prospects,
          "outputs/prospect_classification.csv",
          row.names = FALSE)

# Conjoint importance table
write.csv(importance_table,
          "outputs/attribute_importance.csv",
          row.names = FALSE)

# Ranked conjoint profiles
write.csv(profiles_ranked,
          "outputs/profiles_ranked.csv",
          row.names = FALSE)

# =========================================================
# ======================= END =============================
# =========================================================
