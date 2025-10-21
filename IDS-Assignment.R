datasets::iris
# Data Cleaning and Transformation in R (Iris Dataset)
 # ---------------------------------------------------
 
   # Load libraries
   library(dplyr)     # for data manipulation
 library(ggplot2)   # for visualization
 library(readr)     # for consistency (not used but kept for structure)
 
  # Load dataset
   data(iris)
 
   # Quick overview ----
  glimpse(iris)
Rows: 150
Columns: 5
$ Sepal.Length <dbl> 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4, 4.9, 5.4, …
$ Sepal.Width  <dbl> 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.1, 3.7, …
$ Petal.Length <dbl> 1.4, 1.4, 1.3, 1.5, 1.4, 1.7, 1.4, 1.5, 1.4, 1.5, 1.5, …
$ Petal.Width  <dbl> 0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.3, 0.2, 0.2, 0.1, 0.2, …
$ Species      <fct> setosa, setosa, setosa, setosa, setosa, setosa, setosa,…
 summary(iris)
Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
Median :5.800   Median :3.000   Median :4.350   Median :1.300  
Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
Species  
setosa    :50  
versicolor:50  
virginica :50  



head(iris, 10)
Sepal.Length Sepal.Width Petal.Length Petal.Width Species
1           5.1         3.5          1.4         0.2  setosa
2           4.9         3.0          1.4         0.2  setosa
3           4.7         3.2          1.3         0.2  setosa
4           4.6         3.1          1.5         0.2  setosa
5           5.0         3.6          1.4         0.2  setosa
6           5.4         3.9          1.7         0.4  setosa
7           4.6         3.4          1.4         0.3  setosa
8           5.0         3.4          1.5         0.2  setosa
9           4.4         2.9          1.4         0.2  setosa
10          4.9         3.1          1.5         0.1  setosa
 
   
   # Handling Missing Values ----
 # Check total missing values
  > sum(is.na(iris))
[1] 0

   # Count missing values per column
   colSums(is.na(iris))
Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
0            0            0            0            0 
 
   # Example: replace missing numeric values (if any) with column mean
   iris_clean <- iris %>%
  +     mutate(across(where(is.numeric), 
                      +                   ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
 
   # Verify no missing values remain
   colSums(is.na(iris_clean))
Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
0            0            0            0            0 
 
   
   # Handling Duplicates ----
 # Count duplicates
   sum(duplicated(iris_clean))
[1] 1

  # Remove duplicates
  iris_clean <- iris_clean %>% distinct()
 
   # Verify structure again
   str(iris_clean)
'data.frame':	149 obs. of  5 variables:
  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
$ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
$ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
$ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
$ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

   
   # Verify and Adjust Data Types ----
 # Convert Species to factor (if not already)
   iris_clean <- iris_clean %>%
  +     mutate(Species = as.factor(Species))

   # Check unique species
   unique(iris_clean$Species)
[1] setosa     versicolor virginica 
Levels: setosa versicolor virginica
 
   
   # Outlier Handling ----
 # Boxplot for outlier detection in Petal.Length
   ggplot(iris_clean, aes(x = Species, y = Petal.Length, fill = Species)) +
  +     geom_boxplot() +
  +     labs(title = "Outlier Detection in Petal Length", y = "Petal Length (cm)")
 
   # Winsorize: cap Petal.Length at 3 SD from mean
   mean_petal <- mean(iris_clean$Petal.Length, na.rm = TRUE)
 sd_petal <- sd(iris_clean$Petal.Length, na.rm = TRUE)
 
 iris_clean <- iris_clean %>%
  +     mutate(Petal.Length = ifelse(Petal.Length > mean_petal + 3*sd_petal,
                                     +                                  mean_petal + 3*sd_petal,
                                     +                                  Petal.Length))
   
   # Feature Engineering ----
 iris_clean <- iris_clean %>%
  +     mutate(
    +         Sepal.Area = Sepal.Length * Sepal.Width,     # new feature: sepal area
    +         Petal.Area = Petal.Length * Petal.Width,     # new feature: petal area
    +         Sepal.Ratio = Sepal.Length / Sepal.Width     # ratio feature
    +     )
 
 # Normalize/scale numeric columns
   iris_clean <- iris_clean %>%
  +     mutate(across(where(is.numeric), scale))
 
   
   # Data Summarization and Aggregation ----
 # Average Petal.Length by Species
   iris_clean %>%
  +     group_by(Species) %>%
  +     summarise(avg_petal_length = mean(Petal.Length)) %>%
  +     arrange(desc(avg_petal_length))
# A tibble: 3 × 2
Species    avg_petal_length
<fct>                 <dbl>
  1 virginica             1.03 
2 versicolor            0.289
3 setosa               -1.29 

   # Count observations per species
  iris_clean %>%
  +     group_by(Species) %>%
  +     summarise(count = n())
# A tibble: 3 × 2
Species    count
<fct>      <int>
  1 setosa        50
2 versicolor    50
3 virginica     49
 
   
   # Data Visualization After Cleaning ----
 # Trend (comparison) of average Petal.Length by Species
   iris_clean %>%
  +     group_by(Species) %>%
  +     summarise(avg_petal_length = mean(Petal.Length)) %>%
  +     ggplot(aes(x = Species, y = avg_petal_length, fill = Species)) +
  +     geom_bar(stat = "identity") +
  +     labs(title = "Average Petal Length by Species", x = "Species", y = "Avg Petal Length (scaled)") +
  +     theme_minimal()
 
  > # Histogram of Sepal.Length
  > ggplot(iris_clean, aes(x = Sepal.Length)) +
  +     geom_histogram(binwidth = 0.2, fill = "lightblue", color = "black", alpha = 0.7) +
  +     labs(title = "Distribution of Sepal Length (Scaled)", x = "Sepal Length", y = "Frequency")

  

# UNIVARIANT QUESTIONS 



# Load necessary libraries
 library(dplyr)
 library(ggplot2)
 
   # Load dataset
   data(iris)
 iris_clean <- iris
 
   # -------------------------------------------------------
 # 1. General Structure and Overview
   # -------------------------------------------------------
 
   # Q1: How many rows and columns are there?
   dim(iris_clean)
[1] 150   5

 # -------------------------------------------------------
 
 # 2. Sepal.Length Analysis
    # -------------------------------------------------------
 
   # Q6: What is the mean and median Sepal.Length?
    mean(iris_clean$Sepal.Length)
 [1] 5.843333
  median(iris_clean$Sepal.Length)
 [1] 5.8
 
    # Q7: What are the min and max Sepal.Length?
    range(iris_clean$Sepal.Length)
 [1] 4.3 7.9
 
    # Q8: What is the standard deviation of Sepal.Length?
    sd(iris_clean$Sepal.Length)
 [1] 0.8280661
 
    # Q9: Visualize distribution (histogram)
    ggplot(iris_clean, aes(x = Sepal.Length)) +
   +     geom_histogram(binwidth = 0.3, fill = "skyblue", color = "black") +
   +     labs(title = "Distribution of Sepal.Length", x = "Sepal Length", y = "Frequency")
  
   # Q10: Detect potential outliers (boxplot)
   boxplot(iris_clean$Sepal.Length, main = "Boxplot of Sepal.Length", col = "lightgreen")
  
    
    # -------------------------------------------------------
 # 3. Sepal.Width Analysis
    # -------------------------------------------------------
  
    # Q11: Basic statistics of Sepal.Width
    mean(iris_clean$Sepal.Width)
 [1] 3.057333
  sd(iris_clean$Sepal.Width)
 [1] 0.4358663
  summary(iris_clean$Sepal.Width)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 2.000   2.800   3.000   3.057   3.300   4.400 

    # Q12: Distribution plot
    ggplot(iris_clean, aes(x = Sepal.Width)) +
   +     geom_histogram(binwidth = 0.2, fill = "orange", color = "black") +
   +     labs(title = "Distribution of Sepal.Width", x = "Sepal Width", y = "Frequency")
 
 
   # Q13: Boxplot to detect outliers
   boxplot(iris_clean$Sepal.Width, main = "Boxplot of Sepal.Width", col = "lightpink")
 
    
    # -------------------------------------------------------
  # 4. Petal.Length Analysis
    # -------------------------------------------------------
 
    # Q14: Mean, Median, Range of Petal.Length
    mean(iris_clean$Petal.Length)
 [1] 3.758
  median(iris_clean$Petal.Length)
 [1] 4.35
  range(iris_clean$Petal.Length)
 [1] 1.0 6.9
 
    # Q15: Histogram
    ggplot(iris_clean, aes(x = Petal.Length)) +
   +     geom_histogram(binwidth = 0.3, fill = "lightblue", color = "black") +
   +     labs(title = "Distribution of Petal.Length", x = "Petal Length", y = "Frequency")
  
    # Q16: Boxplot for outlier detection
    ggplot(iris_clean, aes(y = Petal.Length)) +
   +     geom_boxplot(fill = "lightgreen") +
   +     labs(title = "Boxplot of Petal.Length", y = "Petal Length")
 
    
    # -------------------------------------------------------
  # 5. Petal.Width Analysis
    # -------------------------------------------------------
 
    # Q17: Mean, SD, and summary of Petal.Width
   mean(iris_clean$Petal.Width)
 [1] 1.199333
  sd(iris_clean$Petal.Width)
 [1] 0.7622377
  summary(iris_clean$Petal.Width)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.100   0.300   1.300   1.199   1.800   2.500 
  
    # Q18: Histogram
    ggplot(iris_clean, aes(x = Petal.Width)) +
   +     geom_histogram(binwidth = 0.2, fill = "violet", color = "black") +
   +     labs(title = "Distribution of Petal.Width", x = "Petal Width", y = "Frequency")
 
    # Q19: Boxplot
    boxplot(iris_clean$Petal.Width, main = "Boxplot of Petal.Width", col = "lightyellow")
 
    
    # -------------------------------------------------------
  # 6. Species (Categorical Variable)
    # -------------------------------------------------------
  
    # Q20: How many unique species exist?
   unique(iris_clean$Species)
 [1] setosa     versicolor virginica 
 Levels: setosa versicolor virginica
 
    # Q21: Frequency count of each species
    table(iris_clean$Species)
 
 setosa versicolor  virginica 
 50         50         50 
 
    # Q22: Visualize species count (bar chart)
    ggplot(iris_clean, aes(x = Species, fill = Species)) +
   +     geom_bar() +
   +     labs(title = "Count of Each Species", x = "Species", y = "Count") +
   +     theme_minimal()
  
    
    # -------------------------------------------------------
  # 7. Engineered Variables (Optional, if created earlier)
    # -------------------------------------------------------
  
    # Example: Create new features
    iris_clean <- iris_clean %>%
   +     mutate(Sepal.Area = Sepal.Length * Sepal.Width,
                +            Petal.Area = Petal.Length * Petal.Width)
 
   # Q23: Summary of Sepal.Area and Petal.Area
   summary(iris_clean$Sepal.Area)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 10.00   15.66   17.66   17.82   20.32   30.02 
  summary(iris_clean$Petal.Area)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.110   0.420   5.615   5.794   9.690  15.870 
 
    # Q24: Histogram of Sepal.Area
    ggplot(iris_clean, aes(x = Sepal.Area)) +
   +     geom_histogram(binwidth = 0.3, fill = "cyan", color = "black") +
   +     labs(title = "Distribution of Sepal.Area", x = "Sepal Area", y = "Frequency")
  
    # Q25: Boxplot for Petal.Area
    ggplot(iris_clean, aes(y = Petal.Area)) +
   +     geom_boxplot(fill = "coral") +
   +     labs(title = "Boxplot of Petal.Area", y = "Petal Area")
  
   
    # -------------------------------------------------------
  # 8. Statistical Insights
    # -------------------------------------------------------
  
    # Q26: Which numeric variable has the highest variance?
    sapply(iris_clean[, 1:4], var)
 Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
 0.6856935    0.1899794    3.1162779    0.5810063 
  
    # Q27: Check for normality (using Shapiro test)
    shapiro.test(iris_clean$Sepal.Length)
 
 Shapiro-Wilk normality test
 
 data:  iris_clean$Sepal.Length
 W = 0.97609, p-value = 0.01018
 
  shapiro.test(iris_clean$Sepal.Width)
 
 Shapiro-Wilk normality test
 
 data:  iris_clean$Sepal.Width
 W = 0.98492, p-value = 0.1012
 
  shapiro.test(iris_clean$Petal.Length)
 
 Shapiro-Wilk normality test
 
 data:  iris_clean$Petal.Length
 W = 0.87627, p-value = 7.412e-10
 
  shapiro.test(iris_clean$Petal.Width)
 
 Shapiro-Wilk normality test
 
 data:  iris_clean$Petal.Width
 W = 0.90183, p-value = 1.68e-08
 
  
    # Q28: Identify numeric columns that may need transformation
    skewness_check <- sapply(iris_clean[, 1:4], function(x) mean((x - mean(x))^3) / sd(x)^3)
  skewness_check
 Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
 0.3086407    0.3126147   -0.2694109   -0.1009166 
 
 
 
 
 
 #BIVARIANT QUESTIONS
 
 
 
 
 
 # 🌼 BIVARIATE EDA on Iris Dataset
  # =======================================================
  
    # Load libraries
    library(dplyr)
  library(ggplot2)
  
    # Load dataset
    data(iris)
  iris_clean <- iris
  
    # -------------------------------------------------------
  # 1. Numerical vs Numerical Relationships
    # -------------------------------------------------------
  
    # Q1: Is there a relationship between Sepal.Length and Sepal.Width?
   ggplot(iris_clean, aes(x = Sepal.Length, y = Sepal.Width)) +
   +     geom_point(color = "steelblue") +
   +     labs(title = "Sepal.Length vs Sepal.Width", x = "Sepal Length", y = "Sepal Width")
  
   # Q2: What is the correlation between Sepal.Length and Sepal.Width?
   cor(iris_clean$Sepal.Length, iris_clean$Sepal.Width)
 [1] -0.1175698
 
   # Q3: Is there a relationship between Petal.Length and Petal.Width?
    ggplot(iris_clean, aes(x = Petal.Length, y = Petal.Width)) +
   +     geom_point(color = "darkgreen") +
   +     labs(title = "Petal.Length vs Petal.Width", x = "Petal Length", y = "Petal Width")
  
    # Q4: What is the correlation between Petal.Length and Petal.Width?
    cor(iris_clean$Petal.Length, iris_clean$Petal.Width)
 [1] 0.9628654
 
    # Q5: Relationship between Sepal.Length and Petal.Length
    ggplot(iris_clean, aes(x = Sepal.Length, y = Petal.Length)) +
   +     geom_point(color = "tomato") +
   +     labs(title = "Sepal.Length vs Petal.Length", x = "Sepal Length", y = "Petal Length")
  
    # Q6: Correlation matrix for all numeric variables
    iris_clean %>%
   +     select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>%
   +     cor()
 Sepal.Length Sepal.Width Petal.Length Petal.Width
 Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
 Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
 Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
 Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000
  
    # Q7: Pairwise relationship (scatterplot matrix)
    pairs(iris_clean[, 1:4], main = "Scatterplot Matrix of Iris Dataset", col = iris_clean$Species)
  
    
    # -------------------------------------------------------
  # 2. Numerical vs Categorical Relationships
    # -------------------------------------------------------
  
   # Q8: How does Sepal.Length vary by Species?
    ggplot(iris_clean, aes(x = Species, y = Sepal.Length, fill = Species)) +
   +     geom_boxplot() +
   +     labs(title = "Sepal.Length by Species", x = "Species", y = "Sepal Length")
 
    # Q9: How does Sepal.Width vary by Species?
    ggplot(iris_clean, aes(x = Species, y = Sepal.Width, fill = Species)) +
   +     geom_boxplot() +
   +     labs(title = "Sepal.Width by Species", x = "Species", y = "Sepal Width")
  
    # Q10: Compare Petal.Length across species
    ggplot(iris_clean, aes(x = Species, y = Petal.Length, fill = Species)) +
   +     geom_boxplot() +
   +     labs(title = "Petal.Length by Species", x = "Species", y = "Petal Length")

    # Q11: Compare Petal.Width across species
    ggplot(iris_clean, aes(x = Species, y = Petal.Width, fill = Species)) +
   +     geom_boxplot() +
   +     labs(title = "Petal.Width by Species", x = "Species", y = "Petal Width")
  
    # Q12: Mean Sepal.Length by Species
    iris_clean %>%
   +     group_by(Species) %>%
   +     summarise(Mean_Sepal_Length = mean(Sepal.Length, na.rm = TRUE))
 # A tibble: 3 × 2
 Species    Mean_Sepal_Length
 <fct>                  <dbl>
   1 setosa                  5.01
 2 versicolor              5.94
 3 virginica               6.59
  
    # Q13: Mean Petal.Length by Species
    iris_clean %>%
   +     group_by(Species) %>%
   +     summarise(Mean_Petal_Length = mean(Petal.Length, na.rm = TRUE))
 # A tibble: 3 × 2
 Species    Mean_Petal_Length
 <fct>                  <dbl>
   1 setosa                  1.46
 2 versicolor              4.26
 3 virginica               5.55
  
    
    # -------------------------------------------------------
  # 3. Pairwise Visual Relationships by Category
    # -------------------------------------------------------
  
    # Q14: Scatter plot of Sepal.Length vs Sepal.Width colored by Species
    ggplot(iris_clean, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
   +     geom_point(size = 3) +
   +     labs(title = "Sepal Dimensions by Species")
  
    # Q15: Scatter plot of Petal.Length vs Petal.Width colored by Species
    ggplot(iris_clean, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
   +     geom_point(size = 3) +
   +     labs(title = "Petal Dimensions by Species")
  
    # Q16: Relationship between Sepal.Length and Petal.Length for each Species (trend lines)
    ggplot(iris_clean, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
   +     geom_point() +
   +     geom_smooth(method = "lm", se = FALSE) +
   +     labs(title = "Sepal.Length vs Petal.Length by Species", x = "Sepal Length", y = "Petal Length")
 `geom_smooth()` using formula = 'y ~ x'
  
    
    # -------------------------------------------------------
  # 4. Correlation Heatmap (optional, needs extra library)
    # -------------------------------------------------------
  
    # Q17: Visual correlation heatmap
    library(reshape2)
 
 Attaching package: ‘reshape2’
 
 The following object is masked from ‘package:tidyr’:
   
   smiths
  corr_data <- round(cor(iris_clean[, 1:4]), 2)
  melted_corr <- melt(corr_data)
  
    ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
   +     geom_tile() +
   +     geom_text(aes(label = value), color = "white", size = 4) +
   +     scale_fill_gradient(low = "blue", high = "red") +
   +     labs(title = "Correlation Heatmap", x = "", y = "") +
   +     theme_minimal()
 
    
    # -------------------------------------------------------
  # 5. Statistical Tests
    # -------------------------------------------------------
 
    # Q18: Is there a significant difference in Sepal.Length among Species?
    anova_result <- aov(Sepal.Length ~ Species, data = iris_clean)
  summary(anova_result)
 Df Sum Sq Mean Sq F value Pr(>F)    
 Species       2  63.21  31.606   119.3 <2e-16 ***
   Residuals   147  38.96   0.265                   
 ---
   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    # Q19: Is there a significant difference in Petal.Width among Species?
    anova_result2 <- aov(Petal.Width ~ Species, data = iris_clean)
  summary(anova_result2)
 Df Sum Sq Mean Sq F value Pr(>F)    
 Species       2  80.41   40.21     960 <2e-16 ***
   Residuals   147   6.16    0.04                   
 ---
   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
   # Q20: Correlation significance test between Petal.Length and Petal.Width
   cor.test(iris_clean$Petal.Length, iris_clean$Petal.Width)
 
 Pearson's product-moment correlation

data:  iris_clean$Petal.Length and iris_clean$Petal.Width
t = 43.387, df = 148, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9490525 0.9729853
sample estimates:
      cor 
0.9628654 

 
 # -------------------------------------------------------
 # 6. Additional Explorations
 # -------------------------------------------------------
 
 # Q21: Relationship between Sepal.Area and Petal.Area (if created)
 iris_clean <- iris_clean %>%
+     mutate(Sepal.Area = Sepal.Length * Sepal.Width,
+            Petal.Area = Petal.Length * Petal.Width)
 
 ggplot(iris_clean, aes(x = Sepal.Area, y = Petal.Area, color = Species)) +
+     geom_point(size = 3) +
+     labs(title = "Sepal.Area vs Petal.Area", x = "Sepal Area", y = "Petal Area")
 
 # Q22: Correlation between Sepal.Area and Petal.Area
 cor(iris_clean$Sepal.Area, iris_clean$Petal.Area)
[1] 0.4545033








#MULTIVARIANT QUESTIONS






 # MULTIVARIATE EDA ON IRIS DATASET
 # =====================================================
 
 # 1️⃣ Load Required Libraries
 # -----------------------------------------------------
 packages <- c("dplyr", "GGally", "corrplot", "ggplot2", "ggfortify", "plotly")
 new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
 if(length(new_packages)) install.packages(new_packages)
 
 library(dplyr)
 library(GGally)
 library(corrplot)
 library(ggplot2)
 library(ggfortify)
 library(plotly)
 
 # 2️⃣ Load Dataset
 # -----------------------------------------------------
 data("iris")
 iris_clean <- iris
 
 # Inspect data
 glimpse(iris_clean)
Rows: 150
Columns: 5
$ Sepal.Length <dbl> 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4, 4.9, 5.4, …
$ Sepal.Width  <dbl> 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.1, 3.7, …
$ Petal.Length <dbl> 1.4, 1.4, 1.3, 1.5, 1.4, 1.7, 1.4, 1.5, 1.4, 1.5, 1.5, …
$ Petal.Width  <dbl> 0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.3, 0.2, 0.2, 0.1, 0.2, …
$ Species      <fct> setosa, setosa, setosa, setosa, setosa, setosa, setosa,…
 summary(iris_clean)
  Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
 Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
 1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
 Median :5.800   Median :3.000   Median :4.350   Median :1.300  
 Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
 3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
 Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
       Species  
 setosa    :50  
 versicolor:50  
 virginica :50  
                
                
                
 
 # 3️⃣ Multivariate Question 1:
 # How are multiple numeric variables correlated?
 # -----------------------------------------------------
 numeric_vars <- iris_clean %>%
+     select_if(is.numeric)
 
 corr_matrix <- cor(numeric_vars)
 print(corr_matrix)
             Sepal.Length Sepal.Width Petal.Length Petal.Width
Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000
 
 corrplot(corr_matrix, method = "color", addCoef.col = "black",
+          number.cex = 0.8, title = "Correlation Heatmap of Iris Features",
+          mar = c(0, 0, 2, 0))
 
 # 4️⃣ Multivariate Question 2:
 # How do pairs of numeric variables relate across species?
 # -----------------------------------------------------
 ggpairs(iris_clean, aes(color = Species),
+         title = "Pairwise Relationships Between Iris Features (by Species)")
 plot: [5, 1] [======================================>--------] 84% est: 0s 
`stat_bin()` using `bins = 30`. Pick better value `binwidth`.
 plot: [5, 2] [========================================>------] 88% est: 0s 
`stat_bin()` using `bins = 30`. Pick better value `binwidth`.
 plot: [5, 3] [==========================================>----] 92% est: 0s 
`stat_bin()` using `bins = 30`. Pick better value `binwidth`.
 plot: [5, 4] [============================================>--] 96% est: 0s 
`stat_bin()` using `bins = 30`. Pick better value `binwidth`.
                                                                           

 # 5️⃣ Multivariate Question 3:
 # How do Sepal and Petal dimensions together separate species?
 # -----------------------------------------------------
 ggplot(iris_clean, aes(x = Petal.Length, y = Petal.Width,
+                        color = Species, size = Sepal.Length)) +
+     geom_point(alpha = 0.8) +
+     labs(title = "Sepal & Petal Dimensions vs Species",
+          x = "Petal Length", y = "Petal Width", size = "Sepal Length") +
+     theme_minimal()
 
 # 6️⃣ Multivariate Question 4:
 # How do three numeric variables interact (3D visualization)?
 # -----------------------------------------------------
 plot_ly(iris_clean, x = ~Sepal.Length, y = ~Petal.Length, z = ~Petal.Width,
+         color = ~Species, colors = "Set1", type = "scatter3d", mode = "markers") %>%
+     layout(title = "3D Relationship Between Sepal & Petal Dimensions")> 
 # 7️⃣ Multivariate Question 5:
 # Can PCA visualize data separability between species?
 # -----------------------------------------------------
 iris_pca <- prcomp(iris_clean[, 1:4], center = TRUE, scale. = TRUE)
 
 # PCA Summary
 summary(iris_pca)
Importance of components:
                          PC1    PC2     PC3     PC4
Standard deviation     1.7084 0.9560 0.38309 0.14393
Proportion of Variance 0.7296 0.2285 0.03669 0.00518
Cumulative Proportion  0.7296 0.9581 0.99482 1.00000
 
 # PCA Biplot
 autoplot(iris_pca, data = iris_clean, colour = 'Species',
+          loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
+     ggtitle("PCA Biplot: Visualizing Species Separation")
Warning message:
`aes_string()` was deprecated in ggplot2 3.0.0.
ℹ Please use tidy evaluation idioms with `aes()`.
ℹ See also `vignette("ggplot2-in-packages")` for more information.
ℹ The deprecated feature was likely used in the ggfortify package.
  Please report the issue at <https://github.com/sinhrks/ggfortify/issues>.
This warning is displayed once every 8 hours.
Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
generated. 
 
 # 8️⃣ Multivariate Question 6:
 # Do different species have distinct multivariate centroids (Mean patterns)?
 # -----------------------------------------------------
 iris_clean %>%
+     group_by(Species) %>%
+     summarise(across(where(is.numeric), mean, .names = "mean_{.col}")) %>%
+     print()
# A tibble: 3 × 5
  Species    mean_Sepal.Length mean_Sepal.Width mean_Petal.Length
  <fct>                  <dbl>            <dbl>             <dbl>
1 setosa                  5.01             3.43              1.46
2 versicolor              5.94             2.77              4.26
3 virginica               6.59             2.97              5.55
# ℹ 1 more variable: mean_Petal.Width <dbl>
 
 # 9️⃣ Multivariate Question 7:
 # How does Sepal.Length relate to both Petal.Length and Species together?
 # -----------------------------------------------------
 ggplot(iris_clean, aes(x = Petal.Length, y = Sepal.Length, color = Species)) +
+     geom_point(size = 3) +
+     geom_smooth(method = "lm", se = FALSE) +
+     labs(title = "Sepal Length vs Petal Length Across Species",
+          x = "Petal Length", y = "Sepal Length") +
+     theme_classic()
`geom_smooth()` using formula = 'y ~ x'
 
 # 🔟 Multivariate Question 8:
 # How do multiple numeric variables vary by Species (boxplots)?
 # -----------------------------------------------------
 iris_long <- iris_clean %>%
+     tidyr::pivot_longer(cols = 1:4, names_to = "Feature", values_to = "Value")
 
 ggplot(iris_long, aes(x = Species, y = Value, fill = Species)) +
+     geom_boxplot(alpha = 0.7) +
+     facet_wrap(~ Feature, scales = "free_y") +
+     labs(title = "Distribution of Each Feature by Species",
+          x = "Species", y = "Value") +
+     theme_minimal()

 

 
 
 
 