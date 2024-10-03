# obesity_data
Predicting Obesity Levels Through Dietary Patterns and Physical Condition
#-----Section-01-Adding Libraries-----
# Loading libraries for Plots, Calculations, modelling and much more.
library(ggplot2)
library(tidyverse)
library(caret)
library(randomForest)
library(Amelia)

#-----Section-02-Set & Get Work Directory-----
# Setting working directory before processing any work or test in using R Script,
# useful to save the Graphs, Plots, Images, and Data in set directory.
setwd(dirname(file.choose()))
# Getting the path directory to confirm the working directory.
getwd()

#-----Section-03-Data Frame & Data Set-----
# Reading the data from csv file and putting it into new data-frame.
DS7003.Data <- read.csv("ObesityDataSet.csv", stringsAsFactors = FALSE)
# To inspect top 6 rows of the data and variable head for general view of data-set.
head(DS7003.Data)
# The counterpart of head() is tail(), which prints the last six rows.
tail(DS7003.Data)
# A data-frame having column(character, integer or logical) variables data type, which stands for “structure”.
str(DS7003.Data)
# Attaching the CSV data 'DS7003.Data' for working in R faster
attach(DS7003.Data)

#-----Section-04-Missing Data?-----
# The data set might have some missing values,
# checking for missing data in the entire data-set.
missing_data <- DS7003.Data
# Displaying the missing data summary so it can give an proper results.
apply(missing_data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
# Mapping up missing values by column using miss-map.
missmap(missing_data, margins = c(10, 5), col = c("black", "grey"), legend = TRUE,main = "Fig.: Missingness Map of DS7003")

# However, to remove variables or data from work space or environment in R.
rm(missing_data)

#-----Section-05-Summary Statistics-----
# Summary for getting Mean, Std. deviation, Maximum and Minimum to check the central tendency.
# These statistics can be useful for understanding the central tendency and variability of the data distribution in each category within the data-set.
# The summary from the data-set can be parametric or non parametric, checking for few columns or variables.
summary(DS7003.Data)

# Table of classes
table(DS7003.Data$NObeyesdad)

# Recode classes as a factor with informative labels
DS7003.Data$NObeyesdad <- factor(DS7003.Data$NObeyesdad,
                                  levels = c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I",
                                             "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"),
                                  labels = c("Insufficient Weight", "Normal Weight", "Overweight Level I",
                                             "Overweight Level II", "Obesity Type I", "Obesity Type II", "Obesity Type III"))

# Table or proportions with more informative labels
round(prop.table(table(DS7003.Data$NObeyesdad)) * 100, digits = 1)

# Summarize three numeric features
summary(DS7003.Data[c("Age", "Weight", "Height")])
# Note contrasts in scale

#-----Section-06-Converting Data-----
# Factor the char to number
# Convert data to numeric
DS7003.Data$Gender <- as.numeric(factor(DS7003.Data$Gender))
DS7003.Data$family_history_with_overweight <- as.numeric(factor(DS7003.Data$family_history_with_overweight))
DS7003.Data$FAVC <- as.numeric(factor(DS7003.Data$FAVC))
DS7003.Data$CAEC <- as.numeric(factor(DS7003.Data$CAEC))
DS7003.Data$SMOKE <- as.numeric(factor(DS7003.Data$SMOKE))
DS7003.Data$SCC <- as.numeric(factor(DS7003.Data$SCC))
DS7003.Data$CALC <- as.numeric(factor(DS7003.Data$CALC))
DS7003.Data$MTRANS <- as.numeric(factor(DS7003.Data$MTRANS))
DS7003.Data$NObeyesdad <- as.numeric(factor(DS7003.Data$NObeyesdad))

# exploratory data analysis

# Attaching the CSV data 'DS7003.Data' for working in R faster
attach(DS7003.Data)


str(DS7003.Data)
#----------------Section-07-Boxplot, Histogram--------------------
# Step 1: Boxplot, and Histogram of Dependent Variable (NObeseydad)

# Boxplot: A boxplot will give a visual representation of the distribution of dependent variable.
# It shows the median, quartiles, and potential outliers.
dev.off()

boxplot(NObeyesdad, main="Fig.: Boxplot of Obesity",
        ylab="Obesity Level", xlab="Obesity")
boxplot(DS7003.Data, main="Fig.: Boxplot of Variables")
# Inspect outliers with boxplot status, getting some variables & objects to understand. 
summary(NObeyesdad)

# gender
boxplot(Gender, main="Fig.: Number of items in each Gender category",
        ylab="count", xlab="Gender")

# age
boxplot(Age, main="Fig.: Number of items in each Age category",
        ylab="count", xlab="age")

# height
boxplot(Height, main="Fig.: Number of items in each Height category",
        ylab="count", xlab="height") 

# weight
boxplot(Weight, main="Fig.: Number of items in each Weight category",
        ylab="count", xlab="weight") 

# Overweight and Height
boxplot(Height, main="Fig.: Number of items in each Height and Obesity categories",
        ylab="height", xlab="NObeyesdad") 

# Overweight and Weight
boxplot(Weight, main="Fig.: Number of items in each Weight and Obesity categories",
        ylab="weight", xlab="NObeyesdad") 

# Overweight and Physical activity 
boxplot(FAF, main="Fig.: Number of items in each Physical activity and Obesity categories",
        ylab="Physical activity ", xlab="NObeyesdad") 

# Overweight and Physical Inactivity 
boxplot(TUE, main="Fig.: Number of items in each Physical Inactivity and Obesity categories",
        ylab="Physical Inactivity", xlab="NObeyesdad") 

# Overweight and Consumption of Alcohol
boxplot(CALC, main="Fig.: Number of items in each Consumption of alcohol and obesity categories",
        ylab="Consumption of Alcohol", xlab="NObeyesdad") 

dev.off()
# Histogram: A histogram will provide a visual representation of the distribution of Obesity,
# allowing to see the frequency distribution of different ranges.
library(ggplot2)
# Create the histogram using ggplot
ggplot(DS7003.Data, aes(x = NObeyesdad)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Obesity Levels", x = "Obesity Level", y = "Frequency")

#----------------------Section 08: Creating Correlation Matrix------------------------
#Correlation Matrix
library(corrgram)
correlation_matrix <- cor(DS7003.Data,method = "pearson")
corrgram(correlation_matrix, order=FALSE,cor.method = "pearson", lower.panel=panel.cor,
         upper.panel = panel.pie, text.panel=panel.txt, main="Correlation Matrix",
         cex.label = 1, mar = c(1,1,1,1))
rm(correlation_matrix)
dev.off()

DS7003.Data_copy <- DS7003.Data

#-------------------Section 09:Train-test split------------------
# Specify the proportion of data you want to assign to the training set
# 80% of the data will be used for training
# Generate indices for the training set
indices <- sample(nrow(DS7003.Data), size = round(0.8 * nrow(DS7003.Data)))

# Create the training set
trn <- DS7003.Data[indices, ]

# Specify the proportion of data you want to assign to the testing set
# 20% of the data will be used for testing
# Generate indices for the testing set
# Create the testing set by excluding the specified indices
tst <- DS7003.Data[-indices, ]

rm(indices)

dim(trn)
dim(tst)

prop.table(table(DS7003.Data$NObeyesdad))

# Create training (80%) and test data (20%) (data already in random order)

obesity_data <- DS7003.Data

shuffle_index <- sample(1:nrow(obesity_data))
obesity_data <- obesity_data[shuffle_index,]


obesity_train <- obesity_data[1:1600, ]
obesity_test <- obesity_data[1601:2111, ]

# Create labels (from first column) for training and test data
obesity_train_labels <- obesity_data[1:1600, 17]
obesity_test_labels <- obesity_data[1601:2111, 17]

table(obesity_train_labels)
table(obesity_test_labels)

#---------------Final End Here-------------

# detach the data frame from environment
detach(DS7003.Data)
