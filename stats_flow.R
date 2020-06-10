# My Working Dir
# setwd('Desktop/Academics/Carleton/years/senior/winter/cs_comps/MediaWiki/ArticleCSV/')

##### MULTINOMIAL REGRESSION #####

# import packages
library(nnet) # for multinom function
library(foreign) 
library(tidyverse) 
library(caret)
library(car)

scale_cols <- function(data) {
  numeric_data <- data
  numeric_data$class <- NULL
  numeric_data <- as.data.frame(scale(numeric_data))
  numeric_data$class <- data$class
  return(numeric_data)
}

process_csv <- function(filename, preset_class = NULL) {
  raw_data = read.csv(file = filename)
  if('title' %in% names(raw_data)) {
    raw_data$title <- NULL
  }
  data = scale_cols(raw_data)
  if(!'class' %in% names(data)) {
    data$class <- preset_class    
  }
  data$class <- factor(data$class, levels = c("SB", "ST", "C", "B", "GA", "FA"))
  data$class <- 
  return(data)
}

run_multinom <- function(dataframe, independent_vars = TRUE, maxit=100) {
  if(independent_vars) {
    return(multinom(class~., data=dataframe, maxit = maxit))
  }
}

get_prediction <- function(model, ground_truth) {
  predictclass_multi <- predict(model, ground_truth)
  return(confusionMatrix(predictclass_multi, ground_truth$class))
}

##### RIDGE REGRESSION #####

# Load libraries, get data & set seed for reproducibility
set.seed(123)    # seed for reproducibility
library(glmnet)  # for ridge regression
library(dplyr)   # for data cleaning
# library(psych)   # for function tr() to compute trace of a matrix

# data("mtcars")
# Center y, X will be standardized in the modelling function
run_ridge <- function(data) {
  y <- data %>% select(class) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
  X <- data %>% select(-class) %>% as.matrix()
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  
  # Setting alpha = 0 implements ridge regression
  ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda = lambdas_to_try,
                        standardize = TRUE,
                        nfolds = 10)
  
  # Plot cross-validation results
  plot(ridge_cv)
}



