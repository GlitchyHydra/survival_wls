library(caTools)
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(haven)
library(survminer)
library(survival)
library(pROC)
library(base)
library(rlist)
library(fuzzySim)
library(randomForest)

setwd("E:/education/thesis")
source("scripts/data_preprocessing.R")
source("evaluation_func.R")
source("scripts/plotter.r")

df <- read_wls_dataset();
df_features_effect <- read_features_effect("cox_impact.csv", upper_threshold=1e-40)