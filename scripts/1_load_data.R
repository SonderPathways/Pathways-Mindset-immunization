#######################
# Load packages
#######################

pacman::p_load("dplyr",
               "rpart", 
               "rpart.plot", 
               "caret", 
               "randomForest", 
               "data.table", 
               "ggplot2", 
               "gplots", 
               "tidyverse", 
               "readxl",
               "corrplot", 
               "ggplot2", 
               "ranger", 
               "haven", 
               "readstata13")

#######################
# Import data
#######################

# Read DHS data 
NDHS <- data.table(read.dta13("../data/NGIR6ADT/NGIR6AFL.DTA"))

# Read segmentation data 
rural <- readRDS("../data/rural_outcomes_vulnerability_class_ranked.rds")
urban <- readRDS("../data/urban_outcomes_vulnerability_class_ranked.rds")
NN <- rbind(rural, urban)

dim(NN) #14979 482 #check sample

# Read Mindset HH data
MS <- read_excel("../data/zero_dose_data_share.xlsx",
                 sheet = "households",
                 col_names = TRUE)

dim(MS) #19371 494 #check sample

##########################################
# Add state variable to segmentation data
##########################################

NN <- NN %>%
  mutate(STATE = str_remove(v023, " urban| rural"),
         STATE = str_to_title(STATE),
         STATE = case_when(
           STATE == "Fct" ~ "Federal Capital Territory",
           TRUE ~ STATE)) 
table(NN$STATE, useNA = "always")

