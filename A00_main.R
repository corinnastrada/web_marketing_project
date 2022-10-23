#### OPTIONS ####

options(scipen=999)
set.seed(12345)

#### LIBRARIES ####
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)

library(rfm)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(arules)
library(arulesViz)
library(tidyr)

#la libreria 'funModeling' verrà inserita nello script D02_CHURN per evitare che causi problemi con la funzione
#'summarize'.

#### DIRECTORIES ####
working_dir = "C:/Users/lorel/Desktop/UNIVERSITA/MAGISTRALE/SECONDO SEMESTRE/web marketing/progetto/script/DSLab_project"
data_dir = "C:/Users/lorel/Desktop/UNIVERSITA/MAGISTRALE/SECONDO SEMESTRE/web marketing/progetto/DMktg_DSLab_data_1"

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####
# Uncomment to execute the entire pipeline of scripts
PIPELINE_scripts <- c(
   'B01_ingestion.R'
   , 'C01_preparation_df1.R'
   , 'C02_preparation_df2.R'
   , 'C03_preparation_df3.R'
   , 'C04_preparation_df4.R'
   , 'C05_preparation_df5.R'
   , 'C06_preparation_df6.R'
   , 'C07_preparation_df7.R'
   , 'D01_RFM.R'
   , 'D02_CHURN.R'
   , 'D03_MBA.R'
   )

for(i in PIPELINE_scripts){
 source(i, echo = TRUE)
}
