library(funModeling)

##MODELLI DI CHURN

##Il modello di churn che verrà sviluppato è di tipo supervisionato

## Gli step sono i seguenti:

##1. Scelta di una data di riferimento nel passato. 
##  La reference date è 01/01/2019

churn_study_period <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE < as.Date("1/1/2019",
                            format = "%d/%m/%Y"),
         TIC_DATE > as.Date("01/10/2018",           ## tre mesi prima poichè il lookback è di 3 mesi
                            format = "%d/%m/%Y"))
head(churn_study_period)

## 2. Dopo, si sceglie la lunghezza del periodo di holdout dopo ciascuna data di riferimento
# Holdout Period: 28/02/2019 
##poichè la purchase time scale è di 60 giorni

churn_holdout <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE < as.Date("28/02/2019",           ##due mesi dopo la reference date
                            format = "%d/%m/%Y"),
         TIC_DATE > as.Date("01/01/2019",
                            format = "%d/%m/%Y"))

no_churner <- unique(churn_holdout$ID_CLI)  

head(churn_holdout)

## 3. Scelta della lunghezza del periodo di lookback prima della reference date
# Periodo di Lookback: 3 mesi

churn_recency <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))

churn_recency$RECENCY <- difftime(as.Date("01/01/2019",
                                          format = "%d/%m/%Y"),          
                                  churn_recency$LAST_PURCHASE_DATE,
                                  units = "days")
#differenza di giorni tra reference date e data di ultimo acquisto

churn_frequency <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(TOT_PURCHASE))
#quante volte ha acquistato un cliente nel periodo di lookback

churn_monetary <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO),
            SPESA = IMPORTO_LORDO - SCONTO) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(IMPORTO_LORDO))
#spesa totale dei clienti nel periodo di lookback

churn <- merge(churn_recency, churn_frequency, by = "ID_CLI")
churn <- merge(churn, churn_monetary, by = "ID_CLI") %>%
  select(ID_CLI,  
         SPESA, #lordo - sconto
         TOT_PURCHASE)
head(churn)

## 4. Assegnazione del target. Ad ogni cliente si assegna un target 0 o 1. 
##    Il valore 1 corrisponde ai clienti che hanno fatto churn nel periodo di holdout
##    Il valore 0 corrisponde ai clienti che non hanno fatto churn nel periodo di holdout


churn$CHURN <- 1

for (i in c(1:nrow(churn))){
  if (churn$ID_CLI[i] %in% no_churner) churn$CHURN[i] <- 0
}
#i clienti che non compaiono nel periodo di holdout, 
##cioè che non hanno fatto acquisti, sono i clienti churn

churn$CHURN <- as.factor(churn$CHURN)

table(churn$CHURN)

##0     1 
##35373 59752

##La maggioranza dei clienti è un churner


##5. Si scelgono le variabili indipendenti che verranno utilizzate nei modelli
## Il loro valore verrà calcolato in riferimento al periodo di lookback

##Le variabili nel dataframe churn sono:
## ID_CLI: id del cliente
##SPESA: Importo lordo - sconto
##TOT_PURCHASE: numero degli acquisti
##CHURN: target


##aggiunta delle variabili

churn <- left_join(churn, df_1_cli_fid_clean[, c("ID_CLI", "LAST_COD_FID")], by = "ID_CLI") # Add Type of Fidelity Card

churn <- left_join(churn, df_2_cli_account_clean[, c("ID_CLI", "TYP_JOB", "TYP_CLI_ACCOUNT" )], by = "ID_CLI")  # Add Type Job

churn <- left_join(churn, df_4_cli_privacy_clean[, c("ID_CLI", "FLAG_DIRECT_MKT" )], by = "ID_CLI")  #aggiungo Flag per il direct Marketing


region <- left_join(df_2_cli_account_clean[, c("ID_CLI", "ID_ADDRESS")],
                    df_3_cli_address_clean[, c("ID_ADDRESS", "REGION")], by = "ID_ADDRESS") # Add Region

campagna <- left_join(df_5_camp_cat_clean[, c("ID_CAMP", "TYP_CAMP")],
                    df_6_camp_event_clean[, c("ID_CAMP", "ID_CLI")], by = "ID_CAMP") # Add Region


churn <- left_join(churn, region, by = "ID_CLI")

churn <- left_join(churn, campagna, by = "ID_CLI")

head(churn)

## Le variabili nel dataframe churn sono ora:
##ID_CLI  
##SPESA 
##TOT_PURCHASE 
##CHURN
##TYP_JOB
##TYP_CLI_ACCOUNT
##FLAG_DIRECT_MKT 
##ID_ADDRESS    
##REGION
##ID_CAMP
##TYP_CAMP

head(churn)

### Modello

##Si divide il dataset in traning e test

churn <- na.omit(churn)

train_index <- createDataPartition(churn$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)
train <- churn[train_index,]
test <- churn[-train_index,]

table(train$CHURN)

##0      1 
##241103 254105 

## Modelli

### Albero di regressione

tree <- rpart(CHURN ~ SPESA + TOT_PURCHASE+  TYP_JOB + TYP_CLI_ACCOUNT +  FLAG_DIRECT_MKT +
                REGION + TYP_CAMP, data = train)

rpart.plot(tree, extra = "auto")

summary(tree) 

##variable importance
##TYP_CAMP TOT_PURCHASE        SPESA      TYP_JOB 
##   65           29            5            1 

printcp(tree) # parametro di complessità

### Random Forest

memory.limit(50000)
tree_rf <- randomForest(CHURN ~ SPESA + TOT_PURCHASE+  TYP_JOB + TYP_CLI_ACCOUNT +  
                          FLAG_DIRECT_MKT + REGION + TYP_CAMP,
                        data = train, ntree = 50)
print(tree_rf)

##Matrice di confusione:
##  0      1       class.error
##0 172393  68710   0.2849819
##1  73961 180144   0.2910647


### Logistic Regression

logistic <- train(CHURN ~ SPESA + TOT_PURCHASE+  TYP_JOB + TYP_CLI_ACCOUNT +  
                    FLAG_DIRECT_MKT + REGION + TYP_CAMP,
                  data = train,
                  method = "glm")
summary(logistic)

##tutte le variabili quantitative risultano estremamente significative, così come 
##quasi tutti i livelli delle variabili qualitative

##AIC: 583396
##Number of Fisher Scoring iterations: 8

### Lasso
lasso <- train(CHURN ~ SPESA + TOT_PURCHASE+  TYP_JOB + TYP_CLI_ACCOUNT +  
                 FLAG_DIRECT_MKT + REGION + TYP_CAMP,
               data = train,
               method = "glmnet",
               family = "binomial")
lasso

##The final values used for the model were alpha = 0.1 and lambda = 0.0003393813.
plot(lasso)


## Prediction dei modelli

## Prediction del modello Tree
pred <- predict(tree, test[, -4], type = "class")
p1 <- unlist(pred)
cm_tree <- confusionMatrix(p1, test$CHURN)

##Accuracy : 0.6909 
##Prediction     0     1
##          0 76766 39033
##          1 26563 69869

##Prediction modello random forest
pred_rf <- predict(tree_rf, test[,-4], type = "class")
cm_rf <- confusionMatrix(pred_rf, test$CHURN)

##accuracy : 0.7138 
##              0     1
##          0 73919 31328
##          1 29410 77574

## Prediction del modello logistico
pred_logistic <- predict(logistic, test[, -4], type = "raw")
cm_log <- confusionMatrix(pred_logistic, test$CHURN)

##accuracy : 0.6936
##                 0     1
##              0 71867 33558
##              1 31462 75344


## Prediction del modello Lasso
pred_lasso <- predict(lasso, test[,-4], type = "raw")
cm_lasso <- confusionMatrix(pred_lasso, test$CHURN)

##Accuracy : 0.6936  
##     0     1
##   0 71824 33522
##   1 31505 75380


# Si definiscono i risultati in un dataFrame

results <- data.frame("MODEL" = c("TREE", "RF", "LOG", "LASSO"), 
                      "ACCURACY" = c(cm_tree$overall[["Accuracy"]],
                                     cm_rf$overall[["Accuracy"]],
                                     cm_log$overall[["Accuracy"]],
                                     cm_lasso$overall[["Accuracy"]]), 
                      "PRECISION" = c(cm_tree$byClass[["Precision"]], 
                                      cm_rf$byClass[["Precision"]],
                                      cm_log$byClass[["Precision"]],
                                      cm_lasso$byClass[["Precision"]]),
                      "RECALL" = c(cm_tree$byClass[["Recall"]], 
                                   cm_rf$byClass[["Recall"]],
                                   cm_log$byClass[["Recall"]],
                                   cm_lasso$byClass[["Recall"]]),
                      "F1" = c(cm_tree$byClass[["F1"]], 
                               cm_rf$byClass[["F1"]],
                               cm_log$byClass[["F1"]],
                               cm_lasso$byClass[["F1"]]),
                      "ACCURACY_LOW" = c(cm_tree$overall[["AccuracyLower"]],
                                         cm_rf$overall[["AccuracyLower"]],
                                         cm_log$overall[["AccuracyLower"]],
                                         cm_lasso$overall[["AccuracyLower"]]),
                      "ACCURACY_UP" = c(cm_tree$overall[["AccuracyUpper"]],
                                        cm_rf$overall[["AccuracyUpper"]],
                                        cm_log$overall[["AccuracyUpper"]],
                                        cm_lasso$overall[["AccuracyUpper"]]))

results

# Plot dei risultati dell'accuratezza

plot_accuracy <- ggplot(data = results, aes(x = factor(MODEL, c("TREE", "RF", "LOG", "LASSO")), y = ACCURACY, group = 1)) +
  geom_errorbar(aes(ymin=ACCURACY_LOW, ymax=ACCURACY_UP), width=.1, color = "black", size = 1) +
  geom_line(color = "#F6D935", size = 1.3) +
  geom_point(color = "#404040", size = 2) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#BFBFBF", colour = "#BFBFBF"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#BFBFBF"),
        text = element_text(size = 13, face = "bold", colour = "#404040")) +
  xlab("MODELLO")

plot_accuracy

# Plot dei risultati della F1

plot_f1 <- ggplot(data = results, aes(x = factor(MODEL, c("TREE", "RF", "LOG", "LASSO")), y = F1, group = 1)) +
  geom_line(color = "#F6D935", size = 1.3) +
  geom_point(color = "#404040", size = 3) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#BFBFBF", colour = "#BFBFBF"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#BFBFBF"),
        text = element_text(size = 13, face = "bold", colour = "#404040")) +
  xlab("MODELLO")

plot_f1

## Plot della Precision

plot_precision <- ggplot(data = results, aes(x = factor(MODEL, c("TREE", "RF", "LOG", "LASSO")), y = PRECISION, group = 1)) +
  geom_line(color = "#F6D935", size = 1.3) +
  geom_point(color = "#404040", size = 3) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#BFBFBF", colour = "#BFBFBF"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#BFBFBF"),
        text = element_text(size = 13, face = "bold", colour = "#404040")) +
  xlab("MODELLO")

plot_precision


## Plot del Recall

plot_recall <- ggplot(data = results, aes(x = factor(MODEL, c("TREE", "RF", "LOG", "LASSO")), y = RECALL, group = 1)) +
  geom_line(color = "#F6D935", size = 1.3) +
  geom_point(color = "#404040", size = 3) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#BFBFBF", colour = "#BFBFBF"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#BFBFBF"),
        text = element_text(size = 13, face = "bold", colour = "#404040")) +
  xlab("MODELLO")

plot_recall

##Mentre per tutte le misure precedenti il modello migliore era il Random Forest,
##qui si nota che il modello con le performance migliori è l'albero


## Sulla base dei risultati osservati, il modello vincente è il Random Forest


