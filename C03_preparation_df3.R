#### FIRST LOOK of df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

## Le colonne del dataframe sono [ID_ADDRESS, CAP, PRV, REGION ]

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  summarize(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

## TOT_ID_ADDRESSes TOT_ROWs
##           361330  1211332

# Si osservano 361330 indirizzi diversi. Può tuttavia capitare che uno stesso indirizzo si
# riferisca a città diverse; i duplicati da eliminare sono le righe con stesso ID_ADDRESS e CAP.


## vengono eliminati i duplicati
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()


#### CLEANING DATA TYPES in df_3 ####

## format string as factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.character(CAP))

#### Pulizia dei valori mancanti in df_3 ####

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP) ##diverso dal valore na
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS))

## Ci sono 337466 indirizzi in cui non è mancante alcun campo contemporaneamente

## Esaminiamo nel dettaglio alcuni dei casi in cui sono presenti valori mancanti

## Nel caso seguente, si guardi dove non è mancante la provincia ma lo è la regione

df_3_cli_address_clean %>% filter(!is.na(PRV) & is.na(REGION))

## MISSING VALUES rows are removed ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))
##si rimuovono le righe in cui mancano tutti contemporaneamente


#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarize(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#


#### EXPLORE COLUMNS of df_3 ####
#### ???? TO DO df_3 ???? ####
# EXPLORE the df_3_cli_address_clean relevant variables

## Si osservano le regioni di ogni cliente (`REGION`):

df3_dist_region<- df_3_cli_address_clean                   %>%
  group_by(REGION)                                %>%  #si raggruppa per REGION
  summarise(TOT_ADDRESS = n_distinct(ID_ADDRESS)) %>%  #numero totale di indirizzi per regione
  mutate(PERCENT = TOT_ADDRESS/sum(TOT_ADDRESS))  %>%  #percentuale
  arrange(desc(PERCENT))                               #ordinato per percentuale 


ggplot(data = df3_dist_region, aes(x = REGION,
                                   y = TOT_ADDRESS)) +
  labs(title = "Address Distribution",
       x     = "Region",
       y     = "Number of Address") +
  geom_bar(stat = "identity",
           fill = "steelblue") +                       
  theme_minimal() +                                    
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
#la maggior parte dei clienti proviene dalla Lombardia, in cui si registrano 97181 clienti.


#### FINAL REVIEW df_3_clean ####

str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)