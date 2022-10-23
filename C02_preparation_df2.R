#### FIRST LOOK of df_2 ####

str(df_2_cli_account)

summary(df_2_cli_account)

## Le variabili presenti nel dataset sono:
## ID_CLI       EMAIL_PROVIDER        W_PHONE        ID_ADDRESS     TYP_CLI_ACCOUNT   TYP_JOB  

#### Pulizia di df_2 ####

df_2_cli_account_clean <- df_2_cli_account

##si controllano i duplicati

df_2_cli_account_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

##non sono stati rilevati valori duplicati


#### Pulizia dei data types in df_2  ####

## si formattano i booleani come factor

df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

## si formattano le variabili categoriali in factor

df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))


#### Pulizia dei dati mancanti in df_2

## MISSING VALUES mappati come natural values 

## si genera una variabile booleana

df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0"))

## I MISSING VALUES sono mappati come un nuovo livello della variabile categoriale 

df_2_cli_account_clean <- df_2_cli_account_clean %>%  
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))


#### CONSISTENCY CHECK ID_CLI in df_1/df_2 ####

##si confronta l'id dei clienti di questo df con quelli del df_1

##si vuole conteggiare il numero di id clienti che sono in entrambi i dataframe


cons_idcli_df1_df2 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_2_cli_account_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_2 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_2) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df2

#!!! tutti gli ID_CLI in df_1 sono anche in o in df_2 e vice-versa !!!#



#### EXPLORE COLUMNS of df_2 ####

### Variable EMAIL_PROVIDER ###

## compute distribution
df_2_dist_emailprovider <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()
#per ogni provider si calcola il numero totale di clienti e la percentuale
#@gmail.com è di gran lunga il provider più diffuso

tot_emailproviders <- n_distinct(df_2_dist_emailprovider$EMAIL_PROVIDER)
#in totale ci sono 20512 provider diversi

#!!! NOTE: too many different values for EMAIL_PROVIDER to be an useful category !!!#



#### ???? TO DO df_2 ???? ####
# COMPUTE THE DISTRIBUTION for the remaining df_2_cli_fid_clean variables

#### RESHAPING df_2 ####

## si mantengono i provider più frequenti e si aggiunge un livello 'others' per quelli meno frequenti ##
df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>% #si calcola la frequenza cumulata
  as.data.frame() %>%
  head(20)

clean_email_providers <- df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))
head(clean_email_providers, 20)
#si mantengono i provider la cui frequenza cumulata è minore o uguale a 0.85; gli altri vengono identificati
#come 'others'; i provider con dati mancanti sono identificati come '(missing)'


df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))
#si aggiunge EMAIL_PROVIDER_CLEAN al dataset df_2_cli_account_clean


#### EXPLORE NEW COLUMNS EMAIL_PROVIDER_CLEAN in df_2 ####

## compute distribution
df2_dist_emailproviderclean <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_dist_emailproviderclean

## plot distribution
plot_df2_dist_emailproviderclean <- (
  ggplot(data=df2_dist_emailproviderclean
         , aes(x=EMAIL_PROVIDER_CLEAN, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="orange") +
    theme_minimal()
)

plot_df2_dist_emailproviderclean


## compute distribution W_PHONE
df2_dist_w_phone <- df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))


## plot distribution
plot_df2_dist_w_phone <- (
  ggplot(data=df2_dist_w_phone
         , aes(x=W_PHONE, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="orange") +
    theme_minimal()
)

plot_df2_dist_w_phone
#la maggiorparte dei clienti ha aggiunto il numero di telefono (più del 92%)


## compute distribution TYP_CLI_ACCOUNT

df2_dist_TYP_CLI_ACCOUNT <- df_2_cli_account_clean %>%
  group_by(TYP_CLI_ACCOUNT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))


## plot distribution
plot_df2_TYP_CLI_ACCOUNT <- (
  ggplot(data=df2_dist_TYP_CLI_ACCOUNT
         , aes(x=TYP_CLI_ACCOUNT, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="orange") +
    theme_minimal()
)

plot_df2_TYP_CLI_ACCOUNT
#il 90% dei clienti ha un account di tipo 4 (più di 30000 clienti)


## compute distribution TYP_JOB

df2_dist_TYP_JOB <- df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))


## plot distribution
plot_df2_TYP_JOB <- (
  ggplot(data=df2_dist_TYP_JOB
         , aes(x=TYP_JOB, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="orange") +
    theme_minimal()
)

plot_df2_TYP_JOB
#la maggiorparte dei clienti non ha indicato che tipo di lavoro svolge


#### FINAL REVIEW df_2_clean ####

str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)
