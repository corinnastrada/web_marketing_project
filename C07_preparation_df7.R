#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)
#si creano 3 variabili: EVENT_DATETIME indica data e ora, EVENT_DATE la data e EVENT_HOUR l'ora (senza minuti nè secondi)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  


#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE, week_start=1)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 6) ~ "weekday"
    , TRUE ~ "other"
    )
  )
#TIC_DATE_WEEKDAY va da 1 a 7: 1 sta per lunedì, 7 sta per domenica
#TIC_DATE_HOLIDAY indica se il giorno è festivo o no, in Italia
#TIC_DATE_TIP indica se il giorno è festivo, se è un giorno della settimana o se è un giorno nel week-end



#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_tic_clean_final %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)     
            , TOT_CLIs = n_distinct(ID_CLI))     

df7_overview
#si calcola la prima data in cui è stata compiuta una transazione e l'ultima.
#si calcola il numero totale di scontrini: più righe possono indicare lo stesso scontrino, poichè uno scontrino
#può includere più acquisti/resi, perciò viene inserito il distinct
#si calcola il numero totale di clienti che hanno fatto almeno una transazione
#Sono stati emessi quasi 1 milione di scontrini da 212124 clienti diversi


### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction
#si raggruppa in base al fatto che si tratti di un acquisto o reso
#si calcola per ogni categoria il numero totale di clienti e di scontrini e la percentuale
#si osserva che tutti i clienti hanno fatto almeno un acquisto, mentre il 22% dei clienti ha fatto 
#almeno un reso


### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour
#calcola il totale e la percentuale di scontrini e di clienti, distinti per acquisto/reso e 
#considerando l'ora del giorno

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_hour
#grafico che indica la frequenza assoluta di scontrini di acquisti e di resi emessi in base all'ora del giorno.
#I momenti nei quali gli acquisti sono più frequenti sono la mattina e il pomeriggio

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent
#si calcola la percentuale di acquisti e di resi in base all'ora del giorno. Non si osservano differenze 
#significative nel rapporto acquisti/resi in base all'ora, ad eccezione delle primissime ore del giorno, 
#la cui dimensione del campione è tuttavia notevolmente più bassa rispetto alle altre ore del giorno.


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
            ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
    select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
    
df7_dist_dep
#raggruppa per reparto e se si tratta di acquisto o reso. Si calcola quindi il totale degli scontrini, dei clienti
#e le relative percentuali sul totale degli scontrini e dei clienti

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    labs(x="codice reparto",
         y="numero scontrini")
)

plot_df7_dist_dep
#si mostra il totale di scontrini per reparto, e per ciascun reparto la quantità di acquisti e di resi
#la maggiorparte degli acquisti provengono dai reparti 10 e 3.

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent
#si calcola la percentuale di acquisti e di resi in base al reparto. Si osservano differenza significative
#in base al reparto, infatti la quantità di resi nel reparto 6 è maggiore che negli altri reparti.


### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp
#stesso discorso di prima: si raggruppa per tipo di giorno (giorno della settimana, weekend, giorno festivo) e 
#se si tratta di acquisto/reso


## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    labs(x="tipo di giorno",
         y="numero degli scontrini")
)

plot_df7_dist_datetyp
#totale degli scontrini in base al giorno, con distinzione tra acquisto e reso

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal() 
)

plot_df7_dist_datetyp_percent
#si calcola la percentuale di acquisti e di resi in base al tipo di giorno. Non si osservano differenza 
#significative nel rapporto acquaiti/resi in base al tipo di giorno


### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

## compute aggregate
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()
#si fa la somma dell'importo lordo degli acquisti e/o resi di uno scontrino; 
#la stessa cosa per gli sconti

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto
#Si calcola la media dell'importo lordo di tutti gli scontrini, sia per gli acquisti che per i resi;
#stessa cosa per gli sconti.
#Mediamente l'importo lordo di un acquisto è 164$, lo sconto medio applicato sui prodotti acquistati è 11.8$;
#Mediamente l'importo lordo di un un reso è -110$, lo sconto medio applicato sui resi è -8.29$;


## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal() +
    labs(y="frequenza assoluta")
)

plot_df7_dist_importo
#frequenza degli importi lordi degli scontrini dei clienti, divisi per acquisto e reso;
#la distribuzione, per gli acquisti, è concentrata su valori poco sopra lo 0, ad indicare che la maggiorparte
#degli acquisti di uno scontrino è a cifre contenute. I resi sono in numero inferiore ripsetto
#agli acquisti e anch'essi sono relativi a cifre piuttosto basse.


## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto
#la distribuzione degli sconti è concentrata attorno allo 0 sia per acquisti che per resi



#### ???? TO DO df_7 ???? ####
# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO
# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)
# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

# compute the distribution of customers by number of purchases (as described in the slides)


# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO
df7_dist_importosconto_cod_rep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>% 
  # Raggruppo per COD_REPARTO & Purchases
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO)) %>%  # Calcolo la somma dell'importo e degli sconti
  ungroup() %>%
  as.data.frame()


# IMPORTO_LORDO
ggplot(data = df7_dist_importosconto_cod_rep,
       aes(fill = DIREZIONE,
           x = COD_REPARTO,
           y = IMPORTO_LORDO)) + 
  geom_bar(stat = "identity") +   
  theme_minimal()                 
#si osserva che gli importi maggiori derivano dai reparti 2, 6 e 7; stesse considerazioni per i resi

# SCONTO
ggplot(data = df7_dist_importosconto_cod_rep,
       aes(fill = DIREZIONE,
           x = COD_REPARTO,
           y = SCONTO)) +      
  geom_bar(stat = "identity") + 
  theme_minimal()               
#si osserva che gli sconti maggiori derivano dai reparti 2, 6 e 7; stesse considerazioni per i resi



# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)

df_7_tic_clean_final$ID_ARTICOLO <- as.factor(df_7_tic_clean_final$ID_ARTICOLO)

df7_dist_id_articolo <- df_7_tic_clean_final                                       %>%
                          filter(DIREZIONE == 1)                            %>% # considero gli acquisti
                          group_by(ID_ARTICOLO)                             %>% # raggruppo per articolo
                          summarise(NUM_TICs = n_distinct(ID_SCONTRINO)) %>% # numero totale di scontrini dove è stato acquistato un determinato articolo
                          ungroup()                                         %>%
                          as.data.frame()                                   %>%
                          arrange(desc(NUM_TICs))
df7_dist_id_articolo
#l'articolo 33700716 è l'articolo più acquistato, 57806 volte



# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

df7_dist_importosconto_id_cli <- df_7_tic_clean_final                      %>%
                                    filter(DIREZIONE == 1)          %>% # considero gli acquisti
                                    group_by(ID_CLI)                %>% # raggruppo per id cliente
                                    summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
                                              SCONTO = sum(SCONTO)) %>% # somma della spesa e degli sconti di un singolo cliente
                                    ungroup()                       %>%
                                    as.data.frame()                 %>%
                                    arrange(desc(IMPORTO_LORDO))

df7_dist_importosconto_id_cli
#il cliente 572977 è quello che ha l'importo lordo maggiore 421636.20 euro.

##in questo caso non è stato realizzato un plot a causa della numerosità degli id dei clienti


## Let's plot the Total Purchase Distribution:

df7_dist_tot_purchase <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1)                             %>% # considero gli acquisti
  group_by(ID_CLI)                                   %>% # raggruppo per id cliente
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>% # numero totale di scontrini associati a un cliente
  arrange(desc(TOT_PURCHASE))                            

df7_dist_tot_purchase
#il cliente 376925 è quello che ha fatto più scontrini (177). Non è però lo stesso cliente che aveva
#associato il maggior importo lordo.


#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)
