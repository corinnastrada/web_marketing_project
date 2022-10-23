#### FIRST LOOK of df_6 ####

str(df_6_camp_event)
summary(df_6_camp_event)

#### START CLEANING df_6 ####

df_6_camp_event_clean <- df_6_camp_event

#### CLEANING DATA TYPES in df_6 ####

## formatting dates and times ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATETIME = as.POSIXct(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S")) %>%
  mutate(EVENT_HOUR = hour(EVENT_DATETIME)) %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATETIME))

#si creano 3 variabili: EVENT_DATETIME indica data e ora, EVENT_DATE la data e 
#EVENT_HOUR l'ora (senza minuti nè secondi)


#### CONSISTENCY CHECK ID_CLI in df_1/df_6 ####

cons_idcli_df1_df6 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_6) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df6

#!!! NOTE: all ID_CLI in df_6 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_6 !!!#  


#### CONSISTENCY CHECK ID_CAMP in df_5/df_6 ####

cons_idcamp_df5_df6 <- df_5_camp_cat_clean %>%
  select(ID_CAMP) %>%
  distinct() %>%
  mutate(is_in_df_5 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CAMP) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CAMP"
  ) %>%
  group_by(is_in_df_5, is_in_df_6) %>%
  summarize(NUM_ID_CAMPs = n_distinct(ID_CAMP)) %>%
  as.data.frame()

cons_idcamp_df5_df6

#!!! NOTE: all ID_CAMP in df_6 are mapped in df_5, but not all ID_CAMP in df_5 are mapped in df_6 !!!#


#### RESHAPING df_6 ####

## remapping TYPE_EVENT values "E" [ERROR] and "B" [BOUNCE] into a level "F" [FAILURE] ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))

## adding type from df_5 ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")
#il dataset risultante contiene ora le informazioni riguardanti il tipo di campagna, associato ad ogni ID_CAMP.


## organize the data adding to each sending event the corresponding opens/clicks/fails

# sends
df_sends <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE = EVENT_DATE) %>%
  as.data.frame()
#si crea un dataframe prendendo le righe dove l'evento è S (send), si mantengono solo le colonne nella select,
#si esclude la variabile TYP_EVENT.
 

# opens
# there could be multiple opens of the same communication
# 1- count the open events
# 2- consider explicitely only the first open

df_opens_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , OPEN_DATETIME = EVENT_DATETIME
         , OPEN_DATE = EVENT_DATE)
#si crea un dataframe prendendo le righe dove l'evento è V (open), si mantengono solo le colonne nella select, 
#si esclude la variabile TYP_EVENT.

total_opens <- df_opens_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarize(NUM_OPENs = n_distinct(ID_EVENT_O))
total_opens
#si somma il numero di eventi "open" raggruppati per cliente, campagna e delivery. Se NUM_OPENs è uguale a 2
#significa che quel cliente ha aperto la mail ricevuta per quella campagna 2 volte.
  
df_opens <- df_opens_prep %>%
  left_join(total_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(OPEN_DATETIME == min(OPEN_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()
df_opens
#si considera solamente la prima volta che il cliente apre la mail. Se il cliente la apre 2 volte, in due orari 
#diversi, si considera solamente la data meno recente.

# clicks
# there could be multiple clicks of the same communication
# 1- count the click events
# 2- consider explicitely only the first click

df_clicks_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT
       , ID_CLI
       , ID_CAMP
       , TYP_CAMP
       , ID_DELIVERY
       , CLICK_DATETIME = EVENT_DATETIME
       , CLICK_DATE = EVENT_DATE)

total_clicks <- df_clicks_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarize(NUM_CLICKs = n_distinct(ID_EVENT_C))

df_clicks <- df_clicks_prep %>%
  left_join(total_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(CLICK_DATETIME == min(CLICK_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()
#stesse analisi di "open"

# fails
df_fails <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , FAIL_DATETIME = EVENT_DATETIME
         , FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATETIME == min(FAIL_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()
#si considera solo l'evento fails e si prende la riga in cui compare per la prima volta il messaggio di fail


# combine sends opens clicks and fails
df_6_camp_event_clean_final <- df_sends %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")     
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%     
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(CLICK_DATE) | OPEN_DATE <= CLICK_DATE) %>%     
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%     
  mutate(OPENED = !is.na(ID_EVENT_O)) %>%
  mutate(CLICKED = !is.na(ID_EVENT_C)) %>%
  mutate(FAILED = !is.na(ID_EVENT_F)) %>%
  mutate(DAYS_TO_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  select(ID_EVENT_S
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE
         
         , OPENED
         , OPEN_DATE
         , DAYS_TO_OPEN
         , NUM_OPENs
         
         , CLICKED
         , CLICK_DATE
         , NUM_CLICKs
         
         , FAILED
         )
#si prende il dataset df_sends e si fa left join con i dataset di open, click e fail. Poichè si tratta
#di un left join significa che tutte le righe di df_sends sono considerate. Se le mail oltre ad essere inviate
#sono anche aperte, ad esempio, avremo una data sulla variabile OPEN_DATE, altrimenti un dato mancante.
#Analoghe considerazioni per click e fail. Inoltre vengono aggiunte tre variabili booleane: OPENED, CLICKED,
#FAILED che indicano se la mail è stato aperta, cliccata, fallita o meno.



#### EXPLORE VARIABLES in df_6 ####

### GENERAL OVERVIEW ###

## compute aggregate
df6_overview <- df_6_camp_event_clean_final %>% 
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))     

df6_overview
#si considera la prima data e l'ultima di invio di una mail, il numero totale di mail mandate e il numero 
#totale di clienti che hanno ricevuto le mail; il distinct è necessario poichè più righe del dataset possono
#fare riferimento allo stesso cliente che ha ricevuto mail per diverse campagne.
#In totale sono state inviate 1556646 mail a 190427 clienti.


### GENERAL OVERVIEW by TYP_CAMP ###

## compute aggregate
df6_overviewbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP) %>%
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overviewbytyp
#come sopra, ma raggruppato per tipo di campagna

## plot aggregate
plot_df6_overviewbytyp <- (
  ggplot(data=df6_overviewbytyp
         , aes(x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_overviewbytyp
#grafico che mostra il numero totale di mail inviate per tipo di campagna


### Variable OPENED ###

## compute aggregate
df6_dist_opened <- df_6_camp_event_clean_final %>%
  group_by(OPENED) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_opened
#si raggruppa in base al fatto che il cliente ha aperto o meno la mail inviata. Si calcola il totale delle mail
#inviate e il totale dei clienti che hanno aperto o meno la mail. Infine si calcola la percentuale per ogni gruppo
#rispetto al totale delle mail inviate e del totale dei clienti.

## plot aggregate
plot_df6_dist_opened <- (
  ggplot(data=df6_dist_opened
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    theme_minimal()
)

plot_df6_dist_opened
#si mostra graficamente la percentuale delle mail inviate aperte e non aperte. Il 17.9% delle mail
#sono state aperte.


### Variable OPENED by TYP_CAMP ###

## compute aggregate
df6_dist_openedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, OPENED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , OPENED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_openedbytyp
#come prima ma raggruppato anche per tipo di campagna

## plot aggregate
plot_df6_dist_openedbytyp <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_dist_openedbytyp

## plot aggregate percent
plot_df6_dist_openedbytyp_percent <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_dist_openedbytyp_percent
#si osserva una percentuale leggermente maggiore di mail aperte per le campagne PRODUCT


### Variable DAYS_TO_OPEN

## compute aggregate
df6_dist_daystoopen <- df_6_camp_event_clean_final %>%
  filter(OPENED) %>%  #filtra per i clienti che hanno aperto la mail
  group_by(ID_CLI) %>%
  summarize(AVG_DAYS_TO_OPEN = floor(mean(DAYS_TO_OPEN))) %>%
  ungroup() %>%
  group_by(AVG_DAYS_TO_OPEN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))     
#per prima cosa per ogni cliente si calcola in media quanti giorni ci mette ad aprire la mail; 
#poi si calcola il numero totale di clienti che ci hanno messo 1, 2, ..., n giorni per aprire la mail

df6_dist_daystoopen

## plot aggregate
plot_df6_dist_daystoopen <- (
  ggplot(data=df6_dist_daystoopen %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal() +
    labs(x="dopo quanti giorni viene aperta la mail",
         y="numero clienti")
)

plot_df6_dist_daystoopen
#l'istogramma mostra che la maggiorparte dei clienti apre la mail il giorno stesso. Il numero di clienti che 
#apre la mail dopo che è stato spedita decresce esponenzialmente con il passare dei giorni dal giorno in cui
#viene spedita la mail.


### DAYS_TO_OPEN vs CUMULATE PERCENT ###

## compute aggregate
df6_dist_daystoopen_vs_cumulate <- df6_dist_daystoopen %>%
  arrange(AVG_DAYS_TO_OPEN) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

## plot aggregate
plot_df6_dist_daystoopen_vs_cumulate <- (
  ggplot(data=df6_dist_daystoopen_vs_cumulate %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=PERCENT_COVERED)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14) +
    theme_minimal()
)

plot_df6_dist_daystoopen_vs_cumulate
#percentuale cumulata di clienti che hanno aperto la mail dopo tot. giorni
#l'andamento del grafico è esponenziale, ad indicare che chi apre la mail tendenzialmente la apre dopo
#pochi giorni


#### ???? TO DO df_6 ???? ####
# EXPLORE the following relevant variables in df_6_camp_event_clean_final:
# - CLICKED/CLICKED by TYP_CAMP
# - FAILED/FAILED by TYP_CAP
# - NUM_OPENs
# - NUM_CLICKs


## Si esplori CLICKED/CLICKED by TYP_CAMP

df6_dist_clickedbytyp <- df_6_camp_event_clean_final                        %>%
                          group_by(TYP_CAMP, CLICKED)                       %>% 
# Si raggruppa per TYP_CAMP & CLICKED
                          summarise(TOT_EVENTs = n_distinct(ID_EVENT_S),
                                    TOT_CLIs = n_distinct(ID_CLI))         %>% 
# Calcolo il numero totale di clienti e il numero totale di mail inviate
                          left_join(df6_overviewbytyp %>%
                                      select(TYP_CAMP,
                                             ALL_TOT_EVENTs = TOT_EVENTs,
                                             ALL_TOT_CLIs = TOT_CLIs),
                                    by = 'TYP_CAMP')                       %>% 
# Left Join con df6_overviewbytyp
                          mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs,
                                 PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs)     %>% 
# Calcolo la percentuale
                          select(TYP_CAMP,
                                 CLICKED,
                                 TOT_EVENTs,
                                 TOT_CLIs,
                                 PERCENT_EVENTs,
                                 PERCENT_CLIs)    # Mantengo queste variabili

ggplot(data = df6_dist_clickedbytyp,
       aes(fill = CLICKED,
           x = TYP_CAMP,
           y = TOT_EVENTs)) +       
  geom_bar(stat = "identity") +     
  theme_minimal()
#Numero totale di mail inviate per tipo di campagna, distinte per mail cliccate o meno.
#La maggiorparte di mail sono di tipo NATIONAL, mentre in generale una bassa percentuale di mail viene cliccata

ggplot(data = df6_dist_clickedbytyp,
       aes(fill = CLICKED,
           x = TYP_CAMP,
           y = TOT_EVENTs)) +       
  geom_bar(position = "fill",
           stat = "identity") +     
  theme_minimal()                   
#Stesso grafico di prima, ma viene mostrata la percentuale


# Si esplori FAILED/FAILED by TYP_CAP

df6_dist_failedbytyp <- df_6_camp_event_clean_final                    %>%
  group_by(TYP_CAMP,
           FAILED)                             %>% 
  # Si raggruppa per TYP_CAMP & FAILED
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S),
            TOT_CLIs = n_distinct(ID_CLI))     %>% 
  # Numero totale di clienti e di mail mandate
  left_join(df6_overviewbytyp                  %>%
              select(TYP_CAMP,
                     ALL_TOT_EVENTs = TOT_EVENTs,
                     ALL_TOT_CLIs = TOT_CLIs),
            by = 'TYP_CAMP')                   %>% 
  # Left Join con df6_overview by type
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs,
         PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%    # Percentuale
  select(TYP_CAMP,
         FAILED,
         TOT_EVENTs,
         TOT_CLIs,
         PERCENT_EVENTs,
         PERCENT_CLIs)  # Mantengo queste variabili


ggplot(data = df6_dist_failedbytyp,
       aes(fill = FAILED,
           x = TYP_CAMP,
           y = TOT_EVENTs)) +   
  geom_bar(stat = "identity") +
  theme_minimal() 
#Numero totale di mail inviate per tipo di campagna, distinte per mail fallite o meno.
#In generale sono poche le mail fallite, e non ci sono differenze significative rispetto al tipo di campagna.

ggplot(data = df6_dist_failedbytyp,
       aes(fill = FAILED,
           x = TYP_CAMP,
           y = TOT_EVENTs)) +   
  geom_bar(position = "fill",   
           stat = "identity") + 
  theme_minimal() 
#Stesso grafico di prima, ma viene mostrata la percentuale


# Si esplori NUM_OPENs

df6_dist_numopens <- df_6_camp_event_clean_final                  %>%
  group_by(NUM_OPENs)                        %>% # Si raggruppa per NUM_OPENs
  summarise(TOT_ID = n_distinct(ID_EVENT_S)) %>% # Numero totale di mail inviate distinte per NUM_OPENs
  mutate(PERCENT = TOT_ID/sum(TOT_ID))       %>% # Percentuale
  arrange(desc(PERCENT))                         

df6_dist_numopens
#la maggiorparte delle mail non vengono aperte (82%), le altre sono aperte una volta (14,2%), mentre è più raro
#che vengano aperte più volte

ggplot(data = df6_dist_numopens,
       aes(x = NUM_OPENs,
           y = TOT_ID)) +        
  geom_bar(stat = "identity",
           fill = "orange") + 
  xlim(0, 15) +                 
  theme_minimal()                

# Si esplori NUM_CLICKs

df6_dist_numclicks <- df_6_camp_event_clean_final                   %>%
  group_by(NUM_CLICKs)                       %>% # Raggruppo per NUM_CLICKs
  summarise(TOT_ID = n_distinct(ID_EVENT_S)) %>% # Numero totale di mail inviate distinte per NUM_CLICKs
  mutate(PERCENT = TOT_ID/sum(TOT_ID))       %>% # Percentuale
  arrange(desc(PERCENT))                         

df6_dist_numclicks
#il 97,7% delle mail inviate non vengono cliccate

ggplot(data = df6_dist_numclicks,
       aes(x = NUM_CLICKs,
           y = TOT_ID)) +        # Data to Plot
  geom_bar(stat = "identity",
           fill = "orange") + # Bar Plot
  xlim(0, 15) +                  # X Limit
  theme_minimal()                # ggplot Theme


#### FINAL REVIEW df_6_clean ####

str(df_6_camp_event_clean_final)
summary(df_6_camp_event_clean_final)
