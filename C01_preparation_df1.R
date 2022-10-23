
##Alcune informazioni sul primo dataset df1

str(df_1_cli_fid)

summary(df_1_cli_fid)

## Le variabili sono [ID_CLI, ID_FID, ID_NEG, TYP_CLI_FID, COD_FID, STATUS_FID, DT_ACTIVE]



df_1_cli_fid_clean <- df_1_cli_fid


### Controllo dei duplicati:

##la chiave primaria in questa tabella è data dalla coppia ID_CLI (cioè id del cliente)
##e ID_FID (cioè id della carta fedeltà)

df_1_cli_fid_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ID_FIDs = n_distinct(ID_FID)
            , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
            , TOT_ROWs = n())
#non sono stati individuati duplicati


## Formattazione delle date
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## Si fissa il formato delle variabili binarie come factor 
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))


## Controllo di consistenza:

## Conteggio del numero di iscrizioni per ogni cliente (cioè per ogni ID_CLI)

num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarize(NUM_FIDs =  n_distinct(ID_FID) 
            , NUM_DATEs = n_distinct(DT_ACTIVE)
            )
## Riferito ad ogni singolo ID cliente:
## NUM_FIDs è il numero di distinte carte fedeltà 
## NUM_DATEs numero di date differenti in cui sono state attivate le carte 
## NUM_FIDs e NUM_DATEs possono essere differenti tra di loro


tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)
tot_id_cli
## ci sono 369472 distinti id dei clienti


dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

## La maggioranza dei clienti (99.8%) ha una sola carta fedeltà sottoscritta in una data.
## Ci sono tuttavia altre casistiche, ad esempio, 2 clienti hanno sottoscritto 4 carte fedeltà in una unica data

## Si esamina più in dettaglio i clienti che hanno più carte di fedeltà

## ad esempio, si guarda quali sono i clienti che hanno sottoscritto 3 carte fedeltà

num_fid_x_cli %>% filter(NUM_FIDs == 3)

## ad esempio, il cliente con id 621814 ha 3 sottoscrizioni

df_1_cli_fid %>% filter(ID_CLI == 621814)

## esse si riferiscono alle date

## 2018-10-13
## 2018-11-13
## 2018-11-20

## Per il cliente con ID 320880 invece c'è una fidelizzazione standard il 2018-04-25, e una standard + una 
## premium al 2018-04-26

df_1_cli_fid %>% filter(ID_CLI == 320880)



#### RESHAPING del primo dataframe 

## Si combinano le informazioni disponibili

## dalla prima sottoscrizione --> data di registrazione, negozio di registrazione
## dall'ultima sottoscrizione --> tipo di carta fedeltà, status
## dal conteggio delle sottoscrizioni --> numero di sottoscrizioni fatte


df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%                          ## raggruppo per ID_CLI 
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%       ## partendo dalla data più lontana di attivazione
  arrange(ID_FID) %>%                           ## si riordina ID_FID
  filter(row_number() == 1) %>% ##?
  ungroup() %>%
  as.data.frame()

## nel caso in cui ci fosse, per il cliente, più di una carta fedeltà, si prende quindi la più vecchia.
## Se ce ne fosse solo una, si prenderebbe l'unica esistente

df_1_cli_fid_first

## si esegue la medesima cosa che nel dataset precedente
## ma in questo caso si prende la carta fedeltà con data di attivazione più recente

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

## si fondono i dataset

df_1_cli_fid_clean <- df_1_cli_fid_last %>%     ##dataset che considera la carta con attivazione più recente
  select(ID_CLI
         , ID_FID
         , LAST_COD_FID = COD_FID
         , LAST_TYP_CLI_FID = TYP_CLI_FID
         , LAST_STATUS_FID = STATUS_FID
         , LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%                   ##dataset che conteggia le carte fedeltà per ogni cliente
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')
#per ogni cliente si sa la data di prima e ultima sottoscrizione, e quante carte fedeltà ha sottoscritto



## Esplorazione delle colonne del primo dataframe

## Si computa la distribuzione delle carte fedeltà in base alla loro categoria 


df1_dist_codfid <- df_1_cli_fid_clean          %>%
  group_by(LAST_COD_FID)                   %>% # Raggruppato in base al Fidelty Program
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>% # numero di clienti totali
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>% # percentuale
  arrange(desc(PERCENT))                       # si riordina per percentuale DESC

df1_dist_codfid

##per ogni categoria si evidenza sia la frequenza assoluta che quella relativa. 
##In questo caso , il 78.4% delle carte sono standard e l'11.9% sono Premium. I business con 
##una carta standard sono il 7.89% dei clienti, mentre la percentuale di business premium è solo dell'1.81%.


## Si esegue un plot di tale variabile

plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid, 
         aes(x=LAST_COD_FID, y=TOT_CLIs)) +
    geom_bar(stat="identity",                 ## Bar Plot
             fill="orange")                   ## tema di ggplot 
  + theme_minimal() 
  + labs(title = "Distribuzione in base al tipo di Fedelity Program", 
          x="Tipologia carta fedeltà",
          y="Numero di clienti") )

plot_df1_dist_codfid


# Si esplora la variabile LAST_TYP_CLI_FID

df1_dist_status <- df_1_cli_fid_clean          %>% 
  group_by(LAST_TYP_CLI_FID)               %>%   # si raggruppa in base a LAST_TYP_CLI_FID
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%   # numero di clienti totali 
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%   # percentuali
  arrange(desc(PERCENT))                         # si riordina in base alla percentuale discendente

#Si osserva che la maggiorparte dei clienti ha un main account, come è evidente anche dal grafico sottostante

# grafico
ggplot(data = df1_dist_status, 
       aes(x = LAST_TYP_CLI_FID, y = TOT_CLIs)) + 
  geom_bar(stat = "identity",                 
           fill = "orange") +                    
  theme_minimal() 


# Si esplora la variabile LAST_STATUS_FID

df2_dist_status <- df_1_cli_fid_clean          %>% 
  group_by(LAST_STATUS_FID)                %>%   # si raggruppa in base allo status dell'ultima carta fedeltà
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%   # numero di clienti totali
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%   # percentuali
  arrange(desc(PERCENT))                         # si riordina in base alla percentuale discendente

#la maggiorparte delle carte fedeltà sono attive 

# grafico
ggplot(data = df2_dist_status, 
       aes(x = LAST_STATUS_FID, y = TOT_CLIs)) + 
  geom_bar(stat = "identity",                 
           fill = "orange") +                    
  theme_minimal()          


## Si studia la distribuzione della data dell'attivazione dell'ultima carta (`LAST_DT_ACTIVE`):

df1_dist_date <- df_1_cli_fid_clean            %>%
  group_by(LAST_DT_ACTIVE)                %>%    #si raggruppa per Date of Activation
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%   #numero totale di clienti
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%   #percentuale
  arrange(desc(LAST_DT_ACTIVE))                         



ggplot(data = df1_dist_date,
       aes(x = LAST_DT_ACTIVE, y = TOT_CLIs)) + 
  geom_line() +                                  
  theme_minimal() + 
  labs(x="Data di attivazione ultima carta fedeltà",
       y="Numero di clienti")                               

#la serie storica non presenta un trend specifico e la media del numero di clienti che hanno attivato l'ultima
#carta è poco inferiore ai 1000 clienti per giorno. C'è però un picco di richieste raggiunto il 2018/11/23.


## Si studia la distribuzione di FIRST_ID_NEG

df2_dist_date <- df_1_cli_fid_clean            %>%
  group_by(FIRST_ID_NEG)                %>%      #si raggruppa per il negozio su cui è stata attivata la prima carta
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%   #numero totale di clienti
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%   #percentuale
  arrange(desc(PERCENT))                         #percentuale ordinata in senso decrescente

ggplot(data = df2_dist_date,
       aes(x = FIRST_ID_NEG, y = TOT_CLIs)) + 
  geom_line() +                                 
  theme_minimal() 

#la maggiorparte delle carte fedeltà sono state sottoscritte nel negozio con id=1, negli altri negozi
#non ci sono differenze significative.


## Si studia la distribuzione della data dell'attivazione della prima carta (`FIRST_DT_ACTIVE`):

df3_dist_date <- df_1_cli_fid_clean            %>%
  group_by(FIRST_DT_ACTIVE)                %>%   #si raggruppa per prima data di attivazione
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%   #numero totale di clienti
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%   #percentuale
  arrange(desc(PERCENT))                         

ggplot(data = df3_dist_date,
       aes(x = FIRST_DT_ACTIVE, y = TOT_CLIs)) + 
  geom_line() +                                  
  theme_minimal()                                

#la distribuzione è molto simile a quella di LAST_DT_ACTIVE, poichè la maggiorparte dei clienti ha una
#sola carta fedeltà


# Si esplora la variabile `NUM_FIDs`

df1_dist_num <- df_1_cli_fid_clean             %>%
  group_by(NUM_FIDs)                       %>% 
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>% 
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>% 
  arrange(desc(PERCENT))                       

ggplot(data = df1_dist_num,               
       aes(x = NUM_FIDs, y = TOT_CLIs)) +      
  geom_bar(stat = "identity",
           fill = "orange") +                
  theme_minimal()    

#quasi la totalità dei clienti ha una sola carta fedeltà



#### FINAL REVIEW df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)


