
##MODELLO RFM

## Il modello si basa su 3 indici quantitativi: 

## Recency: indica quanto di recente il cliente ha effettuato un acquisto. 
##          Ci aspettiamo che più questo è vicino, maggiore è la probabilità che quel cliente
##          scelga di nuovo l'azienda in futuro.
## Frequency: dice quanto spesso il cliente acquista.  Anche in questo caso, 
##            la probabilità di un eventuale acquisto futuro cresce all'aumentare della frequenza.
## Monetary: indica l'ammontare speso dal cliente nel periodo di riferimento



#PURCHASE TIME SCALE
    
# abbiamo osservato che il primo scontrino è stato emesso il 2018-05-01 e l'ultimo scontrino è stato 
# emesso il 2019-04-30

df_7_tic_clean_final$TIC_DATE <- as.Date(df_7_tic_clean_final$TIC_DATE)
  
df <- df_7_tic_clean_final
  
df <- df[order(df$ID_CLI, rev(df$TIC_DATE)),]
#ordiniamo df per ID_CLI
  
dft2 <- df %>%
    group_by(ID_SCONTRINO) %>%
    summarise(ID_CLI = max(ID_CLI),TIC_DATE=max(TIC_DATE)) ##per prendere ogni scontrino una volta sola
  
dft2 <- dft2[order(dft2$ID_CLI, rev(dft2$TIC_DATE)),]
#ordiniamo dft2 per ID_CLI
  
dft3 <- dft2 %>% group_by(ID_CLI) %>% summarise(tot = n()) %>% filter(tot>1) 
#si raggruppa per ogni ID_CLI e si trova quanti scontrini sono stati emessi per ogni cliente;
#filtriamo per i clienti che hanno fatto più di uno scontrino
  
dft3 <- left_join(dft3,dft2,by="ID_CLI") #aggiungiamo ID_SCONTRINO & TIC_DATE
  
dft4 <- dft3 %>%
    arrange(desc(TIC_DATE)) %>%
    group_by(ID_CLI) %>%
    summarise(last=nth(TIC_DATE,1),secondl=nth(TIC_DATE,2))
##per ogni cliente, si individua l'ultima e la penultima data di acquisto
  
p <- ggplot(dft4, aes(x= as.numeric(last - secondl))) +
    geom_histogram(color="black", fill="lightblue") +
    geom_vline(aes(xintercept = 60), color="blue", linetype="dashed", size=1) +
    labs(title = "Ultimo acquisto - penultimo acquisto", x = "days", y = "frequency") +
    scale_x_continuous(breaks=seq(0,300,30)) +
    theme_minimal()
p
  
#dal grafico osserviamo che il purchase time scale è pari a 60 giorni



#CLIENTI ATTIVI

#Consideriamo i clienti attivi se l'ultimo acquisto è stato effettuato dopo la data 28/02/2019: infatti
#il purchase time scale è di 60 giorni, perciò se un cliente non ha acquistato negli ultimi 2 mesi è 
#considerato un cliente non attivo.

rfm_study_period <- df_7_tic_clean_final %>%
                      filter(TIC_DATE > as.Date("28/02/2019",
                                                format = "%d/%m/%Y")) # Active Clients



## Recency

##Se un cliente ha effettuato di recente un acquisto, allora probabilmente sarà più propenso ad effettuare 
#un altro acquisto rispetto ad un altro cliente che invece lo ha compiuto molto tempo fa.

rfm_recency <- rfm_study_period %>%
                  filter(DIREZIONE == 1) %>% 
                  group_by(ID_CLI)       %>% 
                  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))

rfm_recency$RECENCY <- difftime(as.Date("30/04/2019",
                                        format = "%d/%m/%Y"),        
                                     rfm_recency$LAST_PURCHASE_DATE,
                                units = "days")
#calcoliamo la differenza in giorni tra la data di ultimo acquisto e il 30/04/19, ossia l'ultima data
#in cui è stato fatto uno scontrino


##Abbiamo diviso le tipologie dei valori della recency in 3 differenti gruppi. Essi sono:

## Low: al di sotto del 25mo percentule della distribuzione
## Medium: dal 25mo al 75mo percentile
## High: al di sopra del 75mo percentile

#abbiamo deciso di suddividere con questi percentili per isolare meglio i clienti low e high

rfm_recency <- within(rfm_recency,
                 REC_CLASS <- cut(as.numeric(rfm_recency$RECENCY),
                                  breaks = quantile(rfm_recency$RECENCY,
                                                    probs = c(0, .25, .75, 1)), # Quantili
                                  include.lowest = T,
                                  labels = c("low", "medium", "high")))         # Classi

rec_label <- as.data.frame(table(rfm_recency$REC_CLASS))

ggplot(data = rec_label,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity") +                   
  labs(title = "Distribuzione di Recency",
       x     = "Livelli di Recency",
       y     = "Numero di clienti") +                
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)



## Frequency

##La frequenza di acquisto è un elemento da considerare e da tenere in gran conto nel business dell'azienda.
##Essa può essere influenzata da una varietà di fattori, quali la tipologia del prodotto, il prezzo e 
##le sue variazioni e il periodo dell'anno.

rfm_frequency <- rfm_study_period                                      %>%
                    filter(DIREZIONE == 1)                             %>% 
                    group_by(ID_CLI)                                   %>% 
                    summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
                    arrange(desc(TOT_PURCHASE))
#Si calcola il numero totale di scontrini di ogni cliente. Si osserva che il cliente 728342 ha fatto 44
#scontrini negli ultimi due mesi.


summary(rfm_frequency$TOT_PURCHASE)

## La variabile frequency è stata suddivisa in 3 differenti livelli (in base al primo quartile e
## terzo quartile, cioè come prima). 
## Essi sono:
## Low: nel periodo sono stati effettuati meno di 2 acquisti 
## Medium: sono stati effettuati 2 acquisti
## High: sono stati efffettuati più di 2 acquisti (fino a 44)


rfm_frequency <- within(rfm_frequency,
                   FREQ_CLASS <- cut(rfm_frequency$TOT_PURCHASE,
                                     breaks = c(0, 2, 3, 44),             
                                     include.lowest = T,
                                     right = F,
                                     labels = c("low", "medium", "high"))) 

freq_label <- as.data.frame(table(rfm_frequency$FREQ_CLASS))
freq_label

ggplot(data = freq_label,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity") +                   
  labs(title = "Distribuzione di Frequency",
       x     = "Livelli di Frequency",
       y     = "Numero di clienti") +                
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)



## Monetary

##Conoscere quanto è lucrativa, nel periodo considerato, la relazione con il cliente può aiutare a creare azioni
#ad hoc, ad esempio di up-selling, cross-selling e aiutare ad ottimizzare le risorse spese per azioni
#di marketing e customer service

rfm_monetary <- rfm_study_period                            %>%
                  filter(DIREZIONE == 1)                    %>% 
                  group_by(ID_CLI)                          %>%
                  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
                            SCONTO = sum(SCONTO),
                            SPESA = IMPORTO_LORDO - SCONTO) %>%
                  ungroup()                                 %>%
                  as.data.frame()                           %>%
                  arrange(desc(IMPORTO_LORDO))
#Il cliente 96592 è quello che ha speso di più, quasi 157000 euro

rfm_monetary <- within(rfm_monetary,
                   MON_CLASS <- cut(rfm_monetary$SPESA,
                                    breaks = quantile(rfm_monetary$SPESA,
                                                      probs = c(0, .25, .75, 1)),
                                    include.lowest = T,
                                    labels = c("low", "medium", "high"))) # Classes
#si divide in base al 25esimo e 75esimo percentile, o primo e terzo quartile, come prima

table(rfm_monetary$MON_CLASS)


mon_label <- as.data.frame(table(rfm_monetary$MON_CLASS))

ggplot(data = mon_label,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity") +
  labs(title = "Distribuzione di Monetary",
       x     = "Livelli di Monetary",
       y     = "Numero degli acquisti") +                  
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE)




## A questo punto si uniscono i tre differenti dataset in uno unico, che viene chiamato rfm

rfm <- merge(rfm_frequency, # Frequency
             rfm_monetary,  # Monetary
             by = "ID_CLI") # Key for Merge

rfm <- merge(rfm,           # Frequency + Monetary
             rfm_recency,   # Recency
             by = "ID_CLI") # Key for Merge

(head(rfm[ , c("ID_CLI", "REC_CLASS", "FREQ_CLASS", "MON_CLASS")]))


##I percentili relativi alla frequency e alla recency sono combinati tra di loro per definire delle nuove classi.
##Esse descrivono la customer loyality.

rfm$RF <- NA

for(i in c(1:nrow(rfm))){
  if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] <- "One-Timer"
  if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] <- "One-Timer"
  if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "low") rfm$RF[i] <- "Leaving"
  if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] <- "Engaged"
  if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] <- "Engaged"
  if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "medium") rfm$RF[i] <- "Leaving"
  if(rfm$REC_CLASS[i] == "low" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] <- "Top"
  if(rfm$REC_CLASS[i] == "medium" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] <- "Top"
  if(rfm$REC_CLASS[i] == "high" && rfm$FREQ_CLASS[i] == "high") rfm$RF[i] <- "Leaving Top"
}

table(rfm$RF)

#Osserviamo numerosi clienti One-Timer (27402); le altri classi si equivalgono, mentre si registrano
#pochi Leaving Top (1104)


rf <- as.data.frame(table(rfm$RF))
rf

ggplot(data = rf,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity")  +
  scale_fill_distiller(palette = 'RdYlGn') +
  labs(title = "Distribuzione RF",
       x     = "Classi RF",
       y     = "Totale dei clienti") +                              
  theme_minimal() +                                                
  theme(plot.title = element_text(hjust = 0.5)) +                  
  scale_x_discrete(labels = c("Engaged", "Leaving", "Leaving Top",
                              "One Timer", "Top")) + 
  guides(fill = FALSE)


##Quindi, si combinano le classi RF con i gruppi relativi alla variabile Monetary
##al fine di ottenere le classi RFM

rfm$RFM <- NA

for(i in c(1:nrow(rfm))){
  if(rfm$RF[i] == "One-Timer" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Cheap"
  if(rfm$RF[i] == "Leaving" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Tin"
  if(rfm$RF[i] == "Engaged" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Leaving Top" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Top" && rfm$MON_CLASS[i] == "low") rfm$RFM[i] <- "Silver"
  
  if(rfm$RF[i] == "One-Timer" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Tin"
  if(rfm$RF[i] == "Leaving" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Engaged" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Leaving Top" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Silver"
  if(rfm$RF[i] == "Top" && rfm$MON_CLASS[i] == "medium") rfm$RFM[i] <- "Gold"
  
  if(rfm$RF[i] == "One-Timer" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Leaving" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Engaged" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Silver"
  if(rfm$RF[i] == "Leaving Top" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Gold"
  if(rfm$RF[i] == "Top" && rfm$MON_CLASS[i] == "high") rfm$RFM[i] <- "Diamond"
}

table(rfm$RFM)
#si osservano numerosi clienti Tin, pochi clienti Silver
#la somma dei clienti Gold e Diamond è di poco inferiore alla quantità totale dei clienti Tin
#le classi Bronze, Cheap e Diamond hanno quantità di clienti quasi equivalente, intorno alle 10000 unità


rfm_plot <- as.data.frame(table(rfm$RFM))

ggplot(data = rfm_plot,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                        
  geom_bar(stat = "identity") +
  scale_fill_distiller(palette = "RdYlGn") +
  labs(title = "Distribuzione RFM",
       x     = "Classi RFM",
       y     = "Numero dei clienti") +                 
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Bronze", "Cheap", "Copper", "Diamond",
                              "Gold", "Silver", "Tin")) + 
  guides(fill = FALSE)

