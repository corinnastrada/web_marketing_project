
##MARKET BASKET ANALYSIS##

##Implementiamo la MBA per i 100 item che vengono acquistati maggiormente

count_tickets <- df_7_tic_clean_final %>%
  group_by(ID_ARTICOLO) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_max(count, n = 100)  #prende i 100 item pi? acquistati

count_tickets %>%
  slice_max(count, n = 10) %>%
  ggplot(aes(x = reorder(ID_ARTICOLO, count), y = count)) +
  geom_bar(stat= "identity", fill = "purple") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + # Centering Title
  theme(axis.text.x=element_text(angle = 60, vjust = 0.5)) +
  labs(x = "Id articolo",
       y = "Totale acquisti",
       title = "Top 10 dei prodotti pi? venduti")

#l'articolo 33700716 ? di gran lunga il pi? acquistato, con 58335 vendite


tickets_ordered <- df_7_tic_clean_final[order(df_7_tic_clean_final$ID_CLI),]
#si ordina per id cliente

itemList <- plyr::ddply(df_7_tic_clean_final, c("ID_CLI", "TIC_DATE"),
                        function(df1)paste(df1$ID_ARTICOLO, 
                                           collapse = ","))
#lista degli articoli acquistati raggruppati per cliente e data di acquisto


itemList$ID_ARTICOLO <- NULL
itemList$TIC_DATE <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
summary(tr)

##articoli venduti maggiormente:

##  33700716 33817091 34843564 32882024 34252904  (Other) 
##     57324    24524    12715     6477     4807  5411676 


itemFrequencyPlot(tr, topN = 20, type = 'absolute',col='purple')
#grafico che mostra il numero di volte in cui sono stati acquistati gli articoli (vengono mostrati
#i pi? venduti)

rules <- apriori(tr, parameter = list(supp = 0.001, conf = 0.8))
rules <- sort(rules, by = 'confidence', decreasing = TRUE)
summary(rules)
inspect(rules)
#si valuta l'affinit? tra prodotti con Confidence maggiore di 0.8 e Supporto maggiore di 0.001.
#Si ordina in senso decrescente in base al valore di Confidence.


topRules <- rules[1:10]
inspect(topRules)
#questi sono i prodotti con maggiore affinit?:
#i valori di Lift sono molto alti, ad indicare che acquistare questi prodotti insieme ? pi? probabile che
#acquistarli separatemente.
#la Confidenza ? maggiore di 0.9 per tutti questi articoli, cio? la prob. condizionata di acquistare l'articolo Y
#dato l'acquisto dell'articolo X o di alcuni articoli ? molto elevata
#In particolare, la probabilit? di acquistare il prodotto 32079103 dopo aver acquistato i prodotti
#{32078795, 32079082, 32842551} ? del 95%.
#L'associazione avvenuta pi? spesso, 1303 volte, ? tra il prodotto 32079103 e i prodotti {32078935, 32842551}

library(arulesViz)
plot(topRules)
plot(topRules, method = "graph",col='purple')

