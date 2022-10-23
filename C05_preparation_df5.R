#### FIRST LOOK of df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### CLEANING LOW VARIANCE in df_5 ####
#elimino la variabile "CHANNEL_CAMP" 

df_5_camp_cat_clean <- df_5_camp_cat_clean %>% 
  select(-CHANNEL_CAMP)


##Esploriamo la variabile TYP_CAMP:

df5_TYP_CAMP <- df_5_camp_cat_clean                               %>%
                          group_by(TYP_CAMP)                      %>% #raggruppo per tipo di campagna
                          summarise(TOT_ID = n_distinct(ID_CAMP)) %>% #calcolo il numero totale di campagne
                          mutate(PERCENT = TOT_ID/sum(TOT_ID))    %>% #percentuale
                          arrange(desc(PERCENT))                      

ggplot(data = df5_TYP_CAMP,
       aes(x = TYP_CAMP,
           y = TOT_ID)) +                     
  geom_bar(stat = "identity", 
           fill = "orange") +                 
  theme_minimal()                             

#la campagna più frequente è PRODUCT, quella meno frequente LOCAL


#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)

