# Bacheloroppgave 2024

#laster inn de nødvendige pakkene
suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(ggthemes)
  library(hrbrthemes)
  library(car)
  library(sjPlot)
  library(stargazer)
  library(ggridges)
  library(corrplot)
  library(RColorBrewer)
  library(broom)
  library(purrr)
  library(jtools)
  library(kableExtra)
})


# Henter datasettet 
OrginalDatasett <- read_csv("NSD3127.csv")

#velger de mest intressante variabelene for analysen, og gir mer intuitive navn
data_tillit <- OrginalDatasett %>% 
  select("Kjønn" = r26P1, 
         "Fødselsår" = r26P5_1, 
         "Sivilstatus" = r26_bgciv, 
         "Landsdel" = r26P2, 
         "Antall_barn" = r26k2_bgchi,
         "Utdanning" = r26P4_1,
         "Arbeid" = r26_bgday,
         "Helse(1)" = r26_bghea, 
         "Helse(2)" = r26_bghel, 
         "Brutto_inntekt" = r26k2_bginc, 
         "Økonomisk_situasjon_i_dag" = r26_pceco,
         "Økonomisk_situasjon_om_ett_år" = r26_pcecy,
         "Sosial_tillit" = r26_pccoo)




data_tillit %>% 
  filter(!Sosial_tillit %in% c(97, 98)) %>% 
  group_by(Økonomisk_situasjon_i_dag) %>% 
  summarise(length(Sosial_tillit), mean(Sosial_tillit))



data_tillit2 <- data_tillit %>% 
  mutate(Aldersgruppe = factor(Fødselsår, 
                               levels = c(1, 2, 3, 4, 5, 6, 7), 
                               labels = c("1939 eller tidligere", 
                                          "1940-1949", "1950-1959", "1960-1969", 
                                          "1970-1979", "1980-1989", 
                                          "1990 eller senere"))) %>% 
  mutate(Utdanningsnivå = factor(Utdanning, 
                                 levels = c(1, 2, 3), 
                                 labels = c("grunnskole", "videregående", "høyere"), 
                                 exclude = 97)) %>%
  mutate(Økonomisk_situasjon = factor(Økonomisk_situasjon_i_dag, 
                                      levels = c(1, 2, 3, 4, 5, 6, 7),
                                      labels = c("Svært god", "God", "Noe god", 
                                                 "Verken god eller dårlig", "Noe dårlig", 
                                                 "Dårlig", "Svært dårlig"), 
                                      exclude = c(97, 98))) %>% 
  mutate(Årsinntekt = factor(Brutto_inntekt, 
                             levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                             labels = c("Mindre enn 150 000", "150 001 - 300 000", 
                                        "300 001 - 400 000", "400 001 - 500 000", 
                                        "500 001 - 600 000", "600 001 - 700 000", 
                                        "700 001 - 1 000 000", "Mer enn 1 000 000"),
                             exclude = c(97, 98))) %>% 
  mutate(gift_eller_samboer = case_when(
    Sivilstatus %in% c(97, 98)  ~ NA_real_,
    Sivilstatus %in% c(1, 2, 5, 6, 7) ~ 0,
    TRUE ~ 1)) %>% 
  mutate(Mann = ifelse(Kjønn == 1, 
                       1, 
                       0)) %>% 
  mutate(Barn = case_when(
    Antall_barn %in% c(97,98)  ~ NA_real_,
    Antall_barn %in% 0 ~ 0, 
    TRUE ~ 1))

data_tillit2 <- data_tillit2 %>% 
  filter(!Sosial_tillit %in% c(97, 98)) %>% 
  select(Sosial_tillit, Aldersgruppe, Utdanningsnivå, Økonomisk_situasjon, Årsinntekt, gift_eller_samboer, Mann, Barn)






############################################

data <- data_tillit2 %>% 
  mutate(Økonomisk_situasjon = fct_relevel(Økonomisk_situasjon, rev), 
         Aldersgruppe = fct_relevel(Aldersgruppe, rev))


write.csv(data, file = "data_tillit.csv")

Reg <- lm(Sosial_tillit ~ Økonomisk_situasjon + Aldersgruppe + Mann + Utdanningsnivå + gift_eller_samboer + Barn, data = data)


# endrer referansenivå i Økonomisk situasjon
data_2 <- data %>% 
  mutate(Økonomisk_situasjon = relevel(Økonomisk_situasjon, 
                                       ref = "Verken god eller dårlig"))


# Regresjonsmodel 1
Regresjonsmodell_1 <- lm(Sosial_tillit ~ Økonomisk_situasjon + Aldersgruppe + Mann + Utdanningsnivå + gift_eller_samboer + Barn, data = data_2)


# Regresjonsmodell 2 - med årsinntekt
Regresjonsmodell_2 <- lm(Sosial_tillit ~ Økonomisk_situasjon + Aldersgruppe + Mann + Utdanningsnivå + gift_eller_samboer + Barn + Årsinntekt, data = data_2)




summary(Regresjonsmodell_1)
summary(Regresjonsmodell_2)


#variance inflation factor

vif(Regresjonsmodell_1)
vif(Regresjonsmodell_2)


# Figur 1

# Figur
data %>% 
  na.omit() %>% 
  ggplot(aes(x = Økonomisk_situasjon, y = Sosial_tillit, 
             fill= Økonomisk_situasjon)) + 
  geom_violin() +
  labs(x = " ", y = "Sosial Tillit", 
       title = "Sosial Tillit vs. Økonomisk Situasjon", 
       subtitle = "Spørsmål: Hvordan vurderer du din egen økonomiske situasjon i dag?") +
  theme_fivethirtyeight() +
  theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size =8), 
        axis.title.y = element_text(size = 10), 
        plot.title = element_text(face = "bold", size = 15), 
        plot.subtitle = element_text(size = 10), 
        plot.background = element_rect(color = "white"))


# tabell 1

# Generer deskriptiv statistikk for datasettet
descriptive_stats <- describe(data_tillit2)
# Skriv ut tabellen
print(descriptive_stats)



# Lag tabellen
tabell <- data.frame(  Variabel = c("Sosial_tillit", "Aldersgruppe*", "Utdanningsnivå*", "Økonomisk_situasjon*",                "Årsinntekt*", "gift_eller_samboer", "Mann", "Barn"),  `Antall observasjoner` = c(7793, 7787, 7705, 7735, 7604, 6167, 7793, 6149),  Gjennomsnitt = c(7.0626203, 3.9924233, 2.5993511, 2.6178410, 4.8364019, 0.7285552, 0.5041704, 0.1735242),  Standardavvik = c(1.7921888, 1.5397196, 0.5782710, 1.2606073, 1.9675950, 0.4447410, 0.5000147, 0.3787306),  Minimum = c(0, 1, 1, 1, 1, 0, 0, 0),  Maksimum = c(10, 7, 3, 7, 8, 1, 1, 1))
# Konverter tabellen til et pent bilde
kable(tabell, format = "html") %>%  
  kable_styling(full_width = FALSE)




# Tabell 2
stargazer(Regresjonsmodell_1, Regresjonsmodell_2, type = "latex", 
          title = "Resultater fra Regresjonsanalysen (Avhengig Variabel: Sosial Tillit)",
          styles = "qje",
          dep.var.caption = " ",
          single.row = TRUE, 
          column.labels = c("Modell 1", "Modell 2"),
          model.numbers = FALSE,
          intercept.bottom = FALSE, 
          digits = 3,
          dep.var.labels.include = FALSE,
          notes.append = FALSE,
          omit.stat = "rsq",
          out = "regresjonsresultat.txt")


# figur 2

plot_summs(Regresjonsmodell_1, Regresjonsmodell_2, 
           coefs = c("Svært god økonomi"= "Økonomisk_situasjonSvært god", 
                     "God økonomi"= "Økonomisk_situasjonGod",
                     "Noe god økonomi"= "Økonomisk_situasjonNoe god", 
                     "Noe dårlig økonomi"= "Økonomisk_situasjonNoe dårlig", 
                     "Dårlig økonomi"= "Økonomisk_situasjonDårlig", 
                     "Svært dårlig økonomi"= "Økonomisk_situasjonSvært dårlig"), 
           colors = c("violet", "turquoise1"), 
           legend.title = "Regresjonsmodell", 
           model.names = c("Modell 1", "Modell 2"))










