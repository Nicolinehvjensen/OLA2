## Pakker
library(tidyverse)
library(zoo)
library(dkstat)

## Opgave 4.1
## Hent Forbrugertillid via DST API
tillid_meta <- dst_meta(table="FORV1", lang="da")

my_query_tillid <- list(
  INDIKATOR = "*",
  Tid = "*"
)

tillid_data <- dst_get_data(table = "FORV1", query = my_query_tillid)


## Konverter fra langt til bredt format
tillid_data <- tillid_data %>%
  pivot_wider(names_from = INDIKATOR, values_from = value)


## Hent forbrugsdata via DST API
forbrug_meta <- dst_meta(table="NKHC021", lang="da")

my_query_forbrug <- list(
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

forbrug_data <- dst_get_data(table = "NKHC021", query = my_query_forbrug)

## Fjern unødvendig data fra dataframes
forbrug_data <- forbrug_data[,-1:-3]
forbrug_data <- forbrug_data[-1:-36,]
rownames(forbrug_data) <- NULL
tillid_data <- tillid_data[292:nrow(tillid_data),]


## Omkonverter datoer til kvartaler i tillid_data
tillid_data <- tillid_data %>%
  mutate(Kvartal = as.yearqtr(TID, format = "%Y-%m-%d"))


## Gruppering og opsummering med udregning af mean
tillid_data <- tillid_data %>%
  group_by(Kvartal) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

## Tilpas længde i tillid til forbrug (forbrug bliver kun opdateret kvartalsvis imod månedsvis)
tillid_data <- tillid_data[1:nrow(forbrug_data),]


## Udregner realvæksten sammenlignet med samme kvartal sidste år
for (i in (5:nrow(forbrug_data))){
  Realvækst <- (forbrug_data[i, 2] - forbrug_data[i-4, 2]) / forbrug_data[i-4, 2] * 100
  forbrug_data[i,"Realvækst"] = Realvækst
}

## Angiver en kolonne med retning af realvæksten
forbrug_data$Retning <- as.factor(ifelse(forbrug_data$Realvækst >= 0, "Op", "Ned"))

## Skaleringsfaktor til realvækst i anden y-akse
scale_factor <- 2.0

## ggplot
ggplot()+
  geom_bar(aes(x = tillid_data$Kvartal, y = forbrug_data$Realvækst * scale_factor), 
           stat = "identity", fill = "cornflowerblue")+
  geom_line(aes(x = tillid_data$Kvartal, y = tillid_data$Forbrugertillidsindikatoren), 
            color = "orange", size = 1)+
  scale_y_continuous(sec.axis = sec_axis(~ . / scale_factor, name = "Årlig kvartalsvis realvækst i privatforbrug (%)")) +
  labs(x = "År", 
       y = "Forbrugertillidsindikatoren i nettotal", 
       title = "Forbrugertillidsindikatoren falder i højere grad ift. realvæksten",
       subtitle = "Forbrugertillidsindikatoren er et simpelt gennemsnit af 5 spørgsmål",
       caption = "Kilde: Danmarks Statistik 
       Linjen repræsenterer Forbrugertillidsindikatoren (venstre akse) 
       Søjlerne repræsenterer realvæksten i privatforbrug i % (højre akse)")+
  theme_minimal()

## Opgave 4.2
## Gennemsnit for spørgsmålet "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"
mean(tillid_data$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)

## Opgave 4.3
## Hent forbrug i løbende priser
husforbrug_meta <- dst_meta(table="NKHC021", lang="da")

my_query_husforbrug <- list(
  FORMAAAL = "*",
  PRISENHED = "Løbende priser",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

husforbrug_data <- dst_get_data(table = "NKHC021", query = my_query_husforbrug)
View(husforbrug_data)

## Konverter fra langt til bredt format
husforbrug_data <- husforbrug_data %>%
  pivot_wider(names_from = FORMAAAL, values_from = value)

husforbrug_data <- t(husforbrug_data)

## Klip 2023 ud
husforbrug2023_data <- as.data.frame(husforbrug_data[-1:-4, 133:136])

## Ændr kolonnenavne til kvartaler
colnames(husforbrug2023_data) <- c("Q1", "Q2", "Q3", "Q4")

## Konverter Q1, Q2, Q3 ,Q4 til numeric
husforbrug2023_data <- husforbrug2023_data %>% 
  mutate_all(as.numeric)

## Sum alle kvartalerne
husforbrug2023_data <- husforbrug2023_data %>% 
  mutate(sum_2023 = Q1 + Q2 + Q3 + Q4)

## 2020 - 2023
husforbrug2020_data <- as.data.frame(husforbrug_data[-1:-4,121:124])

## Ændr kolonnenavne
colnames(husforbrug2020_data) <- c("Q1", "Q2", "Q3", "Q4")

## Konverter Q1, Q2, Q3 ,Q4 til numeric
husforbrug2020_data <- husforbrug2020_data %>% 
  mutate_all(as.numeric)

## Sum alle kvartalerne
husforbrug2020_data <- husforbrug2020_data %>% 
  mutate(sum_2020 = Q1 + Q2 + Q3 + Q4)

## Sammensæt sum fra 2020 og 2023
husforbrug_sum <- as.data.frame(husforbrug2020_data[,-1:-4])

husforbrug_sum <- cbind(husforbrug_sum, husforbrug2023_data$sum_2023)
colnames(husforbrug_sum) = c("sum2020", "sum2023")
rownames(husforbrug_sum) = rownames(husforbrug2020_data)

## Procentvis ændring i forbruget
husforbrug_sum <- husforbrug_sum %>% 
  mutate(procentvis_ændring = round((sum2023 - sum2020) / sum2020 *100, 2))

## Opgave 4.4
## DI-FTI
## Vælg de relevante kolonner
DIFTI <- tillid_data %>%
  select(Kvartal, 
         `Familiens økonomiske situation i dag, sammenlignet med for et år siden`,
         `Danmarks økonomiske situation i dag, sammenlignet med for et år siden`,
         `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`,
         `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`) 

## Beregn DI Indikator, som simpelt gennemsnit af 4 spørgsmål
DIFTI <- DIFTI %>% 
  mutate(DIFTI = (DIFTI$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                    DIFTI$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                    DIFTI$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`+
                    DIFTI$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`)/4)


## Hent forbrug i kædede værdier fordelt på 15 grupper
husforbrug_meta <- dst_meta(table="NKHC021", lang="da")

query_forbrugsgrupper <- list(
  FORMAAAL = "*",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

forbrugsgrupper_data <- dst_get_data(table = "NKHC021", query = query_forbrugsgrupper)

## Konverter fra langt til bredt format
forbrugsgrupper_data <- forbrugsgrupper_data %>%
  pivot_wider(names_from = FORMAAAL, values_from = value)

## Klip fra år 1999
forbrugsgrupper_data <- forbrugsgrupper_data[-1:-36, -1:-2]

##################################################################

forbrugsgrupper <- colnames(forbrugsgrupper_data)[3:17]

husforbrug_data <- as.data.frame(t(husforbrug_data))
husforbrug_vækst <- husforbrug_data[-1:-36,-c(1, 2, 4)]
rownames(husforbrug_vækst) <- NULL
View(husforbrug_vækst)


## Konverter alle undtagen TID til Numeric
husforbrug_vækst <- husforbrug_vækst %>% 
  mutate_at(vars(-TID), as.numeric)

## Opret dataframe til der hvor væksttallene skal ind
vækst_data <- data.frame(TID = husforbrug_vækst$TID[5:nrow(husforbrug_vækst)]) # Start med TID fra række 5

## LOOOOOOP vækst i forbruget
for (col in 2:ncol(husforbrug_vækst)) {
  # Beregn væksten og tilføj NA for de første 4 rækker
  vækst <- rep(NA, 4) # tilføj 4 NA-værdier i starten
  vækst <- c(vækst, (husforbrug_vækst[[col]][5:nrow(husforbrug_vækst)] - 
                       husforbrug_vækst[[col]][1:(nrow(husforbrug_vækst) - 4)]) / 
               husforbrug_vækst[[col]][1:(nrow(husforbrug_vækst) - 4)] * 100)
  
  vækst <- vækst[-c(1:4)]
  
  vækst_data[[colnames(husforbrug_vækst)[col]]] <- vækst
}

forbrugsgrupper <- intersect(forbrugsgrupper, colnames(vækst_data))

# Fjern "TID", hvis det er i kolonnenavnene
vækst_data <- vækst_data[, !(colnames(vækst_data) %in% "TID")]

tillid_data <- tillid_data[-c(1:4), ]


## Regressions loop vha lapply() - DST
resultater_FTI <- lapply(forbrugsgrupper, function(gruppe) {
  lm(vækst_data[[gruppe]] ~ tillid_data$Forbrugertillidsindikatoren)
})

## Navngiv resultaterne - DST
names(resultater_FTI) <- forbrugsgrupper

DIFTI <- DIFTI[-c(1:4), ]

## Regressions loop vha lapply() - DI
resultater_DI <- lapply(forbrugsgrupper, function(gruppe) {
  lm(vækst_data[[gruppe]] ~ DIFTI$DIFTI)
})

## Navngiv resultaterne - DI
names(resultater_DI) <- forbrugsgrupper

# Saml resultater i en liste med navne
summary_resultater <- list(
  DST = resultater_FTI,
  DI = resultater_DI
)

# Få adgang til specifikke summaries
print(summary_resultater$DST[[1]]) # Første DST-model
print(summary_resultater$DI[[1]]) # Første DI-model

# Korrelation mellem forbrugsgrupper og Forbrugertillidsindikatoren (FTI)
korrelationer_FTI <- sapply(forbrugsgrupper, function(gruppe) {
  cor(vækst_data[[gruppe]], tillid_data$Forbrugertillidsindikatoren, use = "complete.obs")
})

# Korrelation mellem forbrugsgrupper og DI-Forbrugertillidsindikatoren (DI)
korrelationer_DI <- sapply(forbrugsgrupper, function(gruppe) {
  cor(vækst_data[[gruppe]], DIFTI$DIFTI, use = "complete.obs")
})

# Vis korrelationer for FTI
print(korrelationer_FTI)

# Vis korrelationer for DI
print(korrelationer_DI)

