library(dkstat)
library(ggplot2)
library(dkstat)
library(dplyr)
library(tidyr)
library(lubridate)

###Dataindhentning 


## Realvækst
Forbrug.meta <- dst_meta(table="NKHC021", lang="da")

Forbrug.filter <- list(
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*")

Forbrugdata <- dst_get_data(table = "NKHC021", query = Forbrug.filter)
Forbrugdata <- Forbrugdata[,-c(1:3)]

#udregner realvæskten
Forbrugdata$Realvækst <- (Forbrugdata$value / dplyr::lag(Forbrugdata$value, 4) - 1) * 100

Privatforbrug.periode <- as.data.frame(Forbrugdata[c(41:139),c(1,3)]) #tager for den udvalgte periode

#Ændrer fra yyyy-mm-xx til yyyy-Q1
Privatforbrug.periode$TID <- as.Date(Privatforbrug.periode$TID)  # Sørg for, at datoen er i Date format

#Udtræk år og kvartal
Privatforbrug.periode$TID <- paste0(year(Privatforbrug.periode$TID), "-Q", quarter(Privatforbrug.periode$TID))

###Forbrugertillid
#Finder data fra forbrugerforventninger i alle perioder
alltables <- dst_get_tables(lang="da")
dst_search("Forbrugerforventninger")

FORV1.meta <- dst_meta(table = "FORV1", lang = "da")

#liste med relevante filter-variabler.
FORV1.filter <- list(
  INDIKATOR = "*",
  Tid = "*")

FORV1Data <- dst_get_data(table = "FORV1", query = FORV1.filter, lang = "da")
#INDIKATOR                       TID        value
#1 Forbrugertillidsindikatoren 1974-10-01   -15
#2 Forbrugertillidsindikatoren 1974-11-01    NA

#omdanner så det ligner min tidligere csv fil og til dataframe

FORV1.wider <- pivot_wider(FORV1Data, names_from = INDIKATOR, values_from = value)
FORV1 <- as.data.frame(FORV1.wider)

#omdanner hele dataframe med FORV1 til kvartaler med gns
#Sikrer at alle kolonner er numeriske undtagen dato som er sidste kolonne
FORV1$TID <- as.Date(FORV1$TID)

FORV1[, c(2:14)] <- lapply(FORV1[, c(2:14)], function(x) as.numeric(as.character(x))) #markerer col1:13 som numerisk

gns.resultater <-list() # opretter liste til at gemme resultater i

# Beregn kvartalet en gang og gem det
FORV1$TID <- paste(year(FORV1$TID), "Q", quarter(FORV1$TID), sep = "")

# Loop gennem hver af de 13 kolonner
for (i in 2:14) {
  # Beregn gennemsnit af FTI pr. kvartal for den i-te kolonne
  FTI.gns <- aggregate(FORV1[, i] ~ TID, data = FORV1, FUN = function(x) round(mean(x, na.rm = TRUE), 2))
  
  # Gem resultaterne i en liste med navn baseret på kolonne-nummeret
  gns.resultater[[paste0("FTI.gns", i)]] <- FTI.gns[, 2]  #Gem kun gennemsnitsværdierne (kolonne 2)
}

# Saml alle resultater i en ny dataframe
FORV1.kvartal <- data.frame(TID2 = FTI.gns$TID, do.call(cbind, gns.resultater))

#Tilføjer kolonne navne fra gamle data.frame
Overskrifter <- c("Kvartal", colnames(FORV1[2:14]))
colnames(FORV1.kvartal) <-Overskrifter


#FTI MOD REALVÆKST

FTI <- cbind.data.frame(Privatforbrug.periode,FORV1.kvartal[c(92:190),2])
colnames(FTI)[3] <- "Forbrugertillidsindikator"

FTI.cor <- cor(FTI$Realvækst,FTI$Forbrugertillidsindikator)
#0.507 korrelation

FTI.reg <- lm(FTI$Realvækst~FTI$Forbrugertillidsindikator)
summary(FTI.reg)$r.squared 
### 0.2576342 R2


#DI MOD REALVÆKST

#Henter data fra år 2000 til og med 2024 Q3
forbrugertillid_clean <- FORV1.wider[304:602, 1:14]

#Opretter dataframe til DI-FTI 

# Vælg de relevante kolonner, som ser ud til at være kolonne 3, 5, 7 og 13
forbrugertillid_selected <- forbrugertillid_clean %>%
  select(TID, 
         `Familiens økonomiske situation i dag, sammenlignet med for et år siden`,
         `Danmarks økonomiske situation i dag, sammenlignet med for et år siden`,
         `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`,
         `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`
  )  

#Laver kvartalsvis dataframe for DI-FTI

#Konverter TID-kolonnen til en dato
forbrugertillid_selected <- forbrugertillid_selected %>%
  mutate(TID = ymd(TID))  # Sørg for at TID er i datoformat

#Opret kvartalsformat i "YYYY QX" format
forbrugertillid_quarterly <- forbrugertillid_selected %>%
  mutate(Kvartal = paste0(year(TID), " Q", quarter(TID))) %>%  # Opret "YYYY QX"-format
  group_by(Kvartal) %>%  # Gruppér efter kvartal
  summarise(across(-TID, mean, na.rm = TRUE))  # Beregn gennemsnit for hver kolonne, ekskluder TID

# Vis det kvartalsvise gennemsnit
head(forbrugertillid_quarterly)

#Beregner samlet gennemsnit af de relevante indikatorer for hvert kvartal for at lave DI-FTI

forbrugertillid_quarterly <- forbrugertillid_quarterly %>%
  rowwise() %>%  # Sørger for, at operationer sker på tværs af hver række
  mutate(DI.FTI = mean(c_across(where(is.numeric)), na.rm = TRUE))  # Beregn gennemsnit af de numeriske kolonner


#DI og realvækst
forbrugertillid_quarterlyq3 <- forbrugertillid_quarterly[-100, ]

DI <- cbind.data.frame(Privatforbrug.periode,forbrugertillid_quarterlyq3[,6])

DI.cor <- cor(DI$Realvækst,DI$DI.FTI)
#0,5640019

DI.reg <- lm(DI$Realvækst~DI$DI.FTI)
summary(DI.reg)$r.squared 
#0.3180982


#Baums periode til sammenligning 

#FTI BAUM periode i 2020-priser
FTI.B <- cbind.data.frame(Privatforbrug.periode[1:66,c(1:2)],FORV1.kvartal[c(92:157),2])
colnames(FTI.B)[3] <- "Forbrugertillidsindikator"

FTI.cor2 <- cor(FTI.B$Realvækst,FTI.B$Forbrugertillidsindikator)
print(FTI.cor2)
#[1] 0.587442

FTI.reg2 <- lm(FTI.B$Realvækst~FTI.B$Forbrugertillidsindikator)
summary(FTI.reg2)$r.squared 
#[1] 0.3538794


#DI - BAUM periode i 2020-priser
DI.B <- cbind.data.frame(Privatforbrug.periode[1:66,c(1:2)],forbrugertillid_quarterly[c(1:66),6])
DI.cor2 <- cor(DI.B$Realvækst,DI.B$DI.FTI)
print(DI.cor2)
#[1] 0.6849816

DI.reg2 <- lm(DI.B$Realvækst~DI.B$DI.FTI)
summary(DI.reg2)$r.squared 
#[1] 0.4733126


#Opgave 2.2 – Forudsigelser af forbruget
#Beregn/forudsig den årlige realvækst i husholdningernes forbrugsudgift for 3. kvartal 2024 med
#henholdsvis DI’s forbrugertillidsindikator og forbrugertillidsindikatoren fra DST.


# Nye regressionsmodeller med realvækst som afhængig variabel
FTI.reg.rev <- lm(Realvækst ~ Forbrugertillidsindikator, data = FTI)
DI.reg.rev <- lm(Realvækst ~ DI.FTI, data = DI)

# Kendte værdier for FTI og DI-FTI for 2024 Q3
FTI_2024_Q4 <- data.frame(Forbrugertillidsindikator = -9.1)
DI_FTI_2024_Q4 <- data.frame(DI.FTI = -10.2750000)

#Dobbelttjekker med predict
# Forudsig realvækst for 2024 Q3
FTI_realvækst_2024_Q4 <- predict(FTI.reg.rev, newdata = FTI_2024_Q4)
DI_realvækst_2024_Q4 <- predict(DI.reg.rev, newdata = DI_FTI_2024_Q4)

# Vis resultaterne
print(paste("Forudsigelse af realvækst baseret på FTI:", FTI_realvækst_2024_Q4))
print(paste("Forudsigelse af realvækst baseret på DI-FTI:", DI_realvækst_2024_Q4))

##graf
library(ggplot2)

# Kombiner data til graf
graf_data <- data.frame(
  Kvartal = Privatforbrug.periode$TID,
  Realvækst = as.numeric(Privatforbrug.periode$Realvækst),
  FTI = as.numeric(FTI$Forbrugertillidsindikator),
  DI = as.numeric(DI$DI.FTI)
)

# Konverter 'Kvartal' til faktor for at sikre korrekt rækkefølge på x-aksen
graf_data$Kvartal <- factor(graf_data$Kvartal, levels = unique(graf_data$Kvartal))

# Opret graf
ggplot(graf_data, aes(x = Kvartal)) +
  # Søjlediagram for realvækst
  geom_bar(aes(y = Realvækst * 5, fill = "Årlig Realvækst (Pct.)"),  # Gang realvækst med 5 for skalering
           stat = "identity", color = "black", alpha = 0.6) +
  # Linje for DI
  geom_line(aes(y = DI, group = 1, color = "DI's Forbrugertillidsindikator"), linewidth = 1) +
  # Linje for FTI
  geom_line(aes(y = FTI, group = 1, color = "FTI's Forbrugertillidsindikator"), linewidth = 1) +
  # Primær og sekundær akse
  scale_y_continuous(
    name = "DI's og FTI's Forbrugertillidsindikatorer (Nettotal)",  # Venstre y-akse
    breaks = seq(-50, 50, by = 10),  # Venstre y-akse breaks
    limits = c(-50, 50),  # Begrænsning for venstre y-akse
    sec.axis = sec_axis(~ . / 5,  # Divider med 5 for at genskabe original skala for højre y-akse
                        name = "Årlig Realvækst (Pct.)", 
                        breaks = seq(-10, 10, by = 2))  # Breaks for højre y-akse
  ) +
  # Juster x-aksen til kun at vise år
  scale_x_discrete(
    breaks = graf_data$Kvartal[seq(1, length(graf_data$Kvartal), by = 4)]  # Vis kun hvert 4. kvartal (år)
  ) +
  # Tilføj labels og titler
  labs(
    title = "DI's Forbrugertillidsindikator følger den årlige realvækst",
    subtitle = "",
    caption = "Kilde: Danmarks Statistik og DI analyse",
    x = "Tid (År)",
    color = "",
    fill = ""
  ) +
  # Tilpasning af tema og tekst
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue", size = 12),
    axis.title.y.right = element_text(color = "black", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "top"  # Placér forklaringen i toppen
  ) +
  scale_color_manual(values = c("blue", "red")) +  # Farver til linjer
  scale_fill_manual(values = c("gray70"))  # Farve til søjler
