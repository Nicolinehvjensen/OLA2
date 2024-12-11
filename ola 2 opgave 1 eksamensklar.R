library(dkstat) 
library(tidyverse)

## DST BY3 metadata
indbygger_meta <- dst_meta(table = "BY3") 


## Query filter
indbygger_filter <- list(
  BYER = "*",
  FOLKARTAET = "Folketal",
  Tid = "2024"
)

## Hent data
indbygger_data <- (dst_get_data(table = "BY3", query = indbygger_filter))

## Data rensning
## Fjern alle rækker hvor indbyggertallet er 0 
indbygger_data <- indbygger_data[indbygger_data$value != 0,]

## G-sub til at rydde op i bynavne
## Først erstattes tal og bindestreger, dernæste parenteser og alt i mellem dem
indbygger_data$BYER <- gsub("[0-9]+", "", indbygger_data$BYER)
indbygger_data$BYER <- gsub("- ", "", indbygger_data$BYER)
indbygger_data$BYER <- gsub("\\(.*?\\)", "", indbygger_data$BYER)

## Trim evt white-space
indbygger_data$BYER <- trimws(indbygger_data$BYER)

## Gruppér BYER
indbygger_data <- indbygger_data %>%
  group_by(BYER) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

## Ny kolonne med bystørrelse
indbygger_data <- indbygger_data %>%
  mutate(størrelse = case_when(
    value <= 250 ~ "Landet",
    value > 250 & value <= 1000 ~ "Landsby",
    value > 1000 & value <= 2500 ~ "Lille by",
    value > 2500 & value <= 10000 ~ "Almindelig by",
    value > 10000 & value <= 50000 ~ "Større by",
    value > 50000 ~ "Storby"
  ))
##########################
####### Opgave 1.3 #######
##########################

## Indlæs filen
boligsiden <- read_csv("boligsiden copy.csv")

## Fjern alle rækker med NA værdier
boligsiden <- na.omit(boligsiden)

indbygger_data$BYER <- tolower(indbygger_data$BYER)

## Match bynavne format i indbygger data til boligsiden data
indbygger_data$BYER <- gsub("ø", "oe", indbygger_data$BYER)
indbygger_data$BYER <- gsub("æ", "ae", indbygger_data$BYER)
indbygger_data$BYER <- gsub("å", "aa", indbygger_data$BYER)


## Merge de to datasæt baseret på byer, med relevante kolonner
bolig_select <- boligsiden %>% select(by, pris, kvmpris)
indbygger_select <- indbygger_data %>%  select(BYER, størrelse)

samlet_data <- merge(bolig_select, indbygger_select, by.x = "by", by.y = "BYER", all.x = TRUE)

## Mange byer er ikke i DST over byer, men er på Boligsiden
## Jeg fjerner derfor alle med NA'er
samlet_data <- na.omit(samlet_data)

## Beregn gns kvmpris for hver by
## Erstat punktum med komma i kvmpriser
samlet_data$kvmpris <- gsub("\\.", "", samlet_data$kvmpris)

## Kvmpris til numeric
samlet_data$kvmpris <- as.numeric(samlet_data$kvmpris)

## Opret dataframe med gennemsnitlige kvmpris baseret på størrelse
avg_kvmpris <- data.frame(tapply(samlet_data$kvmpris, samlet_data$størrelse, mean, na.rm = TRUE))
colnames(avg_kvmpris) <- "Gns.kvmpris"
avg_kvmpris <- rownames_to_column(avg_kvmpris, var = "Størrelse")

## Alle 11 rækker med størrelse "Landet" frasorteres, da det forskruer resultatet
avg_kvmpris2 <- avg_kvmpris[-2,]

##########################
####### Opgave 1.4 #######
##########################

## Ggplot
ggplot(data = avg_kvmpris2) +
  geom_bar(aes(x = fct_reorder(Størrelse, Gns.kvmpris, .desc=F ), 
               y = Gns.kvmpris, fill = Størrelse), stat = "identity")+
  labs(x = "",
       title = "Kvadratmeterprisen stiger med bystørrelsen",
       caption = "Datakilde: Danmarks Statistik og Boligsiden")+
  theme_minimal()+
  theme(legend.position = "none")
