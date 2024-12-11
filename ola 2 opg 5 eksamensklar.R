library(eurostat)
library(tidyverse)
library(dplyr)
library(restatapi)

#opg 5.1 - se årlige realvækst i realvækst total. 

# total content
alltabs <- get_eurostat_toc()

# search
search_eurostat("household consumption expenditure", type = "dataset")
search_eurostat("household")

#hent metadata
namq_10_gdp <- get_eurostat_dsd("namq_10_gdp")
unique(namq_10_gdp$concept)

#> unique(namq_10_gdp$concept)
#[1] "freq"    "unit"    "s_adj"   "na_item" "geo"  

#lav filter
LV = namq_10_gdp$concept == "geo"

namq_10_gdp %>% filter(concept=='geo') %>% select('code') %>% unique()
#("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES"),

namq_10_gdp %>% filter(concept=="s_adj") %>% select("code") %>% unique()
# SCA

namq_10_gdp %>% filter(concept=="na_item") %>% select("code") %>% unique()
# "P31_S14"

#sammensat
namq.filter <- get_eurostat(
  id = "namq_10_gdp",
  filters = list(
    geo = c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES"), 
    na_item = "P31_S14",                                            
    s_adj = "SCA"                                           
  ),
  time_format = "date"
)


#transformer/omdan så det er en kolonne med hvert land 
# Filtrér kun de nødvendige kolonner
filtered_data2 <- namq.filter %>%
  select(time, geo, values)


wide_data <- filtered_data2 %>%
  pivot_wider(
    names_from = geo,             # Column to create new columns from
    values_from = values,         # Column to fill new columns with
    values_fn = list(values = ~ mean(.x, na.rm = TRUE))  # Calculate mean, ignoring NAs
  )

# Se de første par rækker af det transformerede datasæt
head(wide_data)

#vælger rette dataframe. 2000Q1 - 2024Q3
wide.data.df <- wide_data[c(97:199),]

#beregner realvækst i loop af alle kolonner
library(dplyr)
library(tidyr)
library(lubridate)

# Beregn realvækst for hver kolonne (land)
for (i in 2:ncol(wide.data.df)) { # Start fra 2 for at springe TIME_PERIOD over
  col_name <- names(wide.data.df)[i]
  
  # Beregn realvækst for den nuværende kolonne, undgå NA-værdier
  wide.data.df[[paste0(col_name, "_Realvækst")]] <- 
    ifelse(
      !is.na(wide.data.df[[col_name]]) & !is.na(dplyr::lag(wide.data.df[[col_name]], 4)), 
      (wide.data.df[[col_name]] / dplyr::lag(wide.data.df[[col_name]], 4) - 1) * 100, 
      NA
    )
}

# Se de første rækker af datasættet med realvækst
head(wide.data.df)

#saml i ny dataframe via subset:

realvækst.total <- wide.data.df[c(5:103),c(11:19)]

colnames(realvækst.total) <- substr(colnames(realvækst.total), 1, 2) #de to første bogstaver i kolonnenavnene beholdes

realvækst.total <- cbind(wide.data.df[c(5:103), c(1)], realvækst.total) #tilføjer time

# Omdan realvækst.total til langt format
long_data <- realvækst.total %>%
  pivot_longer(
    cols = -time,               # Alle kolonner undtagen tid (time)
    names_to = "Land",          # Ny kolonne med landekoder
    values_to = "Realvækst"     # Ny kolonne med realvækstværdier
  )

#####

# Lookup-tabel for landekoder og fulde navne
land_full_names <- c(
  DK = "Danmark",
  BE = "Belgien",
  NL = "Holland",
  SE = "Sverige",
  AT = "Østrig",
  DE = "Tyskland",
  FR = "Frankrig",
  IT = "Italien",
  ES = "Spanien"
)

# Ændr koderne til fulde navne i grafen
ggplot(long_data, aes(x = time, y = Realvækst, color = Land, group = Land)) +
  geom_line(size = 1) + 
  labs(
    title = "De europæiske landes realvækst følges i høj grad ad",
    x = "År",
    y = "Realvækst (%)",
    caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)"
  ) +
  
  # Brug fulde navne til farveskalaen
  scale_color_manual(
    values = brewer.pal(length(land_full_names), "Paired"),
    labels = land_full_names
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "grey80")
  ) +
  
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")


#opg 5.2  - gns af realvækst
# Antag at realvækst.total indeholder dine realvækst værdier

# Opret en liste til at gemme gennemsnitsværdierne
gennemsnit_vækst <- numeric(ncol(realvækst.total) - 1)  # -1 for at udelade TIME_PERIOD
land_navne <- colnames(realvækst.total)[-1]  # Udelad TIME_PERIOD

# Beregn gennemsnittet for hver kolonne (land)
for (i in 2:ncol(realvækst.total)) { 
  gennemsnit_vækst[i - 1] <- mean(realvækst.total[[i]], na.rm = TRUE) #gns - første kolonne som er tid
}

# Opret en data frame for de samlede gennemsnit
gennemsnit.total.df <- data.frame(land = land_navne, gennemsnit_vækst = gennemsnit_vækst)

#højeste vækst er Sverige: 
#SE_Realvækst - 2.49


#plotter
ggplot(gennemsnit.total.df, aes(x=fct_reorder(land, gennemsnit_vækst), y=gennemsnit_vækst, fill=land)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(gennemsnit_vækst, 2)), 
            vjust = -0.5, 
            size = 3.5) +
  labs(title = "Sverige har den højeste gsn. realvækst i husholdningernes forbrugsudgift",
       y = "Gns kvartalsvise realvækst(%) fra 2000Q1 til 2024Q3",
       x = NULL,
       caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "black"),  # Centreret og større titel
    axis.title.x = element_text(angle = 90, hjust = 1),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 10)
  )



#opg 5.3 - fjern corona og find højeste realvækst

#identificer periode

#fjern manualt
vækst.minus.corona <- realvækst.total[-c(81:90),]

#beregn gennemsnit
# Beregn gennemsnittet for alle kolonner (undtagen den første kolonne TIME_PERIOD)
gns.minus.corona <- colMeans(vækst.minus.corona[,-1], na.rm = TRUE)

# Opret en data frame for at vise resultaterne
gennemsnit.minus.corona.df <- data.frame(land = names(gns.minus.corona), gns.uden.corona = gns.minus.corona)

# Udskriv gennemsnittene
print(gennemsnit.minus.corona.df)

#plotter - kan slettes
ggplot(gennemsnit.minus.corona.df, aes(x=land, y=gennemsnit_vækst, fill=land))+
  geom_bar(stat = "identity")+
  labs(title = "gns ",
       X = "land",
       y="gns vækst(%)")+
  theme_minimal()+
  theme(axis.title.x = element_text(angle=90,hjust=1))


#sammenlign med forrige dataframe MED coronakrise 
gns.sammenligning <- cbind(gennemsnit.total.df,gennemsnit.minus.corona.df[,-1])
colnames(gns.sammenligning) <- c("Land", "Gns. vækst", "Gns. vækst uden coronaperiode (2020Q1-2022Q2)")

data_long <- reshape2::melt(gns.sammenligning, id.vars = "Land") # omformer

# sammenligning af realvækst før og efter corona
ggplot(data_long, aes(x=fct_reorder(Land, value), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = round(value, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  labs(title = "Coronaperioden påvirker landenes gns. vækst i forbruget", x = NULL, y = "gns. vækst i % med og uden corona krisen -
2000Q1 til 2024Q3",
       caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)")+
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold", color = "black"))+
  scale_fill_brewer(palette = "Paired")

#difference plottes



#udregn difference - 
gns.sammenligning$difference <- gns.sammenligning$`Gns. vækst` - gns.sammenligning$`Gns. vækst uden coronaperiode`

#plotter forskellen
ggplot(gns.sammenligning, aes(x = fct_reorder(Land, difference), y = difference, fill = difference >= 0)) +
  geom_bar(stat = "identity", position = "dodge") +
  
  # Tilføj labels med + og - afhængig af forskellen, tekstfarve er nu sort
  geom_text(aes(label = ifelse(difference >= 0, 
                               paste0("+", round(difference, 2)), 
                               paste0("-", abs(round(difference, 2)))),
  ),  # Fjern farvemapping for at få sort tekst
  vjust = -0.5, size = 4, color = "black") +  # Tekstfarve er sort
  
  # Definer farver: rød for negative, grøn for positive
  scale_fill_manual(values = c("pink", "lightgreen")) +
  
  # Tilpasning af layout
  labs(title = "Coronakrisen haft en største effekt på Spaniens kvartalsvise realvækst", 
       x = NULL, 
       y = "Forskel i gns. realvækst med og uden corona krisen som en outlier",
       caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)") +
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold", color = "black")) # Fjern legend for farver


#Opgave 5.4 – Effekt af Corona på forbruget
#I hvilket europæiske land faldt den gennemsnitligt kvartalsvise realvækst i husholdningernes
#forbrugsudgift, i perioden 1. kvartal 2020 til 2. kvartal 2024, mest?

#markerer periode

periode <- realvækst.total[c(81:99),]

#beregn gns
gns.periode <- colMeans(periode[,-1], na.rm = TRUE)

# Opret en data frame for at vise resultaterne
gns.periode.df <- data.frame(land = names(gns.periode), gns = gns.periode)



#plotter
ggplot(gns.periode.df, aes(x = fct_reorder(land, gns), y = gns, fill = land)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = round(gns, 2)), 
            vjust = -0.5, 
            size = 3.5) +
  labs(title = " Markante forskelle i økonomisk genopretning blandt europæiske lande efter coronakrisen",
       y="Gns kvartalsvise realvækst(%) fra 1. kvartal 2020 til 2. kvartal 2022",
       x=NULL,
       caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)")+
  theme_minimal()+
  scale_fill_brewer(palette = "Paired") +
  theme(axis.title.x = element_text(angle=90,hjust=1),
        plot.title = element_text(size = 13, face = "bold", color = "black"))


#grafer over udvikling
library(ggplot2)
library(tidyverse)

periode.long<- periode %>%
  pivot_longer(-time, names_to = "Country", values_to = "Value")

# Find den absolut laveste værdi i datasættet
overall_min_value <- min(periode.long$Value)
overall_min_time <- periode.long$time[which.min(periode.long$Value)]
overall_min_country <- periode.long$Country[which.min(periode.long$Value)]

library(ggplot2)
library(dplyr)
library(scales) 

# Plot grafen
ggplot(periode.long, aes(x = time, y = Value, color = Country)) +
  geom_line() + 
  geom_point() + 
  geom_text(aes(x = overall_min_time, 
                y = overall_min_value, 
                label = paste(overall_min_country, round(overall_min_value, 2))),
            vjust = 1.5, color = "red") + # Kun den absolut laveste værdi
  labs(title = "Spanien: Største fald og stigning i husholdningernes forbrugsudgifter",
       x = "Tid",
       y = " Gennemsnitlig kvartalsvis realvækst i forbrugsudgifter(%)",
       caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)") +
  scale_color_brewer(palette = "Paired")+
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold", color = "black")) 


periode.long$time <- as.Date(periode.long$time)






