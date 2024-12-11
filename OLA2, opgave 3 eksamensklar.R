library(dkstat)
library(ggplot2)
library(dkstat)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(pROC)

#OPG3.1
#Dan en dummy variable af den kvartalsvise årlige vækst i husholdningernes forbrugsudgift for
#perioden 1. kvartal 1998 til 2. kvartal 2021. Hvor ofte stiger den kvartalsvise årlige vækst i
#husholdningernes forbrugsudgift? Hvor ofte falder den kvartalsvise årlige vækst i husholdningernes
#forbrugsudgift?

#laver dummy variable ud fra realvækstens fortegn. 
#Postitiv/stigning = 1, altså hvis x > 0 = 1
#Negativ/faldende = 0. altså hvis x < 0 = 0

#laver dummy på privatforbrug.periode
Privatforbrug.periode$dummy <- ifelse(Privatforbrug.periode$Realvækst >= 0, print(1), print(0))
table(Privatforbrug.periode$dummy)
#0  22
#1  77  

#Den kvartalsvise årlige vækst i husholdningernes forbrugsudgifter stiger 77 gange (kvartaler)
#og falder 22 gange (kvartaler) i perioden 1. kvartal 2000 til 3. kvartal 2024. 

#Opgave 3.2 – Logistisk regression og forudsigelser
#Lav en logistik regression med dummy variable fra opgave 1.1 og de fire indikatorer i DI’s
#forbrugertillidsindikator. Hvilken retning forudsiger jeres model, at den årlige vækst i
#husholdningernes forbrugsudgift, vil gå i 3. kvartal 2024? (Hint: svaret er enten op eller ned)

## Multipel logistisk regression, Retning som y (forudsig), FTI som x(uafhængig variabel)
retningglm <- glm(formula = Privatforbrug.periode$dummy ~ 
                    FORV1.kvartal$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`[92:190]+
                    FORV1.kvartal$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[92:190]+
                    FORV1.kvartal$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[92:190]+
                    FORV1.kvartal$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[92:190],  
                  family = "binomial")

summary(retningglm)

retningglm$coefficients
# 3 ud af 4 er insignifkante - disse fjernes og vi har derfor kun 1. vi skal have alle med og tager derfor bare DI for Q2

retningglm2 <- glm(formula = Privatforbrug.periode$dummy ~ 
                     FORV1.kvartal$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[92:190],
                   family = "binomial")

summary(retningglm2)


#tages af DI i stedet
retningglm3 <- glm(formula = Privatforbrug.periode$dummy ~
                     DI$DI.FTI,
                   family = "binomial")

summary(retningglm3)

#indikatoren som helhed er siknifikant og vi bruger derfor denne.
# i dette tilfælde bruger vi DI selvom p værdien er lavere, da alle spg skal inkluderes. 

###predict funktion 

# Lav en samlet data frame
combined_data <- data.frame(
  dummy = Privatforbrug.periode$dummy,
  DI_FTI = DI$DI.FTI
)

# Opret modellen med korrekt navngivning
retningglm3 <- glm(dummy ~ DI_FTI, data = combined_data, family = "binomial")
new_data <- data.frame(DI_FTI = -10.275)
predicted_probability <- predict(retningglm3, newdata = new_data, type = "response")
print(predicted_probability)




#Opgave 3.3 – Simpel validering af model
#Hvor ofte forudsiger jeres model i opgave 1.2, at den kvartalsvise årlige realvækst i husholdningernes
#forbrugsugift stiger? Hvor ofte er det så reelt tilfældet, at den kvartalsvise årlige realvækst i
#husholdningernes forbrugsudgift stiger, set i forhold til, hvad jeres model forudsiger? 

#Predict funktion
Forudsigelse <- predict.glm(retningglm3, newdata = NULL, type = c("response"))
Privatforbrug.periode$Forudsigelse <- Forudsigelse

Privatforbrug.periode$Dummy.forudsigelse <- ifelse(Privatforbrug.periode$Forudsigelse >= 0.5, print(1), print(0))
table(Privatforbrug.periode$Dummy.forudsigelse)


Privatforbrug.periode$Dummy.forudsigelse2 <- ifelse(Privatforbrug.periode$Forudsigelse >= 0.7, print(1), print(0))
table(Privatforbrug.periode$Dummy.forudsigelse2)

#rigtigt
predicted_probs <- predict(retningglm3, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.7, 1, 0)
table(Predicted = predicted_classes, Actual = Privatforbrug.periode$dummy)


#Høj sandsynlighed (tæt på 1): Der er en høj sandsynlighed for, at forbruget stiger.
#Lav sandsynlighed (tæt på 0): Der er en lav sandsynlighed for, at forbruget stiger (dvs. forbruget falder).
#vi vurderer at en sandsynlighed på over 0.7 betyder stigning og under betyder fald i forbruget

##roc kurve

# Opret en ROC-kurve
roc_curve <- roc(Privatforbrug.periode$dummy, predicted_probs)

# Plot ROC-kurven
plot(roc_curve, main = "ROC-kurve for logistisk regression", col = "blue", lwd = 2)

# Tilføj AUC (Area Under Curve) til grafen
auc_value <- auc(roc_curve)
text(0.5, 0.5, labels = paste("AUC =", round(auc_value, 3)), cex = 1.2)



