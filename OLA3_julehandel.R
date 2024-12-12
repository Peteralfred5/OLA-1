#####################
####Indlæs Pakker####
#####################

library("dkstat")
library("ggplot2")
library("tidyr")
library("pROC")
library("pls")
library("httr")
library("jsonlite")

#####################################################
###Opgave 1 – Den bedste forbrugertillidsindikator###
#####################################################

############################################
###Opgave 1.1 – Kombinationsalgoritme i R###
############################################

##############################
####Forbrugerforventninger####
##############################

dst_search(string = "Forbrugerforventning", field = "text")

Tabeller <- (dst_get_tables(lang = "da"))

FORV <- dst_meta(table = "FORV1",lang = "da")

FORV_filter <- list(INDIKATOR ="*",
                    Tid = "*")

FORV1 <- dst_get_data(table = "FORV1", query = FORV_filter, lang = "da")

Forventning <- reshape(FORV1, idvar = "TID", timevar = "INDIKATOR", direction = "wide")

colnames(Forventning) <- gsub("value.", "", colnames(Forventning))

n_kategorier <- ncol(Forventning)
n_rows <- nrow(Forventning)
n_groups <- ceiling(n_rows / 3)
Forventning_matrix <- matrix(nrow = n_groups, ncol = n_kategorier)
colnames(Forventning_matrix) <- colnames(Forventning)

for (var in seq_len(n_kategorier)) {
  for (i in seq(1, n_rows, by = 3)) {
    end <- min(i + 2, n_rows)  # Sørg for ikke at gå udenfor rækkerne
    Forventning_matrix[ceiling(i / 3), var] <- round(mean(Forventning[i:end, var], na.rm = TRUE), 2)
  }
}

Forventning <- as.data.frame(Forventning_matrix)

Kvartal_start <- as.Date("1974-10-01")
Kvartal_slut <- as.Date("2024-11-01")

Kvartal_dato <- seq.Date(from = Kvartal_start, to = Kvartal_slut, by = "quarter")

Formater_kvartaler <- function(date) {
  year <- format(date, "%Y")
  month <- as.integer(format(date, "%m")) 
  quarter <- (month - 1) %/% 3 + 1
  return(paste0(year, "Q", quarter))
}

Kvartaler <- sapply(Kvartal_dato, Formater_kvartaler)

Forventning$Kvartal <- Kvartaler

Forventning$Algo <- round(rowMeans(Forventning[c("Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
                                                 "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.",
                                                 "Regner med at kunne spare op i de kommende 12 måneder",
                                                 "Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener")]),2)

Forventning$DI <- round(rowMeans(Forventning[c("Familiens økonomiske situation i dag, sammenlignet med for et år siden",
                                               "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
                                               "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
                                               "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.")]),2)

Forventning$Mikro <- round(rowMeans(Forventning[c("Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.")]),2)

######################
####Nye Nulpunkter####
######################

## Der laves nye nulpunkter for spørgsmålene, da høje positive værdier giver et misvisende billede af intercept ved udarbejdelse af lineær regression
## Det gælder for spg. Regner med at kunne spare op i de kommende 12 måneder og Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener
## Det ses i den statistiske dokumentation samt den historiske udvikling, at der skal meget til, før vi kommer under en værdi på nul
## Tilgangen som anvendes er gennemsnittet som nulpunkt (mean-centering), da det giver symmetri og bevarer variablen i den oprindelige skala.

FamØko_mean <- mean(Forventning$`Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`, na.rm = TRUE)

Forventning$`Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener_nulpunkt` <- 
  round((Forventning$`Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener` - FamØko_mean),2)

sparer_mean <- mean(Forventning$`Regner med at kunne spare op i de kommende 12 måneder`, na.rm = TRUE)

Forventning$`Regner med at kunne spare op i de kommende 12 måneder_nulpunkt` <- 
  round((Forventning$`Regner med at kunne spare op i de kommende 12 måneder` - sparer_mean),2)

Forventning$`Algo_nulpunkt` <- round(rowMeans(Forventning[c("Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
                                                            "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.",
                                                            "Regner med at kunne spare op i de kommende 12 måneder_nulpunkt",
                                                            "Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener_nulpunkt")]),2)

Forbrugertillid <- Forventning[-c(1:101,200),-1]

rownames(Forbrugertillid) <- NULL

#####################
####Privatforbrug####
#####################

FORB <- dst_meta(table = "NKHC1",lang = "da")

FORB_filter <- list(FORBRUGSART = "Privatforbrug",
                    PRISENHED = "2020-priser, kædede værdier",
                    SÆSON = "Sæsonkorrigeret",
                    Tid = "*")

Forbrug <- dst_get_data(table = "NKHC1", query = FORB_filter, lang = "da")

Forbrug <- Forbrug[-c(1:4),-c(1:3)]

rownames(Forbrug) <- NULL

Kategori_Forbrug <- c("Kvartal","Forbrug")

colnames(Forbrug) <- Kategori_Forbrug

Forbrug$Realvaekst <- round(c(rep(0, 4), 
                              (Forbrug$Forbrug[5:length(Forbrug$Forbrug)] / 
                                 Forbrug$Forbrug[1:(length(Forbrug$Forbrug) - 4)] - 1) * 100), 2)

Forbrug1 <- Forbrug[-c(1:36),-2]

rownames(Forbrug1) <- NULL

#####################################
####Privatforbrug & Forventninger####
#####################################

Forbrug_endelig <- cbind(Forbrug1, Forbrugertillid)

## Jf. s. 14 i EU kommissions analyse https://ec.europa.eu/economy_finance/publications/pages/publication14353_en.pdf 
# skal spg 5 og 7 have omvendte fortegn, da de er negativt korreleret med den afhængige variabel, se spg på s. 29

Forbrug_endelig$`Priser i dag, sammenlignet med for et år siden` <- -Forbrug_endelig$`Priser i dag, sammenlignet med for et år siden`

Forbrug_endelig$`Arbejdsløsheden om et år, sammenlignet med i dag` <- -Forbrug_endelig$`Arbejdsløsheden om et år, sammenlignet med i dag`

####################################
####Opgave 3 – Julehandel i 2024####
####################################

#################################
####Opgave 3.1 – Forudsigelse####
#################################

#Lav en Machine Learning model, der med afsæt i DST’s forbrugertillidsindikator, kan forudsige om
#julehandlen i 2024 er større end i 2023.

Forbrugertillidsindikatoren <- Forventning[-c(1:101,201),2]

Forbrug2 <- Forbrug[-c(1:36),-2]

Forbrug_logi <- cbind(Forbrugertillidsindikatoren,Forbrug2)

rownames(Forbrug_logi) <- NULL

Forbrug_logi$Realretning <- ifelse(Forbrug_logi$Realvaekst >= 0, "Op", "Ned")

table(Forbrug_logi$Realretning)

Forbrug_logi$Realretning <- as.factor(Forbrug_logi$Realretning)

Realvaekst_Op <- subset(Forbrug_logi, Realretning == "Op")
Realvaekst_Ned <- subset(Forbrug_logi, Realretning == "Ned")

Mean_Op <- colMeans(Realvaekst_Op[, sapply(Realvaekst_Op, is.numeric)])

Mean_Ned <- colMeans(Realvaekst_Ned[, sapply(Realvaekst_Ned, is.numeric)])

Mean_combined <- rbind(Op = Mean_Op, Ned = Mean_Ned)

Mean_combined <- as.data.frame(Mean_combined)

Mean_combined$Kategori <- rownames(Mean_combined)

Mean_combined <- Mean_combined[,-2]

Mean_long <- pivot_longer(Mean_combined, 
                          cols = -Kategori, 
                          names_to = "Variabel", 
                          values_to = "Gennemsnit")

ggplot(Mean_long, aes(x = Kategori, y = Gennemsnit, fill = Kategori)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gennemsnit for spørgsmålene i FTI (Op vs Ned)", 
       x = "Realretning (Op/Ned)", 
       y = "Gennemsnit") +
  facet_wrap(~ Variabel, scales = "free_y") +  # Særskilte diagrammer for hver variabel
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ForbrugGlm <- glm(Realretning ~ `Forbrugertillidsindikatoren`,
                  data = Forbrug_logi, family = binomial)

ForbrugGlm_Forudsigelse <- predict(ForbrugGlm ,type="response")

GLM2024Q3_DST <- data.frame("Forbrugertillidsindikatoren" = -6.53)
colnames(GLM2024Q3_DST) <- "Forbrugertillidsindikatoren"
Predict_DST2024Q3 <- predict(ForbrugGlm, newdata = GLM2024Q3_DST, type = "response")

DST_2024Q3 <- 1 / (1 + exp(-(1.41874 + 0.12500 * (-6.53))))

## Der er 64,62 % sandsynlighed for at forbruget går op (stiger) i 2024Q3.

GLM2024Q4_DST <- data.frame("Forbrugertillidsindikatoren" = -8.90)
colnames(GLM2024Q4_DST) <- "Forbrugertillidsindikatoren"
Predict_DST2024Q4 <- predict(ForbrugGlm, newdata = GLM2024Q4_DST, type = "response")

DST_2024Q4 <- 1 / (1 + exp(-(1.41874 + 0.12500 * (-8.90))))

## Svar: Der er 57,59 % sandsynlighed for at forbruget går op (stiger) i 2024Q4. Ergo julehandlen forventes at stige.

########################################
####Opgave 3.2 – Validering af model####
########################################

## Konfusion matrix vha. funktion
# Først binariser forudsigelser baseret på 50 % threshold
# ForbrugGlm_Forudsigelse er forudsigelserne beregnet tidligere, se ovenfor.

Threshold <- 0.5 # grænseværdi på 50 % for forudsigelsen

ForbrugGlm_ForudsigelseBin <- ifelse(ForbrugGlm_Forudsigelse > Threshold, "Op", "Ned")

Konfusion_matrix <- table(Predicted = ForbrugGlm_ForudsigelseBin, Actual = Forbrug_logi$Realretning)

print(Konfusion_matrix)

## Konfusion matrix manuel udarbejdet

library(pROC)

# Create the original ROC curve
ROC_kurve <- roc(Forbrug_logi$Realretning, ForbrugGlm_Forudsigelse,
                 levels = c("Ned", "Op"), direction = "<")

# Smooth the ROC curve
Smoothed_ROC <- smooth(ROC_kurve)

# Calculate AUC
roc_auc <- auc(ROC_kurve)

# Plot the smoothed ROC curve
plot(Smoothed_ROC, col = "blue", lwd = 2, main = "Modellens AUC er på 0.75")

# Add AUC as text on the plot
text(x = 0.6, y = 0.2, labels = paste("AUC =", round(roc_auc, 2)), col = "black", cex = 1.2)
