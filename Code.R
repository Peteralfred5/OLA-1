#### OLA ####
#### OPG 1 ####
#### OPG 1.1 ####

library(readr)
f
#indlæs fil som csv
boligdf <- read_delim("boligsiden_key.csv", delim = ";")

#laver liggetid om til numeric ved at fjerne ordet dag
boligdf$liggetid <- as.numeric(gsub("\\.|dag", "", boligdf$liggetid))

# før na.omit 2552 observationer efter 2367
# fjern alle na værdier og tilsvarende række i df 
boligdf <- na.omit(boligdf)

#find salgsopslaget om tousvej 106 i df
Tousvej_106 <- boligdf[boligdf$vej == "tousvej", ]

#find salgsopslaget om egevej 20 i df
Egevej_20 <- boligdf[boligdf$vej == "egevej" & boligdf$vejnr == "20", ]

#### OPG 1.2 ####

#her finder jeg to tilfædige rækker i vores df
boligdf[sample(nrow(boligdf), 2), ]

# de to opslag jeg fik ud første gang ser således ud
# Fuglsangvej 4 & Kongehoej 4

# fuglsangvej url 
"https://www.boligsiden.dk/adresse/fuglsangvej-4-7184-vandel-06300667___4_______"

#Kongehoej 4
"https://www.boligsiden.dk/adresse/kongehoej-4-6600-vejen-05754147___4_______"

#### 2.1 ####

#fjern alle boliger med kvmpris under 10
boligdf <- boligdf[boligdf$kvmpris >= 10, ]

#fjern alle boliger med mdudg over 150000
boligdf <- boligdf[boligdf$mdudg <=15000, ]

# standardafvigelsen for kvmpris og mdudg
standardafvigelse <- sd(boligdf$størrelse, na.rm = TRUE)
print(standardafvigelse)

# lave boligpriser om til numerics
boligdf$pris <- as.numeric(gsub("\\.|kr.", "", boligdf$pris))

# resultat efter gsub 
str(boligdf$pris)

#hsitogram over kvmpris
hist(boligdf$kvmpris, brekas= 50, main = "Kvadratmeterpris", xlab = "Kvadratmeterpris")

#### 2.2 ####

korrelation <- lm(pris ~ størrelse, data = boligdf)
summary(korrelation)

cor <- cor(boligdf$pris, boligdf$størrelse, use = "complete.obs")


#### 2.3 ####

# skabe alder for boligerne i år
boligdf$alder <- 2024-boligdf$opført

uafhaengige_variabler <- c("liggetid", "mdudg", "værelser", "opført")

model_summaries <- list()

for (variabel in uafhaengige_variabler) {
  
  formel <- as.formula(paste("kvmpris ~", variabel))
  
  model <- lm(formel, data = boligdf)
  
  model_summaries[[variabel]] <- summary(model)
}

model_summaries[["liggetid"]]

# kategorisere værelser
boligdf$værelser_kategori <- cut(
  boligdf$værelser,
  breaks = c(-Inf, 2, 4, Inf),  # Grænser for kategorierne
  labels = c("1-2 værelser", "3-4 værelser", "5+ værelser"),
  right = TRUE                  # Inkluder højre grænse i intervallet
)

str(boligdf)

#skabe korrelationsmatrix ved brug af de valgte variabler
korrelationsmatrix <- cor(boligdf[, c("kvmpris", "størrelse", "opført",
                                      "liggetid", "postnr", "grund")])

install.packages("ggcorrplot")
library("ggcorrplot")

ggcorrplot(korrelationsmatrix, method = "square",
           lab = TRUE, type = "lower")


#### 3.1 ####

#lave en terning som bliver kastet 25000 gange 
kast25000 <- function(){
  kast <- sample(1:6, size = 25000, replace = TRUE)
  return(kast)
}

#hvor mange 5'er bliv rullet
antal_5 <- sum(kast25000() == 5)

print(antal_5)

#hvad er sansynligheden for at en 5'er bliver slået = 16.5%
sansynlighed_5 <- antal_5/25000

#### 3.2 ####

# kast 6 terninger en gang og find summen
kast_terning <- function(){
  kast_resultater <- sample(1:6, size = 6, replace = TRUE)
  resultat_6_terninger <- sum(kast_resultater)
}

#kør functionen
kast_terning()

#replicate functionen 10000 gange
kast10000 <- replicate(10000, kast_terning())

#lav en barplot over de 10000 kast
barplot(table(kast10000),
        main = "10000 kast begynder at tage form af en normalfordeling",
        xlab = "Sum af 6 terningekast",
        ylab = "Frekvens(Antal kast)",
        col = "lightblue",
        border = "black")

#### 3.3 ####

kast1000000 <- as.data.frame(replicate(1000000, kast_terning()))

#lav en barplot over de 1000000 kast
barplot(table(kast1000000),
        main = "1000000 kast former en normalfordeling",
        xlab = "Sum af 6 terningekast",
        ylab = "Frekvens(Antal kast)",
        col = "lightblue",
        border = "black")

#### 3.4 ####

random <- sample(1:6)

notrandom <- 2:6

matrix <- cbind(notrandom,random)
print(matrix)

#### 4.1 ####

#indlæs filen 
alko <- read.csv("data2/FU02.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE,
                 strip.white = TRUE, na.strings = c("NA",""), fileEncoding = "latin1")

#fjerne alle na værdier i df
alko <- na.omit(alko)

#reset række numre 
rownames(alko) <- NULL

# lave årstal om til headers
colnames(alko) <- alko[1, ]

#fjern de resterende årstal 
alko <- alko[-1, ]

# Step 2: Remove the `.1` column from the dataframe
alko <- alko[, -1]

# rotere alko med brug af transpose
alko <- as.data.frame(t(alko))

# ændre overskrifterne til kolonenavne
colnames(alko) <- alko[1, ]
alko <- alko[-1, ]

# skabe en ny kolone med årstal for at bedre kunne bruge ggplot
alko$Year <- as.numeric(rownames(alko))

# fjern de tal som er med i overskrifterne
colnames(alko) <- gsub("^[0-9.]+ ", "", colnames(alko))

# lave den nuværende kolonenavne med årstal om til en kolone så den kan bruges i plot
alko$Year <- as.numeric(rownames(alko))

#flyt den nye årstal kolone til den første række
alko <- alko[, c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

# slet årene optil 2000
alko <- alko[-(1:6), ]

# reset colnames
rownames(alko) <- NULL

library(ggplot2) # brugt til at lave et plot
library(tidyr)  # brugt til at omforme det sidste data

# Omdanne data til long format 
alko_long <- pivot_longer(alko, cols = -Year, names_to = "Kategori", values_to = "Forbrug")

# Convert the Forbrug column to numeric
alko_long$Forbrug <- as.numeric(alko_long$Forbrug)

# Remove the unwanted category
alko_long <- alko_long[alko_long$Kategori != "ALKOHOLISKE DRIKKEVARER OG TOBAK", ]

# Skabe plot 
ggplot(alko_long, aes(x = Year, y = Forbrug, color = Kategori, group = Kategori)) +
  geom_line(size = 0.5) +  # Tyggelse af linjerne
  geom_point(size = 0) +   # størrelsen af punkterne
  labs(
    title = "Udvikling i alkoholdforbrug over tid",
    x = "År",
    y = "Faste priser i kr. pr. husstand",
    color = "Kategori"
  ) +
  scale_y_continuous(breaks = seq(0, 3000, by = 1000)) +  # punkter på y axis
  theme_minimal(base_size = 14) +  # Minimal theme with larger text
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16),  # Sæt titlen i midten af plottet
    legend.position = "bottom",                         # Move legend to the bottom
    legend.title = element_text(size = 12),             # Adjust legend title size
    legend.text = element_text(size = 10)               # Adjust legend text size
  )

# fokus på de fire kategorier

# Reshape data from wide to long format
alko_long <- pivot_longer(alko, cols = -Year, names_to = "Kategori", values_to = "Forbrug")

# Convert the Forbrug column to numeric
alko_long$Forbrug <- as.numeric(alko_long$Forbrug)

# Remove the unwanted category
alko_long <- alko_long[alko_long$Kategori != "ALKOHOLISKE DRIKKEVARER OG TOBAK", ]

# Define the categories to highlight
highlight_categories <- c(
  "Spiritus og likør",
  "Vin af druer",
  "Pilsnerøl, guldøl",
  "Øl med lavt alkoholindhold og alkoholfri øl"
)

# Add a new column to group categories into highlight or other
alko_long$color_group <- ifelse(
  alko_long$Kategori %in% highlight_categories,
  alko_long$Kategori,  # Keep the category name for highlighted ones
  "Andre"  # Group all others into "Andre"
)

# Define custom colors for highlighted categories and gray for others
custom_colors <- c(
  "Spiritus og likør" = "blue",
  "Vin af druer" = "purple",
  "Pilsnerøl, guldøl" = "green",
  "Øl med lavt alkoholindhold og alkoholfri øl" = "red",
  "Andre" = "gray"
)

# Create the plot
ggplot(alko_long, aes(x = Year, y = Forbrug, color = color_group, group = Kategori)) +
  geom_line(size = 0.5) +  # Line thickness
  geom_point(size = 0) + # Point size
  labs(
    title = "Udvikling i alkoholdforbrug over tid",
    x = "År",
    y = "Faste priser i kr. pr. husstand",
    color = "Kategori"
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme_minimal(base_size = 14) +  # Minimal theme with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centered title
    legend.position = "bottom",             # Legend at the bottom
    legend.title = element_text(size = 12), # Legend title size
    legend.text = element_text(size = 10)   # Legend text size
  )

#### 4.2 #### 

library(ggcorrplot)

# lav en dataframe med disse tre koloner fra alko dataframen 
kor_mat_plot <- alko[, c("Spiritus og likør", "Vin af druer", "Pilsnerøl, guldøl")]

# konvertere koloner til numerics
kor_mat_plot$`Spiritus og likør` <- as.numeric(kor_mat_plot$`Spiritus og likør`)
kor_mat_plot$`Vin af druer` <- as.numeric(kor_mat_plot$`Vin af druer`)
kor_mat_plot$`Pilsnerøl, guldøl` <- as.numeric(kor_mat_plot$`Pilsnerøl, guldøl`)

# Beregn korrelationsmatrix
kor_matrix <- cor(kor_mat_plot, use = "complete.obs")

# Vis korrelationsmatrix
print(kor_matrix)

# Plot korrelationmatrixen
ggcorrplot(kor_matrix, 
           method = "square",    # Shape of the plot (square tiles)
           type = "lower",       # Display only lower triangle of the matrix
           lab = TRUE,           # Add correlation coefficients to the plot
           lab_size = 5,         # Size of the labels
           colors = c("blue", "white", "red"), # Color gradient
           title = "Stiger danskernes forbrug af vin eller øl, stiger modparten ligeledes",
           legend.title = "Corr") # Title of the legend

#### opg 5.1 ####

# Første kolonne: "Klasse"
klasse <- rep(c("A", "B", "C", "D"), each = 9)

# Anden kolonne: "Uge"
uge <- rep(seq(1, 9), times = 4)

# Tredje kolonne: "karakter" - tilfældige karaktere mellem -3 og 12
set.seed(42)  # For reproducerbare resultater
score <- sample(c(-3,00,02,4,7,10,12), size = 36, replace = TRUE)

# Kombiner til en dataframe
klassedf <- data.frame(Klasse = klasse, Uge = uge, Score = score)

# Print dataframe
print(klassedf)

#### 5.2 ####

nyklasse_df <- data.frame(Klasse = character(), Uge = integer(), Avg_Score = numeric())

for (i in 1:nrow(klassedf)) {
  if (i %% 3 == 0) {
    avg_score <- mean(klassedf$Score[(i-2):i])
    
    nyklasse_row <- data.frame(
      Klasse = klassedf$Klasse[i],
      Uge = klassedf$Uge[i],
      Avg_Score = avg_score
    )
    
    nyklasse_df <- rbind(nyklasse_df, nyklasse_row)
  }
}



