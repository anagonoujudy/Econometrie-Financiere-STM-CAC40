### cours: économétrie financière

# Importation des librairies R dealing with TS 
install.packages("tseries")
install.packages("forecast")
install.packages("seastests")
install.packages("astsa")
install.packages("zoo")
install.packages("lmtest")
install.packages("TTR")
install.packages("psych")
install.packages("rugarch")

#télécharger les librairies
library(tseries)
library(forecast)
library(seastests)
library(astsa)
library(zoo)
library(lmtest)
library(TTR)
library(moments)
library(psych)
library(rugarch)

# IMPORTATION DES DONNEES
chemin_cours_action = "data/STMICROELECTRONICS_historical_price.csv"
chemin_indice = "data/CAC_40_historical_price.csv"


# Action
cours_action =  read.csv(chemin_cours_action, header = 3, sep = ";",skip = 3, dec =".") # Données Journalières
View(cours_action)
str(cours_action) # Vérification de la typologie de chaque colonne 

#Indice 
indice_CAC40 =  read.csv(chemin_indice, header = 3, sep = ";",skip = 3, dec=".") # Données Journalières 
View(indice_CAC40)
str(indice_CAC40) # Vérification de la typologie de chaque colonne 

# TRAITEMENT DES DONNEES
prix_action = cours_action[, c("Date", "Open", "Close")]
prix_indice = indice_CAC40[, c("Date", "Open", "Close")]

prix_action$Date = as.Date(gsub("^'", "", cours_action$Date), format = "%d/%m/%Y") # pour enlever l'apostrophe devant certaines dates
prix_indice$Date = as.Date(gsub("^'", "", cours_action$Date), format = "%d/%m/%Y")

str(prix_action)
str(prix_indice)

# On inverse l'ordre des lignes pour que la dataframe contienne 
#les données du moins récent au plus récent
prix_stm = prix_action[nrow(prix_action):1, ]
prix_cac40 = prix_indice[nrow(prix_indice):1, ]

# Réinitialisation des numéros de lignes
rownames(prix_stm) = NULL
rownames(prix_cac40) = NULL
View(prix_stm)
View(prix_cac40)

# Fréquence hebdomadaires 
# Création des dates hebdomadaires
dates = seq(as.Date("2023-06-02"), as.Date("2025-06-09"), by = "week")
prix_stm_hebdo = prix_stm[prix_stm$Date %in% dates, ] # Pour chercher les valeurs qui valeurs qui correspondent
prix_cac40_hebdo = prix_cac40[prix_cac40$Date %in% dates, ]# aux dates hebdomadaires générées

# Fréquence mensuelles
dates = seq(as.Date("2023-06-02"), as.Date("2025-06-09"), by = "month")
prix_stm_m = prix_stm[prix_stm$Date %in% dates, ] # Pour chercher les valeurs qui valeurs qui correspondent
prix_cac40_m = prix_cac40[prix_cac40$Date %in% dates, ]# aux dates hebdomadaires générées


# CHOIX DU PRIX

# Evolution des prix et comparaion 
#----------------------------------

# Les données collectées sont déjà indexées par le temps. 
# Plus besoin d'appliquer une fonction de transformation en série temp

#fréquence journalier
#Action
plot(prix_stm$Date, prix_stm$Open, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Prix", main = "Prix Open et Close STMicroElectronic")
lines(prix_stm$Date, prix_stm$Close, col = "red", lwd = 2)
legend("topright", legend = c("Open", "Close"), col = c("blue", "red"), lwd = 2)

#Indice

plot(prix_cac40$Date, prix_cac40$Open, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Prix", main = "Prix Open et Close CAC 40")
lines(prix_cac40$Date, prix_cac40$Close, col = "red", lwd = 2)
legend("topleft", legend = c("Open", "Close"), col = c("blue", "red"), lwd = 2)

#Fréquence hebdomadaire

plot(prix_stm_hebdo$Date, prix_stm_hebdo$Close, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Prix", main = "Prix hebdomadaires STM")

# Ajouter une bande verte sur la période de hausse significative
date_debut_hausse <- as.Date("2023-10-20")
date_fin_hausse <- as.Date("2023-12-18")

rect(xleft = date_debut_hausse, xright = date_fin_hausse,
     ybottom = par("usr")[3], ytop = par("usr")[4],
     col = rgb(0, 1, 0, 0.2), border = NA)

# Ajouter une bande rouge sur la période de chute
date_debut_chute <- as.Date("2023-12-19")
date_fin_chute <- as.Date("2025-05-05")

rect(xleft = date_debut_chute, xright = date_fin_chute,
     ybottom = par("usr")[3], ytop = par("usr")[4],
     col = rgb(1, 0, 0, 0.2), border = NA)



#-----------------------------------------------------------------------------------
plot(prix_cac40_hebdo$Date, prix_cac40_hebdo$Close, type = "l", col = "blue", lwd = 2,
     xlab = "Date", ylab = "Prix", main = "Prix journaliers Cac 40")

# Ajouter une bande verte sur la période de hausse significative
date_debut_hausse <- as.Date("2023-10-20")
date_fin_hausse <- as.Date("2024-05-15")

rect(xleft = date_debut_hausse, xright = date_fin_hausse,
     ybottom = par("usr")[3], ytop = par("usr")[4],
     col = rgb(0, 1, 0, 0.2), border = NA)

# Ajouter une bande rouge sur la période de chute
date_debut_chute <- as.Date("2025-02-18")
date_fin_chute <- as.Date("2025-05-05")

rect(xleft = date_debut_chute, xright = date_fin_chute,
     ybottom = par("usr")[3], ytop = par("usr")[4],
     col = rgb(1, 0, 0, 0.2), border = NA)



# VERIFICATION DE LA STATIONNARITE DES PRIX
#STM
acf(prix_stm$Close, lwd = 2, lag.max=20) #ACF decroissant
pacf(prix_stm$Close, lwd = 2,lag.max=20)
adf.test(prix_stm$Close, alternative="explosive",k=0)

#CAC 40
acf(prix_cac40$Close, lwd = 2, lag.max=20) #ACF decroissant
pacf(prix_cac40$Close, lwd = 2,lag.max=20)
adf.test(prix_cac40$Close, alternative="explosive",k=0)



# CALCUL DU RENDEMENT
# Comparaison des méthodes
# Rendements arithmétiques
rdt_arith_stm <- diff(prix_stm$Close) / head(prix_stm$Close, -1)
rdt_arith_cac40 <- diff(prix_cac40$Close) / head(prix_cac40$Close, -1)

# Rendements logarithmiques
# a = c(1,2)
# > diff(log(a))
# [1] 0.6931472 = log(2) - log(1)
# Etant donnée que les prix vont du moins récent au plus récent, il suffit de faire diff(log(prix_stm$Close))
rdt_stm <- diff(log(prix_stm$Close))
rdt_cac40 <- diff(log(prix_cac40$Close))


# Visualisation et comparaison des deux rendements
plot(prix_stm$Date[-nrow(prix_stm)], rdt_arith_stm, type = "l", col = "red", lwd = 2,
     xlab = "Date", ylab = "Rendements",main = "Comparaison des rendements arithmétiques et logarithmiques STM")
lines(prix_stm$Date[-nrow(prix_stm)], rdt_stm, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Arithmétique", "Logarithmique"), col = c("red", "blue"), lwd = 2)

plot(prix_cac40$Date[-nrow(prix_cac40)], rdt_arith_cac40, type = "l", col = "red", lwd = 2,
     xlab = "Date", ylab = "Rendements",main = "Comparaison des rendements arithmétiques et logarithmiques CAC 40")
lines(prix_cac40$Date[-nrow(prix_cac40)], rdt_cac40, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Arithmétique", "Logarithmique"), col = c("red", "blue"), lwd = 2)



#Identification des dates des pics haussiers et baissiers en vue d'une compréhension 
# du contexte macroéconomique et géopolitique à leurs origines.

library(ggplot2)

analyser_rendements = function(donnees_prix) {
  # Extraction
  dates = donnees_prix$Date
  close = donnees_prix$Close
  
  # Calcul des rendements
  rendements = diff(log(close))
  dates_rendements = dates[-1]
  
  moyenne = mean(rendements)
  ecart_type = sd(rendements)
  borne_sup = moyenne + 2*ecart_type
  borne_inf = moyenne - 2*ecart_type
  
  pics_index = which(rendements > borne_sup | rendements < borne_inf)
  dates_pics = dates_rendements[pics_index]
  valeurs_pics = rendements[pics_index]
  
  df = data.frame(Date = dates_rendements, Rendement = rendements)
  
  # Construction du graphique
  p = ggplot(df, aes(x = Date, y = Rendement)) +
    # Bande de stabilité
    geom_ribbon(aes(ymin = borne_inf, ymax = borne_sup), fill = "grey80", alpha = 0.4) +
    
    geom_line(color = "blue", lwd = 1) +
    geom_hline(yintercept = moyenne, lwd = 1, color = "black", linetype = "dashed") +
    geom_hline(yintercept = c(borne_sup, borne_inf), lwd = 1, color = "red", linetype = "dotted") +
    geom_point(data = df[pics_index, ], aes(x = Date, y = Rendement), color = "darkred", size = 2) +
    geom_text(data = df[pics_index, ], aes(label = format(Date, "%Y-%m-%d")),
              vjust = -1, size = 3, color = "darkred") +
    labs(title = "Analyse des rendements : zone stable et pics STM",
         x = "Date", y = "Rendement") +
    theme_minimal()
  
  print(p)
  
}

pics_detectes_stm = analyser_rendements(prix_stm)
pics_detectes_cac40 = analyser_rendements(prix_indice)

# FAITS STYLISES
# Stationnarité des rendements

#STM
acf(rdt_stm , lwd = 2, lag.max=20)
pacf(rdt_stm,lwd = 2, lag.max=20)
adf.test(rdt_stm, k=0)

#CAC 40
acf(rdt_cac40 , lwd = 2, lag.max=20)
pacf(rdt_cac40,lwd = 2, lag.max=20)
adf.test(rdt_cac40, k=0)

#Vérification de l'autocorrélation
Box.test(rdt_stm, lag = 20, type = c("Ljung-Box"), fitdf = 0)
Box.test(rdt_cac40, lag = 20, type = c("Ljung-Box"), fitdf = 0)

#Statstiques descriptives

summary(rdt_stm)
sd(rdt_stm)
skewness(rdt_stm)
kurtosis(rdt_stm)

summary(rdt_cac40)
sd(rdt_cac40)
skewness(rdt_cac40)
kurtosis(rdt_cac40)

#Test de nullité de la moyenne 
t.test(rdt_stm, mu = 0)
t.test(rdt_cac40, mu = 0)

#Distribution de probabilité des rendements
mu <- mean(rdt_stm)
sigma <- sd(rdt_stm)

# Histogramme avec densité
hist(rdt_stm,
     breaks = 30,
     col = "skyblue",
     border = "white",
     prob = TRUE,  
     main = "Histogramme et densités des rendements STM",
     xlab = "Rendements log")

# Ajout de la densité empirique
lines(density(rdt_stm), col = "blue", lwd = 2)

# Ajouter la densité normale théorique
curve(dnorm(x, mean = mu, sd = sigma),
      col = "red", lwd = 2, lty = 2, add = TRUE)

# Ajouter une légende
legend("topleft",
       legend = c("Densité empirique", "Densité normale"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2)


# CAC 40
mu <- mean(rdt_cac40)
sigma <- sd(rdt_cac40)

# Histogramme avec densité
hist(rdt_cac40,
     breaks = 30,
     col = "skyblue",
     border = "white",
     prob = TRUE,  
     main = "Histogramme et densités des rendements CAC 40",
     xlab = "Rendements log")

# Ajouter la densité empirique
lines(density(rdt_cac40), col = "blue", lwd = 2)

# Ajouter la densité normale théorique
curve(dnorm(x, mean = mu, sd = sigma),
      col = "red", lwd = 2, lty = 2, add = TRUE)

# Ajouter une légende
legend("topleft",
       legend = c("Densité empirique", "Densité normale"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2)


#TEST DE NORMALITE

#Fréquence journalière
jarque.bera.test(rdt_stm)
jarque.bera.test(rdt_cac40)

#Fréquence hebdomadaire
jarque.bera.test(diff(log(prix_stm_hebdo$Close)))
jarque.bera.test(diff(log(prix_cac40_hebdo$Close)))

#Fréquence mensuelle
jarque.bera.test(diff(log(prix_stm_m$Close)))
jarque.bera.test(diff(log(prix_cac40_m$Close)))

#Analise de la volatilité
#STM
rdt_action_carre = rdt_stm^2
plot(prix_stm$Date[-nrow(prix_stm)],rdt_action_carre, type = "l",lwd = 3, col = "blue",
     main = "Variance dans de temps STM", xlab = "Dates", ylab = "Rdt au carré")
acf(rdt_action_carre, lwd = 2, main = " ACF Rendements au carré STM")
pacf(rdt_action_carre, lwd = 2, main = " PACF Rendements au carré STM")
#CAC 40 
rdt_indice_carre = rdt_cac40^2
plot(prix_cac40$Date[-nrow(prix_cac40)],rdt_indice_carre, type = "l",lwd = 3, col = "blue",
     main = "Variance dans le temps CAC 40", xlab = "Dates", ylab = "Rdt au carré")
acf(rdt_indice_carre, lwd = 2, main = " ACF Rendements au carré CAC 40")
pacf(rdt_indice_carre, lwd = 2, main = " PACF Rendements au carré CAC 40")


#MEDAF

#Correlation entre l'action et l'indice

cor(rdt_stm,rdt_cac40)

plot(prix_stm$Date[-nrow(prix_stm)], rdt_stm, type = "l", lwd = 3, col = "blue",
     xlab = "Date", ylab = "Rendement", main = "Rendements CAC40 et STM")
lines(prix_stm$Date[-nrow(prix_stm)],rdt_cac40, col = "red", lwd = 3)  #
legend("bottomleft", legend = c("STM", "CAC40"), col = c("blue", "red"), lwd = 3)

# Régresion linéaire
 capm = lm(rdt_stm~ rdt_cac40 )
 summary(capm)

# Test de significativité de beta 
# Extraire les résultats pour β
 beta = coef(capm)[2] 
 
#Analyse des résidus
plot(fitted(capm), resid(capm),
     main = "Résidus vs Valeurs ajustées", xlab = "Valeurs ajustées", ylab = "Résidus")
abline(h = 0, col = "red")

Mresidus<-ts(capm$residuals,start=c(2023,6), end=c(2025,6),frequency = 252)
plot.ts(Mresidus)
abline(h = 0, col = "red")
acf(capm$residuals,lag.max=20) # ACF s'arrête à l'ordre 1
pacf(capm$residuals,lag.max=20) #les coefficients d'auto-correlation partiels sont non significatifs
acf(capm$residuals^2,lag.max = 20) #ACF s'arrête à l'ordre 1


# # Test de Breusch-Pagan
library(lmtest)
bptest(capm)

#-----------------------------------------------------------------------
#MODELE GARCH
# Installer et charger le package si ce n'est pas déjà fait
install.packages("rugarch")
library(rugarch)


# Spécification du modèle GARCH(1,1)
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0),include.mean = TRUE),  # sans ARMA
  distribution.model = "norm"
)

# Ajustement du modèle sur les rendements
fit_action <- ugarchfit(spec = spec, data = rdt_stm)
fit_indice <- ugarchfit(spec = spec, data = rdt_cac40)

# Extraction de la volatilité conditionnelle (éart-type)
volatilite_action <- sigma(fit_action)
volatilite_indice = sigma(fit_indice)

# Visualisation
plot(prix_stm$Date[-nrow(prix_stm)], volatilite_action, type = "l", col = "blue", lwd = 2,
     ylab = "Volatilité", main = "Volatilité conditionnelle (GARCH) du rendement de l'action")

plot(prix_stm$Date[-nrow(prix_stm)],volatilite_indice, type = "l", col = "blue", lwd = 2,
     ylab = "Volatilité", main = "Volatilité conditionnelle (GARCH) CAC 40")

#PREDICTION
library(forecast)

# horizon de prévision
h <- 25
forecast_stm <- ugarchforecast(fit_action, n.ahead = h)
plot(forecast_stm)
forecast_cac40 <- ugarchforecast(fit_indice, n.ahead = h)
plot(forecast_cac40, lwd = 2)


