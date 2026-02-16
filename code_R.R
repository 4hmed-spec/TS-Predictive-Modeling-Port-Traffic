# --- PARTIE VISUALISATION  ---
# 1. Chargement des données
data <- read.csv(file.choose(), header = TRUE, sep = ",", stringsAsFactors = FALSE)# prendre le jeu de données
View(df)

install.packages("ggplot2")
library(ggplot2)
install.packages("forecast") 
library(forecast)
install.packages("corrplot")
library(corrplot)
# 2. Nettoyage et Typage 
# La date doit être au format Date pour les graphiques
data$date <- as.Date(data$date)
# Le jour de la semaine en Facteur ordonné (Lundi au Dimanche)
data$weekday <- factor(data$weekday, 
                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


# A. Vérifier les valeurs manquantes (NA)
nb_na <- sum(is.na(data))
print(paste("Nombre de valeurs manquantes :", nb_na))


# B. Résumé statistique pour détecter les "Valeurs Extrêmes"
# Regarde les Min et Max de "vessel_arrivals"
summary(data[, c("vessel_arrivals", "cargo_tonnage", "oil_price")])

# On sélectionne uniquement les colonnes numériques pertinentes pour la corrélation
cols_numeriques <- data[, c("vessel_arrivals", "cargo_tonnage", "oil_price", "eur_usd", "baltic_index")]
# Calcul de la matrice de corrélation
matrice_corr <- cor(cols_numeriques)
print(matrice_corr)
#  graphique de corrélation 
corrplot(matrice_corr, method="circle", type="upper", tl.col="black", 
         title="Matrice de Corrélation", mar=c(0,0,1,0))

# --- PARTIE graph simple a comprendre pour la partie manageriale   ---

autoplot(ts_navires_hebdo) +
  ggtitle("Évolution du nombre d'arrivées de navires") +
  xlab("Année") +
  ylab("Arrivées de navires") +
  theme_minimal()
# Moyenne par semaine de l'année
data$week <- as.numeric(format(as.Date(data$date), "%U"))
# comparer les années

data$year <- format(as.Date(data$date), "%Y")

annuel <- aggregate(
  vessel_arrivals ~ year,
  data = data,
  sum
)

ggplot(annuel, aes(x = year, y = vessel_arrivals)) +
  geom_col(fill = "steelblue") +
  ggtitle("Nombre annuel d'arrivées de navires") +
  xlab("Année") +
  ylab("Arrivées") +
  theme_minimal()



# --- PARTIE VISUALISATION  ---


# --- PRÉPARATION DES DONNÉES ---
# 1. Série Journalière (pour voir les détails et la semaine)
ts_jour <- ts(data$vessel_arrivals, start=c(2019, 1), frequency=365)
# Graphique temporel 1
print(autoplot(ts_jour) +
        ggtitle("Chronique des arrivées journalières (2019-2023)") +
        xlab("Années") + ylab("Nombre de navires") + # Tendance linéaire
        theme_bw())

# --- ETAPE 1 : Création de la série mensuelle ---
# On calcule la moyenne par mois pour avoir une courbe lisible
data$date <- as.Date(data$date)
data$mois_annee <- format(data$date, "%Y-%m")

# Calcul de la moyenne des arrivées pour chaque mois
donnees_mensuelles <- aggregate(vessel_arrivals ~ mois_annee, data=data, FUN=mean)

# Transformation en Time Series (Fréquence 12 = Janvier à Décembre)
ts_mensuel <- ts(donnees_mensuelles$vessel_arrivals, start=c(2019, 1), frequency=12)

# --- ETAPE 2 : Génération du Seasonal Plot  ---
# year.labels=TRUE : Affiche les années sur le côté
print(ggseasonplot(ts_mensuel, year.labels=TRUE, year.labels.left=TRUE) +
  ggtitle("Figure F : Représentation saisonnière (Mensuelle)") +
  ylab("Moyenne navires/jour") +
  theme_bw())



data$mois_annee <- format(data$date, "%Y-%m")
# Calcul de la moyenne des arrivées pour chaque mois
donnees_mensuelles <- aggregate(vessel_arrivals ~ mois_annee, data=data, FUN=mean)

# Transformation en Time Series (Fréquence 12 = Janvier à Décembre)
ts_mensuel <- ts(donnees_mensuelles$vessel_arrivals, start=c(2019, 1), frequency=12)

# --- Seasonal Plot  ---
# year.labels=TRUE : Affiche les années sur le côté
print(ggseasonplot(ts_mensuel, year.labels=TRUE, year.labels.left=TRUE) +
        ggtitle("Représentation saisonnière (Mensuelle)") +
        ylab("navires/jour") +
        theme_bw())
# --- lage Plot  ---


# 1. Création de l'objet Time Series
# On utilise frequency = 7 pour indiquer un cycle hebdomadaire
data$week_id <- format(as.Date(data$date), "%Y-%U")


donnees_hebdo <- aggregate(
  vessel_arrivals ~ week_id,
  data = data,
  FUN = mean
)

ts_navires_hebdo <- ts(
  donnees_hebdo$vessel_arrivals,
  frequency = 52,
  start = c(2019, 1)
)

# 2. Affichage du subserieplot

ggsubseriesplot(ts_navires_hebdo)

# 2. Affichage du auto corrélogramme


ggAcf(ts_navires_hebdo, lag.max = 60) +
  ggtitle("Auto-corrélogramme – série hebdomadaire") +
  theme_bw()

# 2. Affichage des Lag Plots

gglagplot(ts_navires_hebdo, lags = 7, do.lines = FALSE) +
  ggtitle("Diagrammes de retard - Arrivées de navires ") +
  xlab("Arrivées (t-k)") +
  ylab("Arrivées (t)") +
  theme_minimal()

#Analyse comparative


# --- 1. comparaison type de marchandise ---

df_marchandises <- data.frame(
  date = rep(data$date, 2),
  Volume = c(data$container_teu, data$bulk_liquid),
  Type = rep(c("Conteneurs (TEU)", "Vrac Liquide"), each = nrow(data))
)

g1 <- ggplot(df_marchandises, aes(x = date, y = Volume, color = Type)) +
  geom_line(alpha = 0.6) +
  labs(title = "Comparaison : Conteneurs vs Vrac Liquide", x = "", y = "Volume") +
  theme_minimal() +
  theme(legend.position = "bottom") 
print(g1)



# --- 2. Jours ouvrés vs Weekends ---
data$weekday <- factor(
  data$weekday,
  levels = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
)


g2 <- ggplot(data, aes(x = weekday, y = vessel_arrivals, fill = weekday)) +
  geom_boxplot() +
  labs(title = "Arrivées par jour de la semaine", x = "", y = "Nb Arrivées") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
print(g2)

# --- 3. Impact des Jours fériés ---
# Création de la colonne facteur avec ifelse standard
data$type_jour <- factor(ifelse(data$holiday_be == 1, "Férié", "Normal"))

g3 <- ggplot(data, aes(x = type_jour, y = vessel_arrivals, fill = type_jour)) +
  geom_boxplot() +
  labs(title = "Impact des jours fériés", x = "", y = "Nb Arrivées") +
  scale_fill_manual(values = c("red", "grey")) +
  theme_minimal() +
  theme(legend.position = "none")
print(g3)




# --- .Analyse Technique Détaillée : Décomposition et Saisonnalité ---



# Création de la semaine calendaire
data$week_id <- format(as.Date(data$date), "%Y-%U")
# Calcul moyenne et écart-type par semaine
stats_bb <- aggregate(
  vessel_arrivals ~ week_id,
  data = data,
  FUN = function(x) c(moy = mean(x), sd = sd(x))
)
# Mise en forme du tableau
tableau_bb <- data.frame(
  Semaine = stats_bb$week_id,
  Moyenne = stats_bb$vessel_arrivals[, "moy"],
  Ecart_Type = stats_bb$vessel_arrivals[, "sd"]
)
head(tableau_bb)

plot(
  tableau_bb$Moyenne,
  tableau_bb$Ecart_Type,
  pch = 16,
  xlab = "Moyenne hebdomadaire",
  ylab = "Écart-type hebdomadaire",
  main = "Méthode de Buys & Ballot"
)

abline(lm(Ecart_Type ~ Moyenne, data = tableau_bb), col = "red", lwd = 2)


# Calcul du a pour decider si modele multi ou adi

modele_bb <- lm(Ecart_Type ~ Moyenne, data = tableau_bb)
a <- coef(modele_bb)["Moyenne"]
a



# Application de la décomposition tendance-saisonnalité-erreur.



autoplot(ts_navires_hebdo) +
  ggtitle("Arrivées hebdomadaires de navires (2019–2024)") +
  xlab("Années") +
  ylab("Arrivées moyennes par semaine") +
  theme_bw()

# Première estimation de la tendance (moyenne mobile)


premiere_tendance <- ma(ts_navires_hebdo, order = 52)

# Série sans tendance


serie_sans_tendance <- ts_navires_hebdo - premiere_tendance

# Construction de la matrice saisonnière

matrice_saison <- matrix(
  serie_sans_tendance,
  ncol = 52,
  byrow = TRUE
)

# Construction de la matrice saisonnière

coeff_saisonniers <- colMeans(matrice_saison, na.rm = TRUE)

#Centrage
coeff_saisonniers_corr <- coeff_saisonniers - mean(coeff_saisonniers, na.rm = TRUE)
#Création de la saisonnalité complète St



saisonalite_vect <- rep(
  coeff_saisonniers_corr,
  length.out = length(ts_navires_hebdo)
)

saisonalite <- ts(
  saisonalite_vect,
  start = start(ts_navires_hebdo),
  frequency = 52
)



# Série désaisonnalisée 

serie_desaisonalisee <- ts_navires_hebdo - saisonalite

#Tendance finale
t <- 1:length(serie_desaisonalisee)

modele_tendance <- lm(serie_desaisonalisee ~ t + I(t^2))
tendance <- fitted(modele_tendance)

tendance_ts <- ts(
  tendance,
  start = start(ts_navires_hebdo),
  frequency = 52
)

# Erreur ou bruitt blanc
erreur <- ts_navires_hebdo - tendance_ts - saisonalite

ggAcf(erreur, na.action = na.pass)
gglagplot(erreur)
sd(erreur, na.rm = TRUE)



# --- 0) Sécuriser / préparer la série désaisonnalisée ---
# IMPORTANT : on enlève les NA dus à la moyenne mobile 
y_ts <- serie_desaisonalisee
y <- as.numeric(y_ts)
idx <- which(!is.na(y))      
y <- y[idx]
n <- length(y)

t <- 1:n
ones <- rep(1, n)
y_mat <- cbind(y)

# --- 1) Construire les matrices de régression 
T1 <- cbind(t, ones)                  # linéaire
T2 <- cbind(t^2, t, ones)             # quadratique
T3 <- cbind(t^3, t^2, t, ones)        # cubique

# --- 2) Estimation moindres carrés (formule du prof) ---
A1 <- solve(t(T1) %*% T1) %*% t(T1) %*% y_mat
A2 <- solve(t(T2) %*% T2) %*% t(T2) %*% y_mat
A3 <- solve(t(T3) %*% T3) %*% t(T3) %*% y_mat

# --- 3) Tendance estimée ---
trend1 <- T1 %*% A1
trend2 <- T2 %*% A2
trend3 <- T3 %*% A3

# --- 4) Remettre les tendances au format ts, alignées sur la série originale ---
trend1_full <- rep(NA, length(as.numeric(y_ts)))
trend2_full <- rep(NA, length(as.numeric(y_ts)))
trend3_full <- rep(NA, length(as.numeric(y_ts)))

trend1_full[idx] <- trend1
trend2_full[idx] <- trend2
trend3_full[idx] <- trend3

tendance1_ts <- ts(trend1_full, start = start(ts_navires_hebdo), frequency = 52)
tendance2_ts <- ts(trend2_full, start = start(ts_navires_hebdo), frequency = 52)
tendance3_ts <- ts(trend3_full, start = start(ts_navires_hebdo), frequency = 52)

# --- 5) Erreurs pour chaque modèle : erreur = Z - T - S ---
erreur1 <- ts_navires_hebdo - tendance1_ts - saisonalite
erreur2 <- ts_navires_hebdo - tendance2_ts - saisonalite
erreur3 <- ts_navires_hebdo - tendance3_ts - saisonalite

# --- 6) Indicateurs comparatifs ---
sd1 <- sd(erreur1, na.rm = TRUE)
sd2 <- sd(erreur2, na.rm = TRUE)
sd3 <- sd(erreur3, na.rm = TRUE)

comparaison <- data.frame(
  Modele = c("Lineaire (ordre 1)", "Quadratique (ordre 2)", "Cubique (ordre 3)"),
  SD_Erreur = c(sd1, sd2, sd3)
)
comparaison

# --- 7) Validation graphique (ACF + lagplot) ---
# ACF des erreurs (compare visuellement : le plus "plat" = meilleur)
ggAcf(erreur1, na.action = na.pass) + ggtitle("ACF erreurs - Lineaire")
ggAcf(erreur2, na.action = na.pass) + ggtitle("ACF erreurs - Quadratique")
ggAcf(erreur3, na.action = na.pass) + ggtitle("ACF erreurs - Cubique")

# Lag plots des erreurs
gglagplot(erreur1, lags = 7) + ggtitle("Lag plots erreurs - Lineaire")
gglagplot(erreur2, lags = 7) + ggtitle("Lag plots erreurs - Quadratique")
gglagplot(erreur3, lags = 7) + ggtitle("Lag plots erreurs - Cubique")






# --- Horizon de prévision (1 an = 52 semaines) ---
h <- 52

# Longueur de la série utilisée pour la régression
n <- length(which(!is.na(serie_desaisonalisee)))

# Temps futur
t_prev <- (n + 1):(n + h)
ones_prev <- rep(1, h)

# Matrice de régression cubique (comme pour l'estimation)
T3_prev <- cbind(t_prev^3, t_prev^2, t_prev, ones_prev)

# Prévision de la tendance
tendance_prev_vect <- T3_prev %*% A3

# Répétition de la saisonnalité hebdomadaire
saisonalite_prev_vect <- rep(coeff_saisonniers_corr, length.out = h)

# Prévision finale
prevision_vect <- tendance_prev_vect + saisonalite_prev_vect

# Passage en série temporelle
previsions_ts <- ts(
  prevision_vect,
  start = end(ts_navires_hebdo) + c(0,1),
  frequency = 52
)

# Série complète (observations + prévisions)
serie_avec_previsions <- ts(
  c(ts_navires_hebdo, previsions_ts),
  start = start(ts_navires_hebdo),
  frequency = 52
)

# Visualisation
autoplot(serie_avec_previsions) +
  ggtitle("Prévisions du nombre d'arrivées de navires") +
  xlab("Temps") + ylab("Arrivées de navires")














# Série ajustée (sans l'erreur)
serie_ajustee <- tendance3_ts + saisonalite
serie_ajustee_et_prev <- ts(
  c(serie_ajustee, previsions_ts),
  start = start(ts_navires_hebdo),
  frequency = 52
)





# Visualisation
# Version corrigée
autoplot(ts_navires_hebdo, series = "Série initiale") +
  autolayer(serie_ajustee_et_prev, series = "Série ajustée + prévisions 2025") +
  ggtitle("Série ajustée et prévisions pour 2025") +
  xlab("Année") +
  ylab("Valeur") +
  scale_colour_manual(
    values = c(
      "Série initiale" = "blue",
      "Série ajustée + prévisions 2025" = "orange"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )






