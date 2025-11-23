# Économétrie financière — Analyse de STMicroelectronics (STM) vs CAC 40

## 🎯 Objectif
Application des outils d’économétrie financière à l'étude des rendements journaliers de l’action **STMicroelectronics (STM)** et de l’indice **CAC 40** sur la période **02/06/2023 – 06/06/2025**.  
Le projet couvre le traitement des données, la vérification des faits stylisés, l’estimation du modèle **MEDAF/CAPM**, la modélisation de la volatilité via **GARCH(1,1)** et des prévisions à court terme.

---

## 📂 Structure du dépôt

### `reports/`
Documents finaux :
- `rapport.pdf`
- `synthese.pdf`

### `data/`
Contient les données brutes :
- `stm_prices.csv`
- `cac40_prices.csv`
- metadata.txt (source, fréquence, période)

### `Code/`
- MicroElectronic.R

### Fichiers principaux
- `README.md` — documentation du projet  
- `requirements.txt` — packages R à installer  

---

## 🔬 Méthodologie (résumé)

### 1. Préparation des données
- Importation depuis Euronext  
- Nettoyage des séries  
- Choix du type de prix  
- Calcul des rendements log  
- Tests de stationnarité (ADF)

### 2. Faits stylisés
- Distribution des rendements  
- Analyse ACF/PACF  
- Tests d’effet ARCH  
- Kurtosis, volatilité en clusters

### 3. Modèle MEDAF / CAPM
- Régression OLS  
- Estimation du bêta  
- Analyse des résidus : autocorrélation, hétéroscédasticité

### 4. Modèle GARCH(1,1)
- Estimation pour STM et CAC 40  
- Analyse de la persistance de volatilité  
- Interprétation économétrique

### 5. Prévisions (25 jours)
- Prévision des rendements  
- Prévision de la volatilité conditionnelle

---

## 📈 Résultats principaux (synthèse)

- Le CAPM s’ajuste correctement ; les résultats suggèrent une relation significative entre STM et le marché (CAC 40).  
- Le CAC 40 affiche une volatilité relativement stable.  
- STM présente une volatilité plus élevée et plus persistante.  
- Les prévisions montrent une volatilité anticipée croissante à court terme pour STM, avec des rendements moyens proches de zéro.

---

## ⚙️ Installation des dépendances (R)
Ce projet utilise les packages suivants :

`tseries`, `forecast`, `seastests`, `astsa`, `zoo`, `lmtest`, `TTR`, `psych`, `rugarch`

Installation en une commande :

```r
install.packages(c("tseries","forecast","seastests","astsa","zoo","lmtest","TTR","psych","rugarch"))



🔁 Reproduire l’analyse
1. Cloner le dépôt

git clone https://github.com/VOTRE-USERNAME/Econometrie-Financiere-STM-CAC40.git
cd Econometrie-Financiere-STM-CAC40

2. Installer les dépendances R

📚 Pistes d’amélioration

    Estimation de modèles GARCH asymétriques (EGARCH, TGARCH)

    Modèles multivariés (DCC-GARCH)

    Analyse du risque : VaR, Expected Shortfall

    Simulation de scénarios extrêmes

📄 Licence

Projet distribué sous licence MIT.
✉️ Contact




