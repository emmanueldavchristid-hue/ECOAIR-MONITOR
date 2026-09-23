# 🌍 EcoAir Monitor

### Interactive Air Quality Analysis & Prediction Dashboard

**EcoAir Monitor** est une application interactive développée avec **R et Shiny**, dédiée à l'analyse, à la visualisation et à la prédiction de la qualité de l'air.

Le projet exploite des données de pollution atmosphérique provenant de cinq villes internationales : **Helsinki, Hong Kong, Kigali, Lima et Sofia**. Il permet d'étudier l'évolution des principaux polluants atmosphériques, d'identifier les périodes de dépassement des seuils de référence et d'explorer les relations entre pollution et conditions météorologiques.

L'application combine **analyse statistique, visualisation interactive, cartographie géographique et apprentissage automatique** au sein d'une même interface.

---

## 🎯 Objectifs

L'objectif principal d'EcoAir Monitor est de proposer un outil interactif permettant de :

* suivre l'évolution de la qualité de l'air dans plusieurs villes ;
* comparer les concentrations de différents polluants ;
* identifier les périodes présentant des niveaux de pollution élevés ;
* analyser la relation entre les conditions météorologiques et les concentrations de polluants ;
* appliquer des méthodes d'apprentissage automatique pour classifier les niveaux de pollution ;
* comparer les performances de plusieurs modèles prédictifs.

---

## 🏙️ Zones géographiques étudiées

L'application couvre cinq villes :

| Ville     | Pays      |
| --------- | --------- |
| Helsinki  | Finlande  |
| Hong Kong | Hong Kong |
| Kigali    | Rwanda    |
| Lima      | Pérou     |
| Sofia     | Bulgarie  |

---

## 🧪 Polluants étudiés

Trois principaux polluants atmosphériques sont analysés :

* **PM2.5** — particules fines de diamètre inférieur ou égal à 2,5 µm ;
* **NO₂** — dioxyde d'azote ;
* **O₃** — ozone.

Les concentrations observées peuvent être comparées aux seuils de référence retenus dans le cadre du projet afin d'identifier les situations de dépassement.

---

## 📊 Fonctionnalités

### Vue d'ensemble

Cette section présente une synthèse des données disponibles à travers :

* les moyennes des principaux polluants ;
* des indicateurs statistiques ;
* un tableau récapitulatif ;
* une comparaison entre les différentes villes.

### 📈 Séries temporelles

Cette fonctionnalité permet d'étudier l'évolution des concentrations de polluants au cours du temps.

L'utilisateur peut sélectionner le polluant et explorer son évolution pour les différentes villes étudiées.

### 📦 Distributions

Des boxplots permettent de comparer la distribution des concentrations de polluants entre les villes.

Cette représentation facilite notamment l'identification :

* des niveaux centraux ;
* de la dispersion ;
* des valeurs atypiques ;
* des différences entre zones géographiques.

### 🗺️ Cartographie interactive

Une carte interactive développée avec **Leaflet** permet de visualiser spatialement les niveaux de pollution.

Les couleurs utilisées permettent d'identifier rapidement les différents niveaux de concentration observés.

### ⚠️ Alertes

Cette section identifie les périodes durant lesquelles les concentrations mesurées dépassent les seuils de référence retenus.

Elle permet notamment de suivre le nombre de journées concernées pour chaque polluant et chaque ville.

### 🤖 Prédictions IA

L'application intègre plusieurs algorithmes de classification :

* **Random Forest** ;
* **Support Vector Machine (SVM)** ;
* **Naive Bayes**.

Les modèles sont utilisés pour prédire les niveaux de pollution à partir des variables disponibles.

La section présente également :

* les prédictions des modèles ;
* les matrices de confusion ;
* l'importance des variables pour les modèles concernés.

### 📐 Prédictions linéaires

Cette partie étudie la relation entre la température et les concentrations de polluants à l'aide de modèles linéaires.

L'application permet notamment d'estimer la concentration attendue d'un polluant pour une température de **20 °C**.

### 🏆 Performance des modèles

Les différents modèles de classification sont comparés à travers plusieurs indicateurs :

| Métrique  | Description                                      |
| --------- | ------------------------------------------------ |
| Accuracy  | Proportion globale de classifications correctes  |
| Precision | Proportion de prédictions positives correctes    |
| Recall    | Capacité à identifier les observations positives |
| F1-score  | Compromis entre Precision et Recall              |

Cette comparaison permet d'analyser le comportement des différents algorithmes selon plusieurs critères de performance.

---

## 🛠️ Technologies utilisées

### Langage

* **R**

### Framework

* **Shiny**

### Visualisation

* **ggplot2**
* **Leaflet**
* **grid / gridExtra** selon les composants utilisés

### Manipulation des données

* **dplyr**
* **tidyr**
* **readr**

### Machine Learning

* **Random Forest**
* **SVM**
* **Naive Bayes**

---

## 📁 Structure du projet

```text
ECOAIR-MONITOR/
│
├── app.R
├── 1.R
├── READ_ME.txt
├── .gitignore
│
├── projet/
│   ├── app.R
│   └── projet.Rproj
│
├── 2011.csv
├── 2012.csv
├── 2013.csv
├── 2014.csv
└── 2015.csv
```

> Les fichiers de données brutes ne sont pas versionnés dans le dépôt GitHub en raison de leur taille importante. Ils restent nécessaires à l'exécution complète de l'application et doivent être placés dans le répertoire approprié avant le lancement.

---

## 🚀 Lancement de l'application

Après avoir installé R et les packages nécessaires, ouvrir le projet dans RStudio puis exécuter :

```r
shiny::runApp()
```

ou lancer directement le fichier :

```text
app.R
```

---

## 📚 Contexte académique

Projet réalisé dans le cadre du **Master 2 Data Science – Visualisation de données**, avec pour objectif de mettre en pratique les techniques de préparation, d'analyse et de visualisation interactive des données ainsi que plusieurs méthodes d'apprentissage automatique.

Le projet met particulièrement l'accent sur l'utilisation de **Shiny** pour construire une interface interactive permettant de combiner exploration statistique, visualisation géographique et modèles prédictifs.

---

## 👨‍💻 Auteur

**Emmanuel-Christ**

Master 2 Data Science

---

## 📌 Résumé

**EcoAir Monitor** propose une approche intégrée de l'analyse de la qualité de l'air en combinant exploration statistique, visualisation interactive, analyse géographique et apprentissage automatique.

L'application permet ainsi de passer de l'exploration des données à l'identification des situations de pollution et à l'évaluation de modèles capables de prédire les niveaux de pollution à partir des informations disponibles.
