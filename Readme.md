# Diabeto-app — Clinical Trial Explorer

> Dashboard Shiny interactif pour l'analyse d'essais cliniques en diabétologie et maladies cardiovasculaires.  
> Mini-projet réalisé dans le cadre d'une préparation à une Aternance Data Science en industrie pharmaceutique.

---

## Fonctionnalités

| Feature | Détail |
|---|---|
| **Filtres dynamiques** | Indication, bras de traitement, âge, durée, statut patient |
| **KPI cards réactives** | N patients, taux de complétion, ΔHbA1c moyen, % événements CV |
| **Plotly interactif** | Scatter, barplots, dose-réponse avec barres d'erreur, boxplots |
| **Tableau DT** | Filtres par colonne, export CSV/Excel, coloration conditionnelle |
| **Upload CSV** | Import de données externes avec validation et gestion d'erreurs |
| **Export** | Téléchargement de la sélection filtrée en CSV |
| **UI dynamique** | `renderUI` pour tableau récapitulatif par bras de traitement |
| **shinyjs** | Bannière d'alerte si effectif < 20, show/hide de composants |

---

## Architecture

```
Diabeto-app/
├── app.R          # UI + Server + Modules Shiny
└── README.md
```

### Modules Shiny utilisés

```
app.R
├── Module : kpiUI / kpiServer        → KPI cards dynamiques
├── Module : effUI / effServer        → Graphique d'efficacité (plotly)
└── Module : tableUI / tableServer    → Tableau DT interactif
```

---

## Données

Les données sont **entièrement simulées** (`set.seed(42)`, n = 300 patients) et couvrent 4 indications :

- Diabète de type 2
- Insuffisance cardiaque
- Hypertension
- Dyslipidémie

Variables disponibles : âge, sexe, bras de traitement, dose, durée, HbA1c (init/final), LDL, pression artérielle, score d'efficacité/tolérance, événements cardiovasculaires, statut de complétion.

---

## Installation & lancement

```r
# 1. Installer les dépendances
install.packages(c(
  "shiny", "shinydashboard", "shinyjs",
  "DT", "plotly", "dplyr", "ggplot2", "readr"
))

# 2. Lancer l'app
shiny::runApp("app.R")
```

---

## Concepts Shiny illustrés

- `reactive()` / `reactiveVal()` / `reactiveValues()`
- `observe()` / `observeEvent()` / `eventReactive()`
- `renderUI()` + `uiOutput()` pour UI dynamique
- `NS()` / `moduleServer()` pour la modularisation
- `shinyjs::show()` / `hide()` / `disable()`
- `fileInput()` + `downloadHandler()`
- `DT::datatable()` avec extensions Buttons & Scroller
- `plotly` natif + `ggplotly()`

---

## Auteur

**Amel Amyay** — M2 Data Science en Santé, Université de Lille (Promo 2026)  
