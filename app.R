# ============================================================
# PharmaExplorer — Mini-projet Shiny 
# Features : modules, reactive values, shinyjs, DT, plotly,
#            upload CSV, renderUI dynamique, observer
# Données   : essais cliniques simulés (diabète / cardio)
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(readr)

# 1. DONNÉES SIMULÉES

set.seed(42)
n <- 300

generate_data <- function() {
  tibble(
    patient_id   = paste0("PT-", sprintf("%04d", 1:n)),
    age          = round(rnorm(n, 58, 12)),
    sexe         = sample(c("Homme", "Femme"), n, replace = TRUE),
    indication   = sample(c("Diabète T2", "Insuffisance cardiaque",
                            "Hypertension", "Dyslipidémie"), n,
                          replace = TRUE, prob = c(0.35, 0.25, 0.25, 0.15)),
    traitement   = sample(c("Bras A", "Bras B", "Placebo"), n,
                          replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    dose_mg      = sample(c(25, 50, 100, 200), n, replace = TRUE),
    duree_j      = round(runif(n, 30, 365)),
    hba1c_init   = round(rnorm(n, 8.2, 1.4), 1),
    hba1c_final  = round(rnorm(n, 7.1, 1.2), 1),
    pression_sys = round(rnorm(n, 138, 18)),
    pression_dia = round(rnorm(n, 84, 11)),
    ldl_init     = round(rnorm(n, 3.4, 0.9), 1),
    ldl_final    = round(rnorm(n, 2.8, 0.8), 1),
    evenement_cv = sample(c(0, 1), n, replace = TRUE, prob = c(0.82, 0.18)),
    score_eff    = round(runif(n, 1, 10), 1),
    score_tol    = round(runif(n, 1, 10), 1),
    site         = sample(paste("Centre", LETTERS[1:6]), n, replace = TRUE),
    statut       = sample(c("Complété", "Abandonné", "En cours"),
                          n, replace = TRUE, prob = c(0.70, 0.15, 0.15))
  ) |>
    mutate(
      delta_hba1c = hba1c_final - hba1c_init,
      delta_ldl   = ldl_final - ldl_init,
      age_cat     = cut(age, breaks = c(0, 50, 65, 120),
                        labels = c("<50 ans", "50-65 ans", ">65 ans"))
    )
}

base_data <- generate_data()

# 2. MODULE : KPI CARDS

kpiUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("cards"))
}

kpiServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$cards <- renderUI({
      d <- data()
      n_pts      <- nrow(d)
      pct_comp   <- round(mean(d$statut == "Complété") * 100)
      delta_hba  <- round(mean(d$delta_hba1c, na.rm = TRUE), 2)
      pct_ev     <- round(mean(d$evenement_cv) * 100)
      
      make_card <- function(val, label, color, icon_name) {
        div(class = paste("kpi-card kpi-", color, sep = ""),
            div(class = "kpi-icon", icon(icon_name)),
            div(class = "kpi-val",  val),
            div(class = "kpi-lbl",  label)
        )
      }
      
      div(class = "kpi-row",
          make_card(n_pts,              "Patients",            "blue",   "users"),
          make_card(paste0(pct_comp, "%"), "Taux complétion",  "green",  "check-circle"),
          make_card(delta_hba,          "ΔHbA1c moyen",       "purple", "heartbeat"),
          make_card(paste0(pct_ev, "%"), "Événements CV",     "orange", "exclamation-triangle")
      )
    })
  })
}

# 3. MODULE : GRAPHIQUE EFFICACITÉ

effUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("plot"), height = "380px")
}

effServer <- function(id, data, xvar, yvar, color_var) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      d <- data()
      req(nrow(d) > 0)
      
      p <- ggplot(d, aes_string(x = xvar(), y = yvar(), color = color_var())) +
        geom_point(alpha = 0.65, size = 2.2) +
        geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, alpha = 0.15) +
        scale_color_manual(
          values = c("Bras A" = "#0070C0", "Bras B" = "#00B050",
                     "Placebo" = "#FF6B35", "Homme" = "#2196F3",
                     "Femme" = "#E91E93", "Diabète T2" = "#9C27B0",
                     "Insuffisance cardiaque" = "#F44336",
                     "Hypertension" = "#FF9800", "Dyslipidémie" = "#4CAF50")
        ) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              legend.position = "bottom")
      
      ggplotly(p, tooltip = c("x", "y", "colour")) |>
        layout(legend = list(orientation = "h", y = -0.2))
    })
  })
}

# 4. MODULE : TABLEAU DT

tableUI <- function(id) {
  ns <- NS(id)
  DTOutput(ns("tbl"))
}

tableServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- renderDT({
      d <- data() |>
        select(patient_id, age, sexe, indication, traitement, dose_mg,
               duree_j, hba1c_init, hba1c_final, delta_hba1c,
               evenement_cv, statut, site)
      
      datatable(
        d,
        filter     = "top",
        extensions = c("Buttons", "Scroller"),
        options    = list(
          dom        = "Bfrtip",
          buttons    = list("copy", "csv", "excel"),
          scrollY    = "340px",
          scroller   = TRUE,
          pageLength = 50,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE
      ) |>
        formatStyle("delta_hba1c",
                    backgroundColor = styleInterval(c(-2, 0),
                                                    c("#c8e6c9", "#fff9c4", "#ffcdd2"))
        ) |>
        formatStyle("evenement_cv",
                    backgroundColor = styleEqual(c(0, 1), c("white", "#ffcdd2"))
        )
    })
  })
}

# 5. UI PRINCIPALE

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("

    /* ── Global ── */
    body { font-family: 'Inter', 'Segoe UI', sans-serif;
           background: #F0F4F8; color: #1a2332; }

    /* ── Header ── */
    .app-header {
      background: linear-gradient(135deg, #003087 0%, #0070C0 60%, #00A8E8 100%);
      color: white; padding: 18px 32px 14px; margin-bottom: 24px;
      border-radius: 0 0 12px 12px;
      box-shadow: 0 4px 20px rgba(0,48,135,.25);
    }
    .app-header h2 { margin: 0; font-size: 1.6rem; font-weight: 700;
                     letter-spacing: -.3px; }
    .app-header p  { margin: 4px 0 0; opacity: .82; font-size: .9rem; }

    /* ── KPI Cards ── */
    .kpi-row { display: flex; gap: 14px; flex-wrap: wrap; margin-bottom: 22px; }
    .kpi-card {
      flex: 1; min-width: 140px; border-radius: 10px;
      padding: 16px 18px; text-align: center;
      background: white; box-shadow: 0 2px 10px rgba(0,0,0,.08);
      transition: transform .15s;
    }
    .kpi-card:hover { transform: translateY(-3px); }
    .kpi-icon { font-size: 1.4rem; margin-bottom: 6px; }
    .kpi-val  { font-size: 1.9rem; font-weight: 800; }
    .kpi-lbl  { font-size: .78rem; text-transform: uppercase;
                letter-spacing: .6px; opacity: .7; margin-top: 2px; }
    .kpi-blue   .kpi-icon, .kpi-blue   .kpi-val { color: #0070C0; }
    .kpi-green  .kpi-icon, .kpi-green  .kpi-val { color: #2e7d32; }
    .kpi-purple .kpi-icon, .kpi-purple .kpi-val { color: #6a1b9a; }
    .kpi-orange .kpi-icon, .kpi-orange .kpi-val { color: #e65100; }

    /* ── Panels ── */
    .panel-box {
      background: white; border-radius: 10px;
      padding: 20px 22px; margin-bottom: 18px;
      box-shadow: 0 2px 10px rgba(0,0,0,.07);
    }
    .panel-title {
      font-size: 1rem; font-weight: 700; color: #003087;
      border-left: 4px solid #0070C0; padding-left: 10px;
      margin-bottom: 14px;
    }

    /* ── Sidebar ── */
    .well { background: white; border: none; border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,.07); }

    /* ── Tabs ── */
    .nav-tabs > li.active > a {
      color: #003087 !important; border-bottom: 3px solid #0070C0 !important;
      font-weight: 700;
    }

    /* ── Upload zone ── */
    .upload-info { background: #e3f2fd; border-radius: 8px;
                   padding: 10px 14px; font-size: .85rem; color: #01579b;
                   margin-top: 8px; display: none; }

    /* ── Alert banner ── */
    .alert-banner {
      background: #fff3e0; border-left: 4px solid #ff6d00;
      border-radius: 6px; padding: 10px 14px; font-size: .85rem;
      color: #bf360c; margin-bottom: 14px; display: none;
    }

    /* ── Download button ── */
    .btn-dl { background: #003087; color: white; border: none;
              border-radius: 6px; padding: 7px 16px;
              font-size: .85rem; cursor: pointer; }
    .btn-dl:hover { background: #0070C0; color: white; }

  "))),
  
  # Header
  div(class = "app-header",
      h2(icon("flask"), " PharmaExplorer"),
      p("Dashboard d'analyse d'essai clinique — Données simulées (Diabète · Cardio · HTA · Dyslipidémie)")
  ),
  
  # Alert (shinyjs)
  div(id = "alert_banner", class = "alert-banner",
      icon("exclamation-circle"), " Moins de 20 patients correspondent aux filtres sélectionnés."
  ),
  
  sidebarLayout(
    
    # ── SIDEBAR ──────────────────────────────────────────────
    sidebarPanel(width = 3,
                 
                 div(class = "panel-title", "Filtres"),
                 
                 selectInput("indication", "Indication",
                             choices  = c("Toutes", "Diabète T2", "Insuffisance cardiaque",
                                          "Hypertension", "Dyslipidémie"),
                             selected = "Toutes"
                 ),
                 
                 checkboxGroupInput("traitement", "Bras de traitement",
                                    choices  = c("Bras A", "Bras B", "Placebo"),
                                    selected = c("Bras A", "Bras B", "Placebo")
                 ),
                 
                 sliderInput("age_range", "Tranche d'âge",
                             min = 18, max = 90, value = c(18, 90)
                 ),
                 
                 sliderInput("duree_range", "Durée (jours)",
                             min = 30, max = 365, value = c(30, 365)
                 ),
                 
                 checkboxGroupInput("statut", "Statut patient",
                                    choices  = c("Complété", "Abandonné", "En cours"),
                                    selected = c("Complété", "Abandonné", "En cours")
                 ),
                 
                 hr(),
                 
                 # Upload CSV
                 div(class = "panel-title", "Importer vos données"),
                 fileInput("upload", NULL,
                           accept      = c(".csv", "text/csv"),
                           placeholder = "Choisir un CSV…",
                           buttonLabel = icon("upload")
                 ),
                 div(id = "upload_info", class = "upload-info",
                     icon("check-circle"), textOutput("upload_msg", inline = TRUE)
                 ),
                 
                 hr(),
                 
                 actionButton("reset_filters", "Réinitialiser les filtres",
                              icon  = icon("rotate-left"),
                              class = "btn-dl",
                              style = "width:100%"
                 )
    ),
    
    # ── MAIN PANEL ───────────────────────────────────────────
    mainPanel(width = 9,
              
              kpiUI("kpi"),
              
              tabsetPanel(id = "tabs",
                          
                          # Tab 1 : Exploration graphique
                          tabPanel("Exploration",
                                   div(class = "panel-box",
                                       div(class = "panel-title", "Variables à visualiser"),
                                       fluidRow(
                                         column(4, selectInput("xvar", "Axe X",
                                                               choices = c("age", "duree_j", "hba1c_init", "delta_hba1c",
                                                                           "score_eff", "score_tol", "ldl_init", "delta_ldl",
                                                                           "pression_sys"),
                                                               selected = "age"
                                         )),
                                         column(4, selectInput("yvar", "Axe Y",
                                                               choices = c("delta_hba1c", "score_eff", "score_tol",
                                                                           "hba1c_final", "delta_ldl", "pression_sys",
                                                                           "pression_dia", "duree_j"),
                                                               selected = "delta_hba1c"
                                         )),
                                         column(4, selectInput("color_var", "Couleur par",
                                                               choices  = c("traitement", "sexe", "indication"),
                                                               selected = "traitement"
                                         ))
                                       )
                                   ),
                                   div(class = "panel-box",
                                       div(class = "panel-title", "Nuage de points interactif"),
                                       effUI("eff_plot")
                                   ),
                                   fluidRow(
                                     column(6, div(class = "panel-box",
                                                   div(class = "panel-title", "Distribution par indication"),
                                                   plotlyOutput("bar_indication", height = "280px")
                                     )),
                                     column(6, div(class = "panel-box",
                                                   div(class = "panel-title", "Événements CV par bras"),
                                                   plotlyOutput("bar_ev_cv", height = "280px")
                                     ))
                                   )
                          ),
                          
                          # Tab 2 : Tableau patients
                          tabPanel("Tableau patients",
                                   div(class = "panel-box",
                                       div(class = "panel-title", "Données individuelles filtrées"),
                                       div(style = "margin-bottom:10px",
                                           downloadButton("dl_csv", "Télécharger CSV", class = "btn-dl")
                                       ),
                                       tableUI("tbl_patients")
                                   )
                          ),
                          
                          # Tab 3 : Analyse dose-réponse
                          tabPanel("Dose-réponse",
                                   div(class = "panel-box",
                                       div(class = "panel-title", "Efficacité selon la dose"),
                                       fluidRow(
                                         column(6, plotlyOutput("dose_eff",  height = "320px")),
                                         column(6, plotlyOutput("dose_hba1c",height = "320px"))
                                       )
                                   ),
                                   div(class = "panel-box",
                                       div(class = "panel-title", "Tolérance selon la dose"),
                                       plotlyOutput("dose_tol", height = "300px")
                                   )
                          ),
                          
                          # Tab 4 : Résumé statistique (renderUI dynamique)
                          tabPanel("Résumé",
                                   div(class = "panel-box",
                                       div(class = "panel-title", "Tableau récapitulatif par bras"),
                                       uiOutput("resume_table")
                                   ),
                                   div(class = "panel-box",
                                       div(class = "panel-title", "Boxplots comparatifs"),
                                       selectInput("box_var", NULL,
                                                   choices = c("delta_hba1c", "score_eff", "score_tol",
                                                               "duree_j", "pression_sys"),
                                                   selected = "delta_hba1c", width = "220px"
                                       ),
                                       plotlyOutput("boxplot_comp", height = "320px")
                                   )
                          )
                          
              ) # end tabsetPanel
    ) # end mainPanel
  ) # end sidebarLayout
)

# 6. SERVER 

server <- function(input, output, session) {
  
  # ── Données réactives (upload ou base) ──────────────────
  raw_data <- reactiveVal(base_data)
  
  observeEvent(input$upload, {
    req(input$upload)
    tryCatch({
      df <- read_csv(input$upload$datapath, show_col_types = FALSE)
      # Validation minimale
      needed <- c("age", "sexe", "indication", "traitement", "statut")
      if (!all(needed %in% names(df))) {
        showNotification("CSV incompatible : colonnes manquantes.", type = "error")
        return()
      }
      raw_data(df)
      shinyjs::show("upload_info")
      output$upload_msg <- renderText({
        paste(nrow(df), "patients importés depuis", input$upload$name)
      })
      showNotification("Données importées avec succès !", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur lecture :", e$message), type = "error")
    })
  })
  
  # ── Filtrage réactif ────────────────────────────────────
  filtered <- reactive({
    d <- raw_data()
    
    if (input$indication != "Toutes")
      d <- d |> filter(indication == input$indication)
    
    d <- d |>
      filter(
        traitement %in% input$traitement,
        age        >= input$age_range[1], age <= input$age_range[2],
        duree_j    >= input$duree_range[1], duree_j <= input$duree_range[2],
        statut     %in% input$statut
      )
    d
  })
  
  # ── Observer : alerte petit effectif (shinyjs) ──────────
  observe({
    d <- filtered()
    if (nrow(d) < 20) {
      shinyjs::show("alert_banner")
    } else {
      shinyjs::hide("alert_banner")
    }
  })
  
  # ── Reset filtres ────────────────────────────────────────
  observeEvent(input$reset_filters, {
    updateSelectInput(session,        "indication",  selected = "Toutes")
    updateCheckboxGroupInput(session, "traitement",  selected = c("Bras A","Bras B","Placebo"))
    updateSliderInput(session,        "age_range",   value    = c(18, 90))
    updateSliderInput(session,        "duree_range", value    = c(30, 365))
    updateCheckboxGroupInput(session, "statut",      selected = c("Complété","Abandonné","En cours"))
    shinyjs::hide("upload_info")
  })
  
  # ── Modules ─────────────────────────────────────────────
  kpiServer("kpi", filtered)
  
  effServer("eff_plot", filtered,
            xvar      = reactive(input$xvar),
            yvar      = reactive(input$yvar),
            color_var = reactive(input$color_var)
  )
  
  tableServer("tbl_patients", filtered)
  
  # ── Bar : indication ─────────────────────────────────────
  output$bar_indication <- renderPlotly({
    d <- filtered() |> count(indication)
    plot_ly(d, x = ~indication, y = ~n, type = "bar",
            marker = list(color = c("#0070C0","#00B050","#FF6B35","#9C27B0"))) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "N"),
             margin = list(b = 60))
  })
  
  # ── Bar : événements CV ──────────────────────────────────
  output$bar_ev_cv <- renderPlotly({
    d <- filtered() |>
      group_by(traitement) |>
      summarise(pct_ev = round(mean(evenement_cv) * 100, 1))
    plot_ly(d, x = ~traitement, y = ~pct_ev, type = "bar",
            marker = list(color = c("#0070C0","#00B050","#FF6B35"))) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "% événements CV"),
             margin = list(b = 40))
  })
  
  # ── Dose-réponse ─────────────────────────────────────────
  output$dose_eff <- renderPlotly({
    d <- filtered() |>
      group_by(dose_mg) |>
      summarise(eff = mean(score_eff), sd = sd(score_eff))
    plot_ly(d, x = ~dose_mg, y = ~eff, type = "scatter", mode = "lines+markers",
            error_y = list(type = "data", array = ~sd, visible = TRUE),
            line = list(color = "#0070C0")) |>
      layout(title = "Score efficacité",
             xaxis = list(title = "Dose (mg)"), yaxis = list(title = "Score (1–10)"))
  })
  
  output$dose_hba1c <- renderPlotly({
    d <- filtered() |>
      group_by(dose_mg) |>
      summarise(delta = mean(delta_hba1c, na.rm = TRUE))
    plot_ly(d, x = ~dose_mg, y = ~delta, type = "bar",
            marker = list(color = ifelse(d$delta < 0, "#2e7d32", "#c62828"))) |>
      layout(title = "ΔHbA1c moyen",
             xaxis = list(title = "Dose (mg)"), yaxis = list(title = "ΔHbA1c"))
  })
  
  output$dose_tol <- renderPlotly({
    d <- filtered() |>
      group_by(dose_mg) |>
      summarise(tol = mean(score_tol), sd = sd(score_tol))
    plot_ly(d, x = ~dose_mg, y = ~tol, type = "scatter", mode = "lines+markers",
            error_y = list(type = "data", array = ~sd, visible = TRUE),
            line = list(color = "#e65100")) |>
      layout(xaxis = list(title = "Dose (mg)"), yaxis = list(title = "Score tolérance (1–10)"))
  })
  
  # ── Résumé dynamique (renderUI) ──────────────────────────
  output$resume_table <- renderUI({
    d <- filtered()
    if (nrow(d) == 0) return(p("Aucune donnée."))
    
    summary_df <- d |>
      group_by(traitement) |>
      summarise(
        N           = n(),
        `ΔHbA1c`   = round(mean(delta_hba1c, na.rm = TRUE), 2),
        `Score eff` = round(mean(score_eff),  1),
        `Score tol` = round(mean(score_tol),  1),
        `% CV`      = paste0(round(mean(evenement_cv)*100, 1), "%"),
        `% Complété`= paste0(round(mean(statut == "Complété")*100, 1), "%"),
        .groups = "drop"
      )
    
    rows <- apply(summary_df, 1, function(r) {
      tags$tr(lapply(r, tags$td))
    })
    
    tags$table(class = "table table-hover table-sm",
               tags$thead(tags$tr(lapply(names(summary_df), function(h) tags$th(h)))),
               tags$tbody(rows)
    )
  })
  
  # ── Boxplot comparatif ───────────────────────────────────
  output$boxplot_comp <- renderPlotly({
    d    <- filtered()
    var  <- input$box_var
    colors <- c("Bras A" = "#0070C0", "Bras B" = "#00B050", "Placebo" = "#FF6B35")
    
    traces <- lapply(unique(d$traitement), function(tr) {
      vals <- d[[var]][d$traitement == tr]
      list(type = "box", y = vals, name = tr,
           marker = list(color = colors[tr]))
    })
    
    plotly_empty() |>
      add_trace(type = "box") |>
      { p <- plot_ly(); for (t in traces) p <- add_trace(p, type=t$type, y=t$y, name=t$name, marker=t$marker); p }() |>
      layout(yaxis = list(title = var), showlegend = TRUE)
  })
  
  # ── Download ─────────────────────────────────────────────
  output$dl_csv <- downloadHandler(
    filename = function() paste0("pharma_export_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(filtered(), file)
  )
  
}

# 7. LANCEMENT
shinyApp(ui, server)