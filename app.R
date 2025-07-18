############################################################################################################################################
#
# Cruscotto di monitoraggio PNRR-Giustizia.
# Questo codice implementa la parte client del cruscotto.
# I dati di input sono caricati in locale e sono ottenuti a partire dal file crea_storico.R (nella cartella 'code' del progetto 018_PNRR).
#
# @Paolo Fantini - Ufficio statistico CSM
# Giugno 2025
#
############################################################################################################################################

# 0. SETUP -------------------------------------------------------------------

invisible(sapply(list.files("functions", full.names = T, recursive = T), function(x) source(x, encoding = "UTF-8")))
library(shinydashboard)
load_packages()
load("data/sedi.RData")
distretti <- sedi[, .N, keyby = Distretto][, Distretto]
uffici    <- sedi[, .(`Tipo Ufficio`, Ufficio)][order(`Tipo Ufficio`, Ufficio)]

# 1. HEADER AND SIDEBAR MENU --------------------------------------------------

ui <-
  
  header <- header()

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sb_menu",
    menuItem("Informazioni", tabName = "info", icon = icon("info")),
    menuItem(
      "DT civile",
      tabName = "DT_civile",
      icon = icon("clock"),
      menuSubItem("Complessivo",                     tabName = "DT_complessivo_civ"),
      menuSubItem("Nazionale (totale)",              tabName = "DT_nazionale_civ"),
      menuSubItem("Nazionale (vista per distretti)", tabName = "DT_all_dis_civ"),
      menuSubItem("Distretto (totale)",              tabName = "DT_per_distretto_civ"),
      menuSubItem("Distretto (vista per uffici)",    tabName = "DT_all_uffs_civ"),
      menuSubItem("Ufficio",                         tabName = "DT_per_ufficio_civ")
    ),
    menuItem(
      "DT penale",
      tabName = "DT_penale",
      icon = icon("clock"),
      menuSubItem("Complessivo", tabName = "DT_complessivo_pen"),
      menuSubItem("Nazionale",   tabName = "DT_nazionale_pen"),
      menuSubItem("Distretto",   tabName = "DT_per_distretto_pen"),
      menuSubItem("Ufficio",     tabName = "DT_per_ufficio_pen")
    ),
    menuItem(
      "AR civile (target 2024)",
      tabName = "AR_civile",
      icon = icon("layer-group"),  # stack-overflow
      #menuSubItem("AR (tutti i distretti)", tabName = "AR_tutti_i_distretti_tg24"),
      menuSubItem("Nazionale", tabName = "AR_nazionale_tg24"),
      menuSubItem("Distretto", tabName = "AR_per_distretto_tg24"),
      menuSubItem("Ufficio",   tabName = "AR_per_ufficio_tg24")
      #menuSubItem("Arretrato vs Turnover",  tabName = "AR_vs_TO_MAG")
    ),
    menuItem(
      "AR civile (target 2026)",
      tabName = "AR_civile",
      icon = icon("layer-group"),
      #menuSubItem("AR (tutti i distretti)", tabName = "AR_tutti_i_distretti_tg26"),
      menuSubItem("Nazionale", tabName = "AR_nazionale_tg26"),
      menuSubItem("Distretto", tabName = "AR_per_distretto_tg26"),
      menuSubItem("Ufficio",   tabName = "AR_per_ufficio_tg26")
      #menuSubItem("Arretrato vs Turnover",  tabName = "AR_vs_TO_MAG")
    ),
    menuItem(
      "Flussi civile",
      tabName = "Fls_civile",
      icon = icon("stack-overflow"),
      menuSubItem("Nazionale", tabName = "Flussi_nazionale_civ"),
      menuSubItem("Distretto", tabName = "Flussi_per_distretto_civ"),
      menuSubItem("Ufficio",   tabName = "Flussi_per_ufficio_civ")
    ),
    menuItem(
      "Download",
      tabName = "Download",
      icon = icon("table"),
      #menuSubItem("AR civile - dati sezionali",  tabName = "AR_dati_sezionali"),
      menuSubItem("DT civile - codici oggetto ", tabName = "DT_cdx_oggetto_civ"),
      menuSubItem("AR civile - codici oggetto ", tabName = "AR_cdx_oggetto_civ")
    )
  )
)

# 2. BODY --------------------------------------------------------------------

body <- dashboardBody(
  
  tabItems(
    
    # Menu Item: Info ---------------------------------------------------------------
    
    tabItem(
      tabName = "info",
      fluidRow(
        # box(
        #   title = header_info(),
        #   width = 12,
        #   status = "primary",
        #   headerBorder = T
        # ),
        box(
          title = header_info(),
          width = 12,
          #height = "100px",
          collapsible = T,
          status = "primary",
          timeline()
        ),
        box(
          title = span("Guida alla lettura dei grafici", style = "color:gray"),
          width = 12,
          collapsible = T,
          collapsed = T,
          status = "warning",
          guida()
        ),
        box(
          title = span("Analisi what-if", style = "color:gray"),
          width = 12,
          collapsible = T,
          collapsed = T,
          status = "warning",
          whatif()
        ),
        box(
          title = span("Approfondimenti sugli indicatori", style = "color:gray"),
          width = 12,
          status = "warning",
          collapsible = T,
          collapsed = T,
          note()
        ),
        box(
          title = span("Fonti", style = "color:gray"),
          width = 12,
          status = "warning",
          collapsible = T,
          collapsed = T,
          fonti()
        )
      )
    ),
    
    # Menu Item: DT civile ---------------------------------------------------------------
    
    tabItem(
      tabName = "DT_complessivo_civ",
      fluidRow(
        box(
          title = div("DT civile complessivo (somma dei DT nazionali del Tribunale, della Corte di Appello e della Cassazione)", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_DT_cmp_civ", width = "100%", height = "790px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_cmp_civ", width = "100%", height = "590pt"), type = 5)
        ),
        box(
          title = "Tabella numerica",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_cmp_civ"))
        )
      )
    ),
    
    tabItem(
      tabName = "DT_nazionale_civ",
      fluidRow(
        box(
          title = "DT civile - Dettaglio nazionale per tipo ufficio",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "tipo_ufficio_DT_naz_civ",
            label = "Scegli il tipo ufficio:",
            choices = c("Cassazione", "Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          )
          # ),
          # checkboxInput("is_materia_DT_naz_civ", tags$b("Vista per macromateria"), F)
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_DT_naz_civ !== null && input.tipo_ufficio_DT_naz_civ.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_cda_naz_civ",
              label    = NULL,
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_DT_naz_civ !== null && input.tipo_ufficio_DT_naz_civ.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_trb_naz_civ",
              label    = NULL,
              choices  = choices_trb_civ(is_DT_civ = T),
              selected = choices_trb_civ(is_DT_civ = T)
            )
          ),
          checkboxInput("is_materia_DT_naz_civ", tags$b("Vista per macromateria"), F)
        ),
        box(
          title = div("Grafici", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_DT_naz_civ", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_naz_civ", width = "100%", height = "505pt"), type = 5)
        ),
        box(
          title = "Tabella numerica",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_naz_civ"))
        )
      )
    ),
    
    tabItem(
      tabName = "DT_all_dis_civ",
      fluidRow(
        box(
          title = "DT civile - Tutti i distretti (vista comparata per tipo ufficio)",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "all_dis_DT_civ",
            label = "Scegli il distretto:",
            choices = distretti,
            selected = distretti,
            multiple = T
          ),
          selectInput(
            inputId = "tipo_ufficio_DT_all_dis_civ",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          )
          # ),
          # checkboxInput("is_materia_DT_dis_civ", tags$b("Vista per macromateria"), F)
        ),
        # box(
        #   title = "Scegli le macromaterie",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   conditionalPanel(
        #     condition = "input.tipo_ufficio_DT_dis_civ !== null && input.tipo_ufficio_DT_dis_civ.indexOf('Corte di Appello') >= 0",
        #     checkboxGroupInput(
        #       inputId  = "materie_cda_dis_civ",
        #       label    = NULL,
        #       choices  = choices_cda_civ(),
        #       selected = choices_cda_civ()
        #     )
        #   ),
        #   conditionalPanel(
        #     condition = "input.tipo_ufficio_DT_dis_civ !== null && input.tipo_ufficio_DT_dis_civ.indexOf('Tribunale') >= 0",
        #     checkboxGroupInput(
        #       inputId  = "materie_trb_dis_civ",
        #       label    = NULL,
        #       choices  = choices_trb_civ(is_DT_civ = T),
        #       selected = choices_trb_civ(is_DT_civ = T)
        #     )
        #   ),
        #   checkboxInput("is_materia_DT_dis_civ", tags$b("Vista per macromateria"), F)
        # ),
        box(
          title = div("Grafici", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_DT_all_dis_civ", width = "100%", height = "1000px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_dis_civ", width = "100%", height = "600px"), type = 5)
        ),
        box(
          title = "Tabella numerica",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_all_dis_civ"))
        )
      )
    ),
    
    tabItem(
      tabName = "DT_per_distretto_civ",
      fluidRow(
        box(
          title = "DT civile - Dettaglio per distretto (e per tipo ufficio)",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "distretto_DT_civ",
            label = "Scegli il distretto:",
            choices = distretti,
            selected = "ANCONA"
          ),
          selectInput(
            inputId = "tipo_ufficio_DT_dis_civ",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          )
          # ),
          # checkboxInput("is_materia_DT_dis_civ", tags$b("Vista per macromateria"), F)
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_DT_dis_civ !== null && input.tipo_ufficio_DT_dis_civ.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_cda_dis_civ",
              label    = NULL,
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_DT_dis_civ !== null && input.tipo_ufficio_DT_dis_civ.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_trb_dis_civ",
              label    = NULL,
              choices  = choices_trb_civ(is_DT_civ = T),
              selected = choices_trb_civ(is_DT_civ = T)
            )
          ),
          checkboxInput("is_materia_DT_dis_civ", tags$b("Vista per macromateria"), F)
        ),
        
        ##############################################################################
        
        # TAB SCEGLI GLI UFFICI
        # 
        # box(
        #   title = "Scegli gli uffici",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   conditionalPanel(
        #     condition = "input.tipo_ufficio_DT_dis_civ !== null && input.tipo_ufficio_DT_dis_civ.indexOf('Corte di Appello') >= 0",
        #     checkboxGroupInput(
        #       inputId  = "uffici_cda_dis_civ",
        #       label    = NULL,
        #       choices = uffici[`Tipo Ufficio` == "Corte di Appello"][, Ufficio],
        #       selected = uffici[`Tipo Ufficio` == "Corte di Appello"][, Ufficio]
        #     )
        #   ),
        #   conditionalPanel(
        #     condition = "input.tipo_ufficio_DT_dis_civ !== null && input.tipo_ufficio_DT_dis_civ.indexOf('Tribunale') >= 0",
        #     checkboxGroupInput(
        #       inputId  = "uffici_trb_dis_civ",
        #       label    = NULL,
        #       choices = uffici[`Tipo Ufficio` == "Tribunale"][, Ufficio],
        #       selected = uffici[`Tipo Ufficio` == "Tribunale"][, Ufficio]
        #     )
        #   ),
        #   checkboxInput("is_ufficio_DT_dis_civ", tags$b("Vista per ufficio"), F)
        # ),
        
        ##############################################################################
        
        box(
          title = div("Grafici", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_DT_dis_civ", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_dis_civ", width = "100%", height = "600px"), type = 5)
        ),
        box(
          title = "Tabella numerica",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_dis_civ"))
        )
      )
    ),
    
    tabItem(
      tabName = "DT_all_uffs_civ",
      fluidRow(
        box(
          title = "DT civile - Tutti gli uffici del distretto (vista comparata per tipo ufficio)",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "distretto_DT_all_uffs_civ",
            label = "Scegli il distretto:",
            choices = distretti,
            selected = "ANCONA"
          ),
          selectInput(
            inputId = "tipo_ufficio_DT_all_uffs_civ",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Tribunale"
          )
          # ),
          # checkboxInput("is_materia_DT_dis_civ", tags$b("Vista per macromateria"), F)
        ),
        # box(
        #   title = "Scegli le macromaterie",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   conditionalPanel(
        #     condition = "input.tipo_ufficio_DT_dis_civ !== null && input.tipo_ufficio_DT_dis_civ.indexOf('Corte di Appello') >= 0",
        #     checkboxGroupInput(
        #       inputId  = "materie_cda_dis_civ",
        #       label    = NULL,
        #       choices  = choices_cda_civ(),
        #       selected = choices_cda_civ()
        #     )
        #   ),
        #   conditionalPanel(
        #     condition = "input.tipo_ufficio_DT_dis_civ !== null && input.tipo_ufficio_DT_dis_civ.indexOf('Tribunale') >= 0",
        #     checkboxGroupInput(
        #       inputId  = "materie_trb_dis_civ",
        #       label    = NULL,
        #       choices  = choices_trb_civ(is_DT_civ = T),
        #       selected = choices_trb_civ(is_DT_civ = T)
        #     )
        #   ),
        #   checkboxInput("is_materia_DT_dis_civ", tags$b("Vista per macromateria"), F)
        # ),
        box(
          title = div("Grafici", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_DT_all_uffs_civ", width = "100%", height = "600px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_dis_civ", width = "100%", height = "600px"), type = 5)
        ),
        box(
          title = "Tabella numerica",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_all_uffs_civ"))
        )
      )
    ),
    
    tabItem(
      tabName = "DT_per_ufficio_civ",
      fluidRow(
        box(
          title = "DT civile - Dettaglio per ufficio",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "tipo_ufficio_DT_uff_civ",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_DT_uff_civ !== null && input.tipo_ufficio_DT_uff_civ.indexOf('Corte di Appello') >= 0",
            selectInput(
              inputId = "ufficio_cda_DT_civ",
              label = "Scegli l'ufficio:",
              choices = uffici[`Tipo Ufficio` == "Corte di Appello"][, Ufficio],
              selected = "CORTE DI APPELLO DI ANCONA"
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_DT_uff_civ !== null && input.tipo_ufficio_DT_uff_civ.indexOf('Tribunale') >= 0",
            selectInput(
              inputId = "ufficio_trb_DT_civ",
              label = "Scegli l'ufficio:",
              choices = uffici[`Tipo Ufficio` == "Tribunale"][, Ufficio],
              selected = "TRIBUNALE DI ANCONA"
            )
          )
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_DT_uff_civ !== null && input.tipo_ufficio_DT_uff_civ.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_cda_uff_civ", 
              label    = "Scegli le macromaterie:",
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_DT_uff_civ !== null && input.tipo_ufficio_DT_uff_civ.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_trb_uff_civ",
              label    = "Scegli le macromaterie:",
              choices  = choices_trb_civ(is_DT_civ = T),
              selected = choices_trb_civ(is_DT_civ = T)
            )
          ),
          checkboxInput("is_materia_DT_uff_civ", tags$b("Vista per macromateria"), F)
        ),
        
        box(
          title = div(
            "Analisi what-if",
            span(
              `data-toggle` = "tooltip",
              `data-placement` = "right",
              title = "Come varierebbe la proiezione del DT al 30.06.2026, se un numero addizionale (rispetto alla stima corrente) di procedimenti fosse definito nel primo semestre 2026?",
              icon("info-circle")
            )
          ),
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          numericInput("delta", "Numero addizionale di procedimenti da definire nel primo semestre 2026 (senza distinzione per macromateria - intero ufficio)", value = 0, min = 0, step = 100)
        ),
        
        box(
          title = div("Grafici", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_DT_uff_civ", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_uff_civ", width = "100%", height = "700px"), type = 5)
        ),
        box(
          title = "Tabella numerica",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_uff_civ"))
        )
      )
    ),
    
    # Menu Item: DT penale ---------------------------------------------------------------
    
    tabItem(
      tabName = "DT_complessivo_pen",
      fluidRow(
        box(
          title = div("DT penale complessivo (somma dei DT nazionali del Tribunale, della Corte di Appello e della Cassazione)", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_DT_cmp_pen", width = "100%", height = "790px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_cmp_pen", width = "100%", height = "590pt"), type = 5)
        ),
        box(
          title = "Tabella numerica",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_cmp_pen"))
        )
      )
    ),
    
    tabItem(
      tabName = "DT_nazionale_pen",
      fluidRow(
        box(
          title = "DT penale - Dettaglio nazionale per tipo ufficio",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "tipo_ufficio_DT_naz_pen",
            label = "Scegli il tipo ufficio:",
            choices = c("Cassazione", "Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          ),
          checkboxInput("is_materia_DT_naz_pen", tags$b("Vista per sezione"), F)
        ),
        box(
          title = div("Grafici", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_DT_naz_pen", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_naz_pen", width = "100%", height = "505pt"), type = 5)
        ),
        box(
          title = "Tabella numerica",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_naz_pen"))
        )
      )
    ),
    
    tabItem(
      tabName = "DT_per_distretto_pen",
      fluidRow(
        box(
          title = "DT penale - Dettaglio per distretto (e per tipo ufficio)",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "distretto_DT_pen",
            label = "Scegli il distretto:",
            choices = distretti,
            selected = "ANCONA"
          ),
          selectInput(
            inputId = "tipo_ufficio_DT_dis_pen",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          ),
          checkboxInput("is_materia_DT_dis_pen", tags$b("Vista per sezione"), F)
        ),
        box(
          title = div("Grafici", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_DT_dis_pen", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_dis_pen", width = "100%", height = "600px"), type = 5)
        ),
        box(
          title = "Tabella numerica",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_dis_pen"))
        )
      )
    ),
    
    tabItem(
      tabName = "DT_per_ufficio_pen",
      fluidRow(
        box(
          title = "DT penale - Dettaglio per ufficio",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "ufficio_DT_pen",
            label = "Scegli l'ufficio:",
            choices = uffici[, Ufficio],
            selected = "CORTE DI APPELLO DI ANCONA"
          ),
          checkboxInput("is_materia_DT_uff_pen", tags$b("Vista per sezione"), F)
        ),
        box(
          title = div("Grafici", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_DT_uff_pen", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_uff_pen", width = "100%", height = "700px"), type = 5)
        ),
        box(
          title = "Tabella numerica",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_uff_pen"))
        )
      )
    ),
    
    # Menu Item: AR civile (target 2024) ---------------------------------------------------------------      
    
    # tabItem(
    #   tabName = "AR_tutti_i_distretti_tg24",
    #   fluidRow(
    #     box(
    #       title = div("Arretrato civile per tipo ufficio (tutti i distretti) - target 2024", hlp()),
    #       status = "primary",
    #       collapsible = T,
    #       width = 12,
    #       selectInput(
    #         inputId = "tipo_ufficio_AR_all_dis_civ_tg24",
    #         label = "Scegli il tipo ufficio:",
    #         choices = c("Corte di Appello", "Tribunale"),
    #         selected = "Corte di Appello"
    #       )
    #     ),
    #     box(
    #       title = "Scegli le macromaterie",
    #       status = "primary",
    #       collapsible = T,
    #       collapsed = T,
    #       width = 12,
    #       conditionalPanel(
    #         condition = "input.tipo_ufficio_AR_all_dis_civ_tg24 !== null && input.tipo_ufficio_AR_all_dis_civ_tg24.indexOf('Corte di Appello') >= 0",
    #         checkboxGroupInput(
    #           inputId  = "materie_cda_all_dis_civ_tg24",
    #           label    = NULL,
    #           choices  = choices_cda_civ(),
    #           selected = choices_cda_civ()
    #         )
    #       ),
    #       conditionalPanel(
    #         condition = "input.tipo_ufficio_AR_all_dis_civ_tg24 !== null && input.tipo_ufficio_AR_all_dis_civ_tg24.indexOf('Tribunale') >= 0",
    #         checkboxGroupInput(
    #           inputId  = "materie_trb_all_dis_civ_tg24",
    #           label    = NULL,
    #           choices  = choices_trb_civ(),
    #           selected = choices_trb_civ()
    #         )
    #       )
    #     ),
    #     box(
    #       status = "primary",
    #       collapsible = T,
    #       width = 12,
    #       shinycssloaders::withSpinner(imageOutput("plot_AR_all_dis_civ_tg24", width = "auto", height = "auto"), type = 5)
    #     )
    #   ),
    #   box(
    #     title = "Tabella numerica",
    #     status = "primary",
    #     #solidHeader = T,
    #     collapsible = T,
    #     collapsed = T,
    #     width = 12,
    #     fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_AR_all_dis_civ_tg24"))
    #   )
    # ),
    
    tabItem(
      tabName = "AR_nazionale_tg24",
      fluidRow(
        box(
          title = "AR civile (target 2024) - Dettaglio nazionale per tipo ufficio",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "tipo_ufficio_AR_naz_civ_tg24",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          )
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_naz_civ_tg24 !== null && input.tipo_ufficio_AR_naz_civ_tg24.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_cda_naz_civ_tg24",
              label    = NULL,
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_naz_civ_tg24 !== null && input.tipo_ufficio_AR_naz_civ_tg24.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_trb_naz_civ_tg24",
              label    = NULL,
              choices  = choices_trb_civ(),
              selected = choices_trb_civ()
            )
          ),
          checkboxInput("is_materia_AR_naz_civ_tg24", tags$b("Vista per macromateria"), F)
        ),
        box(
          title = div("Grafici", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_AR_naz_civ_tg24", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_AR_naz_civ_tg24", width = "100%", height = "505pt"), type = 5)
        )#,
        # box(
        #   title = "Tabella numerica",
        #   status = "primary",
        #   #solidHeader = T,
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_AR_naz_civ_tg24"))
        # )
      )
    ),
    
    tabItem(
      tabName = "AR_per_distretto_tg24",
      fluidRow(
        box(
          title = "AR civile (target 2024) - Dettaglio per distretto (e per tipo ufficio)",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "distretto_AR_civ_tg24",
            label = "Scegli il distretto:",
            choices = distretti,
            selected = "ANCONA"
          ),
          selectInput(
            inputId = "tipo_ufficio_AR_dis_civ_tg24",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          )
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_dis_civ_tg24 !== null && input.tipo_ufficio_AR_dis_civ_tg24.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_cda_dis_civ_tg24",
              label    = NULL,
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_dis_civ_tg24 !== null && input.tipo_ufficio_AR_dis_civ_tg24.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_trb_dis_civ_tg24",
              label    = NULL,
              choices  = choices_trb_civ(),
              selected = choices_trb_civ()
            )
          ),
          checkboxInput("is_materia_AR_dis_civ_tg24", tags$b("Vista per macromateria"), F)
        ),
        box(
          title = div("Grafici", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_AR_dis_civ_tg24", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_AR_dis_civ_tg24", width = "100%", height = "650px"), type = 5)
        )#,
        # box(
        #   title = "Tabella numerica",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_AR_dis_civ_tg24"))
        # )
      )
    ),
    
    tabItem(
      tabName = "AR_per_ufficio_tg24",
      fluidRow(
        box(
          title = "AR civile (target 2024) - Dettaglio per ufficio",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "tipo_ufficio_AR_uff_civ_tg24",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_uff_civ_tg24 !== null && input.tipo_ufficio_AR_uff_civ_tg24.indexOf('Corte di Appello') >= 0",
            selectInput(
              inputId = "ufficio_cda_AR_civ_tg24",
              label = "Scegli l'ufficio:",
              choices = uffici[`Tipo Ufficio` == "Corte di Appello"][, Ufficio],
              selected = "CORTE DI APPELLO DI ANCONA"
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_uff_civ_tg24 !== null && input.tipo_ufficio_AR_uff_civ_tg24.indexOf('Tribunale') >= 0",
            selectInput(
              inputId = "ufficio_trb_AR_civ_tg24",
              label = "Scegli l'ufficio:",
              choices = uffici[`Tipo Ufficio` == "Tribunale"][, Ufficio],
              selected = "TRIBUNALE DI ANCONA"
            )
          )
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_uff_civ_tg24 !== null && input.tipo_ufficio_AR_uff_civ_tg24.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_cda_uff_civ_tg24",
              label    = NULL,
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_uff_civ_tg24 !== null && input.tipo_ufficio_AR_uff_civ_tg24.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_trb_uff_civ_tg24",
              label    = NULL,
              choices  = choices_trb_civ(),
              selected = choices_trb_civ()
            )
          ),
          checkboxInput("is_materia_AR_uff_civ_tg24", tags$b("Vista per macromateria"), F)
        ),
        box(
          title = div("Grafici", hlp()),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_AR_uff_civ_tg24", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_AR_uff_civ_tg24", width = "100%", height = "700px"), type = 5)
        )#,
        # box(
        #   title = "Tabella numerica",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_AR_uff_civ_tg24"))
        # ),
        # box(
        #   title = "AR civile per anno di iscrizione - Dati sezionali - 2024",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   fluidPage(style = "width:100%;font-size:100%", dataTableOutput("table_AR_sez_civ_tg26"))
        # )
      )
    ),
    
    # tabItem(
    #   tabName = "AR_vs_TO_MAG",
    #   fluidRow(
    #     box(
    #       title = div("Arretrato vs Turnover magistrati", hlp_to()),
    #       status = "primary",
    #       collapsible = T,
    #       collapsed = F,
    #       width = 12,
    #       shinycssloaders::withSpinner(plotlyOutput("plot_TO_MAG_civ", width = "100%", height = "925px"), type = 5)
    #     )
    #   )
    # ),
    
    # Menu Item: AR civile (target 2026) ---------------------------------------------------------------      
    
    # tabItem(
    #   tabName = "AR_tutti_i_distretti_tg26",
    #   fluidRow(
    #     box(
    #       title = div("Arretrato civile per tipo ufficio (tutti i distretti) - target 2026", hlp()),
    #       status = "primary",
    #       collapsible = T,
    #       width = 12,
    #       selectInput(
    #         inputId = "tipo_ufficio_AR_all_dis_civ_tg26",
    #         label = "Scegli il tipo ufficio:",
    #         choices = c("Corte di Appello", "Tribunale"),
    #         selected = "Corte di Appello"
    #       )
    #     ),
    #     box(
    #       title = "Scegli le macromaterie",
    #       status = "primary",
    #       collapsible = T,
    #       collapsed = T,
    #       width = 12,
    #       conditionalPanel(
    #         condition = "input.tipo_ufficio_AR_all_dis_civ_tg26 !== null && input.tipo_ufficio_AR_all_dis_civ_tg26.indexOf('Corte di Appello') >= 0",
    #         checkboxGroupInput(
    #           inputId  = "materie_cda_all_dis_civ_tg26",
    #           label    = NULL,
    #           choices  = choices_cda_civ(),
    #           selected = choices_cda_civ()
    #         )
    #       ),
    #       conditionalPanel(
    #         condition = "input.tipo_ufficio_AR_all_dis_civ_tg26 !== null && input.tipo_ufficio_AR_all_dis_civ_tg26.indexOf('Tribunale') >= 0",
    #         checkboxGroupInput(
    #           inputId  = "materie_trb_all_dis_civ_tg26",
    #           label    = NULL,
    #           choices  = choices_trb_civ(),
    #           selected = choices_trb_civ()
    #         )
    #       )
    #     ),
    #     box(
    #       status = "primary",
    #       collapsible = T,
    #       width = 12,
    #       shinycssloaders::withSpinner(imageOutput("plot_AR_all_dis_civ_tg26", width = "auto", height = "auto"), type = 5)
    #     )
    #   ),
    #   box(
    #     title = "Tabella numerica",
    #     status = "primary",
    #     #solidHeader = T,
    #     collapsible = T,
    #     collapsed = T,
    #     width = 12,
    #     fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info__AR_all_dis_civ_tg26"))
    #   )
    # ),
    
    tabItem(
      tabName = "AR_nazionale_tg26",
      fluidRow(
        box(
          title = "AR civile (target 2026) - Dettaglio nazionale per tipo ufficio",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "tipo_ufficio_AR_naz_civ_tg26",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          )
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_naz_civ_tg26 !== null && input.tipo_ufficio_AR_naz_civ_tg26.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_cda_naz_civ_tg26",
              label    = NULL,
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_naz_civ_tg26 !== null && input.tipo_ufficio_AR_naz_civ_tg26.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_trb_naz_civ_tg26",
              label    = NULL,
              choices  = choices_trb_civ(),
              selected = choices_trb_civ()
            )
          ),
          checkboxInput("is_materia_AR_naz_civ_tg26", tags$b("Vista per macromateria"), F)
        ),
        box(
          title = div("Grafici", hlp(is_AR_tg26 = T)),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_AR_naz_civ_tg26", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_AR_naz_civ_tg26", width = "100%", height = "505pt"), type = 5)
        )#,
        # box(
        #   title = "Tabella numerica",
        #   status = "primary",
        #   #solidHeader = T,
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_AR_naz_civ_tg26"))
        # )
      )
    ),
    
    tabItem(
      tabName = "AR_per_distretto_tg26",
      fluidRow(
        box(
          title = "AR civile (target 2026) - Dettaglio per distretto (e per tipo ufficio)",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "distretto_AR_civ_tg26",
            label = "Scegli il distretto:",
            choices = distretti,
            selected = "ANCONA"
          ),
          selectInput(
            inputId = "tipo_ufficio_AR_dis_civ_tg26",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          )
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_dis_civ_tg26 !== null && input.tipo_ufficio_AR_dis_civ_tg26.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_cda_dis_civ_tg26",
              label    = NULL,
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_dis_civ_tg26 !== null && input.tipo_ufficio_AR_dis_civ_tg26.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_trb_dis_civ_tg26",
              label    = NULL,
              choices  = choices_trb_civ(),
              selected = choices_trb_civ()
            )
          ),
          checkboxInput("is_materia_AR_dis_civ_tg26", tags$b("Vista per macromateria"), F)
        ),
        box(
          title = div("Grafici", hlp(is_AR_tg26 = T)),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_AR_dis_civ_tg26", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_AR_dis_civ_tg26", width = "100%", height = "650px"), type = 5)
        )#,
        # box(
        #   title = "Tabella numerica",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_AR_dis_civ_tg26"))
        # )
      )
    ),
    
    tabItem(
      tabName = "AR_per_ufficio_tg26",
      fluidRow(
        box(
          title = "AR civile (target 2026) - Dettaglio per ufficio",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "tipo_ufficio_AR_uff_civ_tg26",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_uff_civ_tg26 !== null && input.tipo_ufficio_AR_uff_civ_tg26.indexOf('Corte di Appello') >= 0",
            selectInput(
              inputId = "ufficio_cda_AR_civ_tg26",
              label = "Scegli l'ufficio:",
              choices = uffici[`Tipo Ufficio` == "Corte di Appello"][, Ufficio],
              selected = "CORTE DI APPELLO DI ANCONA"
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_uff_civ_tg26 !== null && input.tipo_ufficio_AR_uff_civ_tg26.indexOf('Tribunale') >= 0",
            selectInput(
              inputId = "ufficio_trb_AR_civ_tg26",
              label = "Scegli l'ufficio:",
              choices = uffici[`Tipo Ufficio` == "Tribunale"][, Ufficio],
              selected = "TRIBUNALE DI ANCONA"
            )
          )
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_uff_civ_tg26 !== null && input.tipo_ufficio_AR_uff_civ_tg26.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_cda_uff_civ_tg26",
              label    = "Scegli le macromaterie:",
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_AR_uff_civ_tg26 !== null && input.tipo_ufficio_AR_uff_civ_tg26.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_trb_uff_civ_tg26",
              label    = "Scegli le macromaterie:",
              choices  = choices_trb_civ(),
              selected = choices_trb_civ()
            )
          ),
          checkboxInput("is_materia_AR_uff_civ_tg26", tags$b("Vista per macromateria"), F)
        ),
        box(
          title = div("Grafici", hlp(is_AR_tg26 = T)),
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_AR_uff_civ_tg26", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_AR_uff_civ_tg26", width = "100%", height = "700px"), type = 5)
        )#,
        # box(
        #   title = "Tabella numerica",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_AR_uff_civ_tg26"))
        # ),
        # box(
        #   title = "AR civile per anno di iscrizione - Dati sezionali al 31.12.2024",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   fluidPage(style = "width:100%;font-size:100%", dataTableOutput("table_AR_sez_civ_tg26"))
        # )
      )
    ),
    
    # Menu Item: Flussi civile (sopravvenuti e definiti) ---------------------------------------------------------------
    
    tabItem(
      tabName = "Flussi_nazionale_civ",
      fluidRow(
        box(
          title = "Flussi civile (sopravvenuti e definiti) - Dettaglio nazionale per tipo ufficio",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "tipo_ufficio_Fls_naz_civ",
            label = "Scegli il tipo ufficio:",
            choices = c("Cassazione", "Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          )
          # ),
          # checkboxInput("is_materia_DT_naz_civ", tags$b("Vista per macromateria"), F)
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_Fls_naz_civ !== null && input.tipo_ufficio_Fls_naz_civ.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_Fls_cda_naz_civ",
              label    = NULL,
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_Fls_naz_civ !== null && input.tipo_ufficio_Fls_naz_civ.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_Fls_trb_naz_civ",
              label    = NULL,
              choices  = choices_trb_civ(is_DT_civ = T),
              selected = choices_trb_civ(is_DT_civ = T)
            )
          ),
          checkboxInput("is_materia_Fls_naz_civ", tags$b("Vista per macromateria"), F)
        ),
        box(
          title  = "Grafici",
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_Fls_naz_civ", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_naz_civ", width = "100%", height = "505pt"), type = 5)
        )
        # box(
        #   title = "Tabella numerica",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_naz_civ"))
        # )
      )
    ),
    
    tabItem(
      tabName = "Flussi_per_distretto_civ",
      fluidRow(
        box(
          title = "Flussi civile (sopravvenuti e definiti) - Dettaglio per distretto (e per tipo ufficio)",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "distretto_Fls_civ",
            label = "Scegli il distretto:",
            choices = distretti,
            selected = "ANCONA"
          ),
          selectInput(
            inputId = "tipo_ufficio_Fls_dis_civ",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          )
          # ),
          # checkboxInput("is_materia_DT_dis_civ", tags$b("Vista per macromateria"), F)
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_Fls_dis_civ !== null && input.tipo_ufficio_Fls_dis_civ.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_Fls_cda_dis_civ",
              label    = NULL,
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_Fls_dis_civ !== null && input.tipo_ufficio_Fls_dis_civ.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_Fls_trb_dis_civ",
              label    = NULL,
              choices  = choices_trb_civ(is_DT_civ = T),
              selected = choices_trb_civ(is_DT_civ = T)
            )
          ),
          checkboxInput("is_materia_Fls_dis_civ", tags$b("Vista per macromateria"), F)
        ),
        box(
          title  = "Grafici",
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_Fls_dis_civ", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_dis_civ", width = "100%", height = "600px"), type = 5)
        )
        # box(
        #   title = "Tabella numerica",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_dis_civ"))
        # )
      )
    ),
    
    tabItem(
      tabName = "Flussi_per_ufficio_civ",
      fluidRow(
        box(
          title = "Flussi civile (sopravvenuti e definiti) - Dettaglio per ufficio",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "tipo_ufficio_Fls_uff_civ",
            label = "Scegli il tipo ufficio:",
            choices = c("Corte di Appello", "Tribunale"),
            selected = "Corte di Appello"
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_Fls_uff_civ !== null && input.tipo_ufficio_Fls_uff_civ.indexOf('Corte di Appello') >= 0",
            selectInput(
              inputId = "ufficio_cda_Fls_civ",
              label = "Scegli l'ufficio:",
              choices = uffici[`Tipo Ufficio` == "Corte di Appello"][, Ufficio],
              selected = "CORTE DI APPELLO DI ANCONA"
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_Fls_uff_civ !== null && input.tipo_ufficio_Fls_uff_civ.indexOf('Tribunale') >= 0",
            selectInput(
              inputId = "ufficio_trb_Fls_civ",
              label = "Scegli l'ufficio:",
              choices = uffici[`Tipo Ufficio` == "Tribunale"][, Ufficio],
              selected = "TRIBUNALE DI ANCONA"
            )
          )
        ),
        box(
          title = "Scegli le macromaterie",
          status = "primary",
          collapsible = T,
          collapsed = T,
          width = 12,
          conditionalPanel(
            condition = "input.tipo_ufficio_Fls_uff_civ !== null && input.tipo_ufficio_Fls_uff_civ.indexOf('Corte di Appello') >= 0",
            checkboxGroupInput(
              inputId  = "materie_Fls_cda_uff_civ", 
              label    = "Scegli le macromaterie:",
              choices  = choices_cda_civ(),
              selected = choices_cda_civ()
            )
          ),
          conditionalPanel(
            condition = "input.tipo_ufficio_Fls_uff_civ !== null && input.tipo_ufficio_Fls_uff_civ.indexOf('Tribunale') >= 0",
            checkboxGroupInput(
              inputId  = "materie_Fls_trb_uff_civ",
              label    = "Scegli le macromaterie:",
              choices  = choices_trb_civ(is_DT_civ = T),
              selected = choices_trb_civ(is_DT_civ = T)
            )
          ),
          checkboxInput("is_materia_Fls_uff_civ", tags$b("Vista per macromateria"), F)
        ),
        box(
          title  = "Grafici",
          status = "primary",
          collapsible = T,
          width = 12,
          shinycssloaders::withSpinner(plotlyOutput("plot_Fls_uff_civ", width = "100%", height = "575px"), type = 5)
          #shinycssloaders::withSpinner(plotOutput("plot_DT_uff_civ", width = "100%", height = "700px"), type = 5)
        )
        # box(
        #   title = "Tabella numerica",
        #   status = "primary",
        #   collapsible = T,
        #   collapsed = T,
        #   width = 12,
        #   fluidPage(style = "width:100%;font-size:100%", dataTableOutput("info_DT_uff_civ"))
        # )
      )
    ),
    
    # tabItem(
    #   tabName = "AR_vs_TO_MAG",
    #   fluidRow(
    #     box(
    #       title = div("Arretrato vs Turnover magistrati", hlp_to()),
    #       status = "primary",
    #       collapsible = T,
    #       collapsed = F,
    #       width = 12,
    #       shinycssloaders::withSpinner(plotlyOutput("plot_TO_MAG_civ", width = "100%", height = "925px"), type = 5)
    #     )
    #   )
    # ),
    
    # Menu Item: Download ---------------------------------------------------------------
    
    # tabItem(
    #   tabName = "AR_dati_sezionali",
    #   fluidRow(
    #     box(
    #       title = "AR civile per anno di iscrizione - Dati sezionali - II semestre 2022",
    #       status = "primary",
    #       collapsible = T,
    #       width = 12,
    #       selectInput(
    #         inputId = "ufficio_AR_sez_civ",
    #         label = "Scegli l'ufficio:",
    #         choices = uffici[, Ufficio],
    #         selected = "CORTE DI APPELLO DI ANCONA"
    #       )
    #     ),
    #     box(
    #       title = "Dati",
    #       status = "primary",
    #       collapsible = T,
    #       collapsed = F,
    #       width = 12,
    #       fluidPage(style = "width:100%;font-size:100%", dataTableOutput('table_AR_sez_civ'), width = "120%")
    #     )
    #   )
    # ),
    
    # Menu Item: Codici oggetto PNRR ---------------------------------------------------------------
    
    # DT civile.
    tabItem(
      tabName = "DT_cdx_oggetto_civ",
      fluidRow(
        box(
          title = "DT civile - Codici oggetto PNRR - (aggiornamento 31/12/2024)",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "ufficio_cdx_oggetto_DT_civ",
            label = "Scegli l'ufficio:",
            choices = uffici[, Ufficio],
            selected = "CORTE DI APPELLO DI ANCONA"
          )
        ),
        box(
          title = "Dati",
          status = "primary",
          collapsible = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput('table_cdx_oggetto_DT_civ'), width = "120%")
        )
      )
    ),
    
    # AR civile.
    tabItem(
      tabName = "AR_cdx_oggetto_civ",
      fluidRow(
        box(
          title = "AR civile (baseline 2022) - Codici oggetto PNRR - target 2026 (aggiornamento 31/12/2024)",
          status = "primary",
          collapsible = T,
          width = 12,
          selectInput(
            inputId = "ufficio_cdx_oggetto_AR_civ",
            label = "Scegli l'ufficio:",
            choices = uffici[, Ufficio],
            selected = "CORTE DI APPELLO DI ANCONA"
          )
        ),
        box(
          title = "Dati",
          status = "primary",
          collapsible = T,
          width = 12,
          fluidPage(style = "width:100%;font-size:100%", dataTableOutput('table_cdx_oggetto_AR_civ'), width = "120%")
        )
      )
    )
    
    # --- #
  )
)

# DASHBOARD PAGE ----------------------------------------------------------

dashboardPage(header, sidebar, body, title = "Monitoraggio PNRR-Giustizia")
#secure_app(dashboardPage(header, sidebar, body, title = "Monitoraggio PNRR-Giustizia"))

############################################################################################################################################
#
# Cruscotto per il monitoraggio PNRR-Giustizia.
# Questo codice implementa la parte server del cruscotto. 
# I dati di input sono caricati in locale e sono ottenuti a partire dal file crea_storico.R (nella cartella 'code' del progetto 018_PNRR).
#
# @Paolo Fantini - Ufficio statistico CSM
# Giugno 2023
#
############################################################################################################################################

# SETUP -------------------------------------------------------------------

# Carica le funzioni e i dati di input.
invisible(sapply(list.files("functions", full.names = T, recursive = T), function(x) source(x, encoding = "UTF-8")))
#load_packages()
#credentials <- create_credentials()
load("data/movi_civ.RData")
load("data/movi_pen.RData")
load("data/stra_civ.RData")
#load("data/stra_sez.RData")
setnames(movi_civ, c("Macroarea CSM"),   c("Materia"))          # per uniformità tra civile e penale
setnames(stra_civ, c("Macroarea CSM"),   c("Materia"))          # idem
setnames(movi_pen, c("Sezione Materia"), c("Materia"))          # idem
da_escludere <- c("ii Impresa", "gi VG in materia di Impresa")  # non ci sono dati per tutti gli anni

# SHINY SERVER --------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # AUTHENTICATION ----------------------------------------------------------
  
  # Call the server part.
  # Check_credentials returns a function to authenticate users.
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  
  # DISPOSITION TIME (civile) --------------------------------------------------------
  
  # Complessivo ----
  output$plot_DT_cmp_civ <- renderPlotly(
    {
      tmp <- create_dt(movi_civ, stra_civ, is_materia = F, is_nazionale = T)[["dt"]][variable == "Disposition Time"]
      tot <- tmp[, .(value = sum(value)), keyby = .(date, settore)][, `:=` (tipo_ufficio = "+ Complessivo +", variable = "Disposition Time")]
      tmp <- rbindlist(list(tmp, tot), use.names = T, fill = T)
      facet_vars <- "tipo_ufficio"
      dt_pred    <- ftarget(tmp, facet_vars, is_checkbox = T)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, w = cdata$output_plot_DT_cmp_civ_width, h = cdata$output_plot_DT_cmp_civ_height)
    }
  )
  output$info_DT_cmp_civ <- DT::renderDataTable(
    {
      tmp <- create_dt(movi_civ, stra_civ, is_materia = F, is_nazionale = T)[["dt"]][variable == "Disposition Time"]
      tot <- tmp[, .(value = sum(value)), keyby = .(date, settore)][, `:=` (tipo_ufficio = "+ Complessivo +", variable = "Disposition Time")]
      tmp <- rbindlist(list(tmp, tot), use.names = T, fill = T)
      facet_vars <- "tipo_ufficio"
      info       <- as_DT(
        data.table(
          "aggregato" = "DT civile",
          ftarget(tmp, facet_vars, is_checkbox = T)[["info"]]
        )
      )
    }
  )
  
  # Vista nazionale ----
  output$plot_DT_naz_civ <- renderPlotly(
    {
      if(input$is_materia_DT_naz_civ) {
        if(input$tipo_ufficio_DT_naz_civ == "Cassazione") {
          out <- create_dt(movi_civ, stra_civ, is_materia = T, is_nazionale = T)
        }
        if(input$tipo_ufficio_DT_naz_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_naz_civ], stra_civ, is_materia = T, is_nazionale = T)
        }
        if(input$tipo_ufficio_DT_naz_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_naz_civ], stra_civ, is_materia = T, is_nazionale = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_nazionale = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_DT_naz_civ == "Cassazione") {
          out <- create_dt(movi_civ, stra_civ, is_materia = F, is_nazionale = T)
        }
        if(input$tipo_ufficio_DT_naz_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_naz_civ], stra_civ, is_materia = F, is_nazionale = T)
        }
        if(input$tipo_ufficio_DT_naz_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_naz_civ], stra_civ, is_materia = F, is_nazionale = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_nazionale = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      }
      tmp     <- dt[tipo_ufficio == input$tipo_ufficio_DT_naz_civ][variable == "Disposition Time"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_naz_civ)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, w = cdata$output_plot_DT_naz_civ_width, h = cdata$output_plot_DT_naz_civ_height)
    }
  )
  output$info_DT_naz_civ <- renderDataTable(
    {
      if(input$is_materia_DT_naz_civ) {
        if(input$tipo_ufficio_DT_naz_civ == "Cassazione") {
          out <- create_dt(movi_civ, stra_civ, is_materia = T, is_nazionale = T)
        }
        if(input$tipo_ufficio_DT_naz_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_naz_civ], stra_civ, is_materia = T, is_nazionale = T)
        }
        if(input$tipo_ufficio_DT_naz_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_naz_civ], stra_civ, is_materia = T, is_nazionale = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_nazionale = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_DT_naz_civ == "Cassazione") {
          out <- create_dt(movi_civ, stra_civ, is_materia = F, is_nazionale = T)
        }
        if(input$tipo_ufficio_DT_naz_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_naz_civ], stra_civ, is_materia = F, is_nazionale = T)
        }
        if(input$tipo_ufficio_DT_naz_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_naz_civ], stra_civ, is_materia = F, is_nazionale = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_nazionale = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      }
      tmp     <- dt[tipo_ufficio == input$tipo_ufficio_DT_naz_civ][variable == "Disposition Time"]
      info <- as_DT(
        data.table(
          "aggregato"    = "DT civile",
          "tipo ufficio" = input$tipo_ufficio_DT_naz_civ,
          ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_naz_civ)[["info"]]
        )
      )
      # if(input$is_materia_DT_naz_civ) {
      #   out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_nazionale = T)
      #   dt         <- out$dt[!materia %in% da_escludere]
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      # } else {
      #   out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_nazionale = T)
      #   dt         <- out$dt
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      # }
      # tmp  <- dt[tipo_ufficio == input$tipo_ufficio_DT_naz_civ][variable == "Disposition Time"]
      # info <- as_DT(
      #   data.table(
      #     "aggregato"    = "DT civile",
      #     "tipo ufficio" = input$tipo_ufficio_DT_naz_civ,
      #     ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_naz_civ)[["info"]]
      #     )
      #   )
    }
  )
  
  
  #################
  
  output$plot_DT_all_dis_civ <- renderPlotly(
    {
      # if(input$is_materia_DT_dis_civ) {
      #   if(input$tipo_ufficio_DT_dis_civ == "Corte di Appello") {
      #     out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = T, is_distretto = T)
      #   }
      #   if(input$tipo_ufficio_DT_dis_civ == "Tribunale") {
      #     out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = T, is_distretto = T)
      #   }
      #   #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_distretto = T)
      #   dt         <- out$dt[!materia %in% da_escludere]
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      # } else {
      if(input$tipo_ufficio_DT_all_dis_civ == "Corte di Appello") {
        out <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
        #out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = F, is_distretto = T)
      }
      if(input$tipo_ufficio_DT_all_dis_civ == "Tribunale") {
        out <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
        #out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = F, is_distretto = T)
      }
      #out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
      dt         <- out$dt
      facet_vars <- out$facet_vars
      facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      #}
      tmp     <- dt[distretto %in% input$all_dis_DT_civ][tipo_ufficio == input$tipo_ufficio_DT_all_dis_civ][variable == "Disposition Time"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = T)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, w = cdata$output_plot_DT_all_dis_civ_width, h = cdata$output_plot_DT_all_dis_civ_height)
    }
  )
  
  output$info_DT_all_dis_civ <- renderDataTable(
    {
      # if(input$is_materia_DT_dis_civ) {
      #   if(input$tipo_ufficio_DT_dis_civ == "Corte di Appello") {
      #     out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = T, is_distretto = T)
      #   }
      #   if(input$tipo_ufficio_DT_dis_civ == "Tribunale") {
      #     out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = T, is_distretto = T)
      #   }
      #   #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_distretto = T)
      #   dt         <- out$dt[!materia %in% da_escludere]
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      # } else {
      if(input$tipo_ufficio_DT_dis_civ == "Corte di Appello") {
        out <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
        #out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = F, is_distretto = T)
      }
      if(input$tipo_ufficio_DT_dis_civ == "Tribunale") {
        out <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
        #out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = F, is_distretto = T)
      }
      #out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
      dt         <- out$dt
      facet_vars <- out$facet_vars
      facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      # }
      tmp     <- dt[distretto %in% input$all_dis_DT_civ][tipo_ufficio == input$tipo_ufficio_DT_all_dis_civ][variable == "Disposition Time"]
      #dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_dis_civ)[["dt_pred"]]
      #fplot(tmp, facet_vars, dt_pred)
      info <- as_DT(
        data.table(
          "aggregato"    = "DT civile",
          "distretto"    = input$all_dis_DT_civ,
          "tipo ufficio" = input$tipo_ufficio_DT_all_dis_civ,
          ftarget(tmp, facet_vars, is_checkbox = T)[["info"]]
        )
      )
    }
  )
  
  ##################
  
  # Vista per distretto ----
  output$plot_DT_dis_civ <- renderPlotly(
    {
      if(input$is_materia_DT_dis_civ) {
        if(input$tipo_ufficio_DT_dis_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = T, is_distretto = T)
        }
        if(input$tipo_ufficio_DT_dis_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = T, is_distretto = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_distretto = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_DT_dis_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = F, is_distretto = T)
        }
        if(input$tipo_ufficio_DT_dis_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = F, is_distretto = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      }
      tmp     <- dt[distretto == input$distretto_DT_civ][tipo_ufficio == input$tipo_ufficio_DT_dis_civ][variable == "Disposition Time"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_dis_civ)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, w = cdata$output_plot_DT_dis_civ_width, h = cdata$output_plot_DT_dis_civ_height)
    }
  )
  output$info_DT_dis_civ <- renderDataTable(
    {
      if(input$is_materia_DT_dis_civ) {
        if(input$tipo_ufficio_DT_dis_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = T, is_distretto = T)
        }
        if(input$tipo_ufficio_DT_dis_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = T, is_distretto = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_distretto = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_DT_dis_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = F, is_distretto = T)
        }
        if(input$tipo_ufficio_DT_dis_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = F, is_distretto = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      }
      tmp     <- dt[distretto == input$distretto_DT_civ][tipo_ufficio == input$tipo_ufficio_DT_dis_civ][variable == "Disposition Time"]
      #dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_dis_civ)[["dt_pred"]]
      #fplot(tmp, facet_vars, dt_pred)
      info <- as_DT(
        data.table(
          "aggregato"    = "DT civile",
          "distretto"    = input$distretto_DT_civ,
          "tipo ufficio" = input$tipo_ufficio_DT_dis_civ,
          ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_dis_civ)[["info"]]
        )
      )
      # if(input$is_materia_DT_dis_civ) {
      #   out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_distretto = T)
      #   dt         <- out$dt[!materia %in% da_escludere]
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      # } else {
      #   out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
      #   dt         <- out$dt
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      # }
      # tmp  <- dt[distretto == input$distretto_DT_civ][tipo_ufficio == input$tipo_ufficio_DT_dis_civ][variable == "Disposition Time"]
      # info <- as_DT(
      #   data.table(
      #     "aggregato"    = "DT civile",
      #     "distretto"    = input$distretto_DT_civ,
      #     "tipo ufficio" = input$tipo_ufficio_DT_dis_civ,
      #     ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_dis_civ)[["info"]]
      #     )
      #   )
    }
  )
  
  
  #**************************
  
  # Vista per distretto ----
  output$plot_DT_all_uffs_civ <- renderPlotly(
    {
      # if(input$is_materia_DT_dis_civ) {
      #   if(input$tipo_ufficio_DT_dis_civ == "Corte di Appello") {
      #     out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = T, is_distretto = T)
      #   }
      #   if(input$tipo_ufficio_DT_dis_civ == "Tribunale") {
      #     out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = T, is_distretto = T)
      #   }
      #   #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_distretto = T)
      #   dt         <- out$dt[!materia %in% da_escludere]
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      # } else {
      if(input$tipo_ufficio_DT_all_uffs_civ == "Corte di Appello") {
        out <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = F)
        #out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = F, is_distretto = T)
      }
      if(input$tipo_ufficio_DT_all_uffs_civ == "Tribunale") {
        out <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = F)
        #out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = F, is_distretto = T)
      }
      #out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
      dt         <- out$dt
      facet_vars <- out$facet_vars
      facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      #}
      tmp     <- dt[distretto == input$distretto_DT_all_uffs_civ][tipo_ufficio == input$tipo_ufficio_DT_all_uffs_civ][variable == "Disposition Time"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = T)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, w = cdata$output_plot_DT_all_uffs_civ_width, h = cdata$output_plot_DT_all_uffs_civ_height)
    }
  )
  output$info_DT_all_uffs_civ <- renderDataTable(
    {
      # if(input$is_materia_DT_dis_civ) {
      #   if(input$tipo_ufficio_DT_dis_civ == "Corte di Appello") {
      #     out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = T, is_distretto = T)
      #   }
      #   if(input$tipo_ufficio_DT_dis_civ == "Tribunale") {
      #     out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = T, is_distretto = T)
      #   }
      #   #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_distretto = T)
      #   dt         <- out$dt[!materia %in% da_escludere]
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      # } else {
      if(input$tipo_ufficio_DT_all_uffs_civ == "Corte di Appello") {
        out <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = F)
        #out <- create_dt(movi_civ[Materia %in% input$materie_cda_dis_civ], stra_civ, is_materia = F, is_distretto = T)
      }
      if(input$tipo_ufficio_DT_all_uffs_civ == "Tribunale") {
        out <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = F)
        #out <- create_dt(movi_civ[Materia %in% input$materie_trb_dis_civ], stra_civ, is_materia = F, is_distretto = T)
      }
      #out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
      dt         <- out$dt
      facet_vars <- out$facet_vars
      facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      #}
      tmp     <- dt[distretto == input$distretto_DT_all_uffs_civ][tipo_ufficio == input$tipo_ufficio_DT_all_uffs_civ][variable == "Disposition Time"]
      #dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_dis_civ)[["dt_pred"]]
      #fplot(tmp, facet_vars, dt_pred)
      info <- as_DT(
        data.table(
          "aggregato"    = "DT civile",
          "distretto"    = input$distretto_DT_all_uffs_civ,
          "tipo ufficio" = input$tipo_ufficio_DT_all_uffs_civ,
          ftarget(tmp, facet_vars, is_checkbox = T)[["info"]]
        )
      )
      # if(input$is_materia_DT_dis_civ) {
      #   out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_distretto = T)
      #   dt         <- out$dt[!materia %in% da_escludere]
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      # } else {
      #   out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
      #   dt         <- out$dt
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      # }
      # tmp  <- dt[distretto == input$distretto_DT_civ][tipo_ufficio == input$tipo_ufficio_DT_dis_civ][variable == "Disposition Time"]
      # info <- as_DT(
      #   data.table(
      #     "aggregato"    = "DT civile",
      #     "distretto"    = input$distretto_DT_civ,
      #     "tipo ufficio" = input$tipo_ufficio_DT_dis_civ,
      #     ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_dis_civ)[["info"]]
      #     )
      #   )
    }
  )
  
  
  #**************************
  
  # Vista per ufficio ----
  output$plot_DT_uff_civ <- renderPlotly(
    {
      if(input$is_materia_DT_uff_civ) {
        if(input$tipo_ufficio_DT_uff_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_uff_civ], stra_civ, is_materia = T)
        }
        if(input$tipo_ufficio_DT_uff_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_uff_civ], stra_civ, is_materia = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_DT_uff_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_uff_civ], stra_civ, is_materia = F)
        }
        if(input$tipo_ufficio_DT_uff_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_uff_civ], stra_civ, is_materia = F)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = F)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      }
      if(input$tipo_ufficio_DT_uff_civ == "Corte di Appello") {
        tmp <- dt[ufficio == input$ufficio_cda_DT_civ]#[variable == "Disposition Time"]
      }
      if(input$tipo_ufficio_DT_uff_civ == "Tribunale") {
        tmp <- dt[ufficio == input$ufficio_trb_DT_civ]#[variable == "Disposition Time"]
      }
      #tmp <- dt[ufficio == input$ufficio_DT_civ][variable == "Disposition Time"]
      if(input$is_materia_DT_uff_civ) {
        if(nrow(tmp[date == "2019-12-31" & materia == "ip Immigrazione e Prot. Internazionale"]) == 0) {  # trucco per evitare di plottare questa materia quando non si dovrebbe (per esempio, nel caso dei tribunali non distrettuali -> caso IVREA)
          tmp <- tmp[!materia %in% c("ip Immigrazione e Prot. Internazionale")]
        }
      }
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_uff_civ, is_delta = T, delta = input$delta)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, is_delta = T, w = cdata$output_plot_DT_uff_civ_width, h = cdata$output_plot_DT_uff_civ_height)
    }
  )
  output$info_DT_uff_civ <- renderDataTable(
    {
      if(input$is_materia_DT_uff_civ) {
        if(input$tipo_ufficio_DT_uff_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_uff_civ], stra_civ, is_materia = T)
        }
        if(input$tipo_ufficio_DT_uff_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_uff_civ], stra_civ, is_materia = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_DT_uff_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_cda_uff_civ], stra_civ, is_materia = F)
        }
        if(input$tipo_ufficio_DT_uff_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_trb_uff_civ], stra_civ, is_materia = F)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = F)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      }
      if(input$tipo_ufficio_DT_uff_civ == "Corte di Appello") {
        tmp <- dt[ufficio == input$ufficio_cda_DT_civ]#[variable == "Disposition Time"]
      }
      if(input$tipo_ufficio_DT_uff_civ == "Tribunale") {
        tmp <- dt[ufficio == input$ufficio_trb_DT_civ]#[variable == "Disposition Time"]
      }
      #tmp <- dt[ufficio == input$ufficio_DT_civ][variable == "Disposition Time"]
      if(input$is_materia_DT_uff_civ) {
        if(nrow(tmp[date == "2019-12-31" & materia == "ip Immigrazione e Prot. Internazionale"]) == 0) {  # trucco per evitare di plottare questa materia quando non si dovrebbe (per esempio, nel caso dei tribunali non distrettuali -> caso IVREA)
          tmp <- tmp[!materia %in% c("ip Immigrazione e Prot. Internazionale")]
        }
      }
      # dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_uff_civ)[["dt_pred"]]
      # fplot(tmp, facet_vars, dt_pred)
      info <- as_DT(
        data.table(
          "aggregato" = "DT civile",
          "ufficio"   = input$ufficio_DT_civ,
          ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_uff_civ, is_delta = T, delta = input$delta)[["info"]]
        )
      )
      # if(input$is_materia_DT_uff_civ) {
      #   out        <- create_dt(movi_civ, stra_civ, is_materia = T)
      #   dt         <- out$dt[!materia %in% da_escludere]
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      # } else {
      #   out        <- create_dt(movi_civ, stra_civ, is_materia = F)
      #   dt         <- out$dt
      #   facet_vars <- out$facet_vars
      #   facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      # }
      # tmp  <- dt[ufficio == input$ufficio_DT_civ][variable == "Disposition Time"]
      # info <- as_DT(
      #   data.table(
      #     "aggregato" = "DT civile",
      #     "ufficio"   = input$ufficio_DT_civ,
      #     ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_uff_civ)[["info"]]
      #     )
      #   )
    }
  )
  
  # DISPOSITION TIME (penale) --------------------------------------------------------
  
  # Complessivo ----
  output$plot_DT_cmp_pen <- renderPlotly(
    {
      tmp <- create_dt(movi_pen, is_materia = F, is_nazionale = T, is_penale = T)[["dt"]][variable == "Disposition Time"]
      tot <- tmp[, .(value = sum(value)), keyby = .(date, settore)][, `:=` (tipo_ufficio = "+ Complessivo +", variable = "Disposition Time")]
      tmp <- rbindlist(list(tmp, tot), use.names = T, fill = T)
      facet_vars <- "tipo_ufficio"
      dt_pred    <- ftarget(tmp, facet_vars, is_checkbox = T)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, is_grid = F, w = cdata$output_plot_DT_cmp_pen_width, h = cdata$output_plot_DT_cmp_pen_height)
    }
  )
  output$info_DT_cmp_pen <- renderDataTable(
    {
      tmp <- create_dt(movi_pen, is_materia = F, is_nazionale = T, is_penale = T)[["dt"]][variable == "Disposition Time"]
      tot <- tmp[, .(value = sum(value)), keyby = .(date, settore)][, `:=` (tipo_ufficio = "+ Complessivo +", variable = "Disposition Time")]
      tmp <- rbindlist(list(tmp, tot), use.names = T, fill = T)
      facet_vars <- "tipo_ufficio"
      info       <- as_DT(
        data.table(
          "aggregato" = "DT penale",
          ftarget(tmp, facet_vars, is_checkbox = T)[["info"]]
        )
      )
    }
  )
  
  # Vista nazionale ----
  output$plot_DT_naz_pen <- renderPlotly(
    {
      if(input$is_materia_DT_naz_pen) {
        out        <- create_dt(movi_pen, is_materia = T, is_nazionale = T, is_penale = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      } else {
        out        <- create_dt(movi_pen, is_materia = F, is_nazionale = T, is_penale = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      }
      tmp     <- dt[tipo_ufficio == input$tipo_ufficio_DT_naz_pen][variable == "Disposition Time"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_naz_pen)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, is_grid = F, w = cdata$output_plot_DT_naz_pen_width, h = cdata$output_plot_DT_naz_pen_height)
    }
  )
  output$info_DT_naz_pen <- renderDataTable(
    {
      if(input$is_materia_DT_naz_pen) {
        out        <- create_dt(movi_pen, is_materia = T, is_nazionale = T, is_penale = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      } else {
        out        <- create_dt(movi_pen, is_materia = F, is_nazionale = T, is_penale = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      }
      tmp  <- dt[tipo_ufficio == input$tipo_ufficio_DT_naz_pen][variable == "Disposition Time"]
      info <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_naz_pen)[["info"]]
      info <- as_DT(
        data.table(
          "aggregato"    = "DT penale",
          "tipo ufficio" = input$tipo_ufficio_DT_naz_pen,
          ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_naz_pen)[["info"]]
        )
      )
    }
  )
  
  # Vista per distretto ----
  output$plot_DT_dis_pen <- renderPlotly(
    {
      if(input$is_materia_DT_dis_pen) {
        out        <- create_dt(movi_pen, is_materia = T, is_distretto = T, is_penale = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      } else {
        out        <- create_dt(movi_pen, is_materia = F, is_distretto = T, is_penale = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      }
      tmp     <- dt[distretto == input$distretto_DT_pen][tipo_ufficio == input$tipo_ufficio_DT_dis_pen][variable == "Disposition Time"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_dis_pen)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, w = cdata$output_plot_DT_dis_pen_width, h = cdata$output_plot_DT_dis_pen_height, is_grid = F)
    }
  )
  output$info_DT_dis_pen <- renderDataTable(
    {
      if(input$is_materia_DT_dis_pen) {
        out        <- create_dt(movi_pen, is_materia = T, is_distretto = T, is_penale = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      } else {
        out        <- create_dt(movi_pen, is_materia = F, is_distretto = T, is_penale = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      }
      tmp  <- dt[distretto == input$distretto_DT_pen][tipo_ufficio == input$tipo_ufficio_DT_dis_pen][variable == "Disposition Time"]
      info <- as_DT(
        data.table(
          "aggregato" = "DT penale",
          "distretto" = input$distretto_DT_pen,
          "tipo ufficio" = input$tipo_ufficio_DT_dis_pen,
          ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_dis_pen)[["info"]]
        )
      )
    }
  )
  
  # Vista per ufficio ----
  output$plot_DT_uff_pen <- renderPlotly(
    {
      if(input$is_materia_DT_uff_pen) {
        out        <- create_dt(movi_pen, is_materia = T, is_penale = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      } else {
        out        <- create_dt(movi_pen, is_materia = F, is_penale = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      }
      tmp     <- dt[ufficio == input$ufficio_DT_pen][variable == "Disposition Time"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_uff_pen)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, is_grid = F, w = cdata$output_plot_DT_uff_pen_width, h = cdata$output_plot_DT_uff_pen_height)
    }
  )
  output$info_DT_uff_pen <- renderDataTable(
    {
      if(input$is_materia_DT_uff_pen) {
        out        <- create_dt(movi_pen, is_materia = T, is_penale = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      } else {
        out        <- create_dt(movi_pen, is_materia = F, is_penale = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      }
      tmp  <- dt[ufficio == input$ufficio_DT_pen][variable == "Disposition Time"]
      info <- as_DT(
        data.table(
          "aggregato" = "DT penale",
          "ufficio"   = input$ufficio_DT_pen,
          ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_uff_pen)[["info"]]
        )
      )
    }
  )
  
  # ARRETRATO (TARGET 2024) --------------------------------------------------------
  
  # # Vista di insieme (tutti i distretti) ----
  # 
  # plotWidth   <- 1600
  # plotHeight  <- 1600
  # plotQuality <- 1
  # 
  # output$plot_AR_all_dis_civ_tg24 <- renderImage(
  #   {
  #     if(input$tipo_ufficio_AR_all_dis_civ_tg24 == "Corte di Appello") {
  #       out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_all_dis_civ_tg24], is_materia = F, is_distretto = T)
  #     }
  #     if(input$tipo_ufficio_AR_all_dis_civ_tg24 == "Tribunale") {
  #       out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_all_dis_civ_tg24], is_materia = F, is_distretto = T)
  #     }
  #     dt         <- out$dt
  #     facet_vars <- out$facet_vars
  #     facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
  #     tmp        <- dt[tipo_ufficio == input$tipo_ufficio_AR_all_dis_civ_tg24][variable == "Pendenti19"]
  #     distretti  <- tmp[, .N, keyby = distretto][, distretto]
  #     FUN <- function(tmp, title) {
  #       dt_pred <- ftarget(tmp, facet_vars, is_checkbox = F)[["dt_pred"]]
  #       return(fplot(tmp, facet_vars, dt_pred, title = title, is_strip = F))
  #     }
  #     ll <- lapply(distretti, function(d) FUN(tmp[distretto == d], title = d))
  #     outfile <- tempfile(fileext = '.png')  # generate the png -> temp file to save the output (it will be removed later by renderImage)  
  #     png(
  #       outfile,  
  #       width  = plotWidth,
  #       height = plotHeight,
  #       res    = 72*plotQuality
  #     )
  #     print(plot_grid(plotlist = ll, ncol = 4, label_x = 0.5))
  #     dev.off()
  #     list(  # return a list containing the filename.
  #       src         = outfile,
  #       contentType = 'image/png',
  #       width       = plotWidth,
  #       height      = plotHeight,
  #       res         = 72 * plotQuality,
  #       alt         = "Arretrato civile - tutti i distretti"
  #     )
  #   },
  #   deleteFile = TRUE
  # )
  # output$info_AR_all_dis_civ_tg24 <- renderDataTable(
  #   {
  #     if(input$tipo_ufficio_AR_all_dis_civ_tg24 == "Corte di Appello") {
  #       out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_all_dis_civ_tg24], is_materia = F, is_distretto = T)
  #     }
  #     if(input$tipo_ufficio_AR_all_dis_civ_tg24 == "Tribunale") {
  #       out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_all_dis_civ_tg24], is_materia = F, is_distretto = T)
  #     }
  #     dt         <- out$dt
  #     facet_vars <- out$facet_vars
  #     facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
  #     tmp        <- dt[tipo_ufficio == input$tipo_ufficio_AR_all_dis_civ_tg24][variable == "Pendenti19"]
  #     distretti  <- tmp[, .N, keyby = distretto][, distretto]
  #     FUN1 <- function(tmp, tipo_ufficio, distretto) {
  #       info <- ftarget(tmp, facet_vars, is_checkbox = F)[["info"]]
  #       return(data.table("aggregato" = "AR civile", "tipo ufficio" = tipo_ufficio, "distretto" = distretto, info))
  #     }
  #     ii <- lapply(distretti, function(d) FUN1(tmp[distretto == d], tipo_ufficio = input$tipo_ufficio_AR_all_dis_civ_tg24, distretto = d))
  #     as_DT(rbindlist(ii, use.names = T, fill = T))
  #     
  #     # DT::datatable(
  #     #   rbindlist(ii, use.names = T, fill = T),
  #     #   extensions = extensions,
  #     #   class      = class,
  #     #   rownames   = rownames,
  #     #   filter     = filter,
  #     #   options    = options
  #     # ) %>%
  #     #   formatPercentage(c("scenario 1 var% [2026 vs 2019]", "scenario 2 var% [2026 vs 2019]"))
  #   }
  # )
  
  # Vista nazionale ----
  output$plot_AR_naz_civ_tg24 <- renderPlotly(
    {
      if(input$is_materia_AR_naz_civ_tg24) {
        #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_nazionale = T)
        if(input$tipo_ufficio_AR_naz_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_naz_civ_tg24], is_materia = T, is_nazionale = T)
        }
        if(input$tipo_ufficio_AR_naz_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_naz_civ_tg24], is_materia = T, is_nazionale = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_naz_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_naz_civ_tg24], is_materia = F, is_nazionale = T)
        }
        if(input$tipo_ufficio_AR_naz_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_naz_civ_tg24], is_materia = F, is_nazionale = T)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      }
      tmp     <- dt[tipo_ufficio == input$tipo_ufficio_AR_naz_civ_tg24][variable == "Pendenti19"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_naz_civ_tg24)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, w = cdata$output_plot_AR_naz_civ_tg24_width, h = cdata$output_plot_AR_naz_civ_tg24_height)
    }
  )
  output$info_AR_naz_civ_tg24 <- renderDataTable(
    {
      if(input$is_materia_AR_naz_civ_tg24) {
        if(input$tipo_ufficio_AR_naz_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_naz_civ_tg24], is_materia = T, is_nazionale = T)
        }
        if(input$tipo_ufficio_AR_naz_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_naz_civ_tg24], is_materia = T, is_nazionale = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_naz_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_naz_civ_tg24], is_materia = F, is_nazionale = T)
        }
        if(input$tipo_ufficio_AR_naz_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_naz_civ_tg24], is_materia = F, is_nazionale = T)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      }
      tmp  <- dt[tipo_ufficio == input$tipo_ufficio_AR_naz_civ_tg24][variable == "Pendenti19"]
      info <- as_DT(
        data.table(
          "aggregato"    = "AR civile",
          "tipo ufficio" = input$tipo_ufficio_AR_naz_civ_tg24,
          ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_naz_civ_tg24)[["info"]]
        )
      )
    }
  )
  
  # Vista per distretto ----
  output$plot_AR_dis_civ_tg24 <- renderPlotly(
    {
      if(input$is_materia_AR_dis_civ_tg24) {
        if(input$tipo_ufficio_AR_dis_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_dis_civ_tg24], is_materia = T, is_distretto = T)
        }
        if(input$tipo_ufficio_AR_dis_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_dis_civ_tg24], is_materia = T, is_distretto = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_dis_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_dis_civ_tg24], is_materia = F, is_distretto = T)
        }
        if(input$tipo_ufficio_AR_dis_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_dis_civ_tg24], is_materia = F, is_distretto = T)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      }
      tmp     <- dt[distretto == input$distretto_AR_civ_tg24][tipo_ufficio == input$tipo_ufficio_AR_dis_civ_tg24][variable == "Pendenti19"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_dis_civ_tg24)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, w = cdata$output_plot_AR_dis_civ_tg24_width, h = cdata$output_plot_AR_dis_civ_tg24_height)
    }
  )
  output$info_AR_dis_civ_tg24 <- renderDataTable(
    {
      if(input$is_materia_AR_dis_civ_tg24) {
        if(input$tipo_ufficio_AR_dis_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_dis_civ_tg24], is_materia = T, is_distretto = T)
        }
        if(input$tipo_ufficio_AR_dis_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_dis_civ_tg24], is_materia = T, is_distretto = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_dis_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_dis_civ_tg24], is_materia = F, is_distretto = T)
        }
        if(input$tipo_ufficio_AR_dis_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_dis_civ_tg24], is_materia = F, is_distretto = T)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      }
      tmp  <- dt[distretto == input$distretto_AR_civ_tg24][tipo_ufficio == input$tipo_ufficio_AR_dis_civ_tg24][variable == "Pendenti19"]
      info <- as_DT(
        data.table(
          "aggregato" = "AR civile",
          "distretto" = input$distretto_AR_civ_tg24,
          "tipo ufficio" = input$tipo_ufficio_AR_dis_civ_tg24,
          ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_dis_civ_tg24)[["info"]]
        )
      )
    }
  )
  
  # Vista per ufficio ----
  output$plot_AR_uff_civ_tg24 <- renderPlotly(  # renderPlotly
    {
      if(input$is_materia_AR_uff_civ_tg24) {
        if(input$tipo_ufficio_AR_uff_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_uff_civ_tg24], is_materia = T)
        }
        if(input$tipo_ufficio_AR_uff_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_uff_civ_tg24], is_materia = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_uff_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_uff_civ_tg24], is_materia = F)
        }
        if(input$tipo_ufficio_AR_uff_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_uff_civ_tg24], is_materia = F)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      }
      if(input$tipo_ufficio_AR_uff_civ_tg24 == "Corte di Appello") {
        tmp <- dt[ufficio == input$ufficio_cda_AR_civ_tg24][variable == "Pendenti19"]
      }
      if(input$tipo_ufficio_AR_uff_civ_tg24 == "Tribunale") {
        tmp <- dt[ufficio == input$ufficio_trb_AR_civ_tg24][variable == "Pendenti19"]
      }
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_uff_civ_tg24)[["dt_pred"]]
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, w = cdata$output_plot_AR_uff_civ_tg24_width, h = cdata$output_plot_AR_uff_civ_tg24_height)
    }
  )
  output$info_AR_uff_civ_tg24 <- renderDataTable(
    {
      if(input$is_materia_AR_uff_civ_tg24) {
        if(input$tipo_ufficio_AR_uff_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_uff_civ_tg24], is_materia = T)
        }
        if(input$tipo_ufficio_AR_uff_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_uff_civ_tg24], is_materia = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_uff_civ_tg24 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_uff_civ_tg24], is_materia = F)
        }
        if(input$tipo_ufficio_AR_uff_civ_tg24 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_uff_civ_tg24], is_materia = F)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      }
      if(input$tipo_ufficio_AR_uff_civ_tg24 == "Corte di Appello") {
        tmp  <- dt[ufficio == input$ufficio_cda_AR_civ_tg24][variable == "Pendenti19"]
        info <- as_DT(
          data.table(
            "aggregato" = "AR civile",
            "ufficio"   = input$ufficio_cda_AR_civ_tg24,
            ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_uff_civ_tg24)[["info"]]
          )
        )
      }
      if(input$tipo_ufficio_AR_uff_civ_tg24 == "Tribunale") {
        tmp  <- dt[ufficio == input$ufficio_trb_AR_civ_tg24][variable == "Pendenti19"]
        info <- as_DT(
          data.table(
            "aggregato" = "AR civile",
            "ufficio"   = input$ufficio_trb_AR_civ_tg24,
            ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_uff_civ_tg24)[["info"]]
          )
        )
      }
    }
  )
  
  # Tabella generale.
  # output$table_AR_sez_civ_tg24 <- DT::renderDataTable(
  #   {
  #     tmp <- copy(stra_sez[Ufficio == input$ufficio_trb_AR_civ_tg24][, !c("Data", "Settore", "Distretto", "Tipo Ufficio")])  # [`z Totale` != 0]
  #     setnames(tmp, c("Macroarea CSM", "z Totale"), c("Materia", "Totale Arretrato ultra"))
  #     tmp[, `:=` (
  #       Ufficio = as.factor(Ufficio),
  #       Sezione = as.factor(Sezione),
  #       Materia = as.factor(Materia)
  #     )
  #     ]
  #     DT::datatable( 
  #       tmp,
  #       extensions = c('Scroller','Buttons'),
  #       class      = 'compact cell-border stripe',
  #       rownames   = F,
  #       filter     = list(position = 'top', clear = F, plain = T),
  #       options    = list(
  #         autoWidth   = T,
  #         deferRender = F,
  #         scrollY     = 625,
  #         scroller    = T,
  #         dom         = 'Bfrtip',
  #         buttons     = c('copy', 'print', 'csv', 'excel')
  #       )
  #     )
  #   },
  #   server = F  # per il download completo dei dati
  # )
  
  # ARRETRATO (TARGET 2026) --------------------------------------------------------
  
  # Vista di insieme (tutti i distretti) ----
  
  # plotWidth   <- 1600
  # plotHeight  <- 1600
  # plotQuality <- 1
  # 
  # output$plot_AR_all_dis_civ_tg26 <- renderImage(
  #   {
  #     if(input$tipo_ufficio_AR_all_dis_civ_tg26 == "Corte di Appello") {
  #       out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_all_dis_civ_tg26], is_materia = F, is_distretto = T)
  #     }
  #     if(input$tipo_ufficio_AR_all_dis_civ_tg26 == "Tribunale") {
  #       out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_all_dis_civ_tg26], is_materia = F, is_distretto = T)
  #     }
  #     dt         <- out$dt
  #     facet_vars <- out$facet_vars
  #     facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
  #     tmp        <- dt[tipo_ufficio == input$tipo_ufficio_AR_all_dis_civ_tg26][variable == "Pendenti22"]
  #     distretti  <- tmp[, .N, keyby = distretto][, distretto]
  #     FUN <- function(tmp, title) {
  #       dt_pred <- ftarget(tmp, facet_vars, is_checkbox = F)[["dt_pred"]]
  #       return(fplot(tmp, facet_vars, dt_pred, title = title, is_strip = F))
  #     }
  #     ll <- lapply(distretti, function(d) FUN(tmp[distretto == d], title = d))
  #     outfile <- tempfile(fileext = '.png')  # generate the png -> temp file to save the output (it will be removed later by renderImage)  
  #     png(
  #       outfile,  
  #       width  = plotWidth,
  #       height = plotHeight,
  #       res    = 72*plotQuality
  #     )
  #     print(plot_grid(plotlist = ll, ncol = 4, label_x = 0.5))
  #     dev.off()
  #     list(  # return a list containing the filename.
  #       src         = outfile,
  #       contentType = 'image/png',
  #       width       = plotWidth,
  #       height      = plotHeight,
  #       res         = 72 * plotQuality,
  #       alt         = "Arretrato civile - tutti i distretti"
  #     )
  #   },
  #   deleteFile = TRUE
  # )
  # output$info_AR_all_dis_civ_tg26 <- renderDataTable(
  #   {
  #     if(input$tipo_ufficio_AR_all_dis_civ_tg26 == "Corte di Appello") {
  #       out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_all_dis_civ_tg26], is_materia = F, is_distretto = T)
  #     }
  #     if(input$tipo_ufficio_AR_all_dis_civ_tg26 == "Tribunale") {
  #       out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_all_dis_civ_tg26], is_materia = F, is_distretto = T)
  #     }
  #     dt         <- out$dt
  #     facet_vars <- out$facet_vars
  #     facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
  #     tmp        <- dt[tipo_ufficio == input$tipo_ufficio_AR_all_dis_civ_tg26][variable == "Pendenti22"]
  #     distretti  <- tmp[, .N, keyby = distretto][, distretto]
  #     FUN1 <- function(tmp, tipo_ufficio, distretto) {
  #       info <- ftarget(tmp, facet_vars, is_checkbox = F)[["info"]]
  #       return(data.table("aggregato" = "AR civile", "tipo ufficio" = tipo_ufficio, "distretto" = distretto, info))
  #     }
  #     ii <- lapply(distretti, function(d) FUN1(tmp[distretto == d], tipo_ufficio = input$tipo_ufficio_AR_all_dis_civ_tg26, distretto = d))
  #     as_DT(rbindlist(ii, use.names = T, fill = T))
  #     
  #     # DT::datatable(
  #     #   rbindlist(ii, use.names = T, fill = T),
  #     #   extensions = extensions,
  #     #   class      = class,
  #     #   rownames   = rownames,
  #     #   filter     = filter,
  #     #   options    = options
  #     # ) %>%
  #     #   formatPercentage(c("scenario 1 var% [2026 vs 2019]", "scenario 2 var% [2026 vs 2019]"))
  #   }
  # )
  
  # Vista nazionale ----
  output$plot_AR_naz_civ_tg26 <- renderPlotly(
    {
      if(input$is_materia_AR_naz_civ_tg26) {
        #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_nazionale = T)
        if(input$tipo_ufficio_AR_naz_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_naz_civ_tg26], is_materia = T, is_nazionale = T)
        }
        if(input$tipo_ufficio_AR_naz_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_naz_civ_tg26], is_materia = T, is_nazionale = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_naz_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_naz_civ_tg26], is_materia = F, is_nazionale = T)
        }
        if(input$tipo_ufficio_AR_naz_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_naz_civ_tg26], is_materia = F, is_nazionale = T)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      }
      tmp <- dt[tipo_ufficio == input$tipo_ufficio_AR_naz_civ_tg26][variable == "Pendenti22"]
      tmp <- tmp[date >= "2022-12-31"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_naz_civ_tg26)[["dt_pred"]]
      setnames(dt_pred, "pred_post_upp", "pred_arr_tg26")
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, is_AR_tg26 = T, w = cdata$output_plot_AR_naz_civ_tg26_width, h = cdata$output_plot_AR_naz_civ_tg26_height)
    }
  )
  output$info_AR_naz_civ_tg26 <- renderDataTable(
    {
      if(input$is_materia_AR_naz_civ_tg26) {
        if(input$tipo_ufficio_AR_naz_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_naz_civ_tg26], is_materia = T, is_nazionale = T)
        }
        if(input$tipo_ufficio_AR_naz_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_naz_civ_tg26], is_materia = T, is_nazionale = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_naz_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_naz_civ_tg26], is_materia = F, is_nazionale = T)
        }
        if(input$tipo_ufficio_AR_naz_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_naz_civ_tg26], is_materia = F, is_nazionale = T)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      }
      tmp  <- dt[tipo_ufficio == input$tipo_ufficio_AR_naz_civ_tg26][variable == "Pendenti22"]
      info <- as_DT(
        data.table(
          "aggregato"    = "AR civile",
          "tipo ufficio" = input$tipo_ufficio_AR_naz_civ_tg26,
          ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_naz_civ_tg26)[["info"]]
        )
      )
    }
  )
  
  # Vista per distretto ----
  output$plot_AR_dis_civ_tg26 <- renderPlotly(
    {
      if(input$is_materia_AR_dis_civ_tg26) {
        if(input$tipo_ufficio_AR_dis_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_dis_civ_tg26], is_materia = T, is_distretto = T)
        }
        if(input$tipo_ufficio_AR_dis_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_dis_civ_tg26], is_materia = T, is_distretto = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_dis_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_dis_civ_tg26], is_materia = F, is_distretto = T)
        }
        if(input$tipo_ufficio_AR_dis_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_dis_civ_tg26], is_materia = F, is_distretto = T)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      }
      tmp     <- dt[distretto == input$distretto_AR_civ_tg26][tipo_ufficio == input$tipo_ufficio_AR_dis_civ_tg26][variable == "Pendenti22"]
      tmp <- tmp[date >= "2022-12-31"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_dis_civ_tg26)[["dt_pred"]]
      setnames(dt_pred, "pred_post_upp", "pred_arr_tg26")
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, is_AR_tg26 = T, w = cdata$output_plot_AR_dis_civ_tg26_width, h = cdata$output_plot_AR_dis_civ_tg26_height)
    }
  )
  output$info_AR_dis_civ_tg26 <- renderDataTable(
    {
      if(input$is_materia_AR_dis_civ_tg26) {
        if(input$tipo_ufficio_AR_dis_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_dis_civ_tg26], is_materia = T, is_distretto = T)
        }
        if(input$tipo_ufficio_AR_dis_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_dis_civ_tg26], is_materia = T, is_distretto = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_dis_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_dis_civ_tg26], is_materia = F, is_distretto = T)
        }
        if(input$tipo_ufficio_AR_dis_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_dis_civ_tg26], is_materia = F, is_distretto = T)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      }
      tmp  <- dt[distretto == input$distretto_AR_civ_tg26][tipo_ufficio == input$tipo_ufficio_AR_dis_civ_tg26][variable == "Pendenti22"]
      info <- as_DT(
        data.table(
          "aggregato" = "AR civile",
          "distretto" = input$distretto_AR_civ_tg26,
          "tipo ufficio" = input$tipo_ufficio_AR_dis_civ_tg26,
          ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_dis_civ_tg26)[["info"]]
        )
      )
    }
  )
  
  # Vista per ufficio ----
  output$plot_AR_uff_civ_tg26 <- renderPlotly(  # renderPlotly
    {
      if(input$is_materia_AR_uff_civ_tg26) {
        if(input$tipo_ufficio_AR_uff_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_uff_civ_tg26], is_materia = T)
        }
        if(input$tipo_ufficio_AR_uff_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_uff_civ_tg26], is_materia = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_uff_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_uff_civ_tg26], is_materia = F)
        }
        if(input$tipo_ufficio_AR_uff_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_uff_civ_tg26], is_materia = F)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      }
      if(input$tipo_ufficio_AR_uff_civ_tg26 == "Corte di Appello") {
        tmp <- dt[ufficio == input$ufficio_cda_AR_civ_tg26][variable == "Pendenti22"]
      }
      if(input$tipo_ufficio_AR_uff_civ_tg26 == "Tribunale") {
        tmp <- dt[ufficio == input$ufficio_trb_AR_civ_tg26][variable == "Pendenti22"]
      }
      tmp <- tmp[date >= "2022-12-31"]
      dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_uff_civ_tg26)[["dt_pred"]]
      setnames(dt_pred, "pred_post_upp", "pred_arr_tg26")
      cdata   <- session$clientData
      fplot(tmp, facet_vars, dt_pred, is_AR_tg26 = T, w = cdata$output_plot_AR_uff_civ_tg26_width, h = cdata$output_plot_AR_uff_civ_tg26_height)
    }
  )
  output$info_AR_uff_civ_tg26 <- renderDataTable(
    {
      if(input$is_materia_AR_uff_civ_tg26) {
        if(input$tipo_ufficio_AR_uff_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_uff_civ_tg26], is_materia = T)
        }
        if(input$tipo_ufficio_AR_uff_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_uff_civ_tg26], is_materia = T)
        }
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_AR_uff_civ_tg26 == "Corte di Appello") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_cda_uff_civ_tg26], is_materia = F)
        }
        if(input$tipo_ufficio_AR_uff_civ_tg26 == "Tribunale") {
          out <- create_dt(movi_civ, stra_civ[Materia %in% input$materie_trb_uff_civ_tg26], is_materia = F)
        }
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      }
      if(input$tipo_ufficio_AR_uff_civ_tg26 == "Corte di Appello") {
        tmp  <- dt[ufficio == input$ufficio_cda_AR_civ_tg26][variable == "Pendenti22"]
        info <- as_DT(
          data.table(
            "aggregato" = "AR civile",
            "ufficio"   = input$ufficio_cda_AR_civ_tg26,
            ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_uff_civ_tg26)[["info"]]
          )
        )
      }
      if(input$tipo_ufficio_AR_uff_civ_tg26 == "Tribunale") {
        tmp  <- dt[ufficio == input$ufficio_trb_AR_civ_tg26][variable == "Pendenti22"]
        info <- as_DT(
          data.table(
            "aggregato" = "AR civile",
            "ufficio"   = input$ufficio_trb_AR_civ_tg26,
            ftarget(tmp, facet_vars, is_checkbox = input$is_materia_AR_uff_civ_tg26)[["info"]]
          )
        )
      }
    }
  )
  
  # Tabella generale.
  output$table_AR_sez_civ_tg26 <- DT::renderDataTable(
    {
      if(input$tipo_ufficio_AR_uff_civ_tg26 == "Corte di Appello") {
        tmp <- copy(stra_sez[Ufficio == input$ufficio_cda_AR_civ_tg26][, !c("Data", "Settore", "Distretto", "Tipo Ufficio")])  # [`z Totale` != 0]
      }
      if(input$tipo_ufficio_AR_uff_civ_tg26 == "Tribunale") {
        tmp <- copy(stra_sez[Ufficio == input$ufficio_trb_AR_civ_tg26][, !c("Data", "Settore", "Distretto", "Tipo Ufficio")])  # [`z Totale` != 0]
      }
      setnames(tmp, c("Macroarea CSM", "z Totale"), c("Materia", "Totale Arretrato ultra"))
      tmp[, `:=` (
        Ufficio = as.factor(Ufficio),
        Sezione = as.factor(Sezione),
        Materia = as.factor(Materia)
      )
      ]
      DT::datatable( 
        tmp,
        extensions = c('Scroller','Buttons'),
        class      = 'compact cell-border stripe',
        rownames   = F,
        filter     = list(position = 'top', clear = F, plain = T),
        options    = list(
          autoWidth   = T,
          deferRender = F,
          scrollY     = 625,
          scroller    = T,
          dom         = 'Bfrtip',
          buttons     = c('copy', 'print', 'csv', 'excel')
        )
      )
    },
    server = F  # per il download completo dei dati
  )
  
  # FLUSSI CIVILE (SOPRAVVENUTI E DEFINITI) --------------------------------------------------------
  
  # Vista nazionale ----
  output$plot_Fls_naz_civ <- renderPlotly(
    {
      if(input$is_materia_Fls_naz_civ) {
        
        if(input$tipo_ufficio_Fls_naz_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_cda_naz_civ], stra_civ, is_materia = T, is_nazionale = T)
        }
        if(input$tipo_ufficio_Fls_naz_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_trb_naz_civ], stra_civ, is_materia = T, is_nazionale = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_nazionale = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_Fls_naz_civ == "Cassazione") {
          out <- create_dt(movi_civ, stra_civ, is_materia = F, is_nazionale = T)
        }
        if(input$tipo_ufficio_Fls_naz_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_cda_naz_civ], stra_civ, is_materia = F, is_nazionale = T)
        }
        if(input$tipo_ufficio_Fls_naz_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_trb_naz_civ], stra_civ, is_materia = F, is_nazionale = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_nazionale = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("tipo_ufficio", "variable")]
      }
      tmp     <- dt[tipo_ufficio == input$tipo_ufficio_Fls_naz_civ][variable %in% c("Sopravvenuti", "Definiti")]
      #dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_naz_civ)[["dt_pred"]]
      cdata   <- session$clientData
      fplot_flussi(tmp, facet_vars, w = cdata$output_plot_Fls_naz_civ_width, h = cdata$output_plot_Fls_naz_civ_height)
    }
  )
  
  # Vista per distretto ----
  output$plot_Fls_dis_civ <- renderPlotly(
    {
      if(input$is_materia_Fls_dis_civ) {
        if(input$tipo_ufficio_Fls_dis_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_cda_dis_civ], stra_civ, is_materia = T, is_distretto = T)
        }
        if(input$tipo_ufficio_Fls_dis_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_trb_dis_civ], stra_civ, is_materia = T, is_distretto = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = T, is_distretto = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_Fls_dis_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_cda_dis_civ], stra_civ, is_materia = F, is_distretto = T)
        }
        if(input$tipo_ufficio_Fls_dis_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_trb_dis_civ], stra_civ, is_materia = F, is_distretto = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = F, is_distretto = T)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "variable")]
      }
      tmp     <- dt[distretto == input$distretto_Fls_civ][tipo_ufficio == input$tipo_ufficio_Fls_dis_civ][variable %in% c("Sopravvenuti", "Definiti")]
      #dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_dis_civ)[["dt_pred"]]
      cdata   <- session$clientData
      fplot_flussi(tmp, facet_vars, w = cdata$output_plot_Fls_dis_civ_width, h = cdata$output_plot_Fls_dis_civ_height)
    }
  )
  
  # Vista per ufficio ----
  output$plot_Fls_uff_civ <- renderPlotly(
    {
      if(input$is_materia_Fls_uff_civ) {
        if(input$tipo_ufficio_Fls_uff_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_cda_uff_civ], stra_civ, is_materia = T)
        }
        if(input$tipo_ufficio_Fls_uff_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_trb_uff_civ], stra_civ, is_materia = T)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = T)
        dt         <- out$dt[!materia %in% da_escludere]
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      } else {
        if(input$tipo_ufficio_Fls_uff_civ == "Corte di Appello") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_cda_uff_civ], stra_civ, is_materia = F)
        }
        if(input$tipo_ufficio_Fls_uff_civ == "Tribunale") {
          out <- create_dt(movi_civ[Materia %in% input$materie_Fls_trb_uff_civ], stra_civ, is_materia = F)
        }
        #out        <- create_dt(movi_civ, stra_civ, is_materia = F)
        dt         <- out$dt
        facet_vars <- out$facet_vars
        facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
      }
      if(input$tipo_ufficio_Fls_uff_civ == "Corte di Appello") {
        tmp <- dt[ufficio == input$ufficio_cda_Fls_civ][variable %in% c("Sopravvenuti", "Definiti")]
      }
      if(input$tipo_ufficio_Fls_uff_civ == "Tribunale") {
        tmp <- dt[ufficio == input$ufficio_trb_Fls_civ][variable %in% c("Sopravvenuti", "Definiti")]
      }
      #tmp <- dt[ufficio == input$ufficio_DT_civ][variable == "Disposition Time"]
      if(input$is_materia_Fls_uff_civ) {
        if(nrow(tmp[date == "2019-12-31" & materia == "ip Immigrazione e Prot. Internazionale"]) == 0) {  # trucco per evitare di plottare questa materia quando non si dovrebbe (per esempio, nel caso dei tribunali non distrettuali -> caso IVREA)
          tmp <- tmp[!materia %in% c("ip Immigrazione e Prot. Internazionale")]
        }
      }
      #dt_pred <- ftarget(tmp, facet_vars, is_checkbox = input$is_materia_DT_uff_civ)[["dt_pred"]]
      cdata   <- session$clientData
      fplot_flussi(tmp, facet_vars, w = cdata$output_plot_Fls_uff_civ_width, h = cdata$output_plot_Fls_uff_civ_height)
    }
  )
  
  # # SCATTER PLOT (TURNOVER MAGISTRATI vs ARRETRATO PRO-CAPITE) ---------------------------
  # 
  # # Scatter plot.
  # output$plot_TO_MAG_civ <- renderPlotly(  # renderPlotly
  #   {
  #     out        <- create_dt(movi_civ, stra_civ, is_materia = F)
  #     dt         <- out$dt
  #     facet_vars <- out$facet_vars
  #     facet_vars <- facet_vars[!facet_vars %in% c("distretto", "tipo_ufficio", "ufficio", "variable")]
  #     tmp <- merge(
  #       dt[variable == "Pendenti19"][date == "2022-12-31"],
  #       turn_mag[, .(Ufficio, `Turnover Totale` = round(`Turnover Totale`, 2))],
  #       by.x = "ufficio",
  #       by.y = "Ufficio"
  #     )
  #     tmp <- merge(
  #       tmp,
  #       orga_mag[, .(ufficio, organico)],
  #       by = "ufficio"
  #     )
  #     tmp[, value := round(value/organico, 2)]  # arretrato pro-capite
  #     sp <- splot(tmp, lvar = "arretrato pro-capite", sede = "ufficio", xlab = "turnover magistrati 2017-2021", ylab = "arretrato pro-capite civile 2022")
  #     sp
  #   }
  # )
  
  # ARRETRATO (PER SEZIONE - solo ultimo semestre) --------------------------------------------------------
  
  # Tabella generale.
  # output$table_AR_sez_civ <- DT::renderDataTable(
  #   {
  #     tmp <- copy(stra_sez[Ufficio == input$ufficio_AR_sez_civ][, !c("Data", "Settore", "Distretto", "Tipo Ufficio")])  # [`z Totale` != 0]
  #     setnames(tmp, c("Macroarea CSM", "z Totale"), c("Materia", "Totale Arretrato ultra"))
  #     tmp[, `:=` (
  #       Ufficio = as.factor(Ufficio),
  #       Sezione = as.factor(Sezione),
  #       Materia = as.factor(Materia)
  #       )
  #     ]
  #     DT::datatable( 
  #       tmp,
  #       extensions = c('Scroller','Buttons'),
  #       class      = 'compact cell-border stripe',
  #       rownames   = F,
  #       filter     = list(position = 'top', clear = F, plain = T),
  #       options    = list(
  #         autoWidth   = T,
  #         deferRender = F,
  #         scrollY     = 625,
  #         scroller    = T,
  #         dom         = 'Bfrtip',
  #         buttons     = c('copy', 'print', 'csv', 'excel')
  #       )
  #     )
  #   },
  #   server = F  # per il download completo dei dati
  # )
  
  # CODICI OGGETTO PNRR -----------------------------------------------------
  
  # DT civile (solo ultimo semestre) ----
  output$table_cdx_oggetto_DT_civ <- renderDataTable(
    {
      tmp <- copy(movi_civ[Ufficio == input$ufficio_cdx_oggetto_DT_civ][Data %in% c("2024-12-31")])
      tmp[, `Disposition Time` := ifelse(`Definiti` > 0, round((`Pendenti Finali`/`Definiti`)*365, 0), NA)]
      setorder(tmp, Data, Materia, `Codice Oggetto`, Oggetto)
      tmp <- tmp[, .(
        #Data    = as.factor(Data),
        Ufficio = as.factor(Ufficio),
        Materia = as.factor(Materia),
        `Codice Oggetto`,
        Oggetto = as.factor(Oggetto),
        Sopravvenuti,
        Definiti,
        `Pendenti Finali`,
        `Disposition Time`
      )
      ]
      DT::datatable(
        tmp,
        extensions = c('Scroller','Buttons'),
        class      = 'compact cell-border stripe',
        rownames   = F,
        filter     = list(position = 'top', clear = F, plain = T),
        options    = list(
          autoWidth   = T,
          deferRender = F,
          scrollY     = 625,
          scroller    = T,
          dom         = 'Bfrtip',
          buttons     = c('copy', 'print', 'csv', 'excel')
        )
      )
    },
    server = F  # per il download completo dei dati
  )
  
  # AR civile (solo ultimo semestre) ----
  output$table_cdx_oggetto_AR_civ <- renderDataTable(
    {
      tmp <- copy(stra_civ[Ufficio == input$ufficio_cdx_oggetto_AR_civ][Data %in% c("2024-12-31")])  #[Arretrato != 0])
      setorder(tmp, Data, Materia, `Codice Oggetto`, Oggetto)
      tmp <- tmp[, .(
        #Data                     = as.factor(Data),
        Ufficio                  = as.factor(Ufficio),
        Materia                  = as.factor(Materia),
        `Codice Oggetto`,
        Oggetto                  = as.factor(Oggetto),
        `Arretrato`              = Pendenti22
      )
      ]
      DT::datatable(
        tmp,
        extensions = c('Scroller','Buttons'),
        class      = 'compact cell-border stripe',
        rownames   = F,
        filter     = list(position = 'top', clear = F, plain = T),
        options    = list(
          autoWidth   = T,
          deferRender = F,
          scrollY     = 625,
          scroller    = T,
          dom         = 'Bfrtip',
          buttons     = c('copy', 'print', 'csv', 'excel')
        )
      )
    },
    server = F  # per il download completo dei dati
  )
  
  # --- #
}
)
# Create the Shiny app
shinyApp(ui = ui, server = server)

