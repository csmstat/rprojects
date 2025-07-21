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
      #icon = icon("clock"),
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
      #icon = icon("clock"),
      menuSubItem("Complessivo", tabName = "DT_complessivo_pen"),
      menuSubItem("Nazionale",   tabName = "DT_nazionale_pen"),
      menuSubItem("Distretto",   tabName = "DT_per_distretto_pen"),
      menuSubItem("Ufficio",     tabName = "DT_per_ufficio_pen")
    ),
    menuItem(
      "AR civile (target 2024)",
      tabName = "AR_civile",
      #icon = icon("layer-group"),  # stack-overflow
      #menuSubItem("AR (tutti i distretti)", tabName = "AR_tutti_i_distretti_tg24"),
      menuSubItem("Nazionale", tabName = "AR_nazionale_tg24"),
      menuSubItem("Distretto", tabName = "AR_per_distretto_tg24"),
      menuSubItem("Ufficio",   tabName = "AR_per_ufficio_tg24")
      #menuSubItem("Arretrato vs Turnover",  tabName = "AR_vs_TO_MAG")
    ),
    menuItem(
      "AR civile (target 2026)",
      tabName = "AR_civile",
      #icon = icon("layer-group"),
      #menuSubItem("AR (tutti i distretti)", tabName = "AR_tutti_i_distretti_tg26"),
      menuSubItem("Nazionale", tabName = "AR_nazionale_tg26"),
      menuSubItem("Distretto", tabName = "AR_per_distretto_tg26"),
      menuSubItem("Ufficio",   tabName = "AR_per_ufficio_tg26")
      #menuSubItem("Arretrato vs Turnover",  tabName = "AR_vs_TO_MAG")
    ),
    menuItem(
      "Flussi civile",
      tabName = "Fls_civile",
      #icon = icon("stack-overflow"),
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
