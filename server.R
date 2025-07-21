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
load_packages()
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
