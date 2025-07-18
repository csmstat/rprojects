note <- function() {
  not <-
    div(
      style = "text-align:justify;color:gray",
      h5(strong("DISPOSITION TIME (DT)"), style = "color:#D81B60"),
      p(
        strong("Complessivo"), "ottenuto come somma dei DT nei tre gradi di giudizio."
      ),
      p(
        "È il rapporto tra il numero dei procedimenti pendenti e il numero dei procedimenti definiti nel periodo di riferimento
        (moltiplicato per 365 o per 182,5 - giorni - a seconda che i dati siano riferiti al 31-12 o al 30-06)."
      ),
      p(
        "È una stima del tempo medio atteso di definizione dei procedimenti, ovvero del tempo necessario (in giorni)
        a smaltire le pendenze nell'ipotesi che il ritmo delle definizioni si mantenga pari a quello del periodo di riferimento."
      ),
      p(
        "Nel calcolo del", strong("DT civile") ,"rientrano esclusivamente i procedimenti contenziosi secondo la classificazione proposta dalla CEPEJ."
      ),
      p(
        "Secondo questa classificazione, per il", strong("Tribunale"), "l’aggregato di riferimento include i procedimenti relativi ai
        seguenti ruoli: Affari civili contenziosi con l’esclusione delle separazioni e dei divorzi consensuali,
        Controversie agrarie, Controversie in materia di lavoro, previdenza, assistenza obbligatoria (inclusi gli speciali
        e gli ATP). Sono invece esclusi i ruoli degli Affari di volontaria giurisdizione, dei Procedimenti speciali e
        sommari, nonché le procedure esecutive e concorsuali con l’eccezione delle istanze di fallimento,
        e a partire dal 15/07/22 dei ricorsi per liquidazione giudiziale, che a seguito dell’entrata in vigore del
        Nuovo Codice della Crisi e dell’Insolvenza hanno sostituito le istanze di fallimento.
        Il Ruolo Affari civili contenziosi include, conteggiandoli nella relativa materia, anche i procedimenti
        trattati con rito sommario di cognizione ex art. 702 bis."
      ),
      p(
        "Per la", strong("Corte di Appello"), "l’aggregato di riferimento include i procedimenti relativi ai seguenti ruoli:
        Affari civili contenziosi, con l’esclusione delle separazioni e dei divorzi consensuali, Controversie agrarie e Controversie
        in materia di lavoro, previdenza, assistenza obbligatoria. Sono escluse le altre tipologie di procedimento, con
        l’unica eccezione dei procedimenti di Equa riparazione."
      ),
      p(
        "Nel calcolo del", strong("DT penale") ,"i criteri richiesti dalla Commissione europea coincidono con quelli utilizzati per le statistiche ufficiali nazionali."
      ),
      p(
        "In base a questi criteri, per il", strong("Tribunale"), "sono considerati i procedimenti relativi
        alle sezioni gip-gup autore noto (mod.20 registro generale degli uffici del giudice per le indagini preliminari
        presso i tribunali), dibattimento I grado (mod.16 registro generale dei tribunali in composizione collegiale e
        monocratica), dibattimento II grado (mod.7 bis registro delle impugnazioni davanti ai tribunali in
        composizione monocratica) e assise (mod.19 registro generale delle corti di assise). Sono pertanto escluse le
        decisioni interlocutorie del Gip, i procedimenti di competenza del Tribunale del riesame e i procedimenti per
        l’applicazione di misure di prevenzione."
      ),
      p(
        "Per la", strong("Corte di Appello"), "l’aggregato di riferimento è dato dai procedimenti relativi alle sezioni ordinaria, assise
        e minorenni (mod.7 registro generale delle corti di appello e corti di assise d’appello)."
      ),
      # p(
      #   "Sono quindi esclusi i ruoli degli affari di volontaria giurisdizione (ed eccezione dell’equa riparazione in corte di appello) e dei procedimenti speciali e sommari,
      #   nonché le separazioni e i divorzi consensuali e le procedure esecutive e concorsuali."
      # ),
      # p(
      #   "Sono invece conteggiate le istanze di fallimento e, a partire dal 15/07/22,
      #   i ricorsi per liquidazione giudiziale, che a seguito dell’entrata in vigore del Nuovo Codice della Crisi e dell’Insolvenza hanno sostituito le istanze di fallimento."
      # ),
      hr(),
      h5(strong("ARRETRATO CIVILE (AR)"), style = "color:#D81B60"),
      p(
        "Ai fini del", strong("target intermedio (2024)"), "è l'insieme dei procedimenti pendenti al 31/12/2019 che hanno superato i termini di ragionevole durata fissati dalla legge: 3 anni nei tribunali e 2 anni nelle corti di appello."
      ),
      p(
        "Ai fini del", strong("target finale (2026)"), "è l'insieme dei procedimenti iscritti fino al 31/12/2022 ancora pendenti a questa data, e dunque suscettibili di superare i termini di ragionevole durata se non definiti entro la scadenza del Piano."
      ),
      p(
        "Per questi obiettivi, rilevano tutti i procedimenti di area SICID (Affari civili contenziosi, Controversie agrarie, Controversie in materia di lavoro, previdenza,
        assistenza obbligatoria, Affari di volontaria giurisdizione, Procedimenti speciali e sommari), con l’esclusione, per il solo Tribunale,
        della materia del Giudice tutelare, dell’Accertamento tecnico preventivo in materia previdenziale (ATP) e
        dell’attività di Ricevimento e Verbalizzazione di dichiarazione giurata."
      )
    )
  return(not)
}