#' Naam        : server.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module bevat de server.


shinyServer(function(input, output, session) {

  # Serverstappen in een vast formaat. Verwerking eenvoudig
  # terug te vinden en chronologisch te volgen.
  log_serverstap <- function(proces, stap, details = NULL) {
    bericht <- paste0("[", proces, "] ", stap)
    if (!is.null(details) && length(details) > 0) {
      details <- paste(details, collapse = " ")
      if (nzchar(details)) {
        bericht <- paste0(bericht, " - ", details)
      }
    }
    logdebug(msg = bericht)
  }

  # vul plaatje bij opstarten
  output$plaatje <- renderImage({
      return(list(
        src = alg_vind_plaatje(),
        contentType = "image/png",
        alt = "Face", height="420", width="420"
      ))
  }, deleteFile = FALSE)


  # Na invullen inputmap: werk selectiemenu's van alle inputbestanden bij
  # en toon verwerkingsmaand boven inputmap
  observeEvent(input$dir_input, {
    logdebug(msg="Inputmap is ingevuld")
    
    # werk selectiemenu's bij
    for (selmenu in c("input_aant_rdvl", "input_aant_wtvl", "input_gemg_rdvl",
                      "input_gemg_kalveren", "input_gemg_kalveren_vj",
                      "input_rvo_slachtingen", "input_rvo_slachthuizen",
                      "output_rvo_naar_spek")) {
      res <- alg_vind_selectie(jaarmaandmap=input$dir_input, selmenu=selmenu)
      updateSelectInput(session, inputId=selmenu,
                        choices=res$selectie, selected=res$kies)
    }
    # toon verwerkingsmaand boven inputmap
    lengte <- nchar(input$dir_input)
    vwmd   <- substr(input$dir_input, lengte-5, lengte)
    output$txt_vwmd <- renderText(vwmd)
  })

  paden <- reactiveValues(
    rvo_spek_input = NULL
  )
  
  #----
  # shiny server toont pop-up met fouten
  toon_harde_fout <- function(titel, fout) {
    melding <- if (inherits(fout, "condition")) {
      conditionMessage(fout)
    } else {
      paste(fout, collapse = "\n")
    }
    melding <- enc2utf8(melding)
    melding <- iconv(melding, from = "", to = "UTF-8", sub = ".")
    
    logerror(msg = paste(titel, melding, sep = " - "))
    
    shinyalert(
      title = titel,
      text = melding,
      type = "error"
    )
    
    invisible(NULL)
  }

  
  
  observeEvent(input$btn_spek_input_maken, {
   
    con <- NULL
    loginfo(msg = "Gebruiker start de verwerking RVO naar SPEK")
    
    tryCatch({
      log_serverstap(
        proces = "RVO-SPEK",
        stap = "Verbinding maken met verwerkingsdatabase",
        details = paste(App$dbservernaam, App$databasenaam, sep = "/")
      )
      con <- DBI::dbConnect(odbc::odbc(),
                            Driver = App$dbdrivernaam,
                            Server = App$dbservernaam,
                            Database = App$databasenaam
      )
      log_serverstap("RVO-SPEK", "Databaseverbinding is geopend")
    
   
   #-------------------------------------------------------------- 
    inputmap <- input$dir_input
    jrmnd <- alg_vind_jaarmaanden(inputmap=inputmap)
    
    pad_rvo_slachtingen <- file.path(inputmap, input$input_rvo_slachtingen)
    pad_rvo_slachthuizen <- file.path(inputmap, input$input_rvo_slachthuizen)
    log_serverstap(
      proces = "RVO-SPEK",
      stap = "Invoer bepaald",
      details = glue::glue(
        "verwerkingsmaand={jrmnd$vwmd}; ",
        "slachtingen={basename(pad_rvo_slachtingen)}; ",
        "slachthuizen={basename(pad_rvo_slachthuizen)}"
      )
    )
    
    # controle op slachtingenbestand
    # controleer_inlees_bestand(pad_rvo_slachtingen,
  #                             con = con)
    
    withProgress(message="", min=0, max = 8, {
      
      mld <- "inputbestanden inlezen."
      stap <- 1
      setProgress(message = mld, value = stap)
      log_serverstap("KV1", mld)
      # KV1 ---------------------------------------------------------------
      df_slachtingen <- inl_lees_csv_bestand_met_kolommen_config(
        pad =  pad_rvo_slachtingen,
        kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
        inlezen_functie = readr::read_csv,
        locale =readr::locale(encoding = "UTF-8"),
        map_fouten = App$logmap,
        na =  App$na_waardes$i_en_r_runderen_slachtingen
      )
      log_serverstap(
        "KV1",
        "RVO-slachtingen ingelezen",
        glue::glue("{nrow(df_slachtingen)} rijen; {ncol(df_slachtingen)} kolommen")
      )
      
      df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
        pad =  pad_rvo_slachthuizen,
        kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
        inlezen_functie = readr::read_csv,
        locale =readr::locale(encoding = "UTF-8"),
        map_fouten = App$logmap,
        na =  App$na_waardes$i_en_r_runderen_slachtplaatsen
      )
      log_serverstap(
        "KV1",
        "RVO-slachthuizen ingelezen",
        glue::glue("{nrow(df_slachthuizen)} rijen; {ncol(df_slachthuizen)} kolommen")
      )

      log_serverstap("KV1", "SKAL-data ophalen")
     df_skal <- oph_haal_skal_op(
        qry_pad = file.path(App$srcmap, "sql", "haal_skal_data_op.sql"),
        hoofsbi = App$rund_skal_hoofdsbi,
        dbdrivernaam=App$skal_dbdrivernaam,
        dbservernaam=App$skal_dbservernaam,
        databasenaam=App$skal_databasenaam,
        dbschema=App$skal_databaseschema,
        tbl_activiteiten = App$tbl_skal_activiteiten,
        tbl_locaties = App$tbl_skal_locaties,
        tbl_certificaten = App$tbl_skal_certificaten,
        tbl_bedrijven = App$tbl_skal_bedrijven
      )
      log_serverstap(
        "KV1",
        "SKAL-data opgehaald",
        glue::glue("{nrow(df_skal)} rijen; {ncol(df_skal)} kolommen")
      )
     
      log_serverstap("KV1", "Landbouwtelling inlezen")
      df_landbouwtelling <- inl_lees_sav_bestand_met_kolommen_config(
        pad =  App$landbouwtellingpad,
        kolommen_config = App$kolomnamen$landbouwtelling,
        map_fouten = App$logmap
      )
      log_serverstap(
        "KV1",
        "Landbouwtelling ingelezen",
        glue::glue("{nrow(df_landbouwtelling)} rijen; {ncol(df_landbouwtelling)} kolommen")
      )
      
      log_serverstap("KV1", "Verblijfsplaatsen inlezen")
      df_verblijfsplaatsen <- inl_lees_sav_bestand_met_kolommen_config(
        pad =  App$verblijfsplaatsenpad,
        kolommen_config = App$kolomnamen$verblijfsplaatsen,
        map_fouten = App$logmap
      )
      log_serverstap(
        "KV1",
        "Verblijfsplaatsen ingelezen",
        glue::glue("{nrow(df_verblijfsplaatsen)} rijen; {ncol(df_verblijfsplaatsen)} kolommen")
      )
      log_serverstap("KV1", "Alle invoerbronnen zijn ingelezen")
      # browser()
      mld <- "Controles toepassen op ruwe gegevens."
      stap <- stap + 1
      setProgress(message = mld, value = stap)
      log_serverstap("KV2", mld)
    
      # KV2 ----------------------------------------------------------------
      
      ## Controles ---------------------------------------------------------
      ### Slachtingen ------------------------------------------------------
      
      if (file.exists(file.path(App$configmap, App$kv2_controles_rvo_slachtingen))) { 
        log_serverstap(
          "KV2",
          "Automatische controles RVO-slachtingen starten",
          basename(App$kv2_controles_rvo_slachtingen)
        )
        
        kv2_controles_slachtingen <- controles_log(
          datasetnaam = "slachtingen",
          pad_inputbestand = pad_rvo_slachtingen,
          pad_sjablon = file.path(App$sjablonenmap, "kv2_controles.qmd"),
          pad_outputmap = App$controlesmap,
          pad_tempmap = tempdir(),
          regels = validate::validator(
            .file = file.path(App$configmap, App$kv2_controles_rvo_slachtingen)),
          df = df_slachtingen,
          id_kolommen = "levens_nr",
          alleen_gebruikt_kolommen = FALSE
        )
        
        
        if(any(kv2_controles_slachtingen$controles_result$fails > 0)) {
          
          mld <- glue::glue(
            "Er zijn fouten gevonden in de automatische controles die zijn",
            " toegepast op het invoerbestand van RVO slachtingen.",
            " Het RVO naar SPEK-proces wordt daarom gestopt.",
            " Zie het bestand {kv2_controles_slachtingen$pad_logbestand}",
            " voor een volledig rapport van alle fouten.")
          
          logerror(msg = mld)
          
          shinyalert(
            title = "Technische controlefout RVO slachtingen.",
            text = mld,
            type = "info"
          )
          setProgress(message = "Er was een milde technische fout maar proces is niet gestopt.", value = 8)
          req(FALSE)
          
        } else {
          mld <- glue::glue(
            "Er zijn GEEN fouten gevonden in de automatische controles die zijn",
            " toegepast op het invoerbestand van RVO slachtingen.",
            " Zie het bestand {kv2_controles_slachtingen$pad_logbestand}.")
        }
        
        logdebug(msg = mld)
        
        shinyalert(
          title = "Controles voor RVO slachtingen bestand.",
          text = mld,
          type = "info"
        )
        log_serverstap(
          "KV2",
          "Automatische controles RVO-slachtingen afgerond",
          kv2_controles_slachtingen$pad_logbestand
        )
      } else {
        mld <- glue::glue(
          "Het yml-bestand voor de slachtingen-controles bestaat niet. ",
          "Het verwachte bestand zou gevonden moeten worden in: ", 
          "{file.path(App$configmap, App$kv2_controles_rvo_slachtingen)}\n",
          "Er werden geen controles toegepast op deze dataset.")
        
        logdebug(mld)
        
        shinyalert(
          title = "Controles voor RVO slachtingen bestand.",
          text = mld,
          type = "info"
        )
        
        # setProgress(message = "Proces gestopt.", value = 8)
        req(FALSE)
        }

      df_n_slachtingen <- df_slachtingen |>
        dplyr::count(levens_nr, eld_code)

      df_niet_uniek_slachtingen <- df_n_slachtingen |>
        dplyr::filter(n > 1)

      if(nrow(df_niet_uniek_slachtingen) > 0) {

        dubbele_regels <- df_niet_uniek_slachtingen |>
          dplyr::mutate(
            code = glue::glue("* levens_nr: {levens_nr}; eld_code: {eld_code}; aantal: {n}")
          ) |>
          dplyr::pull(code) |>
          paste0(collapse = " \n ")


        mld <- glue::glue(
          "We hebben dubbele slachtingen gedetecteerd in de RVO-gegevens op ",
          "basis van het levensnummer en de ELD-code. Dit proces kan niet ",
          "doorgaan omdat slachtingen uniek moeten zijn. Verwijder de dubbele ",
          "records uit het inputbestand en voer deze stap opnieuw uit.\n",
          "De  dubbele levensnummer+ELD code zijn:\n {dubbele_regels}\n",
          "Je kunt deze melding ook vinden in het logbestand."
        )

        shinyalert(
          title = "Controles voor RVO slachtingen bestand.",
          text = mld,
          type = "error"
        )

        logerror(mld)
        setProgress(message = "Proces gestopt.", value = 8)
      }

      req(nrow(df_niet_uniek_slachtingen) == 0)
      log_serverstap(
        "KV2",
        "Uniciteitscontrole RVO-slachtingen geslaagd",
        glue::glue("{nrow(df_slachtingen)} records gecontroleerd")
      )

      
      ### Slachthuizen ------------------------------------------------------
      if (file.exists(file.path(App$configmap, App$kv2_controles_rvo_slachthuizen))) {
        log_serverstap(
          "KV2",
          "Automatische controles RVO-slachthuizen starten",
          basename(App$kv2_controles_rvo_slachthuizen)
        )
        
        kv2_controles_slachthuizen <- controles_log(
          datasetnaam = "slachthuizen",
          pad_inputbestand = pad_rvo_slachthuizen,
          pad_sjablon = file.path(App$sjablonenmap, "kv2_controles.qmd"),
          pad_outputmap = App$controlesmap,
          pad_tempmap = tempdir(),
          regels = validate::validator(
            .file = file.path(App$configmap, App$kv2_controles_rvo_slachthuizen)),
          df = df_slachthuizen,
          id_kolommen = "ubn",
          alleen_gebruikt_kolommen = FALSE
        )
    
        
        if(any(kv2_controles_slachthuizen$controles_result$fails > 0)) {
          
          mld <- glue::glue(
            "Er zijn fouten gevonden in de automatische controles die zijn",
            " toegepast op het invoerbestand van RVO slachthuizen. ",
            " Het RVO naar SPEK-proces wordt daarom gestopt.",
            " Zie het bestand {kv2_controles_slachthuizen$pad_logbestand}",
            " voor een volledig rapport van alle fouten.")
          logerror(msg = mld)
          shinyalert(
            title = "Technische controle fout RVO slachthuizen.",
            text = mld,
            type = "error"
          )
          setProgress(message = "Proces gestopt.", value = 8)
          return(invisible(NULL))
          
        } else {
          mld <- glue::glue(
            "Er zijn GEEN fouten gevonden in de automatische controles die zijn",
            " toegepast op het invoerbestand van RVO slachthuizen",
            " Zie het bestand {kv2_controles_slachthuizen$pad_logbestand}.")
        
        
        logdebug(msg = mld)
        
        shinyalert(
          title = "Controles voor RVO slachthuizen bestand.",
          text = mld,
          type = "info"
        )
        log_serverstap(
          "KV2",
          "Automatische controles RVO-slachthuizen afgerond",
          kv2_controles_slachthuizen$pad_logbestand
        )
      }
        
      } else {
        mld <- glue::glue(
          "Het yml-bestand voor de slachthuizen-controles bestaat niet. ",
          "Het verwachte bestand zou gevonden moeten worden in: ", 
          "{file.path(App$configmap, App$kv2_controles_rvo_slachthuizen)}\n",
          "Er werden geen controles toegepast op deze dataset.")
        
        logdebug(mld)
        
        shinyalert(
          title = "Controles voor RVO slachthuizen bestand.",
          text = mld,
          type = "warning"
        )
      }
      
      df_n_slachthuizen <- df_slachthuizen |> 
        dplyr::count(ubn) 
      
      df_niet_uniek_slachthuizen <- df_n_slachthuizen |> 
        dplyr::filter(n > 1)
      
      if(nrow(df_niet_uniek_slachthuizen) > 0) {
        
        dubbele_regels <- df_niet_uniek_slachthuizen |> 
          dplyr::mutate(
            code = glue::glue("* ubn: {ubn}; aantal: {n}")
          ) |> 
          dplyr::pull(code) |> 
          paste0(collapse = " \n ")
        
        mld <- glue::glue(
          "We hebben dubbele slachthuizen gedetecteerd in de RVO-gegevens op ",
          "basis van het UBN. Dit proces kan niet ",
          "doorgaan omdat slachtingen uniek moeten zijn. Verwijder de dubbele ",
          "records uit het inputbestand en voer deze stap opnieuw uit.\n",
          "De  dubbele UBNs zijn:\n {dubbele_regels}\n",
          "Je kunt deze melding ook vinden in het logbestand."
        )
        
        shinyalert(
          title = "Controles voor RVO slachthuizen bestand.",
          text = mld,
          type = "error"
        )
        
        logerror(mld)
        setProgress(message = "Proces gestopt.", value = 8)
      }
      
      req(nrow(df_niet_uniek_slachthuizen) == 0)
      log_serverstap(
        "KV2",
        "Uniciteitscontrole RVO-slachthuizen geslaagd",
        glue::glue("{nrow(df_slachthuizen)} records gecontroleerd")
      )
      
      ## KV2  verwerken en naar database -------------------------------------
      
      mld <- "KV1 naar KV2"
      stap <- stap + 1
      setProgress(message = mld, value = stap)
      log_serverstap("KV2", mld)
      
      data.frame(
        bestandsnaam = basename(pad_rvo_slachtingen),
        bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
        verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ) |>
        DBI::dbWriteTable(
          conn = con,
          name = DBI::Id(App$databaseschema, "tbl_levering_rvo"),
          value = _,
          append = TRUE
        )
      
      lev_tabel <- DBI::Id(schema = App$databaseschema, 
                          table = "tbl_levering_rvo")
      
      lev_id <- glue::glue_sql(
        "SELECT MAX(levering_id) AS id FROM {`lev_tabel`}",
                      .con = con
      ) |> 
      DBI::dbGetQuery(conn = con, statement = _) |>
        dplyr::pull(id)
      log_serverstap(
        "KV2",
        "Levering geregistreerd",
        glue::glue("levering_id={lev_id}")
      )
      
      kv2_datum <- Sys.time()
      kv2_datum_f <- format(kv2_datum, "%Y-%m-%d %H:%M:%S")
      
      rvo_dieren_en_locaties_invullen(
        df_slachtingen = df_slachtingen,
        df_slachthuizen = df_slachthuizen,
        con = con,
        lev_id = lev_id,
        kv = 2,
        vwmd = jrmnd$vwmd,
        tbl_dieren = App$tbl_dieren,
        tbl_locaties = App$tbl_locaties,
        tbl_dieren_op_locaties = App$tbl_dieren_op_locaties,
        datum_f = kv2_datum_f,
        schema = App$databaseschema
      )  
      log_serverstap(
        "KV2",
        "Dieren en locaties opgeslagen",
        glue::glue("levering_id={lev_id}; verwerkingsmaand={jrmnd$vwmd}")
      )
      # KV3  -----------------------------------------------------------------
      mld <- "KV2 naar KV3"
      stap <- stap + 1
      setProgress(message = mld, value = stap)
      log_serverstap("KV3", mld)
      
      kv3_datum <- kv2_datum + lubridate::dminutes(5)
      kv3_datum_f <- format(kv3_datum, "%Y-%m-%d %H:%M:%S")
      

      ## Correcties -----------------------------------------------------------
      
      mld <- "KV2 naar KV3: correcties uitvoeren"
      stap <- stap + 1
      setProgress(message = mld, value = stap)
      log_serverstap("KV3", mld)
      
      if (file.exists(file.path(App$configmap, App$kv3_correcties_rvo_slachtingen))) { 
        
        basenaam <- "kv3_correcties"
        correctiesbestandnaam <- paste(format(Sys.time(),'%Y%m%d_%H%M%S'), 
                                       basenaam,
                                       "log.csv",
                                       sep = "_")
        
        pad_correctiesbestand <- file.path(
          App$logmap,
          correctiesbestandnaam
        )
        
        mld <- rvo_kv3_correcties(
          df_slachtingen = df_slachtingen,
          df_slachthuizen = df_slachthuizen, 
          correcties_slachtingen = dcmodify::modifier(
            .file = file.path(App$configmap, App$kv3_correcties_rvo_slachtingen)),
          lev_id = lev_id, 
          con = con, 
          pad_sjablon = file.path(App$sjablonenmap, "kv3_correcties.qmd"),
          pad_outputmap = App$controlesmap,
          tbl_dieren =  "tbl_dieren_kv2_kv3",
          tbl_locaties = "tbl_locaties_kv2_kv3",
          tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
          datum_f = kv3_datum_f,
          schema = App$databaseschema,
          vwmd = jrmnd$vwmd
        )
        
        logdebug(mld)
        log_serverstap("KV3", "Automatische correcties afgerond")
        
        shinyalert(
          title = "Correcties RVO slachtingen bestand.",
          text = mld,
          type = "warning"
        )
        
      } else {
        mld <- glue::glue(
          "Het yml-bestand voor de automatische correcties bestaat niet. ",
          "Het verwachte bestand zou gevonden moeten worden in: ", 
          "{file.path(App$configmap, 'kv3_correcties_rvo_slachtingen.yml')}\n",
          "Er werden geen correcties toegepast.")
        
        logdebug(mld)
        
        shinyalert(
          title = "Correcties RVO slachtingen bestand.",
          text = mld,
          type = "warning"
        )
        
        }
      
      ## Koppelingen -----------------------------------------------------------
      mld <- "KV2 naar KV3: koppelingen uitvoeren"
      stap <- stap + 1
      setProgress(message = mld, value = stap)
      log_serverstap("KV3", mld)
      df_db_locaties_kv3 <- dplyr::tbl(con, App$tbl_locaties) |>
        # haal KV3 locaties
        dplyr::filter(
          is.na(eind_datum)
          ) |> 
        dplyr::collect()
      log_serverstap(
        "KV3",
        "Actuele locaties opgehaald",
        glue::glue("{nrow(df_db_locaties_kv3)} locaties")
      )
      
      koppeling_output <- rvo_locaties_koppeling(
        df_locaties_kv3 = df_db_locaties_kv3,
        df_landbouwtelling = df_landbouwtelling,
        df_verblijfsplaatsen = df_verblijfsplaatsen,
        df_skal = df_skal
      )
      log_serverstap(
        "KV3",
        "Locatiekoppeling afgerond",
        glue::glue(
          "{nrow(koppeling_output$locaties_en_skal)} locaties gekoppeld; ",
          "{nrow(koppeling_output$locaties_zonder_skal)} zonder SKAL"
        )
      )
      
      koppeling_rapport <- rvo_locaties_koppeling_log(
        df_locaties_kv3 = df_db_locaties_kv3,
        df_locaties_zonder_adres = koppeling_output$locaties_zonder_adres,
        df_locaties_zonder_relnr = koppeling_output$locaties_zonder_relnr,
        df_locaties_met_relnr_zonder_adres = koppeling_output$locaties_met_relnr_zonder_adres,
        df_locaties_meerdere_relnrs = koppeling_output$locaties_meerdere_relnrs,
        df_locaties_zonder_adres_voor_norm = koppeling_output$locaties_zonder_adres_voor_norm,
        df_locaties_zonder_adres_vanvege_norm = koppeling_output$locaties_zonder_adres_vanvege_norm,
        df_locaties_problematisch_norm_adressen = koppeling_output$locaties_problematisch_norm_adressen,
        df_locaties_zonder_skal = koppeling_output$locaties_zonder_skal,
        df_skal_zonder_adres = koppeling_output$skal_zonder_adres,
        pad_sjablon = file.path(App$sjablonenmap, "kv3_koppelingen.qmd"),
        pad_outputmap = App$controlesmap,
        pad_tempmap = tempdir()
      )
      
      if(file.exists(koppeling_rapport)) {
        
        mld <- glue::glue(
          "Er is een rapport gemaakt met informatie over de koppeling",
          " tussen de RVO en Skal dataset. Je kunt het bestand vinden in:",
          " {koppeling_rapport}")
        
        logdebug(mld)
        
        shinyalert(
          title = "Koppelingen RVO slachtingen bestand.",
          text = mld,
          type = "warning"
        )
      }
      ## Slachtingen -----------------------------------------------------------
      mld <- "KV2 naar KV3: slachtingen bepalen"
      stap <- stap + 1
      setProgress(message = mld, value = stap)
      log_serverstap("KV3", mld)
      df_bio_locaties <- rvo_kv3_bio_locaties(
        df_locaties_en_skal = koppeling_output$locaties_en_skal,
        datum_f = kv3_datum_f,
        con = con,
        schema = App$databaseschema)
      log_serverstap(
        "KV3",
        "Biologische locaties verwerkt",
        glue::glue("{nrow(df_bio_locaties)} records")
      )
      
      df_spek_input <- rvo_kv3_slachtingen(
        con,
        lev_id,
        tbl_dieren = App$tbl_dieren,
        tbl_locaties = App$tbl_locaties,
        tbl_dieren_op_locaties = App$tbl_dieren_op_locaties,
        tbl_bio_locaties = App$tbl_bio_locaties,
        tbl_slachtingen = App$tbl_slachtingen,
        kv3_datum_f = kv3_datum_f,
        schema = App$databaseschema,
        vwmd = jrmnd$vwmd
      )
      log_serverstap(
        "KV3",
        "Slachtingen verwerkt",
        glue::glue("{nrow(df_spek_input)} geaggregeerde records")
      )
      
      ## Eind KV3 ----------------------------------------------------------------
      mld <- "SPEK-input voorbereiden"
      stap <- stap + 1
      setProgress(message = mld, value = stap)
      log_serverstap("RVO-SPEK", mld)
      
      df_spek_input <- rvo_maak_rp3_1(con = con,
                                      vwmd = jrmnd$vwmd)
      log_serverstap(
        "RVO-SPEK",
        "SPEK-data samengesteld",
        glue::glue("{nrow(df_spek_input)} records")
      )
      
      if(nrow(df_spek_input) > 0) {
        
        bestandnaam <- paste(format(Sys.time(),'%Y%m%d_%H%M%S'), 
                                "verwerkt_rvo_runderen.xlsx",
                                sep = "_")
        
        pad_output <-  file.path(inputmap, bestandnaam)
        rvo_formaat_rp3_1_naar_spek_input(df_spek_input) |> 
          writexl::write_xlsx(path = pad_output)
        mld <- glue::glue(
          "Er is een nieuw SPEK-inputbestand opgeslagen: {pad_output}"
        )
        
        paden$rvo_spek_input <- pad_output
        log_serverstap(
          "RVO-SPEK",
          "SPEK-inputbestand opgeslagen",
          pad_output
        )
        
        shinyalert(
          title = "RVO slachtingen",
          html = TRUE,
          text = tagList(
            p(
              mld
            ),
            actionButton(inputId="btn_rvo_spek_input_openen",
                        label="Openen")
          ),
          type = "info"
        )
        
        
      } else {
        mld <-  "Het proces is succesvol beëindigd.
        Er zijn geen slachtingen gevonden in de dataabase voor de geselecteerde maand."
        shinyalert(
          title = "RVO slachtingen",
          text = mld,
          type = "info"
        )
      }
      
  
      logdebug(mld)
      
      # eind withProgress
      mld <- "Klaar!"
      stap <- stap + 1
      setProgress(message = mld, value = stap)
      loginfo(msg = "RVO naar SPEK is succesvol afgerond")
      
    })
  }
  , error = function(e){
    toon_harde_fout( titel =  "RVO naar SPEK is gestopt.",
      fout = e
    )
  }
  , finally = {
    if (!is.null(con)) {
      try({
        if (DBI::dbIsValid(con)) {
          DBI::dbDisconnect(con)
          log_serverstap("RVO-SPEK", "Databaseverbinding is gesloten")
        }
      }, silent = TRUE)
    }
  })
  })
  # Gebruiker klikt op knop Openen van Controle en correctie.
  observeEvent( input$btn_rvo_spek_input_openen, {
    
    req(file.exists(paden$rvo_spek_input))
    
    loginfo(msg="Gebruiker klikt op knop Openen van RVO SPEK input")
      cmd <- paste0('start excel ', '"', paden$rvo_spek_input, '"')
      shell(cmd=cmd)
    
    loginfo(msg="Systeem is gereed met openen controle- en correctiebestand")
  })
  
  # Na klikken op knop Bladeren: open 'verkenner' en vul gekozen map in.
  observeEvent(input$btn_inputdir, {
    loginfo(msg="Gebruiker klikt op knop bladeren")
    keuze <- choose.dir(default=input$dir_input, caption = "Selecteer inputmap")
    if (!is.null(keuze) && !is.na(keuze) && length(keuze)>0 && trimws(keuze)!='') {
      shiny::updateTextInput(session, inputId="dir_input", value=keuze)
    }
  })


  # Na klikken op knop ververs: ververs het plaatje
  observeEvent(input$btn_ververs_plaatje, {
    loginfo(msg="Gebruiker klikt op knop om het plaatje te verversen")
      # vul plaatje bij opstarten
      output$plaatje <- renderImage({
        return(list(
          src = alg_vind_plaatje(),
          contentType = "image/png",
          alt = "Face", height="420", width="420"
        ))
      }, deleteFile = FALSE)
  })

  
  # Gebruiker klikt op knop Help: open gebruikershandleiding
  observeEvent(input$btn_help, {
    loginfo(msg="Gebruiker klikt op knop Help om de gebruikershandleiding te openen")
    doc <- file.path(App$jpgmap, "Systeem Slachtingen - Gebruikershandleiding.docx")
    cmd <- paste0('start winword ', '"', doc, '"')
    shell(cmd=cmd)
  })

  
  # Gebruiker klikt op knop Maken van Controle en correctie
  observeEvent( input$btn_ctcr_maken, {
    # maak de gebruikersinfo leeg
    # NB: het renderen van de tekstvelden gebeurt pas als alle
    # functie-elementen zijn uitgevoerd
    output$txt_ctcr_1 <- renderText(NULL)
    output$txt_ctcr_2 <- renderText(NULL)
    output$txt_ctcr_3 <- renderText(NULL)
    output$txt_ctcr_4 <- renderText(NULL)

    loginfo(msg="Gebruiker klikt op knop Maken van Controle en correctie")
    withProgress(message="", value=0, {
      mld <- "Voorbereiden maken controle- en correctiebestanden"
      setProgress(message=mld, value=0.01)
      log_serverstap("CTCR", mld)

      # haal gegevens op uit invoervelden gui
      inputmap <- input$dir_input

      # leid maanden af
      jrmnd <- alg_vind_jaarmaanden(inputmap=inputmap)
      logdebug(msg=paste("Gekozen verwerkingsmaand:", as.character(jrmnd$vwmd)))
      maanden_vorige_vm  <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3)
      maanden_huidige_vm <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4)
      
      # jaarmaand map wordt aangemaakt in /Werk als dit nog niet bestaat
      alg_bestaat_jaarmaandmap(App$werkmap, getElement(jrmnd, "vwmd"))

      # bepaal inputbestanden
      input_rvo_rdvl_bst     <- file.path(inputmap, input$output_rvo_naar_spek)
      input_nvwa_rdvl_bst    <- file.path(inputmap, input$input_aant_rdvl)
      input_aant_wtvl_bst    <- file.path(inputmap, input$input_aant_wtvl)
      input_verd_gemg_bst    <- file.path(inputmap, input$input_gemg_rdvl)
      input_gemg_kalv_bst    <- file.path(inputmap, input$input_gemg_kalveren)
      input_gemg_kalv_vj_bst <- file.path(inputmap, input$input_gemg_kalveren_vj)

      # bepaal te genereren ctcr-bestanden 
      ctcr_bestanden <- alg_vind_ctcr_bestanden(werkmap=App$werkmap, vwmd=jrmnd$vwmd)
      ctcr_aant_rdvl_bst <- ctcr_bestanden$ctcr_aant_rdvl
      ctcr_aant_wtvl_bst <- ctcr_bestanden$ctcr_aant_wtvl
      ctcr_verd_bst      <- ctcr_bestanden$ctcr_verd
      ctcr_gemg_bst      <- ctcr_bestanden$ctcr_gemg
      log_serverstap(
        "CTCR",
        "Uitvoerpaden bepaald",
        dirname(ctcr_aant_rdvl_bst)
      )

      # controleer of de ctcr-bestanden gemaakt kunnen worden
      ctrl <- alg_controleer_maken_ctcr(input_aant_rdvl=input_nvwa_rdvl_bst,
                                        input_aant_wtvl=input_aant_wtvl_bst,
                                        input_verd_gemg=input_verd_gemg_bst,
                                        input_gemg=input_gemg_kalv_bst,
                                        input_gemg_vj=input_gemg_kalv_vj_bst,
                                        ctcr_aant_rdvl=ctcr_aant_rdvl_bst,
                                        ctcr_aant_wtvl=ctcr_aant_wtvl_bst,
                                        ctcr_verd=ctcr_verd_bst,
                                        ctcr_gemg=ctcr_gemg_bst,
                                        jrmnd=jrmnd)
      
      harde_inputfouten <- alg_ctcr_harde_inputfouten(ctrl)
      log_serverstap(
        "CTCR",
        "Invoercontroles afgerond",
        glue::glue("{length(harde_inputfouten)} harde fouten")
      )

      if (length(harde_inputfouten) > 0) {
        mld <- paste(
          "Er zijn harde controlefouten gevonden in de invoerbestanden.",
          "De controle- en correctiebestanden worden daarom niet gemaakt.",
          "",
          paste0("- ", harde_inputfouten, collapse = "\n"),
          sep = "\n"
        )

        logerror(msg = mld)

        shinyalert(
          title = "Technische controlefout in invoerbestand",
          text = mld,
          type = "error"
        )

        setProgress(message = "Proces gestopt.", value = 1)
        req(FALSE)
      }


      mld <- "Maken controle- en correctiebestand aantallen roodvlees"
      setProgress(message=mld, value=0.2)
      log_serverstap("CTCR", mld)
      # Bestand wordt alleen gemaakt als voor de huidige verwerkingsmaand
      # - levering aantallen roodvlees bestaat
      # - het bestand nog niet eerder is gemaakt
      if (ctrl$file_rdvl_ok && ctrl$ctcr_rdvl_ok){
        
        # input onderstaande functie moet aangepast worden; RVO er bij
        res_aant_rdvl <- maak_ctcr_aant_rdvl(inputbestand_nvwa=input_nvwa_rdvl_bst,
                                             inputbestand_rvo=input_rvo_rdvl_bst,
                                             jrmnd=jrmnd, 
                                             maanden_vorige_vm=maanden_vorige_vm,
                                             maanden_huidige_vm=maanden_huidige_vm,
                                             outputbestand=ctcr_aant_rdvl_bst)
        
        # het tijdelijk ctcr verschil bestand
        outputbest_verschil <- file.path(dirname(ctcr_aant_rdvl_bst), "verschil_nvwa_rvo.xlsx", fsep = .Platform$file.sep)
        res_verschil <- maak_ctcr_verschil_rvo_nvwa(inputbestand_nvwa=input_nvwa_rdvl_bst,
                                                    inputbestand_rvo=input_rvo_rdvl_bst,
                                                    jrmnd=jrmnd, 
                                                    maanden_huidige_vm=maanden_huidige_vm,
                                                    outputbestand=outputbest_verschil)
      } else {
        res_aant_rdvl <- ctrl$txt_rdvl
        res_verschil <- ""
      }
      output$txt_ctcr_1 <- renderText(paste(res_aant_rdvl, res_verschil, sep = ". "))
      loginfo(msg=paste("Systeem meldt op het scherm:", paste0(res_aant_rdvl, res_verschil)))

      mld <- "Maken controle- en correctiebestand aantallen witvlees"
      setProgress(message=mld, value=0.4)
      log_serverstap("CTCR", mld)
      # Bestand wordt alleen gemaakt als voor de huidige verwerkingsmaand
      # - levering aantallen witvlees bestaat
      # - het bestand nog niet eerder is gemaakt
      if (ctrl$file_wtvl_ok && ctrl$ctcr_wtvl_ok){
        res_aant_wtvl <- maak_ctcr_aant_wtvl(inputbestand=input_aant_wtvl_bst,
                                             jrmnd=jrmnd,
                                             maanden_vorige_vm=maanden_vorige_vm,
                                             maanden_huidige_vm=maanden_huidige_vm,
                                             outputbestand=ctcr_aant_wtvl_bst)
       } else {
         res_aant_wtvl <- ctrl$txt_wtvl
       }
      output$txt_ctcr_2 <- renderText(res_aant_wtvl)
      loginfo(msg=paste("Systeem meldt op het scherm:", res_aant_wtvl))

      mld <- "Maken controle- en correctiebestand aantallenverdelingen"
      setProgress(message=mld, value=0.6)
      log_serverstap("CTCR", mld)
      # Bestand wordt alleen gemaakt als voor de huidige verwerkingsmaand
      # - levering aantallenverdelingen runderen bestaat
      # - levering de juiste sheets bevat
      # - het bestand nog niet eerder is gemaakt
      if (ctrl$file_verd_gemg_ok && ctrl$sh_verd_gemg_ok && ctrl$ctcr_verd_ok){
        res_verd <- maak_ctcr_bst_verd(inputbestand=input_verd_gemg_bst,
                                       jrmnd=jrmnd,
                                       maanden_vorige_vm=maanden_vorige_vm,
                                       maanden_huidige_vm=maanden_huidige_vm,
                                       outputbestand=ctcr_verd_bst)
      } else {
        res_verd <- ctrl$txt_verd
      }
      output$txt_ctcr_3 <- renderText(res_verd)
      loginfo(msg=paste("Systeem meldt op het scherm:", res_verd))

      mld <- "Maken controle- en correctiebestand gemiddelde gewichten"
      setProgress(message=mld, value=0.8)
      log_serverstap("CTCR", mld)
      # Bestand wordt alleen gemaakt als voor de huidige verwerkingsmaand:
      # - levering gemiddelde gewichten roodvlees bestaat
      # - levering gemiddelde gewichten kalveren 0-8 mnd bestaat
      # - levering gemiddelde gewichten kalveren 0-8 mnd vorig kalenderjaar
      #                                               bestaat indien nodig
      # - leveringsbestanden de juiste sheets bevatten
      # - het bestand nog niet eerder is gemaakt
      if (ctrl$file_gemg_kalv_ok && ctrl$file_gemg_kalv_vj_ok && ctrl$file_verd_gemg_ok &&
          ctrl$sh_gemg_kalv_ok && ctrl$sh_gemg_kalv_vj_ok && ctrl$sh_verd_gemg_ok &&
          ctrl$ctcr_gemg_ok){
        res_gemg <- maak_ctcr_bst_gemg(inputbestand1=input_verd_gemg_bst,
                                       inputbestand2=input_gemg_kalv_bst,
                                       inputbestand3=input_gemg_kalv_vj_bst,
                                       jrmnd=jrmnd,
                                       maanden_vorige_vm=maanden_vorige_vm,
                                       maanden_huidige_vm=maanden_huidige_vm,
                                       outputbestand=ctcr_gemg_bst)
      } else {
        res_gemg <- ctrl$txt_gemg
      }
      output$txt_ctcr_4 <- renderText(res_gemg)
      loginfo(msg=paste("Systeem meldt op het scherm:", res_gemg))

      ###################################
      
      mld <- "Maken controlebestand diertotalen"
      setProgress(message=mld, value=0.9)
      log_serverstap("CTCR", mld)
      # Bestand wordt alleen gemaakt als voor de huidige verwerkingsmaand
      # - levering aantallen roodvlees bestaat
      if (ctrl$file_rdvl_ok){
        # het diertotalen ctcr bestand
        outputbest_dier_tot <- file.path(dirname(ctcr_aant_rdvl_bst), "controle_dier_tot.pdf", fsep = .Platform$file.sep)
        res_dier_tot <- maak_ctcr_dier_tot(jrmnd = jrmnd,
                                           levering_bst_nvwa = input_nvwa_rdvl_bst,
                                           levering_bst_rvo = input_rvo_rdvl_bst,
                                           maanden_huidige_vm = maanden_huidige_vm,
                                           schema = App$databaseschema,
                                           tabel = App$tbl_microbase_rdvl,
                                           tabel_def = App$tbl_microbase_rdvl_def,
                                           outputbestand = outputbest_dier_tot)
      } else {
        res_aant_rdvl <- ctrl$txt_rdvl
        res_verschil <- ""
      }
      output$txt_ctcr_5 <- renderText(res_dier_tot)
      loginfo(msg=paste("Systeem meldt op het scherm:", res_dier_tot))
      #####################################
      
      setProgress(message="Gereed", value=1)
      loginfo(msg = "Maken van controle- en correctiebestanden is afgerond")
    })
  })


  # Gebruiker klikt op knop Openen van Controle en correctie.
  observeEvent( input$btn_ctcr_openen, {
    # maak de gebruikersinfo leeg
    output$txt_ctcr_1 <- renderText(NULL)
    output$txt_ctcr_2 <- renderText(NULL)
    output$txt_ctcr_3 <- renderText(NULL)
    output$txt_ctcr_4 <- renderText(NULL)
    output$txt_ctcr_5 <- renderText(NULL)

    loginfo(msg="Gebruiker klikt op knop Openen van Controle en correctie")
    inputmap <- input$dir_input
    jrmnd <- alg_vind_jaarmaanden(inputmap=inputmap)
    jaar <- substr(jrmnd$vwmd,1,4)
    vwmd <- jrmnd$vwmd
    ctcr_map <- file.path(App$werkmap, jaar, vwmd)
    keuze <- choose.files(default=file.path(ctcr_map,
                                            "*.xlsx"),
                          multi=FALSE,
                          caption="Selecteer bestand in verwerkingsmap")
    if (!is.null(keuze) && !is.na(keuze) && length(keuze)>0 && trimws(keuze)!='') {
      cmd <- paste0('start excel ', '"', keuze, '"')
      shell(cmd=cmd)
    }

    loginfo(msg="Systeem is gereed met openen controle- en correctiebestand")
  })


  # Gebruiker klikt op knop Opslaan van Opslag in database.
  observeEvent( input$btn_opslaan, {
    loginfo(msg="Gebruiker klikt op knop Opslaan van Opslag in database")

    # haal gegevens op uit invoervelden gui
    inputmap <- input$dir_input

    # leid maanden af
    jrmnd <- alg_vind_jaarmaanden(inputmap=inputmap)
    logdebug(msg=paste("Gekozen verwerkingsmaand:", as.character(jrmnd$vwmd)))
    maanden_huidige_vm <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4)

    # bepaal inputbestanden
    input_nvwa_rdvl_bst <- file.path(inputmap, input$input_aant_rdvl)
    input_aant_wtvl_bst <- file.path(inputmap, input$input_aant_wtvl)
    input_rvo_rdvl_bst  <- file.path(inputmap, input$output_rvo_naar_spek)
    # bepaal ctcr-bestanden die moeten worden opgeslagen
    ctcr_bestanden <- alg_vind_ctcr_bestanden(werkmap=App$werkmap, vwmd=jrmnd$vwmd)
    ctcr_aant_rdvl_bst <- ctcr_bestanden$ctcr_aant_rdvl
    ctcr_aant_wtvl_bst <- ctcr_bestanden$ctcr_aant_wtvl
    ctcr_verd_bst      <- ctcr_bestanden$ctcr_verd
    ctcr_gemg_bst      <- ctcr_bestanden$ctcr_gemg

    # controleer of de ctcr-bestanden opgeslagen kunnen worden
    ctrl <- alg_controleer_opslaan_in_database(input_nvwa_rdvl_bst, input_aant_wtvl_bst,
                                               ctcr_aant_rdvl_bst, ctcr_aant_wtvl_bst,
                                               ctcr_verd_bst, ctcr_gemg_bst)
    log_serverstap(
      "OPSLAG",
      "Bestandscontroles afgerond",
      glue::glue("bestanden_ok={ctrl$bestanden_ok}")
    )
    if (ctrl$bestanden_ok) {
      withProgress(message="Start", value=0, {

        mld <- "Opslaan controle- en correctiebestanden in de database"
        setProgress(message=mld, value=0.25)
        log_serverstap("OPSLAG", mld)
        opsl_opslaan_ctcr(bst_aant_rdvl_nvwa_ruw=input_nvwa_rdvl_bst,
                          bst_aant_rdvl_rvo_ruw=input_rvo_rdvl_bst,
                          bst_aant_rdvl_gaaf=ctcr_aant_rdvl_bst,
                          bst_aant_wtvl_ruw=input_aant_wtvl_bst,
                          bst_aant_wtvl_gaaf=ctcr_aant_wtvl_bst,
                          bst_verd_gaaf=ctcr_verd_bst,
                          bst_gemg_gaaf=ctcr_gemg_bst,
                          maanden_huidige_vm=maanden_huidige_vm, jrmnd=jrmnd)
        log_serverstap("OPSLAG", "CTCR-gegevens opgeslagen")

        mld <- "Berekenen totaalaantallen"
        setProgress(message=mld, value=0.5)
        log_serverstap("OPSLAG", mld)

        mld <- "Berekenen totaalgewichten"
        setProgress(message=mld, value=0.75)
        log_serverstap("OPSLAG", mld)

        setProgress(message="Gereed", value=1) # maak voortgangsbalk vol
        res_opsl <- "de bestanden zijn opgeslagen in de database"
        loginfo(msg = "Opslaan in microbase en statbase is afgerond")
      })
    } else {
        res_opsl <- ctrl$txt
    }

    # meld gereed
    output$txt_opslag <- renderText(res_opsl)
    loginfo(msg=paste("Systeem meldt op het scherm:", res_opsl))
  })


  # Gebruiker klikt op knop Maken van Analyse.
  observeEvent( input$btn_ana_maken, {
    
    # maak gebruikersinfo leeg
    output$txt_analyse_1 <- renderText(NULL)
    output$txt_analyse_2 <- renderText(NULL)

    loginfo(msg="Gebruiker klikt op knop Maken van Analyse")
    # Toon een voortgangsbalk
    withProgress(message="", value=0, {

      mld <- "Voorbereiden maken analysebestand tijdreeksen"
      setProgress(message=mld, value=0.01)
      log_serverstap("ANALYSE", mld)
      # haal gegevens op uit invoervelden gui
      inputmap <- input$dir_input

      # leid maanden af
      jrmnd <- alg_vind_jaarmaanden(inputmap=inputmap)
      logdebug(msg=paste("Gekozen verwerkingsmaand:", as.character(jrmnd$vwmd)))
      maanden_vorige_vm  <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3)
      maanden_huidige_vm <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4)
      
      # jaarmaand map wordt aangemaakt in Werk als dit nog niet bestaat
      alg_bestaat_jaarmaandmap(App$werkmap, getElement(jrmnd, "vwmd"))

      # bepaal te genereren analysebestanden
      analysebestanden <- alg_vind_analysebestanden(werkmap=App$werkmap,
                                                    vwmd=jrmnd$vwmd)
      ana_tijdreeksen_totaal <- analysebestanden$ana_tijdreeksen_totaal
      ana_tijdreeksen_biologisch <- analysebestanden$ana_tijdreeksen_biologisch
      ana_bijschattingen <- analysebestanden$ana_bijschattingen
      log_serverstap(
        "ANALYSE",
        "Uitvoerpaden bepaald",
        dirname(ana_bijschattingen)
      )

      mld <- "Maken analysebestand tijdreeksen"
      setProgress(message=mld, value=0.3)
      log_serverstap("ANALYSE", mld)
      res_trks <- maak_ana_tijdreeksen(werkmap=App$werkmap, jrmnd, 
                                       outputbestand_totaal=ana_tijdreeksen_totaal, 
                                       outputbestand_biologisch=ana_tijdreeksen_biologisch)
      log_serverstap("ANALYSE", "Analysebestand tijdreeksen afgerond")

      mld <- "Maken analysebestand bijschattingen"
      setProgress(message=mld, value=0.6)
      log_serverstap("ANALYSE", mld)
      res_pcbs <- maak_ana_bijschattingen(werkmap=App$werkmap, jrmnd, maanden_vorige_vm,
                                          maanden_huidige_vm, ana_bijschattingen)
      log_serverstap("ANALYSE", "Analysebestand bijschattingen afgerond")

      setProgress(message="Gereed", value=1) # maak voortgangsbalk vol
    })

    # meld gereed
    output$txt_analyse_1 <- renderText(res_trks)
    output$txt_analyse_2 <- renderText(res_pcbs)
    loginfo(msg="Systeem is gereed met maken analysebestanden")
  })


  # Gebruiker klikt op knop Openen van Analyse.
  observeEvent( input$btn_ana_openen, {
    # maak gebruikersinfo leeg
    output$txt_analyse_1 <- renderText(NULL)
    output$txt_analyse_2 <- renderText(NULL)

    loginfo(msg="Gebruiker klikt op knop Openen van Analyse")
    inputmap <- input$dir_input
    jrmnd <- alg_vind_jaarmaanden(inputmap=inputmap)
    jaar <- substr(jrmnd$vwmd,1,4)
    vwmd <- jrmnd$vwmd
    analyse_map <- file.path(App$werkmap, jaar, vwmd)
    keuze <- choose.files(default=file.path(analyse_map,
                                            "*.xlsx"),
                          multi=FALSE,
                          caption="Selecteer bestand in verwerkingsmap")
    if (!is.null(keuze) && !is.na(keuze) && length(keuze)>0 && trimws(keuze)!='') {
      cmd <- paste0('start excel ', '"', keuze, '"')
      shell(cmd=cmd)
    }

    loginfo(msg="Systeem is gereed met openen analysebestanden")
  })


  # Gebruiker klikt op knop Maken van Output.
  observeEvent( input$btn_output_maken, {
    # maak gebruikersinfo leeg
    output$txt_output_1 <- renderText(NULL)
    output$txt_output_2 <- renderText(NULL)
    output$txt_output_3 <- renderText(NULL)
    output$txt_output_4 <- renderText(NULL)
    output$txt_output_5 <- renderText(NULL)
    inputmap <- input$dir_input
    loginfo(msg = "Gebruiker start het maken van disseminatie-output")

    # leid maanden af
    jrmnd <- alg_vind_jaarmaanden(inputmap=inputmap)
    
    # jaarmaand map wordt aangemaakt in /Output als dit nog niet bestaat
    alg_bestaat_jaarmaandmap(App$output, getElement(jrmnd, "vwmd"))
    
    # leid bestandpaden af
    outputbestanden <- alg_vind_outputbestanden(outputmap=App$outputmap, vwmd=jrmnd$vwmd, jrmnd=jrmnd)
    out_eurostat <- outputbestanden$out_eurostat
    out_statline <- outputbestanden$out_statline
    out_contactpersonen <- outputbestanden$out_contactpersonen
    out_dsc_rdvl <- outputbestanden$out_dsc_rdvl
    out_dsc_wtvl <- outputbestanden$out_dsc_wtvl
    out_mapstructuur <- outputbestanden$out_mapstructuur
    out_bio <- outputbestanden$out_bio
    log_serverstap(
      "OUTPUT",
      "Uitvoerpaden bepaald",
      out_mapstructuur
    )

    
    # check of de config constanten voor eurostat niet ontbreken
    const_uit_config <- list(
      const_estat_dataflow = App$const_estat_dataflow,
      const_estat_freq = App$const_estat_freq,
      const_afronden_aantal = App$const_afronden_aantal,
      const_afronden_gewicht = App$const_afronden_gewicht
      )
    idx_null <- vapply(const_uit_config, is.null, TRUE)
    afwezig <- names(const_uit_config[idx_null])
    
    constanten <- data.frame(
    estat_dataflow = ifelse(is.null(App$const_estat_dataflow),  "ESTAT:ANIP_MTSLS_M(1.0)", App$const_estat_dataflow),
    estat_freq = ifelse(is.null(App$const_estat_freq), "M", App$const_estat_freq),
    afronden_aantal = ifelse(is.null(App$const_afronden_aantal), 3, App$const_afronden_aantal),
    afronden_gewicht = ifelse(is.null(App$const_afronden_gewicht), 3, App$const_afronden_gewicht)
    )
    afronden_decimalen <- c(aantal=constanten$afronden_aantal, 
                            gewicht=constanten$afronden_gewicht)
    
    # als iets uit config (app.ini) ontbreekt, maak pop up in de shiny app
    if (length(afwezig) > 0) {
      shinyalert(
        title = "Onderstaande variabelen en/of waarden zijn niet gevonden. 
        Pas app.ini aan als default niet klopt.",
        text = sprintf("
        Variabele: %s. Geselecteerde default: %s.
                       ---", 
                       afwezig, constanten[, idx_null]),
        type = "info"
      )
    }

    loginfo(msg="Gebruiker klikt op knop Maken van Output")
    withProgress(message="", value=0, {
      mld <- "Verzamelen data voor de output"
      setProgress(message=mld, value=0.01)
      log_serverstap("OUTPUT", mld)
      df_output <- oph_haal_output_op(jrmnd)
      log_serverstap(
        "OUTPUT",
        "Outputdata verzameld",
        glue::glue(
          "stat={nrow(df_output$stat)}; ",
          "dsc_roodvlees={nrow(df_output$dsc_rdvl)}; ",
          "dsc_witvlees={nrow(df_output$dsc_wtvl)} records"
        )
      )
      
      mld <- "Maken output voor Eurostat"
      setProgress(message=mld, value=0.17)
      log_serverstap("OUTPUT", mld)

      res_eurostat_sdmx <- maak_eurostat_sdmx_per_jaar(df=df_output$stat, 
                                                       jrmnd=jrmnd, 
                                                       output_map = out_mapstructuur,
                                                       waarde_dataflow = constanten$estat_dataflow,
                                                       waarde_freq = constanten$estat_freq,
                                                       afronden_decimalen = afronden_decimalen)
                                              
      res_eurostat <- maak_output_eurostat(df=df_output$stat, jrmnd=jrmnd,
                                           maanden_vorige_vm=maanden_vorige_vm,
                                           maanden_huidige_vm=maanden_huidige_vm,
                                           outputbestand=out_eurostat)
      log_serverstap(
        "OUTPUT",
        "Eurostat-output afgerond",
        out_eurostat
      )

      mld <- "Maken output voor Statline"
      setProgress(message=mld, value=0.34)
      log_serverstap("OUTPUT", mld)
      res_statline <- maak_output_statline(df=df_output$stat, jrmnd=jrmnd,
                                           outputbestand=out_statline,
                                           outputbestand_bio=out_bio)
      log_serverstap("OUTPUT", "StatLine-output afgerond", 
                     glue::glue("statline={out_statline}; bio={out_bio}")
                     )
      mld <- "Output bio en output statline gemaakt"
      setProgress(message=mld, value=0.40)
      log_serverstap("OUTPUT", mld)
      
      mld <- "Maken output voor contactpersonen"
      setProgress(message=mld, value=0.51)
      log_serverstap("OUTPUT", mld)
      res_contactpersonen <- maak_output_contactpersonen(df=df_output$stat, jrmnd=jrmnd,
                                                         maanden_vorige_vm=maanden_vorige_vm,
                                                         maanden_huidige_vm=maanden_huidige_vm,
                                                         outputbestand=out_contactpersonen)
      log_serverstap(
        "OUTPUT",
        "Contactpersonenoutput afgerond",
        out_contactpersonen
      )
      
      mld <- "Maken output voor DSC roodvlees"
      setProgress(message=mld, value=0.68)
      log_serverstap("OUTPUT", mld)
      res_dsc_rdvl <- maak_output_dsc_rdvl(df=df_output$dsc_rdvl,
                                           outputbestand=out_dsc_rdvl)
      log_serverstap("OUTPUT", "DSC-output roodvlees afgerond", out_dsc_rdvl)

      mld <- "Maken output voor DSC witvlees"
      setProgress(message=mld, value=0.84)
      log_serverstap("OUTPUT", mld)
      res_dsc_wtvl <- maak_output_dsc_wtvl(df=df_output$dsc_wtvl,
                                           outputbestand=out_dsc_wtvl)
      log_serverstap("OUTPUT", "DSC-output witvlees afgerond", out_dsc_wtvl)

      setProgress(message="Gereed", value=0.93) 
      
      
      mld <- "Maken output voor output bio"
      setProgress(message=mld, value=0.96)
      log_serverstap("OUTPUT", mld)
  
      
      setProgress(message="Gereed", value=1) # maak voortgangsbalk vol
    })

    # meld gereed
    output$txt_output_1 <- renderText(res_eurostat_sdmx)
    output$txt_output_2 <- renderText(res_eurostat)
    output$txt_output_3 <- renderText(res_contactpersonen)
    output$txt_output_4 <- renderText(res_statline)
    output$txt_output_5 <- renderText(res_dsc_rdvl)
    output$txt_output_6 <- renderText(res_dsc_wtvl)
  
    loginfo(msg="Systeem is gereed met maken output")
  })

})
