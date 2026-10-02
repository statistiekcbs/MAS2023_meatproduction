#' Normaliseer de input IenR-bestanden in drie verschillende tabellen zoals
#' opgeslagen in de database.
#'
#' @param df_slachtingen data.frame, de slachtingen IenR inputbestand zoals
#'   geretourneerd door de `inl_lees_csv_bestand_met_kolommen_config`-functie.
#' @param df_slachthuizen data.frame, de slachthuizen inputbestand zoals
#'   geretourneerd door de `inl_lees_csv_bestand_met_kolommen_config`-functie.
#'
#' @return list met drie elementen:
#'    - dieren: data.frame data.frame die overeenkomt met de
#'              structuur van tabel `tbl_dieren_kv2_kv3` in de database.
#'    - dieren_op_locaties: data.frame data.frame die overeenkomt met de
#'                          structuur van tabel `tbl_dieren_op_locaties_kv2_kv3`
#'                          in de database.
#'    - locaties: data.frame data.frame die overeenkomt met de
#'                structuur van tabel `tbl_locaties_kv2_kv3` in de database.

#' @export
rvo_dieren_en_locaties_bestanden_normaliseren <- function(
    df_slachtingen,
    df_slachthuizen) {
  kolommen_slachtingen <- c(
    "levens_nr",
    "datum_geboorte",
    "datum_slacht",
    "datum_eerste_afkalven",
    "datum_import",
    "geslacht",
    "eld_code",
    "landcode_herkomst",
    "landcode_oorsprong",
    "code_reden_einde",
    "einde_oms",
    "ubn_[0-9]",
    "bvg_type_[0-9]",
    "datum_ingang_[0-9]",
    "datum_einde_[0-9]"
  )

  controleer_kolommen(
    verwacht_kolommen = kolommen_slachtingen,
    kolommen_in_df = colnames(df_slachtingen),
    naam_van_df = "df_slachtingen"
  )

  kolommen_slachthuizen <- c(
    "ubn",
    "bvg_type",
    "bvg_postcode_plaatscode",
    "bvg_postcode_lettercode",
    "bvg_huisnummer",
    "bvg_huisnummer_toevoeging",
    "bvg_straatnaam",
    "bvg_plaatsnaam",
    "x_coordinaat",
    "y_coordinaat",
    "status",
    "datum_ingang",
    "datum_einde",
    "datum_ingang_dst",
    "datum_einde_dst",
    "brs_nummer",
    "kvk_nr",
    "naam",
    "aantal"
  )

  controleer_kolommen(
    verwacht_kolommen = kolommen_slachthuizen,
    kolommen_in_df = colnames(df_slachthuizen),
    naam_van_df = "df_slachthuizen"
  )

  df_dieren <- df_slachtingen |>
    dplyr::select(
      levens_nr,
      datum_geboorte,
      datum_slacht,
      datum_eerste_afkalven,
      datum_import,
      geslacht,
      eld_code,
      landcode_herkomst,
      landcode_oorsprong,
      code_reden_einde,
      einde_oms
    ) |>
    dplyr::mutate(
      # leeftijd is ook berekend. Niet echte KV2
      leeftijd_jaren = round(lubridate::time_length(
        lubridate::interval(
          start = datum_geboorte,
          end = datum_slacht
        ),
        unit = "year"
      ), 4),
      diersoort = purrr::pmap_chr(
        .l = list(
          leeftijd_jaren,
          geslacht,
          datum_eerste_afkalven,
          datum_import
        ),
        \(l, g, d, i) rvo_diersoort_bepalen(
          leeftijd_jaar = l,
          geslacht = g,
          datum_afkalven = d,
          datum_import = i,
          koe = "Koeien",
          kalf_jongste = "Kalveren 0-8 mnd",
          kalf_oudste = "Kalveren 8-12 mnd",
          stier = "Stieren",
          vaars = "Vaarzen",
          overige = "Onbekend",
          max_maand_jongste_kalf = 9,
          max_maand_oudste_kalf = 12
        )
      )
    )

  df_dieren_op_locaties <- df_slachtingen |>
    dplyr::select(
      levens_nr, eld_code,
      tidyselect::starts_with(c(
        "ubn", "bvg_type",
        "datum_ingang", "datum_einde"
      ))
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c(
        "ubn", "bvg_type",
        "datum_ingang", "datum_einde"
      )),
      names_to = c(".value", "locatie_geschiedenis"),
      names_pattern = ("(.+)_([0-9])"),
      names_transform = list(
        ubn = as.character,
        bvg_type = as.character,
        datum_ingang = as.Date,
        datum_einde = as.Date,
        locatie_geschiedenis = as.integer
      )
    ) |>
    dplyr::filter(!is.na(ubn)) |>
    dplyr::mutate(
      verblijfsduur_dagen = lubridate::time_length(
        lubridate::interval(
          start = datum_ingang,
          end = datum_einde
        ),
        unit = "day"
      )
    )

  df_locaties <- dplyr::full_join(
    df_slachthuizen,
    df_dieren_op_locaties |> dplyr::distinct(ubn, bvg_type),
    by = dplyr::join_by(bvg_type, ubn)
  )



  list(
    dieren = df_dieren,
    dieren_op_locaties = df_dieren_op_locaties,
    locaties = df_locaties
  )
}

#' Verwerk ruwe of gecorrigeerd RVO slachtingen en in de database invullen
#'
#' Deze functie neemt als input twee data.frames die corresponderen met de
#' voorbewerkte bestanden die door RVO worden geleverd, dit zijn de bestanden
#' slachtingen en slachthuizen.
#'
#' De functie normaliseert eerst de gegevens zodat ze overeenkomen met de
#' databasestructuur. Daarna wordt voor elke tabel in KV2/KV3 gecontroleerd of de
#' records in de huidige levering al in de database aanwezig zijn.
#'
#' Als de records niet aanwezig zijn, worden ze eenvoudig toegevoegd aan de
#' overeenkomstige tabel.
#'
#' Als de records aanwezig zijn maar niet zijn bijgewerkt, wordt er niets gedaan.
#' Als ze zijn bijgewerkt, worden ze toegevoegd aan de database en worden de oude
#' records ongeldig gemaakt (door een eind_datum toe te voegen).
#'
#' @param df_slachtingen data.frame, output van
#'  inl_lees_csv_bestand_met_kolommen_config() van RVO-slachtingen bestand.
#' @param df_slachthuizen data.frame, output van
#'  inl_lees_csv_bestand_met_kolommen_config() van RVO-slachthuizen bestand.
#' @param con DBI connection, een verbinding met de database.
#' @param lev_id integer, de id van de levering die wordt verwerkt. Dit is de
#'  waarde van de kolom  levering_id van de tbl_levering_rvo tabel.
#' @param kv integer, de koppelvlak om in te vullen. Toegestane waarden zij 2 of 3.
#' @param vwmd character, verwerkingsmaand
#' @param tbl_dieren string, de naam van de dieren tabel. Default: App$tbl_dieren
#' @param tbl_locaties  string, de naam van de locaties tabel. Default:
#'  App$tbl_locaties
#' @param tbl_dieren_op_locaties string, de naam van de dieren_op_locaties tabel.
#'  Default: App$tbl_dieren_op_locaties
#' @param datum_f datetime, de datum die wordt toegevoegd aan de invoer_datum
#'  en eind_datum kolom.
#'
#' @return retourneert geen object, maar werkt wel de database bij.
#' @export
rvo_dieren_en_locaties_invullen <- function(
    df_slachtingen,
    df_slachthuizen,
    con,
    lev_id,
    kv,
    vwmd,
    verschil_huidige_mnd_eerste_mnd = App$verschil_huidige_mnd_eerste_mnd,
    tbl_dieren = App$tbl_dieren,
    tbl_locaties = App$tbl_locaties,
    tbl_dieren_op_locaties = App$tbl_dieren_op_locaties,
    schema = "dbo",
    datum_f) {
  
  # Normalizatie --------------------------------------------------------
  genormaliseerd_dataset <- rvo_dieren_en_locaties_bestanden_normaliseren(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen
  )

  df_dieren <- genormaliseerd_dataset$dieren
  df_locaties <- genormaliseerd_dataset$locaties
  df_dieren_op_locaties <- genormaliseerd_dataset$dieren_op_locaties
  
  # Database tabellen met schema voor DBI
  tbl_sc_dieren = DBI::Id(
    schema = schema, 
    table = tbl_dieren)
  
  tbl_sc_dieren_op_locaties = DBI::Id(
    schema = schema, 
    table = tbl_dieren_op_locaties)
  
  tbl_sc_locaties = DBI::Id(
    schema = schema, 
    table = tbl_locaties)
  
  # KV invullen ------------------------------------------------------------
  if (!(kv %in% c(2, 3))) {
    rlang::abort(
      "kv argument moet 2 of 3 zijn."
    )
  }
  
  ## Eerste en huidige maand datum verwerken ---------------------------------
  # We moeten de volledige maanden als volledige datums krijgen om ze in de
  # SQL-query te kunnen gebruiken. We willen de eerste dag van de eerste maand
  # en de laatste dag van de laatste maand
  datum_eerste_maand <- (lubridate::ym(vwmd) - months(abs(verschil_huidige_mnd_eerste_mnd))) |> 
    lubridate::rollbackward(roll_to_first = T)
  
  datum_huidige_maand <- lubridate::ym(vwmd) |> 
    lubridate::rollforward(roll_to_first = F)
  
  ## Dieren verwerken -------------------------------------------------------
  # Maak dieren ongeldig als slachting niet meer aanwezig is voor een van de 
  # in nieuwe levering
  
  if(kv == 2) {
    maak_verwijderde_dieren_ongeldig(
      df_dieren_huidige_levering = df_dieren,
      con = con,
      vwmd = vwmd,
      schema = schema,
      datum = datum_f
    )
  }

  qry_dieren <- glue::glue_sql(
    "
      SELECT
      *
      FROM {`tbl_sc_dieren`}
      WHERE	datum_slacht >= {datum_eerste_maand}
      	AND datum_slacht <= {datum_huidige_maand}
  ",
    .con = con
  ) 
  # browser()
  # We gaan alleen de dieren in de levering verwerken die binnen de maanden
  # vallen dat spek bedoeld is om te verwerken.
  df_dieren_levering_maanden <- df_dieren |> 
    dplyr::filter(
      datum_slacht >= datum_eerste_maand &
      datum_slacht <= datum_huidige_maand
    )
  # haal kv dieren
  df_db_dieren_huidige_kv <- DBI::dbGetQuery(conn = con, 
                                             statement = qry_dieren) 
  
  if(nrow(df_db_dieren_huidige_kv) > 0) {
    df_db_dieren_huidige_kv <- df_db_dieren_huidige_kv |> 
      dplyr::mutate(
      .by = c(levens_nr, eld_code, levering_id),
      kv_invoer_datum = ifelse(kv == 2,
                               min(invoer_datum, na.rm = TRUE),
                               max(invoer_datum, na.rm = TRUE)
      )
    ) |>
      dplyr::filter(
        .by = c(levens_nr, eld_code),
        levering_id == max(levering_id, na.rm = TRUE),
        invoer_datum == kv_invoer_datum
      ) |>
      dplyr::select(-kv_invoer_datum)
  }

  dieren_gemene_kolommen <- c(
    "levens_nr", "geslacht", "datum_geboorte",
    "datum_slacht", "datum_eerste_afkalven",
    "datum_import", "eld_code", "landcode_herkomst", 
    "landcode_oorsprong", "code_reden_einde"
  )
  
  df_dieren_levering_kv <- vergelijk_en_update(
    df_levering = df_dieren_levering_maanden,
    df_db = df_db_dieren_huidige_kv,
    ids = c("levens_nr", "eld_code"),
    pk_id = "dier_id",
    gemene_kolommen = dieren_gemene_kolommen,
    tabel = "tbl_dieren_kv2_kv3",
    tabel_met_levering_id = TRUE,
    datum = datum_f,
    lev_id = lev_id,
    con = con,
    schema = schema
  )
  ## Locaties verwerken ------------------------------------------------------
  # In dit geval halen we op de hele tabel met dplyr want we werwachten
  # niet dat de locaties tabel echt groot wordt
  df_db_locaties_huidige_kv <- dplyr::tbl(
    con, 
    dbplyr::in_schema(schema, tbl_locaties)) |> 
    dplyr::collect() 
  
  if (nrow(df_db_locaties_huidige_kv) > 0) {
    df_db_locaties_huidige_kv <- df_db_locaties_huidige_kv |> 
      # haal kv locaties
      dplyr::mutate(
        .by = c(ubn, levering_id),
        kv_invoer_datum = ifelse(kv == 2,
                                 min(invoer_datum, na.rm = TRUE),
                                 max(invoer_datum, na.rm = TRUE)
        )
      ) |>
      dplyr::filter(
        .by = ubn,
        levering_id == max(levering_id, na.rm = TRUE),
        invoer_datum == kv_invoer_datum
      ) |>
      dplyr::select(-kv_invoer_datum)
  }

  locaties_gemene_kolommen <- c(
    "ubn", "bvg_type",
    "bvg_postcode_plaatscode",
    "bvg_postcode_lettercode",
    "bvg_huisnummer", "bvg_huisnummer_toevoeging",
    "bvg_straatnaam", "bvg_plaatsnaam",
    "x_coordinaat", "y_coordinaat", "status",
    "datum_ingang", "datum_einde",
    "datum_ingang_dst", "datum_einde_dst",
    "brs_nummer", "kvk_nr", "naam", "aantal"
  )
  
  df_locaties_levering_kv <- vergelijk_en_update(
    df_levering = df_locaties,
    df_db = df_db_locaties_huidige_kv,
    ids = "ubn",
    pk_id = "locatie_id",
    gemene_kolommen = locaties_gemene_kolommen,
    tabel = "tbl_locaties_kv2_kv3",
    tabel_met_levering_id = TRUE,
    datum = datum_f,
    lev_id = lev_id,
    con = con,
    schema = schema
  )
  ## dieren op locaties verwerken ----------------------------------------------
  qry_dieren_op_locaties <- glue::glue_sql(
    "
      SELECT 
       dop.dier_op_locatie_id
      ,dop.dier_id
      ,dop.locatie_id
      ,dop.datum_ingang
      ,dop.datum_einde
      ,dop.locatie_geschiedenis
      ,dop.verblijfsduur_dagen
      ,dop.levering_id
      ,dop.invoer_datum
      ,dop.eind_datum
      ,d.levens_nr
      ,d.eld_code
      ,l.ubn
      FROM {`tbl_sc_dieren_op_locaties`} dop
      LEFT JOIN {`tbl_sc_dieren`} d
      ON  dop.dier_id = d.dier_id
      LEFT JOIN {`tbl_sc_locaties`} l
      ON  dop.locatie_id = l.locatie_id
      WHERE	d.datum_slacht >= {datum_eerste_maand}
      	AND d.datum_slacht <= {datum_huidige_maand}
      	AND dop.eind_datum IS NULL
      	AND d.eind_datum IS NULL
      	AND l.eind_datum IS NULL
  ",
    .con = con
  ) 
  
  # haal kv dieren_op_locaties met ubns, el_code, en levens_nr
  df_db_dieren_op_locaties_huidige_kv <- DBI::dbGetQuery(
      conn = con, 
      statement = qry_dieren_op_locaties)
  
  if(nrow(df_db_dieren_op_locaties_huidige_kv) > 0) {
    df_db_dieren_op_locaties_huidige_kv <- df_db_dieren_op_locaties_huidige_kv |> 
      # haal kv
      dplyr::mutate(
        .by = c(levens_nr, eld_code, ubn, levering_id),
        kv_invoer_datum = ifelse(kv == 2,
                                 min(invoer_datum, na.rm = TRUE),
                                 max(invoer_datum, na.rm = TRUE)
        )
      ) |>
      dplyr::filter(
        .by = c(levens_nr, eld_code, ubn),
        levering_id == max(levering_id, na.rm = TRUE),
        invoer_datum == kv_invoer_datum
      ) |>
      dplyr::select(-kv_invoer_datum)
  }
  
  qry_dieren <- glue::glue_sql(
    "
      SELECT
      dier_id
      ,levens_nr
      ,eld_code
      ,levering_id
      ,invoer_datum
      ,eind_datum
      FROM {`tbl_sc_dieren`} sl
      WHERE		datum_slacht >= {datum_eerste_maand}
      	AND datum_slacht <= {datum_huidige_maand}
  ",
    .con = con
  ) 
  
  qry_locaties <- glue::glue_sql(
    "
      SELECT
      locatie_id
      ,ubn
      ,levering_id
      ,invoer_datum
      ,eind_datum
      FROM {`tbl_sc_locaties`} sl
  ",
    .con = con
  ) 
  
  # We hebben dier_id en locatie_id nodig van de dieren en 
  # locaties die we gaan verwerken
  df_dieren_op_locaties_met_ids <- df_dieren_op_locaties |>
    dplyr::inner_join(
      df_dieren_levering_maanden |> dplyr::select(levens_nr, eld_code),
      by = dplyr::join_by(levens_nr, eld_code)
    ) |> 
    # hier gebruiken we inner_join dus we gaan alleen dieren verwerken die
    # al in de database zitten
    dplyr::inner_join(
      DBI::dbGetQuery(conn = con, statement = qry_dieren) |>
        dplyr::mutate(
          .by = c(levens_nr, eld_code, levering_id),
          kv_invoer_datum = ifelse(kv == 2,
                                   min(invoer_datum, na.rm = TRUE),
                                   max(invoer_datum, na.rm = TRUE)
          )
        ) |>
        dplyr::filter(
          .by = c(levens_nr, eld_code),
          levering_id == max(levering_id, na.rm = TRUE),
          invoer_datum == kv_invoer_datum
        ) |>
        dplyr::select(-kv_invoer_datum) |> 
        dplyr::select(dier_id, levens_nr, eld_code),
      by = c("levens_nr", "eld_code")
    ) |>
    # hier gebruiken we inner_join dus we gaan alleen dieren verwerken die
    # al in de database zitten
    dplyr::inner_join(
      DBI::dbGetQuery(conn = con, statement = qry_locaties) |>
        dplyr::mutate(
          .by = c(ubn, levering_id),
          kv_invoer_datum = ifelse(kv == 2,
                                   min(invoer_datum, na.rm = TRUE),
                                   max(invoer_datum, na.rm = TRUE)
          )
        ) |>
        dplyr::filter(
          .by = ubn,
          levering_id == max(levering_id, na.rm = TRUE),
          invoer_datum == kv_invoer_datum
        ) |>
        dplyr::select(-kv_invoer_datum) |> 
        dplyr::select(locatie_id, ubn),
      by = "ubn"
      )
  
  dieren_op_locaties_gemene_kolommen <- c(
    "dier_id", "locatie_id",
    "datum_ingang", "datum_einde",
    "locatie_geschiedenis",
    "verblijfsduur_dagen"
  )
  
  df_dieren_op_locaties_levering_kv <- vergelijk_en_update(
    df_levering = df_dieren_op_locaties_met_ids,
    df_db = df_db_dieren_op_locaties_huidige_kv,
    ids = c("levens_nr", "eld_code", "ubn"),
    pk_id = "dier_op_locatie_id",
    gemene_kolommen = dieren_op_locaties_gemene_kolommen,
    tabel = "tbl_dieren_op_locaties_kv2_kv3",
    tabel_met_levering_id = TRUE,
    datum = datum_f,
    lev_id = lev_id,
    con = con,
    schema = schema
  )
  
  # als records zijn in dieren_op_locaties bijgewert, moeten we
  # de slachtingen tabel ook bij te werken.
  df_db_bijgewerkt <- dplyr::tbl(con,
                                 dbplyr::in_schema(schema,
                                                   tbl_dieren_op_locaties)) |>
    dplyr::filter(eind_datum == datum_f) |>
    dplyr::select(dier_op_locatie_id) |>
    dplyr::collect()

  if (nrow(df_db_bijgewerkt) > 0) {
    # Bijgewerkt records in db krijgen eind_datum
    update_eind_datum(df_db_bijgewerkt,
                      id_kolom = "dier_op_locatie_id",
                      tbl_naam = "tbl_slachtingen_kv3",
                      schema = schema,
                      datum = datum_f,
                      con = con
    )
    
  }
}


#' KV3 correcties toepassen aan RVO bestanden
#'
#' Deze functie neemt als invoer de ruwe invoerbestanden (slachtingen en
#' slachthuizen) en modifierobjecten (afzonderlijk voor elk bestand) en past de
#' correcties toe. Als er correcties zijn aangebracht, wordt er een rapport
#' gemaakt en wordt de database bijgewerkt.
#'
#' @param df_slachtingen data.frame, de slachtingen IenR inputbestand zoals
#'   geretourneerd door de `inl_lees_csv_bestand_met_kolommen_config`-functie.
#' @param df_slachthuizen data.frame, de slachthuizen inputbestand zoals
#'   geretourneerd door de `inl_lees_csv_bestand_met_kolommen_config`-functie.#'
#'   @param correcties_slachtingen
#' @param correcties_slachtingen modifier objecy (van dcmodify package). Regels
#'   voor correcties aan slachtingen bestand
#' @param correcties_slachthuizen  modifier objecy (van dcmodify package).
#'   Regels voor correcties aan slachthuizen bestand
#' @param con DBI connection, een verbinding met de database.
#' @param lev_id integer, de id van de levering die wordt verwerkt. Dit is de
#'   waarde van de kolom  levering_id van de tbl_levering_rvo tabel.
#' @param pad_sjablon character, pad naar de sjablon voor de correcties rapport.
#' @param pad_outputmap character, pad naar de map waar de rapport wordt
#'   opgeslagen.
#' @param ... andere paramaters voor de `rvo_dieren_en_locaties_invullen`
#'   functie
#'
#' @return
#'  - bericht in de shiny app over de uitgevoerde correcties
#'  - bijgewerkte database (als er correcties zijn aangebracht)
#'  - html rapport (als er correcties zijn gemaakt)
#' @export
rvo_kv3_correcties <- function(
    df_slachtingen,
    df_slachthuizen,
    correcties_slachtingen = NULL,
    correcties_slachthuizen = NULL,
    con,
    lev_id,
    pad_sjablon,
    pad_outputmap,
    ...) {
  correcties_input <- list(
    slachtingen = list(
      df = df_slachtingen |> 
        dplyr::mutate(
          levens_nr_eld_code = paste0(levens_nr, "_", eld_code)
        ),
      correcties = correcties_slachtingen
    ),
    slachthuizen = list(
      df = df_slachthuizen,
      correcties = correcties_slachthuizen
    )
  )
  
  # Voer correcties uit voor elke dataset
  correcties_output <- purrr::imap(correcties_input, \(x, idx) {
    correcties_toepassen(
      df = x$df, correcties = x$correcties,
      pad_sjablon = pad_sjablon,
      pad_outputmap = pad_outputmap,
      datasetnaam = idx
    )
  })

  # We hoeven de database alleen bij te werken als er correcties zijn aangebracht
  if (correcties_output$slachtingen$correcties | correcties_output$slachthuizen$correcties) {
    rvo_dieren_en_locaties_invullen(
      df_slachtingen = correcties_output$slachtingen$df,
      df_slachthuizen = correcties_output$slachthuizen$df,
      lev_id = lev_id,
      con = con,
      kv = 3,
      ...
    )

    rapport_bestanden <- paste0(
      c(
        correcties_output$slachtingen$rapportpad,
        correcties_output$slachthuizen$rapportpad
      ),
      collapse = " en "
    )

    bericht <- glue::glue(
      "Automatische correcties werden toegepast op de RVO gegevens. Je ",
      "kunt vinden wat er precies is gecorrigeerd in het bestand(en): {rapport_bestanden}."
    )
  } else {
    bericht <- c(
      "Geen automatische correcties werden toegepast op de RVO gegevens",
      " omdat er geen fouten zijn gevonden"
    )
  }
  bericht
}


#' Koppel locaties in RVO met de Skal data
#'
#' @param df_locaties_kv3 data.frame, kv3 versie van de tbl_locaties_kv2_kv3
#' @param df_verblijfsplaatsen data.frame, de verblijfsplaatsen bestand
#' @param df_landbouwtelling data.frame, de landbouwtelling bestand
#' @param df_skal data.frame, de skal bestand
#'
#' @return list met data.frames:#' 
#'  - locaties_en_skal: data.frame met de locaties die waren met skal gekkopeld
#'  - de andere worden gebruikt om het rapport te maken 
#' @export
rvo_locaties_koppeling <- function(
    df_locaties_kv3,
    df_verblijfsplaatsen,
    df_landbouwtelling,
    df_skal) {
  controleer_kolommen(
    verwacht_kolommen = c(
      "locatie_id",
      "ubn",
      "bvg_postcode_plaatscode",
      "bvg_postcode_lettercode",
      "bvg_huisnummer",
      "bvg_huisnummer_toevoeging"
    ),
    kolommen_in_df = colnames(df_locaties_kv3),
    naam_van_df = "df_locaties_kv3"
  )

  controleer_kolommen(
    verwacht_kolommen = c(
      "ubn",
      "relnr"
    ),
    kolommen_in_df = colnames(df_verblijfsplaatsen),
    naam_van_df = "df_verblijfsplaatsen"
  )

  controleer_kolommen(
    verwacht_kolommen = c(
      "relnr",
      "bvg_postcode_plaatscode",
      "bvg_postcode_lettercode",
      "bvg_huisnummer",
      "bvg_huisnummer_toevoeging",
      "skalnr_lbt"
    ),
    kolommen_in_df = colnames(df_landbouwtelling),
    naam_van_df = "df_lanbouwtelling"
  )
  
  controleer_kolommen(
    verwacht_kolommen = c(
      "postcode",
      "huisnummer",
      "skalnummer",
      "datum_certificatie_geldigheid",
      "datum_geldig_vanaf"
    ),
    kolommen_in_df = colnames(df_skal),
    naam_van_df = "df_skal"
  )

  df_locaties_zonder_adres <- df_locaties_kv3 |>
    # we selecteren locaties zonder adres, dus verblijfsplaatsen
    dplyr::filter(
      is.na(bvg_postcode_plaatscode) |
        is.na(bvg_postcode_lettercode) |
        is.na(bvg_huisnummer)
    ) |>
    dplyr::select(-c(
      bvg_postcode_plaatscode,
      bvg_postcode_lettercode,
      bvg_huisnummer,
      bvg_huisnummer_toevoeging
    ))

  # Koppeling om de adressen te krijgen -------------------------------------
  
  # mergen met verblijfsplaatsen via ubn
  df_locaties_met_relnr <- dplyr::left_join(
    df_locaties_zonder_adres,
    df_verblijfsplaatsen,
    by = "ubn"
  )
  
  # we willen weten welke locaties kunnen niet worden gemerged
  df_locaties_zonder_relnr <- df_locaties_met_relnr |>
    dplyr::filter(is.na(relnr))
  # vervolgens voegen we samen de locaties met de landbouwtelling om de adressen
  # te krijgen
  df_locaties_met_relnr_met_adres <- dplyr::left_join(
    df_locaties_met_relnr,
    df_landbouwtelling,
    by = "relnr"
  )

  df_locaties_met_relnr_zonder_adres <- df_locaties_met_relnr_met_adres |>
    dplyr::filter(!is.na(relnr)) |>
    dplyr::filter(
      is.na(bvg_postcode_plaatscode) |
        is.na(bvg_postcode_lettercode) |
        is.na(bvg_huisnummer)
    )

  # We voegen alle locaties toe elkaar op om ze verder te verwerken
  df_locaties_niet_norm_adres <- dplyr::bind_rows(
    df_locaties_met_relnr_met_adres,
    df_locaties_kv3 |>
      dplyr::filter(!(is.na(bvg_postcode_plaatscode) |
        is.na(bvg_postcode_lettercode) |
        is.na(bvg_huisnummer)))
  )

  # we willen nu de locaties die niet gekoppeld konden worden en daarom geen
  # adres hebben. Dit is handig om ze te verwijderen uit verdere stappen en voor
  # het rapport
  df_locaties_zonder_adres_voor_norm <- df_locaties_niet_norm_adres |>
    dplyr::filter(
      is.na(bvg_postcode_plaatscode) |
        is.na(bvg_postcode_lettercode) |
        is.na(bvg_huisnummer)
    )
  
  # maak een dataframe met alle ubns waarvoor meerdere relnrs zijn gevonden
  df_locaties_meerdere_relnrs <- df_locaties_met_relnr[
    duplicated(df_locaties_met_relnr$ubn) | 
      duplicated(df_locaties_met_relnr$ubn, fromLast=TRUE),
  ] |> 
    subset(select=c(locatie_id, ubn, relnr))

  # Adressen normalisatie ---------------------------------------------------

  df_locaties_norm_adres <- df_locaties_niet_norm_adres |>
    # locaties zonder ander verwijderen want we kunnen niet niet aan skal
    # koppelen zonder adres
    dplyr::anti_join(
      df_locaties_zonder_adres_voor_norm |> dplyr::select(locatie_id),
      by = "locatie_id"
    ) |>
    adresnorm::normaliseer_pcht(
      velden = c(
        postcodecijfers = "bvg_postcode_plaatscode",
        postcodeletters = "bvg_postcode_lettercode",
        huisnummer = "bvg_huisnummer",
        huislettertoevoeging = "bvg_huisnummer_toevoeging"
      ),
      behoud.overig = TRUE
    )

  # we willen problematische adressen vinden. Die zijn adressen die leeg worden
  # na normalisatie

  # Eerste zoeken we genormaliseerd adressen die leeg zijn want adresnorm
  # package retourneert leeg string als een adres kan niet worden verwerkt.
  # De rijen van dit data.frame vertellen ons hoeveel adressen niet
  # genormaliseerd konden worden.
  df_locaties_zonder_adres_vanvege_norm <- df_locaties_norm_adres |>
    dplyr::filter(
      postcode == "" | huisnummer == ""
    ) |>
    dplyr::anti_join(
      df_locaties_zonder_adres_voor_norm |>
        dplyr::select(locatie_id),
      by = "locatie_id"
    )
  # Voor rapportage willen we weten wat de oorspronkelijke adressen waren van de
  # locaties die niet konden worden genormaliseerd
  df_locaties_problematisch_norm_adressen <- df_locaties_niet_norm_adres |>
    dplyr::inner_join(
      df_locaties_zonder_adres_vanvege_norm |>
        dplyr::select(locatie_id),
      by = "locatie_id"
    )

  # Verder gebruiken we alleen locaties met adressen
  df_locaties_norm_adres_schoon <- df_locaties_norm_adres |>
    dplyr::filter(
      postcode != "", huisnummer != ""
    ) |>
    dplyr::select(
      locatie_id,
      skalnr_lbt,
      postcode,
      huisnummer,
      huisletter,
      huisnummertoevoeging
    )

  # Skal adressen normalisatie, hetzelfde proces
  df_skal_zonder_adres <- df_skal |>
    dplyr::filter(
      is.na(huisnummer) | is.na(postcode) | 
        huisnummer == "" |  postcode == ""
    )

  # Locaties en skal samenvoegen --------------------------------------------
  df_locaties_en_skal <- dplyr::inner_join(
    df_locaties_norm_adres_schoon,
    df_skal,
    by = dplyr::join_by(postcode, huisnummer)
  )
  # We willen de locaties weten die niet zijn samengevoegd voor het rapporteren
  # van de gebruiker
  df_locaties_zonder_skal <- dplyr::anti_join(
    df_locaties_norm_adres_schoon,
    df_locaties_en_skal |> dplyr::select(locatie_id),
    by = dplyr::join_by(locatie_id)
  )

  list(
    locaties_zonder_adres = df_locaties_zonder_adres,
    locaties_zonder_relnr = df_locaties_zonder_relnr,
    locaties_meerdere_relnrs= df_locaties_meerdere_relnrs,
    locaties_met_relnr_zonder_adres = df_locaties_met_relnr_zonder_adres,
    locaties_zonder_adres_voor_norm = df_locaties_zonder_adres_voor_norm,
    locaties_problematisch_norm_adressen = df_locaties_problematisch_norm_adressen,
    locaties_zonder_adres_vanvege_norm = df_locaties_zonder_adres_vanvege_norm,
    locaties_zonder_skal = df_locaties_zonder_skal,
    locaties_en_skal = df_locaties_en_skal,
    skal_zonder_adres = df_skal_zonder_adres
  )
}



#' Biologische locaties verwerken en database bijwerken
#'
#' @param df_locaties_en_skal data.frame, output van `rvo_locaties_koppeling`
#'   functie
#' @param con DBI connection, een verbinding met de database.
#' @param bio_certificatie string, het woord of worden dat een certificaat
#'   definieert als biologisch in de Skal dataset
#' @param tbl_bio_locaties string, de naam van de bio locaties in de database
#' @param datum_f datetime, de datum die wordt toegevoegd aan de invoer_datum
#'  en eind_datum kolom.
#'
#' @return bijgewerkt database
#' @export
rvo_kv3_bio_locaties <- function(
    df_locaties_en_skal,
    con,
    bio_certificatie = "Biologisch",
    tbl_bio_locaties = App$tbl_bio_locaties,
    datum_f,
    schema = "dbo") {
  controleer_kolommen(
    verwacht_kolommen = c(
      "locatie_id",
      "resultaat_certificatie",
      "datum_certificatie_geldigheid",
      "datum_geldig_vanaf"
    ),
    kolommen_in_df = colnames(df_locaties_en_skal),
    naam_van_df = "df_locaties_en_skal"
  )
  
  df_bio_locaties <- df_locaties_en_skal |>
    dplyr::filter(resultaat_certificatie %in% bio_certificatie) |>
    dplyr::mutate(
      certificaat_geldig_vanaf = as.Date(datum_geldig_vanaf),
      certificaat_geldig_tot = as.Date(datum_certificatie_geldigheid)
    ) |>
    dplyr::select(
      locatie_id,
      certificaat_geldig_vanaf,
      certificaat_geldig_tot
    )

  df_db_bio_locaties_huidige_kv3 <- dplyr::tbl(
      con,  dbplyr::in_schema(
        schema,
        "tbl_biologische_locaties_kv3")) |>
    dplyr::filter(
      is.na(eind_datum)
    ) |> 
    dplyr::collect()

  bio_locaties_gemene_kolommen <- c(
    "locatie_id",
    "certificaat_geldig_vanaf",
    "certificaat_geldig_tot"
  )
  
  df_bio_locaties_levering_kv3 <- vergelijk_en_update(
    df_levering = df_bio_locaties,
    df_db = df_db_bio_locaties_huidige_kv3,
    ids = c("locatie_id"),
    pk_id = "biologische_locatie_id",
    gemene_kolommen = bio_locaties_gemene_kolommen,
    tabel = "tbl_biologische_locaties_kv3",
    datum_kolommen = c("datum", "certificaat"),
    tabel_met_levering_id = FALSE,
    datum = datum_f,
    con = con,
    schema = schema
  )
}

#' Slachtigen database invullen
#'
#' deze functie gebruikt de KV3-informatie van de verschillende tabellen in de
#' database om de tabel met slachtingen bij te werken en te bepalen of een
#' slachting biologisch was of niet
#'
#' @param con DBI connection, een verbinding met de database.
#' @param lev_id integer, de id van de levering die wordt verwerkt. Dit is de
#'   waarde van de kolom  levering_id van de tbl_levering_rvo tabel.
#' @param tbl_dieren string, de naam van de dieren tabel. Default:
#'   App$tbl_dieren
#' @param tbl_locaties  string, de naam van de locaties tabel. Default:
#'   App$tbl_locaties
#' @param tbl_dieren_op_locaties string, de naam van de dieren_op_locaties
#'   tabel. Default: App$tbl_dieren_op_locaties
#' @param tbl_bio_locaties string, de naam van de bio locaties tabel. Default:
#'   App$tbl_dieren_op_locaties
#' @param tbl_slachtingen string, de naam van de slachtingen tabel. Default:
#'   App$tbl_dieren_op_locaties
#' @param kv3_datum_f datetime, de datum die wordt toegevoegd aan de
#'   invoer_datum en eind_datum kolom.
#'
#' @return
#'  - Bijgewerkt database
#'  - data.frame met aantal slachtingen per slachthuiz die wordt gebruikt om de SPEK input aan te maken
#' @export
rvo_kv3_slachtingen <- function(
    con,
    lev_id,
    tbl_dieren = App$tbl_dieren,
    tbl_locaties = App$tbl_locaties,
    tbl_dieren_op_locaties = App$tbl_dieren_op_locaties,
    tbl_bio_locaties = App$tbl_bio_locaties,
    tbl_slachtingen = App$tbl_slachtingen,
    kv3_datum_f,
    schema= "dbo",
    verschil_huidige_mnd_eerste_mnd = App$verschil_huidige_mnd_eerste_mnd,
    vwmd) {
  
  ## Eerste en huidige maand datum verwerken ---------------------------------
  # We moeten de volledige maanden als volledige datums krijgen om ze in de
  # SQL-query te kunnen gebruiken. We willen de eerste dag van de eerste maand
  # en de laatste dag van de laatste maand
  datum_eerste_maand <- (lubridate::ym(vwmd) - months(abs(verschil_huidige_mnd_eerste_mnd))) |> 
    lubridate::rollbackward(roll_to_first = T)
  
  datum_huidige_maand <- lubridate::ym(vwmd) |> 
    lubridate::rollforward(roll_to_first = F)
  ## Slachtingen tabel -------------------------------------------------------
  tbl_sc_dieren_op_locaties = DBI::Id(
    schema = schema, 
    table = tbl_dieren_op_locaties)
  
  tbl_sc_dieren = DBI::Id(schema = schema, table = tbl_dieren)
  
  tbl_sc_locaties = DBI::Id(schema = schema, table = tbl_locaties)
  
  # Haal dieren_op_locaties gegevens
  ## We hebben nodig van bvg_type en levens_nr om de  slacthing later te bepalen
  ## We gaan gegevens ophalen van de huidige maand (verwerkingsmaand) en een
  #   aantal maanden daarvoor. Dit aantal is momenteel 3 (dus 4 maanden in
  #   totaal), maar het wordt gedefinieerd in het configuratiebestand.
  ## We willen de gegevens van de geselecteerde maanden verwerken in plaats van
  #   alleen die van de huidige levering. De reden hiervoor is dat de informatie
  #   over de bio-locaties in deze bewerking kan zijn veranderd, dus als er items
  #   in dieren_op_locaites zijn die in de vorige bewerking zijn verwerkt en nog
  #   steeds aanwezig en ongewijzigd zijn in de huidige bewerking, zullen ze de
  #   levering_id van de vorige maand hebben. Door te filteren op datum (en
  #   eind_datum NULL te nemen), zorgen we ervoor dat we alle momenteel geldige
  #   records krijgen voor de geselecteerde maanden
  # 
  qry_dieren_op_locaties <- glue::glue_sql(
    "
      SELECT
       dop.dier_op_locatie_id
      ,dop.dier_id
      ,dop.locatie_id
      ,dop.datum_ingang
      ,dop.datum_einde
      ,dop.locatie_geschiedenis
      ,dop.verblijfsduur_dagen
      ,dop.eind_datum
      ,d.diersoort
      ,d.leeftijd_jaren
      ,d.levens_nr
      ,l.bvg_type
      FROM {`tbl_sc_dieren_op_locaties`} dop
      LEFT JOIN {`tbl_sc_dieren`} d
      ON  dop.dier_id = d.dier_id
      LEFT JOIN {`tbl_sc_locaties`} l
      ON  dop.locatie_id = l.locatie_id
      WHERE	d.datum_slacht >= {datum_eerste_maand}
      	AND d.datum_slacht <= {datum_huidige_maand}
      	AND dop.eind_datum IS NULL
      	AND d.eind_datum IS NULL
      	AND l.eind_datum IS NULL
  ",
    .con = con
  ) 
  
  df_db_dieren_op_locaties_huidige_kv <- DBI::dbGetQuery(
    conn = con, 
    statement = qry_dieren_op_locaties)
  
  df_diersoort <- df_db_dieren_op_locaties_huidige_kv |>
    dplyr::mutate(
      dplyr::across(c(tidyselect::starts_with("datum")),
        .fns = lubridate::as_date
      )
    ) |>
    dplyr::select(-tidyselect::ends_with("_datum")) |>
    dplyr::left_join(
      dplyr::tbl(
          con, 
          dbplyr::in_schema(schema, tbl_bio_locaties)) |>
        dplyr::filter(
          is.na(eind_datum)
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          is_locatie_biologisch = TRUE,
          certificaat_geldig_vanaf = lubridate::as_date(certificaat_geldig_vanaf),
          certificaat_geldig_tot = lubridate::as_date(certificaat_geldig_tot)
        ) |>
        dplyr::select(-tidyselect::ends_with("_datum"), -diersoort),
      by = dplyr::join_by(
        locatie_id,
        within(
          datum_ingang,
          datum_einde,
          certificaat_geldig_vanaf,
          certificaat_geldig_tot
        )
      )
    ) |>
    dplyr::select(-tidyselect::starts_with("certificaat")) |>
    # De rijen die niet werden samengevoegd (en dus NA's hebben in de
    # is_locatie_biologisch-kolom) zijn niet biologisch. Dus zetten we ze op FALSE
    tidyr::replace_na(
      list(is_locatie_biologisch = FALSE)
    )
  
  # slachting bio/niet nio bepalen
  df_slachtingen <- rvo_slachting_type_bepalen(
    df = df_diersoort,
    geldig_locatie_types = c("VH", "SP")
  )

  tbl_sc_slachtingen = DBI::Id(schema = schema, table = tbl_slachtingen)
  
  qry <- glue::glue_sql(
    "
      SELECT
       sl.slachting_id
      ,sl.dier_op_locatie_id
      ,sl.is_biologisch
      ,sl.invoer_datum
      ,sl.eind_datum
      FROM {`tbl_sc_slachtingen`} sl
      LEFT JOIN {`tbl_sc_dieren_op_locaties`} d_l
      	ON		sl.dier_op_locatie_id = d_l.dier_op_locatie_id
      LEFT JOIN {`tbl_sc_dieren`} d
      	ON		d_l.dier_id = d.dier_id
      WHERE	d.datum_slacht >= {datum_eerste_maand}
      	AND d.datum_slacht <= {datum_huidige_maand}
      	AND sl.eind_datum IS NULL
      	AND d_l.eind_datum IS NULL
      	AND d.eind_datum IS NULL
  ",
    .con = con
  )
  df_db_slachtingen_huidige_kv3 <- DBI::dbGetQuery(conn = con,
                                                   statement = qry)

  slachtingen_gemene_kolommen <- c(
    "dier_op_locatie_id",
    "is_biologisch"
  )
  
  df_slachtingen_kv3 <- vergelijk_en_update(
    df_levering = df_slachtingen,
    df_db = df_db_slachtingen_huidige_kv3,
    ids = c("dier_op_locatie_id"),
    pk_id = "slachting_id",
    gemene_kolommen = slachtingen_gemene_kolommen,
    tabel = "tbl_slachtingen_kv3",
    datum_kolommen = c("datum"),
    tabel_met_levering_id = FALSE,
    datum = kv3_datum_f,
    con = con,
    schema = schema
  )
  df_output <- data.frame()

  if (nrow(df_slachtingen_kv3) > 0) {
    df_output <- df_slachtingen_kv3 |>
      dplyr::left_join(
        dplyr::tbl(con, dbplyr::in_schema(schema, tbl_dieren_op_locaties)),
        copy = TRUE,
        by = "dier_op_locatie_id"
      ) |>
      dplyr::left_join(
        dplyr::tbl(
            con, dbplyr::in_schema(schema, tbl_locaties)) |> 
          dplyr::select(locatie_id, ubn, naam),
        copy = TRUE,
        by = "locatie_id"
      ) |>
      dplyr::mutate(
        jaar = lubridate::year(lubridate::as_date(datum_einde)),
        maand = lubridate::month(lubridate::as_date(datum_einde)),
      ) |>
      dplyr::summarise(
        .by = c(jaar, maand, ubn, naam, diersoort, is_biologisch),
        aantal_angeboden = dplyr::n()
      )
  }

  return(df_output)
}

#' Haal verwerkt slachtingen uit database en bereken slachtingen per slachthuis,
#' diersoort, en bio. Dus Rustpunt 3.1 
#'
#' @param con DBI connection, een verbinding met de database.
#' @param vwmd character, verwerkingsmaand
#' @param verschil_huidige_mnd_eerste_mnd numeric, Verschil tussen verwerkingsmaand 
#' en oudste verslagmaand. Default App$verschil_huidige_mnd_eerste_mnd
#' @param tbl_dieren string, de naam van de dieren tabel. Default: App$tbl_dieren
#' @param tbl_locaties  string, de naam van de locaties tabel. Default:
#'  App$tbl_locaties
#' @param tbl_dieren_op_locaties string, de naam van de dieren_op_locaties tabel.
#'  Default: App$tbl_dieren_op_locaties
#' @param tbl_slachtingen string, de naam van de slachtingen tabel.
#'  Default: App$tbl_slachtingen
#' @param schema string, database schema. Default:App$databaseschema
#'
#' @return data.frame met kolommen:
#'  jaar, maand, diersoort, ubn, naam, is_biologisch, aantal_angeboden  
#' @export
rvo_maak_rp3_1 <- function(
    con,
    vwmd,
    verschil_huidige_mnd_eerste_mnd = App$verschil_huidige_mnd_eerste_mnd,
    tbl_dieren = App$tbl_dieren,
    tbl_locaties = App$tbl_locaties,
    tbl_dieren_op_locaties = App$tbl_dieren_op_locaties,
    tbl_slachtingen = App$tbl_slachtingen,
    schema = App$databaseschema
    ) {
  
  tbl_sc_dieren = DBI::Id(schema = schema, table = tbl_dieren)
  tbl_sc_locaties = DBI::Id(schema = schema, table = tbl_locaties)
  tbl_sc_dieren_op_locaties = DBI::Id(schema = schema, table = tbl_dieren_op_locaties)
  tbl_sc_slachtingen = DBI::Id(schema = schema, table = tbl_slachtingen)
  
  # We moeten de volledige maanden als volledige datums krijgen om ze in de
  # SQL-query te kunnen gebruiken. We willen de eerste dag van de eerste maand
  # en de laatste dag van de laatste maand
  datum_eerste_maand <- (lubridate::ym(vwmd) - months(abs(verschil_huidige_mnd_eerste_mnd))) |> 
    lubridate::rollbackward(roll_to_first = T)
  
  datum_huidige_maand <- lubridate::ym(vwmd) |> 
    lubridate::rollforward(roll_to_first = F)
  
  glue::glue_sql(
    "
      SELECT
      	YEAR(d.datum_slacht) AS jaar
      	,MONTH(d.datum_slacht) AS maand
      	,d.diersoort
      	,l.ubn
      	,l.naam
      	,sl.is_biologisch
      FROM {`tbl_sc_slachtingen`} sl
      LEFT JOIN {`tbl_sc_dieren_op_locaties`} d_l
      	ON		sl.dier_op_locatie_id = d_l.dier_op_locatie_id
      LEFT JOIN {`tbl_sc_dieren`} d
      	ON		d_l.dier_id = d.dier_id
      LEFT JOIN {`tbl_sc_locaties`} l
      	ON		d_l.locatie_id = l.locatie_id
      WHERE	d.datum_slacht >= {datum_eerste_maand}
      	AND d.datum_slacht <= {datum_huidige_maand}
      	AND sl.eind_datum IS NULL
      	AND d_l.eind_datum IS NULL
      	AND d.eind_datum is NULL
      	AND l.eind_datum IS NULL
  ",
    .con = con
  ) |> 
    DBI::dbGetQuery(conn = con, statement = _) |> 
    dplyr::summarise(
      .by = c(jaar, maand, ubn, naam, diersoort, is_biologisch),
      aantal_angeboden = dplyr::n()
    )
}


#' Formatteer RP3.1 om overeen te komen met de SPEK-input
#'
#' @param df data.frame, output van rvo_kv3_slachtingen
#'
#' @return data.frame
#' @export
rvo_formaat_rp3_1_naar_spek_input <- function(df) {
  df |>
    dplyr::mutate(
      # biologisch = ifelse(is_biologisch,
      #                     "Bio",
      #                     NA_character_),
      ubn = paste0("UBN:", ubn)
    ) |>
    dplyr::select(
      Jaar = jaar,
      Maand = maand,
      DIER_SOORT_OMSCHR = diersoort,
      WERKPLEK_NR = ubn,
      WERKPLEKNAAM = naam,
      AANTAL_AANGEBODEN = aantal_angeboden,
      BIOLOGISCH = is_biologisch
    )
}
