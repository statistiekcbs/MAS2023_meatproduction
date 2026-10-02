#' Update de eind_datum kolom van een tabel.
#' Meerdere rijen kunnen worden bijgewerkt.
#'
#' @param df data.frame, moet een kolom bevatten met de primary key kolom van de
#'   tabel die wordt bijgewerkt. De waarden in deze kolom worden gebruikt in de
#'   UPDATE SQL-clause om de rijen te identificeren die moeten worden bijgewerkt.
#' @param id_kolom character, the naam van de primary key kolom.
#' @param tbl_naam character, the naam van de tabel.
#' @param datum datetime, de datum die wordt toegevoegd aan de eind_datum kolom.
#' @param con DBIConnection object, de verbinding met de database.
#'
#' @return niks
#' @export
update_eind_datum <- function(df, id_kolom, tbl_naam, schema = "dbo", datum, con, chunk_size = 5000) {
  
  # Alleen unieke, geldige IDs gebruiken
  ids <- df |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(id_kolom))
    ) |>
    dplyr::pull(dplyr::all_of(id_kolom)) |>
    stats::na.omit()
  
  # Niets te doen als er geen IDs zijn
  if (length(ids) == 0) {
    return(invisible(NULL))
  }
  
  # IDs opdelen in blokken van maximaal 5000
  chunks <- split(
    ids,
    ceiling(seq_along(ids) / chunk_size)
  )
  
  # Tabelnaam opbouwen
  tbl_sql <- DBI::Id(
    schema = schema,
    table = tbl_naam
  )
  
  #! ID-kolom veilig quoten
  id_sql <- DBI::dbQuoteIdentifier(
    con,
    id_kolom
  )
  
  # Per chunk 1 UPDATE uitvoeren 
  aangepast_rijen <- purrr::map_int(
    chunks,
    \(ids_chunk) {
      
      # Eén SQL-query voor maximaal 5000 IDs
      qry <- glue::glue_sql(
        "
        UPDATE {`schema`}.{`tbl_naam`}              
        SET eind_datum = {datum}
        WHERE {`id_kolom`} IN ({ids_chunk*})
        ",
        .con = con
      )
      
      DBI::dbExecute(
        conn = con,
        statement = qry
      )
    }
  ) |>
    sum()
  
 
  if (aangepast_rijen > 0) {
    message(
      glue::glue(
        "eind datum van {aangepast_rijen} rijen aangepast in tabel {tbl_naam}"
      )
    )
  }
  
  invisible(NULL) 
}


#' Een data.frame vergelijken met een tabel in de database (of een anderere
#' data.frame) en nieuwe records toevoegen en/of bestaande records bijwerken.
#'
#' Deze functie wordt gebruikt tijdens het verwerken van een nieuwe levering.
#' Voor elke tabel die nieuw moet worden ingevuld, wordt deze functie gebruikt
#' om te controleren of er nieuwe records zijn vergeleken met de huidige status
#' van de database. Daarnaast worden de nieuwe records vergeleken met de huidige
#' records op basis van een set kolommen, om te controleren of de nieuwe records
#' bijgewerkte informatie bevatten. Als dat zo is, krijgen de huidige
#' databaserecords een eind_datum en worden de nieuwe, bijgewerkte records aan
#' de tabel toegevoegd.
#' @param df_levering data.frame, de gegevens van de nieuwe levering.
#' @param df_db  data.frame, de huidige gegevens in de database tabel. 
#' @param ids character, de kolomnaam(en) met de ID(s) van de tabel die wordt
#'   verwerkt. Dit zijn niet de primary key-kolommen (surrogate key in database
#'   jargon), maar de natural key (ook database jargon). Bijvoorbeeld, in de
#'   dieren tabel, levens_nr en eld_code zijn de natural keys. Voor de locaties
#'   tabel, ubn is de natural key. Ze identificeren allebei op unieke wijze een
#'   individu (dier of locatie).
#' @param pk_id charater, kolomnaam van de primary key van de tabel.
#' @param gemene_kolommen character, de kolomnamen van de kolommen die moeten
#'   worden vergeleken om te controleren of informatie is bijgewerkt.
#' @param datum_kolommen character, een algemene string of regex die alle
#'   datumkolommen identificeert. Deze wordt gebruikt om deze kolommen te
#'   converteren naar het datumtype.
#' @param tabel character, de naam van de database tabel.
#' @param tabel_met_levering_id boolean, bevat deze tabel een levering_id kolom?
#' @param con DBIConnection object, de verbinding met de database.
#' @param lev_id integer, de nieuwe levering_id
#' @param datum datetime,  de nieuwe datum voor invoer_datum een eind_datum
#' @param log_map pad naar de logmap waar de error bestand wordt opgeslagd
#'   kolommen
#'
#' @return
#' @export
vergelijk_en_update <- function(
    df_levering,
    df_db,
    ids,
    pk_id,
    gemene_kolommen,
    datum_kolommen = "datum",
    tabel,
    database = App$databasenaam,
    server = App$dbservernaam,
    schema = "dbo",
    tabel_met_levering_id = FALSE,
    con,
    lev_id,
    datum,
    log_map = App$logmap) {
  # controleren of eerdere records zijn aangepast
  df_db_bijgewerkt <- df_db |>
    # alleen vergelijk regels met dezelfde ID
    dplyr::inner_join(
      df_levering |> dplyr::select(tidyselect::all_of(ids)),
      by = ids,
      copy = TRUE
    ) |>
    # we moeten de datum kolommen naar date type te transformeren
    dplyr::mutate(
      dplyr::across(tidyselect::contains(datum_kolommen),
        .fns = lubridate::as_date
      )
    ) |>
    # anti join om de niet gelijk regels te vinden
    dplyr::anti_join(
      df_levering |>
        dplyr::select(tidyselect::all_of(gemene_kolommen)),
      by = gemene_kolommen
    ) |>
    dplyr::select(-c(invoer_datum, eind_datum))

  # De nieuwe records zijn die waarvan het id niet aanwezig is in de db
  df_levering_nieuwe_records <- df_levering |>
    dplyr::anti_join(
      df_db |> dplyr::select(tidyselect::all_of(ids)),
      by = ids,
      copy = TRUE
    )

  df_naar_db <- data.frame()

  if (nrow(df_db_bijgewerkt) > 0) {
    # Bijgewerkt records in db krijgen eind_datum
    update_eind_datum(df_db_bijgewerkt,
      id_kolom = pk_id,
      tbl_naam = tabel,
      schema = schema,
      datum = datum,
      con = con
    )
    # We moeten de bijgewerkte records toevoegen aan de database
    df_levering_bijgewerkt_in_db <- df_levering |>
      dplyr::inner_join(
        df_db_bijgewerkt |> dplyr::select(tidyselect::all_of(ids)),
        by = ids
      )

    df_naar_db <- dplyr::bind_rows(
      df_naar_db,
      df_levering_bijgewerkt_in_db
    )
  }

  if (nrow(df_levering_nieuwe_records) > 0) {
    df_naar_db <- dplyr::bind_rows(
      df_naar_db,
      df_levering_nieuwe_records
    )
  }

  if (nrow(df_naar_db) > 0) {
    if (tabel_met_levering_id) {
      df_naar_db <- df_naar_db |>
        dplyr::mutate(
          levering_id = lev_id,
          invoer_datum = lubridate::as_datetime(datum)
        )
    } else {
      df_naar_db <- df_naar_db |>
        dplyr::mutate(
          invoer_datum = lubridate::as_datetime(datum)
        )
    }
     
    df_naar_db <- dplyr::tbl(con, dbplyr::in_schema(schema, tabel)) |> 
      head(0) |> 
      dplyr::collect() |> 
      dplyr::mutate(
        dplyr::across(tidyselect::contains(datum_kolommen),
                      .fns = lubridate::as_date
        )
      ) |> 
      dplyr::bind_rows(
        df_naar_db 
      ) |> 
      dplyr::select(-tidyselect::any_of(pk_id))
    
    db_opslaan_bcp(server = server, database = database, tabel = tabel,
                   schema = schema,
                   data = df_naar_db, skip_eerste_col = TRUE,
                   log_map = log_map)
    
      message(glue::glue(
        "{nrow(df_naar_db)} rijen toegevoegd aan tabel {tabel}."
      ))
  }
  return(df_naar_db)
}

#' Maak dieren ongeldig als slachting niet meer aanwezig is voor een van de 
#' verslagmanden in nieuwe levering. Voorbeeld:
#' 
#' In levering van april is er een slachting met levens_nr X (en eld_code NL) 
#' en slacht datum 02-04-24. Dit wordt aan de database toegevoegd met dier_id 45. 
#' 
#' In levering van mei is levens_nr X (en NL) niet aanwezig. 
#' 
#' Record dier_id 45 krijgt eind_datum.
#'
#' @param df_dieren_huidige_levering data.frame, dieren in huidige levering
#' @param vwmd character, verwerkingsmaand
#' @param verschil_huidige_mnd_eerste_mnd numeric, Verschil tussen verwerkingsmaand 
#' en oudste verslagmaand. Default App$verschil_huidige_mnd_eerste_mnd
#' @param con DBI connection, een verbinding met de database.
#' @param tbl_dieren string, de naam van de dieren tabel. Default: App$tbl_dieren
#' @param schema string, database schema. Default:App$databaseschema
#' @param datum_f datetime, de datum die wordt toegevoegd aan de eind_datum kolom.
#' 
#' @return retourneert een bericht en werkt de database bij.
#' @export
maak_verwijderde_dieren_ongeldig <- function(
    df_dieren_huidige_levering, 
    vwmd,
    verschil_huidige_mnd_eerste_mnd = App$verschil_huidige_mnd_eerste_mnd,
    con,
    tbl_dieren = App$tbl_dieren,
    tbl_dieren_op_locaties = App$tbl_dieren_op_locaties,
    tbl_slachtingen = App$tbl_slachtingen,
    schema = App$databaseschema,
    datum_f) {
  # We moeten de volledige maanden als volledige datums krijgen om ze in de
  # SQL-query te kunnen gebruiken. We willen de eerste dag van de eerste maand
  # en de laatste dag van de laatste maand
  datum_eerste_maand <- (lubridate::ym(vwmd) - months(abs(verschil_huidige_mnd_eerste_mnd))) |> 
    lubridate::rollbackward(roll_to_first = T)
  
  datum_huidige_maand <- lubridate::ym(vwmd) |> 
    lubridate::rollforward(roll_to_first = F)
  
  tbl_sc_dieren <- DBI::Id(schema = schema, table = tbl_dieren)
  
  df_db_dieren <- glue::glue_sql(
    "
    SELECT
	    dier_id
	    ,levens_nr
	    ,eld_code
    FROM {`tbl_sc_dieren`}
    WHERE	datum_slacht >= {datum_eerste_maand}
	    AND datum_slacht <= {datum_huidige_maand}
	    AND eind_datum IS NULL
    ",
    .con = con
  ) |> 
    DBI::dbGetQuery(conn = con, statement = _)
  
 df_verwijderd_dieren <- dplyr::anti_join(
   df_db_dieren,
   df_dieren_huidige_levering,
   by = dplyr::join_by(levens_nr, eld_code)
 )
 
 n_verwijderd <- nrow(df_verwijderd_dieren)
 if (n_verwijderd > 0) {
   
   update_eind_datum(df_verwijderd_dieren,
                     id_kolom = "dier_id",
                     tbl_naam = tbl_dieren,
                     schema = schema,
                     datum = datum_f,
                     con = con
   )   

   # als records zijn in dieren_op_locaties bijgewert, moeten we
   # de slachtingen tabel ook bij te werken.
   df_db_dieren_op_locatie_bijgewerkt <- dplyr::tbl(con,
                                  dbplyr::in_schema(schema,
                                                    tbl_dieren_op_locaties)) |>
     dplyr::filter(eind_datum == datum_f) |>
     dplyr::select(dier_op_locatie_id) |>
     dplyr::collect()
   
   if (nrow(df_db_dieren_op_locatie_bijgewerkt) > 0) {
     # Bijgewerkt records in db krijgen eind_datum
     update_eind_datum(df_db_dieren_op_locatie_bijgewerkt,
                       id_kolom = "dier_op_locatie_id",
                       tbl_naam = "tbl_slachtingen_kv3",
                       schema = schema,
                       datum = datum_f,
                       con = con
     )
     
   }
      
   ids <- df_verwijderd_dieren |> 
     dplyr::mutate(
       id = paste0(levens_nr, ":", eld_code)
     ) |> 
     dplyr::pull(id) |> 
     paste(collapse = ", ")
   
   bericht <- glue::glue(
     "{n_verwijderd} slachtingen die aanwezig waren in de vorige RVO levering",
     " voor de huidige rapporteringsmaanden zijn afwezig in de huidige levering.",
     " Deze zijn ongeldig gemaakt in de database en worden niet gebruikt voor ",
     " verdere analyse. De levens_nr+eld_code van de dieren zijn: {ids}"
   )
   message(bericht)
   }
 }

#' Controleer of een data.frame de verwachte kolom heeft en gooi een fout als
#' dat niet het geval is
#'
#' @param verwacht_kolommen character vector, de namen van de verwachte kolommen
#' @param kolommen_in_df character vector, de werkelijke kolommen in het
#'   data.frame
#' @param naam_van_df character, de naam van de data.frame. Het wordt gebruikt
#'   in de fout bericht.
#' @param call environment, de environment van waaruit de functie wordt
#'   aangeroepen. Dit wordt doorgegeven aan rlang::abort()
#'
#' @return
#' @export
controleer_kolommen <- function(verwacht_kolommen,
                                kolommen_in_df,
                                naam_van_df,
                                call = rlang::caller_env()) {
  is_kolom_aanwezig <- purrr::map_lgl(
    verwacht_kolommen,
    \(verwacht_kol) {
      any(stringr::str_detect(
        string = kolommen_in_df,
        pattern = verwacht_kol
      ))
    }
  )

  if (!all(is_kolom_aanwezig)) {
    missing_kol <- verwacht_kolommen[!is_kolom_aanwezig]
    rlang::abort(
      glue::glue(
        "{missing_kol} kolom(en) is niet aanwezig",
        " in {naam_van_df}"
      ),
      call = call
    )
  }
}


#' Bepaal de diersoort voor runderen
#'
#' @param leeftijd_jaar numeric, de leeftijd van het dier.
#' @param geslacht character, de geslacht van het dier. Gelidige waardes zijn
#'   "V", "M" of NA
#' @param datum_afkalven  date, de datum van de eerste kalving. Kan leeg zijn.
#' @param koe character, de naam van het diersoort voor koeien.
#' @param kalf_jongste character, de naam van het diersoort voor jongste
#'   kalveren, met leeftijd tussen 0 en `max_maand_jongste_kalf` maanden.
#' @param kalf_oudste character, de naam van het diersoort voor oudste kalveren,
#'   met leeftijd tussen `max_maand_jongste_kalf` en `max_maand_oudste_kalf`
#'   maanden.
#' @param stier  character, de naam van het diersoort voor stieren.
#' @param vaars  character, de naam van het diersoort voor vaarzen.
#' @param overige character, de naam van het diersoort voor overige.
#' @param max_maand_jongste_kalf numeric, het maximale aantal maanden voor de
#'   jongste categorie kalveren.
#' @param max_maand_oudste_kalf numeric, het maximale aantal maanden voor de
#'   oudste categorie kalveren.
#'
#' @return character vector met lengte 1.
#' @export
rvo_diersoort_bepalen <- function(
    leeftijd_jaar,
    geslacht,
    datum_afkalven,
    datum_import,
    koe = "Koeien",
    kalf_jongste = "Kalveren 0-8 mnd",
    kalf_oudste = "Kalveren 8-12 mnd",
    stier = "Stieren",
    vaars = "Vaarzen",
    overige = "Onbekend",
    max_maand_jongste_kalf = 9,
    max_maand_oudste_kalf = 12) {
  geldige_geslachten <- c("M", "V", NA)

  if (!(geslacht %in% geldige_geslachten)) {
    opties <- paste0(geldige_geslachten, collapse = ", ")
    rlang::abort(
      message = c(
        "x" = glue::glue("Geslacht ({geslacht}) niet geldig."),
        "i" = glue::glue("Geslacht moet een van de volgende zijn: {opties}.")
      )
    )
  }

  if (!is.na(leeftijd_jaar) & !is.numeric(leeftijd_jaar)) {
    rlang::abort(
      message = "Leeftijd is niet numeriek. Diersoort kan niet bepaald worden."
    )
  }
  if(!is.na(datum_import)) {
    return(overige)
  }
  if (is.na(leeftijd_jaar) | is.na(geslacht)) {
    if (!is.na(datum_afkalven)) {
      return(koe)
    }
    return(overige)
  }

  if ((leeftijd_jaar * 12) < max_maand_jongste_kalf) {
    return(kalf_jongste)
  }

  if ((leeftijd_jaar * 12) < max_maand_oudste_kalf) {
    return(kalf_oudste)
  }

  if (geslacht == "M") {
    return(stier)
  }

  if (!is.na(datum_afkalven)) {
    return(koe)
  } else {
    return(vaars)
  }
}

#' Bepaal het type slachting
#'
#' @param df data.frame, vereiste kolommen zijn: dier_op_locatie_id, dier_id,
#'   locatie_id, levens_nr, bvg_type, verblijfsduur_dagen, leeftijd_jaren,
#'   diersoort, locatie_geschiedenis, is_locatie_biologisch.
#'
#' @param geldig_locatie_types character vector, de locatietypen (bvg_type) waarmee
#'   rekening wordt gehouden bij het bepalen van het slachtingtype.
#'
#' @return data.frame met kolommen: dier_op_locatie_id, diersoort, is_biologisch
#' @export
rvo_slachting_type_bepalen <- function(
    df,
    geldig_locatie_types = c("VH", "SP")) {
  verwacht_kolommen <- c(
    "dier_op_locatie_id",
    "dier_id",
    "locatie_id",
    "levens_nr",
    "bvg_type",
    "verblijfsduur_dagen",
    "leeftijd_jaren",
    "diersoort",
    "locatie_geschiedenis",
    "is_locatie_biologisch"
  )
  controleer_kolommen(
    verwacht_kolommen = verwacht_kolommen,
    kolommen_in_df = colnames(df),
    naam_van_df = "df"
  )
  df |>
    # Check of dier alleen op biologische veehouderijen (type = VH) heeft gestaan
    # geldt niet voor verzamelplaatsen (type = VP) deze kunnen genegeerd worden.
    dplyr:::filter(bvg_type %in% geldig_locatie_types) |>
    # We maken nu een ID per dier dat alleen verandert als een dier van een
    # biologische naar een niet-biologische locatie gaat of vice versa. Daarvoor
    # gebruiken we `consecutive_id`.
    dplyr::arrange(
      .by = dier_id,
      locatie_geschiedenis
    ) |>
    dplyr::mutate(
      .by = dier_id,
      bio_id = dplyr::consecutive_id(is_locatie_biologisch)
    ) |>
    # Met deze ID kunnen we de verblijfsduur in opeenvolgende bio/niet-bio
    # locaties berekenen.
    dplyr::mutate(
      .by = c(dier_id, bio_id),
      verblijfsduur_in_type = sum(verblijfsduur_dagen / 365)
    ) |>
    # Vervolgens passen we de regels toe om te bepalen of een slachting
    # biologisch was
       dplyr::mutate(
      .by = dier_id,
      # is levens_nr leeg?
      is_niet_leeg = !is.na(levens_nr),
      # is dier niet onbekend?
      is_niet_onbekend = diersoort != "Onbekend",
      # is slachthuis bio?
      slachthuis_is_bio =  locatie_geschiedenis == 0 & is_locatie_biologisch,
      # kalf en alle locaties zijn bio dus is bio
      is_geschiedenis_biologish =  leeftijd_jaren < 1 & all(is_locatie_biologisch) | 
        # slachthuis is bio en langer dan een jaar in bio locaties?
        locatie_geschiedenis == 0 & is_locatie_biologisch == TRUE &  verblijfsduur_in_type > 1,
      is_biologisch = is_niet_onbekend & is_niet_leeg & slachthuis_is_bio & is_geschiedenis_biologish
    ) |> 
    # we willen alleen slachthuizen in de tbl_slachtingen_kv3 tabel
    dplyr::filter(locatie_geschiedenis == 0) |>
    dplyr::select(
      dier_op_locatie_id,
      diersoort,
      is_biologisch
    )
}


#' Opslaan tabel in database
#' 
#' Deze functie is overgenomen (en licht aangepast) van SBM_Zorg FRIBS repository
#'  het was in bestand FRIBS/src/algemeen/XX_hulp_database.R
#'
#' Informatie uit de data.frame wordt toegevoegd aan de betreffende tabel. Voor
#' performance-redenen wordt de data eerst naar een csv-bestand weggeschreven en
#' wordt dat bestand met `bcp.exe` in de database gezet (via een tijdelijke tabel)
#'
#' @param server Naam van de server
#' @param database Naam van de database op de server
#' @param schema Naam van het schema waarin de tabel staat
#' @param tabel Naam van de tabel (zonder schema)
#' @param data data.frame met de data die opgeslagen moet worden
#' @param log_map pad naar de logmap waar de error bestand wordt opgeslagd
#' @param skip_eerste_col TRUE/FALSE, moet eerste kolom worden overgeslagen (als
#' eerste kolom in de tabel een primary key is)
#'
#' @return Functie stopt met een foutmelding als er iets fout gaat
#' @export

db_opslaan_bcp <- function(server, database, schema, tabel, data, 
                           log_map, skip_eerste_col = TRUE) {
  
  con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", 
                        Server = server, Database = database)
  # aanmaken tijdelijke tabel met juiste kolommen
  kolommen <- dplyr::tbl(con, dbplyr::in_schema(schema, tabel)) |> 
    colnames()
  if (skip_eerste_col) {
    kolommen <- kolommen[-1]
  }
  # SMES: eigenlijk wil je deze ook vervangen door db_execute_fribs,
  #       maar de kolommenstring is te lang voor glue (max 128 chars!)
  kolommenstring <- paste(kolommen, sep = "", collapse = ", ")
  tijdelijke_tabel <- gsub("-", "_", paste0("##fribs_", uuid::UUIDgenerate()))
  rs <- DBI::dbSendStatement(con, paste0(
    "select top 0 ", kolommenstring,
    " into ", tijdelijke_tabel,
    " from ", schema, ".", tabel
  ))
  DBI::dbClearResult(rs)
  
  # data exporteren naar csv
  bestandsnaam <- tempfile()
  posities <- match(tolower(kolommen), tolower(names(data)))
  stopifnot(all(!is.na(posities)))
  df <- data[, posities]
  stopifnot(all(!is.na(posities)))
  # bit-variabelen vertalen naar 0-1
  for (varname in names(df)) {
    if (is.logical(df[[varname]])) {
      df[, varname] <- as.integer(df[[varname]])
    }
  }
  
  # encoding utf-8
  # veldscheider \x1f in write.table en 0x1f in bcp
  # regelscheider -r 0x1e bij bcp en eol="\x1e"
  write.table(df, bestandsnaam, quote = FALSE, sep = "\x1f", eol = "\x1e", 
              row.names = FALSE, col.names = FALSE, na = "", 
              fileEncoding = "UTF-8")
  
  # data met bcp in tijdelijke tabel zetten
  rc <- system(paste0(
    "bcp ", schema, ".", tijdelijke_tabel, " in ", bestandsnaam, " -S ", server,
    " -d tempdb -T -c -t 0x1f -r 0x1e -C 65001 -e ",
    file.path(log_map, "database_bulk_import_fouten.log")
  ))
  stopifnot("aanroep bcp is mislukt." = rc == 0)
  
  # check tijdelijk tabel content met df content.
  tijdelijke_tabelsql <- DBI::Id(schema = schema, table = tijdelijke_tabel)
  nrowtemp <- glue::glue_sql(
    "
        select count(*) from {`tijdelijke_tabelsql`}
        ",
    values = list(tijdelijke_tabelsql = tijdelijke_tabelsql),
    .con = con
  ) |> DBI::dbGetQuery(conn = con, statement = _)
  
  stopifnot("tijdelijke tabel en input ongelijk" = nrowtemp[, 1] == nrow(df))
  
  # data uit tijdelijke tabel overzetten naar echte tabel
  # smes: ook hier wil je eigenlijk db_execute_fribs gebruiken,
  #       maar loop je er tegen aan dat kolommenstring te lang is.
  rs <- DBI::dbSendStatement(con, paste0(
    "insert into ", schema, ".", tabel,
    " (", kolommenstring, ") select ", kolommenstring, " from ", tijdelijke_tabel
  ))
  DBI::dbClearResult(rs)
  
  # tijdelijke tabel opruimen
  glue::glue_sql("drop table {`tijdelijke_tabelsql`}",
                   values = list(tijdelijke_tabelsql = tijdelijke_tabelsql),
                   .con = con) |> 
    DBI::dbGetQuery(conn = con, statement = _)
  file.remove(bestandsnaam)
  DBI::dbDisconnect(con)
}
