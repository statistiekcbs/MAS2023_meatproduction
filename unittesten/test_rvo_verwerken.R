library(testthat)
root_map <- here::here()
src_map <- file.path(root_map, "src")
test_map <- file.path(root_map, "unittesten")
testdata_map <- file.path(test_map, "testdata")
# maak de map relatief voor quarto, want het ondersteunt geen netwerkmappen.
# wanneer we de testen draaien vanuit voer_testen_uit.R, zitten we in de 
# unittesten map. Vandaar ".."
sjablonen_map <- file.path("..", "src", "sjablonen")

source(file.path(src_map, "rvo_verwerken.R"))
source(file.path(src_map, "rvo_controles_en_correcties.R"))
source(file.path(src_map, "rvo_hulpfuncties.R"))
source(file.path(src_map, "synthetische_data.R"))
source(file.path(src_map, "inlezen.R"))

test_that("normalisatie van rvo input retourneert drie data.frames", {
  pad_configbestand <- file.path(testdata_map, "voorbeeld_config.ini")
  App <- yaml::yaml.load_file(
    input = pad_configbestand,
    eval.expr = TRUE
  )

  set.seed(6)

  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0.5,
    straatnamen = maak_straatnamen(),
    n_regels = 1,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # slachtingen bestand
  pad_slachtingen <- withr::local_tempfile(fileext = ".csv")

  readr::write_csv(
    x = synthetische_dataset$slachtingen,
    file = pad_slachtingen
  )

  df_slachtingen_ruw <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  )

  # slachthuizen bestand
  pad_slachthuizen <- withr::local_tempfile(fileext = ".csv")

  readr::write_csv(
    x = synthetische_dataset$slachthuizen,
    file = pad_slachthuizen
  )

  df_slachthuizen_ruw <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )

  output <- rvo_dieren_en_locaties_bestanden_normaliseren(
    df_slachtingen = df_slachtingen_ruw,
    df_slachthuizen = df_slachthuizen_ruw
  )

  expect_named(output, c("dieren", "dieren_op_locaties", "locaties"))
  expect_s3_class(output$dieren, "data.frame")
  expect_s3_class(output$dieren_op_locaties, "data.frame")
  expect_s3_class(output$locaties, "data.frame")
})

test_that("normalisatie van rvo input retourneert drie data.frames met de juiste kolommen", {
  
  pad_configbestand <- file.path(testdata_map, "voorbeeld_config.ini")
  
  App <- yaml::yaml.load_file(
    input = pad_configbestand,
    eval.expr = TRUE
  )
  
  set.seed(6)

  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0.5,
    straatnamen = maak_straatnamen(),
    n_regels = 1,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # slachtingen bestand
  pad_slachtingen <- withr::local_tempfile(fileext = ".csv")

  readr::write_csv(
    x = synthetische_dataset$slachtingen,
    file = pad_slachtingen
  )

  df_slachtingen_ruw <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  )

  # slachthuizen bestand
  pad_slachthuizen <- withr::local_tempfile(fileext = ".csv")

  readr::write_csv(
    x = synthetische_dataset$slachthuizen,
    file = pad_slachthuizen
  )

  df_slachthuizen_ruw <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )

  output <- rvo_dieren_en_locaties_bestanden_normaliseren(
    df_slachtingen = df_slachtingen_ruw,
    df_slachthuizen = df_slachthuizen_ruw
  )


  verwachte_kolomnamen_dieren <- c(
    "levens_nr",
    "datum_geboorte",
    "datum_slacht",
    "datum_eerste_afkalven",
    "datum_import",
    "geslacht",
    "eld_code",
    "landcode_herkomst",
    "landcode_oorsprong",
    "hkr_code",
    "hkr_oms",
    "code_reden_einde",
    "einde_oms",
    "leeftijd_jaren",
    "diersoort"
  )
  expect_named(
    output$dieren,
    verwachte_kolomnamen_dieren,
    ignore.order = TRUE
  )

  verwachte_kolomnamen_dieren_op_locaties <- c(
    "levens_nr",
    "eld_code",
    "locatie_geschiedenis",
    "ubn",
    "bvg_type",
    "datum_ingang",
    "datum_einde",
    "verblijfsduur_dagen"
  )
  expect_named(
    output$dieren_op_locaties,
    verwachte_kolomnamen_dieren_op_locaties,
    ignore.order = TRUE
  )

  verwachte_kolomnamen_locaties <- c(
    "ubn",
    "bvg_type",
    "bvg_type_oms",
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
    "type_rel",
    "kvk_nr",
    "naam",
    "naam_voorletters",
    "naam_voorvoegsels",
    "naam_toevoeging",
    "aantal"
  )
  expect_named(
    output$locaties,
    verwachte_kolomnamen_locaties,
    ignore.order = TRUE
  )
})

test_that("normalisatie van rvo input retourneert drie data.frames met de juiste rijen", {
  pad_configbestand <- file.path(testdata_map, "voorbeeld_config.ini")
  App <- yaml::yaml.load_file(
    input = pad_configbestand,
    eval.expr = TRUE
  )

  set.seed(6)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 50,
    # locaties = locaties,
    biologische_fractie = 0.5,
    straatnamen = maak_straatnamen(),
    n_regels = 1,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # slachtingen bestand
  pad_slachtingen <- withr::local_tempfile(fileext = ".csv")

  readr::write_csv(
    x = synthetische_dataset$slachtingen,
    file = pad_slachtingen
  )

  df_slachtingen_ruw <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  )

  # slachthuizen bestand
  pad_slachthuizen <- withr::local_tempfile(fileext = ".csv")

  readr::write_csv(
    x = synthetische_dataset$slachthuizen,
    file = pad_slachthuizen
  )

  df_slachthuizen_ruw <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )

  output <- rvo_dieren_en_locaties_bestanden_normaliseren(
    df_slachtingen = df_slachtingen_ruw,
    df_slachthuizen = df_slachthuizen_ruw
  )

  expect_equal(
    nrow(output$dieren),
    nrow(df_slachtingen_ruw)
  )

  df_verwacht_dieren_op_locaties <- df_slachtingen_ruw |>
    dplyr::select(
      tidyselect::starts_with("bvg_type")
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "naam",
      values_to = "waarde"
    ) |>
    dplyr::filter(!is.na(waarde))

  expect_equal(
    nrow(output$dieren_op_locaties),
    nrow(df_verwacht_dieren_op_locaties)
  )

  n_vh_locaties <- df_verwacht_dieren_op_locaties |>
    dplyr::filter(waarde == "VH") |>
    nrow()

  expect_equal(
    nrow(output$locaties),
    nrow(df_slachthuizen_ruw) + n_vh_locaties
  )
})

test_that("rvo_dieren_en_locaties_invullen functie: KV2 invullen met RVO data", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
  # maak een verbinding met de ont database
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  
  DBI::dbExecute(con, qry)
  # synth dataset aanmaken
  set.seed(10)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0,
    straatnamen = maak_straatnamen(),
    n_regels = 2,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # we slaan de dataset op als csv en gebruiken onze functies om de
  # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
  # zoals in SPEK zal gebeuren
  pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachtingen,
    file = pad_rvo_slachtingen
  )

  df_slachtingen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  )
  pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachthuizen,
    file = pad_rvo_slachthuizen
  )

  df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )

  # Verwachtingen obv de input data
  verwachte_n_dieren <- nrow(df_slachtingen)

  df_verwacht_dieren_op_locaties <- df_slachtingen |>
    dplyr::select(tidyselect::starts_with(c("ubn", "bvg_type"))) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c("ubn", "bvg_type")),
      names_to = c(".value", "locatie_geschiedenis"),
      names_pattern = ("(.+)_([0-9])"),
      names_transform = list(
        ubn = as.character,
        bvg_type = as.character,
        locatie_geschiedenis = as.integer
      )
    ) |>
    dplyr::filter(!is.na(ubn)) |>
    dplyr::arrange(locatie_geschiedenis) |>
    dplyr::select(ubn, bvg_type, locatie_geschiedenis)

  verwachte_verblijfsduur_eerste_vh <- lubridate::time_length(
    lubridate::interval(
      start = df_slachtingen$datum_ingang_1,
      end = df_slachtingen$datum_einde_1
    ),
    unit = "days"
  )

  df_verwacht_locaties <- df_verwacht_dieren_op_locaties |>
    dplyr::select(-locatie_geschiedenis) |>
    dplyr::bind_rows(
      df_slachthuizen |> dplyr::select(ubn, bvg_type)
    ) |>
    dplyr::distinct()

  data.frame(
    bestandsnaam = "test",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  
  # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke database te
  # vullen
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # kv2 tabel uit de database ophalen
  df_db_tbl_dieren <- dplyr::tbl(
      con, 
      dbplyr::in_schema(test_schema, "tbl_dieren_kv2_kv3")) |>
    dplyr::collect()

  df_db_dieren_op_locaties <- dplyr::tbl(
      con, 
      dbplyr::in_schema(test_schema, "tbl_dieren_op_locaties_kv2_kv3")) |>
    dplyr::collect()

  df_db_tbl_locaties <- dplyr::tbl(
      con, 
      dbplyr::in_schema(test_schema, "tbl_locaties_kv2_kv3")) |>
    dplyr::collect()

  # testen
  expect_equal(nrow(df_db_tbl_dieren), verwachte_n_dieren)
  expect_equal(df_db_tbl_dieren$levens_nr, df_slachtingen$levens_nr)

  expect_equal(nrow(df_db_dieren_op_locaties), nrow(df_verwacht_dieren_op_locaties))

  df_db_dieren_op_locaties |>
    dplyr::filter(locatie_geschiedenis == 1) |>
    dplyr::pull(verblijfsduur_dagen) |>
    expect_equal(verwachte_verblijfsduur_eerste_vh)

  expect_equal(nrow(df_db_tbl_locaties), nrow(df_verwacht_locaties))
  expect_equal(sort(df_db_tbl_locaties$ubn), sort(df_verwacht_locaties$ubn))


  DBI::dbDisconnect(con)
})

test_that("rvo_dieren_en_locaties_invullen functie: KV2 invullen met RVO data,
          twee slachtingen met dezelfde levens_nr maar met verschillende eld_code", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
   con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  
  DBI::dbExecute(con, qry)
  # synth dataset aanmaken
  set.seed(10)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0,
    straatnamen = maak_straatnamen(),
    n_regels = 2,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  
  # we slaan de dataset op als csv en gebruiken onze functies om de
  # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
  # zoals in SPEK zal gebeuren
  pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachtingen,
                   file = pad_rvo_slachtingen
  )
  
  df_slachtingen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  ) |>  
    # hier simuleren we een levering waarbij er twee slachtingen zijn met
    # dezelfde levens_nr maar verschillende eld_code
    dplyr::mutate(
      levens_nr = ifelse(dplyr::row_number() == 1, 
                         levens_nr,
                         dplyr::lag(levens_nr)),
      eld_code = ifelse(dplyr::row_number() == 1, 
                        eld_code,
                        "NR")
  )
  
  pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachthuizen,
                   file = pad_rvo_slachthuizen
  )
  
  df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )
  
  # Verwachtingen obv de input data
  verwachte_n_dieren <- nrow(df_slachtingen)
  
  verwachte_dieren_op_locaties <- df_slachtingen |>
    dplyr::select(tidyselect::starts_with(c("ubn", "bvg_type"))) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c("ubn", "bvg_type")),
      names_to = c(".value", "locatie_geschiedenis"),
      names_pattern = ("(.+)_([0-9])"),
      names_transform = list(
        ubn = as.character,
        bvg_type = as.character,
        locatie_geschiedenis = as.integer
      )
    ) |>
    dplyr::filter(!is.na(ubn)) |>
    dplyr::arrange(locatie_geschiedenis) |>
    dplyr::select(ubn, bvg_type, locatie_geschiedenis) |> 
    nrow()
  
  data.frame(
    bestandsnaam = "test",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  
 
  # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke database te
  # vullen
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  # kv2 tabel uit de database ophalen
  df_db_tbl_dieren <- dplyr::tbl(con, 
                                 dbplyr::in_schema(test_schema, 
                                                   "tbl_dieren_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_dieren_op_locaties <- dplyr::tbl(con, 
                                         dbplyr::in_schema(test_schema, 
                                                           "tbl_dieren_op_locaties_kv2_kv3")) |>
    dplyr::collect()
  
  # testen
  expect_equal(nrow(df_db_tbl_dieren), verwachte_n_dieren)
  expect_equal(nrow(df_db_dieren_op_locaties), verwachte_dieren_op_locaties)
  
  DBI::dbDisconnect(con)
})

test_that("rvo_dieren_en_locaties_invullen functie: 
          De gebruiker probeert KV2 twee keer te vullen met dezelfde gegevens
          en de database wordt dan niet bijgewerkt.", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  DBI::dbExecute(con, qry)
  
  # synth dataset aanmaken
  set.seed(10)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0,
    straatnamen = maak_straatnamen(),
    n_regels = 1,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # we slaan de dataset op als csv en gebruiken onze functies om de
  # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
  # zoals in SPEK zal gebeuren
  pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachtingen,
    file = pad_rvo_slachtingen
  )

  df_slachtingen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  )
  pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachthuizen,
    file = pad_rvo_slachthuizen
  )

  df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )

  # Verwachtingen opv de input data
  verwachte_n_dieren <- nrow(df_slachtingen)

  df_verwacht_dieren_op_locaties <- df_slachtingen |>
    dplyr::select(tidyselect::starts_with(c("ubn", "bvg_type"))) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c("ubn", "bvg_type")),
      names_to = c(".value", "locatie_geschiedenis"),
      names_pattern = ("(.+)_([0-9])"),
      names_transform = list(
        ubn = as.character,
        bvg_type = as.character,
        locatie_geschiedenis = as.integer
      )
    ) |>
    dplyr::filter(!is.na(ubn)) |>
    dplyr::arrange(locatie_geschiedenis) |>
    dplyr::select(ubn, bvg_type, locatie_geschiedenis)

  verwachte_verblijfsduur_eerste_vh <- lubridate::time_length(
    lubridate::interval(
      start = df_slachtingen$datum_ingang_1,
      end = df_slachtingen$datum_einde_1
    ),
    unit = "days"
  )

  df_verwacht_locaties <- df_verwacht_dieren_op_locaties |>
    dplyr::select(-locatie_geschiedenis) |>
    dplyr::bind_rows(
      df_slachthuizen |> dplyr::select(ubn, bvg_type)
    ) |>
    dplyr::distinct()
  
  data.frame(
    bestandsnaam = "test",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke database te
  # vullen
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # kv2 tabel uit de database ophalen
  df_db_tbl_dieren <- dplyr::tbl(con, 
                                 dbplyr::in_schema(test_schema, 
                                                   "tbl_dieren_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_dieren_op_locaties <- dplyr::tbl(con, 
                                         dbplyr::in_schema(test_schema, 
                                                           "tbl_dieren_op_locaties_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_tbl_locaties <- dplyr::tbl(con, 
                                   dbplyr::in_schema(test_schema, 
                                                     "tbl_locaties_kv2_kv3")) |>
    dplyr::collect()

  # testen
  expect_equal(nrow(df_db_tbl_dieren), verwachte_n_dieren)
  expect_equal(df_db_tbl_dieren$levens_nr, df_slachtingen$levens_nr)

  expect_equal(nrow(df_db_dieren_op_locaties), nrow(df_verwacht_dieren_op_locaties))

  df_db_dieren_op_locaties |>
    dplyr::filter(locatie_geschiedenis == 1) |>
    dplyr::pull(verblijfsduur_dagen) |>
    expect_equal(verwachte_verblijfsduur_eerste_vh)

  expect_equal(nrow(df_db_tbl_locaties), nrow(df_verwacht_locaties))
  expect_equal(sort(df_db_tbl_locaties$ubn), sort(df_verwacht_locaties$ubn))

  # Tweede keer met deselfde data en levering id
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )


  df_db_tbl_dieren <- dplyr::tbl(con, 
                                 dbplyr::in_schema(test_schema, 
                                                   "tbl_dieren_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_dieren_op_locaties <- dplyr::tbl(con, 
                                         dbplyr::in_schema(test_schema, 
                                                           "tbl_dieren_op_locaties_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_tbl_locaties <- dplyr::tbl(con, 
                                   dbplyr::in_schema(test_schema, 
                                                     "tbl_locaties_kv2_kv3")) |>
    dplyr::collect()

  # testen
  expect_equal(nrow(df_db_tbl_dieren), verwachte_n_dieren)
  expect_equal(df_db_tbl_dieren$levens_nr, df_slachtingen$levens_nr)

  expect_equal(nrow(df_db_dieren_op_locaties), nrow(df_verwacht_dieren_op_locaties))

  df_db_dieren_op_locaties |>
    dplyr::filter(locatie_geschiedenis == 1) |>
    dplyr::pull(verblijfsduur_dagen) |>
    expect_equal(verwachte_verblijfsduur_eerste_vh)

  expect_equal(nrow(df_db_tbl_locaties), nrow(df_verwacht_locaties))
  expect_equal(sort(df_db_tbl_locaties$ubn), sort(df_verwacht_locaties$ubn))

  DBI::dbDisconnect(con)
})

test_that("rvo_dieren_en_locaties_invullen functie:
          Als een slachting aanwezig is in een tweede levering,
          maar de informatie is nog steeds hetzelfde,
          dan wordt de database niet bijgewerkt.", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  DBI::dbExecute(con, qry)
  # synth dataset aanmaken
  set.seed(10)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0,
    straatnamen = maak_straatnamen(),
    n_regels = 1,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # we slaan de dataset op als csv en gebruiken onze functies om de
  # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
  # zoals in SPEK zal gebeuren
  pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachtingen,
    file = pad_rvo_slachtingen
  )

  df_slachtingen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  )
  pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachthuizen,
    file = pad_rvo_slachthuizen
  )

  df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )

  # Verwachtingen opv de input data
  verwachte_n_dieren <- nrow(df_slachtingen)

  df_verwacht_dieren_op_locaties <- df_slachtingen |>
    dplyr::select(tidyselect::starts_with(c("ubn", "bvg_type"))) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c("ubn", "bvg_type")),
      names_to = c(".value", "locatie_geschiedenis"),
      names_pattern = ("(.+)_([0-9])"),
      names_transform = list(
        ubn = as.character,
        bvg_type = as.character,
        locatie_geschiedenis = as.integer
      )
    ) |>
    dplyr::filter(!is.na(ubn)) |>
    dplyr::arrange(locatie_geschiedenis) |>
    dplyr::select(ubn, bvg_type, locatie_geschiedenis)

  verwachte_verblijfsduur_eerste_vh <- lubridate::time_length(
    lubridate::interval(
      start = df_slachtingen$datum_ingang_1,
      end = df_slachtingen$datum_einde_1
    ),
    unit = "days"
  )

  df_verwacht_locaties <- df_verwacht_dieren_op_locaties |>
    dplyr::select(-locatie_geschiedenis) |>
    dplyr::bind_rows(
      df_slachthuizen |> dplyr::select(ubn, bvg_type)
    ) |>
    dplyr::distinct()

  
  data.frame(
    bestandsnaam = "test",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke
  # database te vullen
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  df_db_tbl_dieren <- dplyr::tbl(con, 
                                 dbplyr::in_schema(test_schema, 
                                                   "tbl_dieren_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_dieren_op_locaties <- dplyr::tbl(con, 
                                         dbplyr::in_schema(test_schema, 
                                                           "tbl_dieren_op_locaties_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_tbl_locaties <- dplyr::tbl(con, 
                                   dbplyr::in_schema(test_schema, 
                                                     "tbl_locaties_kv2_kv3")) |>
    dplyr::collect()

  # testen
  expect_equal(nrow(df_db_tbl_dieren), verwachte_n_dieren)
  expect_equal(df_db_tbl_dieren$levens_nr, df_slachtingen$levens_nr)

  expect_equal(
    nrow(df_db_dieren_op_locaties),
    nrow(df_verwacht_dieren_op_locaties)
  )

  df_db_dieren_op_locaties |>
    dplyr::filter(locatie_geschiedenis == 1) |>
    dplyr::pull(verblijfsduur_dagen) |>
    expect_equal(verwachte_verblijfsduur_eerste_vh)

  expect_equal(
    nrow(df_db_tbl_locaties),
    nrow(df_verwacht_locaties)
  )
  expect_equal(
    sort(df_db_tbl_locaties$ubn),
    sort(df_verwacht_locaties$ubn)
  )

  data.frame(
    bestandsnaam = "test2",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  # Tweede levering, hier gebruiken we hetzelfde data.frame en
  # veranderen we alleen de leverings-id
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen,
    lev_id = 2,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  df_db_tbl_dieren <- dplyr::tbl(con, 
                                 dbplyr::in_schema(test_schema, 
                                                   "tbl_dieren_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_dieren_op_locaties <- dplyr::tbl(con, 
                                         dbplyr::in_schema(test_schema, 
                                                           "tbl_dieren_op_locaties_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_tbl_locaties <- dplyr::tbl(con, 
                                   dbplyr::in_schema(test_schema, 
                                                     "tbl_locaties_kv2_kv3")) |>
    dplyr::collect()

  # testen
  # we verwachten dat de database in dezelfde staat is als na
  # de eerste levering
  expect_equal(nrow(df_db_tbl_dieren), verwachte_n_dieren)
  expect_equal(df_db_tbl_dieren$levens_nr, df_slachtingen$levens_nr)

  expect_equal(
    nrow(df_db_dieren_op_locaties),
    nrow(df_verwacht_dieren_op_locaties)
  )

  df_db_dieren_op_locaties |>
    dplyr::filter(locatie_geschiedenis == 1) |>
    dplyr::pull(verblijfsduur_dagen) |>
    expect_equal(verwachte_verblijfsduur_eerste_vh)

  expect_equal(
    nrow(df_db_tbl_locaties),
    nrow(df_verwacht_locaties)
  )
  expect_equal(
    sort(df_db_tbl_locaties$ubn),
    sort(df_verwacht_locaties$ubn)
  )

  DBI::dbDisconnect(con)
})

test_that("rvo_dieren_en_locaties_invullen functie:
          Als een slachting is bijgewerkt in een tweede levering,
          wordt de overeenkomstige tabel in de database bijgewerkt.
          In dit geval is de tabel tbl_dieren_op_locaties_kv2_kv3.", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  DBI::dbExecute(con, qry)
            

  # synth dataset aanmaken
  set.seed(10)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0,
    straatnamen = maak_straatnamen(),
    n_regels = 1,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # we slaan de dataset op als csv en gebruiken onze functies om de
  # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
  # zoals in SPEK zal gebeuren
  pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachtingen,
    file = pad_rvo_slachtingen
  )

  df_slachtingen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  )
  pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachthuizen,
    file = pad_rvo_slachthuizen
  )

  df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )

  # Verwachtingen opv de input data
  verwachte_n_dieren <- nrow(df_slachtingen)

  df_verwacht_dieren_op_locaties <- df_slachtingen |>
    dplyr::select(tidyselect::starts_with(c("ubn", "bvg_type"))) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c("ubn", "bvg_type")),
      names_to = c(".value", "locatie_geschiedenis"),
      names_pattern = ("(.+)_([0-9])"),
      names_transform = list(
        ubn = as.character,
        bvg_type = as.character,
        locatie_geschiedenis = as.integer
      )
    ) |>
    dplyr::filter(!is.na(ubn)) |>
    dplyr::arrange(locatie_geschiedenis) |>
    dplyr::select(ubn, bvg_type, locatie_geschiedenis)

  verwachte_verblijfsduur_eerste_vh <- lubridate::time_length(
    lubridate::interval(
      start = df_slachtingen$datum_ingang_1,
      end = df_slachtingen$datum_einde_1
    ),
    unit = "days"
  )

  df_verwacht_locaties <- df_verwacht_dieren_op_locaties |>
    dplyr::select(-locatie_geschiedenis) |>
    dplyr::bind_rows(
      df_slachthuizen |> dplyr::select(ubn, bvg_type)
    ) |>
    dplyr::distinct()

  data.frame(
    bestandsnaam = "test",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke database te
  # vullen
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # Tweede levering met een aangepast slachting
  # De data van de locaties waar het dier was zijn gewijzigd, dus de
  # data in de tbl_dieren_op_locaties_kv2_kv3 moeten worden bijgewerkt.
  df_slachtingen_aangepast <- df_slachtingen |> dplyr::mutate(
    datum_ingang_0 = Sys.Date(),
    datum_einde_0 = Sys.Date(),
    datum_einde_1 = Sys.Date()
  )

  data.frame(
    bestandsnaam = "test_2",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen_aangepast,
    df_slachthuizen = df_slachthuizen,
    lev_id = 2,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # kv2 tabel uit de database ophalen
  df_db_tbl_dieren <- dplyr::tbl(con, 
                                 dbplyr::in_schema(test_schema, 
                                                   "tbl_dieren_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_dieren_op_locaties <- dplyr::tbl(con, 
                                         dbplyr::in_schema(test_schema, 
                                                           "tbl_dieren_op_locaties_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_tbl_locaties <- dplyr::tbl(con, 
                                   dbplyr::in_schema(test_schema, 
                                                     "tbl_locaties_kv2_kv3")) |>
    dplyr::collect()

  # testen
  expect_equal(nrow(df_db_tbl_dieren), verwachte_n_dieren)
  expect_equal(df_db_tbl_dieren$levens_nr, df_slachtingen$levens_nr)
  # De data van twee locaties verschillen in deze levering, dus de
  # tabel moet twee extra rijen hebben
  expect_equal(
    nrow(df_db_dieren_op_locaties),
    nrow(df_verwacht_dieren_op_locaties) + 2
  )

  expect_equal(nrow(df_db_tbl_locaties), nrow(df_verwacht_locaties))
  expect_equal(sort(df_db_tbl_locaties$ubn), sort(df_verwacht_locaties$ubn))

  DBI::dbDisconnect(con)
})

test_that("rvo_dieren_en_locaties_invullen functie:
          Als een slachting is bijgewerkt in een tweede levering,
          wordt de overeenkomstige tabel in de database bijgewerkt.
          In dit geval is de tabel tbl_dieren_kv2_kv3", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  DBI::dbExecute(con, qry)

  # synth dataset aanmaken
  set.seed(10)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0,
    straatnamen = maak_straatnamen(),
    n_regels = 1,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # we slaan de dataset op als csv en gebruiken onze functies om de
  # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
  # zoals in SPEK zal gebeuren
  pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachtingen,
    file = pad_rvo_slachtingen
  )

  df_slachtingen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  )
  pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachthuizen,
    file = pad_rvo_slachthuizen
  )

  df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )

  # Verwachtingen opv de input data
  verwachte_n_dieren <- nrow(df_slachtingen)

  df_verwacht_dieren_op_locaties <- df_slachtingen |>
    dplyr::select(tidyselect::starts_with(c("ubn", "bvg_type"))) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c("ubn", "bvg_type")),
      names_to = c(".value", "locatie_geschiedenis"),
      names_pattern = ("(.+)_([0-9])"),
      names_transform = list(
        ubn = as.character,
        bvg_type = as.character,
        locatie_geschiedenis = as.integer
      )
    ) |>
    dplyr::filter(!is.na(ubn)) |>
    dplyr::arrange(locatie_geschiedenis) |>
    dplyr::select(ubn, bvg_type, locatie_geschiedenis)

  verwachte_verblijfsduur_eerste_vh <- lubridate::time_length(
    lubridate::interval(
      start = df_slachtingen$datum_ingang_1,
      end = df_slachtingen$datum_einde_1
    ),
    unit = "days"
  )

  df_verwacht_locaties <- df_verwacht_dieren_op_locaties |>
    dplyr::select(-locatie_geschiedenis) |>
    dplyr::bind_rows(
      df_slachthuizen |> dplyr::select(ubn, bvg_type)
    ) |>
    dplyr::distinct()

  data.frame(
    bestandsnaam = "test",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke database te
  # vullen
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # De gegevens (geboorte_datum) van het dier was gewijzigd, dus
  # de tabellen tbl_dieren_kv2_kv3 en tbl_dieren_op_locaties_kv2_kv3
  # (zie hieronder) moeten worden bijgewerkt.
  data.frame(
    bestandsnaam = "test_2",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  
  df_slachtingen_aangepast <- df_slachtingen |> dplyr::mutate(
    datum_geboorte = datum_geboorte + lubridate::days(1)
  )
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen_aangepast,
    df_slachthuizen = df_slachthuizen,
    lev_id = 2,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # kv2 tabel uit de database ophalen
  df_db_tbl_dieren <- dplyr::tbl(con, 
                                 dbplyr::in_schema(test_schema, 
                                                   "tbl_dieren_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_dieren_op_locaties <- dplyr::tbl(con, 
                                         dbplyr::in_schema(test_schema, 
                                                           "tbl_dieren_op_locaties_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_tbl_locaties <- dplyr::tbl(con, 
                                   dbplyr::in_schema(test_schema, 
                                                     "tbl_locaties_kv2_kv3")) |>
    dplyr::collect()

  # testen

  # Aangezien de informatie over het dier is bijgewerkt, wordt
  # een nieuwe rij toegevoegd
  expect_equal(nrow(df_db_tbl_dieren), verwachte_n_dieren + 1)
  # De rij die werd toegevoegd met de vorige bewerking moet een
  # eind_datum krijgen
  eind_datum_eerste_levering <- df_db_tbl_dieren |>
    dplyr::filter(invoer_datum == min(invoer_datum)) |>
    dplyr::pull(eind_datum)
  expect_true(!is.na(eind_datum_eerste_levering))

  # Aangezien de tbl_dieren_op_locaties_kv2_kv3-tabel verwijst naar de
  # tbl_dieren_kv2_kv3-tabel, moeten alle records daar worden
  # bijgewerkt en verwijzen naar de laatste versie van deze slachting
  # (die van de tweede levering)
  expect_equal(
    nrow(df_db_dieren_op_locaties),
    nrow(df_verwacht_dieren_op_locaties) * 2
  )

  eind_datum_eerste_levering <- df_db_dieren_op_locaties |>
    dplyr::filter(invoer_datum == min(invoer_datum)) |>
    dplyr::pull(eind_datum)
  expect_true(all(!is.na(eind_datum_eerste_levering)))

  expect_equal(nrow(df_db_tbl_locaties), nrow(df_verwacht_locaties))
  expect_equal(
    sort(df_db_tbl_locaties$ubn),
    sort(df_verwacht_locaties$ubn)
  )

  DBI::dbDisconnect(con)
})

test_that("rvo_dieren_en_locaties_invullen functie:
          Als een locatie is bijgewerkt in een tweede levering,
          wordt de overeenkomstige tabel in de database bijgewerkt.
          In dit geval is de tabellen tbl_locaties_kv2_kv3 en tbl_dieren_kv2_kv3", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  DBI::dbExecute(con, qry)

  # synth dataset aanmaken
  set.seed(10)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0,
    straatnamen = maak_straatnamen(),
    n_regels = 1,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # we slaan de dataset op als csv en gebruiken onze functies om de
  # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
  # zoals in SPEK zal gebeuren
  pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachtingen,
    file = pad_rvo_slachtingen
  )

  df_slachtingen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  )
  pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachthuizen,
    file = pad_rvo_slachthuizen
  )

  df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )

  # Verwachtingen opv de input data
  verwachte_n_dieren <- nrow(df_slachtingen)

  df_verwacht_dieren_op_locaties <- df_slachtingen |>
    dplyr::select(tidyselect::starts_with(c("ubn", "bvg_type"))) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c("ubn", "bvg_type")),
      names_to = c(".value", "locatie_geschiedenis"),
      names_pattern = ("(.+)_([0-9])"),
      names_transform = list(
        ubn = as.character,
        bvg_type = as.character,
        locatie_geschiedenis = as.integer
      )
    ) |>
    dplyr::filter(!is.na(ubn)) |>
    dplyr::arrange(locatie_geschiedenis) |>
    dplyr::select(ubn, bvg_type, locatie_geschiedenis)

  verwachte_verblijfsduur_eerste_vh <- lubridate::time_length(
    lubridate::interval(
      start = df_slachtingen$datum_ingang_1,
      end = df_slachtingen$datum_einde_1
    ),
    unit = "days"
  )

  df_verwacht_locaties <- df_verwacht_dieren_op_locaties |>
    dplyr::select(-locatie_geschiedenis) |>
    dplyr::bind_rows(
      df_slachthuizen |> dplyr::select(ubn, bvg_type)
    ) |>
    dplyr::distinct()
  
  data.frame(
    bestandsnaam = "test",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  
  # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke
  # database te vullen
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # De gegevens (naam_voorletters) van het dier was gewijzigd, dus
  # de tabelen tbl_locaties_kv2_kv3 en tbl_dieren_op_locaties_kv2_kv3
  # (zie hieronder) moeten worden bijgewerkt.
  data.frame(
    bestandsnaam = "test+2",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  
  df_slachthuizen_aangepast <- df_slachthuizen |>
    dplyr::mutate(
      naam_voorletters = sample(LETTERS, dplyr::n())
    )

  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen_aangepast,
    lev_id = 2,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  df_db_tbl_dieren <- dplyr::tbl(con, 
                                 dbplyr::in_schema(test_schema, 
                                                   "tbl_dieren_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_dieren_op_locaties <- dplyr::tbl(con, 
                                         dbplyr::in_schema(test_schema, 
                                                           "tbl_dieren_op_locaties_kv2_kv3")) |>
    dplyr::collect()
  
  df_db_tbl_locaties <- dplyr::tbl(con, 
                                   dbplyr::in_schema(test_schema, 
                                                     "tbl_locaties_kv2_kv3")) |>
    dplyr::collect()

  # Testen

  expect_equal(nrow(df_db_tbl_dieren), verwachte_n_dieren)

  # Aangezien de tbl_dieren_op_locaties_kv2_kv3-tabel verwijst naar de
  # tbl_locaties_kv2_kv3-tabel, moeten alle records daar worden
  # bijgewerkt en verwijzen naar de laatste versie van deze locatie
  # (die van de tweede levering)
  expect_equal(
    nrow(df_db_dieren_op_locaties),
    nrow(df_verwacht_dieren_op_locaties) + 1
  )

  eind_datum_eerste_levering <- df_db_dieren_op_locaties |>
    dplyr::filter(
      locatie_geschiedenis == 0,
      invoer_datum == min(invoer_datum)
    ) |>
    dplyr::pull(eind_datum)
  expect_true(!is.na(eind_datum_eerste_levering))

  # Aangezien de informatie over alle slachthuizen, worden nieuwe
  # rijen toegevoegd
  expect_equal(
    nrow(df_db_tbl_locaties),
    # een van de locaties hier is een slachthuis
    nrow(df_verwacht_dieren_op_locaties) - 1 +
      # alle slachthuizen zijn bijgewerkt en dus opnieuw
      # toegevoegd
      nrow(df_slachthuizen) * 2
  )
  # De rijen die werden toegevoegd met de vorige levering moeten een
  # eind_datum krijgen
  eind_datum_eerste_levering <- df_db_tbl_locaties |>
    dplyr::filter(
      invoer_datum == min(invoer_datum),
      bvg_type == "SP"
    ) |>
    dplyr::pull(eind_datum)
  expect_true(all(!is.na(eind_datum_eerste_levering)))


  DBI::dbDisconnect(con)
})

test_that("rvo_dieren_en_locaties_invullen functie:
          Als een slachting is bijgewerkt in een tweede levering,
          krijgt de tabel tbl_slacthingen_kv3 een eind_datum ook.", {
            # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
            # Sever database die door SPEK wordt gebruikt
            con <- DBI::dbConnect(odbc::odbc(),
                                  Driver = App$dbdrivernaam,
                                  Server = App$dbservernaam,
                                  Database = App$databasenaam
            )
            test_schema <- "unittest"
            # unittest schema in ont database aanmaken. 
            # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
            # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
            # database'.
            qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
              paste(collapse = "\n") |> 
              glue::as_glue()
            DBI::dbExecute(con, qry)
            
            # synth dataset aanmaken
            set.seed(10)
            synthetische_dataset <- maak_synthetische_dataset(
              n_locaties = 10,
              biologische_fractie = 0,
              straatnamen = maak_straatnamen(),
              n_regels = 1,
              eerste_datum_slacht = "2024-01-01",
              laatste_datum_slacht = "2024-05-01"
            )
            # we slaan de dataset op als csv en gebruiken onze functies om de
            # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
            # zoals in SPEK zal gebeuren
            pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
            readr::write_csv(synthetische_dataset$slachtingen,
                             file = pad_rvo_slachtingen
            )
            
            df_slachtingen <- inl_lees_csv_bestand_met_kolommen_config(
              pad =  pad_rvo_slachtingen,
              kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
              inlezen_functie = readr::read_csv
            )
            pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
            readr::write_csv(synthetische_dataset$slachthuizen,
                             file = pad_rvo_slachthuizen
            )
            
            df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
              pad =  pad_rvo_slachthuizen,
              kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
              inlezen_functie = readr::read_csv
            )
            
            # Verwachtingen opv de input data
            verwachte_n_dieren <- nrow(df_slachtingen)
            
            df_verwacht_dieren_op_locaties <- df_slachtingen |>
              dplyr::select(tidyselect::starts_with(c("ubn", "bvg_type"))) |>
              tidyr::pivot_longer(
                cols = tidyselect::starts_with(c("ubn", "bvg_type")),
                names_to = c(".value", "locatie_geschiedenis"),
                names_pattern = ("(.+)_([0-9])"),
                names_transform = list(
                  ubn = as.character,
                  bvg_type = as.character,
                  locatie_geschiedenis = as.integer
                )
              ) |>
              dplyr::filter(!is.na(ubn)) |>
              dplyr::arrange(locatie_geschiedenis) |>
              dplyr::select(ubn, bvg_type, locatie_geschiedenis)
            
            verwachte_verblijfsduur_eerste_vh <- lubridate::time_length(
              lubridate::interval(
                start = df_slachtingen$datum_ingang_1,
                end = df_slachtingen$datum_einde_1
              ),
              unit = "days"
            )
            kv2_datum <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            df_verwacht_locaties <- df_verwacht_dieren_op_locaties |>
              dplyr::select(-locatie_geschiedenis) |>
              dplyr::bind_rows(
                df_slachthuizen |> dplyr::select(ubn, bvg_type)
              ) |>
              dplyr::distinct()
            
            data.frame(
              bestandsnaam = "test",
              bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
              verwerkingsdatum = kv2_datum
            ) |>
              DBI::dbWriteTable(
                conn = con,
                name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
                value = _,
                append = TRUE
              )
            # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke database te
            # vullen
            rvo_dieren_en_locaties_invullen(
              df_slachtingen = df_slachtingen,
              df_slachthuizen = df_slachthuizen,
              lev_id = 1,
              con = con, kv = 2,
              vwmd = "202404",
              tbl_dieren = "tbl_dieren_kv2_kv3",
              tbl_locaties = "tbl_locaties_kv2_kv3",
              tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
              schema = test_schema,
              datum_f =  kv2_datum
            )
            # Slachtingen tabel invullen
            dplyr::tbl(con, dbplyr::in_schema(test_schema, "tbl_dieren_op_locaties_kv2_kv3")) |> 
              dplyr::left_join(
                dplyr::tbl(con, dbplyr::in_schema(test_schema, "tbl_locaties_kv2_kv3")) |> 
                  dplyr::select(locatie_id, bvg_type),
                by = "locatie_id"
              ) |> 
              dplyr::filter(bvg_type == "SP") |> 
              dplyr::collect() |> 
              dplyr::mutate(
                is_biologisch = FALSE,
                invoer_datum = lubridate::ymd_hms(kv2_datum)
              ) |> 
              dplyr::select(dier_op_locatie_id,
                            is_biologisch, 
                            invoer_datum,
                            eind_datum) |> 
              DBI::dbAppendTable(
                conn = con,
                name = DBI::Id(test_schema, "tbl_slachtingen_kv3"),
                value = _,
                append = TRUE
              )
            
            kv2_datum <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            # De gegevens (geboorte_datum) van het dier was gewijzigd, dus
            # de tabellen tbl_dieren_kv2_kv3 en tbl_dieren_op_locaties_kv2_kv3
            # (zie hieronder) moeten worden bijgewerkt.
            data.frame(
              bestandsnaam = "test_2",
              bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
              verwerkingsdatum = kv2_datum
            ) |>
              DBI::dbWriteTable(
                conn = con,
                name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
                value = _,
                append = TRUE
              )
            
            df_slachtingen_aangepast <- df_slachtingen |> dplyr::mutate(
              datum_geboorte = datum_geboorte + lubridate::days(1)
            )
            rvo_dieren_en_locaties_invullen(
              df_slachtingen = df_slachtingen_aangepast,
              df_slachthuizen = df_slachthuizen,
              lev_id = 2,
              con = con, kv = 2,
              vwmd = "202404",
              tbl_dieren = "tbl_dieren_kv2_kv3",
              tbl_locaties = "tbl_locaties_kv2_kv3",
              tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
              schema = test_schema,
              datum_f =  kv2_datum
            )
            
            # kv2 tabel uit de database ophalen
            df_db_tbl_slachtingen <- dplyr::tbl(con, 
                                           dbplyr::in_schema(test_schema, 
                                                             "tbl_slachtingen_kv3")) |>
              dplyr::collect()
            
            # testen
            # eind datum is niet leeg
            expect_false(is.na(df_db_tbl_slachtingen$eind_datum))
            
            DBI::dbDisconnect(con)
          })


test_that("rvo_kv3_correcties functie:
          KV3 dieren tabel invullen met gecorrigeerd RVO data", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  DBI::dbExecute(con, qry)

  # synth dataset aanmaken
  set.seed(10)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0,
    straatnamen = maak_straatnamen(),
    n_regels = 2,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # we slaan de dataset op als csv en gebruiken onze functies om de
  # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
  # zoals in SPEK zal gebeuren
  pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachtingen,
    file = pad_rvo_slachtingen
  )

  regel_fout <- 1
  df_slachtingen_ruw <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  ) |>
    # de geboortedatum valt later dan de slachtdatum
    fout_geboorte_datum(idx = regel_fout)

  pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachthuizen,
    file = pad_rvo_slachthuizen
  )

  df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )
  
  data.frame(
    bestandsnaam = "test",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke database te
  # vullen
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen_ruw,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # automatische correcties uitvoeren
  # geboortedatum van de foutive regel moet NA worden
  correcties <- dcmodify::modifier(
    if (datum_geboorte > datum_slacht) datum_geboorte <- NA
  )
  pad_fouten_bestand <- withr::local_tempfile(fileext = ".csv")

  tempmap <- withr::local_tempdir()

  bericht <- rvo_kv3_correcties(
    df_slachtingen = df_slachtingen_ruw,
    df_slachthuizen = df_slachthuizen,
    correcties_slachtingen = correcties,
    lev_id = 1,
    con = con,
    vwmd = "202404",
    pad_sjablon = file.path(sjablonen_map, "kv3_correcties.qmd"),
    pad_outputmap = tempmap,
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  # kv2 tabel uit de database ophalen
  df_db_tbl_dieren <- dplyr::tbl(con, 
                                 dbplyr::in_schema(test_schema, 
                                                   "tbl_dieren_kv2_kv3")) |>
    dplyr::collect()
  DBI::dbDisconnect(con)

  # Testen
  ## Verwachtingen:
  ## - De dier met foutive regel moet twee regels in de dieren hebben, een
  ##    voor de KV2 versie (ruwe gegevens) en een met de KV3 versie (gecorrigeerd)

  foutive_dier_id <- df_slachtingen_ruw$levens_nr[regel_fout]
  df_foutive_dier_db <- df_db_tbl_dieren |>
    dplyr::filter(levens_nr == foutive_dier_id)

  expect_equal(nrow(df_foutive_dier_db), 2)

  # De KV2 versie moet een ingevulde eind_datum hebben
  # De KV2 versie is de regel waarin datum_geboorte niet leeg is
  df_foutive_dier_db |>
    dplyr::filter(!is.na(datum_geboorte)) |>
    dplyr::pull(eind_datum) |>
    is.na() |>
    expect_false()

  # De KV3 versie moet een leeg eind_datum hebben
  # De KV3 versie is de regel waarin datum_geboorte leeg is
  df_foutive_dier_db |>
    dplyr::filter(is.na(datum_geboorte)) |>
    dplyr::pull(eind_datum) |>
    is.na() |>
    expect_true()
})

test_that("rvo_dieren_en_locaties_invullen functie: 
          De gebruiker probeert KV2 twee keer te vullen met dezelfde gegevens 
          (die worden gecorrigeerd) en de database wordt dan niet bijgewerkt. ", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  DBI::dbExecute(con, qry)
  # synth dataset aanmaken
  set.seed(10)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0,
    straatnamen = maak_straatnamen(),
    n_regels = 2,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-04-30"
  )
  # we slaan de dataset op als csv en gebruiken onze functies om de
  # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
  # zoals in SPEK zal gebeuren
  pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachtingen,
                   file = pad_rvo_slachtingen
  )
  
  regel_fout <- 1
  df_slachtingen_ruw <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  ) |>
    # de geboortedatum valt later dan de slachtdatum
    fout_geboorte_datum(idx = regel_fout)
  
  pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachthuizen,
                   file = pad_rvo_slachthuizen
  )
  
  df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )
  
  data.frame(
    bestandsnaam = "test",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke database te
  # vullen
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen_ruw,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  # automatische correcties uitvoeren
  # geboortedatum van de foutive regel moet NA worden
  correcties <- dcmodify::modifier(
    if (datum_geboorte > datum_slacht) datum_geboorte <- NA
  )
  pad_fouten_bestand <- withr::local_tempfile(fileext = ".csv")
  
  tempmap <- withr::local_tempdir()
  
  # KV invullen
  bericht <- rvo_kv3_correcties(
    df_slachtingen = df_slachtingen_ruw,
    df_slachthuizen = df_slachthuizen,
    correcties_slachtingen = correcties,
    lev_id = 1,
    con = con,
    vwmd = "202404",
    pad_sjablon = file.path(sjablonen_map, "kv3_correcties.qmd"),
    pad_outputmap = tempmap,
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  # we willen weten de aantal rijen in de tabellen om later te vergelijken
  n_rijen_na_kv3_dieren <- dplyr::tbl(
      con,
      dbplyr::in_schema(test_schema, "tbl_dieren_kv2_kv3")) |>
    dplyr::collect() |> 
    nrow()
  n_rijen_na_kv3_locaties <- dplyr::tbl(
      con, 
      dbplyr::in_schema(test_schema, "tbl_locaties_kv2_kv3")) |>
    dplyr::collect() |> 
    nrow()
  n_rijen_na_kv3_dieren_op_locaties <-  dplyr::tbl(
      con, 
      dbplyr::in_schema(test_schema, "tbl_dieren_op_locaties_kv2_kv3")) |>
    dplyr::collect() |> 
    nrow()
  # Tweede keer dezelfde input data nlezen
  ## KV2
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen_ruw,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  n_rijen_na_tweede_inlezen_dieren <-  dplyr::tbl(
      con,
      dbplyr::in_schema(test_schema, "tbl_dieren_kv2_kv3")) |>
    dplyr::collect() |> 
    nrow()
  n_rijen_na_tweede_inlezen_locaties <- dplyr::tbl(
      con, 
      dbplyr::in_schema(test_schema, "tbl_locaties_kv2_kv3")) |>
    dplyr::collect() |> 
    nrow()
  n_rijen_na_tweede_inlezen_dieren_op_locaties <- dplyr::tbl(
      con, 
      dbplyr::in_schema(test_schema, "tbl_dieren_op_locaties_kv2_kv3")) |>
    dplyr::collect() |> 
    nrow()
  
  DBI::dbDisconnect(con)
  
  # We verwachten dezelfde aantal rijen in alle tabellen na de tweede inlezen want 
  # we hebben dezelfde input data gebruikt
  expect_equal(n_rijen_na_tweede_inlezen_dieren,
               n_rijen_na_kv3_dieren)
  
  expect_equal(n_rijen_na_tweede_inlezen_locaties,
               n_rijen_na_kv3_locaties)
  
  expect_equal(n_rijen_na_tweede_inlezen_dieren_op_locaties,
               n_rijen_na_kv3_dieren_op_locaties)
})

test_that("rvo_locaties_kopppeling functie: returneer de juiste objecten", {
  df_locaties <- data.frame(
    locatie_id = c(11, 22),
    ubn = c(1, 2),
    bvg_postcode_plaatscode = c(NA, "1111"),
    bvg_postcode_lettercode = c(NA, "AA"),
    bvg_huisnummer = c(NA, "33"),
    bvg_huisnummer_toevoeging = c(NA, NA)
  )

  df_verblijfsplaatsen <- data.frame(
    ubn = 1,
    relnr = 1
  )

  df_landbouwtelling <- data.frame(
    relnr = 1,
    bvg_postcode_plaatscode = c("2222"),
    bvg_postcode_lettercode = c("BB"),
    bvg_huisnummer = c("1"),
    bvg_huisnummer_toevoeging = c(NA),
    skalnr_lbt = "22222"
  )

  df_skal <- data.frame(
    skalnummer = "2222",
    huisnummer = "1",
    postcode = "2222BB",
    resultaat_certificatie = "Biologisch",
    datum_certificatie_geldigheid = "01012022",
    datum_geldig_vanaf  = "01012021"
  )


  output <- rvo_locaties_koppeling(
    df_locaties_kv3 = df_locaties,
    df_landbouwtelling = df_landbouwtelling,
    df_verblijfsplaatsen = df_verblijfsplaatsen,
    df_skal = df_skal
  )

  expect_type(output, "list")

  expect_named(
    output,
    c(
      "locaties_zonder_adres",
      "locaties_zonder_relnr",
      "locaties_meerdere_relnrs",
      "locaties_met_relnr_zonder_adres",
      "locaties_zonder_adres_voor_norm",
      "locaties_problematisch_norm_adressen",
      "locaties_zonder_adres_vanvege_norm",
      "locaties_zonder_skal",
      "locaties_en_skal",
      "skal_zonder_adres"
    )
  )
  # alle objecten zijn data.frames
  purrr::walk(output, \(df) expect_s3_class(df, "data.frame"))
})

test_that("rvo_locaties_kopppeling functie: koppelt RVO aan skal locaties", {
  df_locaties <- data.frame(
    locatie_id = c(11, 22),
    ubn = c(1, 2),
    bvg_postcode_plaatscode = c(NA, "1111"),
    bvg_postcode_lettercode = c(NA, "AA"),
    bvg_huisnummer = c(NA, "33"),
    bvg_huisnummer_toevoeging = c(NA, NA)
  )

  df_verblijfsplaatsen <- data.frame(
    ubn = 1,
    relnr = 1
  )

  df_landbouwtelling <- data.frame(
    relnr = 1,
    bvg_postcode_plaatscode = c("2222"),
    bvg_postcode_lettercode = c("BB"),
    bvg_huisnummer = c("1"),
    bvg_huisnummer_toevoeging = c(NA),
    skalnr_lbt = "22222"
  )
  
  df_skal <- data.frame(
    skalnummer = "22221",
    huisnummer = "1",
    postcode = "2222BB",
    resultaat_certificatie = "Biologisch",
    datum_certificatie_geldigheid = "01012022",
    datum_geldig_vanaf  = "01012021"
  )


  output <- rvo_locaties_koppeling(
    df_locaties_kv3 = df_locaties,
    df_landbouwtelling = df_landbouwtelling,
    df_verblijfsplaatsen = df_verblijfsplaatsen,
    df_skal = df_skal
  )


  expect_s3_class(output$locaties_en_skal, "data.frame")
  expect_equal(nrow(output$locaties_en_skal), 1)
  expect_equal(output$locaties_en_skal$resultaat_certificatie, "Biologisch")
})

test_that("rvo_locaties_kopppeling functie: retourneert RVO locaties zonder adres", {
  df_locaties <- data.frame(
    locatie_id = c(11, 22),
    ubn = c(1, 2),
    bvg_postcode_plaatscode = c(NA, "1111"),
    bvg_postcode_lettercode = c(NA, "AA"),
    bvg_huisnummer = c(NA, "33"),
    bvg_huisnummer_toevoeging = c(NA, NA)
  )

  df_verblijfsplaatsen <- data.frame(
    ubn = 1,
    relnr = 1
  )

  df_landbouwtelling <- data.frame(
    relnr = 1,
    bvg_postcode_plaatscode = c("2222"),
    bvg_postcode_lettercode = c("BB"),
    bvg_huisnummer = c("1"),
    bvg_huisnummer_toevoeging = c(NA),
    skalnr_lbt = "22222"
  )
  
  df_skal <- data.frame(
    skalnummer = "22221",
    huisnummer = "1",
    postcode = "2222BB",
    resultaat_certificatie = "Biologisch",
    datum_certificatie_geldigheid = "01012022",
    datum_geldig_vanaf  = "01012021"
  )

  output <- rvo_locaties_koppeling(
    df_locaties_kv3 = df_locaties,
    df_landbouwtelling = df_landbouwtelling,
    df_verblijfsplaatsen = df_verblijfsplaatsen,
    df_skal = df_skal
  )

  expect_s3_class(output$locaties_zonder_adres, "data.frame")
  expect_equal(nrow(output$locaties_zonder_adres), 1)
  expect_equal(output$locaties_zonder_adres$locatie_id, 11)
})

test_that(
  "rvo_locaties_kopppeling functie: retourneert niet gekoppeld locaties: zonder relnr
  In deze situatie is er een locatie (verblijfsplaats) in RVO dataset zonder adres
  die niet aan de landbouwtelling kan worden gekoppeld.
  We verwachten dus de niet gekoppeld locatie als output van de functie,
  in de locaties_zonder_relnr object",
  {
    df_locaties <- data.frame(
      locatie_id                = c(22),
      ubn                       = c(1),
      bvg_postcode_plaatscode   = c(NA_character_),
      bvg_postcode_lettercode   = c(NA_character_),
      bvg_huisnummer            = c(NA_character_),
      bvg_huisnummer_toevoeging = c(NA_character_)
    )

    df_verblijfsplaatsen <- data.frame(
      ubn   = c(2),
      relnr = c(3)
    )

    df_landbouwtelling <- data.frame(
      relnr = 3,
      bvg_postcode_plaatscode = c("2222"),
      bvg_postcode_lettercode = c("BB"),
      bvg_huisnummer = c("1"),
      bvg_huisnummer_toevoeging = c(NA),
      skalnr_lbt = "22222"
    )
    
    df_skal <- data.frame(
      skalnummer = "22221",
      huisnummer = "1",
      postcode = "2222BB",
      resultaat_certificatie = "Biologisch",
      datum_certificatie_geldigheid = "01012022",
      datum_geldig_vanaf  = "01012021"
    )


    output <- rvo_locaties_koppeling(
      df_locaties_kv3 = df_locaties,
      df_landbouwtelling = df_landbouwtelling,
      df_verblijfsplaatsen = df_verblijfsplaatsen,
      df_skal = df_skal
    )

    expect_s3_class(output$locaties_zonder_relnr, "data.frame")
    expect_equal(nrow(output$locaties_zonder_relnr), 1)
    expect_equal(output$locaties_zonder_adres$locatie_id, 22)
  }
)

test_that(
  "rvo_locaties_koppeling functie: retourneert locaties waarvan het UBN aan 
  meerdere relnrs kan worden gekoppeld in de skal database.", 
  {
    df_locaties <- data.frame(
      locatie_id                = c(22),
      ubn                       = c(1),
      bvg_postcode_plaatscode   = c(NA_character_),
      bvg_postcode_lettercode   = c(NA_character_),
      bvg_huisnummer            = c(NA_integer_),
      bvg_huisnummer_toevoeging = c(NA_character_)
    )
    
    df_verblijfsplaatsen <- data.frame(
      ubn   = c(1,1),
      relnr = c(3,4)
    )
    
    df_landbouwtelling <- data.frame(
      relnr                     = c(3, 4),
      bvg_postcode_plaatscode   = c("2222", "2222"),
      bvg_postcode_lettercode   = c("BB", "BB"),
      bvg_huisnummer            = c(1, 1),
      bvg_huisnummer_toevoeging = c(NA_character_, NA_character_),
      skalnr_lbt = c(NA_character_, NA_character_)
    )
    
    df_skal <- data.frame(
      skalnummer                    = "222221",
      huisnummer                    = "1",
      postcode                      =  "2222BB",
      resultaat_certificatie        = "Biologisch",
      datum_certificatie_geldigheid = "01012022",
      datum_geldig_vanaf  = "01012021"
    )
    
    
    output <- rvo_locaties_koppeling(
      df_locaties_kv3 = df_locaties,
      df_landbouwtelling = df_landbouwtelling,
      df_verblijfsplaatsen = df_verblijfsplaatsen,
      df_skal = df_skal
    )
    
    expect_s3_class(output$locaties_meerdere_relnrs, "data.frame")
    expect_equal(nrow(output$locaties_meerdere_relnrs), 2)
    expect_setequal(output$locaties_meerdere_relnrs$ubn, c(1,1))
    expect_setequal(output$locaties_meerdere_relnrs$relnr, c(3,4))
  }
)

test_that(
  "rvo_locaties_kopppeling functie: retourneert niet gekoppeld locaties:
    geen adres na koppeling met landobouwtelling.
  In deze situatie wordt de RVO-locatie (verblijfsplaats) aan de landbouwtelling
  gekoppeld maar de landbowutelling bevat geen adres.",
  {
    df_locaties <- data.frame(
      locatie_id                = c(22),
      ubn                       = c(1),
      bvg_postcode_plaatscode   = c(NA_character_),
      bvg_postcode_lettercode   = c(NA_character_),
      bvg_huisnummer            = c(NA_integer_),
      bvg_huisnummer_toevoeging = c(NA_character_)
    )

    df_verblijfsplaatsen <- data.frame(
      ubn   = c(1),
      relnr = c(3)
    )

    df_landbouwtelling <- data.frame(
      relnr                     = 3,
      bvg_postcode_plaatscode   = NA_character_,
      bvg_postcode_lettercode   = NA_character_,
      bvg_huisnummer            = NA_integer_,
      bvg_huisnummer_toevoeging = c(NA_character_),
      skalnr_lbt = c(NA_character_)
    )
    
    df_skal <- data.frame(
      skalnummer                    = "222221",
      huisnummer                    = "1",
      postcode                      =  "2222BB",
      resultaat_certificatie        = "Biologisch",
      datum_certificatie_geldigheid = "01012022",
      datum_geldig_vanaf  = "01012021"
    )


    output <- rvo_locaties_koppeling(
      df_locaties_kv3 = df_locaties,
      df_landbouwtelling = df_landbouwtelling,
      df_verblijfsplaatsen = df_verblijfsplaatsen,
      df_skal = df_skal
    )

    expect_s3_class(output$locaties_met_relnr_zonder_adres, "data.frame")
    expect_equal(nrow(output$locaties_met_relnr_zonder_adres), 1)
    expect_equal(output$locaties_met_relnr_zonder_adres$locatie_id, 22)
    expect_equal(
      output$locaties_met_relnr_zonder_adres,
      output$locaties_zonder_adres_voor_norm
    )
  }
)

# locaties_problematisch_norm_adressen  en locaties_zonder_adres_vanvege_norm
# zijn nie getest. Deze zijn bedoeld om de gevallen op te vangen waarin
# adresnorm er niet in slaagt om het adres te normaliseren. Maar op dit moment
# verwijdert adresnorm geen ongeldige postcodes of huisnummers, dus deze twee
# dataframes zullen leeg zijn. Ik laat ze staan voor het geval adresnorm in de
# toekomst dit gedrag verandert.

test_that(
  "rvo_locaties_kopppeling functie: retourneert niet gekoppeld locaties:
    de verblijfsplaats staat niet in skal.
  In deze situatie wordt de RVO-locatie (verblijfsplaats) aan de landbouwtelling
  gekoppeld maar de adres staat niet in skal dus ze kunnen niet worden gekoppeld",
  {
    df_locaties <- data.frame(
      locatie_id                = c(22),
      ubn                       = c(1),
      bvg_postcode_plaatscode   = c(NA_character_),
      bvg_postcode_lettercode   = c(NA_character_),
      bvg_huisnummer            = c(NA_character_),
      bvg_huisnummer_toevoeging = c(NA_character_)
    )

    df_verblijfsplaatsen <- data.frame(
      ubn   = c(1),
      relnr = c(3)
    )

    df_landbouwtelling <- data.frame(
      relnr                     = 3,
      bvg_postcode_plaatscode   = "1989",
      bvg_postcode_lettercode   = "SP",
      bvg_huisnummer            = "12",
      bvg_huisnummer_toevoeging = c(NA_character_),
      skalnr_lbt = c(NA_character_)
    )
    
    df_skal <- data.frame(
      skalnummer                    = "222221",
      huisnummer                    = "1",
      postcode                      =  "2222BB",
      resultaat_certificatie        = "Biologisch",
      datum_certificatie_geldigheid = "01012022",
      datum_geldig_vanaf  = "01012021"
    )

    output <- rvo_locaties_koppeling(
      df_locaties_kv3 = df_locaties,
      df_landbouwtelling = df_landbouwtelling,
      df_verblijfsplaatsen = df_verblijfsplaatsen,
      df_skal = df_skal
    )

    expect_s3_class(output$locaties_zonder_skal, "data.frame")
    expect_equal(nrow(output$locaties_zonder_skal), 1)
    expect_equal(output$locaties_zonder_skal$locatie_id, 22)
  }
)

test_that(
  "rvo_locaties_kopppeling functie: retourneert niet gekoppeld locaties:
    de adres in skal is leeg.",
  {
    df_locaties <- data.frame(
      locatie_id                = c(22),
      ubn                       = c(1),
      bvg_postcode_plaatscode   = c(NA_character_),
      bvg_postcode_lettercode   = c(NA_character_),
      bvg_huisnummer            = c(NA_character_),
      bvg_huisnummer_toevoeging = c(NA_character_)
    )

    df_verblijfsplaatsen <- data.frame(
      ubn   = c(1),
      relnr = c(3)
    )

    df_landbouwtelling <- data.frame(
      relnr                     = 3,
      bvg_postcode_plaatscode   = "1989",
      bvg_postcode_lettercode   = "SP",
      bvg_huisnummer            = "12",
      bvg_huisnummer_toevoeging = c(NA_character_),
      skalnr_lbt = c(NA_character_)
    )
    
    df_skal <- data.frame(
      skalnummer                    = "222221",
      huisnummer                    = NA_character_,
      postcode                      =  NA_character_,
      resultaat_certificatie        = "Biologisch",
      datum_certificatie_geldigheid = "01012022",
      datum_geldig_vanaf  = "01012021"
    )

    output <- rvo_locaties_koppeling(
      df_locaties_kv3 = df_locaties,
      df_landbouwtelling = df_landbouwtelling,
      df_verblijfsplaatsen = df_verblijfsplaatsen,
      df_skal = df_skal
    )

    expect_s3_class(output$skal_zonder_adres, "data.frame")
    expect_equal(nrow(output$skal_zonder_adres), 1)
    expect_equal(df_skal, output$skal_zonder_adres)
  }
)

test_that("rvo_kv3_bio_locaties functie:
          Nieuwe bio locaties worden toegevoegd aan de database tabel", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  DBI::dbExecute(con, qry)

  # We gaan een locatie toevoegen aan de tbl_locaties_kv2_kv3 tabel
  
  # synth dataset aanmaken
  set.seed(10)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0,
    straatnamen = maak_straatnamen(),
    n_regels = 2,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # we slaan de dataset op als csv en gebruiken onze functies om de
  # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
  # zoals in SPEK zal gebeuren
  pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachtingen,
                   file = pad_rvo_slachtingen
  )
  
  df_slachtingen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  )
  
  pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachthuizen,
                   file = pad_rvo_slachthuizen
  )
  
  df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )
  
  data.frame(
    bestandsnaam = "test",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke database te
  # vullen
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  df_locaties_en_skal <- data.frame(
    locatie_id = 1,
    resultaat_certificatie = "Biologisch",
    datum_certificatie_geldigheid = "2022-09-12",
    datum_geldig_vanaf  = "2020-01-01"
  )

  rvo_kv3_bio_locaties(
    df_locaties_en_skal = df_locaties_en_skal,
    con = con,
    bio_certificatie = "Biologisch",
    tbl_bio_locaties = "tbl_biologische_locaties_kv3",
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    schema = test_schema
    )

  df_db <- dplyr::tbl(con, 
                      dbplyr::in_schema(
                        test_schema,
                        "tbl_biologische_locaties_kv3")) |>
    dplyr::collect()

  expect_equal(nrow(df_db), 1)
  DBI::dbDisconnect(con)
})

test_that("rvo_kv3_bio_locaties functie:
          bio locaties worden niet toegevoegd aan de database tabel als
          ze al in de database zitten", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  DBI::dbExecute(con, qry)
  
  # We gaan een locatie toevoegen aan de tbl_locaties_kv2_kv3 tabel
  
  # synth dataset aanmaken
  set.seed(10)
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = 10,
    biologische_fractie = 0,
    straatnamen = maak_straatnamen(),
    n_regels = 2,
    eerste_datum_slacht = "2024-01-01",
    laatste_datum_slacht = "2024-05-01"
  )
  # we slaan de dataset op als csv en gebruiken onze functies om de
  # RVO-invoerbestanden in te lezen, zodat we de juiste kolomnamen krijgen,
  # zoals in SPEK zal gebeuren
  pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachtingen,
                   file = pad_rvo_slachtingen
  )
  
  df_slachtingen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachtingen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    inlezen_functie = readr::read_csv
  )
  
  pad_rvo_slachthuizen <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(synthetische_dataset$slachthuizen,
                   file = pad_rvo_slachthuizen
  )
  
  df_slachthuizen <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  pad_rvo_slachthuizen,
    kolommen_config = App$kolomnamen$i_en_r_runderen_slachtplaatsen,
    inlezen_functie = readr::read_csv
  )
  
  data.frame(
    bestandsnaam = "test",
    bestandshash = cli::hash_file_sha256(pad_rvo_slachtingen),
    verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = test_schema, table = "tbl_levering_rvo"),
      value = _,
      append = TRUE
    )
  # we voeren nu de functie uit om de kv2-tabellen van de tijdelijke database te
  # vullen
  rvo_dieren_en_locaties_invullen(
    df_slachtingen = df_slachtingen,
    df_slachthuizen = df_slachthuizen,
    lev_id = 1,
    con = con, kv = 2,
    vwmd = "202404",
    tbl_dieren = "tbl_dieren_kv2_kv3",
    tbl_locaties = "tbl_locaties_kv2_kv3",
    tbl_dieren_op_locaties = "tbl_dieren_op_locaties_kv2_kv3",
    schema = test_schema,
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
            
  df_locaties_en_skal <- data.frame(
    locatie_id = 1,
    resultaat_certificatie = "Biologisch",
    datum_certificatie_geldigheid = "2022-09-12",
    datum_geldig_vanaf = "2020-09-12"
  )
  
  rvo_kv3_bio_locaties(
    df_locaties_en_skal = df_locaties_en_skal,
    con = con,
    bio_certificatie = "Biologisch",
    tbl_bio_locaties = "tbl_biologische_locaties_kv3",
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    schema = test_schema
  )

  rvo_kv3_bio_locaties(
    df_locaties_en_skal = df_locaties_en_skal,
    con = con,
    bio_certificatie = "Biologisch",
    tbl_bio_locaties = "tbl_biologische_locaties_kv3",
    datum_f =  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    schema = test_schema
  )
  
  df_db <- dplyr::tbl(con, 
                      dbplyr::in_schema(
                        test_schema,
                        "tbl_biologische_locaties_kv3")) |>
    dplyr::collect()

  expect_equal(nrow(df_db), 1)
  DBI::dbDisconnect(con)
})
