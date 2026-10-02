library(testthat)
root_map <- here::here()
src_map <- file.path(root_map, "src")
test_map <- file.path(root_map, "unittesten")
testdata_map <- file.path(test_map, "testdata")

source(file.path(src_map, "rvo_verwerken.R"))
source(file.path(src_map, "rvo_controles_en_correcties.R"))
source(file.path(src_map, "rvo_hulpfuncties.R"))
source(file.path(src_map, "synthetische_data.R"))
source(file.path(src_map, "inlezen.R"))


test_that("De eind_datum kolom wordt bijgewerkt", {
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = ":memory:"
  )
  # dbo schema toevoegen
  tmp <- withr::local_tempfile()
  DBI::dbExecute(con, paste0("ATTACH '", tmp, "' AS dbo"))

  df_db <- data.frame(
    id = 1,
    eind_datum = NA
  )

  tabel_naam <- "tbl_dieren"
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("dbo", tabel_naam),
    value = df_db
  )


  df_levering <- data.frame(
    id = 1
  )

  nieuwe_datum <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  update_eind_datum(
    df = df_levering,
    id_kolom = "id",
    tbl_naam = tabel_naam,
    datum = nieuwe_datum,
    con = con,
    schema = "dbo"
  )

  df_nieuwe_db <- DBI::dbGetQuery(
    conn = con,
    glue::glue("SELECT * FROM {tabel_naam}")
  )
  DBI::dbDisconnect(con)
  expect_equal(df_nieuwe_db$eind_datum, nieuwe_datum)
})

test_that("De eind_datum kolom wordt bijgewerkt ook met meerdere rijen", {
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = ":memory:"
  )
  # dbo schema toevoegen
  tmp <- withr::local_tempfile()
  DBI::dbExecute(con, paste0("ATTACH '", tmp, "' AS dbo"))
  
  df_db <- data.frame(
    id = 1:100,
    eind_datum = NA
  )

  tabel_naam <- "tbl_dieren"
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("dbo", tabel_naam),
    value = df_db
  )


  df_levering <- data.frame(
    id = 1:100
  )

  nieuwe_datum <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  update_eind_datum(
    df = df_levering,
    id_kolom = "id",
    tbl_naam = tabel_naam,
    datum = nieuwe_datum,
    con = con,
    schema = "dbo"
  )

  df_nieuwe_db <- DBI::dbGetQuery(
    conn = con,
    glue::glue("SELECT * FROM dbo.{tabel_naam}")
  )
  DBI::dbDisconnect(con)
  
  expect_equal(
    unique(df_nieuwe_db$eind_datum), nieuwe_datum
  )
})


test_that("Nieuwe gegevens zijn al aanwezig in db", {
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = ":memory:"
  )

  df_db <- data.frame(
    id = 1,
    levens_nr = "000000",
    eigenschaap_1 = letters[1],
    invoer_datum = "2024-05-01",
    eind_datum = NA,
    levering_id = 1
  )

  DBI::dbWriteTable(
    conn = con,
    name = "tbl_dieren",
    value = df_db
  )


  df_levering <- data.frame(
    data.frame(
      levens_nr = "000000",
      eigenschaap_1 = letters[1]
    )
  )

  nieuwe_datum <- "2024-06-01"
  nieuwe_lev_id <- 2

  df_result <- vergelijk_en_update(
    df_levering = df_levering,
    df_db = df_db,
    pk_id = "id",
    ids = "levens_nr",
    gemene_kolommen = c("levens_nr", "eigenschaap_1"),
    datum_kolommen = "datum",
    tabel = "tbl_dieren",
    tabel_met_levering_id = TRUE,
    con = con,
    lev_id = nieuwe_lev_id,
    datum = nieuwe_datum
  )

  df_nieuwe_db <- DBI::dbGetQuery(
    conn = con,
    "SELECT * FROM tbl_dieren"
  )

  expect_equal(
    nrow(df_nieuwe_db),
    1
  )

  expect_equal(
    df_nieuwe_db$eigenschaap_1,
    letters[1]
  )

  DBI::dbDisconnect(con)
})



test_that("Nieuwe gegevens worden toegevoegd als ze niet aanwezig zijn in db", {
  # we maken een tijdelijke sqlite database met dezelfde tabellen als de SQL
  # Sever database die door SPEK wordt gebruikt
  # maak een verbinding met de ont database
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"

  
  df_db <- data.frame(
    id = 1,
    levens_nr = "000000",
    eigenschaap_1 = letters[1],
    invoer_datum = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    eind_datum = NA,
    levering_id = 1
  )

  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "temp_tabel"),
    value = df_db,
    overwrite= TRUE
  )

  df_levering <- data.frame(
    data.frame(
      levens_nr = "000001",
      eigenschaap_1 = letters[2]
    )
  )

  nieuwe_datum <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  nieuwe_lev_id <- 2

  expect_message(
    df_result <- vergelijk_en_update(
      df_levering = df_levering,
      df_db = df_db,
      pk_id = "id",
      ids = "levens_nr",
      gemene_kolommen = c("levens_nr"),
      datum_kolommen = "datum",
      tabel = "temp_tabel",
      tabel_met_levering_id = TRUE,
      con = con,
      lev_id = nieuwe_lev_id,
      datum = nieuwe_datum,
      schema = "unittest"
    ),
    "1 rijen toegevoegd aan tabel temp_tabel"
  )

  expect_equal(
    df_result |> dplyr::pull(levens_nr),
    df_levering |> dplyr::pull(levens_nr)
  )

  df_nieuwe_db <- DBI::dbGetQuery(
    conn = con,
    "SELECT * FROM unittest.temp_tabel"
  )

  expect_equal(
    nrow(df_nieuwe_db),
    2
  )

  expect_equal(
    df_nieuwe_db$eigenschaap_1,
    letters[1:2]
  )

  
  DBI::dbGetQuery(conn = con, statement = "drop table unittest.temp_tabel")
  
  DBI::dbDisconnect(con)
})

test_that("Nieuwe gegevens worden NIET toegevoegd als ze aanwezig zijn in db", {
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  test_schema <- "unittest"
  
  df_db <- data.frame(
    id = 1,
    levens_nr = "000000",
    eigenschaap_1 = letters[1],
    invoer_datum = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    eind_datum = NA,
    levering_id = 1
  )

  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "temp_tabel"),
    value = df_db,
    overwrite = TRUE
  )

  df_levering <- data.frame(
    data.frame(
      levens_nr = c("000000", "000001"),
      eigenschaap_1 = letters[1:2]
    )
  )

  nieuwe_datum <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  nieuwe_lev_id <- 2

  expect_message(
    df_result <- vergelijk_en_update(
      df_levering = df_levering,
      df_db = df_db,
      pk_id = "id",
      ids = "levens_nr",
      gemene_kolommen = c("levens_nr", "eigenschaap_1"),
      datum_kolommen = "datum",
      tabel = "temp_tabel",
      tabel_met_levering_id = TRUE,
      con = con,
      lev_id = nieuwe_lev_id,
      datum = nieuwe_datum,
      schema = "unittest"
    ),
    "1 rijen toegevoegd aan tabel temp_tabel."
  )

  expect_equal(
    df_result |> dplyr::pull(levens_nr),
    df_levering |> 
      dplyr::filter(levens_nr == "000001") |> 
      dplyr::pull(levens_nr)
  )
  
  df_nieuwe_db <- DBI::dbGetQuery(
    conn = con,
    "SELECT * FROM unittest.temp_tabel"
  )
  
  expect_equal(
    nrow(df_nieuwe_db),
    2
  )
  
  expect_equal(
    df_nieuwe_db$eigenschaap_1,
    letters[1:2]
  )
  
  DBI::dbGetQuery(conn = con, statement = "drop table unittest.temp_tabel")
  
  DBI::dbDisconnect(con)
})

test_that("Records in DB zijn bijgerwerkt als de nieuwe gegevens wijzigingen hebben", {
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  
  test_schema <- "unittest"

  df_db <- data.frame(
    id = 1:2,
    levens_nr = c("1", "2"),
    eigenschaap_1 = letters[1:2],
    invoer_datum = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    eind_datum = lubridate::NA_Date_,
    levering_id = 1
  )

  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "temp_tabel"),
    value = df_db,
    overwrite = TRUE
  )

  df_levering <- data.frame(
    data.frame(
      levens_nr = c("1", "2"),
      eigenschaap_1 = letters[c(1, 3)]
    )
  )

  nieuwe_datum <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  nieuwe_lev_id <- 2

  df_result <- vergelijk_en_update(
    df_levering = df_levering,
    df_db = dplyr::tbl(con, dbplyr::in_schema(test_schema, "temp_tabel")) |> 
      dplyr::collect(),
    pk_id = "id",
    ids = "levens_nr",
    gemene_kolommen = c("levens_nr", "eigenschaap_1"),
    datum_kolommen = "datum",
    tabel = "temp_tabel",
    tabel_met_levering_id = TRUE,
    con = con,
    lev_id = nieuwe_lev_id,
    datum = nieuwe_datum,
    schema = "unittest"
  )
  
  expect_equal(
    df_result |> dplyr::pull(levens_nr),
    df_levering |> 
      dplyr::filter(levens_nr == "2") |>
      dplyr::pull(levens_nr)
  )
  
  df_nieuwe_db <- DBI::dbGetQuery(
    conn = con,
    "SELECT * FROM unittest.temp_tabel"
  )

  expect_equal(
    nrow(df_nieuwe_db),
    3
  )

  expect_equal(
    df_nieuwe_db$eigenschaap_1,
    letters[1:3]
  )
  # oude record krijgen eind_datum
  expect_false(
    df_nieuwe_db |>
      dplyr::filter(id == 2) |>
      dplyr::pull(eind_datum) |> 
      is.na()
  )
  DBI::dbGetQuery(conn = con, statement = "drop table unittest.temp_tabel")

  DBI::dbDisconnect(con)
})

test_that("Meerdere rijen en kolommen", {
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  
  test_schema <- "unittest"
  
  df_db <- data.frame(
    id = 1:20,
    levens_nr = 1:20,
    eigenschaap_1 = letters[1:20],
    eigenschaap_2 = LETTERS[1:20],
    eigenschaap_3 = 1:20,
    invoer_datum = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    eind_datum = lubridate::NA_Date_,
    levering_id = 1
  )
  
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "temp_tabel"),
    value = df_db,
    overwrite = TRUE
  )

  df_levering <- data.frame(
    data.frame(
      levens_nr = 11:40,
      eigenschaap_1 = c(letters[11:14], letters),
      eigenschaap_2 = c(LETTERS[11:20], LETTERS[1:20]),
      eigenschaap_3 = 11:40
    )
  )

  nieuwe_datum <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  nieuwe_lev_id <- 2

  expect_message(
    df_result <- vergelijk_en_update(
      df_levering = df_levering,
      df_db = df_db,
      pk_id = "id",
      ids = "levens_nr",
      gemene_kolommen = c("levens_nr", "eigenschaap_1"),
      datum_kolommen = "datum",
      tabel = "temp_tabel",
      tabel_met_levering_id = TRUE,
      con = con,
      lev_id = nieuwe_lev_id,
      datum = nieuwe_datum,
      schema = "unittest"
    )
  )


  df_nieuwe_db <- DBI::dbGetQuery(
    conn = con,
    "SELECT * FROM unittest.temp_tabel"
  )

  expect_equal(
    nrow(df_nieuwe_db),
    nrow(df_db) + nrow(df_levering) - 4
  )

  # oude record krijgen eind_datum
  expect_equal(
    df_nieuwe_db |>
      dplyr::filter(id >= 15 & id <= 20) |>
      dplyr::pull(eind_datum) |> unique(),
    format(as.POSIXct(nieuwe_datum) , "%Y-%m-%d")
  )

  DBI::dbDisconnect(con)
})




test_that("Alle diersorten zijn correct bepaald", {
  jongste_kalf <- ":-)"

  output <- rvo_diersoort_bepalen(
    leeftijd_jaar = 0.1,
    geslacht = "V",
    datum_afkalven = "2020-12-22",
    datum_import = NA,
    kalf_jongste = jongste_kalf,
    max_maand_jongste_kalf = 8
  )

  expect_equal(output, jongste_kalf)

  oudste_kalf <- ":--)"

  output <- rvo_diersoort_bepalen(
    leeftijd_jaar = 0.9,
    geslacht = "V",
    datum_afkalven = "2020-12-22",
    datum_import = NA,
    kalf_oudste = oudste_kalf,
    max_maand_oudste_kalf = 12
  )

  expect_equal(output, oudste_kalf)

  koe <- "koeien"

  output <- rvo_diersoort_bepalen(
    leeftijd_jaar = NA,
    geslacht = NA,
    datum_afkalven = "2020-12-22",
    datum_import = NA,
    koe = koe
  )

  expect_equal(output, koe)

  output <- rvo_diersoort_bepalen(
    leeftijd_jaar = 3,
    geslacht = "V",
    datum_afkalven = "2020-12-22",
    datum_import = NA,
    koe = koe
  )

  expect_equal(output, koe)


  overige <- ":-("

  output <- rvo_diersoort_bepalen(
    leeftijd_jaar = NA,
    geslacht = NA,
    datum_afkalven = NA,
    datum_import = NA,
    overige = overige
  )

  expect_equal(output, overige)


  stier <- ">:-)"

  output <- rvo_diersoort_bepalen(
    leeftijd_jaar = 3,
    geslacht = "M",
    datum_afkalven = NA,
    datum_import = NA,
    stier = stier
  )

  expect_equal(output, stier)

  vaars <- "vaars"

  output <- rvo_diersoort_bepalen(
    leeftijd_jaar = 3,
    geslacht = "V",
    datum_afkalven = NA,
    datum_import = NA,
    vaars = vaars
  )

  expect_equal(output, vaars)
  
  onb <- "Onbekend"
    
  output <- rvo_diersoort_bepalen(
    leeftijd_jaar = 4,
    geslacht = "V",
    datum_afkalven = "2020-12-22",
    datum_import = "2020-12-22",
    koe = koe
  )
  
  expect_equal(output, onb)
})


test_that("Ongeldige input waarden worden opgevangen in de rvo_diersoort_bepalen functie", {
  # Geslacht kan maar V of M zijn
  expect_error(
    output <- rvo_diersoort_bepalen(
      leeftijd_jaar = 3,
      geslacht = "Vrouw",
      datum_afkalven = NA
    ),
    ".*Geslacht \\(Vrouw\\) niet geldig"
  )
  # leeftijd moet numeriek zijn
  expect_error(
    output <- rvo_diersoort_bepalen(
      leeftijd_jaar = "oud",
      geslacht = "V",
      datum_afkalven = NA
    ),
    ".*Leeftijd is niet numeriek. Diersoort kan niet bepaald worden."
  )
  expect_error(
    output <- rvo_diersoort_bepalen(
      leeftijd_jaar = "3",
      geslacht = "V",
      datum_afkalven = NA
    ),
    ".*Leeftijd is niet numeriek. Diersoort kan niet bepaald worden."
  )
})


test_that("rvo_diersoort_bepalen kan in een data.frame gebruikt worden", {
  df_input <- data.frame(
    leeftijd_jaren = c(0.2, 0.9, 3),
    geslacht = c("V", "V", "M"),
    datum_eerste_afkalven = NA,
    datum_import = NA
  )

  df_output <- df_input |>
    dplyr::mutate(
      diersoort = purrr::pmap_chr(
        .l = list(
          leeftijd_jaar = leeftijd_jaren,
          geslacht = geslacht,
          datum_afkalven = datum_eerste_afkalven,
          datum_import = datum_import
        ),
        .f = rvo_diersoort_bepalen,
        kalf_jongste = "tiny cow"
      )
    )
  expect_named(df_output, c(
    "leeftijd_jaren", "geslacht",
    "datum_eerste_afkalven", "datum_import",
    "diersoort"
  ))

  # kan ook met extra argumenten die niet in de data.frame zitten
  df_output <- df_input |>
    dplyr::mutate(
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
          kalf_jongste = "super tiny cow",
          kalf_oudste = "tiny cow",
          stier = "stier",
          max_maand_jongste_kalf = 8,
          max_maand_oudste_kalf = 12
        )
      )
    )

  expect_equal(
    df_output$diersoort,
    c("super tiny cow", "tiny cow", "stier")
  )
})



test_that("De rvo_slachting_type_bepalen functie retourneert de juiste output qua formaat", {
  
  df_input <-  data.frame(
    dier_op_locatie_id = sample(1:1E6, 3),
    dier_id = sample(1:1E6, 1),
    levens_nr = sample(1:1E6, 1),
    locatie_id = sample(1:1E6, 3),
    verblijfsduur_dagen = c(1, 300, 440),
    leeftijd_jaren = 5.3,
    diersoort = "Koeien",
    locatie_geschiedenis = c(0, 1, 2),
    bvg_type = c("SP", "VH", "VH"),
    is_locatie_biologisch = c(F, F, F)
  )
  
  df_output <- rvo_slachting_type_bepalen(df_input)
  expect_s3_class(df_output, "data.frame")
  # kolomnamen
  expect_named(df_output, c("dier_op_locatie_id", 
                            "diersoort",
                            "is_biologisch"))
  
  # De dier_op_locatie_id moet overeenkomen met het record van het slachthuis
  # (dus locatie_geschiedenis is 0)
  expect_equal(
    df_output$dier_op_locatie_id,
    df_input |> dplyr::filter(locatie_geschiedenis == 0) |> dplyr::pull(dier_op_locatie_id)
  )
  # En natuurlijk moet de diersoort nog steeds hetzelfde zijn
  expect_equal(
    df_output$diersoort,
    unique(df_input$diersoort)
  )
})  

test_that("Als slachthuis niet bio is, dan wordt de slachting niet bio", {
  
  # Eerste geval, slachthuis niet bio dus niet bio slachting
  df_input <-  data.frame(
    dier_op_locatie_id = sample(1:1E6, 3),
    dier_id = sample(1:1E6, 1),
    levens_nr = sample(1:1E6, 1),
    locatie_id = sample(1:1E6, 3),
    verblijfsduur_dagen = c(1, 300, 440),
    leeftijd_jaren = 5.3,
    diersoort = "Koeien",
    locatie_geschiedenis = c(0, 1, 2),
    bvg_type = c("SP", "VH", "VH"),
    is_locatie_biologisch = c(F, T, T)
  )
  
  df_output <- rvo_slachting_type_bepalen(df_input)
  
  expect_false(df_output$is_biologisch)
})  

test_that("Als het dier nooit op een biolocatie was, dan is de slachting ook niet bio", {
  
  df_input <-  data.frame(
    dier_op_locatie_id = sample(1:1E6, 3),
    dier_id = sample(1:1E6, 1),
    levens_nr = sample(1:1E6, 1),
    locatie_id = sample(1:1E6, 3),
    verblijfsduur_dagen = c(1, 300, 440),
    leeftijd_jaren = 0.9,
    diersoort = "Koeien",
    locatie_geschiedenis = c(0, 1, 2),
    bvg_type = c("SP", "VH", "VH"),
    is_locatie_biologisch = c(F, F, F)
  )
  
  df_output <- rvo_slachting_type_bepalen(df_input)
  
  expect_false(df_output$is_biologisch)
})  

test_that("Als een kalf een keer in een niet bio-locatie was, 
          dan is de slachting ook niet bio", {
            
            df_input <-  data.frame(
              dier_op_locatie_id = sample(1:1E6, 3),
              dier_id = sample(1:1E6, 1),
              levens_nr = sample(1:1E6, 1),
              locatie_id = sample(1:1E6, 3),
              verblijfsduur_dagen = c(1, 30, 200),
              leeftijd_jaren = 0.9,
              diersoort = "Kalveren 9-12 maanden",
              locatie_geschiedenis = c(0, 1, 2),
              bvg_type = c("SP", "VH", "VH"),
              is_locatie_biologisch = c(T, T, F)
            )
            
            df_output <- rvo_slachting_type_bepalen(df_input)
            
            expect_false(df_output$is_biologisch)
          })

test_that("Zelfs als het dier lange tijd in een biolocatie verbleef, 
          als er sprake is van een kort verblijf in een verblijfsplaat niet bio, 
          dan is de slachting ook niet bio", {
            
            df_input <-  data.frame(
              dier_op_locatie_id = sample(1:1E6, 4),
              dier_id = sample(1:1E6, 1),
              levens_nr = sample(1:1E6, 1),
              locatie_id = sample(1:1E6, 4),
              verblijfsduur_dagen = c(1, 4, 650, 700),
              leeftijd_jaren = 5,
              diersoort = "Stieren",
              locatie_geschiedenis = c(0, 1, 2, 3),
              bvg_type = c("SP", "VH", "VH", "VH"),
              is_locatie_biologisch = c(T, F, T, T)
            )
            
            df_output <- rvo_slachting_type_bepalen(df_input)
            
            expect_false(df_output$is_biologisch)
          })  

test_that("Als het slachthuis bio was, en alle andere verblijfsplaatsen zijn 
          ook bio en het dier zat er langer dan een jaar in, 
          dan is de slachting bio", {
            
            df_input <-  data.frame(
              dier_op_locatie_id = sample(1:1E6, 4),
              dier_id = sample(1:1E6, 1),
              levens_nr = sample(1:1E6, 1),
              locatie_id = sample(1:1E6, 4),
              verblijfsduur_dagen = c(1, 30, 650, 700),
              leeftijd_jaren = 5,
              diersoort = "Stieren",
              locatie_geschiedenis = c(0, 1, 2, 3),
              bvg_type = c("SP", "VH", "VH", "VH"),
              is_locatie_biologisch = c(T, T, T, T)
            )
            
            df_output <- rvo_slachting_type_bepalen(df_input)
            
            expect_true(df_output$is_biologisch)
          })

test_that("Als het slachthuis bio was, en alle verblijfsplaatsen
          in het laatste jaar bio waren, dan is de slachting bio. 
          Zelfs als een VH in het verleden niet bio was", {
            
            df_input <-  data.frame(
              dier_op_locatie_id = sample(1:1E6, 4),
              dier_id = sample(1:1E6, 1),
              levens_nr = sample(1:1E6, 1),
              locatie_id = sample(1:1E6, 4),
              verblijfsduur_dagen = c(1, 30, 650, 700),
              leeftijd_jaren = 5,
              diersoort = "Stieren",
              locatie_geschiedenis = c(0, 1, 2, 3),
              bvg_type = c("SP", "VH", "VH", "VH"),
              is_locatie_biologisch = c(T, T, T, F)
            )
            
            df_output <- rvo_slachting_type_bepalen(df_input)
            
            expect_true(df_output$is_biologisch)  
          })


test_that("Voor kalveren, als het slachthuis bio was 
          en alle verblijfsplaatsen waren bio, dan is de slachting bio", {
            
            df_input <-  data.frame(
              dier_op_locatie_id = sample(1:1E6, 4),
              dier_id = sample(1:1E6, 1),
              levens_nr = sample(1:1E6, 1),
              locatie_id = sample(1:1E6, 4),
              verblijfsduur_dagen = c(1, 30, 60, 35),
              leeftijd_jaren = 0.4,
              diersoort = "Kalf",
              locatie_geschiedenis = c(0, 1, 2, 3),
              bvg_type = c("SP", "VH", "VH", "VH"),
              is_locatie_biologisch = c(T, T, T, T)
            )
            
            df_output <- rvo_slachting_type_bepalen(df_input)
            
            expect_true(df_output$is_biologisch)
          })

test_that("Bij het bepalen van het slachting type wordt alleen rekening gehouden
          met slachthuizen en verblijfsplaatsen", {
            
            df_input <-  data.frame(
              dier_op_locatie_id = sample(1:1E6, 4),
              dier_id = sample(1:1E6, 1),
              levens_nr = sample(1:1E6, 1),
              locatie_id = sample(1:1E6, 4),
              verblijfsduur_dagen = c(1, 1, 650, 700),
              leeftijd_jaren = 5,
              diersoort = "Stieren",
              locatie_geschiedenis = c(0, 1, 2, 3),
              # the locatie #1 is een niet-bio verzamelplaats
              bvg_type = c("SP", "VP", "VH", "VH"),
              is_locatie_biologisch = c(T, F, T, T)
            )
            
            df_output <- rvo_slachting_type_bepalen(df_input)
            # ook al was de verzamelplaats niet bio, de slachting is toch bio
            # omdat we alleen rekening houden met slachthuizen en
            # verblijfsplaatsen
            expect_true(df_output$is_biologisch)  
          })


test_that("Als levens_nr ontbreekt, slachting is niet bio", {
  
  df_input <-  data.frame(
    dier_op_locatie_id = sample(1:1E6, 4),
    dier_id = sample(1:1E6, 1),
    levens_nr = NA,
    locatie_id = sample(1:1E6, 4),
    verblijfsduur_dagen = c(1, 1, 650, 700),
    leeftijd_jaren = 5,
    diersoort = "Stieren",
    locatie_geschiedenis = c(0, 1, 2, 3),
    bvg_type = c("SP", "VH", "VH", "VH"),
    is_locatie_biologisch = c(T, F, T, T)
  )
  
  df_output <- rvo_slachting_type_bepalen(df_input)
  expect_false(df_output$is_biologisch) 
  
})


test_that("Als er geen SP is, slachting is niet bio", {
  
  df_input <-  data.frame(
    dier_op_locatie_id = sample(1:1E6, 4),
    dier_id = sample(1:1E6, 1),
    levens_nr = sample(1:1E6, 1),
    locatie_id = sample(1:1E6, 4),
    verblijfsduur_dagen = c(1, 1, 650, 700),
    leeftijd_jaren = 5,
    diersoort = "Stieren",
    locatie_geschiedenis = c(0, 1, 2, 3),
    bvg_type = c("VH", "VH", "VH", "VH"),
    is_locatie_biologisch = c(T, F, T, T)
  )
  
  df_output <- rvo_slachting_type_bepalen(df_input)
  expect_false(df_output$is_biologisch) 
})


test_that("Als bvg_type van SP ontbreekt, 
          de slachting wordt niet verwerkt", {
            
            # dit verschilt van het vorige geval omdat we de SP-locatie kwijtraken als we filteren op geldige locaties. 
            # Ik weet niet zeker of dit is wat er zou moeten gebeuren.
            df_input <-  data.frame(
              dier_op_locatie_id = sample(1:1E6, 4),
              dier_id = sample(1:1E6, 1),
              levens_nr = sample(1:1E6, 1),
              locatie_id = sample(1:1E6, 4),
              verblijfsduur_dagen = c(1, 1, 650, 700),
              leeftijd_jaren = 5,
              diersoort = "Stieren",
              locatie_geschiedenis = c(0, 1, 2, 3),
              bvg_type = c(NA, "VH", "VH", "VH"),
              is_locatie_biologisch = c(T, F, T, T)
            )
            
            df_output <- rvo_slachting_type_bepalen(df_input)
            expect_equal(nrow(df_output), 0)
          })

test_that("maak_verwijderde_dieren_ongeldig functie:
          Een dier is aanwezig in levering maar niet in levering van volgende maand
          ", {
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
              n_regels = 5,
              eerste_datum_slacht = "2024-03-01",
              laatste_datum_slacht = "2024-04-01"
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
            kv2_datum <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
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
              vwmd = "202403",
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
            
            # nu maken we een nieuwe dataset voor de nieuwe levering
            # we willen simuleren 
            
            idx_slacht_te_verwijderen <- 1:2
            
            df_verwijderd_dier <- df_slachtingen |> 
              dplyr::filter(dplyr::row_number() %in% idx_slacht_te_verwijderen) 
            
            set.seed(20)
            synthetische_dataset <- maak_synthetische_dataset(
              n_locaties = 10,
              biologische_fractie = 0,
              straatnamen = maak_straatnamen(),
              n_regels = 2,
              eerste_datum_slacht = "2024-04-01",
              laatste_datum_slacht = "2024-05-01"
            )
            
            pad_rvo_slachtingen <- withr::local_tempfile(fileext = ".csv")
            readr::write_csv(synthetische_dataset$slachtingen,
                             file = pad_rvo_slachtingen
            )
            df_slachtingen_nieuwe_levering <- inl_lees_csv_bestand_met_kolommen_config(
              pad =  pad_rvo_slachtingen,
              kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
              inlezen_functie = readr::read_csv
            ) |> 
              # hier voegen we toe de slachtingen van vorige levering behalve
              # twee slachtingens
              dplyr::bind_rows(
                df_slachtingen |> 
                  dplyr::filter(!(dplyr::row_number() %in% idx_slacht_te_verwijderen))
              )
            
            expect_message(
              maak_verwijderde_dieren_ongeldig(
                df_dieren_huidige_levering = df_slachtingen_nieuwe_levering,
                con = con,
                vwmd = "202404",
                schema = test_schema,
                datum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
              )  
            )
            
            df_db_dieren <- dplyr::tbl(
              con, 
              dbplyr::in_schema(test_schema, "tbl_dieren_kv2_kv3")) |>
              dplyr::collect() 
            
            expect_equal(
              df_db_dieren |> 
                dplyr::filter(!is.na(eind_datum)) |> 
                dplyr::select(levens_nr, eld_code),
              df_verwijderd_dier |> 
                dplyr::select(levens_nr, eld_code)
            )
            # de slachtingen tabel moet ook een eind_datum krijgen
            
            df_db_dieren_op_locaties <- dplyr::tbl(
              con, 
              dbplyr::in_schema(test_schema, "tbl_dieren_op_locaties_kv2_kv3")) |>
              dplyr::collect() 
            
            df_db_slachtingen <- dplyr::tbl(
              con,
              dbplyr::in_schema(test_schema, "tbl_slachtingen_kv3")
            ) |> 
              dplyr::collect()
            
            eind_datum_slachtingen <- df_db_dieren |> 
              dplyr::filter(!is.na(eind_datum)) |> 
              dplyr::select(dier_id) |> 
              dplyr::left_join(
                df_db_dieren_op_locaties |> 
                  dplyr::select(dier_op_locatie_id, dier_id),
                by = "dier_id"
              ) |> 
              dplyr::left_join(
                df_db_slachtingen,
                by = "dier_op_locatie_id"
              ) |> 
              dplyr::pull(eind_datum)
            
            expect_true(all(is.na(eind_datum_slachtingen)))
            
            DBI::dbDisconnect(con)
          })
