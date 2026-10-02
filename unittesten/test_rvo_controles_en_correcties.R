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


test_that("een_regel_toepassen functie retournneert de juiste objecten", {
  regel <- validate::validator(
    niet_uniek = is_unique(levens_nr)
  )

  validate::description(regel)[[1]] <- "levens_nr is niet uniek"

  df_input <- data.frame(
    levens_nr = c(1, 1)
  )

  df_output <- een_regel_toepassen(
    regel = regel,
    df = df_input,
    alleen_gebruikt_kolommen = FALSE
  )

  expect_s3_class(df_output, "tbl")
  expect_named(df_output, c("description", "foutive_rijen", "controles"))
  expect_s3_class(df_output$controles[[1]], "data.frame")
  expect_s3_class(df_output$foutive_rijen[[1]], "data.frame")
  expect_type(df_output$description, "character")
  expect_named(
    df_output$controles[[1]],
    c(
      "name", "items", "passes", "fails", "nNA",
      "error", "warning", "expression"
    )
  )
  expect_named(
    df_output$foutive_rijen[[1]],
    c("rij_idx", "levens_nr")
  )
})

test_that("een_regel_toepassen functie retournneert de juiste kolommen", {
  
  regel <- validate::validator(
    ontbrekende_geboorte_datum = !is.na(datum_geboorte)
  )

  validate::description(regel)[[1]] <- "Ontbrekende geboorte datum"

  df_input <- data.frame(
    levens_nr = c(1, 2),
    eld_code = c("NL", "NL"),
    datum_geboorte = NA,
    een_andere_kolom = 3,
    nog_een_andere_kolom = 4
  )

  df_output <- een_regel_toepassen(
    regel = regel,
    df = df_input,
    alleen_gebruikt_kolommen = TRUE,
    id_kolommen = c("levens_nr", "eld_code")
  )

  expect_named(
    df_output$foutive_rijen[[1]],
    c("rij_idx", "levens_nr", "eld_code",  "datum_geboorte")
  )


  df_output <- een_regel_toepassen(
    regel = regel,
    df = df_input,
    alleen_gebruikt_kolommen = FALSE,
    id_kolommen = c("levens_nr", "eld_code")
  )

  expect_named(
    df_output$foutive_rijen[[1]],
    c(
      "rij_idx", "levens_nr", "eld_code", "datum_geboorte",
      "een_andere_kolom", "nog_een_andere_kolom"
    )
  )
})

test_that("een_regel_toepassen functie vindt de rijen met fouten", {
  regel <- validate::validator(
    ontbrekende_geboorte_datum = !is.na(datum_geboorte)
  )

  validate::description(regel)[[1]] <- "Ontbrekende geboorte datum"

  df_input <- data.frame(
    levens_nr = c(1, 2),
    eld_code = c("NL", "NL"),
    datum_geboorte = c(NA, "01-01-2022"),
    een_andere_kolom = 3,
    nog_een_andere_kolom = 4
  )

  df_output <- een_regel_toepassen(
    regel = regel,
    df = df_input,
    alleen_gebruikt_kolommen = TRUE,
    id_kolommen = c("levens_nr", "eld_code")
  )

  df_foutive_rijen <- df_output |>
    dplyr::select(foutive_rijen) |>
    tidyr::unnest(foutive_rijen)

  expect_equal(
    nrow(df_foutive_rijen),
    1
  )
  expect_equal(
    df_foutive_rijen$rij_idx,
    1
  )
})


test_that("controles_toepassen functie kan meerdere regels toepassen", {
  regels <- validate::validator(
    niet_uniek = is_unique(levens_nr),
    ontbrekende_geboorte_datum = !is.na(datum_geboorte)
  )

  descs <- c(
    "levens_nr is niet uniek",
    "Ontbrekende geboorte datum"
  )
  names(descs) <- names(regels)

  validate::description(regels)[[1]] <- descs[1]
  validate::description(regels)[[2]] <- descs[2]

  df_input <- data.frame(
    levens_nr = c(1, 1),
    eld_code = c("NL", "NL"),
    datum_geboorte = c(NA, "01-01-2024"),
    een_andere_kolom = 3,
    nog_een_andere_kolom = 4
  )

  df_output <- controles_toepassen(
    regel = regels,
    df = df_input,
    alleen_gebruikt_kolommen = TRUE,
    id_kolommen = c("levens_nr", "eld_code")
  )

  # beide regels zijn in de output
  expect_equal(
    df_output$description,
    descs
  )
  # De juiste rijen worden gevonden voor elke regel
  expect_equal(
    df_output$foutive_rijen[[1]]$rij_idx,
    c(1, 2)
  )

  expect_equal(
    df_output$foutive_rijen[[2]]$rij_idx,
    c(1)
  )
})

test_that("log html bestand is gemaakt", {
  regels <- validate::validator(
    niet_uniek = is_unique(levens_nr),
    ontbrekende_geboorte_datum = !is.na(datum_geboorte)
  )

  descs <- c(
    "levens_nr is niet uniek",
    "Ontbrekende geboorte datum"
  )
  names(descs) <- names(regels)

  validate::description(regels)[[1]] <- descs[1]
  validate::description(regels)[[2]] <- descs[2]

  df_input <- data.frame(
    levens_nr = c(1, 1),
    datum_geboorte = c(NA, "01-01-2024"),
    een_andere_kolom = 3,
    nog_een_andere_kolom = 4
  )

  pad_tempmap <- withr::local_tempdir()

  controles <- controles_log(
    datasetnaam = "dataset",
    pad_inputbestand = "pad/naar/input/bestand.csv",
    pad_sjablon = file.path(sjablonen_map, "kv2_controles.qmd"),
    pad_outputmap = pad_tempmap,
    pad_tempmap = pad_tempmap,
    regels = regels,
    df = df_input,
    id_kolommen = c("levens_nr", "eld_code"),
    alleen_gebruikt_kolommen = FALSE
  )
  # Hier testen we alleen of het html-bestand is gemaakt, maar we kunnen
  # waarschijnlijk testen of het bestand eruit ziet zoals we verwachten met
  # snapshots
  expect_true(
    file.exists(
      controles$pad_logbestand
    )
  )
})

test_that("correcties_toepassen functie:
          Correcties en rapport wordt niet uitgevoerd als geen fout in de input data staat", {
  App <- yaml::yaml.load_file(
    input = file.path(
      testdata_map,
      "voorbeeld_config.ini"
    ),
    eval.expr = TRUE
  )

  df <- data.frame(
    x = 1,
    y = 3
  )
  correcties <- dcmodify::modifier(
    if (is.na(y)) y <- 42
  )

  tempmap <- withr::local_tempdir()

  output <- correcties_toepassen(
    df = df, correcties = correcties,
    pad_sjablon = file.path(sjablonen_map, "kv3_correcties.qmd"),
    pad_outputmap = tempmap,
    datasetnaam = "test"
  )

  expect_false(output$correcties)
  expect_s3_class(output$df, "data.frame")
  expect_equal(output$df$y, 3)
  expect_null(output$rapportpad)
})

test_that("correcties_toepassen functie:
          Correcties en rapport wordt uitgevoerd als een fout in de input data staat", {
  App <- yaml::yaml.load_file(
    input = file.path(
      testdata_map,
      "voorbeeld_config.ini"
    ),
    eval.expr = TRUE
  )

  df <- data.frame(
    x = c(1,2,3,4,5,6,7,8,9,10),
    y = c(NA,1,NA,NA,1,1,1,1,1,1)
  )
  correcties <- dcmodify::modifier(
    if (is.na(y)) y <- 42
  )

  tempmap <- withr::local_tempdir()

  output <- correcties_toepassen(
    df = df, correcties = correcties,
    pad_sjablon = file.path(sjablonen_map, "kv3_correcties.qmd"),
    pad_outputmap = tempmap,
    datasetnaam = "test"
  )

  expect_true(output$correcties)
  expect_s3_class(output$df, "data.frame")
  expect_setequal(output$df$x, c(1,3,4))
  expect_setequal(output$df$y, c(42,42,42))
  expect_true(file.exists(output$rapportpad))
})

test_that("rvo_locaties_koppeling_log functie:
          de koppeling rapport wordt gemaakt", {
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
    skalnr_lbt = "343434"
  )
  
  df_skal <- data.frame(
    huisnummer                   = c("bellenhof 570D-456S", ""),
    postcode                     = c("2222BB", "2222FR"),
    skalnummer                   = c("343434", "454545"),  
    resultaat_certificatie       = c("Biologisch"),
    datum_certificatie_geldigheid = c("31-12-2022"),
    datum_geldig_vanaf           = c("01-01-2024"),
    hoofdsbi                     = c("17.6")
  )
  
  koppeling_output <- rvo_locaties_koppeling(
    df_locaties_kv3 = df_locaties,
    df_landbouwtelling = df_landbouwtelling,
    df_verblijfsplaatsen = df_verblijfsplaatsen,
    df_skal = df_skal
  )

  App <- yaml::yaml.load_file(
    input = file.path(
      testdata_map,
      "voorbeeld_config.ini"
    ),
    eval.expr = TRUE
  )

  tempmap <- withr::local_tempdir()

  koppeling_rapport <- rvo_locaties_koppeling_log(
    df_locaties_kv3 = df_locaties,
    df_locaties_zonder_adres = koppeling_output$locaties_zonder_adres,
    df_locaties_zonder_relnr = koppeling_output$locaties_zonder_relnr,
    df_locaties_meerdere_relnrs = koppeling_output$locaties_meerdere_relnrs,
    df_locaties_met_relnr_zonder_adres = koppeling_output$locaties_met_relnr_zonder_adres,
    df_locaties_zonder_adres_voor_norm = koppeling_output$locaties_zonder_adres_voor_norm,
    df_locaties_zonder_adres_vanvege_norm = koppeling_output$locaties_zonder_adres_vanvege_norm,
    df_locaties_problematisch_norm_adressen = koppeling_output$locaties_problematisch_norm_adressen,
    df_locaties_zonder_skal = koppeling_output$locaties_zonder_skal,
    df_skal_zonder_adres = koppeling_output$skal_zonder_adres,
    pad_sjablon = file.path(sjablonen_map, "kv3_koppelingen.qmd"),
    pad_outputmap = tempmap
  )
  # we testen alleen of het bestand is gemaakt, niet de inhoud
  expect_true(file.exists(koppeling_rapport))
})
