# source het te testen script
root_map <- here::here()
tst_map <- file.path(root_map, "unittesten")
src_map <- file.path(root_map, 'src')
source(file.path(src_map, "outputbestand_eurostat_sdmx.R"))
source(file.path(src_map, "ophalen_outputdata.R"))


library(testthat)


##############################################################################
context("oph_leid_aggregaten_af()")

test_that("de output aggegraten correct tot stand komen", {
  
  # Pluimvee totaal (Vleeskuikens, Overige kippen, Eenden, Kalkoenen, Duiven, 
  # Fazanten, Ganzen, Parelhoenders, Patrijzen, Struisvogels)
  
  kalkoenen_aant <- 1
  kalkoenen_totg <- 10 
  andere_categorieen <- c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd",
                          "Kalveren 8-12 mnd","Lammeren","Volwassen schapen",
                          "Varkens","Eenhoevige dieren","Geiten",
                          "Vleeskuikens","Overige kippen","Eenden",
                          "Duiven","Fazanten","Ganzen","Parelhoenders",
                          "Patrijzen","Struisvogels")
  df_input <- data.frame(
    stringsAsFactors = FALSE,
    dcat = c(
      "Kalkoenen", andere_categorieen
    ),
    vsmd1_aant = c(kalkoenen_aant, rep(0, length(andere_categorieen))),
    vsmd1_totg = c(kalkoenen_totg, rep(0, length(andere_categorieen))),
    vsmd2_aant = NA,
    vsmd2_totg = NA,
    vsmd3_aant = NA,
    vsmd3_totg = NA,
    vsmd4_aant = NA,
    vsmd4_totg = NA
  )
  
  res <- oph_leid_aggregaten_af(df_input)
  
  expect_identical(res["Pluimvee totaal", "vsmd1_aant"], kalkoenen_aant)
  expect_identical(res["Pluimvee totaal", "vsmd1_totg"], kalkoenen_totg)
  
})


##############################################################################
context("schrijf_sdmx_csv()")

# goedpad
test_that("de structuur van de csv voldoet aan de SDMX standaard", {
  
  # definieer data
  df_input <- data.frame(
    DATAFLOW = "ESTAT:ANI_SLAUGHT_M(20210826.0)",
    FREQ = "M",
    REF_AREA = "NL",
    UNIT = c("THS_HD", "THS_T"),
    MEAT = "B1000",
    MEATITEM = "SL",
    TIME_PERIOD = "2018-03", 
    OBS_VALUE = c(1.999, 109.999), 
    OBS_STATUS = c("P", NA_character_), 
    CONF_STATUS = c(NA_character_, "C"), 
    OBS_COMMENT = NA
    )
  path <- withr::local_tempfile(
    pattern = "eurostat_sdmx.csv"
  )
  
  schrijf_sdmx_csv(df_input, path)
  
  
  ######### Het bestand bestaat ##############################################
  
  expect_true(file.exists(path))
  
  
  ######### Het tabel is geschreven met: #####################################
  #' waarden gescheiden met ; 
  #' decimalen sep is punt
  #' zonder rij indexes
  
  res_read <- read.table(
    path,
    header = TRUE,
    sep = ";",
    dec = ".",
    fileEncoding = "UTF-8",
    na.strings = ""
  )
  
  expect_identical(res_read, df_input)
  
  
  ######### eol is carriage return én Line feed (CRLF, oftewel \r\n) ########
  
  file_bytes <- readBin(path, file.info(path)$size, what = "raw")
  last_two_file_bytes <- file_bytes[c(length(file_bytes) - 1, length(file_bytes))]
  crlf_bytes <- charToRaw("\r\n")
  
  expect_identical(last_two_file_bytes, crlf_bytes)

})

# foutpad
test_that("de geschreven sdmx bevat geen BOM (Byte Order Mark) in encoding", {
  
  # definieer data
  df_input <- data.frame(
    DATAFLOW = "ESTAT:ANI_SLAUGHT_M(20210826.0)",
    FREQ = "M",
    REF_AREA = "NL",
    UNIT = c("THS_HD", "THS_T"),
    MEAT = "B1000",
    MEATITEM = "SL",
    TIME_PERIOD = "2018-03", 
    OBS_VALUE = c(1.999, 109.999), 
    OBS_STATUS = c("P", NA_character_), 
    CONF_STATUS = c(NA_character_, "C"), 
    OBS_COMMENT = NA
  )
  path_bom <- withr::local_tempfile(
    pattern = "eurostat_bom.csv"
  )
  
  # schrijf een file met BOM, then write the "data" eurostat data
  connection <- file(path_bom, 'w')
  writeChar("\ufeff", connection, eos = NULL)
  write.table(df_input,
              file = connection,
              sep = ";",
              eol = "\n",
              dec = ".",
              row.name = FALSE,
              fileEncoding = "UTF-8"
  )
  close(connection)
  
  path_no_bom <- withr::local_tempfile(
    pattern = "eurostat_no_bom.csv"
  )
  
  schrijf_sdmx_csv(df_input, path_no_bom)
  
  # lees het bom bestand in
  connection <- file(path_bom, "rb")
  bom_bytes <- readBin(connection, "raw", 3)
  close(connection)
  
  # lees no bom bestand in
  connection <- file(path_no_bom, "rb")
  no_bom_bytes <- readBin(connection, "raw", 3)
  close(connection)
  
  expect_false(all(bom_bytes == no_bom_bytes))
  
})


##############################################################################
context("maak_eurostat_sdmx_per_jaar()")

# goedpad

test_that("de filenaam correct wordt geschreven wanneer één jaar wordt verwerkt", {
  
  df_input <- data.frame(dcat=c("Stieren", "Geiten", "Kalveren 0-8 mnd"),
                         vsmd1_aant=1999,
                         vsmd2_aant=1000,
                         vsmd3_aant=1000,
                         vsmd4_aant=1000,
                         vsmd1_totg=109999,
                         vsmd2_totg=100000,
                         vsmd3_totg=100000,
                         vsmd4_totg=100000
  )
  
  jrmnd <- list(mnd1="201803", mnd2="201804", mnd3="201805", mnd4="201806",
                vwmd="201806", vvwmd="201805")
  decimalen <- c(aantal=3, gewicht=3)

  tmpdir <- withr::local_tempdir(
    pattern = "eurostat_zonder_jaarovergang"
  )
  
  waarde_dataflow <- "ESTAT:ANIP_MTSLS_M(1.0)"
  waarde_freq <- "M"

  maak_eurostat_sdmx_per_jaar(df_input, jrmnd, tmpdir,
    waarde_dataflow = waarde_dataflow,
    waarde_freq = waarde_freq,
    afronden_decimalen = decimalen 
  )

  
  ######### er wordt één file geschreven #####################################
  
  files <- list.files(tmpdir)
  res <- length(files)
  verw <- 1
  
  expect_equal(res, verw)
  
})


test_that("de files correct worden geschreven voor overgangsjaren", {
  
  df_input <- data.frame(dcat=c("Stieren", "Geiten", "Kalveren 0-8 mnd"),
                         vsmd1_aant=1999,
                         vsmd2_aant=1000,
                         vsmd3_aant=1000,
                         vsmd4_aant=1000,
                         vsmd1_totg=109999,
                         vsmd2_totg=100000,
                         vsmd3_totg=100000,
                         vsmd4_totg=100000
  )
  
  jrmnd <- list(mnd1="201811", mnd2="201812", mnd3="201901", mnd4="201902",
                vwmd="201902", vvwmd="201901")
  decimalen <- c(aantal=3, gewicht=3)
  
  tmpdir <- withr::local_tempdir(
    pattern = "eurostat_jaarovergang"
  )
  
  waarde_dataflow <- "ESTAT:ANIP_MTSLS_M(1.0)"
  waarde_freq <- "M"
  
  maak_eurostat_sdmx_per_jaar(df_input, jrmnd, tmpdir,
    waarde_dataflow = waarde_dataflow,
    waarde_freq = waarde_freq,
    afronden_decimalen = decimalen 
  )
  
  ######### er worden twee files geschreven #################################
  
  filenames <- list.files(tmpdir)
  res <- length(filenames)
  verw <- 2
  expect_equal(res, verw)
  
  
  ######### de files beide verschillende jaren in de naam ###################
  
  filenames <- list.files(tmpdir)
  is_2018 <- stringr::str_detect(filenames, "2018")
  is_2019 <- stringr::str_detect(filenames, "2019")
  
  expect_true(sum(is_2018 = TRUE) == 1)
  expect_true(sum(is_2019 = TRUE) == 1)
  
})


##############################################################################
context("construct_dsnc()")

# goedpad
test_that("de filenaam correct wordt aangemaakt", {
  
  ######### de file heeft de verwachte naam ##################################
  
  jrmnd <- list(mnd4="201806")
  res <- construct_dsnc(jrmnd)
  verw <- "ANIP_MTSLS_M_NL_2018_0006.csv"
  
  expect_identical(res, verw)
  
  
  ######### de 0-padding voor een maand in double digits klopt ###############
  
  jrmnd <- list(mnd4="201812")
  res <- construct_dsnc(jrmnd)
  verw <- "ANIP_MTSLS_M_NL_2018_0012.csv"
  
  expect_identical(res, verw)

})


##############################################################################
context("leid_data_af_eurostat()")

# goedpad
test_that("de juiste kolomnamen aanwezig zijn and in uppercase", {

  # definieer data
  df_input <- data.frame(dcat=c("Stieren", "Geiten", "Kalveren 0-8 mnd"),
                         vsmd1_aant=1999,
                         vsmd2_aant=1000,
                         vsmd3_aant=1000,
                         vsmd4_aant=1000,
                         vsmd1_totg=109999,
                         vsmd2_totg=100000,
                         vsmd3_totg=100000,
                         vsmd4_totg=100000
  )
  jrmnd <- list(mnd1="201803", mnd2="201804", mnd3="201805", mnd4="201806",
                vwmd="201806", vvwmd="201805")
  decimalen <- c(aantal=3, gewicht=3)

  ######### kolomnamen zijn in uppercase (voor  arrange functie) ############
  
  expect_no_error(leid_data_af_eurostat(df_input, jrmnd, 
                                        afronden_decimalen=decimalen),
                  message = "object '(A-Z_)' not found")
  
  
  ######### verwachte kolomnamen zijn aanwezig #############################
  
  verw <- c(
    "DATAFLOW", "FREQ", "REF_AREA", "AGRIPROD", "STAT_CHAR",
    "UNIT_MEASURE", "TIME_PERIOD", "OBS_VALUE", "OBS_STATUS", "CONF_STATUS",
    "OBS_COMMENT", "OBS_PERIOD", "UNIT_MULT", "DECIMALS"
  )
  res <- leid_data_af_eurostat(df_input, jrmnd)
  expect_identical(object=names(res), expected=verw)
  
})


test_that("de waarden van de kolommen kloppen", {
  
  # definieer data
  df_input <- data.frame(dcat=c("Runderen totaal", "Kalveren 0-8 mnd"),
                         vsmd1_aant=1000,
                         vsmd2_aant=1000,
                         vsmd1_totg=1000000,
                         vsmd2_totg=1000000
  )
  jrmnd <- list(mnd1="201803", mnd2="201804", mnd3="201805", mnd4="201806",
                vwmd="201806", vvwmd="201805")
  decimalen <- c(aantal=3, gewicht=3)
  
  # definieer verwachte tabel
  verw <- tibble::tribble(
    ~AGRIPROD,   ~TIME_PERIOD, ~UNIT_MEASURE,    ~OBS_VALUE,
    "B1000", "2018-03",    "THS_HD", "1.000",
    "B1000", "2018-03",    "THS_T",  "1.000",
    "B1000", "2018-04",    "THS_HD", "1.000",
    "B1000", "2018-04",    "THS_T",  "1.000",
    "B1110", "2018-03",    "THS_HD", "1.000",
    "B1110", "2018-03",    "THS_T",  "1.000",
    "B1110", "2018-04",    "THS_HD", "1.000",
    "B1110", "2018-04",    "THS_T",  "1.000"
  )
  
  
  res <- leid_data_af_eurostat(df_input, 
                               jrmnd, 
                               afronden_decimalen=decimalen)[1:8, ]

  # onderstaande kolommen mogen geen NA hebben (zijn constanten)
  expect_true(sum(is.na(res[, "DATAFLOW"])) == 0)
  expect_true(sum(is.na(res[, "FREQ"])) == 0)
  expect_true(sum(is.na(res[, "REF_AREA"])) == 0)
  expect_true(sum(is.na(res[, "STAT_CHAR"])) == 0)
  expect_true(sum(is.na(res[, "DECIMALS"])) == 0)
  
  # onderstaande kolommen mogen geen NA hebben (bewerkt in functie)
  expect_true(sum(is.na(res[, "UNIT_MEASURE"])) == 0)
  expect_true(sum(is.na(res[, "AGRIPROD"])) == 0)
  expect_true(sum(is.na(res[, "TIME_PERIOD"])) == 0)
  expect_true(sum(is.na(res[, "OBS_VALUE"])) == 0)
  
  # rijvolgorde (agriprod, time_period, unit)
  expect_identical(object=res[, "AGRIPROD"], expected=verw[, "AGRIPROD"])
  expect_identical(object=res[, "TIME_PERIOD"], expected=verw[, "TIME_PERIOD"])
  expect_identical(object=res[, "UNIT_MEASURE"], expected=verw[, "UNIT_MEASURE"])
  
  # OBS_VALUES: 3 decimals after komma for tons and heads
  expect_identical(object=res[, "OBS_VALUE"], expected = verw[, "OBS_VALUE"])
  
  # TIME_PERIOD: jaartallen zijn in format YYYY-mm
  expect_true(
    stringr::str_detect(res[1, "TIME_PERIOD"], "[0-9]{4}-[0-9]{2}$")
  )
  
  #' NOTE: check het patroon van P-vlag alleen als SPEK niet voor 
  #' jaarlijkse cijfers wordt gebruikt
  # verw_aantal_p <- 
  
  # OBS_STATUS: is alleen NA of P; nul onverwachte waarden
  expect_true(
    sum(!(res$OBS_STATUS %in% c("P", NA_character_))) == 0
  )
  
  # CONF_STATUS: is alleen NA of C; nul onverwachte waarden
  expect_true(
    sum(!(res$CONF_STATUS %in% c("C", NA_character_))) == 0
  )
  
  # OBS_COMMENT: alle waarden moeten NA zijn
  expect_true(all(is.na(res[, "OBS_COMMENT"])))
  expect_true(all(is.na(res[, "OBS_PERIOD"])))
  expect_true(all(is.na(res[, "UNIT_MULT"])))
  
})

test_that("alle verwachte diercodes staan in de gemaakte tabel", {
  # definieer data
  df_input <- data.frame(dcat=c("Runderen totaal", "Kalveren 0-8 mnd"),
                         vsmd1_aant=1999,
                         vsmd2_aant=1000,
                         vsmd1_totg=109999,
                         vsmd2_totg=100000
  )
  jrmnd <- list(mnd1="201803", mnd2="201804", mnd3="201805", mnd4="201806",
                vwmd="201806", vvwmd="201805")
  
  verw <- c(
    "B1220", "B1210", "B1230", "B1240", "B1110", "B1120", "B1000",
    "B4110", "B4190", "B4100", "B4200", "B3100", "B5000",
    "B7200", "B7300", "B7100"
    )

  res <- leid_data_af_eurostat(df_input, jrmnd)
  unieke_codes <- unique(res$AGRIPROD)
  
  # het aantal unieke codes komt overeen
  expect_true(
    length(unieke_codes) == length(verw)
  )
  
  # de inhoud van de codes komt overeen
  expect_true(
    sum(unieke_codes %in% verw) == length(verw)
  )
  
})
