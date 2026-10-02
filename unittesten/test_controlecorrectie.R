#' Naam        : test_controlecorrectie.R
#' Auteur(s)   : Hugo Pineda Hernandez (HPEZ) en Valerie Sawirja (VSAA)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module voert de unittesten uit voor de volgende functies:
#'             : verzamel_data_ctcr_rdvl()
#'             : selecteer_frame_rdvl()
#'             : selecteer_frame_wtvl()
#'             : stel_samen_ctcr_rdvl()
#'             : voeg_rvo_runderen_bij_nvwa()
#'             : maak_ctcr_verschil_rvo_nvwa()

library(openxlsx)
library(lubridate)
library(readxl)
library(plyr)
library(logging)
library(RODBC)
library(dplyr)

# Dit bestand is geen onafhankelijk testbestand. De variabelen die zijn
# gedefinieerd in het 'voer_testen_uit.R'-bestand zijn nodig om de tests in dit
# bestand uit te voeren

source(file.path(src_map, "algemeen.R"))
source(file.path(src_map, "afleiden_indicatoren.R"))
source(file.path(src_map, "bijschatten.R"))
source(file.path(src_map, "ctcrbestand_aantallen_roodvlees.R"))
source(file.path(src_map, "inlezen.R"))
source(file.path(src_map, "database.R"))
source(file.path(src_map, "synthetische_data.R"))
source(file.path(src_map, "opslaan.R"))


test_that("dataverzameling uit DB en levering goed gaat in de eindsituatie", {
  ########### SITUATIE 3: RVO in database voor een jaar, RVO in levering ------- 
  
  # maak een verbinding met de ont database
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  
  DBI::dbExecute(con, qry)
  
  # nu gaan we de database vullen met behulp van de syndata functies om de
  # benodigde gegevens toe te voegen om één maand te verwerken
  verw_maand <- "202404"
  start_rvo <- "202304" # één jaar voor de verwerkingsmaand
  
  # Hiermee worden ook de invoerbestanden voor een maand aangemaakt, dus we doen
  # dit in een tijdelijke map (die wordt verwijderd nadat de test is voltooid)
  temp_map <- withr::local_tempdir()
  
  # we moeten de jaar- en maandmap maken in de tijdelijke map
  file.path(temp_map, "2024", verw_maand) |> 
    dir.create(temp_map, recursive = TRUE)
  
  paden <- synthetiseer_spek_bestanden_e_ini_db(
    verw_maand = "202404", 
    con = con,
    input_map = temp_map, 
    db_schema = "unittest",
    n_regels = 5,
    rvo_slachtingen_kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    is_bio = TRUE,
    start_rvo = start_rvo) 
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  maanden_vorige_vm  <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3)
  maanden_huidige_vm <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4)
  
  # definieren koppeltabel
  df_koppelen_vvwm <- data.frame(
    maand_id = c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3"),
    vsmd = maanden_vorige_vm
  )
  
  res <- verzamel_data_ctcr_rdvl(jrmnd = jrmnd,
                                 levering_bst_nvwa = paden$rdvl_lev_pad, 
                                 levering_bst_rvo = paden$rdvl_rvo_lev_pad,
                                 maanden_huidige_vm = maanden_huidige_vm)
  
  
  
  ###### inhoud draaitabel komt uit database en levering -----------------------
  
  # DEEL 1: wat er in de databse zit
  inhoud_database <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl")) |> 
    dplyr::filter(vwmd == jrmnd$vvwmd) |>
    dplyr::select(-aant_ruw) |>
    dplyr::collect()
  
  # resultaat uit functie (database deel)
  res_data <- res$data
  res_data_uit_db <- dplyr::select(res_data, slnm, wrkp, dsrt, vvm_mnd1, vvm_mnd2, vvm_mnd3, is_biologisch) |>
    tidyr::pivot_longer(cols = c(vvm_mnd1, vvm_mnd2, vvm_mnd3),
                        names_to = "maand_id", values_to = "aant_gaaf") |>
    dplyr::left_join(df_koppelen_vvwm, by = "maand_id") |>
    dplyr::select(-maand_id)
  
  # vergeleken met de DB moet resultaat uit de functie extra rijen hebben: 
  # RVO dcats zonder aant_gaaf waarden.
  # (komt doordat RVO waarden alleen in de levering zitten, nog niet in DB)
  
  rvo_dcats <- c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd", "Kalveren 8-12 mnd")
  
  # niet-rund roodvlees komt overeen
  df_res_verschil <- dplyr::anti_join(
    res_data_uit_db |> dplyr::filter(!dsrt %in% rvo_dcats),
    inhoud_database |> dplyr::filter(!dsrt %in% rvo_dcats),
    by = dplyr::join_by(vsmd, wrkp, slnm, dsrt, is_biologisch)
  ) |> 
    nrow() |> 
    expect_equal(0)
  
  # mogelijk verschil in afwezigheid van "is_biologisch" uit vwmd1-vwmd3
  # selecteer runderen & maanden die in de draaitabel komen
  df_res_verschil <- dplyr::full_join(
    res_data_uit_db |> 
      dplyr::filter(dsrt %in% rvo_dcats) |>
      dplyr::filter(vsmd %in% maanden_huidige_vm), 
    inhoud_database |> 
      dplyr::filter(dsrt %in% rvo_dcats) |>
      dplyr::filter(vsmd %in% maanden_huidige_vm),
    by = dplyr::join_by(vsmd, wrkp, slnm, dsrt, is_biologisch)
  ) 
  expect_equal(df_res_verschil$aant_gaaf.x, df_res_verschil$aant_gaaf.y)
  
  
  # DEEL 2: wat er in de levering zit
  # verwachten dat de bestanden nvwa runderen heeft en de res uit functie niet meer
  df_inhoud_nvwa <- inl_lees_input_rdvl(paden$rdvl_lev_pad, jrmnd)
  df_inhoud_rvo <- inl_lees_input_rdvl(paden$rdvl_rvo_lev_pad, jrmnd)
  df_inhoud_bestand <- rbind(df_inhoud_nvwa, df_inhoud_rvo)
  
  # resultaat uit de functie (levering deel)
  res_data <- res$data
  res_data_uit_lev <- dplyr::select(res_data, slnm, wrkp, dsrt, is_biologisch, tidyselect::all_of(maanden_huidige_vm)) |>
    tidyr::pivot_longer(cols = tidyselect::all_of(maanden_huidige_vm),
                        names_to = "vsmd", values_to = "aant")
  
  # voer vergelijking uit
  df_res_verschil <- dplyr::anti_join(res_data_uit_lev, df_inhoud_bestand,
                                      by = dplyr::join_by(vsmd, wrkp, slnm, dsrt, is_biologisch))
  
  verw_dcats <- c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd", "Kalveren 8-12 mnd")
  expect_true(all(df_res_verschil$dsrt %in% verw_dcats))
  expect_true(all(is.na(df_res_verschil$aant)))
  
  ###### inhoud data_def (uit database) ----------------------------------------
  
  # wat er in de databse zit
  inhoud_database <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl_def")) |>
    dplyr::collect()
  
  # resultaat uit functie
  res_data <- res$data_def
  
  # Er mogen geen verschillen zijn tussen de input gegevens en de gegevens die
  # zijn opgeslagen in de database.
  res_data |> 
    dplyr::anti_join(inhoud_database,
                     by = dplyr::join_by(vsmd, wrkp, slnm, dsrt)) |> 
    nrow() |> 
    expect_equal(0)
  
  
  ###### inhoud data_lev (uit levering) ----------------------------------------
  df_inhoud_nvwa <- inl_lees_input_rdvl(paden$rdvl_lev_pad, jrmnd)
  df_inhoud_rvo <- inl_lees_input_rdvl(paden$rdvl_rvo_lev_pad, jrmnd)
  df_inhoud_bestand <- rbind(df_inhoud_nvwa, df_inhoud_rvo)
  
  # resultaat uit functie
  res_data <- res$data_lev
  
  res_data |> 
    dplyr::anti_join(df_inhoud_bestand,
                     by = dplyr::join_by(vsmd, wrkp, slnm, dsrt)) |> 
    nrow() |> 
    expect_equal(0)
  
  
  ###### inhoud data_vorige_lev (uit database) ---------------------------------
  inhoud_database <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl")) |> 
    dplyr::filter(vwmd == jrmnd$vvwmd) |>
    dplyr::collect()
  
  res_data <- res$data_vorige_lev
  
  res_data |> 
    dplyr::anti_join(inhoud_database,
                     by = dplyr::join_by(vsmd, wrkp, slnm, dsrt)) |> 
    nrow() |> 
    expect_equal(0)
})
  

test_that("dataverzameling uit DB en levering goed gaat in de tussensituatie", {
  ########### SITUATIE 2: RVO & NVWA in database, RVO & NVWA in levering ------- 
  
  
  # maak een verbinding met de ont database
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  
  DBI::dbExecute(con, qry)
  
  # nu gaan we de database vullen met behulp van de syndata functies om de
  # benodigde gegevens toe te voegen om één maand te verwerken
  verw_maand <- "202404"
  start_rvo <- "202403"
  
  # Hiermee worden ook de invoerbestanden voor een maand aangemaakt, dus we doen
  # dit in een tijdelijke map (die wordt verwijderd nadat de test is voltooid)
  temp_map <- withr::local_tempdir()
  
  # we moeten de jaar- en maandmap maken in de tijdelijke map
  file.path(temp_map, "2024", verw_maand) |> 
    dir.create(temp_map, recursive = TRUE)
  
  paden <- synthetiseer_spek_bestanden_e_ini_db(
    verw_maand = verw_maand, 
    con = con,
    input_map = temp_map, 
    db_schema = "unittest",
    n_regels = 5,
    rvo_slachtingen_kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    is_bio = TRUE,
    start_rvo = start_rvo) # er is RVO data in DB voor de afgelopen 1 maand
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  maanden_vorige_vm  <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3)
  maanden_huidige_vm <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4)
  
  # definieren koppeltabel
  df_koppelen_vvwm <- data.frame(
    maand_id = c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3"),
    vsmd = maanden_vorige_vm
  )
  
  res <- verzamel_data_ctcr_rdvl(jrmnd = jrmnd,
                                 levering_bst_nvwa = paden$rdvl_lev_pad, 
                                 levering_bst_rvo = paden$rdvl_rvo_lev_pad,
                                 maanden_huidige_vm = maanden_huidige_vm)
  

  ###### inhoud draaitabel komt uit database en levering -----------------------
  
  # DEEL 1: wat er in de databse zit
  inhoud_database <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl")) |> 
    dplyr::filter(vwmd == jrmnd$vvwmd) |>
    dplyr::select(-aant_ruw) |>
    dplyr::collect()
  
  # resultaat uit functie (database deel)
  res_data <- res$data
  res_data_uit_db <- dplyr::select(res_data, slnm, wrkp, dsrt, vvm_mnd1, vvm_mnd2, vvm_mnd3, is_biologisch) |>
    tidyr::pivot_longer(cols = c(vvm_mnd1, vvm_mnd2, vvm_mnd3),
                        names_to = "maand_id", values_to = "aant_gaaf") |>
    dplyr::left_join(df_koppelen_vvwm, by = "maand_id") |>
    dplyr::select(-maand_id)
  
  # vergeleken met de DB moet resultaat uit de functie extra rijen hebben: 
  # RVO dcats zonder aant_gaaf waarden.
  # (komt doordat RVO waarden alleen in de levering zitten, nog niet in DB)
  
  
  rvo_dcats <- c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd", "Kalveren 8-12 mnd")
  
  # niet-rund roodvlees komt overeen
  df_res_verschil <- dplyr::anti_join(
    res_data_uit_db |> dplyr::filter(!dsrt %in% rvo_dcats),
    inhoud_database |> dplyr::filter(!dsrt %in% rvo_dcats),
    by = dplyr::join_by(vsmd, wrkp, slnm, dsrt, is_biologisch)
  ) |> 
    nrow() |> 
    expect_equal(0)
  
  # mogelijk verschil in afwezigheid van "is_biologisch" uit vwmd1-vwmd3
  # selecteer runderen & maanden die in de draaitabel komen
  df_res_verschil <- dplyr::full_join(
    res_data_uit_db |> 
      dplyr::filter(dsrt %in% rvo_dcats) |>
      dplyr::filter(vsmd %in% maanden_huidige_vm), 
    inhoud_database |> 
      dplyr::filter(dsrt %in% rvo_dcats) |>
      dplyr::filter(vsmd %in% maanden_huidige_vm),
    by = dplyr::join_by(vsmd, wrkp, slnm, dsrt, is_biologisch)
  ) 
  expect_equal(df_res_verschil$aant_gaaf.x, df_res_verschil$aant_gaaf.y)
  
  
  # DEEL 2: wat er in de levering zit
  # verwachten dat de bestanden nvwa runderen heeft en de res uit functie niet meer
  df_inhoud_nvwa <- inl_lees_input_rdvl(paden$rdvl_lev_pad, jrmnd)
  df_inhoud_rvo <- inl_lees_input_rdvl(paden$rdvl_rvo_lev_pad, jrmnd)
  df_inhoud_bestand <- rbind(df_inhoud_nvwa, df_inhoud_rvo)
  
  # resultaat uit de functie (levering deel)
  res_data <- res$data
  res_data_uit_lev <- dplyr::select(res_data, slnm, wrkp, dsrt, is_biologisch, tidyselect::all_of(maanden_huidige_vm)) |>
    tidyr::pivot_longer(cols = tidyselect::all_of(maanden_huidige_vm),
                        names_to = "vsmd", values_to = "aant")
  
  # voer vergelijking uit
  df_res_verschil <- dplyr::anti_join(res_data_uit_lev, df_inhoud_bestand,
                                      by = dplyr::join_by(vsmd, wrkp, slnm, dsrt, is_biologisch))
  
  verw_dcats <- c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd", "Kalveren 8-12 mnd")
  expect_true(all(df_res_verschil$dsrt %in% verw_dcats))
  expect_true(all(is.na(df_res_verschil$aant)))
  
  # het verschil is alleen voor de maanden voordat RVO start
  mnd_voor_start <- maanden_huidige_vm[1 : (which(maanden_huidige_vm == start_rvo) - 1)]
  expect_true(all(df_res_verschil$vsmd %in% mnd_voor_start))
  
  
  ###### inhoud data_def (uit database) ----------------------------------------
  
  # wat er in de databse zit
  inhoud_database <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl_def")) |>
    dplyr::collect()
  
  # resultaat uit functie
  res_data <- res$data_def
  
  # Er mogen geen verschillen zijn tussen de input gegevens en de gegevens die
  # zijn opgeslagen in de database.
  res_data |> 
    dplyr::anti_join(inhoud_database,
                     by = dplyr::join_by(vsmd, wrkp, slnm, dsrt)) |> 
    nrow() |> 
    expect_equal(0)
  
  
  ###### inhoud data_lev (uit levering) ----------------------------------------
  df_inhoud_nvwa <- inl_lees_input_rdvl(paden$rdvl_lev_pad, jrmnd)
  df_inhoud_rvo <- inl_lees_input_rdvl(paden$rdvl_rvo_lev_pad, jrmnd)
  df_inhoud_bestand <- rbind(df_inhoud_nvwa, df_inhoud_rvo)
  
  # resultaat uit functie
  res_data <- res$data_lev
  
  res_data |> 
    dplyr::anti_join(df_inhoud_bestand,
                     by = dplyr::join_by(vsmd, wrkp, slnm, dsrt)) |> 
    nrow() |> 
    expect_equal(0)
  
  
  ###### inhoud data_vorige_lev (uit database) ---------------------------------
  inhoud_database <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl")) |> 
    dplyr::filter(vwmd == jrmnd$vvwmd) |>
    dplyr::collect()
  
  res_data <- res$data_vorige_lev
  
  res_data |> 
    dplyr::anti_join(inhoud_database,
                     by = dplyr::join_by(vsmd, wrkp, slnm, dsrt)) |> 
    nrow() |> 
    expect_equal(0)
})

  
test_that("dataverzameling uit DB en levering goed gaat in de startsituatie", {
  ########### SITUATIE 1: NVWA in database, RVO & NVWA in levering ------------ 
  
  
  # maak een verbinding met de ont database
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  
  DBI::dbExecute(con, qry)
  
  # nu gaan we de database vullen met behulp van de syndata functies om de
  # benodigde gegevens toe te voegen om één maand te verwerken
  verw_maand <- "202404"
  
  # Hiermee worden ook de invoerbestanden voor een maand aangemaakt, dus we doen
  # dit in een tijdelijke map (die wordt verwijderd nadat de test is voltooid)
  temp_map <- withr::local_tempdir()
  
  # we moeten de jaar- en maandmap maken in de tijdelijke map
  file.path(temp_map, "2024", verw_maand) |> 
    dir.create(temp_map, recursive = TRUE)
  
  paden <- synthetiseer_spek_bestanden_e_ini_db(
    verw_maand = verw_maand, 
    con = con,
    input_map = temp_map, 
    db_schema = "unittest",
    n_regels = 5,
    rvo_slachtingen_kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen)
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  maanden_vorige_vm  <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3)
  maanden_huidige_vm <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4)
  
  # definieren koppeltabel
  df_koppelen_vvwm <- data.frame(
    maand_id = c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3"),
    vsmd = maanden_vorige_vm
  )
  
  res <- verzamel_data_ctcr_rdvl(jrmnd = jrmnd,
                                 levering_bst_nvwa = paden$rdvl_lev_pad, 
                                 levering_bst_rvo = paden$rdvl_rvo_lev_pad,
                                 maanden_huidige_vm = maanden_huidige_vm)

  
  ###### inhoud draaitabel komt uit database en levering -----------------------
  
  # DEEL 1: wat er in de databse zit
  inhoud_database <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl")) |> 
    dplyr::filter(vwmd == jrmnd$vvwmd) |>
    dplyr::select(-aant_ruw) |>
    dplyr::collect()
  
  # resultaat uit functie (database deel)
  res_data <- res$data
  res_data_uit_db <- dplyr::select(res_data, slnm, wrkp, dsrt, vvm_mnd1, vvm_mnd2, vvm_mnd3, is_biologisch) |>
    tidyr::pivot_longer(cols = c(vvm_mnd1, vvm_mnd2, vvm_mnd3),
                        names_to = "maand_id", values_to = "aant_gaaf") |>
    dplyr::left_join(df_koppelen_vvwm, by = "maand_id") |>
    dplyr::select(-maand_id)
    
  res_data_uit_db$is_biologisch <- ifelse(res_data_uit_db$is_biologisch == "1", TRUE, FALSE)
  
  
  # vergeleken met de DB moet resultaat uit de functie extra rijen hebben: 
  # RVO dcats zonder aant_gaaf waarden.
  # (komt doordat RVO waarden alleen in de levering zitten, nog niet in DB)
  
  df_res_verschil <- dplyr::anti_join(res_data_uit_db, inhoud_database,
                     by = dplyr::join_by(vsmd, wrkp, slnm, dsrt, is_biologisch))
  
  verw_dcats <- c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd", "Kalveren 8-12 mnd")
  expect_true(all(df_res_verschil$dsrt %in% verw_dcats))
  expect_true(all(is.na(df_res_verschil$aant_gaaf)))
  

  # DEEL 2: wat er in de leveringen zit
  df_inhoud_nvwa <- inl_lees_input_rdvl(paden$rdvl_lev_pad, jrmnd)
  df_inhoud_rvo <- inl_lees_input_rdvl(paden$rdvl_rvo_lev_pad, jrmnd)
  df_inhoud_bestand <- rbind(df_inhoud_nvwa, df_inhoud_rvo)
  
  # resultaat uit de functie (levering deel)
  res_data <- res$data
  res_data_uit_lev <- dplyr::select(res_data, slnm, wrkp, dsrt, tidyselect::all_of(maanden_huidige_vm)) |>
    tidyr::pivot_longer(cols = tidyselect::all_of(maanden_huidige_vm),
                        names_to = "vsmd", values_to = "aant")
  
  # voer vergelijking uit
  res_data_uit_lev |> 
    dplyr::anti_join(df_inhoud_bestand,
                     by = dplyr::join_by(vsmd, wrkp, slnm, dsrt)) |> 
    nrow() |> 
    expect_equal(0)
  
  
  ###### inhoud data_def (uit database) ----------------------------------------
  
  # wat er in de databse zit
  inhoud_database <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl_def")) |>
    dplyr::collect()
  
  # resultaat uit functie
  res_data <- res$data_def
  
  # Er mogen geen verschillen zijn tussen de input gegevens en de gegevens die
  # zijn opgeslagen in de database.
  res_data |> 
    dplyr::anti_join(inhoud_database,
                     by = dplyr::join_by(vsmd, wrkp, slnm, dsrt)) |> 
    nrow() |> 
    expect_equal(0)
  
  
  ###### inhoud data_lev (uit levering) ----------------------------------------
  df_inhoud_nvwa <- inl_lees_input_rdvl(paden$rdvl_lev_pad, jrmnd)
  df_inhoud_rvo <- inl_lees_input_rdvl(paden$rdvl_rvo_lev_pad, jrmnd)
  df_inhoud_bestand <- rbind(df_inhoud_nvwa, df_inhoud_rvo)
  
  # resultaat uit functie
  res_data <- res$data_lev
  
  res_data |> 
    dplyr::anti_join(df_inhoud_bestand,
                     by = dplyr::join_by(vsmd, wrkp, slnm, dsrt)) |> 
    nrow() |> 
    expect_equal(0)
  
  
  ###### inhoud data_vorige_lev (uit database) ---------------------------------
  inhoud_database <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl")) |> 
    dplyr::filter(vwmd == jrmnd$vvwmd) |>
    dplyr::collect()
  
  res_data <- res$data_vorige_lev
  
  res_data |> 
    dplyr::anti_join(inhoud_database,
                     by = dplyr::join_by(vsmd, wrkp, slnm, dsrt)) |> 
    nrow() |> 
    expect_equal(0)

})


test_that("de bijschattingen in de dataverzameling goed gaan", {
  # maak een verbinding met de ont database
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = App$dbdrivernaam,
                        Server = App$dbservernaam,
                        Database = App$databasenaam
  )
  # unittest schema in ont database aanmaken. 
  # Hiermee worden alle tabellen in het unittest schema verwijderd en alle
  # tabellen opnieuw aangemaakt. De test wordt dus uitgevoerd in een 'schone
  # database'.
  qry <- readr::read_lines(file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  
  DBI::dbExecute(con, qry)
  
  # nu gaan we de database vullen met behulp van de syndata functies om de
  # benodigde gegevens toe te voegen om één maand te verwerken
  verw_maand <- "202404"
  
  # Hiermee worden ook de invoerbestanden voor een maand aangemaakt, dus we doen
  # dit in een tijdelijke map (die wordt verwijderd nadat de test is voltooid)
  temp_map <- withr::local_tempdir()
  
  # we moeten de jaar- en maandmap maken in de tijdelijke map
  file.path(temp_map, "2024", verw_maand) |> 
    dir.create(temp_map, recursive = TRUE)
  
  
  paden <- synthetiseer_spek_bestanden_e_ini_db(
    verw_maand = verw_maand, 
    con = con,
    input_map = temp_map, 
    db_schema = "unittest",
    n_regels = 5,
    rvo_slachtingen_kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen)
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  maanden_vorige_vm  <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3)
  maanden_huidige_vm <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4)
  

  ###### de bijschattingen; onafhankelijk van situatie -------------------------
  df_aangepast <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl")) |> 
    dplyr::filter(vwmd == jrmnd$vvwmd) |>
    dplyr::select(-aant_ruw) |>
    dplyr::collect() |> 
    dplyr::slice_sample(n = 10)  |> 
    dplyr::mutate(
      aant = as.character(aant_gaaf + 3)
    ) |> 
    dplyr::select(
      vwmd, vsmd, wrkp, dsrt, aant
    )
  
  
  df_aangepast |> 
    purrr::pwalk(
      \(vwmd, vsmd, wrkp, dsrt, aant) {
        
        glue::glue_sql(
          "UPDATE unittest.tbl_microbase_rdvl 
    SET aant_gaaf = {aant}
    WHERE vwmd = {vwmd} AND vsmd = {vsmd} AND wrkp = {wrkp} AND dsrt = {dsrt}",
          .con = con
        ) |> 
          DBI::dbExecute(conn = con, statement = _)
        
      }
    )
  
  
  res <- verzamel_data_ctcr_rdvl(jrmnd = jrmnd,
                                 levering_bst_nvwa = paden$rdvl_lev_pad, 
                                 levering_bst_rvo = paden$rdvl_rvo_lev_pad,
                                 maanden_huidige_vm = maanden_huidige_vm)
  
  # voer de verwachting uit
  expect_identical(
    res$bijschatting_vorige_lev |>  dplyr::select(vsmd, wrkp, dsrt, aant) |> dplyr::arrange(vsmd, wrkp, dsrt),
    df_aangepast  |>  dplyr::select(vsmd, wrkp, dsrt, aant) |> dplyr::arrange(vsmd, wrkp, dsrt) |> as.data.frame()
    
  )
})


test_that("de data.frame kolommen voor roodvlees correct zijn gedefinieerd", {
  test_maand <- "202311"
  jrmnd <- alg_vind_jaarmaanden(test_maand)
  
  ##### Basissituatie
  
  df <- data.frame(
    check.names = FALSE,
    slnm = c("Slachterij A", "Slachterij B"),
    wrkp = c("000001", "000002"),
    dsrt = c("Lammeren", "Koeien"),
    is_biologisch = c(NA, 0),
    vvm_mnd1 = c(6990, NA),
    vvm_mnd2 = c(2167, NA),
    vvm_mnd3 = c(5914, NA),
    "202308" = c(6990, 7343),
    "202309" = c(2167, 573),
    "202310" = c(5914, 4436),
    "202311" = c(3760, 5772),
    bron = c("NVWA", "RVO"),
    ind1 = c(NA, NA),
    ind2 = c(NA, NA),
    ind3 = c(6216.75, NA)
  )
  
  expect_no_error(
    selecteer_frame_rdvl(df, vsmd = c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4))
  )
  
  ##### expect error als:
  
  # kolom is_biologisch ontbreekt
  expect_error(
    selecteer_frame_rdvl(dplyr::select(df, -c("is_biologisch")), 
                         vsmd = c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4)),
    "undefined columns selected"
  )
  
  # kolom bron ontbreekt
  expect_error(
    selecteer_frame_rdvl(dplyr::select(df, -c("bron")), 
                         vsmd = c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4)),
    "undefined columns selected"
  )
  
})

test_that("de data.frame kolommen voor witvlees correct zijn gedefinieerd", {
  test_maand <- "202311"
  jrmnd <- alg_vind_jaarmaanden(test_maand)
  
  ##### Basissituatie
  
  df <- data.frame(
    check.names = FALSE,
    slnm = c("Slachterij A", "Slachterij B"),
    wrkp = c("000001", "000002"),
    dsrt = c("Kippen", "Eenden"),
    vvm_mnd1 = c(6990, NA),
    vvm_mnd2 = c(2167, NA),
    vvm_mnd3 = c(5914, NA),
    "202308" = c(6990, 7343),
    "202309" = c(2167, 573),
    "202310" = c(5914, 4436),
    "202311" = c(3760, 5772),
    ind1 = c(NA, NA),
    ind2 = c(NA, NA),
    ind3 = c(6216.75, NA)
  )
  
  expect_no_error(
    selecteer_frame_wtvl(df, vsmd = c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4))
  )
  
  ###### mogelijke extra kolommen is_biologisch en bron kolommen geven geen error
  
  df <- data.frame(
    check.names = FALSE,
    slnm = c("Slachterij A", "Slachterij B"),
    wrkp = c("000001", "000002"),
    dsrt = c("Kippen", "Eenden"),
    is_biologisch = c(NA, NA),
    vvm_mnd1 = c(6990, NA),
    vvm_mnd2 = c(2167, NA),
    vvm_mnd3 = c(5914, NA),
    "202308" = c(6990, 7343),
    "202309" = c(2167, 573),
    "202310" = c(5914, 4436),
    "202311" = c(3760, 5772),
    bron = c(NA, NA),
    ind1 = c(NA, NA),
    ind2 = c(NA, NA),
    ind3 = c(6216.75, NA)
  )
  
  expect_no_error(
    selecteer_frame_wtvl(df, vsmd = c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4))
  )
})

test_that("de kolommen is_biologisch en databron in het ctcr-bestand komen", {
  test_maand <- "202311"
  
  df <- data.frame(
    slnm = c("Slachterij A", "Slachterij B"),
    wrkp = c("000001", "000002"),
    dsrt = c("Lammeren", "Koeien"),
    is_biologisch = c(NA, 0),
    vvm_mnd1 = c(6990, NA),
    vvm_mnd2 = c(2167, NA),
    vvm_mnd3 = c(5914, NA),
    `202308` = c(6990, 7343),
    `202309` = c(2167, 573),
    `202310` = c(5914, 4436),
    `202311` = c(3760, 5772),
    bron = c("NVWA", "RVO"),
    ind1 = c(NA, NA),
    ind2 = c(NA, NA),
    ind3 = c(6216.75, NA)
  )
  df_bijschatting <- data.frame(
    slnm = c(),
    wrkp = c(),
    dsrt = c(),
    vsmd = c(),
    aant = c()
  )
  temp_pad <- withr::local_tempfile(fileext = ".xlsx")
  
  # voer functie uit
  jrmnd <- alg_vind_jaarmaanden(test_maand)
  stel_samen_ctcr_rdvl(maanden_vorige_vm = jrmnd[1:3],
                       maanden_huidige_vm = jrmnd,
                       data = df,
                       bijschatting = df_bijschatting,
                       bijschatting_vorige_lev = df_bijschatting,
                       bestand = temp_pad)
  
  # lees de data in zonder headers, vanaf rij 3
  res <- as.data.frame(
    readxl::read_xlsx(temp_pad, skip = 2, col_names = FALSE)
  )
  
  # expect that nr kolommen overeenkomen
  expect_true(length(res) == length(df))
  
  # hernoem kolommen em expect dat de inhoud precies overeenkomt
  names(res) <- names(df)
  expect_equal(res, df)
})


test_that("de RVO runderen- en NVWA roodvleeslevering worden goed samengevoegd", {
  
  # RVO rund en NVWA roodvlees uit verschillende slachthuizen ------------------
  test_maand <- "202311"
  
  # nvwa data
  df_nvwa <- data.frame(
    stringsAsFactors = FALSE,
    Jaar = c("2023"),
    Maand = c("11"),
    DIER_SOORT_CODE = c("GE"),
    DIER_SOORT_OMSCHR = c("GEIT"),
    WERKPLEK_NR = c("000001"),
    WERKPLEKNAAM = c("Slachterij A"),
    WERKPLAATS = c(NA),
    AANTAL_AANGEBODEN = c(100)
  )
  temp_nvwa <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_nvwa, temp_nvwa)
  
  # rvo data
  df_rvo <- data.frame(
    stringsAsFactors = FALSE,
    Jaar = c("2023"),
    Maand = c("11"),
    DIER_SOORT_OMSCHR = c("Koeien"),
    WERKPLEK_NR = c("UBN:000002"),
    WERKPLEKNAAM = c("Slachterij B"),
    WERKPLAATS = c(NA),
    AANTAL_AANGEBODEN = c(1),
    BIOLOGISCH = c(TRUE)
  )
  temp_rvo <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_rvo, temp_rvo)
  
  # stel verwachting 
  df_verw <- data.frame(
    stringsAsFactors = FALSE,
    vwmd = c("202311", "202311"),
    vsmd = c("202311", "202311"),
    wrkp = c("UBN:000002", "000001"),
    slnm = c("Slachterij B",
             "Slachterij A"),
    dsrt = c("Koeien", "Geiten"),
    aant = c(1, 100),
    is_biologisch = c(TRUE, FALSE),
    bron = c("RVO", "NVWA")
  )
  
  # voer de functie uit
  jrmnd <- alg_vind_jaarmaanden(test_maand, -3)
  res <- voeg_rvo_runderen_bij_nvwa(bst_nvwa=temp_nvwa, bst_rvo=temp_rvo,
                                    jrmnd=jrmnd)
  
  expect_identical(res, df_verw)
  
  
  # RVO rund en NVWA geit uit zelfde slachthuis -------------------------------
  test_maand <- "202311"
  
  # nvwa data
  df_nvwa <- data.frame(
    stringsAsFactors = FALSE,
    Jaar = c("2023"),
    Maand = c("11"),
    DIER_SOORT_CODE = c("GE"),
    DIER_SOORT_OMSCHR = c("GEIT"),
    WERKPLEK_NR = c("000001"),
    WERKPLEKNAAM = c("Slachterij A"),
    WERKPLAATS = c(NA),
    AANTAL_AANGEBODEN = c(100)
  )
  temp_nvwa <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_nvwa, temp_nvwa)
  
  # rvo data
  df_rvo <- data.frame(
    stringsAsFactors = FALSE,
    Jaar = c("2023"),
    Maand = c("11"),
    DIER_SOORT_OMSCHR = c("Koeien"),
    WERKPLEK_NR = c("UBN:000001"),
    WERKPLEKNAAM = c("Slachterij A"),
    WERKPLAATS = c(NA),
    AANTAL_AANGEBODEN = c(1),
    BIOLOGISCH = c(TRUE)
  )
  temp_rvo <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_rvo, temp_rvo)
  
  # stel verwachting 
  df_verw <- data.frame(
    stringsAsFactors = FALSE,
    vwmd = c("202311", "202311"),
    vsmd = c("202311", "202311"),
    wrkp = c("UBN:000001", "000001"),
    slnm = c("Slachterij A",
             "Slachterij A"),
    dsrt = c("Koeien", "Geiten"),
    aant = c(1, 100),
    is_biologisch = c(TRUE, FALSE),
    bron = c("RVO", "NVWA")
  )
  
  # voer de functie uit
  jrmnd <- alg_vind_jaarmaanden(test_maand, -3)
  res <- voeg_rvo_runderen_bij_nvwa(bst_nvwa=temp_nvwa, bst_rvo=temp_rvo,
                                    jrmnd=jrmnd)
  
  expect_identical(res, df_verw)
  
  
  # RVO rund en NVWA rund uit zelfde slachthuis -------------------------------
  test_maand <- "202311"
  
  # nvwa data
  df_nvwa <- data.frame(
    stringsAsFactors = FALSE,
    Jaar = c("2023"),
    Maand = c("11"),
    DIER_SOORT_CODE = c("RU"),
    DIER_SOORT_OMSCHR = c("RUND"),
    WERKPLEK_NR = c("000001"),
    WERKPLEKNAAM = c("Slachterij A"),
    WERKPLAATS = c(NA),
    AANTAL_AANGEBODEN = c(100)
  )
  temp_nvwa <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_nvwa, temp_nvwa)
  
  # rvo data
  df_rvo <- data.frame(
    stringsAsFactors = FALSE,
    Jaar = c("2023"),
    Maand = c("11"),
    DIER_SOORT_OMSCHR = c("Koeien"),
    WERKPLEK_NR = c("UBN:000001"),
    WERKPLEKNAAM = c("Slachterij A"),
    WERKPLAATS = c(NA),
    AANTAL_AANGEBODEN = c(101),
    BIOLOGISCH = c(TRUE)
  )
  temp_rvo <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_rvo, temp_rvo)
  
  # stel verwachting 
  df_verw <- data.frame(
    stringsAsFactors = FALSE,
    vwmd = c("202311"),
    vsmd = c("202311"),
    wrkp = c("UBN:000001"),
    slnm = c("Slachterij A"),
    dsrt = c("Koeien"),
    aant = c(101),
    is_biologisch = c(TRUE),
    bron = c("RVO")
  )
  
  # voer de functie uit
  jrmnd <- alg_vind_jaarmaanden(test_maand, -3)
  res <- voeg_rvo_runderen_bij_nvwa(bst_nvwa=temp_nvwa, bst_rvo=temp_rvo,
                                    jrmnd=jrmnd)
  
  expect_identical(res, df_verw)
})


test_that("het rvo/nvwa vergelijkingsbestand aggregeerd zoals verwacht", {
  jrmnd <- alg_vind_jaarmaanden("202311")
  
  ##### Berekenen van het verschil tussen rvo en nvwa aantallen gaat goed
  
  # definieer data: gemeenschappelijk voor beide test dfs
  df_basis <- data.frame(
    stringsAsFactors = FALSE,
    Jaar = "2023",
    Maand = 8:11, 
    DIER_SOORT_CODE = c("KA", "KA", "RU", "RU"),
    WERKPLEK_NR = c("1", "2", "3", "4"),
    WERKPLEKNAAM = c("Slachterij 1", "Slachterij 2", "Slachterij 3",
                     "Slachterij 4"),
    WERKPLAATS = NA_character_,
    AANTAL_AANGEBODEN = c(1, 1, 1, 100)
  )
  # rvo data
  df_rvo <- df_basis |> 
    dplyr::mutate(
      DIER_SOORT_OMSCHR = c("Kalveren 0-8 mnd", "Kalveren 8-12 mnd", "Koeien", "Vaarzen"),
      is_biologisch = FALSE
    )
  temp_rvo <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_rvo, temp_rvo)
  
  # nvwa data
  df_nvwa <- df_basis |>
    dplyr::mutate(
      DIER_SOORT_OMSCHR = c("KALF", "KALF", "RUND", "RUND")
    )
  temp_nvwa <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_nvwa, temp_nvwa)
  
  # voer de functie uit
  res <- verzamel_data_ctcr_nvwa_rvo(jrmnd, temp_rvo, temp_nvwa)
  
  # verwacht dat er geen verschil is tussen rvo en nvwa
  expect_equal(res$`Controle (I&R - NVWA)`, c(NA, NA, NA, 0))
  
  # verwacht dat alleen verwerkingsmaand 4 wordt gebruikt voor de controle kolom
  expect_equal(res$`I&R_202311` - res$aant_202311, res$`Controle (I&R - NVWA)`)
  
  # voer de functie uit
  temp_verschil <- withr::local_tempfile(pattern="verschil", fileext = ".xlsx") 
  res <- maak_ctcr_verschil_rvo_nvwa(jrmnd, temp_nvwa, temp_rvo, jrmnd[1:4],
                                     temp_verschil)
  # verwacht dat er een excel bestand wordt gemaakt
  expect_true(file.exists(temp_verschil))
  
  # verwacht dat de rapportage naar de gebruiker klopt
  expect_identical(
    res, 
    paste("bestand", basename(temp_verschil), "is gemaakt")
  )
  
  
  ##### Aggregeren van rundersubcategorieen bij éénzelfde bedrijf gaat goed
  # nvwa data
  df_nvwa <- data.frame(
    Jaar = "2023",
    Maand = 11, 
    WERKPLAATS = NA_character_,
    DIER_SOORT_OMSCHR = c("KALF", "RUND"),
    AANTAL_AANGEBODEN = c(2, 400),
    DIER_SOORT_CODE = c("KA", "RU"),
    WERKPLEK_NR = c("1", "2"),
    WERKPLEKNAAM = c("Slachterij 1", "Slachterij 2")
  )
  temp_nvwa <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_nvwa, temp_nvwa)
  
  # rvo data
  df_rvo <- data.frame(
    Jaar = "2023",
    Maand = 11, 
    WERKPLAATS = NA_character_,
    DIER_SOORT_OMSCHR = c("Kalveren 0-8 mnd", "Kalveren 8-12 mnd", "Koeien", "Vaarzen"),
    AANTAL_AANGEBODEN = c(1, 1, 200, 200),
    DIER_SOORT_CODE = c("KA", "KA", "RU", "RU"),
    WERKPLEK_NR = c("1", "1", "2", "2"),
    WERKPLEKNAAM = c("Slachterij 1", "Slachterij 1", "Slachterij 2",
                     "Slachterij 2"),
    is_biologisch = FALSE
  )
  temp_rvo <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_rvo, temp_rvo)
  # voer de functie uit
  res <- verzamel_data_ctcr_nvwa_rvo(jrmnd, temp_rvo, temp_nvwa)
  # aggregeren van kalveren OF runderen bij één bedrijf
  expect_identical(res$`I&R_202311`, df_nvwa$AANTAL_AANGEBODEN)
  
  # aggregeren van kalveren EN runderen bij één bedrijf
  df_nvwa <- df_nvwa |>
    dplyr::mutate(
      WERKPLEK_NR = "1",
      WERKPLEKNAAM = "Slachterij 1"
    )
  temp_nvwa <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_nvwa, temp_nvwa)
  
  df_rvo <- df_rvo |>
    dplyr::mutate(
      WERKPLEK_NR = "1",
      WERKPLEKNAAM = "Slachterij 1",
    )
  temp_rvo <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_rvo, temp_rvo)
  
  res <- verzamel_data_ctcr_nvwa_rvo(jrmnd, temp_rvo, temp_nvwa)
  
  expect_identical(res$`Controle (I&R - NVWA)`, c(0, 0))
  
  
  ##### Als er geen nvwa runderen zijn, 
  
  # nvwa data
  df_nvwa <- data.frame(
    Jaar = "2023",
    Maand = 11, 
    WERKPLAATS = NA_character_,
    DIER_SOORT_OMSCHR = c("GEIT", "VARKEN"),
    AANTAL_AANGEBODEN = c(2, 400),
    DIER_SOORT_CODE = c("GE", "VA"),
    WERKPLEK_NR = c("1", "2"),
    WERKPLEKNAAM = c("Slachterij 1", "Slachterij 2")
  )
  temp_nvwa <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_nvwa, temp_nvwa)
  
  # rvo data
  df_rvo <- data.frame(
    Jaar = "2023",
    Maand = 11, 
    WERKPLAATS = NA_character_,
    DIER_SOORT_OMSCHR = c("Kalveren 0-8 mnd", "Koeien"),
    AANTAL_AANGEBODEN = c(1, 200),
    DIER_SOORT_CODE = c("KA", "RU"),
    WERKPLEK_NR = c("1", "3"),
    WERKPLEKNAAM = c("Slachterij 1", "Slachterij 3"),
    BIOLOGISCH = FALSE
  )
  temp_rvo <- withr::local_tempfile(fileext = ".xlsx")
  write.xlsx(df_rvo, temp_rvo)
  temp_verschil <- withr::local_tempfile(pattern="analyse_rdvl", fileext = ".xlsx")
  
  # verwacht dat de runderen check functie werkt
  nvwa_heeft_runderen <- check_nvwa_voor_runderen(jrmnd, temp_nvwa)
  expect_false(nvwa_heeft_runderen)
  
  res <- maak_ctcr_verschil_rvo_nvwa(jrmnd, temp_nvwa, temp_rvo, jrmnd[1:4],
                                     temp_verschil)
  
  # verwacht dat er geen ctcr verschil bestand is gemaakt
  expect_false(file.exists(temp_verschil))
  
  # verwacht dat rapportage naar gebruiker correct is
  expect_identical(
    res, 
    paste("NVWA bevat geen runderen. bestand", basename(temp_verschil), "is niet gemaakt")
  )
  
})
