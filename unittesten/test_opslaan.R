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
source(file.path(src_map, "ctcrbestand_aantallen_witvlees.R"))
source(file.path(src_map, "inlezen.R"))
source(file.path(src_map, "database.R"))
source(file.path(src_map, "synthetische_data.R"))
source(file.path(src_map, "opslaan.R"))

test_that("het (biologische) roodvlees correct in microbase wordt opgeslagen", {
  
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
  qry <- readr::read_lines(
      file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")
    ) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  
  DBI::dbExecute(con, qry)
  
  # nu gaan we de database vullen met behulp van de syndata functies om de
  # benodigde gegevens toe te voegen om één maand te verwerken
  verw_maand <- "202404"
  start_rvo <- "202304"
  
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
    start_rvo = start_rvo)
  # We zijn klaar met het vullen van de database, dus we verbreken de
  # verbinding. Vanaf nu worden de SPEK-functies gebruikt en wordt een andere
  # verbinding (via RODBC) gebruikt
  
  
  # In deze test gebruiken we SPEK-functies die berichten naar een logbestand
  # schrijven (via de logging package). Daarom moeten we dit bestand maken. We
  # gebruiken weer een tijdelijk bestand dat na de test wordt verwijderd
  logbestand   <- withr::local_tempfile()
  addHandler(handler=writeToFile, file=logbestand, level='DEBUG')
  
  # Nu testen we de SPEK-functies die een controles en correctie-bestand maken
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  maanden_vorige_vm  <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3)
  maanden_huidige_vm <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4)
  
  ctcr_bestanden <- alg_vind_ctcr_bestanden(werkmap=temp_map, vwmd=jrmnd$vwmd)
  ctcr_aant_rdvl_bst <- ctcr_bestanden$ctcr_aant_rdvl
  ctcr_aant_wtvl_bst <- ctcr_bestanden$ctcr_aant_wtvl
  ctcr_verd_bst      <- ctcr_bestanden$ctcr_verd
  ctcr_gemg_bst      <- ctcr_bestanden$ctcr_gemg
  
  input_nvwa_rdvl_bst    <- paden$rdvl_lev_pad
  input_rvo_rdvl_bst     <- paden$rdvl_rvo_lev_pad
  input_aant_wtvl_bst    <- paden$wtvl_lev_pad
  input_verd_gemg_bst    <- paden$gemg_pad
  input_gemg_kalv_bst    <- paden$gemg_kalv_pad
  input_gemg_kalv_vj_bst <- NA
  
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
  
  # we willen weten hoeveel rijen zijn er in de database voordat we de nieuwe
  # data inlezen
  n_rijen_tbl_microbase_rdvl_voor <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl")) |> 
    dplyr::count() |> 
    dplyr::collect() |> 
    dplyr::pull(n)
  
  n_rijen_tbl_microbase_rdvl_def_voor <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl_def")) |> 
    dplyr::count() |> 
    dplyr::collect() |> 
    dplyr::pull(n)
  
  res_aant_rdvl <- maak_ctcr_aant_rdvl(inputbestand_nvwa=input_nvwa_rdvl_bst,
                                       inputbestand_rvo=input_rvo_rdvl_bst,
                                       jrmnd=jrmnd,
                                       maanden_vorige_vm=maanden_vorige_vm,
                                       maanden_huidige_vm=maanden_huidige_vm,
                                       outputbestand=ctcr_aant_rdvl_bst)
  # Slaan de nieuwe data op
  ops_updaten_micro_rdvl(bst_aant_rdvl_nvwa_ruw=input_nvwa_rdvl_bst, 
                         bst_aant_rdvl_rvo_ruw=input_rvo_rdvl_bst,
                         bst_aant_gaaf=ctcr_aant_rdvl_bst,
                         maanden_huidige_vm=maanden_huidige_vm,
                         jrmnd=jrmnd)
  
  # tbl_microbase_rdvl ------------------------------------------------------
  
  # In deze tabel moeten we evenveel nieuwe rijen hebben als rijen in het
  # invoerbestand voor de laatste vier maanden:
  
  # nieuwe aantal rijen
  n_rijen_tbl_microbase_rdvl_na <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl")) |> 
    dplyr::count() |> 
    dplyr::collect() |> 
    dplyr::pull(n)
  
  # rijen uit het ctcr bestand
  df_ruw <- voeg_rvo_runderen_bij_nvwa(bst_nvwa=input_nvwa_rdvl_bst, 
                                       bst_rvo=input_rvo_rdvl_bst, jrmnd)
  df_gaaf <- inl_lees_ctcr_aant_rdvl(bst_aant_gaaf=ctcr_aant_rdvl_bst,
                                     maanden_huidige_vm=maanden_huidige_vm,
                                     jrmnd=jrmnd) |> 
    dplyr::mutate(
      is_biologisch = as.logical(is_biologisch)
    )
  df_input <- ops_voeg_ruw_ctcr_samen(df_ruw=df_ruw, df_ctcr=df_gaaf)
  
  # verschil in aantal rijen geijk aan aantal rijen in input
  expect_equal(n_rijen_tbl_microbase_rdvl_na - n_rijen_tbl_microbase_rdvl_voor, 
               nrow(df_input))
  # Er mogen geen verschillen zijn tussen de input gegevens en de gegevens die
  # zijn opgeslagen in de database.
  df_in_db <- dplyr::tbl(con, I("unittest.tbl_microbase_rdvl")) |>
    dplyr::collect()
  
  df_input |> 
    dplyr::anti_join(df_in_db,
                     copy = TRUE,
                     by = dplyr::join_by(vwmd, vsmd, wrkp, slnm, dsrt, is_biologisch)) |> 
    nrow() |> 
    expect_equal(0)
  
  # tbl_microbase_rdvl_def --------------------------------------------------
  n_rijen_tbl_microbase_rdvl_def_na <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_rdvl_def")) |> 
    dplyr::count() |> 
    dplyr::collect() |> 
    dplyr::pull(n)
  
  
  n_rijen_def <- df_input |> 
    dplyr::filter(vsmd == "202401") |> 
    nrow()
  
  # verschil in aantal rijen geijk aan aantal rijen in input
  expect_equal(n_rijen_tbl_microbase_rdvl_def_na - n_rijen_tbl_microbase_rdvl_def_voor, 
               n_rijen_def)
  
  DBI::dbDisconnect(con)
})


test_that("het witvlees correct in microbase wordt opgeslagen", {
  
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
  qry <- readr::read_lines(
    file.path(testdata_map, "spek_test_database_met_bio_kolom.sql")
  ) |>
    paste(collapse = "\n") |> 
    glue::as_glue()
  
  DBI::dbExecute(con, qry)
  
  # nu gaan we de database vullen met behulp van de syndata functies om de
  # benodigde gegevens toe te voegen om één maand te verwerken
  verw_maand <- "202404"
  start_rvo <- "202304"
  
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
    start_rvo = start_rvo)
  # We zijn klaar met het vullen van de database, dus we verbreken de
  # verbinding. Vanaf nu worden de SPEK-functies gebruikt en wordt een andere
  # verbinding (via RODBC) gebruikt
  
  
  # In deze test gebruiken we SPEK-functies die berichten naar een logbestand
  # schrijven (via de logging package). Daarom moeten we dit bestand maken. We
  # gebruiken weer een tijdelijk bestand dat na de test wordt verwijderd
  logbestand   <- withr::local_tempfile()
  addHandler(handler=writeToFile, file=logbestand, level='DEBUG')
  
  # Nu testen we de SPEK-functies die een controles en correctie-bestand maken
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  maanden_vorige_vm  <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3)
  maanden_huidige_vm <- c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4)
  
  ctcr_bestanden <- alg_vind_ctcr_bestanden(werkmap=temp_map, vwmd=jrmnd$vwmd)
  ctcr_aant_rdvl_bst <- ctcr_bestanden$ctcr_aant_rdvl
  ctcr_aant_wtvl_bst <- ctcr_bestanden$ctcr_aant_wtvl
  ctcr_verd_bst      <- ctcr_bestanden$ctcr_verd
  ctcr_gemg_bst      <- ctcr_bestanden$ctcr_gemg
  
  input_nvwa_rdvl_bst    <- paden$rdvl_lev_pad
  input_rvo_rdvl_bst     <- paden$rdvl_rvo_lev_pad
  input_aant_wtvl_bst    <- paden$wtvl_lev_pad
  input_verd_gemg_bst    <- paden$gemg_pad
  input_gemg_kalv_bst    <- paden$gemg_kalv_pad
  input_gemg_kalv_vj_bst <- NA
  
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
  
  # we willen weten hoeveel rijen zijn er in de database voordat we de nieuwe
  # data inlezen
  n_rijen_tbl_microbase_wtvl_voor <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_wtvl")) |> 
    dplyr::count() |> 
    dplyr::collect() |> 
    dplyr::pull(n)
  
  n_rijen_tbl_microbase_wtvl_def_voor <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_wtvl_def")) |> 
    dplyr::count() |> 
    dplyr::collect() |> 
    dplyr::pull(n)
  
  res_aant_wtvl <- maak_ctcr_aant_wtvl(inputbestand=input_aant_wtvl_bst,
                                       jrmnd=jrmnd,
                                       maanden_vorige_vm=maanden_vorige_vm,
                                       maanden_huidige_vm=maanden_huidige_vm,
                                       outputbestand=ctcr_aant_wtvl_bst)
  # Slaan de nieuwe data op
  ops_updaten_micro_wtvl(bst_aant_ruw=input_aant_wtvl_bst,
                         bst_aant_gaaf=ctcr_aant_wtvl_bst,
                         maanden_huidige_vm=maanden_huidige_vm,
                         jrmnd=jrmnd)
  
  # tbl_microbase_wtvl ------------------------------------------------------
  
  # In deze tabel moeten we evenveel nieuwe rijen hebben als rijen in het
  # invoerbestand voor de laatste vier maanden:
  
  # nieuwe aantal rijen
  n_rijen_tbl_microbase_wtvl_na <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_wtvl")) |> 
    dplyr::count() |> 
    dplyr::collect() |> 
    dplyr::pull(n)
  
  # rijen uit het ctcr bestand
  df_ruw  <- inl_lees_input_wtvl(inputbestand=input_aant_wtvl_bst, 
                                 jrmnd=jrmnd) |>
    dplyr::mutate(is_biologisch = FALSE)
  df_gaaf <- inl_lees_ctcr_aant_wtvl(bst_aant_gaaf=ctcr_aant_wtvl_bst,
                                     maanden_huidige_vm=maanden_huidige_vm,
                                     jrmnd=jrmnd) |>
    dplyr::mutate(is_biologisch = FALSE)
  
  df_input <- ops_voeg_ruw_ctcr_samen(df_ruw=df_ruw, df_ctcr=df_gaaf)
  
  # verschil in aantal rijen geijk aan aantal rijen in input
  expect_equal(n_rijen_tbl_microbase_wtvl_na - n_rijen_tbl_microbase_wtvl_voor, 
               nrow(df_input))
  # Er mogen geen verschillen zijn tussen de input gegevens en de gegevens die
  # zijn opgeslagen in de database.
  df_in_db <- dplyr::tbl(con, I("unittest.tbl_microbase_wtvl")) |>
    dplyr::collect()
  
  df_input |> 
    dplyr::anti_join(df_in_db,
                     copy = TRUE,
                     by = dplyr::join_by(vwmd, vsmd, wrkp, slnm, dsrt, is_biologisch)) |> 
    nrow() |> 
    expect_equal(0)
  
  # tbl_microbase_wtvl_def --------------------------------------------------
  n_rijen_tbl_microbase_wtvl_def_na <- con |> 
    dplyr::tbl(I("unittest.tbl_microbase_wtvl_def")) |> 
    dplyr::count() |> 
    dplyr::collect() |> 
    dplyr::pull(n)
  
  
  n_rijen_def <- df_input |> 
    dplyr::filter(vsmd == "202401") |> 
    nrow()
  
  # verschil in aantal rijen geijk aan aantal rijen in input
  expect_equal(n_rijen_tbl_microbase_wtvl_def_na - n_rijen_tbl_microbase_wtvl_def_voor, 
               n_rijen_def)
  
  DBI::dbDisconnect(con)
})


test_that("Diersoort (tbl_statbase_dsrt) aggregaties en opslaan in database:
          een diersoort in een van de tabellen.", {
  
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
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  
  
  aantal_ruw <- c(1, 10)
  aantal_gaaf <- c(10, 100)
  # we maken input data, rdvl tabel met maar een dier
  df_rdvl <- tibble::tribble(
    ~vwmd,    ~wrkp, ~dsrt,
    verw_maand,  1, "Diersoort", 
    verw_maand,  2, "Diersoort", 
  ) |> dplyr::mutate(
        aant_ruw = aantal_ruw, 
        aant_gaaf = aantal_gaaf,
        vsmd = jrmnd$mnd1,
        slnm = paste0("SL:", wrkp)
  )
  # input data naar de database
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_microbase_rdvl"),
    value = df_rdvl,
    append = TRUE
  )
  
  df_dsrt_dcat <- data.frame(
    dsrt = "Diersoort",
    dcat = "Diersoort"
  )
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
    value = df_dsrt_dcat,
    overwrite = TRUE
  )
  
  # Aggregatie en oplsaan gebeurt binnen dezelfe functie
  ops_updaten_stat_dsrt(jrmnd)
  

  # tabel ophalen
  df_db_tbl_statbase_dsrt <- con |> 
    dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dsrt")) |> 
    dplyr::collect()
    
  # we verwachten maar een rij want er is maar een diersoort in de rdvl tabel
  
  expect_equal(nrow(df_db_tbl_statbase_dsrt), 1)
  
  expect_equal(df_db_tbl_statbase_dsrt$dsrt, "Diersoort")
  expect_equal(df_db_tbl_statbase_dsrt$tota_ruw, sum(aantal_ruw))
  expect_equal(df_db_tbl_statbase_dsrt$tota_gaaf, sum(aantal_gaaf))
  pct <- ((sum(aantal_gaaf) - sum(aantal_ruw)) / sum(aantal_ruw)) * 100
  expect_equal(df_db_tbl_statbase_dsrt$pcbs, pct)
  
  DBI::dbDisconnect(con)
})

test_that("Diersoort (tbl_statbase_dsrt) aggregaties en opslaan in database:
          twee diersorten in een van de tabellen.", {
  
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
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  
  
  aantal_ruw <- c(1, 10)
  aantal_gaaf <- c(10, 100)
  # we maken input data, rdvl tabel met maar een dier
  df_rdvl <- tibble::tribble(
    ~vwmd,    ~wrkp, ~dsrt,
    verw_maand,  1, "Diersoort_1", 
    verw_maand,  2, "Diersoort_2", 
  ) |> dplyr::mutate(
    aant_ruw = aantal_ruw, 
    aant_gaaf = aantal_gaaf,
    vsmd = jrmnd$mnd1,
    slnm = paste0("SL:", wrkp)
  )
  # input data naar de database
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_microbase_rdvl"),
    value = df_rdvl,
    append = TRUE
  )
  # de aggregatie functie grebruikt een database tabel om alle mogelijk 
  # diersoorten te aggregeren. 
  # We overschrijven de standaardwaarden voor de test
  df_dsrt_dcat <- data.frame(
    dsrt = c("Diersoort_1", "Diersoort_2"),
    dcat = c("Diersoort_1", "Diersoort_2")
  )
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
    value = df_dsrt_dcat,
    overwrite = TRUE
  )
  
  # Aggregatie en oplsaan gebeurt binnen dezelfe functie
  ops_updaten_stat_dsrt(jrmnd)
  
  # tabel ophalen
  df_db_tbl_statbase_dsrt <- con |> 
    dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dsrt")) |> 
    dplyr::collect()
  
  # we verwachten maar een rij want er is maar een diersoort in de rdvl tabel
  
  expect_equal(nrow(df_db_tbl_statbase_dsrt), 2)
  
  expect_equal(df_db_tbl_statbase_dsrt$dsrt, c("Diersoort_1", "Diersoort_2"))
  expect_equal(df_db_tbl_statbase_dsrt$tota_ruw, aantal_ruw)
  expect_equal(df_db_tbl_statbase_dsrt$tota_gaaf, aantal_gaaf)
  pct <- ((aantal_gaaf -aantal_ruw) / aantal_ruw) * 100
  expect_equal(df_db_tbl_statbase_dsrt$pcbs, pct)
  
  DBI::dbDisconnect(con)
})

test_that("Diersoort (tbl_statbase_dsrt) aggregaties en opslaan in database:
          twee diersorten in twee tabellen.", {
            
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
            
            jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                          verschil=App$verschil_huidige_mnd_eerste_mnd)
            
            
            aantal_ruw <- c(1, 10)
            aantal_gaaf <- c(10, 100)
            # we maken input data, rdvl tabel met maar een dier
            df <- tibble::tribble(
              ~vwmd,    ~wrkp, ~dsrt,
              verw_maand,  1, "Diersoort_1", 
              verw_maand,  2, "Diersoort_2", 
            ) |> dplyr::mutate(
              aant_ruw = aantal_ruw, 
              aant_gaaf = aantal_gaaf,
              vsmd = jrmnd$mnd1,
              slnm = paste0("SL:", wrkp)
            )
            # input data naar de database
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_microbase_rdvl"),
              value = df |> dplyr::filter(dsrt == "Diersoort_1"),
              append = TRUE
            )
            
            # input data naar de database
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_microbase_wtvl"),
              value = df |> dplyr::filter(dsrt == "Diersoort_2"),
              append = TRUE
            )
            # de aggregatie functie grebruikt een database tabel om alle mogelijk 
            # diersoorten te aggregeren. 
            # We overschrijven de standaardwaarden voor de test
            df_dsrt_dcat <- data.frame(
              dsrt = c("Diersoort_1", "Diersoort_2"),
              dcat = c("Diersoort_1", "Diersoort_2")
            )
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
              value = df_dsrt_dcat,
              overwrite = TRUE
            )
            
            # Aggregatie en oplsaan gebeurt binnen dezelfe functie
            ops_updaten_stat_dsrt(jrmnd)
            
            
            # tabel ophalen
            df_db_tbl_statbase_dsrt <- con |> 
              dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dsrt")) |> 
              dplyr::collect()
            
            # we verwachten maar een rij want er is maar een diersoort in de rdvl tabel
            
            expect_equal(nrow(df_db_tbl_statbase_dsrt), 2)
            
            expect_equal(df_db_tbl_statbase_dsrt$dsrt, c("Diersoort_1", "Diersoort_2"))
            expect_equal(df_db_tbl_statbase_dsrt$tota_ruw, aantal_ruw)
            expect_equal(df_db_tbl_statbase_dsrt$tota_gaaf, aantal_gaaf)
            pct <- ((aantal_gaaf -aantal_ruw) / aantal_ruw) * 100
            expect_equal(df_db_tbl_statbase_dsrt$pcbs, pct)
            
            DBI::dbDisconnect(con)
          })

test_that("Diersoort (tbl_statbase_dsrt) aggregaties en opslaan in database:
          diersoorten die niet in de huidige verwerkingsmaand aanweizig zijn
          worden nog in de aggregaties meegenomen.", {
            
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
            
            jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                          verschil=App$verschil_huidige_mnd_eerste_mnd)
            
            
            aantal_ruw <- c(1, 10)
            aantal_gaaf <- c(10, 100)
            # we maken input data, rdvl tabel met maar een dier
            df <- tibble::tribble(
              ~vwmd,    ~wrkp, ~dsrt,
              verw_maand,  1, "Diersoort_1", 
              verw_maand,  2, "Diersoort_2", 
            ) |> dplyr::mutate(
              aant_ruw = aantal_ruw, 
              aant_gaaf = aantal_gaaf,
              vwmd = verw_maand,
              vsmd = jrmnd$mnd1,
              slnm = paste0("SL:", wrkp)
            )
            # input data naar de database
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_microbase_rdvl"),
              value = df,
              append = TRUE
            )
            
            # de aggregatie functie grebruikt een database tabel om alle mogelijk 
            # diersoorten te aggregeren. 
            # We overschrijven de standaardwaarden voor de test
            df_dsrt_dcat <- data.frame(
              # diersoort_4 is een diersoort die niet in de huidige vwmd staat
              dsrt = c("Diersoort_1", "Diersoort_2", "Diersoort_4"),
              dcat = c("Diersoort_1", "Diersoort_2", "Diersoort_4")
            )
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
              value = df_dsrt_dcat,
              overwrite = TRUE
            )
            
            # Aggregatie en oplsaan gebeurt binnen dezelfe functie
            ops_updaten_stat_dsrt(jrmnd)
            
            
            # tabel ophalen
            df_db_tbl_statbase_dsrt <- con |> 
              dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dsrt")) |> 
              dplyr::collect()
            
            # we verwachten maar een rij want er is maar een diersoort in de rdvl tabel
            
            expect_equal(nrow(df_db_tbl_statbase_dsrt), 3)
            
            expect_equal(df_db_tbl_statbase_dsrt$dsrt, c("Diersoort_1", 
                                                         "Diersoort_2",
                                                         "Diersoort_4"))
            
            tota_ruw_dsrt_4 <- df_db_tbl_statbase_dsrt |> 
              dplyr::filter(dsrt == "Diersoort_4") |> 
              dplyr::pull(tota_ruw)
            
            expect_true(is.na(tota_ruw_dsrt_4))
            
            tota_gaaf_dsrt_4 <- df_db_tbl_statbase_dsrt |> 
              dplyr::filter(dsrt == "Diersoort_4") |> 
              dplyr::pull(tota_gaaf)
            expect_true(is.na(tota_gaaf_dsrt_4))
            
            
            DBI::dbDisconnect(con)
          })

test_that("Diersoort (tbl_statbase_dsrt) aggregaties en opslaan in database:
          twee diersorten die tot dezelfe categorie behoren.", {
            
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
            
            jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                          verschil=App$verschil_huidige_mnd_eerste_mnd)
            
            
            aantal_ruw <- c(1, 10)
            aantal_gaaf <- c(10, 100)
            # we maken input data, rdvl tabel met maar een dier
            df <- tibble::tribble(
              ~vwmd,    ~wrkp, ~dsrt,
              verw_maand,  1, "Diersoort_1", 
              verw_maand,  2, "Diersoort_2", 
            ) |> dplyr::mutate(
              aant_ruw = aantal_ruw, 
              aant_gaaf = aantal_gaaf,
              vsmd = jrmnd$mnd1,
              slnm = paste0("SL:", wrkp)
            )
            # input data naar de database
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_microbase_rdvl"),
              value = df,
              append = TRUE
            )
            
            # de aggregatie functie grebruikt een database tabel om alle mogelijk 
            # diersoorten te aggregeren. 
            # We overschrijven de standaardwaarden voor de test
            df_dsrt_dcat <- data.frame(
              dsrt = c("Diercat_1", "Diercat_1"),
              dcat = c("Diersoort_1", "Diersoort_2")
            )
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
              value = df_dsrt_dcat,
              overwrite = TRUE
            )
            
            # Aggregatie en oplsaan gebeurt binnen dezelfe functie
            ops_updaten_stat_dsrt(jrmnd)
            
            
            # tabel ophalen
            df_db_tbl_statbase_dsrt <- con |> 
              dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dsrt")) |> 
              dplyr::collect()
            
            # we verwachten maar een rij met de diercategorie
            
            expect_equal(nrow(df_db_tbl_statbase_dsrt), 1)
            
            expect_equal(df_db_tbl_statbase_dsrt$dsrt, c("Diercat_1"))
            expect_equal(df_db_tbl_statbase_dsrt$tota_ruw, sum(aantal_ruw))
            expect_equal(df_db_tbl_statbase_dsrt$tota_gaaf, sum(aantal_gaaf))
            pct <- ((sum(aantal_gaaf) - sum(aantal_ruw)) / sum(aantal_ruw)) * 100
            expect_equal(df_db_tbl_statbase_dsrt$pcbs, pct)
            
            DBI::dbDisconnect(con)
          })

test_that("Diersoort (tbl_statbase_dsrt) aggregaties en opslaan in database:
          twee diersorten die tot dezelfe categorie behoren en een andere
          diersoort waarvoor dsrt == dcat.", {
            
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
            
            jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                          verschil=App$verschil_huidige_mnd_eerste_mnd)
            
            aantal_ruw_dsrt_3 <- 100
            aantal_gaaf_dsrt_3 <- 140
            aantal_ruw <-  c(1,   10, aantal_ruw_dsrt_3)
            aantal_gaaf <- c(10, 100, aantal_gaaf_dsrt_3)
            # we maken input data, rdvl tabel met maar een dier
            df <- tibble::tribble(
              ~vwmd,    ~wrkp, ~dsrt,
              verw_maand,  1, "Diersoort_1", 
              verw_maand,  2, "Diersoort_2", 
              verw_maand,  2, "Diersoort_3"
            ) |> dplyr::mutate(
              aant_ruw = aantal_ruw, 
              aant_gaaf = aantal_gaaf,
              vsmd = jrmnd$mnd1,
              slnm = paste0("SL:", wrkp)
            )
            # input data naar de database
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_microbase_rdvl"),
              value = df,
              append = TRUE
            )
            
            # de aggregatie functie grebruikt een database tabel om alle mogelijk 
            # diersoorten te aggregeren. 
            # We overschrijven de standaardwaarden voor de test
            df_dsrt_dcat <- data.frame(
              dsrt = c("Diercat_1",   "Diercat_1",   "Diersoort_3"),
              dcat = c("Diersoort_1", "Diersoort_2", "Diersoort_3")
            )
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
              value = df_dsrt_dcat,
              overwrite = TRUE
            )
            
            # Aggregatie en oplsaan gebeurt binnen dezelfe functie
            ops_updaten_stat_dsrt(jrmnd)
            
            
            # tabel ophalen
            df_db_tbl_statbase_dsrt <- con |> 
              dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dsrt")) |> 
              dplyr::collect()
            
            # we verwachten maar een rij met de diercategorie
            
            expect_equal(nrow(df_db_tbl_statbase_dsrt), 2)
            
            expect_equal(df_db_tbl_statbase_dsrt$dsrt, c("Diercat_1", "Diersoort_3"))
            
            df_diersoort_3 <- df_db_tbl_statbase_dsrt |> 
              dplyr::filter(dsrt == "Diersoort_3")
            expect_equal(df_diersoort_3$tota_ruw, sum(aantal_ruw_dsrt_3))
            expect_equal(df_diersoort_3$tota_gaaf, sum(aantal_gaaf_dsrt_3))
            pct <- ((sum(aantal_gaaf_dsrt_3) - sum(aantal_ruw_dsrt_3)) / 
                      sum(aantal_ruw_dsrt_3)) * 100
            expect_equal(df_diersoort_3$pcbs, pct)
            
            DBI::dbDisconnect(con)
          })

test_that("Diersoort (tbl_statbase_dsrt) aggregaties en opslaan in database:
          een slachthuis met bio en niet bio dieren.", {
            
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
            
            jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                          verschil=App$verschil_huidige_mnd_eerste_mnd)
            
            aantal_ruw_bio       <- c(10)
            aantal_gaaf_bio      <- c(15)
            aantal_ruw_niet_bio  <- c(100)
            aantal_gaaf_niet_bio <- c(95)
            # we maken input data, rdvl tabel met maar een dier
            df <- tibble::tribble(
              ~is_biologisch,           ~aant_ruw,          ~aant_gaaf,
                        TRUE,      aantal_ruw_bio,     aantal_gaaf_bio,
                       FALSE, aantal_ruw_niet_bio, aantal_gaaf_niet_bio
            ) |> 
              dplyr::mutate(
                # de rest is gelijk voor beide rijen
                vwmd = verw_maand,
                dsrt = "Diersoort_1",
                wrkp = 1,
                vwmd = verw_maand,
                vsmd = jrmnd$mnd1,
                slnm = paste0("SL:", wrkp)
            )
            # input data naar de database
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_microbase_rdvl"),
              value = df,
              append = TRUE
            )
            
            # de aggregatie functie grebruikt een database tabel om alle mogelijk 
            # diersoorten te aggregeren. 
            # We overschrijven de standaardwaarden voor de test
            df_dsrt_dcat <- data.frame(
              # diersoort_4 is een diersoort die niet in de huidige vwmd staat
              dsrt = c("Diersoort_1"),
              dcat = c("Diersoort_1")
            )
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
              value = df_dsrt_dcat,
              overwrite = TRUE
            )
            
            # Aggregatie en oplsaan gebeurt binnen dezelfe functie
            ops_updaten_stat_dsrt(jrmnd)
            
            
            # tabel ophalen
            df_db_tbl_statbase_dsrt <- con |> 
              dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dsrt")) |> 
              dplyr::collect()
            
            # we verwachten maar een rij want er is maar een diersoort in de rdvl tabel
            
            expect_equal(nrow(df_db_tbl_statbase_dsrt), 2)
            
            expect_equal(unique(df_db_tbl_statbase_dsrt$dsrt), c("Diersoort_1"))
            
            expect_equal(df_db_tbl_statbase_dsrt$is_biologisch, c(TRUE, FALSE))
       
            totaal_niet_bio <- df_db_tbl_statbase_dsrt |> 
              dplyr::filter(!is_biologisch) |> 
              dplyr::pull(tota_gaaf)
            
            expect_equal(totaal_niet_bio, aantal_gaaf_niet_bio)
            
            totaal_bio <- df_db_tbl_statbase_dsrt |> 
              dplyr::filter(is_biologisch) |> 
              dplyr::pull(tota_gaaf)
            
            expect_equal(totaal_bio, aantal_gaaf_bio)
            
            DBI::dbDisconnect(con)
          })

test_that("Diercategorieen (tbl_statbase_dcat) aggregaties en opslaan in database:
          een diersoort in een van de tabellen.", {
            
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
            
            jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                          verschil=App$verschil_huidige_mnd_eerste_mnd)
            
            
            aantal_ruw <- c(1, 10)
            aantal_gaaf <- c(10, 100)
            gemg <- 100
            # we maken input data, rdvl tabel met maar een dier
            df_rdvl <- tibble::tribble(
              ~vwmd,    ~wrkp, ~dsrt,
              verw_maand,  1, "Diersoort_1", 
              verw_maand,  2, "Diersoort_1", 
            ) |> 
              dplyr::mutate(
                aant_ruw = aantal_ruw, 
                aant_gaaf = aantal_gaaf,
                vsmd = jrmnd$mnd1,
                slnm = paste0("SL:", wrkp)
            )
            
            # input data naar de database
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_microbase_rdvl"),
              value = df_rdvl,
              append = TRUE
            )
            
            df_dsrt_dcat <- data.frame(
              dsrt = c("Diersoort_1"),
              dcat = c("Diersoort_1")
            )
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
              value = df_dsrt_dcat,
              overwrite = TRUE
            )
            
            df_gemg <-  tibble::tribble(
                             ~dcat,  ~gemg,
              "Diersoort_1",   gemg
            ) |> 
              dplyr::mutate(
                vwmd = verw_maand,
                vsmd = jrmnd$mnd1
              )
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_microbase_gemg"),
              value = df_gemg,
              append = TRUE
            )
            
            # Aggregatie en oplsaan gebeurt binnen dezelfe functie
            ops_updaten_stat_dcat(jrmnd)
            
            # tabel ophalen
            df_db_tbl_statbase_dcat <- con |> 
              dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dcat")) |> 
              dplyr::collect()
            
            # we verwachten maar een rij want er is maar een diersoort in de rdvl tabel
            
            expect_equal(nrow(df_db_tbl_statbase_dcat), 1)
            
            expect_equal(df_db_tbl_statbase_dcat$dcat, "Diersoort_1")
            expect_equal(df_db_tbl_statbase_dcat$tota_gaaf, sum(aantal_gaaf))
            gewicht <- sum(aantal_gaaf) * gemg
            expect_equal(df_db_tbl_statbase_dcat$totg, gewicht)
            
            DBI::dbDisconnect(con)
          })

test_that("Diercategorieen (tbl_statbase_dcat) aggregaties en opslaan in database:
           een slachthuis met bio en niet bio dieren.", {
            
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
            
            jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                          verschil=App$verschil_huidige_mnd_eerste_mnd)
            
            aantal_ruw_bio       <- c(10)
            aantal_gaaf_bio      <- c(15)
            aantal_ruw_niet_bio  <- c(100)
            aantal_gaaf_niet_bio <- c(95)
            gemg <- 100
            # we maken input data, rdvl tabel met maar een dier
            df_rdvl <- tibble::tribble(
              ~is_biologisch,           ~aant_ruw,          ~aant_gaaf,
              TRUE,      aantal_ruw_bio,     aantal_gaaf_bio,
              FALSE, aantal_ruw_niet_bio, aantal_gaaf_niet_bio
            ) |> 
              dplyr::mutate(
                # de rest is gelijk voor beide rijen
                vwmd = verw_maand,
                dsrt = "Diersoort_1",
                wrkp = 1,
                vwmd = verw_maand,
                vsmd = jrmnd$mnd1,
                slnm = paste0("SL:", wrkp)
              )
            # input data naar de database
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_microbase_rdvl"),
              value = df_rdvl,
              append = TRUE
            )
            
            df_dsrt_dcat <- data.frame(
              dsrt = c("Diersoort_1"),
              dcat = c("Diersoort_1")
            )
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
              value = df_dsrt_dcat,
              overwrite = TRUE
            )
            
            df_gemg <-  tibble::tribble(
              ~dcat,  ~gemg,
              "Diersoort_1",   gemg
            ) |> 
              dplyr::mutate(
                vwmd = verw_maand,
                vsmd = jrmnd$mnd1
              )
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id("unittest", "tbl_microbase_gemg"),
              value = df_gemg,
              append = TRUE
            )
            
            
            # Aggregatie en oplsaan gebeurt binnen dezelfe functie
            ops_updaten_stat_dcat(jrmnd)
            
            
            # tabel ophalen
            df_db_tbl_statbase_dcat <- con |> 
              dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dcat")) |> 
              dplyr::collect()
            
            # we verwachten twee rijen want er is maar een diersoort maar met 
            # bio / niet bio slachtingen
            
            expect_equal(nrow(df_db_tbl_statbase_dcat), 2)
            
            expect_equal(unique(df_db_tbl_statbase_dcat$dcat), "Diersoort_1")
            
            expect_equal(df_db_tbl_statbase_dcat$is_biologisch, c(TRUE, FALSE))
            
            totaal_niet_bio <- df_db_tbl_statbase_dcat |> 
              dplyr::filter(!is_biologisch) |> 
              dplyr::pull(tota_gaaf)
            
            expect_equal(totaal_niet_bio, aantal_gaaf_niet_bio)
            
            totaal_bio <- df_db_tbl_statbase_dcat |> 
              dplyr::filter(is_biologisch) |> 
              dplyr::pull(tota_gaaf)
            
            expect_equal(totaal_bio, aantal_gaaf_bio)
            
            DBI::dbDisconnect(con)
          })


test_that("'Onbekend' op ratio over de rund diercategorieen wordt verdeeld
          in de ops_updaten_stat_dsrt functie", {
            
  # gebaseerd op de "twee diersorten die tot dezelfe categorie behoren" test
  
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
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  
  # we maken het totaal aantal runderen 1000, met 100 "onbekend"
  aantal_ruw <- c(100, 500, 500)
  aantal_gaaf <- c(100, 500, 500)
  
  # we maken input data, rdvl tabel met maar een dier
  df <- tibble::tribble(
    ~vwmd,    ~wrkp, ~dsrt,
    verw_maand,  0, "Onbekend",
    verw_maand,  1, "Koeien", 
    verw_maand,  2, "Stieren", 
  ) |> dplyr::mutate(
    aant_ruw = aantal_ruw, 
    aant_gaaf = aantal_gaaf,
    vsmd = jrmnd$mnd1,
    slnm = paste0("SL:", wrkp)
  )
  # input data naar de database
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_microbase_rdvl"),
    value = df,
    append = TRUE
  )
  
  # de aggregatie functie grebruikt een database tabel om alle mogelijk 
  # diersoorten te aggregeren. 
  # We overschrijven de standaardwaarden voor de test
  df_dsrt_dcat <- data.frame(
    dsrt = c("Volwassen runderen", "Volwassen runderen"),
    dcat = c("Koeien", "Stieren")
  )
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
    value = df_dsrt_dcat,
    overwrite = TRUE
  )
  
  # test de verdeling in de stats_dsrt functie
  ops_updaten_stat_dsrt(jrmnd)
  
  # tabel ophalen
  df_db_tbl_statbase_dsrt <- con |> 
    dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dsrt")) |> 
    dplyr::collect()
  
  # we verwachten maar één rij met de diercategorie
  expect_equal(nrow(df_db_tbl_statbase_dsrt), 1)
  expect_equal(df_db_tbl_statbase_dsrt$dsrt, c("Volwassen runderen"))
  
  # we verwachten dat de aantallen "Onbekend" zijn meegenomen in de aggregatie
  expect_equal(df_db_tbl_statbase_dsrt$tota_ruw, sum(aantal_ruw))
  expect_equal(df_db_tbl_statbase_dsrt$tota_gaaf, sum(aantal_gaaf))
  
  DBI::dbDisconnect(con)
})


test_that("'Onbekend' op ratio over de rund diercategorieen wordt verdeeld in
          de functie ops_updaten_stat_dcat", {
            
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
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  
  # we maken het totaal aantal runderen 1000, met 100 "onbekend"
  aantal_ruw <- c(100, 500, 500)
  aantal_gaaf <- c(100, 500, 500)
  gemg <- 100
  
  # we maken input data, rdvl tabel met maar een dier
  df <- tibble::tribble(
    ~vwmd,    ~wrkp, ~dsrt,
    verw_maand,  0, "Onbekend",
    verw_maand,  1, "Koeien", 
    verw_maand,  2, "Stieren", 
  ) |> dplyr::mutate(
    aant_ruw = aantal_ruw, 
    aant_gaaf = aantal_gaaf,
    vsmd = jrmnd$mnd1,
    slnm = paste0("SL:", wrkp)
  )
  # input data naar de database
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_microbase_rdvl"),
    value = df,
    append = TRUE
  )
  
  # de aggregatie functie grebruikt een database tabel om alle mogelijk 
  # diersoorten te aggregeren. 
  # We overschrijven de standaardwaarden voor de test
  df_dsrt_dcat <- data.frame(
    dsrt = c("Volwassen runderen", "Volwassen runderen"),
    dcat = c("Koeien", "Stieren")
  )
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
    value = df_dsrt_dcat,
    overwrite = TRUE
  )
  
  df_gemg <-  tibble::tribble(
    ~dcat,  ~gemg,
    "Koeien",   gemg,
    "Stieren", gemg,
  ) |> 
    dplyr::mutate(
      vwmd = verw_maand,
      vsmd = jrmnd$mnd1
    )
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_microbase_gemg"),
    value = df_gemg,
    append = TRUE
  )
  
  # Aggregatie en oplsaan gebeurt binnen dezelfe functie
  ops_updaten_stat_dcat(jrmnd)
  
  # tabel ophalen
  df_db_tbl_statbase_dcat <- con |> 
    dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dcat")) |> 
    dplyr::collect()
  
  
  # we verwachten twee rijen, "Onbekend" is weg
  expect_equal(nrow(df_db_tbl_statbase_dcat), 2)
  expect_equal(df_db_tbl_statbase_dcat$dcat, c("Koeien", "Stieren"))
  
  # we verwachten dat de het totale aantal is behouden
  expect_equal(sum(df_db_tbl_statbase_dcat$tota_gaaf), sum(aantal_gaaf))

  # we verwachten dat "Onbekend" over alle categorieen is verdeeld
  expect_equal(length(unique(df_db_tbl_statbase_dcat$tota_gaaf)), 1)
  
  DBI::dbDisconnect(con)
})


test_that("De verdeling van 'Onbekend' per verslagmaand gebeurt", {
  # gebaseerd op de "twee diersorten die tot dezelfe categorie behoren" test
  
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
  volgende_verw_maand <- "202405"
  vers_maand <- "202402"
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  volgende_jrmnd <- alg_vind_jaarmaanden(inputmap=volgende_verw_maand,
                                         verschil=App$verschil_huidige_mnd_eerste_mnd)
  
  # we maken het totaal aantal runderen 1000, met 100 "onbekend"
  aantal <- c(100, 1000)
  
  # we maken input data voor één verslagmaand over twee verwerkingsmaanden
  df_vsmd <- tibble::tribble(
    ~wrkp, ~dsrt,      ~vsmd,
    0,     "Onbekend", vers_maand,
    1,     "Koeien",   vers_maand,
  ) |> 
    dplyr::mutate(
      aant_ruw = aantal, 
      aant_gaaf = aantal,
      slnm = paste0("SL:", wrkp)
    ) |> 
    dplyr::group_by(
      wrkp, dsrt
    ) |> 
    tidyr::crossing(
      vwmd = c(verw_maand, volgende_verw_maand), 
      .name_repair = "unique"
    ) |>
    dplyr::ungroup()
  
  # voeg een extra rij toe voor een willekeurige verslagmaand, 
  # zodat het totale aantal per verwerkingsmaand verschilt
  df <- dplyr::bind_rows(
    df_vsmd, 
    df_vsmd |>
      dplyr::filter(wrkp == 1, vwmd == volgende_verw_maand) |> 
      dplyr::mutate(vsmd = jrmnd$mnd1, vwmd = verw_maand, dsrt = "Koeien")
  )
  
  # input data naar de database
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_microbase_rdvl"),
    value = df,
    append = TRUE
  )
  
  # de aggregatie functie grebruikt een database tabel om alle mogelijk 
  # diersoorten te aggregeren. 
  # We overschrijven de standaardwaarden voor de test
  df_dsrt_dcat <- data.frame(
    dsrt = c("Volwassen runderen"),
    dcat = c("Koeien")
  )
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
    value = df_dsrt_dcat,
    overwrite = TRUE
  )
  
  # update statsbase voor vwmd en volgende vwmd
  ops_updaten_stat_dsrt(jrmnd)
  ops_updaten_stat_dsrt(volgende_jrmnd)
  
  # tabel ophalen
  df_db_tbl_statbase_dsrt <- con |>
    dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dsrt")) |> 
    dplyr::collect()
  
  # we verwachten drie rijen in totaal
  expect_equal(nrow(df_db_tbl_statbase_dsrt), 3)
  
  # we verwachten twee rijen met dezelfde verslagmaand
  df_res <- filter(df_db_tbl_statbase_dsrt, vsmd == vers_maand)
  expect_equal(nrow(df_res), 2)
  
  # we verwachten dat de verwerkingsmaanden hiervan verschillend zijn
  expect_true(all(df_res$vwmd == c(verw_maand, volgende_verw_maand)))
  
  # we verwachten dat de aantallen voor dezelfde verslagmaand hetzelfde zijn, 
  # onafhankelijk van andere aantallen in een verwerkingsmaand
  expect_equal(unique(df_res$tota_ruw), sum(aantal))
  expect_equal(unique(df_res$tota_gaaf), sum(aantal))
  
  DBI::dbDisconnect(con)
})


test_that("de 'Onbekend' categorie alleen over de runderen wordt verdeeld
          in de functie ops_updaten_stat_dcat", {
  
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
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  
  # we maken het totaal aantal runderen 1000, met 100 "onbekend"
  aantal_ruw <- c(100, 1000, 1000)
  aantal_gaaf <- c(100, 1000, 1000)
  gemg <- 100
  
  # we maken input data, rdvl tabel met maar een dier
  df <- tibble::tribble(
    ~vwmd,    ~wrkp, ~dsrt,
    verw_maand,  0, "Onbekend",
    verw_maand,  1, "Koeien", 
    verw_maand,  2, "Geiten", 
  ) |> dplyr::mutate(
    aant_ruw = aantal_ruw, 
    aant_gaaf = aantal_gaaf,
    vsmd = jrmnd$mnd1,
    slnm = paste0("SL:", wrkp)
  )
  # input data naar de database
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_microbase_rdvl"),
    value = df,
    append = TRUE
  )
  
  # de aggregatie functie grebruikt een database tabel om alle mogelijk 
  # diersoorten te aggregeren. 
  # We overschrijven de standaardwaarden voor de test
  df_dsrt_dcat <- data.frame(
    dsrt = c("Volwassen runderen", "Volwassen geiten"),
    dcat = c("Koeien", "Geiten")
  )
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
    value = df_dsrt_dcat,
    overwrite = TRUE
  )
  
  df_gemg <-  tibble::tribble(
    ~dcat,  ~gemg,
    "Koeien",   gemg,
    "Geiten", gemg,
  ) |> 
    dplyr::mutate(
      vwmd = verw_maand,
      vsmd = jrmnd$mnd1
    )
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_microbase_gemg"),
    value = df_gemg,
    append = TRUE
  )
  
  # Aggregatie en oplsaan gebeurt binnen dezelfe functie
  ops_updaten_stat_dcat(jrmnd)
  
  # tabel ophalen
  df_db_tbl_statbase_dcat <- con |> 
    dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dcat")) |> 
    dplyr::collect()
  
  
  # we verwachten twee rijen, "Onbekend" is weg
  expect_equal(nrow(df_db_tbl_statbase_dcat), 2)
  expect_equal(df_db_tbl_statbase_dcat$dcat, c("Geiten", "Koeien"))
  
  # we verwachten dat de het totale aantal is behouden
  expect_equal(sum(df_db_tbl_statbase_dcat$tota_gaaf), sum(aantal_gaaf))
  
  # we verwachten dat "Onbekend" alleen over de Koe categorie is verdeeld
  tot_rund <- df_db_tbl_statbase_dcat |>
    dplyr::filter(dcat == "Koeien") |> 
    dplyr::pull(tota_gaaf)
  
  tot_geit <- df_db_tbl_statbase_dcat |>
    dplyr::filter(dcat == "Geiten") |> 
    dplyr::pull(tota_gaaf)
  
  expect_true(tot_rund > tot_geit)
  
  DBI::dbDisconnect(con)
})

test_that("'Onbekend' op ratio over de rund diercategorieen wordt verdeeld
          in de ops_updaten_stat_dsrt functie", {

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
  
  jrmnd <- alg_vind_jaarmaanden(inputmap=verw_maand,
                                verschil=App$verschil_huidige_mnd_eerste_mnd)
  
  # we maken het totaal aantal runderen 1000, met 100 "onbekend"
  aantal <- c(100, 1000, 1000)
  
  # we maken input data, rdvl tabel met maar een dier
  df <- tibble::tribble(
    ~vwmd,    ~wrkp, ~dsrt,
    verw_maand,  0, "Onbekend",
    verw_maand,  1, "Koeien", 
    verw_maand,  2, "Geiten", 
  ) |> dplyr::mutate(
    aant_ruw = aantal, 
    aant_gaaf = aantal,
    vsmd = jrmnd$mnd1,
    slnm = paste0("SL:", wrkp)
  )
  # input data naar de database
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_microbase_rdvl"),
    value = df,
    append = TRUE
  )
  
  # de aggregatie functie grebruikt een database tabel om alle mogelijk 
  # diersoorten te aggregeren. 
  # We overschrijven de standaardwaarden voor de test
  df_dsrt_dcat <- data.frame(
    dsrt = c("Volwassen runderen", "Volwassen geiten"),
    dcat = c("Koeien", "Geiten")
  )
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id("unittest", "tbl_diersoorten_en_diercategorieen"),
    value = df_dsrt_dcat,
    overwrite = TRUE
  )
  
  # test de verdeling in de stats_dsrt functie
  ops_updaten_stat_dsrt(jrmnd)
  
  # tabel ophalen
  df_db_tbl_statbase_dsrt <- con |> 
    dplyr::tbl(dbplyr::in_schema("unittest", "tbl_statbase_dsrt")) |> 
    dplyr::collect()
  
  # we verwachten twee rijen met de diercategorie
  expect_equal(nrow(df_db_tbl_statbase_dsrt), 2)
  expect_equal(df_db_tbl_statbase_dsrt$dsrt, c("Volwassen geiten", "Volwassen runderen"))
  
  # we verwachten dat het totale aantal is behouden
  expect_equal(sum(df_db_tbl_statbase_dsrt$tota_ruw), sum(aantal))
  expect_equal(sum(df_db_tbl_statbase_dsrt$tota_gaaf), sum(aantal))
  
  # we verwachten door de 'Onbekend' toevoeging dat de rundcategorie groter is
  tot_rund <- df_db_tbl_statbase_dsrt |>
    dplyr::filter(dsrt == "Volwassen runderen") |>
    dplyr::pull(tota_ruw)
  
  tot_geit <- df_db_tbl_statbase_dsrt |>
    dplyr::filter(dsrt == "Volwassen geiten") |>
    dplyr::pull(tota_ruw)
  
  expect_true(tot_rund > tot_geit)
  
  DBI::dbDisconnect(con)
})
