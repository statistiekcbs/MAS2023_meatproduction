#' Naam        : test_algemeen.R
#' Auteur(s)   : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module bevat unittesten voor de functies uit algemeen.R
library(testthat)
library(lubridate)
library(readxl)

# source het te testen script
root_map <- here::here()
tst_map <- file.path(root_map, "unittesten")
tst_data_map <- file.path(tst_map, "testdata")
src_map <- file.path(root_map, 'src')
source(file.path(src_map, "algemeen.R"))
##############################################################################
context("alg_vind_default_inputmap()") # niet testen, omdat je dan de functie gewoon nabouwt

##############################################################################
context("alg_vind_ctcr_bestanden()") # niet testen, omdat je dan de functie gewoon nabouwt

##############################################################################
context("alg_vind_analysebestanden()") # niet testen, omdat je dan de functie gewoon nabouwt

##############################################################################
context("alg_vind_outputbestanden()") # niet testen, omdat je dan de functie gewoon nabouwt

##############################################################################
context("alg_hercodeer_maand()") # niet testen, omdat je dan de functie gewoon nabouwt

##############################################################################
context("alg_controleer_maken_ctcr()")

# basic flow
test_that("de input correct en volledig is", {
  # default waarden voor alle testinstanties binnen deze test
  # testmap <- ".\\testdata\\alg_controleer_maken_ctcr"
  testmap <- file.path(tst_data_map, "alg_controleer_maken_ctcr")
  input_aant_rdvl <- file.path(testmap, "CBS Roodvlees jan 2017 tm dec 2017   23-01-2018.xls")
  input_aant_wtvl <- file.path(testmap, "CBS Witvlees jan 2017 tm dec 2017   23-01-2018.xls")
  input_verd_gemg <- file.path(testmap, "Melding CBS per maand gem gewicht.xls")
  input_gemg      <- file.path(testmap, "Slachtgewichten Kalveren tm november CBS 2017.xlsx")
  input_gemg_vj   <- file.path(testmap, "Slachtgewichten Kalveren tm november CBS 2018.xlsx")
  ctcr_aant_rdvl  <- file.path(testmap, "ontbreekt_aantallen_roodvlees.xlsx")
  ctcr_aant_wtvl  <- file.path(testmap, "ontbreekt_aantallen_witvlees.xlsx")
  ctcr_verd       <- file.path(testmap, "ontbreekt_aantallenverdelingen.xlsx")
  ctcr_gemg       <- file.path(testmap, "ontbreekt_gemiddeldegewichten.xlsx")

  # geen jaarovergang
  jrmnd <- list(mnd1="201708",mnd2="201709",mnd3="201709",mnd4="201710",vwmd="201710")
  res <- alg_controleer_maken_ctcr(input_aant_rdvl, input_aant_wtvl,
                                   input_verd_gemg, input_gemg, input_gemg_vj,
                                   ctcr_aant_rdvl, ctcr_aant_wtvl,
                                   ctcr_verd, ctcr_gemg, jrmnd)
  expect_identical(object=res$file_rdvl_ok, expected=TRUE)
  expect_identical(object=res$file_wtvl_ok, expected=TRUE)
  expect_identical(object=res$file_verd_gemg_ok, expected=TRUE)
  expect_identical(object=res$file_gemg_kalv_ok, expected=TRUE)
  expect_identical(object=res$file_gemg_kalv_vj_ok, expected=TRUE)
  expect_identical(object=res$sh_verd_gemg_ok, expected=TRUE)
  expect_identical(object=res$sh_gemg_kalv_ok, expected=TRUE)
  expect_identical(object=res$sh_gemg_kalv_vj_ok, expected=TRUE)
  expect_identical(object=res$ctcr_rdvl_ok, expected=TRUE)
  expect_identical(object=res$ctcr_wtvl_ok, expected=TRUE)
  expect_identical(object=res$ctcr_verd_ok, expected=TRUE)
  expect_identical(object=res$ctcr_gemg_ok, expected=TRUE)
  expect_identical(object=res$txt_rdvl, expected="")
  expect_identical(object=res$txt_wtvl, expected="")
  expect_identical(object=res$txt_verd, expected="")
  expect_identical(object=res$txt_gemg, expected="")

  # inclusief jaarovergang
  jrmnd <- list(mnd1="201710",mnd2="201711",mnd3="201712",mnd4="201801",vwmd="201801")
  res <- alg_controleer_maken_ctcr(input_aant_rdvl, input_aant_wtvl,
                                   input_verd_gemg, input_gemg, input_gemg_vj,
                                   ctcr_aant_rdvl, ctcr_aant_wtvl,
                                   ctcr_verd, ctcr_gemg, jrmnd)
  expect_identical(object=res$file_rdvl_ok, expected=TRUE)
  expect_identical(object=res$file_wtvl_ok, expected=TRUE)
  expect_identical(object=res$file_verd_gemg_ok, expected=TRUE)
  expect_identical(object=res$file_gemg_kalv_ok, expected=TRUE)
  expect_identical(object=res$file_gemg_kalv_vj_ok, expected=TRUE)
  expect_identical(object=res$sh_verd_gemg_ok, expected=TRUE)
  expect_identical(object=res$sh_gemg_kalv_ok, expected=TRUE)
  expect_identical(object=res$sh_gemg_kalv_vj_ok, expected=TRUE)
  expect_identical(object=res$ctcr_rdvl_ok, expected=TRUE)
  expect_identical(object=res$ctcr_wtvl_ok, expected=TRUE)
  expect_identical(object=res$ctcr_verd_ok, expected=TRUE)
  expect_identical(object=res$ctcr_gemg_ok, expected=TRUE)
  expect_identical(object=res$txt_rdvl, expected="")
  expect_identical(object=res$txt_wtvl, expected="")
  expect_identical(object=res$txt_verd, expected="")
  expect_identical(object=res$txt_gemg, expected="")
})

# alternative flow
test_that("de input niet volledig is", {
  # default waarden voor alle testinstanties binnen deze test
  # testmap <- ".\\testdata\\alg_controleer_maken_ctcr"
  testmap <- file.path(tst_data_map, "alg_controleer_maken_ctcr")
  input_aant_rdvl <- file.path(testmap, "CBS Roodvlees jan 2017 tm dec 2017   23-01-2018.xls")
  input_aant_wtvl <- file.path(testmap, "CBS Witvlees jan 2017 tm dec 2017   23-01-2018.xls")
  input_verd_gemg <- file.path(testmap, "Melding CBS per maand gem gewicht.xls")
  input_gemg      <- file.path(testmap, "Slachtgewichten Kalveren tm november CBS 2017.xlsx")
  input_gemg_vj   <- file.path(testmap, "Slachtgewichten Kalveren tm november CBS 2018.xlsx")
  ctcr_aant_rdvl  <- file.path(testmap, "ontbreekt_aantallen_roodvlees.xlsx")
  ctcr_aant_wtvl  <- file.path(testmap, "ontbreekt_aantallen_witvlees.xlsx")
  ctcr_verd       <- file.path(testmap, "ontbreekt_aantallenverdelingen.xlsx")
  ctcr_gemg       <- file.path(testmap, "ontbreekt_gemiddeldegewichten.xlsx")

  # inputbestanden ontbreken
  jrmnd <- list(mnd1="201710",mnd2="201711",mnd3="201712",mnd4="201801",vwmd="201802")
  input_aant_rdvl <- file.path(testmap, "xxx.xls")
  input_aant_wtvl <- file.path(testmap, "xxx.xls")
  input_verd_gemg <- file.path(testmap, "xxx.xls")
  input_gemg <- file.path(testmap, "xxx.xls")
  input_gemg_vj <- file.path(testmap, "xxx.xls")
  res <- alg_controleer_maken_ctcr(input_aant_rdvl, input_aant_wtvl,
                                   input_verd_gemg, input_gemg, input_gemg_vj,
                                   ctcr_aant_rdvl, ctcr_aant_wtvl,
                                   ctcr_verd, ctcr_gemg, jrmnd)
  verw_txt_rdvl_ok <- "levering aantallen roodvlees bestaat niet"
  expect_identical(object=res$file_rdvl_ok, expected=FALSE)
  expect_identical(object=res$txt_rdvl, expected=verw_txt_rdvl_ok)
  verw_txt_wtvl_ok <- "levering aantallen witvlees bestaat niet"
  expect_identical(object=res$file_wtvl_ok, expected=FALSE)
  expect_identical(object=res$txt_wtvl, expected=verw_txt_wtvl_ok)
  verw_txt_verd_gemg_ok <- "levering aantallenverdelingen en gemiddelde gewichten bestaat niet"
  expect_identical(object=res$file_verd_gemg_ok, expected=FALSE)
  expect_identical(object=res$txt_verd, expected=verw_txt_verd_gemg_ok)
  verw_txt_gemg_kalv_ok <- paste0("levering gemiddelde gewichten kalveren bestaat niet",
                                  "; levering gemiddelde gewichten kalveren vorig jaar bestaat niet")
  expect_identical(object=res$file_gemg_kalv_ok, expected=FALSE)
  expect_identical(object=res$file_gemg_kalv_vj_ok, expected=FALSE)
  expect_identical(object=res$txt_gemg, expected=verw_txt_gemg_kalv_ok)

  # zet defaultwaarden weer terug
  input_aant_rdvl <- file.path(testmap, "CBS Roodvlees jan 2017 tm dec 2017   23-01-2018.xls")
  input_aant_wtvl <- file.path(testmap, "CBS Witvlees jan 2017 tm dec 2017   23-01-2018.xls")
  input_verd_gemg <- file.path(testmap, "Melding CBS per maand gem gewicht.xls")
  input_gemg      <- file.path(testmap, "Slachtgewichten Kalveren tm november CBS 2017.xlsx")
  input_gemg_vj   <- file.path(testmap, "Slachtgewichten Kalveren tm november CBS 2018.xlsx")

  # inputbestand gem gewicht kalveren vj ontbreekt, maar gem gewicht kalv is er wel
  jrmnd <- list(mnd1="201710",mnd2="201711",mnd3="201712",mnd4="201801",vwmd="201802")
  input_gemg_vj <- file.path(testmap, "xxx.xls")
  res <- alg_controleer_maken_ctcr(input_aant_rdvl, input_aant_wtvl,
                                   input_verd_gemg, input_gemg, input_gemg_vj,
                                   ctcr_aant_rdvl, ctcr_aant_wtvl,
                                   ctcr_verd, ctcr_gemg, jrmnd)
  verw_txt_gemg_kalv_ok <- paste0("levering gemiddelde gewichten kalveren vorig jaar bestaat niet")
  expect_identical(object=res$file_gemg_kalv_vj_ok, expected=FALSE)
  expect_identical(object=res$txt_gemg, expected=verw_txt_gemg_kalv_ok)

  # zet defaultwaarden weer terug
  input_gemg_vj <- file.path(testmap, "Slachtgewichten Kalveren tm november CBS 2018.xlsx")

  # een tabblad ontbreekt zonder jaarovergang
  jrmnd <- list(mnd1="201906",mnd2="201907",mnd3="201908",mnd4="201909",vwmd="201909")
  res <- alg_controleer_maken_ctcr(input_aant_rdvl, input_aant_wtvl,
                                   input_verd_gemg, input_gemg, input_gemg_vj,
                                   ctcr_aant_rdvl, ctcr_aant_wtvl,
                                   ctcr_verd, ctcr_gemg, jrmnd)
  verw_txt_verd_gemg <- "tabblad 2019 ontbreekt in levering aantallenverdelingen en gemiddelde gewichten"
  expect_identical(object=res$sh_verd_gemg_ok, expected=FALSE)
  expect_identical(object=res$txt_verd, expected=verw_txt_verd_gemg)
  verw_txt_gemg <- paste("tabblad Slachtgewichten Kalveren 2019 ontbreekt in levering gemiddelde gewichten kalveren")
  expect_identical(object=res$sh_gemg_kalv_ok, expected=FALSE)
  expect_identical(object=res$sh_gemg_kalv_vj_ok, expected=TRUE)
  expect_identical(object=res$txt_gemg, expected=verw_txt_gemg)

  # tabbladen ontbreekt met jaarovergang
  jrmnd <- list(mnd1="201910",mnd2="201911",mnd3="201912",mnd4="202001",vwmd="202001")
  res <- alg_controleer_maken_ctcr(input_aant_rdvl, input_aant_wtvl,
                                   input_verd_gemg, input_gemg, input_gemg_vj,
                                   ctcr_aant_rdvl, ctcr_aant_wtvl,
                                   ctcr_verd, ctcr_gemg, jrmnd)
  verw_txt_verd_gemg <- "tabblad 2019 of 2020 ontbreekt in levering aantallenverdelingen en gemiddelde gewichten"
  expect_identical(object=res$sh_verd_gemg_ok, expected=FALSE)
  expect_identical(object=res$txt_verd, expected=verw_txt_verd_gemg)
  verw_txt_gemg <- paste("tabblad Slachtgewichten Kalveren 2020 ontbreekt in levering gemiddelde gewichten kalveren;",
                         "tabblad Slachtgewichten Kalveren 2019 ontbreekt in levering gemiddelde gewichten kalveren vorig jaar")
  expect_identical(object=res$sh_gemg_kalv_ok, expected=FALSE)
  expect_identical(object=res$sh_gemg_kalv_vj_ok, expected=FALSE)
  expect_identical(object=res$txt_gemg, expected=verw_txt_gemg)

  # tabbladen ontbreekt met jaarovergang alleen in gemiddelde gewichten kalveren vj
  jrmnd <- list(mnd1="201710",mnd2="201711",mnd3="201712",mnd4="201801",vwmd="201802")
  input_gemg_vj <- file.path(testmap, "Slachtgewichten Kalveren tm november CBS 2017_tb_ontbreekt.xlsx")
  res <- alg_controleer_maken_ctcr(input_aant_rdvl, input_aant_wtvl,
                                   input_verd_gemg, input_gemg, input_gemg_vj,
                                   ctcr_aant_rdvl, ctcr_aant_wtvl,
                                   ctcr_verd, ctcr_gemg, jrmnd)
  expect_identical(object=res$sh_verd_gemg_ok, expected=TRUE)
  expect_identical(object=res$sh_gemg_kalv_ok, expected=TRUE)
  verw_txt_gemg <- paste("tabblad Slachtgewichten Kalveren 2017 ontbreekt in levering gemiddelde gewichten kalveren vorig jaar")
  expect_identical(object=res$sh_gemg_kalv_vj_ok, expected=FALSE)
  expect_identical(object=res$txt_gemg, expected=verw_txt_gemg)

  # zet defaultwaarden weer terug
  input_gemg_vj <- file.path(testmap, "Slachtgewichten Kalveren tm november CBS 2018.xlsx")

  # ctcr-bestand bestaat al
  jrmnd <- list(mnd1="201708",mnd2="201709",mnd3="201709",mnd4="201710",vwmd="201710")
  ctcr_aant_rdvl  <- file.path(testmap, "analysebestand_aantallen_roodvleesXXXX.xlsx")
  ctcr_aant_wtvl  <- file.path(testmap, "analysebestand_aantallen_witvleesXXXX.xlsx")
  ctcr_verd       <- file.path(testmap, "analysebestand_aantallenverdelingenXXXX.xlsx")
  ctcr_gemg       <- file.path(testmap, "analysebestand_gemiddeldegewichtenXXXX.xlsx")
  res <- alg_controleer_maken_ctcr(input_aant_rdvl, input_aant_wtvl,
                                   input_verd_gemg, input_gemg, input_gemg_vj,
                                   ctcr_aant_rdvl, ctcr_aant_wtvl,
                                   ctcr_verd, ctcr_gemg, jrmnd)
  verw_txt_rdvl_ok <- "bestand analysebestand_aantallen_roodvleesXXXX.xlsx bestaat al en wordt niet overschreven"
  expect_identical(object=res$ctcr_rdvl_ok, expected=FALSE)
  expect_identical(object=res$txt_rdvl, expected=verw_txt_rdvl_ok)
  verw_txt_wtvl_ok <- "bestand analysebestand_aantallen_witvleesXXXX.xlsx bestaat al en wordt niet overschreven"
  expect_identical(object=res$ctcr_wtvl_ok, expected=FALSE)
  expect_identical(object=res$txt_wtvl, expected=verw_txt_wtvl_ok)
  verw_txt_verd_ok <- "bestand analysebestand_aantallenverdelingenXXXX.xlsx bestaat al en wordt niet overschreven"
  expect_identical(object=res$ctcr_verd_ok, expected=FALSE)
  expect_identical(object=res$txt_verd, expected=verw_txt_verd_ok)
  verw_txt_gemg_ok <- "bestand analysebestand_gemiddeldegewichtenXXXX.xlsx bestaat al en wordt niet overschreven"
  expect_identical(object=res$ctcr_gemg_ok, expected=FALSE)
  expect_identical(object=res$txt_gemg, expected=verw_txt_gemg_ok)
})

##############################################################################
context("alg_controleer_opslaan_in_database()")
test_that("het opslaan mag doorgaan", {
    input_aant_rdvl <- file.path(tst_data_map, "alg_controleer_opslaan_in_database", "CBS Roodvlees jan 2017 tm dec 2017   23-01-2018.xls")
    input_aant_wtvl <- file.path(tst_data_map, "alg_controleer_opslaan_in_database", "CBS Witvlees jan 2017 tm dec 2017   23-01-2018.xls")
    ctcr_aant_rdvl <- file.path(tst_data_map, "alg_controleer_opslaan_in_database", "analysebestand_aantallen_roodvlees.xlsx")
    ctcr_aant_wtvl <- file.path(tst_data_map, "alg_controleer_opslaan_in_database", "analysebestand_aantallen_witvlees.xlsx")
    ctcr_verd      <- file.path(tst_data_map, "alg_controleer_opslaan_in_database", "analysebestand_aantallenverdelingen.xlsx")
    ctcr_gemg      <- file.path(tst_data_map, "alg_controleer_opslaan_in_database", "analysebestand_gemiddeldegewichten.xlsx")
    res <- alg_controleer_opslaan_in_database(input_aant_rdvl, input_aant_wtvl,
                                              ctcr_aant_rdvl, ctcr_aant_wtvl,
                                              ctcr_verd, ctcr_gemg)
    expect_identical(object=res$bestanden_ok, expected=TRUE)
    expect_identical(object=res$txt, expected="")
})

test_that("het opslaan niet mag doorgaan", {
    input_aant_rdvl <-file.path(tst_data_map, "alg_controleer_opslaan_in_database", "CBS Roodvlees jan 2017 tm dec 2017   23-01-2018.xls")
    input_aant_wtvl <-file.path(tst_data_map, "alg_controleer_opslaan_in_database", "CBS Witvlees jan 2017 tm dec 2017   23-01-2018.xls")
    ctcr_aant_rdvl <- file.path(tst_data_map, "alg_controleer_opslaan_in_database", "analysebestand_aantallen_roodvlees.xlsx")
    ctcr_aant_wtvl <- file.path(tst_data_map, "alg_controleer_opslaan_in_database", "analysebestand_aantallen_witvlees.xlsx")
    ctcr_verd      <- file.path(tst_data_map, "alg_controleer_opslaan_in_database", "analysebestand_aantallenverdelingen.xlsx")
    ctcr_gemg      <- file.path(tst_data_map, "alg_controleer_opslaan_in_database", "XXXXX_gemiddeldegewichten.xlsx")
    res <- alg_controleer_opslaan_in_database(input_aant_rdvl, input_aant_wtvl,
                                              ctcr_aant_rdvl, ctcr_aant_wtvl,
                                              ctcr_verd, ctcr_gemg)
    verw <- paste(" de controle- en correctiebestanden zijn nog niet",
                  "compleet; er zijn geen gegevens opgeslagen in de",
                  "database")
    expect_identical(object=res$bestanden_ok, expected=FALSE)
    expect_identical(object=res$txt, expected=verw)
})

##############################################################################
context("alg_vind_selectie()")

# goedpad
test_that("het juiste bestand wordt gevonden bij 1 bestand van elk type", {
    # variabelen geldig voor alle testen in deze sectie
    data1 <- file.path(tst_data_map, "alg_vind_selectie_a", "201703")
    verw_sel <- c("CBS Roodvlees jan 2017 tm dec 2017   23-01-2018.xls",
                  "CBS Witvlees jan 2017 tm dec 2017   23-01-2018.xls",
                  "Melding CBS per maand gem gewicht.xls",
                  "Slachtgewichten Kalveren tm november CBS 2016.xlsx",
                  "Slachtgewichten Kalveren tm november CBS 2017.xlsx")
    # roodvlees
    data2 <- "input_aant_rdvl"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2,
                               verschil=-3)
    expect_identical(object=res$selectie, expected=verw_sel)
    verw_kies <- c("CBS Roodvlees jan 2017 tm dec 2017   23-01-2018.xls")
    expect_identical(object=res$kies, expected=verw_kies)

    # witvlees
    data2 <- "input_aant_wtvl"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2,
                               verschil=-3)
    expect_identical(object=res$selectie, expected=verw_sel)
    verw_kies <- c("CBS Witvlees jan 2017 tm dec 2017   23-01-2018.xls")
    expect_identical(object=res$kies, expected=verw_kies)

    # gemiddelde gewichten en aantallenverdeling roodvlees
    data2 <- "input_gemg_rdvl"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2,
                               verschil=-3)
    expect_identical(object=res$selectie, expected=verw_sel)
    verw_kies <- c("Melding CBS per maand gem gewicht.xls")
    expect_identical(object=res$kies, expected=verw_kies)

    # gemiddeld gewicht kalveren
    data2 <- "input_gemg_kalveren"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2,
                               verschil=-3)
    expect_identical(object=res$selectie, expected=verw_sel)
    verw_kies <- c("Slachtgewichten Kalveren tm november CBS 2017.xlsx")
    expect_identical(object=res$kies, expected=verw_kies)

    # gemiddeld gewicht kalveren vorig jaar
    data2 <- "input_gemg_kalveren_vj"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2,
                               verschil=-3)
    expect_identical(object=res$selectie, expected=verw_sel)
    verw_kies <- c("Slachtgewichten Kalveren tm november CBS 2016.xlsx")
    expect_identical(object=res$kies, expected=verw_kies)
})

test_that("het gemiddelde gewichten kalveren niet nodig is", {
    data1 <- file.path(tst_data_map, "alg_vind_selectie_a", "201710")

    # gemiddeld gewicht kalveren vorig jaar
    data2 <- "input_gemg_kalveren_vj"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2,
                               verschil=-3)
    verw_sel <- c("<Geen selectie nodig...>")
    expect_identical(object=res$selectie, expected=verw_sel)
    verw_kies <- c("<Geen selectie nodig...>")
    expect_identical(object=res$kies, expected=verw_kies)
})

test_that("het juiste bestand wordt gevonden bij meerdere bestanden van elk type", {
    # variabelen geldig voor alle testen in deze sectie
    data1 <- file.path(tst_data_map, "alg_vind_selectie_b", "201703")
    verw_sel <- c("<Selecteer...>",
                  "CBS Roodvlees jan 2017 tm dec 2017   23-01-2018 - kopie.xls",
                  "CBS Roodvlees jan 2017 tm dec 2017   23-01-2018.xls",
                  "CBS Witvlees jan 2017 tm dec 2017   23-01-2018 - kopie.xls",
                  "CBS Witvlees jan 2017 tm dec 2017   23-01-2018.xls",
                  "Melding CBS per maand gem gewicht - kopie.xls",
                  "Melding CBS per maand gem gewicht.xls",
                  "Slachtgewichten Kalveren tm november CBS 2017 - kopie.xlsx",
                  "Slachtgewichten Kalveren tm november CBS 2017.xlsx")
    verw_kies <- c("<Selecteer...>")

    # roodvlees
    data2 <- "input_aant_rdvl"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2,
                               verschil=-3)
    expect_identical(object=res$selectie, expected=verw_sel)
    expect_identical(object=res$kies, expected=verw_kies)

    # witvlees
    data2 <- "input_aant_wtvl"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2,
                               verschil=-3)
    expect_identical(object=res$selectie, expected=verw_sel)
    expect_identical(object=res$kies, expected=verw_kies)

    # gemiddelde gewichten en aantallenverdeling roodvlees
    data2 <- "input_gemg_rdvl"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2,
                               verschil=-3)
    expect_identical(object=res$selectie, expected=verw_sel)
    expect_identical(object=res$kies, expected=verw_kies)

    # gemiddeld gewicht kalveren
    data2 <- "input_gemg_kalveren"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2,
                               verschil=-3)
    expect_identical(object=res$selectie, expected=verw_sel)
    expect_identical(object=res$kies, expected=verw_kies)
})

test_that("geen bestand wordt gevonden bij geen bestand van elk type", {
    # variabelen geldig voor alle testen in deze sectie
    data1 <- file.path(tst_data_map, "alg_vind_selectie_c", "201703")
    verw_sel <- c('<Geen bestanden gevonden...>')
    verw_kies <- c('<Geen bestanden gevonden...>')

    # roodvlees
    data2 <- "input_aant_rdvl"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2)
    expect_identical(object=res$selectie, expected=verw_sel)
    expect_identical(object=res$kies, expected=verw_kies)

    # witvlees
    data2 <- "input_aant_wtvl"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2)
    expect_identical(object=res$selectie, expected=verw_sel)
    expect_identical(object=res$kies, expected=verw_kies)

    # gemiddelde gewichten en aantallenverdeling roodvlees
    data2 <- "input_gemg_rdvl"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2)
    expect_identical(object=res$selectie, expected=verw_sel)
    expect_identical(object=res$kies, expected=verw_kies)

    # gemiddeld gewicht kalveren
    data2 <- "input_gemg_kalveren"
    res   <- alg_vind_selectie(jaarmaandmap=data1, selmenu=data2)
    expect_identical(object=res$selectie, expected=verw_sel)
    expect_identical(object=res$kies, expected=verw_kies)
})

# foutpad
test_that("het juiste bestand niet kan worden gevonden", {
    # jaarmaandmap bestaat niet
    data1 <- file.path(tst_map, "testdataXXX", "alg_vind_selectie_b", "201703")
    data2 <- "input_aant_rdv"
    expect_that(alg_vind_selectie(jaarmaandmap=data1, selmenu=data2),
                throws_error("object 'inputmap' not found"))

    # selmenu bestaat niet
    data1 <- file.path(tst_data_map, "alg_vind_selectie_a", "201703")
    data2 <- "input_aant_rdvXXX"
    expect_that(alg_vind_selectie(jaarmaandmap=data1, selmenu=data2),
                throws_error("object 'inputmap' not found"))
})

##############################################################################
context("alg_vind_plaatje()") # niet testen, omdat je dan de functie gewoon nabouwt

##############################################################################
context("alg_vind_jaarmaanden()")

# goedpad
test_that("de juiste jaarmaanden worden gevonden", {
    # geen jaarovergang
    data <- "D:\\testmap\\201806"
    res  <- alg_vind_jaarmaanden(inputmap=data, verschil=-3)
    verw <- list(mnd1="201803", mnd2="201804", mnd3="201805", mnd4="201806",
                 vwmd="201806", vvwmd="201805")
    expect_identical(object=res, expected=verw)

    # verslagmaanden gaan over kalenderjaargrens heen
    data <- "D:\\testmap\\201802"
    res  <- alg_vind_jaarmaanden(inputmap=data, verschil=-3)
    verw <- list(mnd1="201711", mnd2="201712", mnd3="201801", mnd4="201802",
                 vwmd="201802", vvwmd="201801")
    expect_identical(object=res, expected=verw)

    # pak laatste jaarmaand
    data <- "D:\\testmap\\201611\\201802"
    res  <- alg_vind_jaarmaanden(inputmap=data, verschil=-3)
    verw <- list(mnd1="201711", mnd2="201712", mnd3="201801", mnd4="201802",
                 vwmd="201802", vvwmd="201801")
    expect_identical(object=res, expected=verw)

    # verschil is niet-negatief
    data <- "D:\\testmap\\201703"
    res  <- alg_vind_jaarmaanden(inputmap=data, verschil=3)
    verw <- list(mnd1="201706", mnd2="201707", mnd3="201708", mnd4="201709",
                 vwmd="201703", vvwmd="201702")
    expect_identical(object=res, expected=verw)
})

# foutpad
test_that("de juiste jaarmaanden niet kunnen gevonden", {
    # inputmap eindigt niet op jaarmaand
    data <- "D:\\testmap\\2018XX"
    expect_that(alg_vind_jaarmaanden(inputmap=data, verschil=-5),
                throws_error("character string is not in a standard unambiguous format"))

    # Verschil geen integer
    data <- "D:\\testmap\\201805"
    expect_that(alg_vind_jaarmaanden(inputmap=data, verschil='min vijf'),
                throws_error())
    #Foutmelding: error in evaluating the argument 'e2' in selecting a method for function '%m+%':
    #    no applicable method for 'months' applied to an object of class "character"
})

##############################################################################
context("alg_is_leeg()")

# goedpad
test_that("de waarde als leeg wordt aangeduid", {
  w <- ""
  res  <- alg_is_leeg(w=w)
  verw <- TRUE
  expect_identical(object=res, expected=verw)

  w <- ''
  res  <- alg_is_leeg(w=w)
  verw <- TRUE
  expect_identical(object=res, expected=verw)

  w <- " "
  res  <- alg_is_leeg(w=w)
  verw <- TRUE
  expect_identical(object=res, expected=verw)

  w <- "   "
  res  <- alg_is_leeg(w=w)
  verw <- TRUE
  expect_identical(object=res, expected=verw)

  w <- NA
  res  <- alg_is_leeg(w=w)
  verw <- TRUE
  expect_identical(object=res, expected=verw)

  w <- NA_character_
  res  <- alg_is_leeg(w=w)
  verw <- TRUE
  expect_identical(object=res, expected=verw)

  w <- NA_integer_
  res  <- alg_is_leeg(w=w)
  verw <- TRUE
  expect_identical(object=res, expected=verw)

  w <- NA_complex_
  res  <- alg_is_leeg(w=w)
  verw <- TRUE
  expect_identical(object=res, expected=verw)

  w <- NA_real_
  res  <- alg_is_leeg(w=w)
  verw <- TRUE
  expect_identical(object=res, expected=verw)

  w <- " leeg "
  res  <- alg_is_leeg(w=w)
  verw <- FALSE
  expect_identical(object=res, expected=verw)

})

##############################################################################
context("controleer_inlees_bestand()")

test_that("een inleesbestand die de vorige keer is verwerkt, 
          kan niet nog een keer verwerkt worden", {
      
      levering <- data.frame(
      info_1 = letters[1:2],
      info_2 = LETTERS[1:2]
    )
    
    pad_levering <- withr::local_tempfile(pattern = "slachtingen_20240610-20241010 ", fileext = ".csv")
    write.csv2(levering, pad_levering)
    
    db_levering <- data.frame(
      levering_id = 1,
      bestandshash = cli::hash_file_sha256(pad_levering),
             bestandsnaam = basename(pad_levering),
             verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ) 
    
    pad_configbestand <- file.path(tst_data_map, "voorbeeld_config.ini")
    App <- yaml::yaml.load_file(
      input = pad_configbestand,
      eval.expr = TRUE
    )
    
    con <- DBI::dbConnect(odbc::odbc(), 
                          Driver = App$dbdrivernaam, 
                          Server = App$dbservernaam, 
                          Database = App$databasenaam)
    test_schema = "unittest"
    
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(test_schema, "temp_tbl"),
      value = db_levering,
      overwrite = TRUE
    )
    
    nieuw_pad <- withr::local_tempfile(pattern = "slachtingen_20240610-20241010 ", fileext = ".csv")
    write.csv2(levering, nieuw_pad)
    
    expect_error(controleer_inlees_bestand(nieuw_pad,
                                           con = con,
                                           dbschema = test_schema,
                                           tbl_naam = "temp_tbl"))
    
    DBI::dbDisconnect(con)
    
          })

test_that("een inleesbestand die niet eerder is verwerkt, 
          wordt niet tegengehouden", {
            
            levering <- data.frame(
              info_1 = letters[1:2],
              info_2 = LETTERS[1:2]
            )
            
            pad_levering <- withr::local_tempfile(pattern = "slachtingen_20240610-20241010 ", fileext = ".csv")
            write.csv2(levering, pad_levering)
            
            db_levering <- data.frame(
              levering_id = 1,
              bestandshash = cli::hash_file_sha256(pad_levering),
              bestandsnaam = basename(pad_levering),
              verwerkingsdatum = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            ) 
            
            pad_configbestand <- file.path(tst_data_map, "voorbeeld_config.ini")
            App <- yaml::yaml.load_file(
              input = pad_configbestand,
              eval.expr = TRUE
            )
            
            con <- DBI::dbConnect(odbc::odbc(), 
                                  Driver = App$dbdrivernaam, 
                                  Server = App$dbservernaam, 
                                  Database = App$databasenaam)
            test_schema = "unittest"
            
            DBI::dbWriteTable(
              conn = con,
              name = DBI::Id(test_schema, "temp_tbl"),
              value = db_levering,
              overwrite = TRUE
            )
            
            nieuwe_levering <- data.frame(
              info_1 = letters[3:4],
              info_2 = LETTERS[3:4]
            )
            
            nieuw_pad <- withr::local_tempfile(pattern = "slachtingen_20240610-20241010 ", fileext = ".csv")
            write.csv2(nieuwe_levering, nieuw_pad)
            
            expect_no_error(controleer_inlees_bestand(nieuw_pad,
                                                   con = con,
                                                   dbschema = test_schema,
                                                   tbl_naam = "temp_tbl"))
            
            DBI::dbDisconnect(con)
            
          })
