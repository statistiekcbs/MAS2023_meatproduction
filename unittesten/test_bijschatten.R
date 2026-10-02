#' Naam        : test_bijschatten.R
#' Auteur(s)   : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module bevat unittesten voor de functies uit bijschatten.R
library(testthat)
library(lubridate)
library(readxl)

# source het te testen script
root_map <- here::here()
src_map <- file.path(root_map, 'src')
source(file.path(src_map, "bijschatten.R"))
source(file.path(src_map, "algemeen.R"))
##############################################################################
context("bij_maak_rond()")

test_that("de waarde correct rond wordt gemaakt", {

    # w is nan
    data <- NaN
    res  <- bij_maak_rond(w=data)
    verw <- 0
    expect_identical(object=res, expected=verw)

    # w < 10
    data <- 5
    res  <- bij_maak_rond(w=data)
    verw <- 5
    expect_identical(object=res, expected=verw)

    # w < 100
    data <- 52
    res  <- bij_maak_rond(w=data)
    verw <- 50
    expect_identical(object=res, expected=verw)

    # w < 500
    data <- 433
    res  <- bij_maak_rond(w=data)
    verw <- 450
    expect_identical(object=res, expected=verw)

    # w > 500
    data <- 888
    res  <- bij_maak_rond(w=data)
    verw <- 900
    expect_identical(object=res, expected=verw)

})


##############################################################################
context("bij_voeg_bijschattingen_toe() geen bijschatting")
test_that("geen waarde wordt bijgeschat", {

    # geen indicator
    data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                           vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                           '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                           ind1=c(""), ind2=c(""), ind3=c(""),
                           check.names=FALSE, stringsAsFactors=FALSE)
    data_def <- data.frame(vsmd=c(201711), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
    maanden_huidige_vm <- c('201712', '201801', '201802', '201803')

    res  <- bij_voeg_bijschattingen_toe(data, data_def, maanden_huidige_vm)

    verw_df <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                          vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                          '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                          ind1=c(""), ind2=c(""), ind3=c(""),
                          check.names=FALSE, stringsAsFactors=FALSE)
    expect_identical(object=res$df, expected=verw_df)

    verw_bs <- data.frame("slnm"=character(), "wrkp"=character(),
                          "dsrt"=character(), "vsmd"=character(),
                          "aant"=character(), stringsAsFactors=FALSE)
    expect_identical(object=res$df_bijschatting, expected=verw_bs)

})


##############################################################################
context("bij_voeg_bijschattingen_toe() wel bijschatting")
test_that("bijgeschatten plaatsvindt door waarde vorig jaar over te nemen", {

    # geen indicator
    data     <- data.frame(slnm=c("A"), wrkp=c("A"), dsrt=c("A"),
                           vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
                           '201712'=c(1000), '201801'=c(1000), '201802'=c(NA), '201803'=c(1000),
                           ind1=c(""), ind2=c(""), ind3=c(""),
                           check.names=FALSE, stringsAsFactors=FALSE)
    data_def <- data.frame(vsmd=c(201711), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(1000), stringsAsFactors=FALSE)
    maanden_huidige_vm <- c('201712', '201801', '201802', '201803')

    res  <- bij_voeg_bijschattingen_toe(data, data_def, maanden_huidige_vm)

    verw_df <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                          vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
                          '201712'=c(1000), '201801'=c(1000), '201802'=c(1000), '201803'=c(1000),
                          ind1=c(""), ind2=c(""), ind3=c(""),
                          check.names=FALSE, stringsAsFactors=FALSE)

    expect_identical(object=res$df, expected=verw_df)

    verw_bs <- data.frame("slnm"=c("A"), "wrkp"=c("A"),
                          "dsrt"=c("A"), "vsmd"=c("201802"),
                          "aant"=c("1000"), stringsAsFactors=FALSE) # waarde is string

    expect_identical(object=res$df_bijschatting, expected=verw_bs)

})

test_that("bijgeschatten plaatsvindt door 12-maandsgemiddelde over te nemen", {

    # geen indicator
    data     <- data.frame(slnm=c("A"), wrkp=c("A"), dsrt=c("A"),
                           vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
                           '201712'=c(1000), '201801'=c(1000), '201802'=c(1000), '201803'=c(NA),
                           ind1=c(""), ind2=c(""), ind3=c(""),
                           check.names=FALSE, stringsAsFactors=FALSE)
    data_def <- data.frame(vsmd=c(201711), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(1000), stringsAsFactors=FALSE)
    maanden_huidige_vm <- c('201712', '201801', '201802', '201803')

    res  <- bij_voeg_bijschattingen_toe(data, data_def, maanden_huidige_vm)

    verw_df <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                          vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
                          '201712'=c(1000), '201801'=c(1000), '201802'=c(1000), '201803'=c(1000),
                          ind1=c(""), ind2=c(""), ind3=c(""),
                          check.names=FALSE, stringsAsFactors=FALSE)

    expect_identical(object=res$df, expected=verw_df)

    verw_bs <- data.frame("slnm"=c("A"), "wrkp"=c("A"),
                          "dsrt"=c("A"), "vsmd"=c("201803"),
                          "aant"=c("1000"), stringsAsFactors=FALSE) # waarde is string

    expect_identical(object=res$df_bijschatting, expected=verw_bs)

})


##############################################################################
context("bij_voeg_bijschattingen_toe() toch geen bijschatting")
test_that("bijgeschatten niet plaatsvindt vanwege kalkoenen", {

    # geen indicator
    data     <- data.frame(slnm=c("A"), wrkp=c("A"), dsrt=c("Kalkoenen"),
                           vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
                           '201712'=c(1000), '201801'=c(1000), '201802'=c(NA), '201803'=c(NA),
                           ind1=c(""), ind2=c(""), ind3=c(""),
                           check.names=FALSE, stringsAsFactors=FALSE)
    data_def <- data.frame(vsmd=c(201711), wrkp=c("A"), slnm=c("A"), dsrt=c("Kalkoenen"), aant_gaaf=c(1000), stringsAsFactors=FALSE)
    maanden_huidige_vm <- c('201712', '201801', '201802', '201803')

    res  <- bij_voeg_bijschattingen_toe(data, data_def, maanden_huidige_vm)

    verw_df <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("Kalkoenen"),
                          vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
                          '201712'=c(1000), '201801'=c(1000), '201802'=c(NA), '201803'=c(NA),
                          ind1=c(""), ind2=c(""), ind3=c(""),
                          check.names=FALSE, stringsAsFactors=FALSE)
    verw_df[, 4:10] <- sapply(verw_df[, 4:10], as.numeric) # maak van de waarden numerics
    expect_identical(object=res$df, expected=verw_df)

    verw_bs <- data.frame("slnm"=character(), "wrkp"=character(),
                          "dsrt"=character(), "vsmd"=character(),
                          "aant"=character(), stringsAsFactors=FALSE)

    expect_identical(object=res$df_bijschatting, expected=verw_bs)

})

test_that("bijgeschatten niet plaatsvindt omdat 12-maandsgemiddelde < 100", {

    # geen indicator
    data     <- data.frame(slnm=c("A"), wrkp=c("A"), dsrt=c("A"),
                           vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
                           '201712'=c(1000), '201801'=c(1000), '201802'=c(1000), '201803'=c(NA),
                           ind1=c(""), ind2=c(""), ind3=c(""),
                           check.names=FALSE, stringsAsFactors=FALSE)
    data_def <- data.frame(vsmd=c(201711), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(50), stringsAsFactors=FALSE)
    maanden_huidige_vm <- c('201712', '201801', '201802', '201803')

    res  <- bij_voeg_bijschattingen_toe(data, data_def, maanden_huidige_vm)

    verw_df <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                          vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
                          '201712'=c(1000), '201801'=c(1000), '201802'=c(1000), '201803'=c(NA),
                          ind1=c(""), ind2=c(""), ind3=c(""),
                          check.names=FALSE, stringsAsFactors=FALSE)
    verw_df[, 4:10] <- sapply(verw_df[, 4:10], as.numeric) # maak van de waarden numerics

    expect_identical(object=res$df, expected=verw_df)

    verw_bs <- data.frame("slnm"=character(), "wrkp"=character(),
                          "dsrt"=character(), "vsmd"=character(),
                          "aant"=character(), stringsAsFactors=FALSE)

    expect_identical(object=res$df_bijschatting, expected=verw_bs)

})

