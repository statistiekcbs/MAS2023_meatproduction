#' Naam        : test_inlezen.R
#' Auteur(s)   : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module bevat unittesten voor de functies uit inlezen.R
library(testthat)
library(lubridate)
library(plyr)
library(readxl)
library(logging)

# source het te testen script
root_map <- here::here()
src_map <- file.path(root_map, 'src')
tst_map <- file.path(root_map, "unittesten")
tst_data_map <- file.path(tst_map, "testdata")
source(file.path(src_map, "inlezen.R"))

##############################################################################
context("inl_maak_draaitabel()")

# goedpad
test_that("een correcte draaitabel gemaakt wordt", {
    ######### Basissituatie ##################################################
    # definieer testdata
    df  <- data.frame('vwmd'=c('201801', '201801'),
                      'vsmd'=c('201712', '201712'),
                      'slnm'=c('s1', 's2'),
                      'wrkp'=c('w1', 'w2'),
                      'dsrt'=c('Lammeren', 'Kalveren'),
                      'aant'=c(100, 200))
    vsmd <- c('201712')
    # definieer verwachting
    df_verw <- data.frame('vwmd'=c('201801', '201801'),
                          'slnm'=c('s1', 's2'),
                          'wrkp'=c('w1', 'w2'),
                          'dsrt'=c('Lammeren', 'Kalveren'),
                          '201712'=c(100, 200),
                          check.names=FALSE)
    df_verw[, c('201712')] <- sapply(df_verw[, c('201712')], as.character)
    # voer functie uit en valideer resultaat aan verwachting
    res <- inl_maak_draaitabel(df, vsmd)
    expect_identical(object=res, expected=df_verw)
    
    ######### Dataframe bevat mix NVWA RVO en extra kolommen: biologisch en bron
    # definieer testdata
    df  <- data.frame('vwmd'=c('201801', '201801'),
                      'vsmd'=c('201712', '201712'),
                      'slnm'=c('s1', 's2'),
                      'wrkp'=c('w1', 'w2'),
                      'dsrt'=c('Lammeren', 'Kalveren 0-8 mnd'),
                      'aant'=c(100, 200),
                      'is_biologisch'=c(NA, TRUE),
                      'bron'=c('NVWA', 'RVO'))
    vsmd <- c('201712')
    # definieer verwachting
    df_verw <- data.frame('vwmd'=c('201801', '201801'),
                          'slnm'=c('s1', 's2'),
                          'wrkp'=c('w1', 'w2'),
                          'dsrt'=c('Lammeren', 'Kalveren 0-8 mnd'),
                          '201712'=c(100, 200),
                          'is_biologisch'=c(NA, TRUE),
                          'bron'=c('NVWA', 'RVO'),
                          check.names=FALSE)
    df_verw[, c('201712')] <- sapply(df_verw[, c('201712')], as.character)
    # voer functie uit en valideer resultaat aan verwachting
    res <- inl_maak_draaitabel(df, vsmd)
    expect_identical(object=res, expected=df_verw)

    ######### NA's worden opgevuld met lege velden ###########################
    # definieer testdata
    df  <- data.frame('vwmd'=c('201801', '201801'),
                      'vsmd'=c('201711', '201712'),
                      'slnm'=c('s1', 's2'),
                      'wrkp'=c('w1', 'w2'),
                      'dsrt'=c('Lammeren', 'Kalveren'),
                      'aant'=c(100, 200))
    vsmd <- c('201711', '201712')
    # definieer verwachting
    df_verw <- data.frame('vwmd'=c('201801', '201801'),
                          'slnm'=c('s1', 's2'),
                          'wrkp'=c('w1', 'w2'),
                          'dsrt'=c('Lammeren', 'Kalveren'),
                          '201711'=c(100, ''),
                          '201712'=c('', 200),
                          check.names=FALSE)
    df_verw[, c('201711', '201712')] <- sapply(df_verw[, c('201711', '201712')],
                                               as.character)
    # voer functie uit en valideer resultaat aan verwachting
    res <- inl_maak_draaitabel(df, vsmd)
    expect_identical(object=res, expected=df_verw)

    ######### vsmd past niet bij df: geef leeg dataframe terug ###############
    # definieer testdata
    df  <- data.frame('vwmd'=c('201801', '201801'),
                      'vsmd'=c('201712', '201712'),
                      'slnm'=c('s1', 's2'),
                      'wrkp'=c('w1', 'w2'),
                      'dsrt'=c('Lammeren', 'Kalveren'),
                      'aant'=c(100, 200))
    vsmd <- c('201610', '201611')
    # definieer verwachting
    df_verw <- data.frame('vwmd'=character(),
                          'slnm'=character(),
                          'wrkp'=character(),
                          'dsrt'=character(),
                          '201610'=character(),
                          '201611'=character(),
                          check.names=FALSE, stringsAsFactors=FALSE)
    # voer functie uit en valideer resultaat aan verwachting
    res <- inl_maak_draaitabel(df, vsmd)
    expect_identical(object=res, expected=df_verw)

    ######### df bevat andere vsmd dan in vsmd: filter deze eruit ############
    # definieer testdata
    df  <- data.frame('vwmd'=c('201801', '201801'),
                      'vsmd'=c('201712', '201711'),
                      'slnm'=c('s1', 's2'),
                      'wrkp'=c('w1', 'w2'),
                      'dsrt'=c('Kalveren', 'Lammeren'),
                      'aant'=c(100, 200))
    vsmd <- c('201712')
    # definieer verwachting
    df_verw <- data.frame('vwmd'=c('201801'),
                          'slnm'=c('s1'),
                          'wrkp'=c('w1'),
                          'dsrt'=c('Kalveren'),
                          '201712'=c(100),
                          check.names=FALSE)
    df_verw[, c('201712')] <- sapply(df_verw[, c('201712')], as.character)
    # levels(df_verw$slnm) <- c("s1", "s2")
    # levels(df_verw$wrkp) <- c("w1", "w2")
    # levels(df_verw$dsrt) <- c("Kalveren", "Lammeren")
    # voer functie uit en valideer resultaat aan verwachting
    res <- inl_maak_draaitabel(df, vsmd)
    expect_identical(object=res, expected=df_verw)
})

# foutpad
 test_that("geen draaitabel gemaakt kan worden", {
    ######### verplichte kolom ontbreekt in df: geef leeg df terug ############
    # definieer testdata
    df  <- data.frame('vwmd'=c('201801', '201801'),
                      'vsmd'=c('201712', '201712'),
                      'wrkp'=c('w1', 'w2'),
                      'dsrt'=c('Lammeren', 'Kalveren'),
                      'aant'=c(100, 200))
    vsmd <- c('201711')
    # definieer verwachting
    df_verw <- data.frame('vwmd'=character(),
                          'slnm'=character(),
                          'wrkp'=character(),
                          'dsrt'=character(),
                          '201711'=character(),
                          check.names=FALSE, stringsAsFactors=FALSE)
    # voer functie uit en valideer resultaat aan verwachting
    res <- inl_maak_draaitabel(df, vsmd)
    expect_identical(object=res, expected=df_verw)

    ######### vsmd leeg: geef foutmelding terug ##############################
    # definieer testdata
    df  <- data.frame('vwmd'=c('201801', '201801'),
                      'vsmd'=c('201712', '201712'),
                      'slnm'=c('s1', 's2'),
                      'wrkp'=c('w1', 'w2'),
                      'dsrt'=c('Lammeren', 'Kalveren'),
                      'aant'=c(100, 200))
    vsmd <- c('')
    # definieer verwachting
    expect_that(inl_maak_draaitabel(df, vsmd),
                throws_error("undefined columns selected"))
})


##############################################################################
context("inl_sla_draaitabel_plat()")

# goedpad
test_that("een correcte platte tabel gemaakt wordt", {
    ######### Basissituatie ##################################################
    # definieer testdata
    df   <- data.frame('vwmd'=c('201801', '201801'),
                       'slnm'=c('s1', 's2'),
                       'wrkp'=c('w1', 'w2'),
                       'dsrt'=c('Lammeren', 'Kalveren'),
                       '201711'=c(100, 200),
                       '201712'=c(300, 400),
                       check.names=FALSE, stringsAsFactors=FALSE)
    kols_res <- c('vwmd', 'vsmd', 'slnm', 'wrkp', 'dsrt', 'aant')

    # definieer verwachting
    df_verw <- data.frame('vwmd'=c('201801', '201801', '201801', '201801'),
                          'vsmd'=c('201711', '201712', '201711', '201712'),
                          'slnm'=c('s1', 's1', 's2', 's2'),
                          'wrkp'=c('w1', 'w1', 'w2', 'w2'),
                          'dsrt'=c('Lammeren','Lammeren','Kalveren','Kalveren'),
                          'aant'=c(100, 300, 200, 400),
                          check.names=FALSE, stringsAsFactors=FALSE)
    df_verw[, c('aant')] <- sapply(df_verw[, c('aant')], as.character)
    # voer functie uit en valideer resultaat aan verwachting
    res <- inl_sla_draaitabel_plat(df, kols_res)
    expect_identical(object=res, expected=df_verw)

    ######### NA's worden overgeslagen ########################################
    # definieer testdata
    df   <- data.frame('vwmd'=c('201801', '201801'),
                       'slnm'=c('s1', 's2'),
                       'wrkp'=c('w1', 'w2'),
                       'dsrt'=c('Lammeren', 'Kalveren'),
                       '201711'=c(100, NA),
                       '201712'=c(NA, 400),
                       check.names=FALSE, stringsAsFactors=FALSE)
    kols_res <- c('vwmd', 'vsmd', 'slnm', 'wrkp', 'dsrt', 'aant')

    # definieer verwachting
    df_verw <- data.frame('vwmd'=c('201801', '201801'),
                          'vsmd'=c('201711', '201712'),
                          'slnm'=c('s1', 's2'),
                          'wrkp'=c('w1', 'w2'),
                          'dsrt'=c('Lammeren', 'Kalveren'),
                          'aant'=c(100, 400),
                          check.names=FALSE, stringsAsFactors=FALSE)
    df_verw[, c('aant')] <- sapply(df_verw[, c('aant')], as.character)
    # voer functie uit en valideer resultaat aan verwachting
    res <- inl_sla_draaitabel_plat(df, kols_res)
    expect_identical(object=res, expected=df_verw)

    ######### kols_res passen niet bij df: geef vage tabel terug ##############
    # definieer testdata
    df   <- data.frame('vwmd'=c('201801', '201801'),
                       'slnm'=c('s1', 's2'),
                       'wrkp'=c('w1', 'w2'),
                       'dsrt'=c('Lammeren', 'Kalveren'),
                       '201711'=c(100, 200),
                       '201712'=c(300, 400),
                       check.names=FALSE, stringsAsFactors=FALSE)
    kols_res <- c('vwmdX', 'vsmdX', 'slnmX', 'wrkpX', 'dsrtX', 'aantX')

    # definieer verwachting
    df_verw <- data.frame('vwmdX'=c(NA_character_, NA_character_, NA_character_, NA_character_),
                          'vsmdX'=c(NA_character_, NA_character_, NA_character_, NA_character_),
                          'slnmX'=c(NA_character_, NA_character_, NA_character_, NA_character_),
                          'wrkpX'=c(NA_character_, NA_character_, NA_character_, NA_character_),
                          'dsrtX'=c(NA_character_, NA_character_, NA_character_, NA_character_),
                          'aantX'=c(NA_character_, NA_character_, NA_character_, NA_character_),
                          'vsmd'=c(100, 300, 200, 400),
                          check.names=FALSE, stringsAsFactors=FALSE)
    df_verw[, c('vsmd')] <- sapply(df_verw[, c('vsmd')], as.character)
    # voer functie uit en valideer resultaat aan verwachting
    res <- inl_sla_draaitabel_plat(df, kols_res)
    expect_identical(object=res, expected=df_verw)
})

##############################################################################
context("inl_voeg_dsrt_toe()")

# goedpad
test_that("de kolom dsrt correct wordt toegevoegd", {
    ######### Basissituatie ##################################################
    # definieer testdata
    df   <- data.frame('dcat'=c('Stieren', 'Koeien', 'Vaarzen',
                                'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd'),
                       check.names=FALSE, stringsAsFactors=FALSE)
    # definieer verwachting
    df_verw <- data.frame('dcat'=c('Stieren', 'Koeien', 'Vaarzen',
                                   'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd'),
                          'dsrt'=c('Volwassen runderen','Volwassen runderen',
                                   'Volwassen runderen','Kalveren','Kalveren'),
                          check.names=FALSE, stringsAsFactors=FALSE)
    # voer functie uit en valideer resultaat aan verwachting
    res <- inl_voeg_dsrt_toe(df)
    expect_identical(object=res, expected=df_verw)

    ######### Voor onbekende dcat wordt dsrt NA ##############################
    # definieer testdata
    df   <- data.frame('dcat'=c('Bieren', 'Koeien', 'Vaarzen',
                                'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd'),
                       check.names=FALSE, stringsAsFactors=FALSE)
    # definieer verwachting
    df_verw <- data.frame('dcat'=c('Bieren', 'Koeien', 'Vaarzen',
                                   'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd'),
                          'dsrt'=c(NA_character_,'Volwassen runderen',
                                   'Volwassen runderen','Kalveren','Kalveren'),
                          check.names=FALSE, stringsAsFactors=FALSE)
    # voer functie uit en valideer resultaat aan verwachting
    res <- inl_voeg_dsrt_toe(df)
    expect_identical(object=res, expected=df_verw)
})

# foutpad
test_that("geen draaitabel gemaakt kan worden", {
    ######### kolom dcat ontbreekt: geef foutmelding terug ###################
    # definieer testdata
    df   <- data.frame('XXXX'=c('Stieren', 'Koeien', 'Vaarzen',
                                'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd'),
                       check.names=FALSE, stringsAsFactors=FALSE)
    # definieer verwachting
    expect_that(inl_voeg_dsrt_toe(df),
                throws_error("replacement has 0 rows, data has 5"))
})

##############################################################################
context("inl_lees_hist_aant()") # niet testen want alleen ophalen uit database

##############################################################################
context("inl_lees_hist_verd()") # niet testen want alleen ophalen uit database

##############################################################################
context("inl_lees_hist_gemg()") # niet testen want alleen ophalen uit database

##############################################################################
context("inl_hercodeer_rdvl_dsrt()")

# goedpad
test_that("de soort omschrijving goed wordt gehercodeerd", {
    so <- "Kalf"
    verw <- "Kalveren"
    res <- inl_hercodeer_rdvl_dsrt(so)
    expect_identical(object=res, expected=verw)

    so <- "Rund"
    verw <- "Volwassen runderen"
    res <- inl_hercodeer_rdvl_dsrt(so)
    expect_identical(object=res, expected=verw)

    so <- "Schaap jonger dan 1 jaar"
    verw <- "Lammeren"
    res <- inl_hercodeer_rdvl_dsrt(so)
    expect_identical(object=res, expected=verw)

    so <- "Schaap ouder dan 1 jaar"
    verw <- "Volwassen schapen"
    res <- inl_hercodeer_rdvl_dsrt(so)
    expect_identical(object=res, expected=verw)

    so <- "Varken"
    verw <- "Varkens"
    res <- inl_hercodeer_rdvl_dsrt(so)
    expect_identical(object=res, expected=verw)

    so <- "Eenhoevig dier"
    verw <- "Eenhoevige dieren"
    res <- inl_hercodeer_rdvl_dsrt(so)
    expect_identical(object=res, expected=verw)

    so <- "Geit"
    verw <- "Geiten"
    res <- inl_hercodeer_rdvl_dsrt(so)
    expect_identical(object=res, expected=verw)

    so <- "Lama"
    verw <- "Lama"
    res <- inl_hercodeer_rdvl_dsrt(so)
    expect_identical(object=res, expected=verw)
})

##############################################################################
context("inl_hercodeer_rdvl_SOORT_OMSCHRIJVING()")

# goedpad
test_that("de kolom SOORTOMSCHRIJVING goed wordt gehercodeerd", {
    df <- data.frame("SOORT OMSCHRIJVING"=c("Kalf", "Rund"),
                     check.names=FALSE,
                     stringsAsFactors=FALSE)
    verw <- data.frame("SOORT OMSCHRIJVING"=c("Kalf", "Rund"),
                       "dsrt"=c("Kalveren", "Volwassen runderen"),
                       check.names=FALSE,
                       stringsAsFactors=FALSE)
    res <- inl_hercodeer_rdvl_SOORT_OMSCHRIJVING(df)
    expect_identical(object=res, expected=verw)

    df <- data.frame("SOORT OMSCHRIJVING"=c("Kalf", "Rund", "Aap"),
                     check.names=FALSE,
                     stringsAsFactors=FALSE)
    verw <- data.frame("SOORT OMSCHRIJVING"=c("Kalf", "Rund", "Aap"),
                       "dsrt"=c("Kalveren", "Volwassen runderen", "Aap"),
                       check.names=FALSE,
                       stringsAsFactors=FALSE)
    res <- inl_hercodeer_rdvl_SOORT_OMSCHRIJVING(df)
    expect_identical(object=res, expected=verw)
})

##############################################################################
context("inl_lees_input_rdvl()")

# goedpad
test_that("het inputbestand roodvlees goed wordt ingelezen", {
    bst <- file.path(tst_data_map, "inl_lees_input_rdvl",
                     "CBS Roodvlees apr 2017 tm mrt 2018   16-04-2018.xls")
    jrmnd <- list(mnd1="201711", mnd4="201802", vwmd="201802")
    res <- inl_lees_input_rdvl(inputbestand=bst, jrmnd=jrmnd)
    
    # 1861 rijen verwacht
    expect_equal(object=nrow(res), expected=1861)
    # 7 kolommen verwacht
    expect_equal(
      object=names(res), 
      expected=c("vwmd", "vsmd", "wrkp", "slnm", "dsrt", "aant", "is_biologisch"))
    sel <- subset(res, vwmd == "201802" & vsmd == "201802" &
                       slnm == "BEDRIJFSNAAM 174" & dsrt == "Varkens")
    expect_identical(object=sel[,"aant"], expected=70609) # waarde in sheet
})

##############################################################################
context("inl_hercodeer_wtvl_MAAND()")

# goedpad
test_that("de maand goed wordt omgecodeerd", {
    df <- data.frame("MAAND" = c("januari", "februari", "maart", "april",
                                 "mei", "juni", "juli", "augustus", "september",
                                 "oktober", "november", "december"),
                     "NR" = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     stringsAsFactors=FALSE)
    df_verw <- data.frame("MAAND" = c("januari", "februari", "maart", "april",
                                      "mei", "juni", "juli", "augustus",
                                      "september", "oktober", "november",
                                      "december"),
                          "NR" = c(1,2,3,4,5,6,7,8,9,10,11,12),
                          "mnd" = c("01","02","03","04","05","06",
                                    "07","08","09","10","11","12"),
                          stringsAsFactors=FALSE)
    res <- inl_hercodeer_wtvl_MAAND(df)
    expect_identical(object=res, expected=df_verw)
})

test_that("de maand goed wordt omgecodeerd ongeacht hoofdletters", {
    df <- data.frame("MAAND" = c("JANUARI", "FEBRUARI", "MAART", "APRIL",
                                 "MEI", "JUNI", "JULI", "AUGUSTUS", "SEPTEMBER",
                                 "OKTOBER", "NOVEMBER", "DECEMBER"),
                     "NR" = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     stringsAsFactors=FALSE)
    df_verw <- data.frame("MAAND" = c("JANUARI", "FEBRUARI", "MAART", "APRIL",
                                      "MEI", "JUNI", "JULI", "AUGUSTUS",
                                      "SEPTEMBER", "OKTOBER", "NOVEMBER",
                                      "DECEMBER"),
                          "NR" = c(1,2,3,4,5,6,7,8,9,10,11,12),
                          "mnd" = c("01","02","03","04","05","06",
                                    "07","08","09","10","11","12"),
                          stringsAsFactors=FALSE)
    res <- inl_hercodeer_wtvl_MAAND(df)
    expect_identical(object=res, expected=df_verw)
})

##############################################################################
context("inl_hercodeer_wtvl_dsrt()")

# goedpad
test_that("de diersoort voor witvlees goed wordt gehercodeerd", {
    ds <- "Struisvogels"
    verw <- "Struisvogels"
    res <- inl_hercodeer_wtvl_dsrt(ds)
    expect_identical(object=res, expected=verw)

    ds <- "Duiven"
    verw <- "Duiven"
    res <- inl_hercodeer_wtvl_dsrt(ds)
    expect_identical(object=res, expected=verw)

    ds <- "Eenden"
    verw <- "Eenden"
    res <- inl_hercodeer_wtvl_dsrt(ds)
    expect_identical(object=res, expected=verw)

    ds <- "Kalkoenen"
    verw <- "Kalkoenen"
    res <- inl_hercodeer_wtvl_dsrt(ds)
    expect_identical(object=res, expected=verw)

    ds <- "Kippen"
    verw <- "Overige kippen"
    res <- inl_hercodeer_wtvl_dsrt(ds)
    expect_identical(object=res, expected=verw)

    ds <- "Parelhoenders"
    verw <- "Parelhoenders"
    res <- inl_hercodeer_wtvl_dsrt(ds)
    expect_identical(object=res, expected=verw)

    ds <- "Vleeskuikens"
    verw <- "Vleeskuikens"
    res <- inl_hercodeer_wtvl_dsrt(ds)
    expect_identical(object=res, expected=verw)

    ds <- "Fazanten"
    verw <- "Fazanten"
    res <- inl_hercodeer_wtvl_dsrt(ds)
    expect_identical(object=res, expected=verw)

    ds <- "Ganzen"
    verw <- "Ganzen"
    res <- inl_hercodeer_wtvl_dsrt(ds)
    expect_identical(object=res, expected=verw)

    ds <- "Patrijzen"
    verw <- "Patrijzen"
    res <- inl_hercodeer_wtvl_dsrt(ds)
    expect_identical(object=res, expected=verw)

    ds <- "Feniksen"
    verw <- "Feniksen"
    res <- inl_hercodeer_wtvl_dsrt(ds)
    expect_identical(object=res, expected=verw)
})

##############################################################################
context("inl_hercodeer_wtvl_DIERSOORT()")

# goedpad
test_that("de kolom DIERSOORT goed wordt gehercodeerd", {
    df <- data.frame("DIERSOORT"=c("Patrijzen", "Kippen"),
                     check.names=FALSE,
                     stringsAsFactors=FALSE)
    verw <- data.frame("DIERSOORT"=c("Patrijzen", "Kippen"),
                       "dsrt"=c("Patrijzen", "Overige kippen"),
                       check.names=FALSE,
                       stringsAsFactors=FALSE)
    res <- inl_hercodeer_wtvl_DIERSOORT(df)
    expect_identical(object=res, expected=verw)

    df <- data.frame("DIERSOORT"=c("Patrijzen", "Kippen", "Kolibries"),
                     check.names=FALSE,
                     stringsAsFactors=FALSE)
    verw <- data.frame("DIERSOORT"=c("Patrijzen", "Kippen", "Kolibries"),
                       "dsrt"=c("Patrijzen", "Overige kippen", "Kolibries"),
                       check.names=FALSE,
                       stringsAsFactors=FALSE)
    res <- inl_hercodeer_wtvl_DIERSOORT(df)
    expect_identical(object=res, expected=verw)

})

##############################################################################
context("inl_lees_input_wtvl()")

# goedpad
test_that("het inputbestand witvlees goed wordt ingelezen", {
    bst <- file.path(tst_data_map, "inl_lees_input_wtvl",
                     "CBS Witvlees apr 2017 tm mrt 2018   16-04-2018.xls")
    jrmnd <- list(mnd1="201711", mnd4="201802", vwmd="201802")
    res <- inl_lees_input_wtvl(inputbestand=bst, jrmnd=jrmnd)
    expect_equal(object=nrow(res), expected=93) # 93 rijen
    expect_equal(object=ncol(res), expected=6) # 6 kolommen
    sel <- subset(res, vwmd == "201802" & vsmd == "201802" &
                      slnm == "BEDRIJFSNAAM 24" & dsrt == "Vleeskuikens")
    expect_identical(object=sel[,"aant"], expected=26247) # waarde in sheet
})

##############################################################################
context("inl_lees_input_verd_gemg()")

# goedpad
test_that("het inputbestand voor aantallenverdelingen en gewichten goed wordt ingelezen", {
    bst <- file.path(tst_data_map, "inl_lees_input_verd_gemg",
                     "Melding CBS per maand gem gewicht.xls")
    # geen jaarovergang: een tabblad nodig
    jrmnd <- list(mnd1="201703", mnd4="201706", vwmd="201706")
    res <- inl_lees_input_verd_gemg(inputbestand=bst, jrmnd=jrmnd)
    # valideer res$df_verd
    expect_equal(object=nrow(res$df_verd), expected=3) # 3 rijen
    expect_equal(object=ncol(res$df_verd), expected=5) # 5 kolommen
    expect_identical(object=round(res$df_verd["Stieren","hvm_mnd4"], 5),
                     expected=round(13.32,5)) # waarde in sheet
    # valideer res$df_gemg
    expect_equal(object=nrow(res$df_gemg), expected=7) # 7 rijen
    expect_equal(object=ncol(res$df_gemg), expected=5) # 5 kolommen
    expect_identical(object=round(res$df_gemg["Stieren","hvm_mnd4"], 5),
                     expected=round(461.86, 5)) # waarde in sheet

    # jaarovergang: twee tabbladen nodig als mnd1=november (controleer ook waarde)
    jrmnd <- list(mnd1="201711", mnd4="201802", vwmd="201802")
    res <- inl_lees_input_verd_gemg(inputbestand=bst, jrmnd=jrmnd)
    # valideer res$df_verd
    expect_equal(object=nrow(res$df_verd), expected=3) # 3 rijen
    expect_equal(object=ncol(res$df_verd), expected=5) # 5 kolommen
    expect_identical(object=round(res$df_verd["Stieren","hvm_mnd4"], 5),
                     expected=round(10.49519,5)) # waarde in sheet
    # valideer res$df_gemg
    expect_equal(object=nrow(res$df_gemg), expected=7) # 7 rijen
    expect_equal(object=ncol(res$df_gemg), expected=5) # 5 kolommen
    expect_identical(object=round(res$df_gemg["Stieren","hvm_mnd4"], 5),
                     expected=round(447.91292, 5)) # waarde in sheet

    # jaarovergang: twee tabbladen nodig als mnd1=oktober (controleer waarde niet meer)
    jrmnd <- list(mnd1="201710", mnd4="201801", vwmd="201801")
    res <- inl_lees_input_verd_gemg(inputbestand=bst, jrmnd=jrmnd)
    # valideer res$df_verd
    expect_equal(object=nrow(res$df_verd), expected=3) # 3 rijen
    expect_equal(object=ncol(res$df_verd), expected=5) # 5 kolommen
    # valideer res$df_gemg
    expect_equal(object=nrow(res$df_gemg), expected=7) # 7 rijen
    expect_equal(object=ncol(res$df_gemg), expected=5) # 5 kolommen

    # jaarovergang: twee tabbladen nodig als mnd1=december (controleer waarde niet meer)
    jrmnd <- list(mnd1="201712", mnd4="201803", vwmd="201803")
    res <- inl_lees_input_verd_gemg(inputbestand=bst, jrmnd=jrmnd)
    # valideer res$df_verd
    expect_equal(object=nrow(res$df_verd), expected=3) # 3 rijen
    expect_equal(object=ncol(res$df_verd), expected=5) # 5 kolommen
    # valideer res$df_gemg
    expect_equal(object=nrow(res$df_gemg), expected=7) # 7 rijen
    expect_equal(object=ncol(res$df_gemg), expected=5) # 5 kolommen

})

##############################################################################
context("inl_lees_input_gemg_kalv()")

# goedpad
test_that("het inputbestand gemiddelde gewichten goed wordt ingelezen", {
    # 1 bestand nodig, want mnd1 < 10
    bst <- file.path(tst_data_map, "inl_lees_input_gemg_kalv", "201712",
                     "Slachtgewichten Kalveren CBS 2017.xlsx")
    bst_vj <- "x"
    jrmnd <- list(mnd1="201709", mnd4="201712", vwmd="201712")
    res <- inl_lees_input_gemg_kalv(inputbestand=bst, inputbestand_vj=bst_vj,
                                    jrmnd=jrmnd)
    expect_equal(object=nrow(res), expected=1) # 3 rijen
    expect_equal(object=ncol(res), expected=5) # 5 kolommen
    expect_identical(object=round(res["Kalveren 0-8 mnd","hvm_mnd4"], 5),
                     expected=round(146.6, 5)) # waarde in sheet

    # 2 bestanden nodig, want mnd1 = november (controleer ook waarde)
    bst <- file.path(tst_data_map, "inl_lees_input_gemg_kalv", "201802",
                     "Slachtgewichten Kalveren tm februari CBS 2018.xlsx")
    bst_vj <- file.path(tst_data_map, "inl_lees_input_gemg_kalv", "201802",
                        "Slachtgewichten Kalveren CBS 2017.xlsx")
    jrmnd <- list(mnd1="201711", mnd4="201802", vwmd="201802")
    res <- inl_lees_input_gemg_kalv(inputbestand=bst, inputbestand_vj=bst_vj,
                                    jrmnd=jrmnd)
    expect_equal(object=nrow(res), expected=1) # 3 rijen
    expect_equal(object=ncol(res), expected=5) # 5 kolommen
    expect_identical(object=round(res["Kalveren 0-8 mnd","hvm_mnd4"], 4),
                     expected=round(149.0815, 4)) # waarde in sheet

    # 2 bestanden nodig, want mnd1 = oktober (controleer waarde niet meer)
    bst <- file.path(tst_data_map, "inl_lees_input_gemg_kalv", "201802",
                     "Slachtgewichten Kalveren tm februari CBS 2018.xlsx")
    bst_vj <- file.path(tst_data_map, "inl_lees_input_gemg_kalv", "201802",
                        "Slachtgewichten Kalveren CBS 2017.xlsx")
    jrmnd <- list(mnd1="201710", mnd4="201801", vwmd="201801")
    res <- inl_lees_input_gemg_kalv(inputbestand=bst, inputbestand_vj=bst_vj,
                                    jrmnd=jrmnd)
    expect_equal(object=nrow(res), expected=1) # 3 rijen
    expect_equal(object=ncol(res), expected=5) # 5 kolommen

    # 2 bestanden nodig, want mnd1 = december (controleer waarde niet meer)
    bst <- file.path(tst_data_map, "inl_lees_input_gemg_kalv", "201802",
                     "Slachtgewichten Kalveren tm februari CBS 2018.xlsx")
    bst_vj <- file.path(tst_data_map, "inl_lees_input_gemg_kalv", "201802",
                        "Slachtgewichten Kalveren CBS 2017.xlsx")
    jrmnd <- list(mnd1="201712", mnd4="201803", vwmd="201803")
    res <- inl_lees_input_gemg_kalv(inputbestand=bst, inputbestand_vj=bst_vj,
                                    jrmnd=jrmnd)
    expect_equal(object=nrow(res), expected=1) # 3 rijen
    expect_equal(object=ncol(res), expected=5) # 5 kolommen


})

##############################################################################
context("inl_lees_ctcr_aant_rdvl()")
context("inl_lees_ctcr_aant_wtvl()")

# goedpad
test_that("het ctcrbestand aantallen roodvlees goed wordt ingelezen", {
    bst <- file.path(tst_data_map, "inl_lees_ctcr_aant",
                     "analysebestand_aantallen_roodvlees.xlsx")
    maanden_huidige_vm <- c("201712", "201801", "201802", "201803")
    jrmnd <- list(vwmd="201803")
    res <- inl_lees_ctcr_aant_rdvl(bst_aant_gaaf=bst,
                                   maanden_huidige_vm=maanden_huidige_vm,
                                   jrmnd=jrmnd)
    expect_equal(object=nrow(res), expected=371) # 371 rijen
    expect_equal(object=ncol(res), expected=7) # 7 kolommen
    sel <- subset(res, vwmd == "201803" & vsmd == "201712" &
                       slnm == "BEDRIJFSNAAM 2" & dsrt == "Eenhoevige dieren")
    expect_identical(object=sel[,"aant"], expected='6') # waarde in sheet
})

test_that("het ctcrbestand aantallen witvlees goed wordt ingelezen", {

    bst <- file.path(tst_data_map, "inl_lees_ctcr_aant",
                     "analysebestand_aantallen_witvlees.xlsx")
    maanden_huidige_vm <- c("201712", "201801", "201802", "201803")
    jrmnd <- list(vwmd="201803")
    res <- inl_lees_ctcr_aant_wtvl(bst_aant_gaaf=bst,
                                   maanden_huidige_vm=maanden_huidige_vm,
                                   jrmnd=jrmnd)
    expect_equal(object=nrow(res), expected=24) # 24 rijen
    expect_equal(object=ncol(res), expected=6) # 6 kolommen
    sel <- subset(res, vwmd == "201803" & vsmd == "201712" &
                      slnm == "BEDRIJFSNAAM 2" & dsrt == "Vleeskuikens")
    expect_identical(object=sel[,"aant"], expected='2675148') # waarde in sheet
})

##############################################################################
context("inl_lees_ctcr_verd()")
# goedpad
test_that("het ctcrbestand aantallenverdelingen goed wordt ingelezen", {
    bst <- file.path(tst_data_map, "inl_lees_ctcr_verd",
                     "analysebestand_aantallenverdelingen.xlsx")
    maanden_huidige_vm <- c("201712", "201801", "201802", "201803")
    jrmnd <- list(vwmd="201803")
    res <- inl_lees_ctcr_verd(bst_gaaf=bst,
                              maanden_huidige_vm=maanden_huidige_vm,
                              jrmnd=jrmnd)
    expect_equal(object=nrow(res), expected=20) # 20 rijen
    expect_equal(object=ncol(res), expected=5) # 5 kolommen
    sel <- subset(res, vwmd == "201803" & vsmd == "201802" &
                       dsrt == "Volwassen runderen" & dcat == "Vaarzen")
    expect_identical(object=sel[,"verd"], expected='2.33') # waarde in sheet
})

##############################################################################
context("inl_lees_ctcr_gemg()")
# goedpad
test_that("het ctcrbestand gemiddelde gewichten goed wordt ingelezen", {
    bst <- file.path(tst_data_map, "inl_lees_ctcr_gemg",
                     "analysebestand_gemiddeldegewichten.xlsx")
    maanden_huidige_vm <- c("201712", "201801", "201802", "201803")
    jrmnd <- list(vwmd="201803")
    res <- inl_lees_ctcr_gemg(bst_gaaf=bst,
                              maanden_huidige_vm=maanden_huidige_vm,
                              jrmnd=jrmnd)
    expect_equal(object=nrow(res), expected=78) # 58 rijen
    expect_equal(object=ncol(res), expected=4) # 4 kolommen
    sel <- subset(res, vwmd == "201803" & vsmd == "201803" &
                       dcat == "Struisvogels")
    expect_identical(object=sel[,"gemg"], expected='130') # waarde in sheet
})

##############################################################################
context("inl_lees_vlp_tijdreeksen()") # niet testen want alleen ophalen uit database

##############################################################################
context("inl_lees_hist_tijdreeksen()") # niet testen want alleen ophalen uit database

##############################################################################
context("inl_lees_bijschattingen()") # niet testen want alleen ophalen uit database

##############################################################################
context("inl_lees_statbase_output()") # niet testen want alleen ophalen uit database

test_that("inl_lees_csv_bestand_met_kolommen_config-functie stelt juiste kolomnamen en -typen in", {
  
  # we maken een dataframe en slaan het op in een tijdelijk bestand
  df_naar_csv <- data.frame(
    KOLOM_A = 1:10,
    KOLOM_B = letters[1:10],
    # we maken een karakterkolom met een getal om te testen of het instellen van
    # het type goed werkt
    KOLOM_C = as.character(1:10)
  )
  
  pad <- withr::local_tempfile(
    fileext = ".csv",
    pattern = "input_bestand")
  
  readr::write_csv(x = df_naar_csv, file = pad)
  
  # Het argument kolommen_config van de functie
  # inl_lees_csv_bestand_met_kolommen_config neemt een lijst als input. Elk
  # element van de lijst stelt een kolom voor. De naam van het element staat
  # voor de kolomnaam in de output. Elk element is een lijst met twee elementen.
  # Het element `naam` bevat de kolomnaam in het bestand, het element `type`
  # bevat het type dat de kolom moet hebben. De typen moeten overeenkomen met de
  # types die zijn toegestaan door readr::cols
  kolommen_config <- list(
    kolom_a = list(
      naam = "KOLOM_A",
      type = readr::col_integer()
    ),
    kolom_b = list(
      naam = "KOLOM_B",
      type = readr::col_character()
    ),
    kolom_c = list(
      naam = "KOLOM_C",
      type = readr::col_character()
    )
  )
  
  df_output <- inl_lees_csv_bestand_met_kolommen_config(
    pad = pad, 
    kolommen_config = kolommen_config)
  
  expect_equal(colnames(df_output), c("kolom_a", "kolom_b", "kolom_c"))
  expect_type(df_output$kolom_a, "integer")
  expect_type(df_output$kolom_b, "character")
  expect_type(df_output$kolom_c, "character")
})



test_that("Error handling in de inl_lees_csv_bestand_met_kolommen_config functie", {
  
  kolommen_config <- list(
    kolom_a = list(
      naam = "KOLOM_A",
      type = readr::col_integer()
    ),
    kolom_b = list(
      naam = "KOLOM_B",
      type = readr::col_character()
    ),
    kolom_c = list(
      naam = "KOLOM_C",
      type = readr::col_character()
    )
  )
  
  # bestand bestaat niet
  expect_error(
    inl_lees_csv_bestand_met_kolommen_config(
      pad = "", 
      kolommen_config = kolommen_config),
    "Het bestand kan niet worden gelezen."
  )
  df_naar_csv <- data.frame(
    KOLOM_A = 1:10,
    KOLOM_B = letters[1:10],
    KOLOM_C = as.character(1:10)
  )
  
  pad <- withr::local_tempfile(
    fileext = ".csv",
    pattern = "input_bestand")
  
  readr::write_csv(x = df_naar_csv, file = pad)
  
  
  # de aantal kolommen in kolom_config is anders dan in bestand
  kolommen_config <- list(
    kolom_a = list(
      naam = "KOLOM_A",
      type = readr::col_integer()
    ),
    kolom_c = list(
      naam = "KOLOM_C",
      type = readr::col_character()
    )
  )
  
  expect_error(
    inl_lees_csv_bestand_met_kolommen_config(
      pad = pad, 
      kolommen_config = kolommen_config),
    "Het aantal kolommen in het bestand is anders dan het aantal kolommen in de config."
  )
  
  # de functie om de bestand in te lezen hoort niet bij de readr package
  expect_error(
    inl_lees_csv_bestand_met_kolommen_config(
      pad = pad, 
      kolommen_config = kolommen_config,
      inlezen_functie = dplyr::select),
    "De inlezen functie moet een functie van het readr package zijn."
  )
})


test_that("Warnings handling in de inl_lees_csv_bestand_met_kolommen_config functie", {
  
  # Warnings kunnen in twee gevallen door readr worden gegooid (daar zorgen wij
  # tenminste voor): 
  #  - Er zijn parsing errors. Dus het verwachte type van een
  #    kolom komt niet overeen met wat er werkelijk in het bestand staat. Denk aan
  #    het definiëren van een kolom als numeriek en het hebben van letters in die
  #    kolom in het bestand
  #  - Een (of meer) van de verwachte kolomnamen wordt (worden) niet gevonden 
  #    in het werkelijke bestand.
  
  # Parsing errors
  
  # Als er parsingerrors optreden tijdens het lezen van het bestand, maken we
  # een bestand met een samenvatting van deze fouten. De samenvatting is in
  # feite een compacte versie van de uitvoer van readr::problems
  
  df_naar_csv <- data.frame(
    KOLOM_A = 1:2,
    KOLOM_B = letters[1:2]
  )
  
  temp_map <- withr::local_tempdir()
  
  input_bestand_pad <- withr::local_tempfile(
    fileext = ".csv",
    pattern = "input_bestand",
    tmpdir  = temp_map)
  
  
  readr::write_csv(x = df_naar_csv, file = input_bestand_pad)
  
  kolommen_config <- list(
    kolom_a = list(
      naam = "KOLOM_A",
      type = readr::col_integer()
    ),
    kolom_b = list(
      naam = "KOLOM_B",
      # we hebben deze kolom ingesteld op numeriek, maar hij bevat letters, dus
      # zou hij moeten falen
      type = readr::col_integer()
    )
  )
  
  # we verwachten een error als readr geeft een warning
  expect_error(
    inl_lees_csv_bestand_met_kolommen_config(
      pad = input_bestand_pad, 
      kolommen_config = kolommen_config,
      map_fouten = temp_map)
    )
  
  # We verwachten ook een bestand met de sammevatting.
  # In dit geval, verwachten we maar een rij in dit bestand want 
  # er is maar een type van parsing error
  
  df_samevatting_parsing_errors <- readr::read_csv2(
    file = list.files(temp_map, pattern = "fouten_in_.*\\.csv", full.names = TRUE)
  )
  expect_equal(
    nrow(df_samevatting_parsing_errors), 1
  )
  # De verwacht type van de kolom waar de parsing error is gevonden
  expect_equal(
    df_samevatting_parsing_errors$expected, 
    "an integer"
    )
  # De kolom index waar de parsing error is gevonden
  expect_equal(
    df_samevatting_parsing_errors$col, 
    2
  )
  # De rijen waar de parsing errors zijn gevonden
  expect_equal(
    df_samevatting_parsing_errors$rijen, 
    "2, 3"
  )
  
  
  # een van de kolommen heeft een andere naam dan is gedefinieerd in de config
  df_naar_csv <- data.frame(
    KOLOM_A = 1:10,
    KOLOM_B = letters[1:10]
  )
  
  pad <- withr::local_tempfile(
    fileext = ".csv",
    pattern = "input_bestand")
  
  readr::write_csv(x = df_naar_csv, file = pad)
  
  kolommen_config <- list(
    kolom_a = list(
      naam = "KOLOM_A",
      type = readr::col_integer()
    ),
    kolom_b = list(
      naam = "NAAM_KLOPT_NIET",
      type = readr::col_character()
    )
  )
  
  expect_error(
    inl_lees_csv_bestand_met_kolommen_config(
      pad = pad, 
      kolommen_config = kolommen_config,
      inlezen_functie = readr::read_csv,
      "."),  
    "We hebben een waarschuwing gedetecteerd tijdens het lezen van het bestand"
  )
})


test_that("inl_lees_excel_bestand_met_kolommen_config-functie stelt juiste kolomnamen en -typen in", {
  
  # we maken een dataframe en slaan het op in een tijdelijk bestand
  df_naar_excel <- data.frame(
    KOLOM_A = 1:10,
    KOLOM_B = letters[1:10],
    # we maken een karakterkolom met een getal om te testen of het instellen van
    # het type goed werkt
    KOLOM_C = as.character(1:10)
  )
  
  pad <- withr::local_tempfile(
    fileext = ".excel",
    pattern = "input_bestand")
  
  writexl::write_xlsx(x = df_naar_excel, path = pad)
  
  # Het argument kolommen_config van de functie
  # inl_lees_csv_bestand_met_kolommen_config neemt een lijst als input. Elk
  # element van de lijst stelt een kolom voor. De naam van het element staat
  # voor de kolomnaam in de output. Elk element is een lijst met twee elementen.
  # Het element `naam` bevat de kolomnaam in het bestand, het element `type`
  # bevat het type dat de kolom moet hebben. De typen moeten overeenkomen met de
  # types die zijn toegestaan door readxl::read_excel
  kolommen_config <- list(
    kolom_a = list(
      naam = "KOLOM_A",
      type = "numeric"
    ),
    kolom_b = list(
      naam = "KOLOM_B",
      type = "text"
    ),
    kolom_c = list(
      naam = "KOLOM_C",
      type = "text"
    )
  )
  
  df_output <- inl_lees_excel_bestand_met_kolommen_config(
    pad = pad, 
    kolommen_config = kolommen_config)
  
  expect_equal(colnames(df_output), c("kolom_a", "kolom_b", "kolom_c"))
  expect_type(df_output$kolom_a, "double")
  expect_type(df_output$kolom_b, "character")
  expect_type(df_output$kolom_c, "character")
})



test_that("Error handling in de inl_lees_excel_bestand_met_kolommen_config functie", {
  
  kolommen_config <- list(
    kolom_a = list(
      naam = "KOLOM_A",
      type = "numeric"
    )
  )
  
  # bestand bestaat niet
  expect_error(
    inl_lees_excel_bestand_met_kolommen_config(
      pad = "", 
      kolommen_config = kolommen_config),
    "Het bestand kan niet worden gelezen."
  )
  
  df_naar_excel <- data.frame(
    KOLOM_A = 1:10,
    KOLOM_B = letters[1:10],
    KOLOM_C = as.character(1:10)
  )
  
  pad <- withr::local_tempfile(
    fileext = ".xlsx",
    pattern = "input_excel_bestand")
  
  writexl::write_xlsx(x = df_naar_excel, path = pad)
  
  
  # de aantal kolommen in kolom_config is anders dan in bestand
  kolommen_config <- list(
    kolom_a = list(
      naam = "KOLOM_A",
      type = "numeric"
    ),
    kolom_c = list(
      naam = "KOLOM_C",
      type = "text"
    )
  )
  
  expect_error(
    inl_lees_excel_bestand_met_kolommen_config(
      pad = pad, 
      kolommen_config = kolommen_config),
    " Het bestand kan niet worden gelezen."
  )
  
  # Sommige kolommen hebben een andere naam dan is gedefinieerd in de config
  df_naar_excel <- data.frame(
    KOLOM_A = 1:10,
    KOLOM_B = letters[1:10],
    KOLOM_C = 1:10
  )
  
  pad <- withr::local_tempfile(
    fileext = ".xlsx",
    pattern = "input_bestand")
  
  writexl::write_xlsx(x = df_naar_excel, path = pad)
  
  kolommen_config <- list(
    kolom_a = list(
      naam = "NAAM_KLOPT_NIET",
      type = "numeric"
    ),
    kolom_b = list(
      naam = "KOLOM_B",
      type = "text"
    ),
    kolom_c = list(
      naam = "NAAM_KLOPT_OOK_NIET",
      type = "text"
    )
  )
  
  expect_error(
    inl_lees_excel_bestand_met_kolommen_config(
      pad = pad, 
      kolommen_config = kolommen_config)
  )
})


test_that("Warning handling in de inl_lees_excel_bestand_met_kolommen_config functie", {
  
  # Warnings kunnen in twee gevallen door readr worden gegooid (daar zorgen wij
  # tenminste voor): 
  #  - Er zijn parsing errors. Dus het verwachte type van een
  #    kolom komt niet overeen met wat er werkelijk in het bestand staat. Denk aan
  #    het definiëren van een kolom als numeriek en het hebben van letters in die
  #    kolom in het bestand
  #  - Een (of meer) van de verwachte kolomnamen wordt (worden) niet gevonden 
  #    in het werkelijke bestand.
  
  # Parsing errors
  
  # Als er parsingerrors optreden tijdens het lezen van het bestand, maken we
  # een bestand met een samenvatting van deze fouten. De samenvatting is in
  # feite een compacte versie van de uitvoer van readr::problems
  df_naar_excel <- data.frame(
    KOLOM_A = 1:2,
    KOLOM_B = letters[1:2]
  )
  temp_map <- withr::local_tempdir()
  
  input_bestand_pad <- withr::local_tempfile(
    fileext = ".xlsx",
    pattern = "input_bestand",
    tmpdir  = temp_map)
  
  
  writexl::write_xlsx(x = df_naar_excel, path = input_bestand_pad)
  
  kolommen_config <- list(
    kolom_a = list(
      naam = "KOLOM_A",
      type = "numeric"
    ),
    kolom_b = list(
      naam = "KOLOM_B",
      # we hebben deze kolom ingesteld op numeriek, maar hij bevat letters, dus
      # zou hij moeten falen
      type = "numeric"
    )
  )
  
  # We verwachten een error als read_excel geeft een warning (en stop_met_warnings TRUE is)    
  
  expect_error(
    inl_lees_excel_bestand_met_kolommen_config(
      pad = input_bestand_pad, 
      kolommen_config = kolommen_config,
      map_fouten = temp_map,
      stop_met_warnings = TRUE)
  )
  
  # We verwachten ook een bestand met de sammevatting.
  # In dit geval, verwachten we maar een rij in dit bestand want 
  # er is maar een type van parsing error
  
  df_samevatting_parsing_errors <- readr::read_csv2(
    file = list.files(temp_map, pattern = "fouten_in_.*\\.csv", full.names = TRUE)
  )
  expect_equal(
    nrow(df_samevatting_parsing_errors), 1
  )
  # De verwacht type van de kolom waar de parsing error is gevonden
  expect_equal(
    df_samevatting_parsing_errors$expected, 
    "numeric"
  )
  # De kolom index waar de parsing error is gevonden
  expect_equal(
    df_samevatting_parsing_errors$col, 
    "B"
  )
  # De rijen waar de parsing errors zijn gevonden
  expect_equal(
    df_samevatting_parsing_errors$rijen, 
    "2, 3"
  )
  
  # Als stop_met_warnings FALSE is, dan gaat de functie door. Er is dan geen error
  # maar een warning
  expect_warning(
    inl_lees_excel_bestand_met_kolommen_config(
      pad = input_bestand_pad, 
      kolommen_config = kolommen_config,
      map_fouten = temp_map,
      stop_met_warnings = FALSE)
  )
})

test_that("het type wijzigen met de verander_type-functie", {
  
  # character naar numeric
  x <- 1:3
  
  input <- as.character(x)
  
  output <- verander_type(x = input, type = "numeric")

  expect_equal(output, x)
  
  # character naar logical
  x <- c(TRUE, FALSE)
  
  input <- as.character(x)
  
  output <- verander_type(input, type = "logical")
  
  expect_equal(output, x)
  
  # character naar date
  x <- Sys.Date()
  
  input <- as.character(x)
  
  output <- verander_type(input, type = "date")
  
  expect_equal(output, x)
})

test_that("Warnings in verander_type functie", {
  
  x <- c("1", "a", "2")
  
  expect_warning(
    output <- verander_type(x, type = "numeric"),
    regexp = ".*posities: 1, 3."
  )
  
  expect_equal(
    output,
    c(1, NA, 2)
  )  
  
  expect_warning(
    verander_type(x, type = "numeric", kol_naam = "X"),
    regexp = ".*bij het parsen van kolom X.*"
  )
})

test_that("Errors in check_input_type functie", {
  
  expect_error(
    check_input_type(input_type = "X"),
    regexp = ".*Input type \\(X\\) niet geldig."
  )
  # met niet default mogelijk types  
  expect_error(
    check_input_type(input_type = "X", geldige_types = LETTERS[1:3]),
    regexp = ".*Input type \\(X\\) niet geldig."
  )
})


test_that("inl_lees_sav_bestand_met_kolommen_config kan SAV bestanden inlezen", {
  
  df_naar_csv <- data.frame(
    KOLOM_A = 1:2,
    KOLOM_B = rep("a", 2),
    KOLOM_C = as.character(rep(Sys.Date(), 2)),
    KOLOM_D = as.character(c(TRUE, FALSE))
    
  )
  
  pad <- withr::local_tempfile(
    fileext = ".sav",
    pattern = "input_bestand")
  
  haven::write_sav(data = df_naar_csv, path = pad)
  
  # Het argument kolommen_config van de functie
  # inl_lees_sav_bestand_met_kolommen_config neemt een lijst als input. Elk
  # element van de lijst stelt een kolom voor. De naam van het element staat
  # voor de kolomnaam in de output. Elk element is een lijst met twee elementen.
  # Het element `naam` bevat de kolomnaam in het bestand, het element `type`
  # bevat het type dat de kolom moet hebben. De typen kunnen numeric, logical,
  # character, of date zijn.
  kolommen_config <- list(
    kolom_a = list(
      naam = "KOLOM_A",
      type = "numeric"
    ),
    kolom_b = list(
      naam = "KOLOM_B",
      type = "character"
    ),
    kolom_c = list(
      naam = "KOLOM_C",
      type = "date"
    ),
    kolom_d = list(
      naam = "KOLOM_D",
      type = "logical"
    )
  )
  
  df_output <- inl_lees_sav_bestand_met_kolommen_config(
    pad = pad, 
    kolommen_config = kolommen_config,
    map_fouten = withr::local_tempdir(), 
    stop_met_warnings = FALSE)

  expect_s3_class(df_output, "data.frame")
  
  expect_equal(nrow(df_output), 2)
  expect_type(df_output$kolom_a, "double")
  expect_type(df_output$kolom_b, "character")
  expect_true(is.Date(df_output$kolom_c))
  expect_type(df_output$kolom_d, "logical")
  })


test_that("Warning handling in de inl_lees_sav_bestand_met_kolommen_config functie", {
  
  
  df_naar_csv <- data.frame(
    # we maken een karakterkolom met een getal om te testen of het instellen van
    # het type goed werkt
    KOLOM_C = c(as.character(1), "a")
  )
  
  pad <- withr::local_tempfile(
    fileext = ".sav",
    pattern = "input_bestand")
  
  haven::write_sav(data = df_naar_csv, path = pad)
  
  kolommen_config <- list(
    kolom_c = list(
      naam = "KOLOM_C",
      type = "numeric"
    )
  )
  
  outputmap <- withr::local_tempdir()
  
  expect_warning(
    df_output <- inl_lees_sav_bestand_met_kolommen_config(
      pad = pad, 
      kolommen_config = kolommen_config,
      map_fouten = outputmap, 
      stop_met_warnings = FALSE)
  )
  
  # als er problemen zijn tijdens het wijzigen van typen, moeten we een bestand
  # krijgen met de problemen die zijn gevonden
  bestandnaam_data <- basename(pad) |> stringr::str_remove(".sav")
  verwacht_bestandnaam <- glue::glue("fouten_in_{bestandnaam_data}.csv")
  
  expect_true(
    file.exists(file.path(outputmap, verwacht_bestandnaam))
  )
  # De waarden die  kunnen worden aangepast (type), moeten aanwezig zijn in de
  # uitvoer, de waarden die dat niet kunnen, moeten NA's zijn.
  expect_equal(
    df_output$kolom_c,
    c(1, NA)
  )
})


test_that("Regex bij kolomnamen en SAV config", {
  
  df_naar_csv <- data.frame(
    KOLOM45_A = 1:2,
    KOLOM_B = letters[1:2]
  )
  
  pad <- withr::local_tempfile(
    fileext = ".sav",
    pattern = "input_bestand")
  
  haven::write_sav(data = df_naar_csv, path = pad)
  
  kolommen_config <- list(
    kolom_a = list(
      naam = "KOLOM[0-9]{2}_A",
      type = "numeric"
    ),
    kolom_b = list(
      naam = "KOLOM_B",
      type = "character"
    )
  )
  
  df_output <- inl_lees_sav_bestand_met_kolommen_config(
    pad = pad, 
    kolommen_config = kolommen_config,
    map_fouten = withr::local_tempdir(), 
    stop_met_warnings = FALSE)
  
  expect_s3_class(df_output, "data.frame")
  expect_equal(nrow(df_output), 2)
  expect_type(df_output$kolom_a, "double")
  
})

