#' Naam        : test_afleiden_indicatoren.R
#' Auteur(s)   : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module bevat unittesten voor de functies uit afleiden_indicatoren.R
library(testthat)

# source het te testen script
root_map <- here::here()
src_map <- file.path(root_map, 'src')
source(file.path(src_map, "afleiden_indicatoren.R"))
##############################################################################
context("afl_voeg_toe_indicator()")

test_that("de juiste indicator wordt afgeleid", {

    # eerste indicator
    data1 <- ""
    data2 <- "X"
    data3 <- ";"
    res  <- afl_voeg_toe_indicator(df_cel=data1, ind_txt=data2, scheider=data3)
    verw <- "X"
    expect_identical(object=res, expected=verw)

    # tweede indicator
    data1 <- "X"
    data2 <- "Y"
    data3 <- ";"
    res  <- afl_voeg_toe_indicator(df_cel=data1, ind_txt=data2, scheider=data3)
    verw <- "X;Y"
    expect_identical(object=res, expected=verw)

    # derde indicator
    data1 <- "X;Y"
    data2 <- "Z"
    data3 <- ";"
    res  <- afl_voeg_toe_indicator(df_cel=data1, ind_txt=data2, scheider=data3)
    verw <- "X;Y;Z"
    expect_identical(object=res, expected=verw)

    # geen indicator
    data1 <- ""
    data2 <- ""
    data3 <- ";"
    res  <- afl_voeg_toe_indicator(df_cel=data1, ind_txt=data2, scheider=data3)
    verw <- ""
    expect_identical(object=res, expected=verw)

    # eerste indicator integer
    data1 <- ""
    data2 <- 3
    data3 <- ";"
    res  <- afl_voeg_toe_indicator(df_cel=data1, ind_txt=data2, scheider=data3)
    verw <- "3"
    expect_identical(object=res, expected=verw)

    # nieuwe indicator integer
    data1 <- "X"
    data2 <- 3
    data3 <- ";"
    res  <- afl_voeg_toe_indicator(df_cel=data1, ind_txt=data2, scheider=data3)
    verw <- "X;3"
    expect_identical(object=res, expected=verw)

    # nieuwe indicator leeg
    data1 <- "X"
    data2 <- ""
    data3 <- ";"
    res  <- afl_voeg_toe_indicator(df_cel=data1, ind_txt=data2, scheider=data3)
    verw <- "X"
    expect_identical(object=res, expected=verw)

    # nieuwe indicator NA
    data1 <- "X"
    data2 <- NA
    data3 <- ";"
    res  <- afl_voeg_toe_indicator(df_cel=data1, ind_txt=data2, scheider=data3)
    verw <- "X"
    expect_identical(object=res, expected=verw)
})


##############################################################################
context("afl_leid_af_indicatoren_rdvl_wtvl() geen indicator")
test_that("geen indicator voor aantallen roodvlees en witvlees", {

    # geen indicator
    data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                           vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                           '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                           ind1=c(""), ind2=c(""), ind3=c(""),
                           check.names=FALSE, stringsAsFactors=FALSE)
    data_def <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
    data_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
    data_vorige_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_ruw=c(100), aant_gaaf=c(100), stringsAsFactors=FALSE)
    jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)

    res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
                                              data_vorige_lev, jrmnd)

    verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                       vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                       '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                       ind1=c(""), ind2=c(""), ind3=c(100),
                       check.names=FALSE, stringsAsFactors=FALSE)
    
    expect_identical(object=res, expected=verw)

    # geen indicator length(gem_def) == 0
    data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                           vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                           '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                           ind1=c(""), ind2=c(""), ind3=c(""),
                           check.names=FALSE, stringsAsFactors=FALSE)
    data_def <- data.frame(vsmd=c(201712), wrkp=c("B"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
    data_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
    data_vorige_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_ruw=c(100), aant_gaaf=c(100), stringsAsFactors=FALSE)
    jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)

    res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
                                              data_vorige_lev, jrmnd)

    verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                       vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                       '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                       ind1=c(""), ind2=c(""), ind3=c(NA),
                       check.names=FALSE, stringsAsFactors=FALSE)
    verw$ind3 <- sapply(verw$ind3, as.numeric) # maak van de waarden numerics

    expect_identical(object=res, expected=verw)
    
    
    ######### --> doen alleen of de gemiddelde van ind3 nog klopt met de verschuiving van kolommen 
    # GOOD CASE SCENARIO
    data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"), is_biologisch=c(NA),
                           vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                           '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                           bron=c("NVWA"), ind1=c(""), ind2=c(""), ind3=c(""),
                           check.names=FALSE, stringsAsFactors=FALSE)
    data_def <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
    data_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), is_biologisch=c(NA), bron=c("NVWA"), stringsAsFactors=FALSE)
    data_vorige_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_ruw=c(100), aant_gaaf=c(100), stringsAsFactors=FALSE)
    jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)
    
    res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
                                              data_vorige_lev, jrmnd)
    
    verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"), is_biologisch=c(NA),
                      vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                      '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                      bron=c("NVWA"), ind1=c(""), ind2=c(""), ind3=c(100),
                      check.names=FALSE, stringsAsFactors=FALSE)
    
    expect_identical(res, verw)

})

##############################################################################
context("afl_leid_af_indicatoren_rdvl_wtvl() ind1=N")
test_that("ind1=N voor aantallen roodvlees en witvlees juist wordt afgeleid", {

  # wel indicator: data_vorige_lev bevat geen wrkp="A"
  data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                         vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                         '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                         ind1=c(""), ind2=c(""), ind3=c(""),
                         check.names=FALSE, stringsAsFactors=FALSE)
  data_def <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
  data_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
  data_vorige_lev <- data.frame(vsmd=c(""), wrkp=c(""), slnm=c(""), dsrt=c(""), aant_ruw=c(100), aant_gaaf=c(""), stringsAsFactors=FALSE)
  jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)

  res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
                                            data_vorige_lev, jrmnd)

  verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                     vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                     '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                     ind1=c("N"), ind2=c(""), ind3=c(100),
                     check.names=FALSE, stringsAsFactors=FALSE)
  verw$ind3 <- sapply(verw$ind3, as.numeric) # maak van de waarden numerics
  expect_identical(object=res, expected=verw)

})

##############################################################################
context("afl_leid_af_indicatoren_rdvl_wtvl() ind1=S")
test_that("ind1=S voor aantallen roodvlees en witvlees juist wordt afgeleid", {

  # wel indicator: nieuwste vsmd in data_lev ligt voor jrmnd$mnd1
  data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                         vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                         '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                         ind1=c(""), ind2=c(""), ind3=c(""),
                         check.names=FALSE, stringsAsFactors=FALSE)
  data_def <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
  data_lev <- data.frame(vsmd=c(""), wrkp=c(""), slnm=c(""), dsrt=c(""), aant_gaaf=c(""), stringsAsFactors=FALSE)
  data_vorige_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_ruw=c(100), aant_gaaf=c(100), stringsAsFactors=FALSE)
  jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)

  res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
                                            data_vorige_lev, jrmnd)

  verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                     vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                     '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(100),
                     ind1=c("S"), ind2=c(""), ind3=c(100),
                     check.names=FALSE, stringsAsFactors=FALSE)
  verw$ind3 <- sapply(verw$ind3, as.numeric) # maak van de waarden numerics
  expect_identical(object=res, expected=verw)

})

##############################################################################
context("afl_leid_af_indicatoren_rdvl_wtvl() ind2=O")
test_that("ind1=O voor aantallen roodvlees en witvlees juist wordt afgeleid", {

  # wel indicator: gemiddeld aantal dieren < 1.000: waarde voor nieuwste verslagmaand te hoog
  data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                         vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                         '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(1000),
                         ind1=c(""), ind2=c(""), ind3=c(""),
                         check.names=FALSE, stringsAsFactors=FALSE)
  data_def <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
  data_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
  data_vorige_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_ruw=c(100), aant_gaaf=c(100), stringsAsFactors=FALSE)
  jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)

  res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
                                            data_vorige_lev, jrmnd)

  verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                     vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                     '201712'=c(100), '201801'=c(100), '201802'=c(100), '201803'=c(1000),
                     ind1=c(""), ind2=c("O"), ind3=c(100),
                     check.names=FALSE, stringsAsFactors=FALSE)
  verw$ind3 <- sapply(verw$ind3, as.numeric) # maak van de waarden numerics
  expect_identical(object=res, expected=verw)

  # # wel indicator: gemiddeld aantal dieren < 1.000: waarde voor nieuwste verslagmaand te laag
  # #                (stiekem komt ook indi2=K mee)
  # data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
  #                        vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
  #                        '201712'=c(0), '201801'=c(0), '201802'=c(0), '201803'=c(0),
  #                        ind1=c(""), ind2=c(""), ind3=c(""),
  #                        check.names=FALSE, stringsAsFactors=FALSE)
  # data_def <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
  # data_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
  # data_vorige_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_ruw=c(100), aant_gaaf=c(100), stringsAsFactors=FALSE)
  # jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)
  # 
  # res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
  #                                           data_vorige_lev, jrmnd)
  # 
  # verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
  #                    vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
  #                    '201712'=c(0), '201801'=c(0), '201802'=c(0), '201803'=c(0),
  #                    ind1=c(""), ind2=c("O;K"), ind3=c(100),
  #                    check.names=FALSE, stringsAsFactors=FALSE)
  # verw$ind3 <- sapply(verw$ind3, as.numeric) # maak van de waarden numerics
  # expect_identical(object=res, expected=verw)

  # wel indicator: 1000 =< gemiddeld aantal dieren < 10.000: waarde voor nieuwste verslagmaand te hoog
  data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                         vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
                         '201712'=c(1000), '201801'=c(1000), '201802'=c(1000), '201803'=c(1000000),
                         ind1=c(""), ind2=c(""), ind3=c(""),
                         check.names=FALSE, stringsAsFactors=FALSE)
  data_def <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(1000), stringsAsFactors=FALSE)
  data_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(1000), stringsAsFactors=FALSE)
  data_vorige_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_ruw=c(1000), aant_gaaf=c(1000), stringsAsFactors=FALSE)
  jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)

  res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
                                            data_vorige_lev, jrmnd)

  verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                     vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
                     '201712'=c(1000), '201801'=c(1000), '201802'=c(1000), '201803'=c(1000000),
                     ind1=c(""), ind2=c("O"), ind3=c(1000),
                     check.names=FALSE, stringsAsFactors=FALSE)
  verw$ind3 <- sapply(verw$ind3, as.numeric) # maak van de waarden numerics
  expect_identical(object=res, expected=verw)

  # # wel indicator: 1.000 =< gemiddeld aantal dieren < 10.000: waarde voor nieuwste verslagmaand te laag
  # #                (stiekem komt ook indi2=K mee)
  # data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
  #                        vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
  #                        '201712'=c(0), '201801'=c(0), '201802'=c(0), '201803'=c(0),
  #                        ind1=c(""), ind2=c(""), ind3=c(""),
  #                        check.names=FALSE, stringsAsFactors=FALSE)
  # data_def <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
  # data_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
  # data_vorige_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_ruw=c(100), aant_gaaf=c(100), stringsAsFactors=FALSE)
  # jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)
  # 
  # res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
  #                                           data_vorige_lev, jrmnd)
  # 
  # verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
  #                    vvm_mnd1=c(1000), vvm_mnd2=c(1000), vvm_mnd3=c(1000),
  #                    '201712'=c(0), '201801'=c(0), '201802'=c(0), '201803'=c(0),
  #                    ind1=c(""), ind2=c("O;K"), ind3=c(100),
  #                    check.names=FALSE, stringsAsFactors=FALSE)
  # verw$ind3 <- sapply(verw$ind3, as.numeric) # maak van de waarden numerics
  # expect_identical(object=res, expected=verw)

  # wel indicator: 10.000 =< gemiddeld aantal dieren < 100.000: waarde voor nieuwste verslagmaand te hoog
  data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                         vvm_mnd1=c(10000), vvm_mnd2=c(10000), vvm_mnd3=c(10000),
                         '201712'=c(10000), '201801'=c(10000), '201802'=c(10000), '201803'=c(10000000),
                         ind1=c(""), ind2=c(""), ind3=c(""),
                         check.names=FALSE, stringsAsFactors=FALSE)
  data_def <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(10000), stringsAsFactors=FALSE)
  data_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(10000), stringsAsFactors=FALSE)
  data_vorige_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_ruw=c(10000), aant_gaaf=c(10000), stringsAsFactors=FALSE)
  jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)

  res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
                                            data_vorige_lev, jrmnd)

  verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                     vvm_mnd1=c(10000), vvm_mnd2=c(10000), vvm_mnd3=c(10000),
                     '201712'=c(10000), '201801'=c(10000), '201802'=c(10000), '201803'=c(10000000),
                     ind1=c(""), ind2=c("O"), ind3=c(10000),
                     check.names=FALSE, stringsAsFactors=FALSE)
  verw$ind3 <- sapply(verw$ind3, as.numeric) # maak van de waarden numerics
  expect_identical(object=res, expected=verw)

  # # wel indicator: 1.000 =< gemiddeld aantal dieren < 10.000: waarde voor nieuwste verslagmaand te laag
  # #                (stiekem komt ook indi2=K mee)
  # data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
  #                        vvm_mnd1=c(10000), vvm_mnd2=c(10000), vvm_mnd3=c(10000),
  #                        '201712'=c(0), '201801'=c(0), '201802'=c(0), '201803'=c(0),
  #                        ind1=c(""), ind2=c(""), ind3=c(""),
  #                        check.names=FALSE, stringsAsFactors=FALSE)
  # data_def <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(1000), stringsAsFactors=FALSE)
  # data_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(1000), stringsAsFactors=FALSE)
  # data_vorige_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_ruw=c(1000), aant_gaaf=c(1000), stringsAsFactors=FALSE)
  # jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)
  # 
  # res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
  #                                           data_vorige_lev, jrmnd)
  # 
  # verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
  #                    vvm_mnd1=c(10000), vvm_mnd2=c(10000), vvm_mnd3=c(10000),
  #                    '201712'=c(0), '201801'=c(0), '201802'=c(0), '201803'=c(0),
  #                    ind1=c(""), ind2=c("O;K"), ind3=c(1000),
  #                    check.names=FALSE, stringsAsFactors=FALSE)
  # verw$ind3 <- sapply(verw$ind3, as.numeric) # maak van de waarden numerics
  # expect_identical(object=res, expected=verw)

})

##############################################################################
context("afl_leid_af_indicatoren_rdvl_wtvl() ind2=K")
# is ook al getest bij afl_leid_af_indicatoren_rdvl_wtvl() ind2=O
test_that("ind2=K voor aantallen roodvlees en witvlees juist wordt afgeleid", {

  # wel indicator: waarde in vorige verslagmaand niet NA was, maar in huidige
  # verslagmaand wel
  data     <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                         vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                         '201712'=c(100), '201801'=c(100), '201802'=c(NA_integer_), '201803'=c(100),
                         ind1=c(""), ind2=c(""), ind3=c(""),
                         check.names=FALSE, stringsAsFactors=FALSE)
  data_def <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
  data_lev <- data.frame(vsmd=c(201612), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_gaaf=c(100), stringsAsFactors=FALSE)
  data_vorige_lev <- data.frame(vsmd=c(201712), wrkp=c("A"), slnm=c("A"), dsrt=c("A"), aant_ruw=c(100), aant_gaaf=c(100), stringsAsFactors=FALSE)
  jrmnd    <- list(mnd1=201712, mnd2=201801, mnd3=201802, mnd4=201803)

  res  <- afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev,
                                            data_vorige_lev, jrmnd)

  verw <- data.frame(slnm=c("A"), wrkp=c("A"),  dsrt=c("A"),
                     vvm_mnd1=c(100), vvm_mnd2=c(100), vvm_mnd3=c(100),
                     '201712'=c(100), '201801'=c(100), '201802'=c(NA_integer_), '201803'=c(100),
                     ind1=c(""), ind2=c("K"), ind3=c(100),
                     check.names=FALSE, stringsAsFactors=FALSE)
  verw$ind3 <- sapply(verw$ind3, as.numeric) # maak van de waarden numerics  
  expect_identical(object=res, expected=verw)

})

##############################################################################
context("afl_leid_af_indicatoren_verd() geen indicator")
test_that("geen indicator voor aantallenverdelingen", {

    # geen indicator
    verd_goed <- c(10, 88, 2, 11, 89) # correcte aantallenverdeling: 10 + 88 + 2 = 100 & 11 + 89 = 100
    data     <- data.frame(dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           vvm_mnd1=verd_goed, vvm_mnd2=verd_goed, vvm_mnd3=verd_goed,
                           hvm_mnd1=verd_goed, hvm_mnd2=verd_goed, hvm_mnd3=verd_goed, hvm_mnd4=verd_goed,
                           ind1=c("", "", "", "", ""), ind2=c("", "", "", "", ""), ind3=c("", "", "", "", ""),
                           row.names=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           check.names=FALSE, stringsAsFactors=FALSE)

    data_def <- data.frame(vsmd=c(201711,201711,201711,201711,201711),
                           dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           verd=verd_goed, stringsAsFactors=FALSE)

    res  <- afl_leid_af_indicatoren_verd(data, data_def)

    verw <- data.frame(dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           vvm_mnd1=verd_goed, vvm_mnd2=verd_goed, vvm_mnd3=verd_goed,
                           hvm_mnd1=verd_goed, hvm_mnd2=verd_goed, hvm_mnd3=verd_goed, hvm_mnd4=verd_goed,
                           ind1=c("", "", "", "", ""), ind2=c("", "", "", "", ""), ind3=c("", "", "", "", ""),
                           row.names=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           check.names=FALSE, stringsAsFactors=FALSE)
    expect_identical(object=res, expected=verw)

})

##############################################################################
context("afl_leid_af_indicatoren_verd() ind1=X")
test_that("ind1=X voor aantallenverdelingen juist wordt afgeleid", {

    # wel indicator: ind1=X voor stieren, koeien en vaarzen in elke vsmd
    verd_goed <- c(100, 88, 2, 11, 89) # 100 + 88 + 2 =/= 100
    data     <- data.frame(dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           vvm_mnd1=verd_goed, vvm_mnd2=verd_goed, vvm_mnd3=verd_goed,
                           hvm_mnd1=verd_goed, hvm_mnd2=verd_goed, hvm_mnd3=verd_goed, hvm_mnd4=verd_goed,
                           ind1=c("", "", "", "", ""), ind2=c("", "", "", "", ""), ind3=c("", "", "", "", ""),
                           row.names=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           check.names=FALSE, stringsAsFactors=FALSE)

    data_def <- data.frame(vsmd=c(201711,201711,201711,201711,201711),
                           dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           verd=verd_goed, stringsAsFactors=FALSE)

    res  <- afl_leid_af_indicatoren_verd(data, data_def)

    verw <- data.frame(dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           vvm_mnd1=verd_goed, vvm_mnd2=verd_goed, vvm_mnd3=verd_goed,
                           hvm_mnd1=verd_goed, hvm_mnd2=verd_goed, hvm_mnd3=verd_goed, hvm_mnd4=verd_goed,
                           ind1=c("X;X;X;X", "X;X;X;X", "X;X;X;X", "", ""), ind2=c("", "", "", "", ""), ind3=c("", "", "", "", ""),
                           row.names=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           check.names=FALSE, stringsAsFactors=FALSE)
    expect_identical(object=res, expected=verw)

    # wel indicator: ind1=X voor kalveren in elke vsmd
    verd_goed <- c(10, 88, 2, 110, 89) # 110 + 89 =/= 100
    data     <- data.frame(dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           vvm_mnd1=verd_goed, vvm_mnd2=verd_goed, vvm_mnd3=verd_goed,
                           hvm_mnd1=verd_goed, hvm_mnd2=verd_goed, hvm_mnd3=verd_goed, hvm_mnd4=verd_goed,
                           ind1=c("", "", "", "", ""), ind2=c("", "", "", "", ""), ind3=c("", "", "", "", ""),
                           row.names=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           check.names=FALSE, stringsAsFactors=FALSE)

    data_def <- data.frame(vsmd=c(201711,201711,201711,201711,201711),
                           dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           verd=verd_goed, stringsAsFactors=FALSE)

    res  <- afl_leid_af_indicatoren_verd(data, data_def)

    verw <- data.frame(dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           vvm_mnd1=verd_goed, vvm_mnd2=verd_goed, vvm_mnd3=verd_goed,
                           hvm_mnd1=verd_goed, hvm_mnd2=verd_goed, hvm_mnd3=verd_goed, hvm_mnd4=verd_goed,
                           ind1=c("", "", "", "X;X;X;X", "X;X;X;X"), ind2=c("", "", "", "", ""), ind3=c("", "", "", "", ""),
                           row.names=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           check.names=FALSE, stringsAsFactors=FALSE)
    expect_identical(object=res, expected=verw)

})

##############################################################################
context("afl_leid_af_indicatoren_verd() ind2=X")
test_that("ind2=X voor aantallenverdelingen juist wordt afgeleid", {

    # wel indicator: afwijking te hoog voor stieren, koeien en vaarzen (stiekem komt ook indi3=X mee)
    verd_goed <- c(10, 88, 2, 11, 89) # correcte aantallenverdeling: 10 + 88 + 2 = 100 & 11 + 89 = 100
    verd_hvm <- c(30, 50, 20, 11, 89) # correcte aantallenverdeling: 10 + 88 + 2 = 100 & 11 + 89 = 100
    data     <- data.frame(dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           vvm_mnd1=verd_goed, vvm_mnd2=verd_goed, vvm_mnd3=verd_goed,
                           hvm_mnd1=verd_hvm, hvm_mnd2=verd_hvm, hvm_mnd3=verd_hvm, hvm_mnd4=verd_hvm,
                           ind1=c("", "", "", "", ""), ind2=c("", "", "", "", ""), ind3=c("", "", "", "", ""),
                           row.names=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           check.names=FALSE, stringsAsFactors=FALSE)

    data_def <- data.frame(vsmd=c(201711,201711,201711,201711,201711),
                           dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           verd=verd_goed, stringsAsFactors=FALSE)

    res  <- afl_leid_af_indicatoren_verd(data, data_def)

    verw <- data.frame(dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           vvm_mnd1=verd_goed, vvm_mnd2=verd_goed, vvm_mnd3=verd_goed,
                           hvm_mnd1=verd_hvm, hvm_mnd2=verd_hvm, hvm_mnd3=verd_hvm, hvm_mnd4=verd_hvm,
                           ind1=c("", "", "", "", ""), ind2=c("X", "X", "X", "", ""), ind3=c("X", "X", "X", "", ""),
                           row.names=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           check.names=FALSE, stringsAsFactors=FALSE)
    expect_identical(object=res, expected=verw)

    # wel indicator: afwijking te hoog voor kalveren (stiekem komt ook indi3=X mee)
    verd_goed <- c(10, 88, 2, 11, 89) # correcte aantallenverdeling: 10 + 88 + 2 = 100 & 11 + 89 = 100
    verd_hvm <- c(10, 88, 2, 50, 50) # correcte aantallenverdeling: 10 + 88 + 2 = 100 & 11 + 89 = 100
    data     <- data.frame(dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           vvm_mnd1=verd_goed, vvm_mnd2=verd_goed, vvm_mnd3=verd_goed,
                           hvm_mnd1=verd_hvm, hvm_mnd2=verd_hvm, hvm_mnd3=verd_hvm, hvm_mnd4=verd_hvm,
                           ind1=c("", "", "", "", ""), ind2=c("", "", "", "", ""), ind3=c("", "", "", "", ""),
                           row.names=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           check.names=FALSE, stringsAsFactors=FALSE)

    data_def <- data.frame(vsmd=c(201711,201711,201711,201711,201711),
                           dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           verd=verd_goed, stringsAsFactors=FALSE)

    res  <- afl_leid_af_indicatoren_verd(data, data_def)

    verw <- data.frame(dcat=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           vvm_mnd1=verd_goed, vvm_mnd2=verd_goed, vvm_mnd3=verd_goed,
                           hvm_mnd1=verd_hvm, hvm_mnd2=verd_hvm, hvm_mnd3=verd_hvm, hvm_mnd4=verd_hvm,
                           ind1=c("", "", "", "", ""), ind2=c("", "", "", "X", "X"), ind3=c("", "", "", "X", "X"),
                           row.names=c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd"),
                           check.names=FALSE, stringsAsFactors=FALSE)
    expect_identical(object=res, expected=verw)

})

##############################################################################
context("afl_leid_af_indicatoren_verd() ind3=X")  # is al getest bij afl_leid_af_indicatoren_verd() ind2=X

##############################################################################
context("afl_leid_af_indicatoren_gemg() geen indicator")
test_that("geen indicator voor gemiddelde aantallen", {

    # geen indicator # correcte gemiddelde gewichten
    aant_goed <- c(400, 300, 200, 150, 200, 20, 30, 95, 200, 13,
                   1.7, 2.0, 2.0, 0.4, 0.8, 8.0, 12.0, 1.3, 0.4, 150)
    dcat <- c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd",
                 "Kalveren 8-12 mnd", "Lammeren", "Volwassen schapen", "Varkens",
                 "Eenhoevige dieren", "Geiten", "Vleeskuikens", "Overige kippen",
                 "Eenden", "Duiven", "Fazanten", "Ganzen", "Kalkoenen",
                 "Parelhoenders", "Patrijzen", "Struisvogels")
    ind_leeg <- c("", "", "", "", "", "", "", "", "", "",
                   "", "", "", "", "", "", "", "", "", "")
    data     <- data.frame(dcat=dcat,
                           vvm_mnd1=aant_goed, vvm_mnd2=aant_goed, vvm_mnd3=aant_goed,
                           hvm_mnd1=aant_goed, hvm_mnd2=aant_goed, hvm_mnd3=aant_goed, hvm_mnd4=aant_goed,
                           ind1=ind_leeg, ind2=ind_leeg, ind3=ind_leeg,
                           row.names=dcat, check.names=FALSE, stringsAsFactors=FALSE)

    res  <- afl_leid_af_indicatoren_gemg(data)

    ind2 <- c(330, 285, 190, 135, 180, 18, 25,  90, 170, 12, 1.5, 1.9, 1.8, 0.3, 0.6,  1.5, 3.0, 1.0, 0.3, 100)
    ind3 <- c(475, 315, 250, 160, 205, 23, 35, 100, 340, 14, 2.0, 2.5, 2.5, 0.5, 1.0, 12.0,  15, 1.5, 0.5, 160)
    verw <- data.frame(dcat=dcat,
                           vvm_mnd1=aant_goed, vvm_mnd2=aant_goed, vvm_mnd3=aant_goed,
                           hvm_mnd1=aant_goed, hvm_mnd2=aant_goed, hvm_mnd3=aant_goed, hvm_mnd4=aant_goed,
                           ind1=ind_leeg, ind2=ind2, ind3=ind3,
                           row.names=dcat, check.names=FALSE, stringsAsFactors=FALSE)
    expect_identical(object=res, expected=verw)

})

##############################################################################
context("afl_leid_af_indicatoren_gemg() ind1=X")
test_that("ind1=X voor gemiddelde aantallen", {

    # geen indicator # correcte gemiddelde gewichten
    aant_goed <- c(888, 300, 200, 150, 200, 20, 30, 95, 200, 13,
                   1.7, 2.0, 2.0, 0.4, 0.8, 8.0, 12.0, 1.3, 0.4, 150)
    dcat <- c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd",
                 "Kalveren 8-12 mnd", "Lammeren", "Volwassen schapen", "Varkens",
                 "Eenhoevige dieren", "Geiten", "Vleeskuikens", "Overige kippen",
                 "Eenden", "Duiven", "Fazanten", "Ganzen", "Kalkoenen",
                 "Parelhoenders", "Patrijzen", "Struisvogels")
    ind_leeg <- c("", "", "", "", "", "", "", "", "", "",
                   "", "", "", "", "", "", "", "", "", "")
    data     <- data.frame(dcat=dcat,
                           vvm_mnd1=aant_goed, vvm_mnd2=aant_goed, vvm_mnd3=aant_goed,
                           hvm_mnd1=aant_goed, hvm_mnd2=aant_goed, hvm_mnd3=aant_goed, hvm_mnd4=aant_goed,
                           ind1=ind_leeg, ind2=ind_leeg, ind3=ind_leeg,
                           row.names=dcat, check.names=FALSE, stringsAsFactors=FALSE)

    res  <- afl_leid_af_indicatoren_gemg(data)
    ind_niet_leeg <- c("X", "", "", "", "", "", "", "", "", "",
                       "", "", "", "", "", "", "", "", "", "")
    ind2 <- c(330, 285, 190, 135, 180, 18, 25,  90, 170, 12, 1.5, 1.9, 1.8, 0.3, 0.6,  1.5, 3.0, 1.0, 0.3, 100)
    ind3 <- c(475, 315, 250, 160, 205, 23, 35, 100, 340, 14, 2.0, 2.5, 2.5, 0.5, 1.0, 12.0,  15, 1.5, 0.5, 160)
    verw <- data.frame(dcat=dcat,
                           vvm_mnd1=aant_goed, vvm_mnd2=aant_goed, vvm_mnd3=aant_goed,
                           hvm_mnd1=aant_goed, hvm_mnd2=aant_goed, hvm_mnd3=aant_goed, hvm_mnd4=aant_goed,
                           ind1=ind_niet_leeg, ind2=ind2, ind3=ind3,
                           row.names=dcat, check.names=FALSE, stringsAsFactors=FALSE)
    expect_identical(object=res, expected=verw)

})
