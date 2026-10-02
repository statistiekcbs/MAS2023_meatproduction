#' Naam        : afleiden_indicatoren.R
#' Auteur(s)   : Mariska Dank (MDAK), Renzo Ghianni (FGNI),
#'               Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om de indicatoren voor de controle- en correctiebestanden
#'               af te leiden.


#' Voeg de nieuwe indicator toe
#'
#' Deze functie plakt de nieuwe indicator achter reeds afgeleide indicator(en),
#' gescheiden door een puntkomma.
#'
#' @param df_cel string met reeds afgeleide indicator(en) (lege string indien
#'                nog geen indicator)
#' @param ind_txt Nieuwe indicator (hoeft geen string te zijn)
#' @return nieuwe indicator
afl_voeg_toe_indicator <- function(df_cel, ind_txt, scheider=";") {
  if (df_cel == "") {
    nieuwe_indicator <- as.character(ind_txt)
  } else if (ind_txt == "" | is.na(ind_txt)) {
    nieuwe_indicator <- df_cel
  } else {
    nieuwe_indicator <- paste(df_cel, ind_txt, sep=scheider)
  }

  return(nieuwe_indicator)
}


#' Leid indicatoren af voor controle- en correctiebestanden voor aantallen
#' roodvlees en aantallen witvlees
#'
#' Bepaal de waarde van de indicatoren op basis van nieuwe levering en historie.
#'
#' @param data dataframe met inhoud van ctcr-bestand
#' @param data_def dataframe met waarden uit laatste 12 maanden die definitief
#'                 zijn om 12-maandsgemiddelde uit af te leiden
#' @param data_lev dataframe met data van levering
#' @param data_vorige_lev dataframe met data van vorige levering
#' @param jrmnd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @return data dataframe met inhoud van ctcr-bestand en afgeleide indicatoren
#' @example (achteraf bijwerken met realistische voorbeelden)
#' > data
#'                    slnm    wrkp               dsrt vvm_mnd1 vvm_mnd2 vvm_mnd3 201712 201801 201802 201803 ind1 ind2 ind3
#' BEDRIJFSNAAM 1 0901354           Kalveren     1500     1500     1600   1500   1500   1600   1600
#' BEDRIJFSNAAM 1 0901354 Volwassen runderen     5000     5000     7200   5000   5000   7200   6900
#'  BEDRIJFSNAAM 2 1646092  Eenhoevige dieren        6        1        2      6      1      2      1
#' > data_def
#'   vsmd    wrkp                      slnm               dsrt aant_gaaf
#' 201711 0300445 BEDRIJFSNAAM 3             Geiten         2
#' 201711 0300773         BEDRIJFSNAAM 4             Geiten       500
#' 201711 0300773         BEDRIJFSNAAM 4           Kalveren      2457
#' > data_lev
#'     vwmd   vsmd    wrkp                        slnm              dsrt aant
#'  201803 201712 0200098                BEDRIJFSNAAM 5 Eenhoevige dieren    2
#'  201803 201712 0200586      BEDRIJFSNAAM 6 Eenhoevige dieren    1
#'  201803 201712 0201432             BEDRIJFSNAAM 7 Eenhoevige dieren    7
#' > data_vorige_lev
#'   vwmd   vsmd    wrkp                        slnm               dsrt aant_ruw aant_gaaf
#' 201802 201711 0300875 BEDRIJFSNAAM 8            Varkens      167       167
#' 201802 201711 0300875 BEDRIJFSNAAM 8 Volwassen runderen       54        54
#' 201802 201711 0300988     BEDRIJFSNAAM 9           Lammeren       13        13
#' > afl_leid_af_indicatoren_rdvl_wtvl(data, data_def, data_lev, data_vorige_lev)
#'                    slnm    wrkp               dsrt vvm_mnd1 vvm_mnd2 vvm_mnd3 201712 201801 201802 201803 ind1 ind2 ind3
#' BEDRIJFSNAAM 1 0901354           Kalveren     1500     1500     1600   1500   1500   1600   1600
#' BEDRIJFSNAAM 1 0901354 Volwassen runderen     5000     5000     7200   5000   5000   7200   6900
#'  BEDRIJFSNAAM 2 1646092  Eenhoevige dieren        6        1        2      6      1    200      1         O
afl_leid_af_indicatoren_rdvl_wtvl <- function(data, data_def, data_lev,
                                              data_vorige_lev, jrmnd) {
  # zet kolommen eerst terug naar numeriek
  data_def[, "aant_gaaf"] <- sapply(data_def[, "aant_gaaf"], as.numeric)

  # ##################### indicator 1 (= Bedrijf) ####################### #
  # als werkpleknummer voorkomt in levering, maar niet in levering van vorige
  # verwerkingsmaand, dan ind1 = N (bedrijf wellicht nieuw)

  # vind alle unieke wrkp's in vorige levering
  wrkp_vorige_lev <- unique(data_vorige_lev[, "wrkp"])

  # controleer voor elke rij in data of wrkp voorkomt in vorige levering
  for (i in 1:nrow(data)){
      if (!(data[i, "wrkp"] %in% wrkp_vorige_lev)) { # wrkp niet in vorige levering
      data[i,"ind1"] <- afl_voeg_toe_indicator(df_cel=data[i,"ind1"], ind_txt="N")
    }
  }

  # ##################### indicator 1 (= Bedrijf) ####################### #
  # als werkpleknummer voor de laatste vier maanden geen waarden heeft,
  # dan ind1 = S (bedrijf wellicht gestopt)

  # vind alle unieke wrkp's in levering
  wrkp_lev <- unique(data_lev[, "wrkp"])

#   # vind alle wrkp's waarvoor de maximale vsmd kleiner is dan de oudste
#   # verslagmaand
#   browser()
#   max_data_lev <- aggregate(data_lev, by=list(data_lev$wrkp), FUN="max")
#   wellicht_gestopt <- max_data_lev[max_data_lev$vsmd < jrmnd$mnd1, ]

  # zet indicator in rij als wrkp wellicht gestopt is
  for (i in 1:nrow(data)) {
    if (!(data[i, "wrkp"] %in% wrkp_lev)) {
      data[i,"ind1"] <- afl_voeg_toe_indicator(df_cel=data[i,"ind1"], ind_txt="S")
    }
  }

  # ##################### indicator 2 (= Aantal) ####################### #
  # als totaal aantal dieren onverwacht hoog of laag is, dan ind2 = O
  # als totaal aantal dieren gemiddeld < 1.000 en afwijking >= 100%, dan ind2 = O
  # als 1.000 =< totaal aantal dieren gemiddeld < 10.000 en afwijking >= 50%, dan ind2 = O
  # als totaal aantal dieren gemiddeld >= 10.000 en afwijking >= 25%, dan ind2 = O
  # browser()
  # zet alle aantallen dieren uit de levering incl bijschattingen in 1 dataframe
  # onder elkaar
  data_hvm1 <- data[, c("wrkp", jrmnd$mnd1)] # kolom wrkp en hvm1
  data_hvm2 <- data[, c("wrkp", jrmnd$mnd2)] # kolom wrkp en hvm2
  data_hvm3 <- data[, c("wrkp", jrmnd$mnd3)] # kolom wrkp en hvm3
  data_hvm4 <- data[, c("wrkp", jrmnd$mnd4)] # kolom wrkp en hvm4
  data_hvm <- as.data.frame(rbind(as.matrix(data_hvm1), as.matrix(data_hvm2),
                                  as.matrix(data_hvm3), as.matrix(data_hvm4)))
  rownames(data_hvm4) <- NULL
  colnames(data_hvm4) <- c("wrkp", "aant")
  data_hvm4$wrkp <- as.character(data_hvm4$wrkp) # van factor naar character
  data_hvm4$aant <- as.numeric(as.character(data_hvm4$aant)) # van factor naar numeric
  data_hvm4 <- data_hvm4[!(is.na(data_hvm4$aant)),] # NA's eruit

  # bepaal gemiddelde aantallen dieren per wrkp uit levering incl bijschattingen
  gem_mnd4_hvm <- aggregate(data_hvm4[, c("aant")], by=list(data_hvm4$wrkp), FUN="mean")
  colnames(gem_mnd4_hvm) <- c("wrkp", "gem")
  #browser()
  gem_mnd4_hvm <- gem_mnd4_hvm[gem_mnd4_hvm$gem != 0,] #MDAK; verderop in de programmatuur wordt gedeeld door gem_def. Delen door 0 kan niet dus die moet er uit gehaald worden.

  # bepaal gemiddelde aantallen dieren per wrkp uit 12 definitieve maanden
  gem_data_def <- aggregate(data_def[, c("aant_gaaf")], by=list(data_def$wrkp), FUN="mean", na.rm=TRUE)
  #print(gem_data_def)
  colnames(gem_data_def) <- c("wrkp", "gem")
  gem_data_def <- gem_data_def[gem_data_def$gem != 0,]  #MDAK; verderop in de programmatuur wordt gedeeld door gem_def. Delen door 0 kan niet dus die moet er uit gehaald worden.
  # controleer voor elke rij of afwijking aantal dieren te groot is obv
  # gemiddeld aantal in laatste verslagmaand van huidige verwerkingsmaand (gem_lvm) 
  # en gemiddeld aantal in laatste 12 definitieve maanden (gem_def)
  #browser()
  for (i in 1:nrow(data)) {
    zet_indicator <- FALSE
    wrkp <- data[i, "wrkp"]
    gem_lvm <- gem_mnd4_hvm[gem_mnd4_hvm$wrkp == wrkp, "gem"]
    gem_def <- gem_data_def[gem_data_def$wrkp == wrkp, "gem"]
    #print(gem_def)
    #print(gem_lvm)
    #print(wrkp)
    if (length(gem_def) == 0) {
      zet_indicator <- FALSE
    } else if (length(gem_lvm) == 0) {
      zet_indicator <- FALSE
    } else if (gem_def < 1000 & abs((gem_lvm - gem_def)/gem_def) >= 1) {
      zet_indicator <- TRUE
    } else if (gem_def >= 1000 & gem_def < 10000 & abs((gem_lvm - gem_def)/gem_def) >= 0.5) {
      zet_indicator <- TRUE
    } else if (gem_def >= 10000 & abs((gem_lvm - gem_def)/gem_def) >= 0.25) {
      zet_indicator <- TRUE
    }
    if (zet_indicator) {
      data[i,"ind2"] <- afl_voeg_toe_indicator(df_cel=data[i,"ind2"], ind_txt="O")
    }
    if (length(gem_def) != 0 ) {
        data[i,"ind3"] <- gem_def
    }

  }
  
  data[, ncol(data)] <- sapply(data[, ncol(data)], as.numeric) # maak van de waarden numerics
  
  # ##################### indicator 2 (= Aantal) ####################### #
  # als aantal dieren lager is dan aantal dieren in dezelfde verslagmaand van
  # de vorige verwerkingsmaand, dan ind2 = K

  # zet kolomnummer van corresponderende verslagmaanden in een lijst van vectoren
  # vb. kolom 4 vvm_mnd1 (vorige verwerkingsmaand) gaat over dezelfde
  #     verslagmaand als kolom 7 (huidige verwerkingsmaand)
  corr_kolnamen <- list(c("vvm_mnd1",jrmnd$mnd1), 
                      c("vvm_mnd2",jrmnd$mnd2), 
                      c("vvm_mnd3",jrmnd$mnd3))
  # controleer voor elke rij of waarde in huidige verwerkingsmaand lager is dan
  # in vorige verwerkingsmaand
  for (i in 1:nrow(data)) {
    zet_indicator = FALSE
    for (kn in corr_kolnamen) {
      # kn[1] is de waarde in de vorige verwerkingsmaand en kn[2] de waarde in
      # de huidige verwerkingsmaand voor dezelfde verslagmaand

      # als beide waarden in dezelfde verslagmaand niet NA zijn
      if (!is.na(data[i, kn[1]]) & !is.na(data[i, kn[2]])) {
        if (data[i, kn[2]] < data[i, kn[1]]) {
          zet_indicator = TRUE
        }
      }
      # als waarde in vorige verslagmaand niet NA was, maar in huidige
      # verslagmaand wel
      if (!is.na(data[i, kn[1]]) & is.na(data[i, kn[2]])) {
          zet_indicator = TRUE
      }
    }

    if (zet_indicator) {
      data[i,"ind2"] <- afl_voeg_toe_indicator(df_cel=data[i,"ind2"], ind_txt="K")
    }
  }

  return(data)
}


#' Leid indicatoren af voor controle- en correctiebestand voor aantallenverdelingen
#'
#' Bepaal de waarde van de indicatoren op basis van nieuwe levering en historie.
#'
#' @param data dataframe met inhoud van ctcr-bestand
#' @param data_def dataframe met waarden uit laatste 12 maanden die definitief
#'                 zijn om 12-maandsgemiddelde uit af te leiden
#' @return data dataframe met inhoud van ctcr-bestand en afgeleide indicatoren
#' @example
#' > data
#'                                dcat vvm_mnd1  vvm_mnd2 vvm_mnd3 hvm_mnd1 hvm_mnd2 hvm_mnd3 hvm_mnd4 ind1 ind2 ind3
#' Stieren                     Stieren     9.47  8.620308 10.49519     9.47     9.00    10.93    10.89
#' Koeien                       Koeien    88.20 89.049692 87.17481    88.20    88.67    86.74    86.78
#' Vaarzen                     Vaarzen     2.33  2.330000  2.33000     2.33     2.33     2.33     2.33
#' Kalveren 0-8 mnd   Kalveren 0-8 mnd    90.25 90.250000 90.25000    90.25    90.25    90.25    90.25
#' Kalveren 8-12 mnd Kalveren 8-12 mnd     9.75  9.750000  9.75000     9.75     9.75     9.75     9.75
#' > data_def
#'    vsmd               dsrt              dcat      verd
#'  201611 Volwassen runderen           Stieren  9.430000
#'  201611 Volwassen runderen            Koeien 88.240000
#'  201611 Volwassen runderen           Vaarzen  2.330000
#'  201611           Kalveren  Kalveren 0-8 mnd 89.830000
#'  201611           Kalveren Kalveren 8-12 mnd 10.170000
#' > afl_leid_af_indicatoren_verd(data, data_def)
#'                                dcat vvm_mnd1  vvm_mnd2 vvm_mnd3 hvm_mnd1 hvm_mnd2 hvm_mnd3 hvm_mnd4 ind1 ind2 ind3
#' Stieren                     Stieren     9.47  8.620308 10.49519     9.47     9.00    10.93    10.89    X
#' Koeien                       Koeien    88.20 89.049692 87.17481    88.20    88.67    86.74    90.00    X
#' Vaarzen                     Vaarzen     2.33  2.330000  2.33000     2.33     2.33     2.33     2.33    X
#' Kalveren 0-8 mnd   Kalveren 0-8 mnd    90.25 90.250000 90.25000    90.25    90.25    90.25    90.25
#' Kalveren 8-12 mnd Kalveren 8-12 mnd     9.75  9.750000  9.75000     9.75     9.75     9.75     9.75
afl_leid_af_indicatoren_verd <- function(data, data_def) {
  # zet kolommen eerst terug naar numeriek
  ind <- c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3", "hvm_mnd1", "hvm_mnd2", "hvm_mnd3", "hvm_mnd4")
  data[, ind] <- sapply(data[, ind], as.numeric)
  data_def[, "verd"] <- sapply(data_def[, "verd"], as.numeric)

  # ##################### indicator 1 (= Som != 100%) ####################### #
  # als verdeling niet optelt tot 100%, dan ind1 = X
  for (mnd in c("hvm_mnd1", "hvm_mnd2", "hvm_mnd3", "hvm_mnd4")) {
    # als stieren + koeien + vaarzen != 100%
    if ((data["Stieren",mnd] + data["Koeien",mnd] + data["Vaarzen",mnd] <  99.99) ||
        (data["Stieren",mnd] + data["Koeien",mnd] + data["Vaarzen",mnd] > 100.01)) {
      data["Stieren","ind1"] <- afl_voeg_toe_indicator(df_cel=data["Stieren","ind1"], ind_txt="X")
      data["Koeien","ind1"]  <- afl_voeg_toe_indicator(df_cel=data["Koeien","ind1"], ind_txt="X")
      data["Vaarzen","ind1"] <- afl_voeg_toe_indicator(df_cel=data["Vaarzen","ind1"], ind_txt="X")
    }

    # als totaal kalveren != 100%
    if ((data["Kalveren 0-8 mnd",mnd] + data["Kalveren 8-12 mnd",mnd] < 99.99) ||
        (data["Kalveren 0-8 mnd",mnd] + data["Kalveren 8-12 mnd",mnd] > 100.01)) {
      data["Kalveren 0-8 mnd","ind1"] <- afl_voeg_toe_indicator(df_cel=data["Kalveren 0-8 mnd","ind1"], ind_txt="X")
      data["Kalveren 8-12 mnd","ind1"] <- afl_voeg_toe_indicator(df_cel=data["Kalveren 8-12 mnd","ind1"], ind_txt="X")
    }
  }

  # ##################### indicator 2 (= Aantal) ############################ #
  # als waarde meer dan 10% afwijkt van 12-maandsgemiddelde, dan ind2 = X

  # vind 12-maandsgemiddelde en zet in dataframe gem
  gem_stier <- mean(data_def[data_def$dcat == "Stieren", 'verd'])
  gem_koe   <- mean(data_def[data_def$dcat == "Koeien", 'verd'])
  gem_vaars <- mean(data_def[data_def$dcat == "Vaarzen", 'verd'])
  gem_kalf1 <- mean(data_def[data_def$dcat == "Kalveren 0-8 mnd", 'verd'])
  gem_kalf2 <- mean(data_def[data_def$dcat == "Kalveren 8-12 mnd", 'verd'])
  gem <- data.frame("verd"=c(gem_stier, gem_koe, gem_vaars, gem_kalf1, gem_kalf2))
  rownames(gem) <- c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd",
                     "Kalveren 8-12 mnd")

  # controleer voor elke diersoort of waarde niet te veel afwijkt van gemiddelde
  # voor elke verslagmaand van de huidige verwerkingsmaand
  for (dcat in c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd",
                 "Kalveren 8-12 mnd")) {
    if(abs(data[dcat,"hvm_mnd4"] - gem[dcat,"verd"]) > 10 ||
       abs(data[dcat,"hvm_mnd3"] - gem[dcat,"verd"]) > 10 ||
       abs(data[dcat,"hvm_mnd2"] - gem[dcat,"verd"]) > 10 ||
       abs(data[dcat,"hvm_mnd1"] - gem[dcat,"verd"]) > 10) {
       data[dcat,"ind2"] <- afl_voeg_toe_indicator(df_cel=data[dcat,"ind2"], ind_txt="X")
    }
  }

  # ##################### indicator 3 (= Verschil) ########################## #
  # als waarde meer dan 5% afwijkt van vorige gevalideerde waarde, dan ind3 = X

  # controleer voor elke diersoort of waarde in huidige verwerkingsmaand niet te
  # veel afwijkt van waarde in vorige verwerkingsmaand
  for (dcat in c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd",
                 "Kalveren 8-12 mnd")) {
    if(abs(data[dcat,"hvm_mnd3"] - data[dcat,"vvm_mnd3"]) > 5 ||
       abs(data[dcat,"hvm_mnd2"] - data[dcat,"vvm_mnd2"]) > 5 ||
       abs(data[dcat,"hvm_mnd1"] - data[dcat,"vvm_mnd1"]) > 5) {
       data[dcat,"ind3"] <- afl_voeg_toe_indicator(df_cel=data[dcat,"ind3"], ind_txt="X")
    }
  }

  return(data)
}


#' Leid indicatoren af voor controle- en correctiebestand voor gemiddelde gewichten
#'
#' Bepaal de waarde van de indicatoren op basis van nieuwe levering en historie.
#' NB. De kolommen van de huidige verwerkingsmaand staan in df_data links
#'
#' @param df_data dataframe met inhoud van ctcr-bestand
#' @return df_data dataframe met inhoud van ctcr-bestand en afgeleide indicatoren
#' @example
#' > data
#'                                dcat hvm_mnd1 hvm_mnd2 hvm_mnd3 hvm_mnd4 vvm_mnd1  vvm_mnd2  vvm_mnd3 ind1  ind2  ind3
#' Stieren                     Stieren   450.78   433.02 429.7400 441.1600   450.78 441.97182 447.91292
#' Kalveren 8-12 mnd Kalveren 8-12 mnd   201.19   190.85 190.2700 192.0100   201.19 203.15400 200.01800
#' Lammeren                   Lammeren    22.06    22.25  23.0700  22.7100    22.06  21.75207  21.66208
#' Varkens                     Varkens    96.30    95.31  95.1500  94.9500    96.30  98.10000  98.00000
#' > afl_leid_af_indicatoren_gemg(data)
#'                                dcat hvm_mnd1 hvm_mnd2 hvm_mnd3 hvm_mnd4 vvm_mnd1  vvm_mnd2  vvm_mnd3 ind1  ind2  ind3
#' Stieren                     Stieren   450.78   433.02 429.7400 441.1600   450.78 441.97182 447.91292      330.0 475.0
#' Kalveren 8-12 mnd Kalveren 8-12 mnd   201.19   190.85 190.2700 192.0100   201.19 203.15400 200.01800      180.0 205.0
#' Lammeren                   Lammeren    22.06    22.25  23.0700  22.7100    22.06  21.75207  21.66208    X  18.0  23.0
#' Varkens                     Varkens    96.30    95.31  95.1500  94.9500    96.30  98.10000  98.00000       90.0 100.0
afl_leid_af_indicatoren_gemg <- function(df_data) {

  # ##################### indicator 2 (= ondergrens) ######################## #
  df_data["Stieren","ind2"]             <- 330
  df_data["Koeien","ind2"]              <- 285
  df_data["Vaarzen","ind2"]             <- 190
  df_data["Kalveren 0-8 mnd","ind2"]    <- 135
  df_data["Kalveren 8-12 mnd","ind2"]   <- 180
  df_data["Lammeren","ind2"]            <- 18
  df_data["Volwassen schapen","ind2"]   <- 25
  df_data["Varkens","ind2"]             <- 90
  df_data["Eenhoevige dieren","ind2"]   <- 170
  df_data["Geiten","ind2"]              <- 12
  df_data["Vleeskuikens","ind2"]        <- 1.5
  df_data["Overige kippen","ind2"]      <- 1.9
  df_data["Eenden","ind2"]              <- 1.8
  df_data["Duiven","ind2"]              <- 0.3
  df_data["Fazanten","ind2"]            <- 0.6
  df_data["Ganzen","ind2"]              <- 1.5
  df_data["Kalkoenen","ind2"]           <- 3.0
  df_data["Parelhoenders","ind2"]       <- 1.0
  df_data["Patrijzen","ind2"]           <- 0.3
  df_data["Struisvogels","ind2"]        <- 100

  # ##################### indicator 3 (= bovengrens) ######################## #
  df_data["Stieren","ind3"]             <- 475
  df_data["Koeien","ind3"]              <- 315
  df_data["Vaarzen","ind3"]             <- 250
  df_data["Kalveren 0-8 mnd","ind3"]    <- 160
  df_data["Kalveren 8-12 mnd","ind3"]   <- 205
  df_data["Lammeren","ind3"]            <- 23
  df_data["Volwassen schapen","ind3"]   <- 35
  df_data["Varkens","ind3"]             <- 100
  df_data["Eenhoevige dieren","ind3"]   <- 340
  df_data["Geiten","ind3"]              <- 14
  df_data["Vleeskuikens","ind3"]        <- 2.0
  df_data["Overige kippen","ind3"]      <- 2.5
  df_data["Eenden","ind3"]              <- 2.5
  df_data["Duiven","ind3"]              <- 0.5
  df_data["Fazanten","ind3"]            <- 1.0
  df_data["Ganzen","ind3"]              <- 12.0
  df_data["Kalkoenen","ind3"]           <- 15
  df_data["Parelhoenders","ind3"]       <- 1.5
  df_data["Patrijzen","ind3"]           <- 0.5
  df_data["Struisvogels","ind3"]        <- 160

  # zet kolommen ondergrens en bovengrens terug naar numeriek
  ind <- c("ind2","ind3")
  df_data[, ind] <- sapply(df_data[, ind], as.numeric)

  # ##################### indicator 1 ######################### #
  # Als waarden van huidige verwerkingsmaand onder de ondergrens
  # of boven de bovengrens liggen, dan ind1 = X

  for (dcat in c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd",
                 "Kalveren 8-12 mnd", "Lammeren", "Volwassen schapen", "Varkens",
                 "Eenhoevige dieren", "Geiten", "Vleeskuikens", "Overige kippen",
                 "Eenden", "Duiven", "Fazanten", "Ganzen", "Kalkoenen",
                 "Parelhoenders", "Patrijzen", "Struisvogels")) {
    if (df_data[dcat,"hvm_mnd1"] < df_data[dcat,"ind2"] ||
        df_data[dcat,"hvm_mnd1"] > df_data[dcat,"ind3"] ||
        df_data[dcat,"hvm_mnd2"] < df_data[dcat,"ind2"] ||
        df_data[dcat,"hvm_mnd2"] > df_data[dcat,"ind3"] ||
        df_data[dcat,"hvm_mnd3"] < df_data[dcat,"ind2"] ||
        df_data[dcat,"hvm_mnd3"] > df_data[dcat,"ind3"] ||
        df_data[dcat,"hvm_mnd4"] < df_data[dcat,"ind2"] ||
        df_data[dcat,"hvm_mnd4"] > df_data[dcat,"ind3"]
        ) {
           df_data[dcat,"ind1"] <- afl_voeg_toe_indicator(df_cel=df_data[dcat,"ind1"], ind_txt="X")
       }
  }

  return(df_data)
}
