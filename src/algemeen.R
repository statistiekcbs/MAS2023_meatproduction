#' Naam        : algemeen.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module bevat functies die voor verschillende deelprocessen
#'               worden gebruikt.


#' Vind de default inputmap.
#'
#' De default inputmap is de jaarmaand-map behorend bij het moment dat de tool
#' wordt opgestart minus twee maanden.
#'
#' Vb. Als de gebruiker het systeem opstart op 03-04-2018, dan is de
#'     bijbehorende default inputmap 201802
#' @param datamap string. Volledig pad naar datamap
#' @return volledig pad naar default inputmap

alg_vind_default_inputmap <- function(datamap=App$datamap) {
  vwmd      <- as.Date(Sys.Date()) %m-% months(2)
  jaar      <- format(vwmd, "%Y")
  maand     <- format(vwmd, "%m")
  jaarmaand <- paste0(jaar, maand)
  inputmap  <- paste(datamap, jaar, jaarmaand, sep="\\")

  return(inputmap)
}


#' Vind de controle- en correctiebestanden.
#'
#' De controle- en correctiebestanden staan in de submap Verwerkingsmap
#' van de gekozen inputmap op het moment dat er een proces wordt gestart.
#'
#' @param inputmap string. Volledig pad naar inputmap
#' @param vwmd string met verwerkingsmaand (JJJJMM)
#' @param verwerkingsmap string. naam van de submap voor verwerking
#' @return 4 controle- en correctiebestanden
alg_vind_ctcr_bestanden <- function(werkmap=App$werkmap, vwmd){
  jaar <- substr(vwmd,1,4)
  a <- file.path(werkmap, jaar, vwmd, sub("VWMD", vwmd, App$ctcr_aant_rdvl))
  b <- file.path(werkmap, jaar, vwmd, sub("VWMD", vwmd, App$ctcr_aant_wtvl))
  c <- file.path(werkmap, jaar, vwmd, sub("VWMD", vwmd, App$ctcr_verd))
  d <- file.path(werkmap, jaar, vwmd, sub("VWMD", vwmd, App$ctcr_gemg))

  return(list(ctcr_aant_rdvl=a, ctcr_aant_wtvl=b, ctcr_verd=c, ctcr_gemg=d))
}


#' Vind de analysebestanden.
#'
#' De analysebestanden staan in de submap Verwerkingsmap
#' van de gekozen inputmap op het moment dat er een proces wordt gestart.
#'
#' @param inputmap string. Volledig pad naar inputmap
#' @param vwmd string met verwerkingsmaand (JJJJMM)
#' @param verwerkingsmap string. naam van de submap voor verwerking
#' @return 2 analysebestanden
alg_vind_analysebestanden <- function(werkmap=App$werkmap, vwmd=jrmnd$vwmd){
	jaar <- substr(vwmd,1,4)								  
    a <- file.path(werkmap, jaar, vwmd, sub("VWMD", vwmd, App$ana_tijdreeksen_totaal))
    b <- file.path(werkmap, jaar, vwmd, sub("VWMD", vwmd, App$ana_tijdreeksen_biologisch))
    c <- file.path(werkmap, jaar, vwmd, sub("VWMD", vwmd, App$ana_bijschattingen))

  return(list(ana_tijdreeksen_totaal=a, ana_tijdreeksen_biologisch=b, ana_bijschattingen=c))
}


#' Vind de outputbestanden.
#'
#' De outputbestanden staan in de submap disseminatiemap
#' van de gekozen inputmap op het moment dat er een proces wordt gestart.
#'
#' @param inputmap string. Volledig pad naar inputmap
#' @param vwmd string met verwerkingsmaand (JJJJMM)
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @param verwerkingsmap string. naam van de submap voor verwerking
#' @return 2 analysebestanden
alg_vind_outputbestanden <- function(outputmap=App$outputmap, vwmd=jrmnd$vwmd, jrmnd){

    # bepaal jaar en maand voor dsc-bestanden; met spatie!
    jr <- substr(jrmnd$mnd1, 1, 4)
    mnd <- substr(jrmnd$mnd1, 5, 6)
    definitievemaandjaar <- paste(alg_hercodeer_maand(mnd), jr)

	  jaar <- substr(vwmd,1,4)
    a <- file.path(outputmap, jaar, vwmd, App$out_eurostat)
    b <- file.path(outputmap, jaar, vwmd, App$out_statline)
    c <- file.path(outputmap, jaar, vwmd, App$out_contactpersonen)
    d <- file.path(outputmap, jaar, vwmd, paste0(App$out_dsc_rdvl_kop, definitievemaandjaar, App$out_dsc_rdvl_staart))
    e <- file.path(outputmap, jaar, vwmd, paste0(App$out_dsc_wtvl_kop, definitievemaandjaar, App$out_dsc_wtvl_staart))
    f <- file.path(outputmap, jaar, vwmd)
    g <- file.path(outputmap, jaar, vwmd, App$out_bio)

    return(list(out_eurostat=a, out_statline=b, out_contactpersonen=c, out_dsc_rdvl=d, out_dsc_wtvl=e,
                out_mapstructuur=f, out_bio=g))
}


alg_hercodeer_maand <- function(maand){
    if (maand == "01"){strmaand <- "januari"}
    else if (maand == "02"){strmaand <- "februari"}
    else if (maand == "03"){strmaand <- "maart"}
    else if (maand == "04"){strmaand <- "april"}
    else if (maand == "05"){strmaand <- "mei"}
    else if (maand == "06"){strmaand <- "juni"}
    else if (maand == "07"){strmaand <- "juli"}
    else if (maand == "08"){strmaand <- "augustus"}
    else if (maand == "09"){strmaand <- "september"}
    else if (maand == "10"){strmaand <- "oktober"}
    else if (maand == "11"){strmaand <- "november"}
    else if (maand == "12"){strmaand <- "december"}

    return(strmaand)
}


#' Controleer de voorwaarden voor het maken van de controle- en
#' correctiebestanden.
#'
#' Deze functie geeft per ctcr-bestand terug of aan de voorwaarden zijn voldaan
#' om het bestand te genereren. Indien niet aan de voorwaarden voldaan wordt,
#' wordt een foutmelding teruggegeven, die op het scherm wordt getoond. De
#' voorwaarden om ctcr-bestanden te maken zijn:
#'  - leveringen zijn aanwezig
#'  - benodigde tabbladen zijn aanwezig
#'  - controle- en correctiebestanden bestaan nog niet
#'
#' @param input_aant_rdvl string met volledig pad naar levering aantallen roodvlees
#' @param input_aant_wtvl string met volledig pad naar levering aantallen witvlees
#' @param input_verd_gemg string met volledig pad naar levering
#'                        aantallenverdelingen en gemiddelde gewichten
#' @param input_gemg string met volledig pad naar gemiddelde gewichten kalveren
#' @param input_gemg_vj string met volledig pad naar gemiddelde gewichten kalveren vorig jaar
#' @param ctcr_aant_rdvl string met volledig pad naar ctcr aantallen roodvlees
#' @param ctcr_aant_wtvl string met volledig pad naar ctcr aantallen witvlees
#' @param ctcr_verd string met volledig pad naar ctcr aantallenverdelingen
#' @param ctcr_gemg string met volledig pad naar gemiddelde gewichten
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @return file_rdvl_ok boolean die aangeeft of levering roodvlees bestaat
#' @return file_wtvl_ok boolean die aangeeft of levering witvlees bestaat
#' @return file_verd_gemg_ok boolean die aangeeft of levering
#'                           aantallenverdelingen en gemiddeld gewichten bestaat
#' @return file_gemg_kalv_ok boolean die aangeeft of levering gemiddelde
#'                           gewichten kalveren bestaat
#' @return file_gemg_kalv_vj_ok boolean die aangeeft of levering gemiddelde
#'                           gewichten kalveren van vorig jaar bestaat
#' @return sh_verd_gemg_ok boolean die aangeeft of benodigde tabblad(en)
#'                         aanwezig zijn in levering aantallenverdelingen en
#'                         gemiddelde gewichten
#' @return sh_gemg_kalv_ok boolean die aangeeft of benodigde tabblad(en)
#'                         aanwezig zijn in levering gemiddelde gewichten kalveren
#' @return sh_gemg_kalv_vj_ok boolean die aangeeft of benodigde tabblad(en)
#'                         aanwezig zijn in levering gemiddelde gewichten kalveren vorig jaar
#' @return ctcr_rdvl_ok boolean die aangeeft of ctcr roodvlees al bestaat
#' @return ctcr_wtvl_ok boolean die aangeeft of ctcr witvlees al bestaat
#' @return ctcr_verd_ok boolean die aangeeft of ctcr aantallenverdelingen
#'                           en gemiddelde gewichten al bestaat
#' @return ctcr_gemg_ok boolean die aangeeft of ctcr gemiddelde gewichten al bestaat
#' @return txt_rdvl string met eventuele foutmelding mbt roodvlees
#' @return txt_wtvl string met eventuele foutmelding mbt witvlees
#' @return txt_verd string met eventuele foutmelding mbt aantallenverdelingen
#' @return txt_gemg string met eventuele foutmelding mbt gemiddeld
#'                       gewicht kalveren (lege string als geen foutmelding)
alg_controleer_maken_ctcr <- function(input_aant_rdvl, input_aant_wtvl,
                                      input_verd_gemg, input_gemg, input_gemg_vj,
                                      ctcr_aant_rdvl, ctcr_aant_wtvl,
                                      ctcr_verd, ctcr_gemg, jrmnd) {

  # leid af hulpinfo voor de tabbladnamen
  mnd1 <- substr(jrmnd$mnd1, 5, 6)
  jr1  <- substr(jrmnd$mnd1, 1, 4)
  jr4  <- substr(jrmnd$mnd4, 1, 4)

  # controleer of leveringen aanwezig zijn
  file_rdvl_ok         <- file.exists(input_aant_rdvl)
  file_wtvl_ok         <- file.exists(input_aant_wtvl)
  file_verd_gemg_ok    <- file.exists(input_verd_gemg)
  file_gemg_kalv_ok    <- file.exists(input_gemg)
  if (jr1 == jr4) {
    file_gemg_kalv_vj_ok <- TRUE
  }
  else {
    file_gemg_kalv_vj_ok <- file.exists(input_gemg_vj)
  }

  # controleer of tabbladen aanwezig zijn
  sh_verd_gemg_ok <- FALSE
  if (file_verd_gemg_ok) {
    sheets_verd_gemg <- excel_sheets(input_verd_gemg)
    if (mnd1<"10"){
      sh_verd_gemg_ok <- jr1 %in% sheets_verd_gemg
    } else {
      sh_verd_gemg_ok <- (jr1 %in% sheets_verd_gemg) && (jr4 %in% sheets_verd_gemg)
    }
  }

  sh_gemg_kalv_ok <- FALSE
  if (file_gemg_kalv_ok) {
    sheets_gemg_kalv <- excel_sheets(input_gemg)
    sh_gemg_kalv_ok <- paste("Slachtgewichten Kalveren",jr4) %in% sheets_gemg_kalv
  }

  sh_gemg_kalv_vj_ok <- TRUE
  if (jr1!=jr4 && file_gemg_kalv_vj_ok){
    sheets_gemg_kalv_vj <- excel_sheets(input_gemg_vj)
    sh_gemg_kalv_vj_ok <- (paste("Slachtgewichten Kalveren",jr1) %in% sheets_gemg_kalv_vj)
  }

  # controleer of ctcr-bestanden niet aanwezig zijn
  ctcr_rdvl_ok      <- !file.exists(ctcr_aant_rdvl)
  ctcr_wtvl_ok      <- !file.exists(ctcr_aant_wtvl)
  ctcr_verd_ok      <- !file.exists(ctcr_verd)
  ctcr_gemg_ok      <- !file.exists(ctcr_gemg)

  # stel outputtekst naar het scherm samen. We geven maximaal een foutmelding
  # terug. Door de belangrijkste fouten achteraan te zetten geven we altijd de
  # belangrijkste foutmeldingen terug.
  txt_rdvl <- ""
  txt_wtvl <- ""
  txt_verd <- ""
  txt_gemg <- ""
  # foutmelding als ctcr-bestand al bestaat
  if (!ctcr_rdvl_ok) {
    txt_rdvl <- paste("bestand", basename(ctcr_aant_rdvl), "bestaat al",
                      "en wordt niet overschreven")
  }
  if (!ctcr_wtvl_ok) {
    txt_wtvl <- paste("bestand", basename(ctcr_aant_wtvl), "bestaat al",
                      "en wordt niet overschreven")
  }
  if (!ctcr_verd_ok) {
    txt_verd <- paste("bestand", basename(ctcr_verd), "bestaat al",
                      "en wordt niet overschreven")
  }
  if (!ctcr_gemg_ok) {
    txt_gemg <- paste("bestand", basename(ctcr_gemg), "bestaat al",
                           "en wordt niet overschreven")
  }
  # foutmelding als tabblad ontbreekt
  if (!sh_verd_gemg_ok) {
    if (mnd1 < "10") {
      txt_verd <- paste("tabblad", jr1, "ontbreekt in levering",
                        "aantallenverdelingen en gemiddelde gewichten")
      txt_gemg <- paste("tabblad", jr1, "ontbreekt in levering",
                        "aantallenverdelingen en gemiddelde gewichten")
    } else {
      txt_verd <- paste("tabblad", jr1, "of", jr4, "ontbreekt in levering",
                        "aantallenverdelingen en gemiddelde gewichten")
      txt_gemg <- paste("tabblad", jr1, "of", jr4, "ontbreekt in levering",
                        "aantallenverdelingen en gemiddelde gewichten")
    }
  }
  if (!sh_gemg_kalv_ok) {
    txt_gemg <- paste("tabblad Slachtgewichten Kalveren", jr4,
                      "ontbreekt in levering", "gemiddelde gewichten kalveren")
  }
  if (!sh_gemg_kalv_vj_ok) {
    if(nchar(txt_gemg) == 0){
      txt_gemg <- paste0("tabblad Slachtgewichten Kalveren ", jr1,
                         " ontbreekt in levering ",
                         "gemiddelde gewichten kalveren vorig jaar")}
    else {
      txt_gemg <- paste0(txt_gemg, "; tabblad Slachtgewichten Kalveren ", jr1,
                         " ontbreekt in levering ",
                         "gemiddelde gewichten kalveren vorig jaar")
    }
  }

  # foutmelding als levering ontbreekt
  if (!file_rdvl_ok) {
    txt_rdvl <- "levering aantallen roodvlees bestaat niet"
  }
  if (!file_wtvl_ok) {
    txt_wtvl <- "levering aantallen witvlees bestaat niet"
  }
  if (!file_verd_gemg_ok) {
    txt_verd <- "levering aantallenverdelingen en gemiddelde gewichten bestaat niet"
    txt_gemg <- "levering aantallenverdelingen en gemiddelde gewichten bestaat niet"
  }
  if (!file_gemg_kalv_ok) {
    txt_gemg <- "levering gemiddelde gewichten kalveren bestaat niet"
  }
  if (!file_gemg_kalv_vj_ok) {
    if (nchar(txt_gemg) == 0){
      txt_gemg <- "levering gemiddelde gewichten kalveren vorig jaar bestaat niet"
    }
    else {
      txt_gemg <- paste0(txt_gemg, "; levering gemiddelde gewichten kalveren vorig jaar bestaat niet")
    }
  }

  return(list(file_rdvl_ok=file_rdvl_ok,
              file_wtvl_ok=file_wtvl_ok,
              file_verd_gemg_ok=file_verd_gemg_ok,
              file_gemg_kalv_ok=file_gemg_kalv_ok,
              file_gemg_kalv_vj_ok=file_gemg_kalv_vj_ok,
              sh_verd_gemg_ok=sh_verd_gemg_ok,
              sh_gemg_kalv_ok=sh_gemg_kalv_ok,
              sh_gemg_kalv_vj_ok=sh_gemg_kalv_vj_ok,
              ctcr_rdvl_ok=ctcr_rdvl_ok,
              ctcr_wtvl_ok=ctcr_wtvl_ok,
              ctcr_verd_ok=ctcr_verd_ok,
              ctcr_gemg_ok=ctcr_gemg_ok,
              txt_rdvl=txt_rdvl,
              txt_wtvl=txt_wtvl,
              txt_verd=txt_verd,
              txt_gemg=txt_gemg))
 }


#' Controleer de voorwaarden voor het opslaan van de ruwe aantallen en de
#' controle- en correctiebestanden in de database.
#'
#' Deze functie verifieert of alle input- en ctcr-bestanden aanwezig zijn. Indien
#' dit niet het geval is, wordt er een foutmelding teruggegeven, die op het 
#' scherm wordt getoond.
#'
#' @param input_aant_rdvl string met volledig pad naar input aantallen roodvlees
#' @param input_aant_wtvl string met volledig pad naar input aantallen witvlees
#' @param ctcr_aant_rdvl string met volledig pad naar ctcr aantallen roodvlees
#' @param ctcr_aant_wtvl string met volledig pad naar ctcr aantallen witvlees
#' @param ctcr_verd string met volledig pad naar ctcr aantallenverdelingen
#' @param ctcr_gemg string met volledig pad naar gemiddelde gewichten kalveren
#' @return ctcr_ok boolean die aangeeft of alle ctcr-bestanden aanwezig zijn
#' @return txt string met eventuele foutmelding
alg_controleer_opslaan_in_database <- function(input_aant_rdvl_bst, input_aant_wtvl_bst,
                                               ctcr_aant_rdvl, ctcr_aant_wtvl,
                                               ctcr_verd, ctcr_gemg) {

    # controleer of input-bestanden aanwezig zijn
    input_rdvl_ok <- file.exists(input_aant_rdvl_bst)
    input_wtvl_ok <- file.exists(input_aant_wtvl_bst)
    input_ok <- input_rdvl_ok && input_wtvl_ok
    
    # controleer of ctcr-bestanden aanwezig zijn
    ctcr_rdvl_ok <- file.exists(ctcr_aant_rdvl)
    ctcr_wtvl_ok <- file.exists(ctcr_aant_wtvl)
    ctcr_verd_ok <- file.exists(ctcr_verd)
    ctcr_gemg_ok <- file.exists(ctcr_gemg)
    ctcr_ok <- ctcr_rdvl_ok && ctcr_wtvl_ok && ctcr_verd_ok && ctcr_gemg_ok
    
    bestanden_ok <- input_ok && ctcr_ok

    txt <- ""
    # stel outputtekst naar het scherm samen.
    if (!bestanden_ok){
        if (!input_ok){
            txt <- paste(txt, "de inputbestanden zijn niet geselecteerd;")
        } 
        if (!ctcr_ok) {
            txt <- paste(txt, "de controle- en correctiebestanden zijn nog niet",
                         "compleet;")
        }
        txt <- paste(txt, "er zijn geen gegevens opgeslagen in de database")
    }

    return(list(bestanden_ok=bestanden_ok, txt=txt))
}


#' Vind de selectie voor selectiemenu.
#'
#' Dit zijn alle .xls- en .xlsx-bestanden in de meegegeven jaarmaand-map. Geef
#' ook de by default te kiezen optie terug. Dit is de eerste optie die voldoet
#' aan het bijbehorende patroon. Als er meerdere opties zijn, kies
#' dan "<Selecteer...>". Voor het selectiemenu input_gemg_kalveren_vj wordt
#' alleen een resultaat teruggegeven indien dit bestand ook nodig is.
#'
#' @param jaarmaandmap string
#' @param selmenu string
#' @param voorbereidingsmap string. Naam van voorbereidingsmap
#' @param verschil. Verschil tussen verwerkingsmaand en oudste verslagmaand
#' @return selectie list
#' @return gekozen string
alg_vind_selectie <- function(jaarmaandmap, selmenu,
                              verschil=App$verschil_huidige_mnd_eerste_mnd) {
  # vind selectiemenu-afhankelijke parameters
  if (selmenu == "input_aant_rdvl") {
    patroon <- 'roodvlees'
    inputmap     <- file.path(jaarmaandmap)
    bestand_type <- ".xlsx$|.xls$"
  } else if (selmenu == "input_aant_wtvl") {
    patroon <- 'witvlees'
    inputmap     <- file.path(jaarmaandmap)
    bestand_type <- ".xlsx$|.xls$"
  } else if (selmenu == "input_gemg_rdvl") {
    patroon <- 'gem gewicht'
    inputmap     <- file.path(jaarmaandmap)
    bestand_type <- ".xlsx$|.xls$"
  } else if (selmenu == "input_gemg_kalveren") {
    patroon <- 'kalveren'
    inputmap     <- file.path(jaarmaandmap)
    bestand_type <- ".xlsx$|.xls$"
  } else if (selmenu == "input_gemg_kalveren_vj") {
    patroon <- 'kalveren'
    inputmap     <- file.path(jaarmaandmap)
    bestand_type <- ".xlsx$|.xls$"
  } else if (selmenu ==  "input_rvo_slachtingen"){
    patroon <- "input_rvo_slachtingen"
    inputmap     <- file.path(jaarmaandmap)
    bestand_type <- ".csv$"
  } else if (selmenu ==  "input_rvo_slachthuizen"){
    patroon <- "input_rvo_slachthuizen"
    inputmap     <- file.path(jaarmaandmap)
    bestand_type <- ".csv$"
  } else if (selmenu ==  "input_skal"){
    patroon <- "skal"
    inputmap     <- file.path(jaarmaandmap)
    bestand_type <- ".xlsx$"
  } else if (selmenu == "output_rvo_naar_spek") {
    patroon <- "output_rvo_naar_spek"
    inputmap     <- file.path(jaarmaandmap)
    bestand_type <- ".xlsx$|.xls$"
  }

  # vind selectie
  selectie <- list.files(path=inputmap, pattern=bestand_type)

  if (length(selectie)==0) {
    selectie <- c('<Geen bestanden gevonden...>')
    return(list("selectie"=selectie, "kies"=selectie))
  }

  # vind in selectie alle opties die voldoen aan van toepassing zijnde patroon
  opties <- grep(pattern=patroon, x=selectie, ignore.case=TRUE, value=TRUE)

  # kies voor input_gemg_kalveren alleen de bestanden met het jaar van de
  # laatste verslagmaand in de naam. Kies voor input_gemg_kalveren_vj alleen
  # de bestanden met het jaar van de eerste verslagmaand in de naam. Indien
  # alle verslagmaanden in hetzelfde jaar vallen, geef dan terug dat
  # input_gemg_kalveren_vj niet nodig is.
  jrmnd <- alg_vind_jaarmaanden(inputmap=jaarmaandmap, verschil=verschil)
  if (selmenu == "input_gemg_kalveren") {
    patroon <- substr(jrmnd$mnd4, 1, 4)
    opties <- grep(pattern=patroon, x=opties, ignore.case=TRUE, value=TRUE)
  } else if (selmenu == "input_gemg_kalveren_vj") {
    if (substr(jrmnd$mnd1, 1, 4) == substr(jrmnd$mnd4, 1, 4)) {
      selectie <- c('<Geen selectie nodig...>')
      return(list("selectie"=selectie, "kies"=selectie))
    } else {
        patroon <- substr(jrmnd$mnd1, 1, 4)
        opties <- grep(pattern=patroon, x=opties, ignore.case=TRUE, value=TRUE)
    }
  }

  # kies de enige optie of voeg anders "<Selecteer...>" toe en kies deze
  if (length(opties) == 1) {
    kies <- opties[1]
  } else {
    selectie <- c("<Selecteer...>", selectie)
    kies <- selectie[1]
  }

  return(list("selectie"=selectie, "kies"=kies))
  }


#' Vind een random dierenplaatje om te tonen in de gui.
#'
#' @param jpgmap volledig pad naar map met plaatjes
#' @return padplaatje volledig pad naar bestand met plaatje
alg_vind_plaatje <- function(jpgmap=App$jpgmap) {
  plaatjes   <- list.files(jpgmap, pattern="jpg")
  plaatje    <- sample(plaatjes, 1)
  padplaatje <- file.path(jpgmap, plaatje)

  return(padplaatje)
  }


#' Vind de benodigde jaarmaanden.
#'
#' Deze functie wordt gebruikt om bij een verwerkingsmaand de vier bijbehorende
#' verslagmaanden te vinden. De benodigde verslagmaanden zijn altijd de maanden
#' -1, -2 en -3 tov de verwerkingsmaand in de inputmap plus de verwerkingsmaand
#' zelf. Ook worden de verwerkingsmaand en de maand voorafgaand aan de
#' verwerkingsmaand ('vorige, verwerkingsmaand', daarom vvwmd) teruggegeven.
#'
#' @param inputmap volledig pad naar inputmap en eindigend op jaarmaand
#' @param verschil. Verschil tussen verwerkingsmaand en oudste verslagmaand
#' @return mnd1 t/m mnd4 verslagmaanden behorende bij verwerkingsmaand
#' @return vwmd verwerkingsmaand
#' @return vvwmd vorige verwerkingsmaand
alg_vind_jaarmaanden <- function(inputmap,
                                 verschil=App$verschil_huidige_mnd_eerste_mnd) {
  lengte <- nchar(inputmap)
  jaar  <- substr(inputmap, lengte-5, lengte-2)
  maand <- substr(inputmap, lengte-1, lengte)
  datum <- as.Date(paste(jaar, maand, "01", sep='-'))

  # vind verslagmaanden
  dt_mnd1 <- datum %m+% months(verschil)
  dt_mnd2 <- dt_mnd1 %m+% months(1)
  dt_mnd3 <- dt_mnd1 %m+% months(2)
  dt_mnd4 <- dt_mnd1 %m+% months(3)
  mnd1 <- paste0(substr(dt_mnd1,1,4), substr(dt_mnd1,6,7))
  mnd2 <- paste0(substr(dt_mnd2,1,4), substr(dt_mnd2,6,7))
  mnd3 <- paste0(substr(dt_mnd3,1,4), substr(dt_mnd3,6,7))
  mnd4 <- paste0(substr(dt_mnd4,1,4), substr(dt_mnd4,6,7))

  # vind verwerkingsmaand (is gelijk aan jaarmaand van inputmap)
  vwmd <- paste0(substr(datum,1,4), substr(datum,6,7)) # zelfde als mnd4

  # vind vorige verwerkingsmaand
  dt_vvwmd <- datum %m-% months(1)
  vvwmd <- paste0(substr(dt_vvwmd,1,4), substr(dt_vvwmd,6,7))

  return(list(mnd1=mnd1, mnd2=mnd2, mnd3=mnd3, mnd4=mnd4,
              vwmd=vwmd, vvwmd=vvwmd))
}


#' Bepaal of waarde leeg of NA is.
#'
#' @param w waarde
#' @return boolean of waarde leeg of NA is (TRUE) of niet (FALSE)
alg_is_leeg <- function(w) {
    if (!is.na(w)) {
        if (trimws(w) != "") {
            return(FALSE)
        }
    }
    return(TRUE)
}


#' Controleer of een jaarmaand map bestaat voor output/werk bestanden. Zo niet,
#' wordt er een map aangemaakt.
#'
#' @param output_map character. De rootmap voor de Output/Werk map, zoals 
#' gedefinieerd in het configuratiebestand App.ini
#' @param jrmnd character. Geeft de verwerkingsmaand aan in het format yyyymm 
#'
#' @return niets.
#'
#' @examples
#' alg_bestaat_jaarmaandmap(App$werkmap, getElement(jrmnd, "vwmd"))
alg_bestaat_jaarmaandmap <- function(output_map, jrmnd) {
  
  # maak het pad naar de map waar bestanden worden opgeslagen
  jaar <- substring(jrmnd, 1, 4)
  map_pad <- file.path(output_map, jaar, jrmnd)
  
  # maak de mappen aan als het pad niet bestaat
  if (!dir.exists(map_pad)) {dir.create(map_pad, recursive = TRUE)}
}

#' Controleer of de bestandsnaam niet al eerder gedraaid is en als dat wel het
#' geval is, of het bestand anders is dan het eerder gedraaide bestand.
#' 
#' @param input_pad character. Het pad naar het inputbestand
#' @param dbschema character. Het schema waarvan de leveringtabel wordt opgehaald.
#' Standaard wordt deze opgehaald uit de app.ini.
#' @param tbl_naam character. De naam van de leveringtabel. standaard = "tbl_levering_rvo".
#' @param con de connectie naar de database.
#' @param datum_kolommen character. Wat bevatten de kolommen met datums.
#' standaard = "datum".
#' @param controle Logical. Standaard TRUE zodat de bestandsnaam gecontroleerd wordt.
#' Bij FALSE wordt de controle overgeslagen.
#' 
#' @return error als het bestand hetzelfde is. Niets als alles goed is.
#' 
#' @examples
#' # controleer_inlees_bestand(file.path(App$datamap, inputbestand), con = con)

controleer_inlees_bestand <- function(input_pad,
                                      dbschema = App$databaseschema,
                                      tbl_naam = "tbl_levering_rvo",
                                      con,
                                      datum_kolommen = "datum",
                                      controle = TRUE) {
  if (controle) {
    vorige_levering <- dplyr::tbl(con, dbplyr::in_schema(dbschema, tbl_naam)) |>
      dplyr::filter(levering_id == max(levering_id, na.rm = TRUE)) |>
      dplyr::collect() |>
      dplyr::mutate(
        dplyr::across(tidyselect::contains(datum_kolommen),
                      .fns = lubridate::as_date)
      )
    
    input_hash <- cli::hash_file_sha256(input_pad)
    if (input_hash == vorige_levering$bestandshash) {
         rlang::abort(
          c(
            "x" = "Er wordt geprobeerd hetzelfde bestand te verwerken.",
            "i" = "Lees de volgende maand of een vernieuwd bestand in."
          )
        )
      return(TRUE)
    }
  }
}


#-------------------------------------------------------------------------------

alg_ctcr_harde_inputfouten <- function(ctrl) {
  fouten <- character(0)
  
  voeg_fout_toe <- function(ok, melding) {
    if (!isTRUE(ok) && length(melding) == 1 && nzchar(melding)) {
      fouten <<- c(fouten, melding)
    }
  }
  
  # Ontbrekende inputbestanden
  voeg_fout_toe(ctrl$file_rdvl_ok, ctrl$txt_rdvl)
  voeg_fout_toe(ctrl$file_wtvl_ok, ctrl$txt_wtvl)
  voeg_fout_toe(ctrl$file_verd_gemg_ok, ctrl$txt_verd)
  voeg_fout_toe(ctrl$file_gemg_kalv_ok, ctrl$txt_gemg)
  voeg_fout_toe(ctrl$file_gemg_kalv_vj_ok, ctrl$txt_gemg)
  
  # Ontbrekende/verkeerde tabbladen
  voeg_fout_toe(ctrl$sh_verd_gemg_ok, ctrl$txt_verd)
  voeg_fout_toe(ctrl$sh_gemg_kalv_ok, ctrl$txt_gemg)
  voeg_fout_toe(ctrl$sh_gemg_kalv_vj_ok, ctrl$txt_gemg)
  
  unique(fouten[nzchar(fouten)])
}




