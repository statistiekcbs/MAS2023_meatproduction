#' Naam        : outputbestand_DSC_witvlees.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om het outputbestand witvlees voor DSC te maken.


#' Hercodeer dsrt van witvlees voor DSC
#'
#' De intern gebruikte diersoort omschrijving dsrt wordt omgecodeerd naar
#' de DSC-benaming.
#'
#' @param dsrt string intern gebruikte diersoort omschrijving
#' @return string met DSC-benaming
#' @example
#' > hercodeer_wtvl_dsrt_dsc(ds="Overige kippen")
#' "Kippen"
hercodeer_wtvl_dsrt_dsc <- function(ds) {
  if (ds == "Struisvogels") {
    return ("Struisvogels")
  } else if (ds == "Duiven") {
    return ("Duiven")
  } else if (ds == "Eenden") {
    return ("Eenden")
  } else if (ds == "Kalkoenen") {
    return ("Kalkoenen")
  } else if (ds == "Overige kippen") { # eigenlijk enige hercodering
    return ("Kippen")
  } else if (ds == "Parelhoenders") {
    return ("Parelhoenders")
  } else if (ds == "Vleeskuikens") {
    return ("Vleeskuikens")
  } else if (ds == "Fazanten") {
    return ("Fazanten")
  } else if (ds == "Ganzen") {
    return ("Ganzen")
  } else if (ds == "Patrijzen") {
    return ("Patrijzen")
  }
}


#' Maak het outputbestand witvlees voor DSC
#'
#' @param df dataframe met weg te schrijven data
#' @param outputbestand string met volledig pad incl naam van outputbestand
#' @return txt string met melding of maken outputbestand gelukt is of niet
maak_output_dsc_wtvl <- function(df, outputbestand) {
  logdebug(msg="Systeem start met maken outputbestand witvlees DSC")

  # hercodeer dsrt naar DSC-benaming
  df$dsrt_dsc <- apply(df[c("dsrt")], 1, function(x) hercodeer_wtvl_dsrt_dsc(x))

  # selecteer kolommen en zet in juiste volgorde
  df <- df[c("slnm", "dsrt_dsc", "aant_gaaf")]

  # output moet beginnen met BOM om utf-8 te forceren in editor  
  BOM <- charToRaw('\xEF\xBB\xBF')
  con <- file(description=outputbestand, open="wb")
  writeBin(BOM, con, endian="little")
  close(con)

  # schrijf data weg naar outputbestand
  write.table(x=df, file=outputbestand, append=TRUE, sep=";", quote=c(1),
              row.names=FALSE, col.names=FALSE, fileEncoding="utf-8")

  # verifieer of outputbestand gemaakt is
  if (file.exists(outputbestand)) {
    txt <- paste("bestand", basename(outputbestand), "is gemaakt")
  } else {
    txt <- paste("bestand", basename(outputbestand), "is niet gemaakt")
  }

  logdebug(msg="Systeem is gereed met maken outputbestand witvlees DSC")
  return(txt)
}
