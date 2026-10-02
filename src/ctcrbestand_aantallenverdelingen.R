#' Naam        : ctcrbestand_aantallenverdelingen.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om het controle- en correctiebestand voor
#'               aantallenverdelingen te maken. Met dit bestand kan geanalyseerd
#'               worden of de aantallenverdelingen plausibel zijn.


#' Maak het ctcrbestand voor de aantallenverdelingen op.
#'
#' @param wb workbook object containing a worksheet
#' @return wb workbook object containing a worksheet
maak_op_verd <- function (wb) {
    # eerste rij: tekst dikgedrukt en gecentreerd en merge cellen per groep
    addStyle(wb, sheet=1, style=createStyle(textDecoration='bold', halign='center'),
             rows=1, cols=1:11, gridExpand=TRUE, stack=TRUE)
    mergeCells(wb, sheet=1, cols=2:4, rows=1)
    mergeCells(wb, sheet=1, cols=5:8, rows=1)
    mergeCells(wb, sheet=1, cols=9:11, rows=1)

    # tweede rij: tekst dikgedrukt
    addStyle(wb, sheet=1, style=createStyle(textDecoration='bold'),
             rows=2, cols=1:11, gridExpand=TRUE, stack=TRUE)

    # cellen vorige verwerkingsmaand: achtergrondkleur lichtgrijs
    addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_vorige_vwmd),
             rows=2:7, cols=2:4, gridExpand=TRUE, stack=TRUE)

    # cellen nieuwe maand: achtergrondkleur lichtgeel
    addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_huidige_vwmd),
             rows=2:7, cols=8, gridExpand=TRUE, stack=TRUE)

    # cellen kalveren: tekst schuingedrukt want constant
    addStyle(wb, sheet=1, style=createStyle(textDecoration='italic'),
             rows=6:7, cols=2:8, gridExpand=TRUE, stack=TRUE)

    # cellen indicatoren: tekst gecentreerd
    addStyle(wb, sheet=1, style=createStyle(halign='center'),
             rows=2:7, cols=9:11, gridExpand=TRUE, stack=TRUE)

    # horizontale lijn onder regel vaarzen
    addStyle(wb, sheet=1, style=createStyle(border='bottom'),
             rows=5, cols=1:11, gridExpand=TRUE, stack=TRUE)

    # verticale lijn rechts van diersoort
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:7, cols=1, gridExpand=TRUE, stack=TRUE)

    # verticale lijn tussen vorige verwerkingsmaand en huidige verwerkingsmaand
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:7, cols=4, gridExpand=TRUE, stack=TRUE)

    # verticale lijn tussen huidige verwerkingsmaand en indicatoren
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:7, cols=8, gridExpand=TRUE, stack=TRUE)

    # stel kolombreedten in
    setColWidths(wb, sheet=1, cols=1, widths=App$breedte_diersoortcat) # kolom diersoort
    setColWidths(wb, sheet=1, cols=9:11, widths=App$breedte_ind) # alle kolommen indicatoren

    # stel weergave numerieke waarden in: toon afgeronde waarden zonder decimalen
    addStyle(wb, 1, style=createStyle(numFmt="0"),
             rows=3:7, cols=2:8, gridExpand=TRUE, stack=TRUE)

    return(wb)
}


#' Stel het controle- en correctiebestand voor de aantallenverdelingen samen.
#'
#' We schrijven de groepkoppen, de kolomkoppen en de data in 1x weg (inclusief
#' de randen eromheen). Vervolgens voegen we de overige opmaak toe. Ten slotte
#' slaan we het Excelbestand op.
#'
#' @param maanden_vorige_vm list met verslagmaanden horend bij vorige
#'                          verwerkingsmaand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param data dataframe met data om weg te schrijven naar excelsheet
#' @param bestand string met volledig pad naar outputbestand
#' @return niets
stel_samen_ctcr_verd <- function(maanden_vorige_vm, maanden_huidige_vm,
                                 data, bestand) {
  wb <- createWorkbook()
  addWorksheet(wb, "aantallenverdelingen")

  # stel headers samen (rij 1 en 2 en kolom 1)
  groepkoppen <- t(c('', 'Vorige verwerkingsmaand', '', '',
                         'Huidige verwerkingsmaand', '', '', '',
                         'Indicatoren', '', ''))
  kolomkoppen <- t(c('Diersoort', maanden_vorige_vm, maanden_huidige_vm,
                     'Som != 100%', 'Aantal', 'Verschil'))
  rijkoppen <- c('Stieren', 'Koeien', 'Vaarzen',
                 'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd')
  aantallenverdeling <- data.frame(rijkoppen, data)

  # schrijf data weg met buitenrand
  writeData(wb, sheet=1, x=groepkoppen, startRow=1, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=kolomkoppen, startRow=2, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=aantallenverdeling, startRow=3, colNames=F, borders=c("surrounding"))

  wb <- maak_op_verd(wb)
  saveWorkbook(wb, file=bestand, overwrite=TRUE)
}


#' Verzamel de gegevens uit de levering en de historische gegevens en
#' stel op basis hiervan een dataframe samen met ongevulde indicatoren.
#'
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param levering_bst string met volledig pad naar inputbestand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param schema string met databaseschema
#' @return data_plus_indi dataframe met data wat op sheet kan worden gezet
#' @example.
#' > verzamel_data_ctcr_verd(jrmnd, levering_bst, maanden_huidige_vm)
#'                           Row.names hvm_mnd1 hvm_mnd2 hvm_mnd3 hvm_mnd4              dcat vvm_mnd1 vvm_mnd2 vvm_mnd3 ind1 ind2 ind3
#' Stieren                     Stieren     9.47    10.66    10.66    10.66           Stieren
#' Koeien                       Koeien    88.20    81.01    80.01    87.01            Koeien
#' Vaarzen                     Vaarzen     2.33     2.33     2.33     2.33           Vaarzen
#' Kalveren 0-8 mnd   Kalveren 0-8 mnd    90.25    90.25    90.25    90.25  Kalveren 0-8 mnd
#' Kalveren 8-12 mnd Kalveren 8-12 mnd     9.75     9.75     9.75     9.75 Kalveren 8-12 mnd
verzamel_data_ctcr_verd <- function(jrmnd, levering_bst,
                                    schema=App$databaseschema){
  # lees de verdeling voor volwassen runderen in
  df_verd_volw <- inl_lees_input_verd_gemg(jrmnd=jrmnd,
                                           inputbestand=levering_bst)$df_verd

  # voeg constanten toe voor jonge runderen en voeg de gegevens samen
  kolomkoppen_mnd <- c("hvm_mnd1", "hvm_mnd2", "hvm_mnd3", "hvm_mnd4")
  kalf_jong <- 90.25
  kalf_oud  <- 9.75
  kalf_kol  <- c(kalf_jong, kalf_oud)
  namen <- c("Kalveren 0-8 mnd", "Kalveren 8-12 mnd")
  df_verd_jong  <- data.frame(kalf_kol, kalf_kol, kalf_kol, kalf_kol, namen)
  colnames(df_verd_jong) <- c(kolomkoppen_mnd, "dcat")
  rownames(df_verd_jong) <- c("Kalveren 0-8 mnd", "Kalveren 8-12 mnd")
  df_levering <- rbind(df_verd_volw, df_verd_jong)

  # haal gegevens van vorige verwerkingsmaand op uit de database
  df_hist_data <- inl_lees_hist_verd(jrmnd=jrmnd)

  # voeg gegevens samen
  df_data <- merge(df_hist_data, df_levering, by=c("dcat"), sort=FALSE)
  rownames(df_data) <- df_data$dcat

  # initialiseer indicatoren en voeg toe
  df_indi <- data.frame("ind1"=c('','','','',''),
                        "ind2"=c('','','','',''),
                        "ind3"=c('','','','',''))
  #type moet voor dit dataframe op character worden gezet
  df_indi <- data.frame(lapply(df_indi, as.character), stringsAsFactors=FALSE)
  df_data_plus_indi <- cbind(df_data,df_indi)

  # verzamel data uit definitieve maanden
  # als mnd1 201809, dan def_maand_min 201709 (zelfde maand jaar eerder)
  def_maand_min <- paste0(as.character(as.integer(substr(jrmnd$mnd1,1,4))-1),
                          substr(jrmnd$mnd1,5,6))
  sqlstr <- paste0("select * from ", schema, ".tbl_microbase_verd_def ",
                   "where vsmd < ", jrmnd$mnd1, " and vsmd >= ", def_maand_min)
  data_def <- dat_lees_db(sqlstr)
  
  return(list(data=df_data_plus_indi, data_def=data_def))
}


#' Selecteer de gewenste rijen en kolommen in de juiste volgorde. Extra controle
#' om ervoor te zorgen dat alle kolommen aanwezig zijn voor het maken van de
#' ctcr-sheet.
#'
#' @param df dataframe
#' @return df dataframe
selecteer_frame_verd <- function(df) {
  df <- df[c("Stieren", "Koeien", "Vaarzen", "Kalveren 0-8 mnd", "Kalveren 8-12 mnd"),
           c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3",
             "hvm_mnd1", "hvm_mnd2", "hvm_mnd3", "hvm_mnd4",
             "ind1", "ind2", "ind3")]
  return(df)
}


#' Maak het controle- en correctiebestand voor de aantallenverdelingen.
#'
#' @param inputbestand string met volledig pad naar inputbestand
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param maanden_vorige_vm list met verslagmaanden horend bij vorige
#'                          verwerkingsmaand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param outputbestand string met volledig pad naar outputbestand
#' @return txt. string met tekst die naar het scherm wordt geschreven
maak_ctcr_bst_verd <- function(inputbestand, jrmnd, maanden_vorige_vm,
                                maanden_huidige_vm, outputbestand){
  logdebug(msg=paste("Systeem start maken ctcr-bestand aantallenverdeling",
                    outputbestand))

  withProgress(message="", value=0, {
    mld <- "Verzamelen data"
    setProgress(message=mld, value=0.25)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    res <- verzamel_data_ctcr_verd(jrmnd=jrmnd, levering_bst=inputbestand)
    df_data     <- res$data
    df_data_def <- res$data_def

    mld <- "Afleiden indicatoren"
    setProgress(message=mld, value=0.50)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data <- afl_leid_af_indicatoren_verd(data=df_data, data_def=df_data_def)

    mld <- "Wegschrijven naar Excel"
    setProgress(message=mld, value=0.75)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data <- selecteer_frame_verd(df=df_data)
    stel_samen_ctcr_verd(maanden_vorige_vm=maanden_vorige_vm,
                         maanden_huidige_vm=maanden_huidige_vm,
                         data=df_data, bestand=outputbestand)

    setProgress(message="Gereed", value=1.0)
  })

  # verifieer of outputbestand gemaakt is
  if (file.exists(outputbestand)) {
    txt <- paste("bestand", basename(outputbestand), "is gemaakt")
  } else {
    txt <- paste("bestand", basename(outputbestand), "is niet gemaakt")
  }

  logdebug(msg="Systeem is gereed met maken ctcr-bestand aantallenverdeling")

  return(txt=txt)
}
