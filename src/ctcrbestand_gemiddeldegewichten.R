#' Naam        : ctcrbestand_gemiddeldegewichten.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om het controle- en correctiebestand voor gemiddelde
#'               gewichten te maken. Met dit bestand kan geanalyseerd worden of
#'               de gemiddelde gewichten plausibel zijn.


#' Maak het ctcr-bestand voor de gemiddelde gewichten op.
#'
#' @param wb Workbook object containing a worksheet.
#' @return wb Workbook object containing a worksheet.
maak_op_gemg <- function (wb) {
    # eerste rij: tekst dikgedrukt en gecentreerd en merge cellen per groep
    addStyle(wb, sheet=1, style=createStyle(textDecoration='bold', halign='center'),
             rows=1, cols=1:11, gridExpand=TRUE, stack=TRUE)
    mergeCells(wb, sheet=1, cols=2:4, rows=1)
    mergeCells(wb, sheet=1, cols=5:8, rows=1)
    mergeCells(wb, sheet=1, cols=9:11, rows=1)

    # tweede rij: tekst dikgedrukt
    addStyle(wb, sheet=1, style=createStyle(textDecoration='bold'),
             rows=2, cols=1:11, gridExpand=TRUE, stack=TRUE)

    # cellen vorige verwerkingsmaand: achtergrondkleur kleur_vorige_vwmd
    addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_vorige_vwmd),
             rows=2:22, cols=2:4, gridExpand=TRUE, stack=TRUE)

    # cellen nieuwe maand: achtergrondkleur kleur_huidige_vwmd
    addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_huidige_vwmd),
             rows=2:22, cols=8, gridExpand=TRUE, stack=TRUE)

    # cellen witvlees, eenhoevigen en geiten: tekst schuingedrukt want constant
    addStyle(wb, sheet=1, style=createStyle(textDecoration='italic'),
             rows=11:22, cols=2:8, gridExpand=TRUE, stack=TRUE)

    # cellen indicatoren: tekst gecentreerd
    addStyle(wb, sheet=1, style=createStyle(halign='center'),
             rows=2:22, cols=9:11, gridExpand=TRUE, stack=TRUE)

    # lijn onder regel varkens
    addStyle(wb, sheet=1, style=createStyle(border='bottom'),
             rows=10, cols=1:11, gridExpand=TRUE, stack=TRUE)

    # lijn rechts van diersoort
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:22, cols=1, gridExpand=TRUE, stack=TRUE)

    # lijn tussen vorige verwerkingsmaand en huidige verwerkingsmaand
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:22, cols=4, gridExpand=TRUE, stack=TRUE)

    # lijn tussen huidige verwerkingsmaand en indicatoren
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:22, cols=8, gridExpand=TRUE, stack=TRUE)

    # stel kolombreedten in
    setColWidths(wb, sheet=1, cols=1, widths=App$breedte_diersoortcat) # kolom diersoort
    setColWidths(wb, sheet=1, cols=9:11, widths=App$breedte_ind) # alle kolommen indicatoren

    # stel weergave numerieke waarden in: toon afgeronde waarden zonder decimalen voor roodvlees
    addStyle(wb, sheet=1, style=createStyle(numFmt="0"),
             rows=3:12, cols=2:8, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, sheet=1, style=createStyle(numFmt="0.0"),
             rows=13:22, cols=2:8, gridExpand=TRUE, stack=TRUE)

    return(wb)
}


#' Stel het controle- en correctiebestand voor de gemiddelde gewichten samen.
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
stel_samen_ctcr_gemg <- function(maanden_vorige_vm, maanden_huidige_vm,
                                 data, bestand) {
  wb <- createWorkbook()
  addWorksheet(wb, "gemiddelde_gewichten")

  # stel headers samen (rij 1 en 2 en kolom 1)
  groepkoppen <- t(c('', 'Vorige verwerkingsmaand', '', '',
                     'Huidige verwerkingsmaand', '', '', '',
                     'Indicatoren', '', ''))
  kolomkoppen <- t(c('Diersoort', maanden_vorige_vm, maanden_huidige_vm,
                     'Gewicht', 'Ondergrens', 'Bovengrens'))
  rijkoppen <- c('Stieren', 'Koeien', 'Vaarzen',
                 'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd',
                 'Lammeren', 'Volwassen schapen',
                 'Varkens', 'Eenhoevige dieren', 'Geiten', 'Vleeskuikens',
                 'Overige kippen', 'Eenden', 'Duiven', 'Fazanten', 'Ganzen',
                 'Kalkoenen', 'Parelhoenders', 'Patrijzen', 'Struisvogels')
  gemiddelde_gewichten <- data.frame(rijkoppen, data)

  # schrijf data weg met buitenrand
  writeData(wb, sheet=1, x=groepkoppen, startRow=1, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=kolomkoppen, startRow=2, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=gemiddelde_gewichten, startRow=3, colNames=F, borders=c("surrounding"))

  wb <- maak_op_gemg(wb)
  saveWorkbook(wb, file=bestand, overwrite=TRUE)
}


#' Verzamel de gegevens uit de levering en de historische gegevens en
#' stel op basis hiervan een dataframe samen met ongevulde indicatoren.
#'
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param levering_bst string met volledig pad naar inputbestand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#'
#' @return data_plus_indi dataframe met data wat op sheet kan worden gezet
#' @example.
#' > verzamel_data_ctcr_verd(jrmnd, levering_bst, maanden_huidige_vm)
#'      dcat vvm_mnd1 vvm_mnd2 vvm_mnd3 201712 201801 201802 201803 indi1 indi2 indi3
#'  Stieren     <NA>     <NA>     <NA>    500
#'  Koeien      <NA>     <NA>     <NA>    400
#'  Vaarzen     <NA>     <NA>     <NA>    350
verzamel_data_ctcr_gemg <- function(jrmnd, gemiddelde_gewichten_bst,
                                    kalveren_bst, kalveren_vj_bst){

  kolomkoppen_hmnd <- c("hvm_mnd1", "hvm_mnd2", "hvm_mnd3", "hvm_mnd4")

  # haal levering op
  df_levering <- inl_lees_input_verd_gemg(inputbestand=gemiddelde_gewichten_bst,
                                          jrmnd=jrmnd)$df_gemg

  # lees de waarden voor de kalveren in en voeg toe
  df_kalveren <- inl_lees_input_gemg_kalv(inputbestand=kalveren_bst,
                                    inputbestand_vj=kalveren_vj_bst,
                                    jrmnd=jrmnd)
  df_levering["Kalveren 0-8 mnd",] <- df_kalveren

  # voeg constanten toe
  eenhoevigen	 <- App$eenhoevigen
  geiten	     <- App$geiten
  vleeskuikens   <- App$vleeskuikens
  overige_kippen <- App$overige_kippen
  eenden	     <- App$eenden
  duiven 	     <- App$duiven
  fazanten	     <- App$fazanten
  ganzen	     <- App$ganzen
  kalkoenen   	 <- App$kalkoenen
  parelhoenders	 <- App$parelhoenders
  patrijzen	     <- App$patrijzen
  struisvogels   <- App$struisvogels

  const_cols <- c(eenhoevigen, geiten, vleeskuikens, overige_kippen, eenden,
                  duiven, fazanten, ganzen, kalkoenen, parelhoenders, patrijzen,
                  struisvogels)
  df_constanten  <- data.frame(const_cols, const_cols, const_cols, const_cols)
  colnames(df_constanten) <- kolomkoppen_hmnd
  df_constanten$dcat <- c("Eenhoevige dieren","Geiten","Vleeskuikens",
                          "Overige kippen","Eenden","Duiven","Fazanten","Ganzen",
                          "Kalkoenen","Parelhoenders","Patrijzen","Struisvogels")
  df_levering <- rbind(df_levering, df_constanten)

  # haal gegevens van vorige verwerkingsmaand op uit de database
  df_hist_data <- inl_lees_hist_gemg(jrmnd=jrmnd)

  # voeg gegevens samen
  df_data <- merge(df_levering, df_hist_data, by=c("dcat"), all=TRUE, sort=FALSE)

  # initialiseer indicatoren en voeg toe
  df_indi <- data.frame("ind1"=rep("", nrow(df_data)),
                        "ind2"=rep("", nrow(df_data)),
                        "ind3"=rep("", nrow(df_data)))
  #type moet voor dit dataframe op character worden gezet
  df_indi <- data.frame(lapply(df_indi, as.character), stringsAsFactors=FALSE)
  df_data_plus_indi <- cbind(df_data,df_indi)
  rownames(df_data_plus_indi) <- df_data_plus_indi[,"dcat"]

  return(df_data_plus_indi)
}


#' Selecteer de gewenste rijen en kolommen in de juiste volgorde. Extra controle
#' om ervoor te zorgen dat alle kolommen aanwezig zijn voor het maken van de
#' ctcr-sheet.
#'
#' @param df dataframe
#' @param vsdm list met verslagmaanden horend bij verwerkingsmaand
#' @return df dataframe
selecteer_frame_gemg <- function(df) {
  df <- df[c("Stieren","Koeien","Vaarzen","Kalveren 0-8 mnd","Kalveren 8-12 mnd",
             "Lammeren","Volwassen schapen","Varkens","Eenhoevige dieren",
             "Geiten","Vleeskuikens","Overige kippen","Eenden","Duiven",
             "Fazanten","Ganzen","Kalkoenen","Parelhoenders","Patrijzen",
             "Struisvogels"),
           c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3",
             "hvm_mnd1", "hvm_mnd2", "hvm_mnd3", "hvm_mnd4",
             "ind1", "ind2", "ind3")]
  return(df)
}


#' Maak het analysebestand voor de gemiddelde gewichten.
#'
#' @param inputbestand1 string met volledig pad naar inputbestand met gemiddelde
#'                      gewichten roodvlees
#' @param inputbestand2 string met volledig pad naar inputbestand met geniddelde
#'                      gewichten kalveren 0-8 mnd
#' @param inputbestand3 string met volledig pad naar inputbestand met geniddelde
#'                      gewichten kalveren 0-8 mnd vorig jaar
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param maanden_vorige_vm list met verslagmaanden horend bij vorige
#'                          verwerkingsmaand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param outputbestand string met volledig pad naar outputbestand
#' @return txt. string met tekst die naar het scherm wordt geschreven
maak_ctcr_bst_gemg <- function(inputbestand1, inputbestand2, inputbestand3,
                               jrmnd, maanden_vorige_vm, maanden_huidige_vm,
                               outputbestand) {

  logdebug(msg=paste("Systeem start maken ctcr-bestand gemiddelde gewichten",
                     outputbestand))

  withProgress(message="", value=0, {
    mld <- "Verzamelen data"
    setProgress(message=mld, value=0.25)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data <- verzamel_data_ctcr_gemg(jrmnd=jrmnd,
                                       gemiddelde_gewichten_bst=inputbestand1,
                                       kalveren_bst=inputbestand2,
                                       kalveren_vj_bst=inputbestand3)

    mld <- "Afleiden indicatoren"
    setProgress(message=mld, value=0.50)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data <- afl_leid_af_indicatoren_gemg(df_data)

    mld <- "Wegschrijven naar Excel"
    setProgress(message=mld, value=0.75)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data <- selecteer_frame_gemg(df=df_data)
    stel_samen_ctcr_gemg(maanden_vorige_vm=maanden_vorige_vm,
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

  logdebug(msg="Systeem is gereed met maken ctcr-bestand gemiddelde gewichten")
  return(txt)
}
