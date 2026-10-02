#' Naam        : analysebestand_bijschattingen.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om het analysebestand voor bijschattingen te maken.
#'               Met dit bestand kan geanalyseerd worden of de bijschattingen
#'               van de laatste maanden plausibel zijn.


#' Maak het analysebestand voor de bijschattingen op.
#'
#' @param wb Workbook object containing a worksheet.
#' @return wb Workbook object containing a worksheet.
maak_op_pcbs <- function (wb) {
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
             rows=2:17, cols=2:4, gridExpand=TRUE, stack=TRUE)

    # cellen nieuwe maand: achtergrondkleur kleur_huidige_vwmd
    addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_huidige_vwmd),
             rows=2:17, cols=8, gridExpand=TRUE, stack=TRUE)

    # cellen indicatoren: tekst gecentreerd
    addStyle(wb, sheet=1, style=createStyle(halign='center'),
             rows=2:17, cols=9:11, gridExpand=TRUE, stack=TRUE)

    # lijn onder regel geiten
    addStyle(wb, sheet=1, style=createStyle(border='bottom'),
             rows=7, cols=1:11, gridExpand=TRUE, stack=TRUE)

    # lijn rechts van diersoort
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:17, cols=1, gridExpand=TRUE, stack=TRUE)

    # lijn tussen vorige verwerkingsmaand en huidige verwerkingsmaand
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:17, cols=4, gridExpand=TRUE, stack=TRUE)

    # lijn tussen huidige verwerkingsmaand en indicatoren
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:17, cols=8, gridExpand=TRUE, stack=TRUE)

    # stel kolombreedten in
    setColWidths(wb, sheet=1, cols=1, widths=App$breedte_diersoortcat) # kolom diersoort
    setColWidths(wb, sheet=1, cols=9:11, widths=App$breedte_ind) # alle kolommen indicatoren

    # stel weergave numerieke waarden in: geef 1 decimaal
    addStyle(wb, sheet=1, style=createStyle(numFmt="0.0"),
             rows=3:17, cols=2:8, gridExpand=TRUE, stack=TRUE)

    return(wb)
}


#' Stel het analysebestand voor de bijschattingen samen.
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
stel_samen_ana_bijschattingen <- function(data, jrmnd, maanden_vorige_vm, maanden_huidige_vm, bestand) {
  wb <- createWorkbook()
  addWorksheet(wb, "bijschattingen")
  #freezePane(wb, "bijschattingen" ,  firstActiveRow = 3,  firstActiveCol = 2)

  # stel headers samen (rij 1 en kolom 1)
  # stel headers samen (rij 1 en 2 en kolom 1)
  groepkoppen <- t(c('', 'Vorige verwerkingsmaand', '', '',
                     'Huidige verwerkingsmaand', '', '', '',
                     'Indicatoren', '', ''))
  kolomkoppen <- t(c('Diersoort', maanden_vorige_vm, maanden_huidige_vm,
                     'N.v.t.', 'N.v.t.', 'N.v.t.'))
  rijkoppen <- c('Lammeren', 'Volwassen schapen',
                 'Varkens', 'Eenhoevige dieren', 'Geiten', 'Vleeskuikens',
                 'Overige kippen', 'Eenden', 'Duiven', 'Fazanten', 'Ganzen',
                 'Kalkoenen', 'Parelhoenders', 'Patrijzen', 'Struisvogels')
  ana_bijschattingen <- data.frame(data)

  # schrijf data weg met buitenrand
  writeData(wb, sheet=1, x=groepkoppen, startRow=1, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=kolomkoppen, startRow=2, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=ana_bijschattingen, startRow=3, colNames=F, borders=c("surrounding"))

  wb <- maak_op_pcbs(wb)
  saveWorkbook(wb, file=bestand, overwrite=TRUE)
}


#' Verzamel de definitieve en voorlopige gegevens uit de database
#' stel op basis hiervan een dataframe samen
#'
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#'
#' @return df_tot dataframe met data die op sheet kunnen worden gezet
#' @example.
#' > verzamel_data_ana_tijdreeksen(jrmnd)
#'    dcat   jr      januari     februari maart april mei juni juli augustus september  ...   december
#' Stieren 2013           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...         NA
#' Stieren 2014           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...         NA
#' Stieren 2015           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...         NA
#' Stieren 2016           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...   5263.227
#' Stieren 2017     4734.408     5502.624    NA    NA  NA   NA   NA       NA        NA  ...         NA
verzamel_data_ana_bijschattingen <- function(jrmnd){

  # haal gegevens op uit de database
  df_data <- inl_lees_bijschattingen(jrmnd)

  # initialiseer indicatoren en voeg toe
  df_indi <- data.frame("ind1"=rep("", nrow(df_data)),
                        "ind2"=rep("", nrow(df_data)),
                        "ind3"=rep("", nrow(df_data)))
  #type moet voor dit dataframe op character worden gezet
  df_indi <- data.frame(lapply(df_indi, as.character), stringsAsFactors=FALSE)
  
  df_data_plus_indi <- cbind(df_data,df_indi)
  rownames(df_data_plus_indi) <- df_data_plus_indi[,"dsrt"]
  
  df_data_plus_indi <- df_data_plus_indi[!df_data_plus_indi$dsrt %in% c("Volwassen runderen", "Kalveren"),]
  df_data_plus_indi[, colnames(df_data_plus_indi)[2:8]] <- sapply(df_data_plus_indi[, colnames(df_data_plus_indi)[2:8]], as.numeric)

  return(df_data_plus_indi)
}


#' Selecteer de gewenste rijen en kolommen in de juiste volgorde. Extra controle
#' om ervoor te zorgen dat alle kolommen aanwezig zijn voor het maken van de
#' ctcr-sheet.
#'
#'
#' @param df dataframe
#' @return df dataframe.
#'    dcat   jr      januari     februari maart april mei juni juli augustus september  ...   december
#' Stieren 2013           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...         NA
#' Stieren 2014           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...         NA
#' Stieren 2015           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...         NA
#' Stieren 2016           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...   5263.227
#' Stieren 2017     4734.408     5502.624    NA    NA  NA   NA   NA       NA        NA  ...         NA
#' ...
selecteer_frame_tijdreeksen <- function(df) {
  df <- df[, c("dcat", "jr", "januari","februari", "maart", "april", "mei",
               "juni", "juli", "augustus","september","oktober","november",
               "december")]
  return(df)
}


#' Maak het analysebestand voor de bijschattingen
#'
#' We schrijven de groepkoppen, de kolomkoppen en de data in 1x weg (inclusief
#' de randen eromheen). Vervolgens voegen we de overige opmaak toe. Ten slotte
#' slaan we het Excelbestand op.
#'
#' @param inputmap map string
#' @param jrmnd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param outputbestand string
#' @return txt string
#' @examples.
maak_ana_bijschattingen <- function(werkmap=App$werkmap, jrmnd, maanden_vorige_vm,
                                    maanden_huidige_vm, outputbestand) {

  logdebug(msg=paste("Systeem start maken analysebestand bijschattingen in",
                     outputbestand))

  withProgress(message="", value=0, {
    mld <- "Verzamelen data"
    setProgress(message=mld, value=0.25)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data <- verzamel_data_ana_bijschattingen(jrmnd=jrmnd)

    mld <- "Afleiden indicatoren"
    setProgress(message=mld, value=0.50)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))

    #df_data <- selecteer_frame_bijschattingen(df=df_data)

    mld <- "Wegschrijven naar Excel"
    setProgress(message=mld, value=0.75)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    stel_samen_ana_bijschattingen(data=df_data,
                                  jrmnd=jrmnd,
                                  maanden_vorige_vm=maanden_vorige_vm,
                                  maanden_huidige_vm=maanden_huidige_vm,
                                  bestand=outputbestand)

    setProgress(message="Gereed", value=1.0)
  })

  # verifieer of outputbestand gemaakt is
  if (file.exists(outputbestand)) {
    txt <- paste("bestand", basename(outputbestand), "is gemaakt")
  } else {
    txt <- paste("bestand", basename(outputbestand), "is niet gemaakt")
  }

  logdebug(msg="Systeem is gereed met maken analysebestand tijdreeksen")
  return(txt)
}
