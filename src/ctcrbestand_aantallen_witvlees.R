#' Naam        : ctcrbestand_aantallen_witvlees.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om het controle- en correctiebestand voor aantallen
#'               witvlees te maken. Met dit bestand kan geanalyseerd worden of
#'               de aantallen witvlees plausibel zijn.
#' NB.         : Dit script gebruikt functies uit ctcr_aantallen_roodvlees.R:
#'                 - maak_op_aant_rdvl_wtvl()
#'                 - stel_samen_ctcr_rdvl_wtvl()
#'                 - selecteer_frame_wtvl()
#'                 - bepaal_bijschattingen_vorige_lev_rdvl_wtvl()


#' Verzamel alle benodigde gegevens
#'
#' Verzamel de gegevens uit de levering en de gave historische gegevens en
#' stel op basis hiervan een dataframe samen met ongevulde indicatoren in de
#' vorm van een draaitabel zodat deze in het ctcr-bestand kan worden geplaatst.
#' Verzamel ook de waarden van de 12 definitieve maanden voor de bijschattingen
#' en de indicatoren. Verzamel ten slotte ook de data van de levering en de data
#' van de levering van de vorige verwerkingsmaand voor het afleiden van de
#' indicatoren.
#'
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param levering_bst string met volledig pad naar inputbestand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param schema string met databaseschema
#' @param tabel string met microbasetabelnaam
#' @param tabel_def string met microbasetabelnaam voor definitieve cijfers
#' @return data dataframe met data wat op sheet kan worden gezet
#' @return data_def dataframe met data van 12 definitieve maanden
#' @return data_lev dataframe met data van levering
#' @return data_vorige_lev dataframe met data van vorige levering
#' @return bijschatting_vorige_lev dataframe met bijschattingen vorige levering
#' @example.
#' > verzamel_data_ctcr_wtvl(jrmnd, levering_bst, maanden_huidige_vm)$data
#'     slnm    wrkp     dsrt vvm_mnd1 vvm_mnd2 vvm_mnd3 201712 201801 201802 201803 indi1 indi2 indi3
#' BEDRIJFSNAAM 1646092   Geiten     <NA>     <NA>     <NA>      6
#' BEDRIJFSNAAM 1646092 Kalveren     <NA>     <NA>     <NA>      9
#' BEDRIJFSNAAM 1646092 Lammeren     <NA>     <NA>     <NA>    112
verzamel_data_ctcr_wtvl <- function(jrmnd, levering_bst, maanden_huidige_vm,
                                    schema=App$databaseschema,
                                    tabel=App$tbl_microbase_wtvl,
                                    tabel_def=App$tbl_microbase_wtvl_def) {
  # haal levering op en zet om in draaitabel
  df_lev_plat <- inl_lees_input_wtvl(inputbestand=levering_bst, jrmnd=jrmnd)
  df_lev      <- inl_maak_draaitabel(df=df_lev_plat, vsmd=maanden_huidige_vm)
  df_lev      <- subset(df_lev, select=-c(vwmd)) # verwijder kolom vwmd

  # haal de historische data op uit de database
  df_hist     <- inl_lees_hist_aant(jrmnd=jrmnd, tabel=App$tbl_microbase_wtvl)

  # voeg historische data en levering samen
  data <- merge(df_hist, df_lev, by=c("slnm", "wrkp", "dsrt"), all=TRUE)
  
  # een extra frame wordt toegevoegd met def cijfers vanaf 14 maanden voor verwerkingsmaand
  # dit frame is nodig om te voorkomen dat er teveel lege regels worden verwijderd
  #df_def_max <- inl_lees_hist_max_aant_eerste_mnd_tot_4_mnd_terug_max(jrmnd, tabel_def)
  #data <- merge(data, df_def_max, by=c("slnm", "wrkp", "dsrt"), all=TRUE)
  #data <- data[rowSums(is.na(data)) != ncol(data), ] # rijen niet als alleen NA's
  # de toegevoegde kolom wordt weer verwijderd
  #data <- subset(data, select = -max_aantal)
  
  data[, 4:10] <- sapply(data[, 4:10], as.numeric) # maak van de waarden numerics

  # voeg initialisatie indicatoren toe
  indi <- data.frame("ind1"=rep("", nrow(data)),
                     "ind2"=rep("", nrow(data)),
                     "ind3"=rep("", nrow(data)))
  # type moet voor dit dataframe op character worden gezet
  indi <- data.frame(lapply(indi, as.character), stringsAsFactors=FALSE)
  data_plus_indi <- cbind(data,indi)

  # verzamel data uit definitieve maanden
  # als mnd1 201809, dan def_maand_min 201709 (zelfde maand jaar eerder)
  def_maand_min <- paste0(as.character(as.integer(substr(jrmnd$mnd1,1,4))-1),
                          substr(jrmnd$mnd1,5,6))
  sqlstr <- paste0("select * from ", schema, ".", tabel_def,
                   " where vsmd < ", jrmnd$mnd1, " and vsmd >= ", def_maand_min)
  data_def <- dat_lees_db(sqlstr)

  # verzamel data van vorige levering
  sqlstr <- paste0("select * from ", schema, ".", tabel,
                   " where vwmd = ", jrmnd$vvwmd)
  data_vorige_lev <- dat_lees_db(sqlstr)

  # bepaal bijschattingen vorige levering
  df_bijschatting_vorige_lev <- bepaal_bijschattingen_vorige_lev_rdvl_wtvl(df=data_vorige_lev)

  return(list(data=data_plus_indi,
              data_def=data_def,
              data_lev=df_lev_plat,
              data_vorige_lev=data_vorige_lev,
              bijschatting_vorige_lev=df_bijschatting_vorige_lev))
}


#' Maak het controle- en correctiebestand voor de aantallen witvlees.
#'
#' @param inputbestand string met volledig pad naar inputbestand
#' @param jrmnd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param maanden_vorige_vm list met verslagmaanden horend bij vorige
#'                          verwerkingsmaand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param outputbestand string met volledig pad naar outputbestand
#' @return txt string met tekst die naar het scherm wordt geschreven
maak_ctcr_aant_wtvl <- function(inputbestand, jrmnd, maanden_vorige_vm,
                                maanden_huidige_vm, outputbestand) {
  logdebug(msg=paste("Systeem start maken ctcr-bestand aantallen witvlees",
                      outputbestand))

  withProgress(message="", value=0, {
      mld <- "Verzamelen data"
      setProgress(message=mld, value=0.2)
      logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
      res <- verzamel_data_ctcr_wtvl(jrmnd=jrmnd, levering_bst=inputbestand,
                                      maanden_huidige_vm=maanden_huidige_vm)
      data                    <- res$data
      data_def                <- res$data_def # data laatste 12 definitieven maanden
      data_lev                <- res$data_lev # data levering
      data_vorige_lev         <- res$data_vorige_lev # data vorige levering
      bijschatting_vorige_lev <- res$bijschatting_vorige_lev

      mld <- "Toevoegen bijschattingen"
      setProgress(message=mld, value=0.4)
      logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
      data_bij <- bij_voeg_bijschattingen_toe(df=data, df_def=data_def,
                                              maanden_huidige_vm=maanden_huidige_vm)
      data         <- data_bij$df
      bijschatting <- data_bij$df_bijschatting

      mld <- "Afleiden indicatoren"
      setProgress(message=mld, value=0.6)
      logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
      data <- afl_leid_af_indicatoren_rdvl_wtvl(data=data,
                                                data_def=data_def,
                                                data_lev=data_lev,
                                                data_vorige_lev=data_vorige_lev,
                                                jrmnd=jrmnd)

      mld <- "Toevoegen regels voor incidenteel slachten"
      setProgress(message=mld, value=0.7)
      # een extra kolom wordt toegevoegd met het maximum van de definitieve cijfers 
      # vanaf 14 maanden voor de verwerkingsmaand 
      # dit is nodig om informatie te tonen van bedrijven die incidenteel een bepaald 
      # diertype slachten
      wtvl_def=App$tbl_microbase_wtvl_def
      df_def_max <- inl_lees_hist_max_aant_x_maand_terug(jrmnd, wtvl_def)
      data <- merge(data, df_def_max, by=c("slnm", "wrkp", "dsrt"), all=TRUE)
      data <- data[rowSums(is.na(data)) != ncol(data), ] # rijen niet als alleen NA's
      # de toegevoegde kolom wordt weer verwijderd
      data <- subset(data, select = -max_aantal)

      mld <- "Wegschrijven naar Excel"
      setProgress(message=mld, value=0.8)
      logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
      data <- selecteer_frame_wtvl(df=data, vsmd=maanden_huidige_vm)
      stel_samen_ctcr_wtvl(maanden_vorige_vm=maanden_vorige_vm,
                           maanden_huidige_vm=maanden_huidige_vm,
                           data=data, bijschatting=bijschatting,
                           bijschatting_vorige_lev=bijschatting_vorige_lev,
                           bestand=outputbestand)

      setProgress(message="Gereed", value=1.0)
  })

  # verifieer of outputbestand gemaakt is
  if (file.exists(outputbestand)) {
    txt <- paste("bestand", basename(outputbestand), "is gemaakt")
  } else {
    txt <- paste("bestand", basename(outputbestand), "is niet gemaakt")
  }

  logdebug(msg="Systeem is gereed met maken ctcr-bestand aantallen witvlees")

  return(txt=txt) # outputtekst naar het scherm
}

#' Stel het controle- en correctiebestand voor de aantallen samen.
#'
#' We schrijven de groepkoppen, de kolomkoppen en de data in 1x weg (inclusief
#' de randen eromheen). Vervolgens voegen we de overige opmaak toe. Ten slotte
#' slaan we het Excelbestand op.
#'
#' @param maanden_vorige_vm list met verslagmaanden horend bij vorige
#'                          verwerkingsmaand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param data dataframe met data om weg te schrijven naar excelsheet
#' @param bijschatting dataframe met de bijschattingen
#' @param bijschatting_vorige_lev dataframe met bijschattingen vorige levering
#' @param bestand string met volledig pad naar outputbestand
#' @return niets
stel_samen_ctcr_wtvl <- function(maanden_vorige_vm, maanden_huidige_vm,
                                 data, bijschatting,
                                 bijschatting_vorige_lev, bestand) {
  wb <- createWorkbook()
  addWorksheet(wb, "aantallen")
  freezePane(wb, "aantallen",  firstActiveRow=3)
  
  # stel headers samen (rij 1 en 2)
  groepkoppen <- t(c('', '', '', 'Vorige verwerkingsmaand', '', '',
                     'Huidige verwerkingsmaand', '', '', '',
                     'Indicatoren', '', ''))
  kolomkoppen <- t(c('Naam slachtbedrijf', 'Werkpleknr', 'Diersoort',
                     maanden_vorige_vm, maanden_huidige_vm,
                     'Slachtbedrijf', 'Aantal', 'Gemiddelde'))
  
  # schrijf data weg met buitenrand
  writeData(wb, sheet=1, x=groepkoppen, startRow=1, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=kolomkoppen, startRow=2, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=data, startRow=3, colNames=F, borders=c("surrounding"))
  
  wb <- maak_op_aant_wtvl(wb, data, bijschatting, bijschatting_vorige_lev,
                               maanden_huidige_vm)
  
  saveWorkbook(wb, file=bestand, overwrite=TRUE)
}

#' Maak het ctcrbestand voor de aantallen op.
#'
#' @param wb workbook object containing a worksheet
#' @param data dataframe met data
#' @param bijschatting dataframe met bijschattingen om bijschatting te markeren
#' @param bijschatting_vorige_lev dataframe met bijschattingen vorige levering
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @return wb workbook object containing a worksheet
maak_op_aant_wtvl <- function(wb, data, bijschatting,
                                   bijschatting_vorige_lev, maanden_huidige_vm){
  laatste_rij <- nrow(data)+2
  
  # eerste rij: tekst dikgedrukt en gecentreerd en merge cellen per groep
  addStyle(wb, sheet=1, style=createStyle(textDecoration='bold', halign='center'),
           rows=1, cols=1:11, gridExpand=TRUE, stack=TRUE)
  mergeCells(wb, sheet=1, cols=4:6, rows=1)
  mergeCells(wb, sheet=1, cols=7:10, rows=1)
  mergeCells(wb, sheet=1, cols=11:13, rows=1)
  
  # tweede rij: tekst dikgedrukt
  addStyle(wb, sheet=1, style=createStyle(textDecoration='bold'),
           rows=2, cols=1:13, gridExpand=TRUE, stack=TRUE)
  
  # cellen vorige verwerkingsmaand: achtergrondkleur lichtgrijs
  addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_vorige_vwmd),
           rows=2:laatste_rij, cols=4:6, gridExpand=TRUE, stack=TRUE)
  
  # cellen nieuwe maand: achtergrondkleur lichtgeel
  addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_huidige_vwmd),
           rows=2:laatste_rij, cols=10, gridExpand=TRUE, stack=TRUE)
  
  # cellen indicatoren: tekst gecentreerd; alleen eerste 2 indicatoren
  addStyle(wb, sheet=1, style=createStyle(halign='center'),
           rows=2:laatste_rij, cols=11:12, gridExpand=TRUE, stack=TRUE)
  
  # verticale lijn rechts van diersoort
  addStyle(wb, sheet=1, style=createStyle(border='right'),
           rows=1:laatste_rij, cols=3, gridExpand=TRUE, stack=TRUE)
  
  # verticale lijn tussen vorige verwerkingsmaand en huidige verwerkingsmaand
  addStyle(wb, sheet=1, style=createStyle(border='right'),
           rows=1:laatste_rij, cols=6, gridExpand=TRUE, stack=TRUE)
  
  # verticale lijn tussen huidige verwerkingsmaand en indicatoren
  addStyle(wb, sheet=1, style=createStyle(border='right'),
           rows=1:laatste_rij, cols=10, gridExpand=TRUE, stack=TRUE)
  
  # horizontale lijn tussen slachtbedrijven (als wrkp verandert)
  for (i in 1:(length(data$wrkp)-1)){
    if (data$wrkp[i] != data$wrkp[i+1]) {
      addStyle(wb, sheet=1, style=createStyle(border='bottom'),
               rows=i+2, cols=1:13, gridExpand=TRUE, stack=TRUE)
    }          # rows=i+2 omdat data op rij 3 begint
  }
  
  # maak bijschattingen rood van vorige levering
  if (nrow(bijschatting_vorige_lev) > 0) {
    for (b in 1:nrow(bijschatting_vorige_lev)) {
      bs <- bijschatting_vorige_lev[b, ]
      i <- 2 + which(data$slnm == bs$slnm &
                       data$wrkp == bs$wrkp &
                       data$dsrt == bs$dsrt,
                     arr.ind=TRUE) # rijnr van bijschatting
      j <- 3 + which(maanden_huidige_vm == bs$vsmd, arr.ind=TRUE)# kolomnr van bijschatting
      addStyle(wb, sheet=1, style=createStyle(fontColour=App$kleur_bijschatting),
               rows=i, cols=j, gridExpand=TRUE, stack=TRUE)
    }
  }
  
  # maak bijschattingen rood van huidige levering
  if (nrow(bijschatting) > 0) {
    for (b in 1:nrow(bijschatting)) {
      bs <- bijschatting[b, ]
      i <- 2 + which(data$slnm == bs$slnm &
                       data$wrkp == bs$wrkp &
                       data$dsrt == bs$dsrt,
                     arr.ind=TRUE) # rijnr van bijschatting
      j <- 6 + which(maanden_huidige_vm == bs$vsmd, arr.ind=TRUE)# kolomnr van bijschatting
      addStyle(wb, sheet=1, style=createStyle(fontColour=App$kleur_bijschatting),
               rows=i, cols=j, gridExpand=TRUE, stack=TRUE)
    }
  }
  
  # stel kolombreedten in
  setColWidths(wb, sheet=1, cols=1, widths=App$breedte_slachthuis) # kolom slachthuis naam
  setColWidths(wb, sheet=1, cols=2, widths=App$breedte_werkplek) # kolom werkplek
  setColWidths(wb, sheet=1, cols=3, widths=App$breedte_diersoort) # kolom diersoort
  setColWidths(wb, sheet=1, cols=11:13, widths=App$breedte_ind) # alle kolommen indicatoren
  
  # stel weergave numerieke waarden in: toon afgeronde waarden zonder decimalen
  addStyle(wb, 1, style=createStyle(numFmt="#,##0"),
           rows=3:laatste_rij, cols=4:10, gridExpand=TRUE, stack=TRUE)
  addStyle(wb, 1, style=createStyle(numFmt="#,##0"),
           rows=3:laatste_rij, cols=13, gridExpand=TRUE, stack=TRUE)
  
  return(wb)
}
