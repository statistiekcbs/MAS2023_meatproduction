#' Naam        : ctcrbestand_aantallen_roodvlees.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om het controle- en correctiebestand voor aantallen
#'               roodvlees te maken. Met dit bestand kan geanalyseerd worden of
#'               de aantallen roodvlees plausibel zijn.
#' NB.         : Deze functies worden ook gebruikt in ctcr_aantallen_witvlees.R:
#'                 - maak_op_aant_rdvl_wtvl()
#'                 - stel_samen_ctcr_rdvl_wtvl()
#'                 - selecteer_frame_wtvl()
#'                 - bepaal_bijschattingen_vorige_lev_rdvl_wtvl()


#' Maak het ctcrbestand voor de aantallen op.
#'
#' NB. Deze functie wordt gebruikt voor roodvlees en voor witvlees.
#'
#' @param wb workbook object containing a worksheet
#' @param data dataframe met data
#' @param bijschatting dataframe met bijschattingen om bijschatting te markeren
#' @param bijschatting_vorige_lev dataframe met bijschattingen vorige levering
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @return wb workbook object containing a worksheet
maak_op_aant_rdvl <- function(wb, data, bijschatting,
                                   bijschatting_vorige_lev, maanden_huidige_vm){
    laatste_rij <- nrow(data)+2
    # eerste rij: tekst dikgedrukt en gecentreerd en merge cellen per groep
    addStyle(wb, sheet=1, style=createStyle(textDecoration='bold', halign='center'),
             rows=1, cols=1:15, gridExpand=TRUE, stack=TRUE)
    mergeCells(wb, sheet=1, cols=5:7, rows=1)
    mergeCells(wb, sheet=1, cols=8:11, rows=1)
    mergeCells(wb, sheet=1, cols=13:15, rows=1)

    # tweede rij: tekst dikgedrukt
    addStyle(wb, sheet=1, style=createStyle(textDecoration='bold'),
             rows=2, cols=1:15, gridExpand=TRUE, stack=TRUE)

    # cellen vorige verwerkingsmaand: achtergrondkleur lichtgrijs
    addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_vorige_vwmd),
             rows=2:laatste_rij, cols=5:7, gridExpand=TRUE, stack=TRUE)

    # cellen nieuwe maand: achtergrondkleur lichtgeel
    addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_huidige_vwmd),
             rows=2:laatste_rij, cols=11, gridExpand=TRUE, stack=TRUE)

    # cellen indicatoren: tekst gecentreerd; alleen eerste 2 indicatoren
    addStyle(wb, sheet=1, style=createStyle(halign='center'),
             rows=2:laatste_rij, cols=13:14, gridExpand=TRUE, stack=TRUE)
    
    # cellen databron: tekst gecentreerd
    addStyle(wb, sheet=1, style=createStyle(halign='center'),
             rows=2:laatste_rij, cols=12, gridExpand=TRUE, stack=TRUE)

    # verticale lijn rechts van diersoort
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:laatste_rij, cols=3, gridExpand=TRUE, stack=TRUE)

    # verticale lijn tussen vorige verwerkingsmaand en huidige verwerkingsmaand
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:laatste_rij, cols=7, gridExpand=TRUE, stack=TRUE)

    # verticale lijn tussen huidige verwerkingsmaand en databron
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:laatste_rij, cols=11, gridExpand=TRUE, stack=TRUE)
    
    # verticale lijn tussen databron en indicatoren
    addStyle(wb, sheet=1, style=createStyle(border='right'),
             rows=1:laatste_rij, cols=12, gridExpand=TRUE, stack=TRUE)
    # browser()
    # horizontale lijn tussen slachtbedrijven (als wrkp verandert)
    # for (i in 1:(length(data$wrkp)-1)){
    #     if (data$wrkp[i] != data$wrkp[i+1]) {
    #         addStyle(wb, sheet=1, style=createStyle(border='bottom'),
    #                  rows=i+2, cols=1:115, gridExpand=TRUE, stack=TRUE)
    #     }          # rows=i+2 omdat data op rij 3 begint
    # }
    
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
    setColWidths(wb, sheet=1, cols=13:15, widths=App$breedte_ind) # alle kolommen indicatoren

    # stel weergave numerieke waarden in: toon afgeronde waarden zonder decimalen
    addStyle(wb, 1, style=createStyle(numFmt="#,##0"),
             rows=3:laatste_rij, cols=5:11, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, 1, style=createStyle(numFmt="#,##0"),
             rows=3:laatste_rij, cols=15, gridExpand=TRUE, stack=TRUE)
    
    return(wb)
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
stel_samen_ctcr_rdvl <- function(maanden_vorige_vm, maanden_huidige_vm,
                                      data, bijschatting,
                                      bijschatting_vorige_lev, bestand) {
  wb <- createWorkbook()
  addWorksheet(wb, "aantallen")
  freezePane(wb, "aantallen",  firstActiveRow=3)
  
  # stel headers samen (rij 1 en 2)
  groepkoppen <- t(c('', '', '', '', 'Vorige verwerkingsmaand', '', '',
                                     'Huidige verwerkingsmaand', '', '', '' , '',
                                     'Indicatoren', '', ''))
  kolomkoppen <- t(c('Naam slachtbedrijf', 'Werkpleknr', 'Diersoort', 'Biologisch',
                     maanden_vorige_vm, maanden_huidige_vm, 'Databron',
                     'Slachtbedrijf', 'Aantal', 'Gemiddelde'))

  # schrijf data weg met buitenrand
  writeData(wb, sheet=1, x=groepkoppen, startRow=1, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=kolomkoppen, startRow=2, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=data, startRow=3, colNames=F, borders=c("surrounding"))

  wb <- maak_op_aant_rdvl(wb, data, bijschatting, bijschatting_vorige_lev,
                               maanden_huidige_vm)

  saveWorkbook(wb, file=bestand, overwrite=TRUE)
}


#' Leid af welke waarden in de vorige verwerkingsmaand zijn bijgeschat
#'
#' NB. Deze functie wordt gebruikt voor roodvlees en voor witvlees.
#'
#' @param df dataframe met
#' @return bijschatting_vorige_lev dataframe met bijschattingen vorige levering
#' @example
#' > bepaal_bijschattingen_vorige_lev_rdvl_wtvl(df)
#'                    slnm    wrkp               dsrt   vsmd aant
#' BEDRIJFSNAAM 0901354           Kalveren 201711 1500
#' BEDRIJFSNAAM 0901354 Volwassen runderen 201711 5000
#' BEDRIJFSNAAM 0901354           Kalveren 201712 1500
bepaal_bijschattingen_vorige_lev_rdvl_wtvl <- function (df) {
  # initaliseer dataframe om alle bijschattingen bij te houden
  df_bijschatting <- data.frame("slnm"=character(), "wrkp"=character(),
                                "dsrt"=character(), "vsmd"=character(),
                                "aant"=character(), stringsAsFactors=FALSE)

  for (i in 1:nrow(df)) {
    # bepaal aant_ruw afwijkt van aant_gaaf
    rij <- df[i, ]
    bijgeschat <- FALSE
    if (is.na(rij$aant_ruw) && !is.na(rij$aant_gaaf)) { # ruw leeg, gaaf gevuld
      bijgeschat <- TRUE
    } else if (!is.na(rij$aant_ruw) && !is.na(rij$aant_gaaf)) {
        if (rij$aant_ruw != rij$aant_gaaf) { # waarde ruw en gaaf verschilt
          bijgeschat <- TRUE
        }
    }

    if (bijgeschat) {
      rec <- c(rij$slnm, rij$wrkp, rij$dsrt, rij$vsmd, rij$aant_gaaf)
      # voeg toe aan df_bijschatting
      df_bijschatting <- rbind(df_bijschatting, rec, stringsAsFactors=FALSE)
      colnames(df_bijschatting) <- c("slnm", "wrkp", "dsrt",
                                     "vsmd", "aant") # weer goedzetten
    }
  }

  return (df_bijschatting)
}


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
#' > verzamel_data_ctcr_rdvl(jrmnd, levering_bst, maanden_huidige_vm)$data
#'     slnm    wrkp     dsrt vvm_mnd1 vvm_mnd2 vvm_mnd3 201712 201801 201802 201803 indi1 indi2 indi3
#' BEDRIJFSNAAM 1646092   Geiten     <NA>     <NA>     <NA>      6
#' BEDRIJFSNAAM 1646092 Kalveren     <NA>     <NA>     <NA>      9
#' BEDRIJFSNAAM 1646092 Lammeren     <NA>     <NA>     <NA>    112
verzamel_data_ctcr_rdvl <- function(jrmnd, levering_bst_nvwa, levering_bst_rvo,
                                    maanden_huidige_vm,
                                    schema=App$databaseschema,
                                    tabel=App$tbl_microbase_rdvl,
                                    tabel_def=App$tbl_microbase_rdvl_def){
  
  # voeg de rvo en nvwa data samen
  df_lev_rdvl <- voeg_rvo_runderen_bij_nvwa(bst_nvwa=levering_bst_nvwa, 
                                            bst_rvo=levering_bst_rvo, jrmnd)

  # haal levering op en zet om in draaitabel
  df_lev      <- inl_maak_draaitabel(df=df_lev_rdvl, vsmd=maanden_huidige_vm,
                                     extra_kolommen=c("is_biologisch", "bron"))
  df_lev      <- subset(df_lev, select=-c(vwmd)) # verwijder kolom vwmd
  
  # haal de historische data op uit de database
  df_hist     <- inl_lees_hist_aant(jrmnd=jrmnd, tabel=App$tbl_microbase_rdvl)
  
  # haal hist data met runderen weg (gebruiken nu gesplitste onderverdelingen)
  df_hist_zonder_runderen <- dplyr::filter(df_hist, dsrt != "Kalveren" & dsrt != "Volwassen runderen")
    
  # voeg historische data en levering samen
  data <- merge(df_hist_zonder_runderen, df_lev, by=c("wrkp", "dsrt", "is_biologisch"), all=TRUE)
  data$slnm.y <- ifelse(is.na(data$slnm.y), data$slnm.x, data$slnm.y) #aanpassing MDAK
  data$slnm <- data$slnm.y #MDAK variabele slnm 
  data$slnm.x <- NULL #MDAK verwijderen kolom slnm.x
  data$slnm.y <- NULL #MDAK verwijderen kolom slnm.y
  # kolomvolgorde aanpassen
  data <- data |>
    dplyr::relocate("slnm") |> # slachterijnaam als meest linkse kolom 
    dplyr::relocate("is_biologisch", .after = "dsrt") |>
    dplyr::relocate("bron", .after = last_col()) # bron als meest rechtse kolom
  data <- data[order(data$slnm),] #MDAK sorteren op slnm
  

  # ##################################################
  # een extra frame wordt toegevoegd met def cijfers vanaf 14 maanden voor verwerkingsmaand
  # dit frame is nodig om te voorkomen dat er teveel lege regels worden verwijderd
  #df_def_max <- inl_lees_hist_max_aant_eerste_mnd_tot_4_mnd_terug_max(jrmnd, tabel_def)
  #data <- merge(data, df_def_max, by=c("slnm", "wrkp", "dsrt"), all=TRUE)
  #data <- data[rowSums(is.na(data)) != ncol(data), ] # rijen niet als alleen NA's
  ## de toegevoegde kolom wordt weer verwijderd
  #data <- subset(data, select = -max_aantal)
  # ##################################################
  data[, 5:11] <- sapply(data[, 5:11], as.numeric) # maak van de waarden numerics

  # voeg initialisatie indicatoren toe
  indi <- data.frame("ind1"=rep("", nrow(data)),
                     "ind2"=rep("", nrow(data)),
                     "ind3"=rep("", nrow(data)))
  # type moet voor dit dataframe op character worden gezet
  indi <- data.frame(lapply(indi, as.character), stringsAsFactors=FALSE)
  data_plus_indi <- cbind(data, indi)

  # verzamel data uit definitieve maanden
  # als mnd1 201809, dan def_maand_min 201709 (zelfde maand jaar eerder)
  def_maand_min <- paste0(as.character(as.integer(substr(jrmnd$mnd1,1,4))-1),
                          substr(jrmnd$mnd1,5,6))
  sqlstr <- paste0("select * from ", schema, ".", tabel_def,
                   " where vsmd < ", jrmnd$mnd1, " and vsmd >= ", def_maand_min)
  data_def <- dat_lees_db(sqlstr) 
  data_def_nvwa_runderloos <- dplyr::filter(data_def, dsrt != "Kalveren" & dsrt != "Volwassen runderen")
  
  # verzamel data van vorige levering
  sqlstr <- paste0("select * from ", schema, ".", tabel,
                   " where vwmd = ", jrmnd$vvwmd)
  data_vorige_lev <- dat_lees_db(sqlstr) 
  data_vorige_lev_nvwa_runderloos <- dplyr::filter(data_vorige_lev, dsrt != "Kalveren" & dsrt != "Volwassen runderen")
  
  # bepaal bijschattingen vorige levering
  df_bijschatting_vorige_lev <- bepaal_bijschattingen_vorige_lev_rdvl_wtvl(df=data_vorige_lev)

  return(list(data=data_plus_indi,
              data_def=data_def_nvwa_runderloos,
              data_lev=df_lev_rdvl,
              # met NVWA runderen kiezen om indicatoren te behouden; wordt gefiltert op slnm
              data_vorige_lev=data_vorige_lev, # data_vorige_lev_nvwa_runderloos,
              bijschatting_vorige_lev=df_bijschatting_vorige_lev))
}


#' Selecteer de gewenste rijen en kolommen in de juiste volgorde. Extra controle
#' om ervoor te zorgen dat alle kolommen aanwezig zijn voor het maken van de
#' ctcr-sheet. 
#' NB. Deze functie wordt gebruikt voor witvlees.
#'
#' @param df dataframe
#' @param vsdm list met verslagmaanden horend bij verwerkingsmaand
#' @return df dataframe
selecteer_frame_wtvl <- function(df, vsmd) {
  df <- df[, c("slnm", "wrkp", "dsrt",
               "vvm_mnd1", "vvm_mnd2", "vvm_mnd3", vsmd,
               "ind1", "ind2", "ind3")]
  return(df)
}


#' Selecteer de gewenste rijen en kolommen in de juiste volgorde. Extra controle
#' om ervoor te zorgen dat alle kolommen aanwezig zijn voor het maken van de
#' ctcr-sheet.
#'
#' NB. Deze functie wordt gebruikt voor roodvlees (RVO en NVWA)
#'
#' @param df dataframe
#' @param vsdm list met verslagmaanden horend bij verwerkingsmaand
#' @return df dataframe
selecteer_frame_rdvl <- function(df, vsmd) {
  df <- df[, c("slnm", "wrkp", "dsrt", "is_biologisch",
               "vvm_mnd1", "vvm_mnd2", "vvm_mnd3", vsmd, "bron",
               "ind1", "ind2", "ind3")]
  return(df)
}


#' Maak het controle- en correctiebestand voor de aantallen roodvlees.
#'
#' @param inputbestand string met volledig pad naar inputbestand
#' @param jrmnd list met verslagmaanden, verwerkingsmaand en vorige
#'              verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param maanden_vorige_vm list met verslagmaanden horend bij vorige
#'                          verwerkingsmaand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param outputbestand string met volledig pad naar outputbestand
#' @return txt string met tekst die naar het scherm wordt geschreven

maak_ctcr_aant_rdvl <- function(inputbestand_nvwa, inputbestand_rvo, jrmnd, 
                                maanden_vorige_vm, maanden_huidige_vm, 
                                outputbestand) {
  logdebug(msg=paste("Systeem start maken ctcr-bestand aantallen roodvlees",
                      outputbestand))
  withProgress(message="", value=0, {
      mld <- "Verzamelen data"
      setProgress(message=mld, value=0.2)
      logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
      res <- verzamel_data_ctcr_rdvl(jrmnd=jrmnd, 
                                     levering_bst_nvwa=inputbestand_nvwa,
                                     levering_bst_rvo=inputbestand_rvo,
                                     maanden_huidige_vm=maanden_huidige_vm)
      data_met_rvo_runderen   <- res$data
      data_def                <- res$data_def # data laatste 12 definitieve maanden
      data_lev                <- res$data_lev # data levering
      data_vorige_lev         <- res$data_vorige_lev # data vorige levering
      bijschatting_vorige_lev <- res$bijschatting_vorige_lev
      
# MDAK: Tijdelijk er uit halen van foute records. 
# browser()
data_def$aant_gaaf[is.na(data_def$aant_gaaf)] <- 0 

      mld <- "Toevoegen bijschattingen"
      setProgress(message=mld, value=0.4)
      logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
      
      # splits de runderen af voor: 
      # de bijschattingen, de indicatoren, de incidentele slachtingen
      ch_runderen <- c("Kalveren 0-8 mnd", "Kalveren 8-12 mnd", "Koeien",
                       "Vaarzen", "Stieren", "Onbekend")
      data_zonder_runderen <- dplyr::filter(data_met_rvo_runderen, !(dsrt %in% ch_runderen))
      
      data_bij <- bij_voeg_bijschattingen_toe(df=data_zonder_runderen, df_def=data_def,
                                              maanden_huidige_vm=maanden_huidige_vm)
      bijschatting <- data_bij$df_bijschatting
      data <- data_bij$df
      
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
      rdvl_def=App$tbl_microbase_rdvl_def
      df_def_max <- inl_lees_hist_max_aant_x_maand_terug(jrmnd, rdvl_def)
      df_def_max <- ddply(df_def_max, .(wrkp, dsrt), function(x) x[which.max(x$max_aantal),]) #MDAK dubbelen er uit halen
      data <- merge(data, df_def_max, by=c("wrkp", "dsrt"), all=TRUE) #MDAK aanpassing
      data$slnm.x <- ifelse(is.na(data$slnm.x), data$slnm.y, data$slnm.x) #Aanpassing MDAK
      data$slnm <- data$slnm.x #MDAK variabele slnm 
      data$slnm.x <- NULL #MDAK verwijderen kolom slnm.x
      data$slnm.y <- NULL #MDAK verwijderen kolom slnm.y
      data <- dplyr::relocate(data, slnm) #MDAK volgorde kolommen aanpassen
      data <- data[order(data$slnm),] #MDAK sorteren op slnm
      data <- data[rowSums(is.na(data)) != ncol(data), ] # rijen niet als alleen NA's
      # de toegevoegde kolom wordt weer verwijderd
      data_schattingen_zonder_rund <- subset(data, select = -max_aantal)
      
      # voeg runderen weer toe na: 
      # bijschatting, incicatoren, incidentele slachtingen
      class(data_met_rvo_runderen$ind3) <- "double"
      data_met_rvo_runderen <- dplyr::filter(data_met_rvo_runderen, 
                                             (dsrt %in% ch_runderen)) |>
      dplyr::bind_rows(data_schattingen_zonder_rund) |>
      dplyr::arrange(slnm)

      mld <- "Wegschrijven naar Excel"
      setProgress(message=mld, value=0.8)
      logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
      data <- selecteer_frame_rdvl(df=data_met_rvo_runderen, vsmd=maanden_huidige_vm)
      stel_samen_ctcr_rdvl(maanden_vorige_vm=maanden_vorige_vm,
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

  logdebug(msg="Systeem is gereed met maken ctcr-bestand aantallen roodvlees")

  return(txt=txt) # outputtekst naar het scherm
}


#' Maakt het tijdelijke controle-correctie bestand voor verschillen in aantallen 
#' tussen NVWA en RVO runderen 
#'
#' @param jrmnd list. resultaat uit alg_vind_jaarmaanden()
#' @param inputbestand_nvwa string. pad naar de nvwa levering
#' @param inputbestand_rvo string. pad naar de rvo levering
#' @param maanden_huidige_vm list. bevat de eerste vier verslagmaanden uit jrmnd
#' @param outputbestand string. pad naar het bestand dat moet worden opgeslagen.
#'
#' @return String. Of het bestand succesvol is aangemaakt.
maak_ctcr_verschil_rvo_nvwa <- function(jrmnd, inputbestand_nvwa, 
                                        inputbestand_rvo, maanden_huidige_vm,
                                        outputbestand){
  # tel of er nvwa rijen zijn met runderen
  nvwa_heeft_runderen <- check_nvwa_voor_runderen(jrmnd, inputbestand_nvwa)
  
  # zo ja, maak het verschil bestand
  if (nvwa_heeft_runderen) {
    data_verschil <- verzamel_data_ctcr_nvwa_rvo(jrmnd, inputbestand_rvo, inputbestand_nvwa)
    stel_samen_ctcr_nvwa_rvo(maanden_huidige_vm, data_verschil, outputbestand)
  } else {
    txt <- paste("NVWA bevat geen runderen.")
  }
  
  # verifieer of outputbestand gemaakt is
  if (file.exists(outputbestand)) {
    txt <- paste("bestand", basename(outputbestand), "is gemaakt")
  } else {
    txt <- paste(txt, "bestand", basename(outputbestand), "is niet gemaakt")
  }
  
  return(txt=txt) # outputtekst naar het scherm
}


#' Checkt of de nvwa levering runderen bevat. 
#'
#' @param jrmnd list. resultaat uit alg_vind_jaarmaanden()
#' @param inputbestand_nvwa string. pad naar de nvwa levering
#'
#' @return boolean.
#'
#' @examples
check_nvwa_voor_runderen <- function(jrmnd, inputbestand_nvwa){
  df_nvwa <- inl_lees_input_rdvl(inputbestand_nvwa, jrmnd)
  
  # tel of er rijen met runderen zijn
  df_nvwa_runderen <- dplyr::filter(df_nvwa, 
                                    dsrt == "Kalveren" | dsrt == "Volwassen runderen")
  nvwa_heeft_runderen <- ifelse(nrow(df_nvwa_runderen) > 0,
                                no = FALSE,
                                yes = TRUE)
}


#' Verwijdert de runderen uit NVWA en vervangt deze door de runderen uit RVO.
#'
#' @param jrmnd list. verslagmaanden. resultaat uit alg_vind_jaarmaanden()
#' @param inputbestand_nvwa string. pad naar de nvwa levering
#' @param inputbestand_rvo string. pad naar de rvo levering
#'
#' @return data.frame met RVO runderen en de rest van het NVWA roodvlees
voeg_rvo_runderen_bij_nvwa <- function(bst_nvwa, bst_rvo, jrmnd) {
  
  # lees de bestanden in (voegt een kolom met BIO toe)
  df_nvwa <- inl_lees_input_rdvl(bst_nvwa, jrmnd)
  df_rvo <- inl_lees_input_rdvl(bst_rvo, jrmnd)
  
  # selecteer alle NVWA rijen die GEEN Kalveren of Volwassen runderen bevatten
  df_geen_runderen <- df_nvwa |>
    dplyr::filter(
      !(dsrt %in% c("Kalveren",  "Volwassen runderen"))
  ) |>
    dplyr::mutate(
      bron = "NVWA"
  )
  
  df_rvo_plus_bron <- dplyr::mutate(df_rvo, bron="RVO")
  
  # voeg de rijen van runderloze nvwa bij rvo
  df_rdvl <- dplyr::bind_rows(df_rvo_plus_bron, df_geen_runderen)
}


#' Maak het ctcrbestand voor de verschillen tussen rvo en nvwa op.
#'
#' @param wb workbook object containing a worksheet
#' @param data dataframe met data
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @return wb workbook object containing a worksheet
maak_op_vers_nvwa_rvo <- function(wb, data, maanden_huidige_vm){
  laatste_rij <- nrow(data)+2
  
  # eerste rij: tekst dikgedrukt en gecentreerd
  addStyle(wb, sheet=1, style=createStyle(textDecoration='bold', halign='center'),
           rows=1, cols=1:16, gridExpand=TRUE, stack=TRUE)
  
  # tweede rij: tekst dikgedrukt
  addStyle(wb, sheet=1, style=createStyle(textDecoration='bold'),
           rows=2, cols=1:16, gridExpand=TRUE, stack=TRUE)
  
  # cellen vorige verwerkingsmaand: achtergrondkleur lichtgroen
  addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_nvwa_rvo),
           rows=2:laatste_rij, cols=c(5,6,8,9,11,12,14,15), gridExpand=TRUE, stack=TRUE)
  
  # verticale lijn rechts van diersoort
  addStyle(wb, sheet=1, style=createStyle(border='right'),
           rows=1:laatste_rij, cols=4, gridExpand=TRUE, stack=TRUE)
  
  # verticale lijn tussen de verwerkingsmaanden
  addStyle(wb, sheet=1, style=createStyle(border='right'),
           rows=1:laatste_rij, cols=7, gridExpand=TRUE, stack=TRUE)
  addStyle(wb, sheet=1, style=createStyle(border='right'),
           rows=1:laatste_rij, cols=10, gridExpand=TRUE, stack=TRUE)
  addStyle(wb, sheet=1, style=createStyle(border='right'),
           rows=1:laatste_rij, cols=13, gridExpand=TRUE, stack=TRUE)
  
  # verticale lijn tussen data verwerkingsmaand en controle
  addStyle(wb, sheet=1, style=createStyle(border='right'),
           rows=1:laatste_rij, cols=5:6, gridExpand=TRUE, stack=TRUE)
  addStyle(wb, sheet=1, style=createStyle(border='right'),
           rows=1:laatste_rij, cols=8:9, gridExpand=TRUE, stack=TRUE)
  addStyle(wb, sheet=1, style=createStyle(border='right'),
           rows=1:laatste_rij, cols=11:12, gridExpand=TRUE, stack=TRUE)
  addStyle(wb, sheet=1, style=createStyle(border='right'),
           rows=1:laatste_rij, cols=14:15, gridExpand=TRUE, stack=TRUE)
  
  # horizontale lijn tussen slachtbedrijven (als wrkp verandert)
  
  for (i in head(seq_along(data$wrkp), -1)) {
    if (!identical(data$wrkp[i], data$wrkp[i + 1])) {
      addStyle(wb, sheet = 1, style = createStyle(border = "bottom"),
               rows = i + 2, cols = 1:16, gridExpand = TRUE,stack = TRUE)
    }        # rows=i+2 omdat data op rij 3 begint
  }
  
  # stel kolombreedten in
  setColWidths(wb, sheet=1, cols=1, widths=App$breedte_slachthuis) # kolom slachthuis naam
  setColWidths(wb, sheet=1, cols=2, widths=App$breedte_werkplek) # kolom werkplek
  setColWidths(wb, sheet=1, cols=3, widths=App$breedte_werkplek) # kolom ubn
  setColWidths(wb, sheet=1, cols=4, widths=App$breedte_diersoort) # kolom diersoort
  setColWidths(wb, sheet=1, cols=c(7,10,13,16), widths=App$breedte_verschil) # alle kolommen indicatoren
  
  # stel weergave numerieke waarden in: toon afgeronde waarden zonder decimalen
  addStyle(wb, 1, style=createStyle(numFmt="#,##0"),
           rows=3:laatste_rij, cols=5:10, gridExpand=TRUE, stack=TRUE)
  
  return(wb)
}


#' Stel het controlebestand voor de verschillen tussen rvo en nvwa samen.
#'
#' We schrijven de groepkoppen, de kolomkoppen en de data in 1x weg (inclusief
#' de randen eromheen). Vervolgens voegen we de overige opmaak toe. Ten slotte
#' slaan we het Excelbestand op.
#'
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param data dataframe met data om weg te schrijven naar excelsheet
#' @param bestand string met volledig pad naar outputbestand
#' @return niets
stel_samen_ctcr_nvwa_rvo <- function(maanden_huidige_vm, data, bestand) {
  wb <- createWorkbook()
  addWorksheet(wb, "verschil")
  freezePane(wb, "verschil",  firstActiveRow=3)
  
  # stel headers samen (rij 1 en 2)
  groepkoppen <- t(c('', '', '', '', 'I&R', 'NVWA', 'Controle (I&R - NVWA)', 'I&R', 'NVWA', 'Controle (I&R - NVWA)',
                     'I&R', 'NVWA', 'Controle (I&R - NVWA)', 'I&R', 'NVWA', 'Controle (I&R - NVWA)'))
  kolomkoppen <- t(c('Naam slachtbedrijf', 'Werkpleknr', 'Ubn', 'Diersoort', maanden_huidige_vm[1], maanden_huidige_vm[1], 
                     maanden_huidige_vm[1], maanden_huidige_vm[2], maanden_huidige_vm[2], maanden_huidige_vm[2], maanden_huidige_vm[3],
                     maanden_huidige_vm[3], maanden_huidige_vm[3], maanden_huidige_vm[4], maanden_huidige_vm[4], maanden_huidige_vm[4]))
  
  # schrijf data weg met buitenrand
  writeData(wb, sheet=1, x=groepkoppen, startRow=1, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=kolomkoppen, startRow=2, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=data, startRow=3, colNames=F, borders=c("surrounding"))
  
  wb <- maak_op_vers_nvwa_rvo(wb, data, maanden_huidige_vm)
  
  saveWorkbook(wb, file=bestand, overwrite=TRUE)
}


#' Verzamel alle benodigde gegevens om het aantallen controlebestand voor
#' RVO en NVWA te maken.
#'
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param input_rvo string met volledig pad naar inputbestand rvo
#' @param input_nvwa string met volledig pad naar inputbestand nvwa
#' @return data dataframe met data wat op sheet kan worden gezet
verzamel_data_ctcr_nvwa_rvo <- function(jrmnd, input_rvo, input_nvwa, koppelbestandpad = App$koppelbestandpad){
  
  # filter alleen de relevante maanden
  df_nvwa <- inl_lees_input_rdvl(input_nvwa, jrmnd)
  df_rvo <- inl_lees_input_rdvl(input_rvo, jrmnd)
  
  # rvo data moet geaggregeerd worden op diersoort (per verslagmaand) en in wrkp kolom alleen het nummer bewaren
  df_rvo_agg <- df_rvo %>% 
    dplyr::mutate(dsrt = ifelse(
      test = dsrt %in% c("Kalveren 0-8 mnd", "Kalveren 8-12 mnd"), 
      no = "Volwassen runderen",
      yes = "Kalveren")) |>
    dplyr::group_by(dsrt, wrkp, vsmd) %>% 
    dplyr::summarise("I&R" = sum(aant)) %>% 
    dplyr::mutate(wrkp = str_remove(wrkp,"UBN:"))
  
  #deze controle checkt alleen de runderen
  df_nvwa_rund <- df_nvwa %>% 
    dplyr::filter(dsrt == "Kalveren" | dsrt == "Volwassen runderen")
  
  # koppeltabel van de 2 leveringen inladen
  koppel <- read.csv(koppelbestandpad, colClasses = "character")
  koppel$WERKPLEK_NR <- sprintf("%07d",as.numeric(koppel$WERKPLEK_NR))
  
  # data rvo koppelen aan de koppeltabel en kolommen hernoemen
  df_rvo_koppel <- left_join(df_rvo_agg, koppel, by = c("wrkp" = "UBN")) %>% 
   dplyr::rename(ubn = wrkp,
     wrkp = WERKPLEK_NR)
  
  # data nvwa koppelen aan de koppeltabel en kolommen hernoemen
  df_nvwa_koppel <- left_join(df_nvwa_rund, koppel, by = c("wrkp" = "WERKPLEK_NR")) %>% 
    dplyr::rename(ubn = UBN)
  
  # voeg data van de 2 leveringen samen
  df_gekoppeld <- full_join(df_rvo_koppel, df_nvwa_koppel, by = c("wrkp","ubn", "dsrt", "vsmd"))
  
  # als een verslagmaand/slachthuis combinatie in de nvwa data geen gegevens bevat maar de rvo data wel
  # dan ontbreekt de verwerkingsmaand na koppeling dus opnieuw vullen
  df_gekoppeld$vwmd <- jrmnd$vwmd
  
  # data <- data %>% 
  #   left_join(koppel, by = c("wrkp" = "WERKPLEK_NR")) %>%
  #   dplyr::rename(ubn = UBN)
  
  # als een verslagmaand/slachthuis combinatie in de nvwa data geen gegevens bevat maar de rvo data wel
  # dan ontbreekt de slachthuisnaam na koppeling dus opnieuw vullen
  slnm <- df_gekoppeld[,c("wrkp","slnm")]
  slnm <- unique(slnm)
  slnm <- slnm[!is.na(slnm$slnm),]
  slnm <- slnm[!duplicated(slnm$wrkp),]
  
  df_gekoppeld$slnm <- NULL
  df_gevuld <- df_gekoppeld %>%
    left_join(slnm)
  
  # deze kolom komt niet in het controlebestand voor
  df_gevuld$is_biologisch <- NULL
  
  # geef merged data weer per vsmd, als één rij per slachthuis & diersoort
  df_wide <- tidyr::pivot_wider(df_gevuld, names_from = "vsmd", 
                             values_from = c("I&R", "aant")
  )
  
  # kolomnamen van huidige verwerkingsmaan definieren zodat deze kunnen worden vergeleken
  ir_mnd <- c(paste("I&R", jrmnd$mnd1, sep = "_"),paste("I&R", jrmnd$mnd2, sep = "_"),paste("I&R", jrmnd$mnd3, sep = "_"),paste("I&R", jrmnd$mnd4, sep = "_"
                                                                                                                                ))
  nvwa_mnd <- c(paste("aant", jrmnd$mnd1, sep = "_"),paste("aant", jrmnd$mnd2, sep = "_"),paste("aant", jrmnd$mnd3, sep = "_"),paste("aant", jrmnd$mnd4, sep = "_"))
  for (i in 1:4){
    df_wide[, paste("Controle (I&R - NVWA)", jrmnd[i], sep = " ")] <- df_wide[ir_mnd[i]] - df_wide[nvwa_mnd[i]]
  }
  
  # select kolommen
  col_order <- c("slnm", "wrkp","ubn", "dsrt", ir_mnd[1], nvwa_mnd[1], paste("Controle (I&R - NVWA)", jrmnd[1], sep = " "),
                 ir_mnd[2], nvwa_mnd[2], paste("Controle (I&R - NVWA)", jrmnd[2], sep = " "),
                 ir_mnd[3], nvwa_mnd[3], paste("Controle (I&R - NVWA)", jrmnd[3], sep = " "),
                 ir_mnd[4], nvwa_mnd[4], paste("Controle (I&R - NVWA)", jrmnd[4], sep = " "))
  df_final <- df_wide[, col_order]
  df_final <- df_final[order(df_final$slnm),] #MDAK sorteren op slnm
  
  return(df_final)
}

