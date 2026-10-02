#' Naam        : analysebestand_tijdreeksen.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om het analysebestand voor tijdreeksen te maken.
#'               Met dit bestand kan geanalyseerd worden of de gegevens
#'               van de laatste maanden plausibel zijn.


#' Maak het analysebestand voor de tijdreeksen op.
#'
#' @param wb Workbook object containing a worksheet.
#' @return wb Workbook object containing a worksheet.
maak_op_trks <- function (wb, jrmnd) {
    # eerste rij: tekst dikgedrukt en gecentreerd
    addStyle(wb, sheet=1, style=createStyle(textDecoration='bold', border='topbottom'),
             rows=1, cols=1:14, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, sheet=1, style=createStyle(textDecoration='bold', border='right'),
             rows=1:101, cols=2:2, gridExpand=TRUE, stack=TRUE)

    # bereid de kleuring van de voorlopige cijfers voor
    mnd1 <- as.integer(substr(jrmnd$mnd1,5,6))
    if (mnd1 < 10){
        eerste_rij_eerste_kolom <- 100 #buiten beeld; 0 werkt niet
        eerste_rij_laatste_kolom <- 100 #buiten beeld; 0 werkt niet
        tweede_rij_eerste_kolom <- mnd1 + 2
        tweede_rij_laatste_kolom <- mnd1 + 5
    } else if (mnd1 == 10){
        eerste_rij_eerste_kolom <- 12
        eerste_rij_laatste_kolom <- 14
        tweede_rij_eerste_kolom <- 3
        tweede_rij_laatste_kolom <- 3
    }else if (mnd1 == 11){
        eerste_rij_eerste_kolom <- 13
        eerste_rij_laatste_kolom <- 14
        tweede_rij_eerste_kolom <- 3
        tweede_rij_laatste_kolom <- 4
    } else if (mnd1 == 12){
        eerste_rij_eerste_kolom <- 14
        eerste_rij_laatste_kolom <- 14
        tweede_rij_eerste_kolom <- 3
        tweede_rij_laatste_kolom <- 5
    }

    # lijnen onder diersoortcategorie
    # kleuring van de voorlopige cijfers
    i = 6
    while (i<105) {
        addStyle(wb, sheet=1, style=createStyle(border='bottom'),
                 rows=i, cols=1:14, gridExpand=TRUE, stack=TRUE)
        addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_huidige_vwmd),
                 rows=i-1, cols=eerste_rij_eerste_kolom:eerste_rij_laatste_kolom, gridExpand=TRUE, stack=TRUE)
        addStyle(wb, sheet=1, style=createStyle(fgFill=App$kleur_huidige_vwmd),
                 rows=i, cols=tweede_rij_eerste_kolom:tweede_rij_laatste_kolom, gridExpand=TRUE, stack=TRUE)

        if (mnd1 < 10){ #maak de onterecht geel gekleurde vakjes weer wit
            addStyle(wb, sheet=1, style=createStyle(fgFill='white'),
                rows=i-1, cols=eerste_rij_eerste_kolom:eerste_rij_laatste_kolom, gridExpand=TRUE, stack=TRUE)
        }
        i = i+5
    }

    # stel kolombreedten in
    setColWidths(wb, sheet=1, cols=1, widths=App$breedte_diersoortcat) # kolom diersoortcategorie
    setColWidths(wb, sheet=1, cols=2, widths=App$breedte_trks_jaar) # kolom jaar
    setColWidths(wb, sheet=1, cols=3:14, widths=App$breedte_trks_mnd) # kolommen maand

    # stel weergave numerieke waarden in: 1 decimaal
    addStyle(wb, sheet=1, style=createStyle(numFmt="#,##0.0"),
             rows=2:101, cols=3:14, gridExpand=TRUE, stack=TRUE)

    return(wb)
}

#
# #' Stel het controle- en correctiebestand voor de gemiddelde gewichten samen.
# #'
# #' We schrijven de groepkoppen, de kolomkoppen en de data in 1x weg (inclusief
# #' de randen eromheen). Vervolgens voegen we de overige opmaak toe. Ten slotte
# #' slaan we het Excelbestand op.
# #'
# #' @param maanden_vorige_vm list met verslagmaanden horend bij vorige
# #'                          verwerkingsmaand.
# #' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand.
# #' @param data dataframe met data om weg te schrijven naar excelsheet.
# #' @param bestand string met volledig pad naar outputbestand.
# #' @return niets.
stel_samen_ana_tijdreeksen <- function(data, jrmnd, bestand) {
  wb <- createWorkbook()
  addWorksheet(wb, "tijdreeksen")
  freezePane(wb, "tijdreeksen" ,  firstActiveRow = 2,  firstActiveCol = 3)

  # stel headers samen (rij 1 en kolom 1)
  kolomkoppen <- t(c('Categorie', 'jaar', 'januari', 'februari','maart','april','mei','juni',
                     'juli','augustus','september',
                     'oktober','november','december'))
  #rijkoppen <- c('Stieren', 'Koeien', 'Vaarzen',
  #               'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd',
  #               'Lammeren', 'Volwassen schapen',
  #               'Varkens', 'Eenhoevige dieren', 'Geiten', 'Vleeskuikens',
  #               'Overige kippen', 'Eenden', 'Duiven', 'Fazanten', 'Ganzen',
  #               'Kalkoenen', 'Parelhoenders', 'Patrijzen', 'Struisvogels')
  ana_tijdreeksen <- data.frame(data)

  # schrijf data weg met buitenrand
  writeData(wb, sheet=1, x=kolomkoppen, startRow=1, colNames=F, borders=c("surrounding"))
  writeData(wb, sheet=1, x=ana_tijdreeksen, startRow=2, colNames=F, borders=c("surrounding"))

  wb <- maak_op_trks(wb, jrmnd)
  saveWorkbook(wb, file=bestand, overwrite=TRUE)
}
#
#
# #' Verzamel de definitieve en voorlopige gegevens uit de database
# #' stel op basis hiervan een dataframe samen
# #'
# #' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
# #'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
# #' @param verzamel_bio verzamel alleen data over biologische slachtingen
# #'
# #' @return df_tot dataframe met data die op sheet kunnen worden gezet
# #' @example.
# #' > verzamel_data_ana_tijdreeksen(jrmnd)
#'    dcat   jr      januari     februari maart april mei juni juli augustus september  ...   december
#' Stieren 2013           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...         NA
#' Stieren 2014           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...         NA
#' Stieren 2015           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...         NA
#' Stieren 2016           NA           NA    NA    NA  NA   NA   NA       NA        NA  ...   5263.227
#' Stieren 2017     4734.408     5502.624    NA    NA  NA   NA   NA       NA        NA  ...         NA
verzamel_data_ana_tijdreeksen <- function(jrmnd, verzamel_bio=FALSE){
    # bepaal enkele hulpvariabelen
    jr5 <- substr(jrmnd$mnd4, 1, 4)
    jr1 <- as.character(as.integer(jr5)-4)
    jr2 <- as.character(as.integer(jr5)-3)
    jr3 <- as.character(as.integer(jr5)-2)
    jr4 <- as.character(as.integer(jr5)-1)
    jaren <- data.frame('jr'=c(jr1,jr2,jr3,jr4,jr5))
    eerste_def_mnd <- paste0(jr1,"01")
    eerste_vlp_mnd <- jrmnd$mnd1

  # haal gegevens van huidige verwerkingsmaand op
  df_vlp <- inl_lees_vlp_tijdreeksen(eerste_vlp_mnd, jrmnd$vwmd)

  # haal definitieve gegevens op en voeg gegevens samen
  df_def <- inl_lees_hist_tijdreeksen(eerste_def_mnd, eerste_vlp_mnd)
  df_totaal <- rbind(df_vlp, df_def)
  
  if (verzamel_bio) {
    df_totaal <- df_totaal[df_totaal$is_biologisch=="1",]
    df_totaal <- df_totaal[!is.na(df_totaal$dcat),]
  } else {
    df_totaal$totaal <- as.numeric(df_totaal$totaal)
    df_totaal <- group_by(df_totaal, dcat, vsmd) %>% summarize(totaal=sum(totaal))
  }

  df_totaal$jr <- substr(df_totaal$vsmd, 1, 4)
  df_totaal$mnd <- substr(df_totaal$vsmd, 5, 6)

  # maak per maand een dataframe en hernoem de kolom met het totaalaantal
  df_totaal <- df_totaal[,c("dcat","jr","mnd","vsmd","totaal")]
  df_jan <- df_totaal[df_totaal$mnd=="01",]
  colnames(df_jan)[colnames(df_jan) == "totaal"] <- "januari"
  df_feb <- df_totaal[df_totaal$mnd=="02",]
  colnames(df_feb)[colnames(df_feb) == "totaal"] <- "februari"
  df_maa <- df_totaal[df_totaal$mnd=="03",]
  colnames(df_maa)[colnames(df_maa) == "totaal"] <- "maart"
  df_apr <- df_totaal[df_totaal$mnd=="04",]
  colnames(df_apr)[colnames(df_apr) == "totaal"] <- "april"
  df_mei <- df_totaal[df_totaal$mnd=="05",]
  colnames(df_mei)[colnames(df_mei) == "totaal"] <- "mei"
  df_jun <- df_totaal[df_totaal$mnd=="06",]
  colnames(df_jun)[colnames(df_jun) == "totaal"] <- "juni"
  df_jul <- df_totaal[df_totaal$mnd=="07",]
  colnames(df_jul)[colnames(df_jul) == "totaal"] <- "juli"
  df_aug <- df_totaal[df_totaal$mnd=="08",]
  colnames(df_aug)[colnames(df_aug) == "totaal"] <- "augustus"
  df_sep <- df_totaal[df_totaal$mnd=="09",]
  colnames(df_sep)[colnames(df_sep) == "totaal"] <- "september"
  df_okt <- df_totaal[df_totaal$mnd=="10",]
  colnames(df_okt)[colnames(df_okt) == "totaal"] <- "oktober"
  df_nov <- df_totaal[df_totaal$mnd=="11",]
  colnames(df_nov)[colnames(df_nov) == "totaal"] <- "november"
  df_dec <- df_totaal[df_totaal$mnd=="12",]
  colnames(df_dec)[colnames(df_dec) == "totaal"] <- "december"

  # stel een kader samen voor de dieren en alle jaren
  dieren <- data.frame("sortering"=c(1,2,3,4,5,6,7,8,9,10,
                                     11,12,13,14,15,16,17,18,19,20),
                       "dcat"=c('Stieren', 'Koeien', 'Vaarzen',
                                'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd',
                                'Lammeren', 'Volwassen schapen',
                                'Varkens', 'Eenhoevige dieren', 'Geiten', 'Vleeskuikens',
                                'Overige kippen', 'Eenden', 'Duiven', 'Fazanten', 'Ganzen',
                                'Kalkoenen', 'Parelhoenders', 'Patrijzen', 'Struisvogels'))
  if (verzamel_bio) {
    # verzamel alleen dieren die ook biologische records hebben
    dieren <- dieren[dieren$dcat %in% df_totaal$dcat,]
  }
  df_tot <- merge(dieren, jaren, all=TRUE, sort=FALSE)
  colnames(df_tot) <- c('sortering','dcat','jr')
  df_tot <- df_tot[with(df_tot, order(sortering,jr)),]

  # voeg maandkolommen toe
  df_tot <- merge(df_tot, df_jan[,c("dcat","jr","januari")], by=c("dcat","jr"), all=TRUE, sort=FALSE)
  df_tot <- merge(df_tot, df_feb[,c("dcat","jr","februari")], by=c("dcat","jr"), all=TRUE, sort=FALSE)
  df_tot <- merge(df_tot, df_maa[,c("dcat","jr","maart")], by=c("dcat","jr"), all=TRUE, sort=FALSE)
  df_tot <- merge(df_tot, df_apr[,c("dcat","jr","april")], by=c("dcat","jr"), all=TRUE, sort=FALSE)
  df_tot <- merge(df_tot, df_mei[,c("dcat","jr","mei")], by=c("dcat","jr"), all=TRUE, sort=FALSE)
  df_tot <- merge(df_tot, df_jun[,c("dcat","jr","juni")], by=c("dcat","jr"), all=TRUE, sort=FALSE)
  df_tot <- merge(df_tot, df_jul[,c("dcat","jr","juli")], by=c("dcat","jr"), all=TRUE, sort=FALSE)
  df_tot <- merge(df_tot, df_aug[,c("dcat","jr","augustus")], by=c("dcat","jr"), all=TRUE, sort=FALSE)
  df_tot <- merge(df_tot, df_sep[,c("dcat","jr","september")], by=c("dcat","jr"), all=TRUE, sort=FALSE)
  df_tot <- merge(df_tot, df_okt[,c("dcat","jr","oktober")], by=c("dcat","jr"), all=TRUE, sort=FALSE)
  df_tot <- merge(df_tot, df_nov[,c("dcat","jr","november")], by=c("dcat","jr"), all=TRUE, sort=FALSE)
  df_tot <- merge(df_tot, df_dec[,c("dcat","jr","december")], by=c("dcat","jr"), all=TRUE, sort=FALSE)

  # zet de rijen in de goede volgorde
  df_tot <- df_tot[with(df_tot, order(sortering,jr)),]

  rownames(df_tot) <- paste(df_tot$dcat, df_tot$jr)

  kolomkoppen <- c("dcat","jr","januari","februari","maart","april","mei","juni",
                   "juli","augustus","september","oktober","november","december")
  df_tot[, kolomkoppen[3:14]] <- sapply(df_tot[, kolomkoppen[3:14]], as.numeric)

  return(df_tot)
}


# #' Selecteer de gewenste rijen en kolommen in de juiste volgorde. Extra controle
# #' om ervoor te zorgen dat alle kolommen aanwezig zijn voor het maken van de
# #' ctcr-sheet.
# #'
# #'
# #' @param df dataframe.
# #' @return df dataframe.
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


#' Maak het analysebestand voor de tijdreeksen
#'
#' We schrijven de groepkoppen, de kolomkoppen en de data in 1x weg (inclusief
#' de randen eromheen). Vervolgens voegen we de overige opmaak toe. Ten slotte
#' slaan we het Excelbestand op.
#'
#' @param inputmap map string
#' @param jrmnd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param outputbestand_totaal string
#' @param outputbestand_biologisch string
#' @return txt string
#' @examples.
maak_ana_tijdreeksen <- function(werkmap=App$werkmap, 
                                 jrmnd, 
                                 outputbestand_totaal, 
                                 outputbestand_biologisch) {

  logdebug(msg=paste("Systeem start maken tijdreeksen in",
                     outputbestand_totaal, "en",
                     outputbestand_biologisch
                     ))

  withProgress(message="", value=0, {
    mld <- "Verzamelen data (totaal)"
    setProgress(message=mld, value=0.2)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data_totaal <- verzamel_data_ana_tijdreeksen(jrmnd=jrmnd)

    mld <- "Wegschrijven naar Excel (totaal)"
    setProgress(message=mld, value=0.4)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data_totaal <- selecteer_frame_tijdreeksen(df=df_data_totaal)
    stel_samen_ana_tijdreeksen(jrmnd=jrmnd, data=df_data_totaal, bestand=outputbestand_totaal)
    
    mld <- "Verzamelen data (biologisch)"
    setProgress(message=mld, value=0.6)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data_bio <- verzamel_data_ana_tijdreeksen(jrmnd=jrmnd, verzamel_bio=TRUE)
    
    mld <- "Wegschrijven naar Excel (biologisch)"
    setProgress(message=mld, value=0.8)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data_bio <- selecteer_frame_tijdreeksen(df=df_data_bio)
    stel_samen_ana_tijdreeksen(jrmnd=jrmnd, data=df_data_bio, bestand=outputbestand_biologisch)

    setProgress(message="Gereed", value=1.0)
  })

  # verifieer of outputbestand_totaal is
  if (file.exists(outputbestand_totaal)) {
    txt <- paste("bestand", basename(outputbestand_totaal), "is gemaakt")
  } else {
    txt <- paste("bestand", basename(outputbestand_totaal), "is niet gemaakt")
  }
  
  # verifieer of outputbestand_biologisch gemaakt is
  if (file.exists(outputbestand_biologisch)) {
    txt <- paste("bestand", basename(outputbestand_biologisch), "is gemaakt")
  } else {
    txt <- paste("bestand", basename(outputbestand_biologisch), "is niet gemaakt")
  }

  logdebug(msg="Systeem is gereed met maken analysebestand tijdreeksen")
  return(txt)
}
