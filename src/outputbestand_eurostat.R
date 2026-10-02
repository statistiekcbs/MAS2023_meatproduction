#' Naam        : outputbestand_Eurostat.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om het outputbestand voor Eurostat te maken. 


#' Maak het outputbestand voor Eurostat op.
#'
#' @param wb Workbook object containing a worksheet.
#' @return wb Workbook object containing a worksheet.
maak_op_eurostat <- function (wb) {
    # stel kolombreedten in
    setColWidths(wb, sheet=1, cols=1, widths=App$breedte_eurostat_diersoortcat) # kolom diersoort
    setColWidths(wb, sheet=1, cols=2:17, widths=App$breedte_eurostat_cellen) # alle overige kolommen 
    
    # algemene opmaak
    addStyle(wb, sheet=1, style=createStyle(halign='left'),
             rows=1:34, cols=1:17, gridExpand=TRUE, stack=TRUE)

    # stel weergave voor als tekst geformatteerde numerieke waarden in, lijn rechts uit 
    addStyle(wb, sheet=1, style=createStyle(halign='right'),
             rows=1:34, cols=2, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, sheet=1, style=createStyle(halign='right'),
             rows=1:34, cols=6, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, sheet=1, style=createStyle(halign='right'),
             rows=1:34, cols=10, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, sheet=1, style=createStyle(halign='right'),
             rows=1:34, cols=14, gridExpand=TRUE, stack=TRUE)
    
    # status en geheimhouding
    addStyle(wb, sheet=1, style=createStyle(halign='left'),
             rows=1:34, cols=3:5, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, sheet=1, style=createStyle(halign='left'),
             rows=1:34, cols=7:9, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, sheet=1, style=createStyle(halign='left'),
             rows=1:34, cols=11:13, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, sheet=1, style=createStyle(halign='left'),
             rows=1:34, cols=15:17, gridExpand=TRUE, stack=TRUE)

    # weergave numerieke velden
    addStyle(wb, sheet=1, style=createStyle(numFmt="0.000"),
             rows=1:34, cols=2, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, sheet=1, style=createStyle(numFmt="0.000"),
             rows=1:34, cols=6, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, sheet=1, style=createStyle(numFmt="0.000"),
             rows=1:34, cols=10, gridExpand=TRUE, stack=TRUE)
    addStyle(wb, sheet=1, style=createStyle(numFmt="0.000"),
             rows=1:34, cols=14, gridExpand=TRUE, stack=TRUE)
    
    return(wb)
}


#' Stel het outputbestand voor Eurostat samen.
#'
#' We schrijven de data in 1x weg. 
#' Vervolgens voegen we de overige opmaak toe. Ten slotte
#' slaan we het Excelbestand op.
#'
#' @param data dataframe met data om weg te schrijven naar excelsheet.
#' @param bestand string met volledig pad naar outputbestand.
#' @return niets.
stel_samen_out_eurostat <- function(data, bestand) {

  wb <- createWorkbook()
  addWorksheet(wb, "Eurostat")

  out_eurostat <- data.frame(data)

  # schrijf data weg 
  writeData(wb, sheet=1, x=out_eurostat, startRow=1, colNames=F)
  
  wb <- maak_op_eurostat(wb)
  saveWorkbook(wb, file=bestand, overwrite=TRUE)
  
  # open het script en sla het opnieuw op om te voorkomen dat het gegenereerde excel-bestand
  # wordt tegengehouden door het filter bij het verzenden naar een extern adres
  script <- file.path(App$srcmap, App$open_en_sluit_excel_script)
  system(paste0('powershell -executionpolicy bypass -file ', script, ' -excelbestand "', bestand, '"'))
}


#' hercodeer de diersoorten voor eurostat
#' 
#' @param dc string intern gebruikte diersoort omschrijving
#' @return string met Eurostat-benaming
#' @example
#' > hercodeer_dier_eurostat(dc="Kalveren 0-8 mnd")
#' "Calves"
hercodeer_dier_eurostat <- function(dc) {
    
    if (dc == "Stieren"){return("Bulls")}
    else if (dc == "Ossen"){return("Bullocks")}
    #else if (dc == "Ossen en stieren"){return("BullocksAndBulls")}
    else if (dc == "Koeien"){return("Cows")}
    else if (dc == "Vaarzen"){return("Heifers")}
    else if (dc == "Kalveren 0-8 mnd"){return("Calves")}
    else if (dc == "Kalveren 8-12 mnd"){return("YoungCattle")}
    #else if (dc == "Volwassen runderen"){return("AdultCattle")}
    else if (dc == "Runderen totaal"){return("BovineAnimals")}
    else if (dc == "Lammeren"){return("Sheep.lambs")}
    else if (dc == "Volwassen schapen"){return("Sheep.other")}
    else if (dc == "Schapen totaal"){return("Sheep.total")}
    else if (dc == "Geiten"){return("Goats")}
    else if (dc == "Eenhoevige dieren"){return("Equidae")}
    else if (dc == "Varkens"){return("Pigs")}
    else if (dc == "Eenden"){return("Ducks")}
    else if (dc == "Kalkoenen"){return("Turkeys")}
    else if (dc == "Kippen totaal"){return("Chickens")}
    #else if (dc == "Overig pluimvee"){return("OtherPoultry")}
    else if (dc == "Pluimvee totaal"){return("TotalPoultry")}
    else {return(dc)}
}


#' Verzamel de definitieve en voorlopige gegevens uit de database 
#' stel op basis hiervan een dataframe samen 
#'
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden().
#'
#' @return df_tot dataframe met data die op sheet kunnen worden gezet.
#' @example.
#' > leid_af_df_out_eurostat(df, jrmnd)
#'              dcat mnd1_aant mnd1_totg .. mnd4_aant mnd4_totg  mnd1_geheim .. mnd4_geheim mnd1_status .. mnd4_status
#' Stieren   Stieren     6.521    3011.0 ..  5203.201 2330581.1                                                      P
#' Ossen       Ossen     0           0   ..     0           0                                                        P
leid_af_df_out_eurostat <- function(df, jrmnd){
    
    # aantallen delen door 1.000
    # gewichten delen door 1.000.000
    adiv <- 1000
    gdiv <- 1000000
    df$mnd1_aant <- df[, "vsmd1_aant"]/adiv
    df$mnd2_aant <- df[, "vsmd2_aant"]/adiv
    df$mnd3_aant <- df[, "vsmd3_aant"]/adiv
    df$mnd4_aant <- df[, "vsmd4_aant"]/adiv
    df$mnd1_totg <- df[, "vsmd1_totg"]/gdiv
    df$mnd2_totg <- df[, "vsmd2_totg"]/gdiv
    df$mnd3_totg <- df[, "vsmd3_totg"]/gdiv
    df$mnd4_totg <- df[, "vsmd4_totg"]/gdiv

    # vervang na door 0
    df[is.na(df)] <- 0
    
    # vul de geheimhoudings- en de statuskolommen in
    df$mnd1_status <- ''
    df$mnd2_status <- 'P'
    df$mnd3_status <- 'P'
    df$mnd4_status <- 'P'
    df$mnd1_geheim <- ''
    df$mnd2_geheim <- ''
    df$mnd3_geheim <- ''
    df$mnd4_geheim <- ''
    df['Eenden', "mnd1_geheim"] <- 'C'
    df['Eenden', "mnd2_geheim"] <- 'C'
    df['Eenden', "mnd3_geheim"] <- 'C'
    df['Eenden', "mnd4_geheim"] <- 'C'
    df['Pluimvee totaal', "mnd1_geheim"] <- 'C'
    df['Pluimvee totaal', "mnd2_geheim"] <- 'C'
    df['Pluimvee totaal', "mnd3_geheim"] <- 'C'
    df['Pluimvee totaal', "mnd4_geheim"] <- 'C'
    df['Kalkoenen', "mnd1_geheim"] <- 'C'
    df['Kalkoenen', "mnd2_geheim"] <- 'C'
    df['Kalkoenen', "mnd3_geheim"] <- 'C'
    df['Kalkoenen', "mnd4_geheim"] <- 'C'
    # nieuwe saio verwacht kolom obs_comment
    df$mnd1_comment <- ''
    df$mnd2_comment <- ''
    df$mnd3_comment <- ''
    df$mnd4_comment <- ''

    df$dier_eurostat <- apply(df[c("dcat")], 1, function(x) hercodeer_dier_eurostat(x))
    rownames(df) <- df$dier_eurostat
    
    return(df)
}


#' Geef het getal weer voor Eurostat
#' @param getal
#' @return als tekst geformatteerd getal
#' @example 
#' > format_getal_eurostat(2343131332.09073)
#' [1] "2343131332.091"
format_getal_eurostat <- function(getal){
    #return(formatC(getal, format="f", big.mark=",", digits=3))
    return(formatC(getal, format="f", big.mark="", digits=3))
}



selecteer_frame_eurostat <- function(df) {
    
    #rijen en kolommen
    rijen <- c("BovineAnimals",
               "Bullocks","Bulls","Cows","Heifers","Calves",
               "YoungCattle","Pigs","Sheep.total","Sheep.lambs", "Sheep.other",
               "Goats", "Equidae","TotalPoultry","Chickens","Ducks", "Turkeys")
    kolommen_tota <- c("mnd1_aant","mnd1_status","mnd1_geheim","mnd1_comment",
                       "mnd2_aant","mnd2_status","mnd2_geheim","mnd2_comment",
                       "mnd3_aant","mnd3_status","mnd3_geheim","mnd3_comment",
                       "mnd4_aant","mnd4_status","mnd4_geheim","mnd4_comment")
    kolommen_totg <- c("mnd1_totg","mnd1_status","mnd1_geheim","mnd1_comment",
                       "mnd2_totg","mnd2_status","mnd2_geheim","mnd2_comment",
                       "mnd3_totg","mnd3_status","mnd3_geheim","mnd3_comment",
                       "mnd4_totg","mnd4_status","mnd4_geheim","mnd4_comment")
    kolommen_out <- c("mnd1","mnd1_status","mnd1_geheim","mnd1_comment",
                      "mnd2","mnd2_status","mnd2_geheim","mnd2_comment",
                      "mnd3","mnd3_status","mnd3_geheim","mnd3_comment",
                      "mnd4","mnd4_status","mnd4_geheim","mnd4_comment")
    
    df_heads <- df[rijen, kolommen_tota]
    colnames(df_heads) <- kolommen_out
    rownames(df_heads) <- paste0(rownames(df_heads), ".1000heads")
    df_tons <- df[rijen, kolommen_totg]
    colnames(df_tons) <- kolommen_out
    rownames(df_tons) <- paste0(rownames(df_tons), ".1000tons")
    df_out_1 <- rbind(df_heads, df_tons)
    df_out_2 <- data.frame("dierkolom"=rownames(df_out_1))
    df_out_3 <- cbind(df_out_2, df_out_1)
    
    return(df_out_3)
}

#' Maak het outputbestand voor eurostat
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
maak_output_eurostat <- function(df,
                                 jrmnd,
                                 maanden_vorige_vm,
                                 maanden_huidige_vm,
                                 outputbestand=App$out_eurostat){

  logdebug(msg=paste("Systeem start maken outputbestand Eurostat",
                     outputbestand))

  withProgress(message="", value=0, {
    mld <- "Verzamelen data"
    setProgress(message=mld, value=0.25)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data <- leid_af_df_out_eurostat(df=df, jrmnd=jrmnd)

    mld <- "Wegschrijven naar Excel"
    setProgress(message=mld, value=0.50)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_out <-selecteer_frame_eurostat(df_data)
    stel_samen_out_eurostat(data=df_out, bestand=outputbestand)
    
    setProgress(message="Gereed", value=1.0)
  })

  # verifieer of outputbestand gemaakt is
  if (file.exists(outputbestand)) {
    txt <- paste("bestand", basename(outputbestand), "is gemaakt")
  } else {
    txt <- paste("bestand", basename(outputbestand), "is niet gemaakt")
  }

  logdebug(msg="Systeem is gereed met maken outputbestand eurostat")
  return(txt)
}
