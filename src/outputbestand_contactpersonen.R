#' Naam        : outputbestand_Eurostat.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om het outputbestand voor contactpersonen te maken. 


#' Maak het outputbestand voor contactpersonen op.
#'
#' @param wb Workbook object containing a worksheet.
#' @return wb Workbook object containing a worksheet.
maak_op_contactpersonen <- function (wb) {
    # eerste rij: tekst dikgedrukt en gecentreerd en merge cellen per groep
    addStyle(wb, sheet=1, style=createStyle(halign='left'),
             rows=1:36, cols=1:13, gridExpand=TRUE, stack=TRUE)

    # stel kolombreedten in
    setColWidths(wb, sheet=1, cols=1, widths=App$breedte_contactpersonen_diersoortcat) # kolom diersoort
    setColWidths(wb, sheet=1, cols=2:13, widths=App$breedte_contactpersonen_cellen) # alle overige kolommen 
    
    # stel weergave voor als tekst geformatteerde numerieke waarden in, lijn rechts uit 
    addStyle(wb, sheet=1, style=createStyle(halign='right'),
             rows=1:36, cols=2:5, gridExpand=TRUE, stack=TRUE)

    # weergave numerieke velden
    addStyle(wb, sheet=1, style=createStyle(numFmt="#,##0.00"),
             rows=2:35, cols=2:5, gridExpand=TRUE, stack=TRUE)

    return(wb)
}


# 
#' Stel het outputbestand voor contactpersonen samen.
#'
#' We schrijven de data in 1x weg. 
#' Vervolgens voegen we de overige opmaak toe. Ten slotte
#' slaan we het Excelbestand op.
#'
#' @param data dataframe met data om weg te schrijven naar excelsheet.
#' @param bestand string met volledig pad naar outputbestand.
#' @return niets.
stel_samen_out_contactpersonen <- function(data, bestand) {

  wb <- createWorkbook()
  addWorksheet(wb, "Contactpersonen")

  out_contactpersonen <- data.frame(data)

  kolomkoppen <- t(c('CASE_LBL', 'Maand.min.3_sum', 'Maand.min.2_sum',
                     'Maand.min.1_sum', 'Maand.min.0_sum'))

  # schrijf data weg 
  writeData(wb, sheet=1, x=kolomkoppen, startRow=1, colNames=F)
  writeData(wb, sheet=1, x=out_contactpersonen, startRow=2, colNames=F)
  
  wb <- maak_op_contactpersonen(wb)
  saveWorkbook(wb, file=bestand, overwrite=TRUE)

  # open het script en sla het opnieuw op om te voorkomen dat het gegenereerde excel-bestand
  # wordt tegengehouden door het filter bij het verzenden naar een extern adres
  script <- file.path(App$srcmap, App$open_en_sluit_excel_script)
  system(paste0('powershell -executionpolicy bypass -file ', script, ' -excelbestand "', bestand, '"'))
}


hercodeer_dier_contactpersonen <- function(dc) {
    
    if (dc == "Ossen en stieren"){return("stierenossen")}
    else if (dc == "Koeien"){return("koeien")}
    else if (dc == "Vaarzen"){return("vaarzen")}
    else if (dc == "Kalveren totaal"){return("Kalf")}
    else if (dc == "Kalveren 0-8 mnd"){return("kalf.tot.8.mnd")}
    else if (dc == "Kalveren 8-12 mnd"){return("kalf.8.tot.12.mnd")}
    else if (dc == "Volwassen runderen"){return("VolwRund")}
    else if (dc == "Lammeren"){return("Schaap_jonger_dan_1_jaar")}
    else if (dc == "Schapen totaal"){return("Schapen")}
    else if (dc == "Geiten"){return("Geit")}
    else if (dc == "Varkens"){return("Varken")}
    else if (dc == "Eenhoevige dieren"){return("Eenhoevig_dier")}
    else if (dc == "Eenden"){return("Eenden")}
    else if (dc == "Kalkoenen"){return("Kalkoenen")}
    else if (dc == "Vleeskuikens"){return("Vleeskuikens")}
    else if (dc == "Overige kippen"){return("OverigeKippen")}
    else if (dc == "Overig pluimvee"){return("OvPluimvee")}
}


#' Verzamel de definitieve en voorlopige gegevens uit de database 
#' stel op basis hiervan een dataframe samen 
#'
#' @param df dataframe met data
#' @return df_tot dataframe met data die op sheet kunnen worden gezet.
#' @example.
#' > leid_af_df_out_contactpersonen(df)
leid_af_df_out_contactpersonen <- function(df){
    
    kolomkoppen <- c("dcat", "vsmd1_aant", "vsmd1_totg", "vsmd2_aant", "vsmd2_totg", 
                     "vsmd3_aant", "vsmd3_totg", "vsmd4_aant", "vsmd4_totg")
    
    rijen <- c("Ossen en stieren","Volwassen runderen","Koeien","Vaarzen",
               "Kalveren totaal","Kalveren 0-8 mnd","Kalveren 8-12 mnd",
               "Varkens","Schapen totaal","Lammeren","Geiten","Eenhoevige dieren",
               "Vleeskuikens","Overige kippen","Kalkoenen","Eenden","Overig pluimvee")
    
    df <- df[rijen,]
    # gewichten delen door 1.000
    gdiv <- 1000
    df$mnd1_aant <- df[, "vsmd1_aant"]
    df$mnd2_aant <- df[, "vsmd2_aant"]
    df$mnd3_aant <- df[, "vsmd3_aant"]
    df$mnd4_aant <- df[, "vsmd4_aant"]
    df$mnd1_totg <- df[, "vsmd1_totg"]/gdiv
    df$mnd2_totg <- df[, "vsmd2_totg"]/gdiv
    df$mnd3_totg <- df[, "vsmd3_totg"]/gdiv
    df$mnd4_totg <- df[, "vsmd4_totg"]/gdiv

    # vervang na door 0
    df[is.na(df)] <- 0
    
    df$dier_contactpersonen <- apply(df[c("dcat")], 1, function(x) hercodeer_dier_contactpersonen(x))
    rownames(df) <- df$dier_contactpersonen
    
    return(df)
}


#' Geef het getal weer voor contactpersonen
#' @param getal
#' @return als tekst geformatteerd getal
#' @example 
#' > format_getal_contactpersonen(2343131332.09073)
#' [1] "2.343.131.332,09"
format_getal_contactpersonen <- function(getal){
    return(formatC(getal, format="f", big.mark=".", decimal.mark=",", digits=2))
}


#' Hercodeer dcat voor gewichten in output contactpersonen
#'
#' De diersoort aanduiding voor aantallen wordt omgecodeerd naar
#' de aanduiding voor gewichten.
#'
#' @param dcat string gebruikte  omschrijving
#' @return string met benaming voor gewichten voor contactpersonen
#' @example
#' > hercodeer_rdvl_dsrt_dsc(ds="Kalveren")
#' "Kalf"
hercodeer_dcat_gewicht <- function(dc) {

    if (dc == "VolwRund") {
        return ("TotGewVolwRund")
    } else if (dc == "stierenossen") {
        return ("TotGewStierenOssen")
    } else if (dc == "koeien") {
        return ("TotGewKoeien")
    } else if (dc == "vaarzen") {
        return ("TotGewVaarzen")
    } else if (dc == "Kalf") {
        return ("TotGewKalf")
    } else if (dc == "kalf.tot.8.mnd") {
        return ("TotGewKalf.tot.8.mnd")
    } else if (dc == "kalf.8.tot.12.mnd") {
        return ("TotGewKalf.8.tot.12.mnd")
    } else if (dc == "Varken") {
        return ("TotGewVarkens")
    } else if (dc == "Schapen") {
        return ("TotGewSchapen")
    } else if (dc == "Schaap_jonger_dan_1_jaar") {
        return ("TotGewSchapen.jong")
    } else if (dc == "Geit") {
        return ("TotGewGeiten")
    } else if (dc == "Eenhoevig_dier") {
        return ("TotGewEenhoevigen")
    } else if (dc == "Vleeskuikens") {
        return ("TotGewVleeskuikens")
    } else if (dc == "OverigeKippen") {
        return ("TotGewOverigeKippen")
    } else if (dc == "Kalkoenen") {
        return ("TotGewKalkoenen")
    } else if (dc == "Eenden") {
        return ("TotGewEenden")
    } else if (dc == "OvPluimvee") {
        return ("TotGewOvPluimvee")
    }
}


#' Selecteer de gewenste rijen en kolommen in de juiste volgorde. Extra controle
#' om ervoor te zorgen dat alle kolommen aanwezig zijn voor het maken van de
#' output.
#'
#'
#' @param df dataframe.
#' @return df dataframe.
#'              dcat mnd1_aant mnd1_totg .. mnd4_aant mnd4_totg  mnd1_geheim .. mnd4_geheim mnd1_status .. mnd4_status
#' Stieren   Stieren     6.521    3011.0 ..  5203.201 2330581.1                                                      P
#' Ossen       Ossen     0           0   ..     0           0                                                        P
selecteer_frame_contactpersonen <- function(df) {
    #rijen en kolommen
    
    rijen <- c("VolwRund","stierenossen","koeien","vaarzen","Kalf","kalf.tot.8.mnd","kalf.8.tot.12.mnd",
               "Varken","Schapen","Schaap_jonger_dan_1_jaar","Geit","Eenhoevig_dier","Vleeskuikens",
               "OverigeKippen","Kalkoenen","Eenden","OvPluimvee")
    kolommen_tota <- c("mnd1_aant","mnd2_aant","mnd3_aant","mnd4_aant")
    kolommen_totg <- c("mnd1_totg","mnd2_totg","mnd3_totg","mnd4_totg")
    kolommen_gnrk <- c("dier_contactpersonen","mnd1","mnd2","mnd3","mnd4")
    kolommen_out <- c("dier_contactpersonen","mnd1","mnd2","mnd3","mnd4")
    
    #df_dieren_aantallen <- as.data.frame(df$dcat)
    df_dieren_aantallen <- df[rijen, c("dier_contactpersonen") ,drop=FALSE]
    df_dieren_gewichten <- as.data.frame(apply(df_dieren_aantallen[rijen, c("dier_contactpersonen"), drop=FALSE], 1, function(x) hercodeer_dcat_gewicht(x)))
    rownames(df_dieren_gewichten) <- df_dieren_gewichten$dcat
    
    # aantallen
    df_heads_0 <- df[rijen, kolommen_tota]
    #df_heads_0 <- sapply(df[rijen, kolommen_tota], format_getal_contactpersonen)
    
    df_heads <- cbind(df_dieren_aantallen,df_heads_0)
    colnames(df_heads) <- kolommen_gnrk
    
    #gewichten
    df_tons_0 <- df[rijen, kolommen_totg]
    #df_tons_0 <- sapply(df[rijen, kolommen_totg], format_getal_contactpersonen)
    df_tons <- cbind(df_dieren_gewichten,df_tons_0)
    colnames(df_tons) <- kolommen_gnrk
    rownames(df_tons) <- df_tons$dcat

    df_out <- rbind(df_heads,df_tons)
    return(df_out)
}


#' Maak het outputbestand voor contactpersonen
#'
#' We schrijven de groepkoppen, de kolomkoppen en de data in 1x weg 
#' Vervolgens voegen we de overige opmaak toe. Ten slotte
#' slaan we het Excelbestand op.
#'
#' @param inputmap map string
#' @param jrmnd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param outputbestand string
#' @return txt string 
#' @examples.
maak_output_contactpersonen <- function(df,
                                        jrmnd,
                                        maanden_vorige_vm,
                                        maanden_huidige_vm,
                                        outputbestand=App$out_contactpersonen){

  logdebug(msg=paste("Systeem start maken outputbestand Contactpersonen",
                     outputbestand))

  withProgress(message="", value=0, {
    mld <- "Verzamelen data"
    setProgress(message=mld, value=0.25)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_data <- leid_af_df_out_contactpersonen(df=df)
    
    mld <- "Wegschrijven naar Excel"
    setProgress(message=mld, value=0.50)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    df_out <-selecteer_frame_contactpersonen(df_data)
    stel_samen_out_contactpersonen(data=df_out, bestand=outputbestand)
    
    setProgress(message="Gereed", value=1.0)
  })

  # verifieer of outputbestand gemaakt is
  if (file.exists(outputbestand)) {
    txt <- paste("bestand", basename(outputbestand), "is gemaakt")
  } else {
    txt <- paste("bestand", basename(outputbestand), "is niet gemaakt")
  }

  logdebug(msg="Systeem is gereed met maken outputbestand contactpersonen")
  return(txt)
}
