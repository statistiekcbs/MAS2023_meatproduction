#' Naam        : outputbestand_Statline.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om het outputbestand voor Statline te maken


#' Hercodeer dieraanduidingen voor Statline
#'
#' De intern gebruikte diersoort omschrijving wordt omgecodeerd naar
#' de Statline-code
#'
#' @param dc string intern gebruikte dier omschrijving
#' @return string met Statline-code
#' @example
#' > hercodeer_dier_Statline(dc="Stieren")
#' "05"
codeer_dier_Statline <- function(dc) {
    
    if (dc == "Runderen totaal"){return("01")}
    else if (dc == "Volwassen runderen"){return("02")}
    else if (dc == "Koeien"){return("03")}
    else if (dc == "Vaarzen"){return("04")}
    else if (dc == "Stieren"){return("05")}
    else if (dc == "Kalveren totaal"){return("06")}
    else if (dc == "Kalveren 0-8 mnd"){return("07")}
    else if (dc == "Kalveren 8-12 mnd"){return("08")}
    else if (dc == "Varkens"){return("09")}
    else if (dc == "Schapen totaal"){return("10")}
    else if (dc == "Lammeren"){return("11")}
    else if (dc == "Geiten"){return("12")}
    else if (dc == "Eenhoevige dieren"){return("13")}
    else if (dc == "Vleeskuikens"){return("15")}
    else if (dc == "Overige kippen"){return("16")}
    else if (dc == "Kalkoenen"){return("17")}
    else if (dc == "Eenden"){return("18")}
    else if (dc == "Overig pluimvee"){return("19")}
    else {return(dc)}
}


#' benoem dieren voor Statline
#'
#' De intern gebruikte diersoort omschrijving wordt omgecodeerd naar
#' de Statline-benaming.
#'
#' @param dc string intern gebruikte dier omschrijving
#' @return string met Statline-benaming
#' @example
#' > hercodeer_dier_Statline(dc="Stieren")
#' "stier"
benoem_dier_Statline <- function(dc) {
    
    if (dc == "Runderen totaal"){return("Totaal runderen")}
    else if (dc == "Volwassen runderen"){return("tot volwassen r")}
    else if (dc == "Koeien"){return(str_pad("koe", 15, "right"))}
    else if (dc == "Vaarzen"){return(str_pad("vaars", 15, "right"))}
    else if (dc == "Stieren"){return(str_pad("stier", 15, "right"))}
    else if (dc == "Kalveren totaal"){return(str_pad("tot kalveren", 15, "right"))}
    else if (dc == "Kalveren 0-8 mnd"){return(str_pad("kalveren jonger", 15, "right"))}
    else if (dc == "Kalveren 8-12 mnd"){return(str_pad("kalveren van 9", 15, "right"))}
    else if (dc == "Varkens"){return(str_pad("varkens", 15, "right"))}
    else if (dc == "Schapen totaal"){return(str_pad("schapen incl la", 15, "right"))}
    else if (dc == "Lammeren"){return(str_pad("schapenlammeren", 15, "right"))}
    else if (dc == "Geiten"){return(str_pad("geiten (incl la", 15, "right"))}
    else if (dc == "Eenhoevige dieren"){return(str_pad("eenhoevigen", 15, "right"))}
    else if (dc == "Vleeskuikens"){return(str_pad("vleeskuikens", 15, "right"))}
    else if (dc == "Overige kippen"){return(str_pad("overige kippen", 15, "right"))}
    else if (dc == "Kalkoenen"){return(str_pad("kalkoenen", 15, "right"))}
    else if (dc == "Eenden"){return(str_pad("eenden", 15, "right"))}
    else if (dc == "Overig pluimvee"){return(str_pad("overig pluimvee", 15, "right"))}
    else {return(dc)}
}


#' Geef het aantal weer voor Statline
#' @param getal
#' @return als tekst geformatteerd getal
#' @example 
#' > format_aantal_statline(2343131332.09073)
#' [1] "         2343131332,1"
format_aantal_statline <- function(getal){

    str_getal_0 <- formatC(getal, format="f", big.mark="", decimal.mark=",", digits=1)
    str_getal <- str_pad(str_getal_0, 21, "left")

    return(str_getal)
}


#' Geef het gewicht weer voor Statline
#' @param getal
#' @return als tekst geformatteerd getal
#' @example 
#' > format_gewicht_statline(2343131332.09073)
#' [1] "     2343131332"
format_gewicht_statline <- function(getal){
    
    str_getal_0 <- formatC(formatC(getal, format="f", big.mark="", digits=0))
    str_getal <- str_pad(str_getal_0, 15, "left")
    
    return(str_getal)
}


#' haal dataframe voor jaarcijfers op uit database
#' 
#' @param jrmnd list met verslagmaanden, verwerkingsmaand en vorige
#'              verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param schema string met databaseschema
#' @param vlp_tabel string met statbasetabelnaam met voorlopige cijfers
#' @param def_tabel string met statbasetabelnaam met definitieve cijfers
#' @return df_jaarcijfers dataframe met jaartotalen obv voorlopige en
#'                        definitieve cijfers
haal_op_df_jaarcijfer_db <- function(jrmnd, 
                                     schema=App$databaseschema, 
                                     vlp_tabel=App$tbl_statbase_dcat,
                                     def_tabel=App$tbl_statbase_dcat_def){
    
    huidige_verwerkingsmaand <- jrmnd$vwmd
    vlp_ondergrens <- jrmnd$mnd1
    def_ondergrens <- paste0(substr(jrmnd$mnd1,1,4),'01')
    def_bovengrens <- jrmnd$mnd1
    
    # bovengrens voor voorlopige cijfers wordt bepaald obv van maand4
    maand <- substr(jrmnd$mnd4, 5, 6)
    vlp_bovengrens <- ''
    if (maand == '12')
    {vlp_bovengrens <- jrmnd$mnd4}
    else if (maand=='01')
    {vlp_bovengrens <- jrmnd$mnd3}
    else if (maand=='02')
    {vlp_bovengrens <- jrmnd$mnd2}
    else if (maand=='03')
    {vlp_bovengrens <- jrmnd$mnd1}
    
    sqlstr <- paste0("select dcat, sum(tota_gaaf) as aant, sum(totg) as totg from ",
                     "(select dcat as dcat, tota_gaaf, totg from ", 
                     schema, ".", vlp_tabel, 
                     " where vwmd='", huidige_verwerkingsmaand, 
                     "' and vsmd>='", vlp_ondergrens, 
                     "' and vsmd<='", vlp_bovengrens,
                     "' union all ",
                     "select dcat as dcat, tota_gaaf, totg ",
                     " from ", schema, ".", def_tabel, 
                     " where vsmd>='", def_ondergrens, 
                     "' and vsmd<'", def_bovengrens, "') x ",
                     "group by dcat")
    
    df_jaarcijfers <- dat_lees_db(sqlstr)
    df_jaarcijfers[is.na(df_jaarcijfers)] <- 0
    
    if (nrow(df_jaarcijfers) > 0) {
        colnames(df_jaarcijfers) <- c("dcat","aant","totg")
    } else {
        logdebug(msg="Geen gegevens gevonden")
        # als query niets teruggeeft, geef leeg dataframe terug
        df_jaarcijfers <- data.frame("dcat"=character(), "aant"=character(),
                                     "totg"=character(),
                                     stringsAsFactors=FALSE)
    }
    
    sqlstr_bio <- paste0("select dcat, sum(tota_gaaf) as aant, sum(totg) as totg from ",
                     "(select dcat as dcat, tota_gaaf, totg from ", 
                     schema, ".", vlp_tabel, 
                     " where vwmd='", huidige_verwerkingsmaand, 
                     "' and vsmd>='", vlp_ondergrens, 
                     "' and vsmd<='", vlp_bovengrens,
                     "' and is_biologisch = 1",
                     " union all ",
                     "select dcat as dcat, tota_gaaf, totg ",
                     " from ", schema, ".", def_tabel, 
                     " where vsmd>='", def_ondergrens, 
                     "' and vsmd<'", def_bovengrens,
                     "' and is_biologisch = 1",") x ",
                     "group by dcat")
    
    df_jaarcijfers_bio <- dat_lees_db(sqlstr_bio)
    df_jaarcijfers_bio[is.na(df_jaarcijfers_bio)] <- 0
    
    if (nrow(df_jaarcijfers_bio) > 0) {
      colnames(df_jaarcijfers_bio) <- c("dcat","aant","totg")
    } else {
      logdebug(msg="Geen gegevens gevonden")
      # als query niets teruggeeft, geef leeg dataframe terug
      df_jaarcijfers_bio <- data.frame("dcat"=character(), "aant"=character(),
                                   "totg"=character(),
                                   stringsAsFactors=FALSE)
    }
    
    return(list(
      df_jaarcijfers = df_jaarcijfers,
      df_jaarcijfers_bio = df_jaarcijfers_bio))
}


#' leid de aggregaten voor het jaarcijfer af
#' 
#' deze code lijkt een verdubbeling van de functie oph_leid_aggregaten_af
#' in ophalen_outputdata.R, maar wijkt af omdat de structuur van het dataframe
#' niet overeenkomt
#' 
#' @param df dataframe met totalen voor diercategorieen
#' @return df_nieuw dataframe met jaartotalen, incl aggregaten
#' @example 
#' > leid_aggregaten_jaarcijfer_af(df)
leid_aggregaten_jaarcijfer_af <- function(df){
    df[, 2:3] <- sapply(df[, 2:3], as.numeric)
    
    # kalveren totaal
    df_kalveren_totaal <- as.data.frame(t(colSums(df[df$dcat == 'Kalveren 0-8 mnd' | 
                                                         df$dcat == 'Kalveren 8-12 mnd', 
                                                     2:ncol(df)], na.rm=TRUE)))
    df_kalveren_totaal$dcat <- 'Kalveren totaal'
    rownames(df_kalveren_totaal) <- df_kalveren_totaal$dcat
    
    # schapen totaal
    df_schapen_totaal <- as.data.frame(t(colSums(df[df$dcat == 'Lammeren' | 
                                                        df$dcat == 'Volwassen schapen', 
                                                    2:ncol(df)], na.rm=TRUE)))
    df_schapen_totaal$dcat <- 'Schapen totaal'
    rownames(df_schapen_totaal) <- df_schapen_totaal$dcat
    
    # overig pluimvee
    df_overig_pluimvee <- as.data.frame(t(colSums(subset(df, dcat %in% c('Duiven', 'Fazanten', 'Ganzen', 
                                                                         'Parelhoenders', 'Patrijzen', 
                                                                         'Struisvogels'))[2:ncol(df)], na.rm=TRUE)))
    df_overig_pluimvee$dcat <- 'Overig pluimvee'
    rownames(df_overig_pluimvee) <- df_overig_pluimvee$dcat
    
    # volwassen runderen
    df_volwassen_runderen <- as.data.frame(t(colSums(subset(df, dcat %in% c('Stieren', 'Koeien', 
                                                                            'Vaarzen'))[2:ncol(df)], na.rm=TRUE)))
    df_volwassen_runderen$dcat <- 'Volwassen runderen'
    rownames(df_volwassen_runderen) <- df_volwassen_runderen$dcat
    
    # runderen totaal
    df_runderen_totaal <- as.data.frame(t(colSums(subset(df, 
                                                         dcat %in% c('Stieren', 'Koeien', 'Vaarzen', 
                                                                     'Kalveren 0-8 mnd', 
                                                                     'Kalveren 8-12 mnd'))[2:ncol(df)], na.rm=TRUE)))
    df_runderen_totaal$dcat <- 'Runderen totaal'
    rownames(df_runderen_totaal) <- df_runderen_totaal$dcat
    
    # voeg de delen samen
    lijstje_dataframes <- list(df, 
                               df_kalveren_totaal, df_volwassen_runderen, df_runderen_totaal, 
                               df_schapen_totaal, df_overig_pluimvee)
    df_nieuw <- do.call(rbind, lijstje_dataframes)
    rownames(df_nieuw) <- df_nieuw$dcat
    
    return(df_nieuw)
}


#' leid de jaartotalen af
#' 
#' @param jrmnd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param rijen vector met te selecteren diersoorten/-categorieen
#' @return df dataframe met jaartotalen, incl aggregaten
#' @example 
#' > leid_af_df_jaarcijfer(jrmnd, rijen)
#'                           diernaam       diercode         aant         totg    mnd
#'       Runderen totaal    Totaal runderen       01   2151343.00  437016573.6 201700
#'       Volwassen runderen tot volwassen r       02    648525.00  201550091.1 201700
#'       Koeien             koe                   03    561453.90  165673890.5 201700
#'       Vaarzen            vaars                 04     15110.63    3371976.5 201700
#'       Stieren            stier                 05     71960.47   32504224.1 201700
leid_af_df_jaarcijfer <- function(jrmnd, 
                                  rijen){
    
    # haal gegevens op uit database
    df_database <- haal_op_df_jaarcijfer_db(jrmnd)
    
    # leid aggregaten af
    df_met_agg <- leid_aggregaten_jaarcijfer_af(df_database$df_jaarcijfers)
    df_met_agg_bio <- leid_aggregaten_jaarcijfer_af(df_database$df_jaarcijfers_bio)
    
    # hercodeer dsrt naar Statlinecode en -benaming; voeg jaarcode voor in kolom mnd
    df_met_agg$diercode <- apply(df_met_agg[c("dcat")], 1, function(x) codeer_dier_Statline(x))
    df_met_agg$diernaam <- apply(df_met_agg[c("dcat")], 1, function(x) benoem_dier_Statline(x))
    df_met_agg$mnd <- paste0(substr(jrmnd$mnd1,1,4),'00')
    
    # zet het dataframe in het juiste format
    kolomnamen <- c("diernaam","diercode","aant","totg","mnd")
    df <- df_met_agg[rijen,kolomnamen]
    
    # nu ook voor biologisch hercoderen
    # hercodeer dsrt naar Statlinecode en -benaming; voeg jaarcode voor in kolom mnd
    df_met_agg_bio$diercode <- apply(df_met_agg_bio[c("dcat")], 1, function(x) codeer_dier_Statline(x))
    df_met_agg_bio$diernaam <- apply(df_met_agg_bio[c("dcat")], 1, function(x) benoem_dier_Statline(x))
    df_met_agg_bio$mnd <- paste0(substr(jrmnd$mnd1,1,4),'00')
    
    # zet het dataframe in het juiste format
    kolomnamen <- c("diernaam","diercode","aant","totg","mnd")
    df_bio <- df_met_agg_bio[rijen,kolomnamen]
    
    return(list(
      df_totaal=df,
      df_bio=df_bio))
}



#' Maak het outputbestand 7123SLAC.dat voor Statline
#'
#' @param df dataframe met weg te schrijven data
#' @param jrmnd  list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden().
#' @param outputbestand string met volledig pad incl naam van outputbestand
#' @param outputbestand_bio string met volledig pad incl naam van biologisch jaarcijferbestand.
#' @return txt string met melding of maken outputbestand gelukt is of niet
maak_output_statline <- function(df, jrmnd, outputbestand, outputbestand_bio=NULL) {
  logdebug(msg="Systeem is start met maken outputbestand voor Statline")
  
  # selecteer de input en zet na op 0    
  rijen <- c("Runderen totaal","Volwassen runderen","Koeien",
             "Vaarzen","Stieren","Kalveren totaal","Kalveren 0-8 mnd","Kalveren 8-12 mnd",
             "Varkens","Schapen totaal","Lammeren","Geiten",
             "Eenhoevige dieren","Vleeskuikens","Overige kippen","Kalkoenen",
             "Eenden","Overig pluimvee")
  df <- df[rijen,]
  df[is.na(df)] <- 0
  
  # hercodeer dsrt naar Statlinecode en -benaming
  df$diercode <- apply(df[c("dcat")], 1, function(x) codeer_dier_Statline(x))
  df$diernaam <- apply(df[c("dcat")], 1, function(x) benoem_dier_Statline(x))

  # herstructureer; zet maanden onder elkaar
  df_mnd1 <- df[, c("diernaam","diercode","vsmd1_aant","vsmd1_totg")]
  df_mnd2 <- df[, c("diernaam","diercode","vsmd2_aant","vsmd2_totg")]
  df_mnd3 <- df[, c("diernaam","diercode","vsmd3_aant","vsmd3_totg")]
  df_mnd4 <- df[, c("diernaam","diercode","vsmd4_aant","vsmd4_totg")]
  df_mnd1$mnd <- jrmnd$mnd1
  df_mnd2$mnd <- jrmnd$mnd2
  df_mnd3$mnd <- jrmnd$mnd3
  df_mnd4$mnd <- jrmnd$mnd4
  kolomnamen <- c("diernaam","diercode","aant","totg","mnd")
  colnames(df_mnd1) <- kolomnamen
  colnames(df_mnd2) <- kolomnamen
  colnames(df_mnd3) <- kolomnamen
  colnames(df_mnd4) <- kolomnamen
  
  # indien nodig wordt het dataframe met jaarcijfers ingevoegd
  maand <- substr(jrmnd$mnd4, 5, 6)
  if (maand %in% list('12','01','02','03')) {
      df_jaar <- leid_af_df_jaarcijfer(jrmnd, rijen)
      df_tot <- rbind(df_jaar$df_totaal, df_mnd1, df_mnd2, df_mnd3, df_mnd4)
      df_bio <- df_jaar$df_bio
  }
  else {
      df_tot <- rbind(df_mnd1, df_mnd2, df_mnd3, df_mnd4)    
  }
  
  rownames(df_tot) <- do.call(paste, c(df_tot[c("diercode","mnd")], sep=""))
  
  # deel aantallen en kilo's door 1000
  df_tot$aantal <- df_tot$aant/1000
  df_tot$gewicht <- df_tot$totg/1000
  
  # geeft de getallen het gewenste format
  df_aantallen <- sapply(df_tot[, c("aantal")], format_aantal_statline)
  df_gewichten <- sapply(df_tot[, c("gewicht")], format_gewicht_statline)
  df_tot$aantallen <- df_aantallen
  df_tot$gewichten <- df_gewichten
  
  # maak gewichten leeg voor kalkoenen, code 17
  df_tot$gewichten[df_tot$diercode == "17"] <- str_pad("", 15, "left")

  # selecteer kolommen en zet in juiste volgorde
  df_totaal <- df_tot[c("diernaam","diercode","mnd","aantallen","gewichten")]
  
  logdebug(msg="Klaar met maken df_totaal")
  
  write.table(x=df_totaal, file=outputbestand, sep="", quote=FALSE,
              row.names=FALSE, col.names=FALSE, fileEncoding="utf-8")

  
  # verifieer of outputbestand gemaakt is
  if (file.exists(outputbestand)) {
    txt <- paste("bestand", basename(outputbestand), "is gemaakt")
  } else {
    txt <- paste("bestand", basename(outputbestand), "is niet gemaakt")
  }
  
  if (maand %in% c('12','01','02','03')) {
    logdebug(msg='starten met maken df_bio output.')
    df_bio <- df_bio[c("diernaam","diercode","mnd","aant","totg")]
    write.xlsx(x=df_bio, file=outputbestand_bio, overwrite=TRUE)
    txt <- paste(txt, "en bestand", basename(outputbestand_bio), "is gemaakt.")
  }

  logdebug(msg="Systeem is gereed met maken outputbestand roodvlees DSC")
  
  return(txt)
}
