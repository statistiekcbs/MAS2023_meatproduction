#' Naam        : ophalen_outputdata.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om een dataframe samen te stellen met alle gegevens
#'               die voor de verschillende afnemers nodig zijn.


#' Leid extra aggregaten af voor de output
#'
#' @param df dataframe met statbasedata 
#' @return df_nieuw dataframe met extra afgeleide aggregaten
#' @examples.
#' > oph_leid_aggregaten_af(df)
#'                              dcat vsmd1_aant vsmd1_totg vsmd2_aant vsmd2_totg vsmd3_aant vsmd3_totg vsmd4_aant vsmd4_totg
#' Stieren                   Stieren   6818.521  3011809.0   5806.246  2617339.8   5103.567  2255632.9   5203.201  2330581.1
#' Ossen                       Ossen      0            0        0            0        0            0        0            0
#' Ossen en stieren Ossen en stieren   6818.521  3011809.0   5806.246  2617339.8   5103.567  2255632.9   5203.201  2330581.1
#' Koeien                     Koeien  53286.620 16021155.2  54077.184 16222073.7  52720.980 15789300.8  43218.655 13062060.4
#' Vaarzen                   Vaarzen   1433.859   316581.7   1428.570   300442.5   1379.453   290515.6   1155.144   249615.1
oph_leid_aggregaten_af <- function(df) {
    
    # ossen
    df_ossen <- data.frame("dcat"="Ossen", 
                           "vsmd1_aant"=0, "vsmd1_totg"=0, 
                           "vsmd2_aant"=0, "vsmd2_totg"=0, 
                           "vsmd3_aant"=0, "vsmd3_totg"=0, 
                           "vsmd4_aant"=0, "vsmd4_totg"=0, 
                           stringsAsFactors=FALSE)
    rownames(df_ossen) <- df_ossen$dcat
    
    # ossen en stieren
    df_ossen_en_stieren <- as.data.frame(df[df$dcat == 'Stieren', 2:ncol(df)])
    df_ossen_en_stieren$dcat <- 'Ossen en stieren'
    rownames(df_ossen_en_stieren) <- df_ossen_en_stieren$dcat
    
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
    
    # kippen totaal
    df_kippen_totaal <- as.data.frame(t(colSums(df[df$dcat == 'Vleeskuikens' | 
                                                   df$dcat == 'Overige kippen', 
                                                   2:ncol(df)], na.rm=TRUE)))
    df_kippen_totaal$dcat <- 'Kippen totaal'
    rownames(df_kippen_totaal) <- df_kippen_totaal$dcat
    
    # overig pluimvee
    df_overig_pluimvee <- as.data.frame(t(colSums(subset(df, dcat %in% c('Duiven', 'Fazanten', 'Ganzen', 
                                                                         'Parelhoenders', 'Patrijzen', 
                                                                         'Struisvogels'))[2:ncol(df)], na.rm=TRUE)))
    df_overig_pluimvee$dcat <- 'Overig pluimvee'
    rownames(df_overig_pluimvee) <- df_overig_pluimvee$dcat
    
    # pluimvee totaal, 2023-12-04 avat kalkoenen toegevoegd
    df_pluimvee_totaal <- as.data.frame(t(colSums(subset(df, dcat %in% c('Eenden', 'Duiven', 'Fazanten',
                                                                         'Ganzen', 'Parelhoenders', 'Patrijzen', 
                                                                         'Struisvogels', 'Overige kippen', "Kalkoenen",    
                                                                         'Vleeskuikens'))[2:ncol(df)], na.rm=TRUE)))
    df_pluimvee_totaal$dcat <- 'Pluimvee totaal'
    rownames(df_pluimvee_totaal) <- df_pluimvee_totaal$dcat
    
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
                               df_ossen, df_ossen_en_stieren, df_kalveren_totaal, df_volwassen_runderen, df_runderen_totaal, 
                               df_schapen_totaal, df_kippen_totaal, df_overig_pluimvee, df_pluimvee_totaal)
    df_nieuw <- do.call(rbind, lijstje_dataframes)
    rownames(df_nieuw) <- df_nieuw$dcat
    
    return(df_nieuw)
}
    


#' Haal de data op uit de database voor de output
#'
#' @param jrmnd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param schema string met databaseschema
#' @param tbl_microbase_rdvl string met tabelnaam microbase roodvlees
#' @param tbl_microbase_wtvl string met tabelnaam microbase witvlees
#' @return stat dataframe met statbasedata als draaitabel
#' @return dsc_rdvl dataframe met rdvl-microbasedata als platte tabel voor dsc
#' @return dsc_wtvl dataframe met wtvl-microbasedata als platte tabel voor dsc
#' @examples.
#' > oph_haal_output_op(jrmnd)$stat
#'            dcat vsmd1_aant vsmd1_totg vsmd2_aant vsmd2_totg vsmd3_aant vsmd3_totg vsmd4_aant vsmd4_totg
#' Stieren Stieren   6818.521  3011809.0   5806.246  2617339.8   5103.567  2255632.9   5203.201  2330581.1
#' Koeien   Koeien  53286.620 16021155.2  54077.184 16222073.7  52720.980 15789300.8  43218.655 13062060.4
#' Vaarzen Vaarzen   1433.859   316581.7   1428.570   300442.5   1379.453   290515.6   1155.144   249615.1
#' > haal_output_op(jrmnd)$dsc_rdvl
#'                             slnm               dsrt aant_gaaf
#' BEDRIJFSNAAM 1          Kalveren      3280
#'        BEDRIJFSNAAM 2             Geiten      1243
#'       BEDRIJFSNAAM 3 Volwassen runderen         2
#' > haal_output_op(jrmnd)$dsc_wtvl
#'                     slnm           dsrt aant_gaaf
#'          BEDRIJFSNAAM 1   Vleeskuikens   1466758
#'            BEDRIJFSNAAM 2 Overige kippen    384588
#' BEDRIJFSNAAM 3   Vleeskuikens   5601016
oph_haal_output_op <- function(jrmnd,
                               schema=App$databaseschema,
                               tbl_microbase_rdvl=App$tbl_microbase_rdvl,
                               tbl_microbase_wtvl=App$tbl_microbase_wtvl) {

  logdebug(msg=paste("Systeem start maken het verzamelen van de outputdata"))

  # bereken eerste maand voor toevoegen van 0 aan dsc-output
  dt_verwerkingsmaand <- as.Date(paste(substr(jrmnd$vwmd,1,4), substr(jrmnd$vwmd,5,6), "01", sep='-'))
  dt_eerstemaand <- dt_verwerkingsmaand %m+% months(-14)
  eerste_maand <- paste0(substr(dt_eerstemaand,1,4), substr(dt_eerstemaand,6,7))
    
  withProgress(message="", value=0, {
    mld <- "Verzamelen data statbase"
    setProgress(message=mld, value=0.25)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    stat <- inl_lees_statbase_output(jrmnd=jrmnd)
    stat_incl_agg <- oph_leid_aggregaten_af(stat)

    mld <- "Verzamelen data microbase roodvlees"
    setProgress(message=mld, value=0.50)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))

    sqlstr <- paste0("select slnm, wrkp, dsrt, aant_gaaf, is_biologisch from ",
                     schema, ".", tbl_microbase_rdvl,
                     " where vwmd = '", jrmnd$vwmd,
                     "' and vsmd = '", jrmnd$mnd1, "'")
    dsc_rdvl_1 <- dat_lees_db(sqlstr) |>
      # tel is_biologisch bij elkaar op
      dplyr::summarise(
        .by = c(slnm, wrkp, dsrt),
        aant_gaaf = sum(aant_gaaf)
      )

    # haal op max_aantal in voorgaande 14 maanden uit tbl_microbase_rdvl_def
    # koppel aan dsc_rdvl, NA vervangen door 0
    sqlstr <- paste0("select slnm, wrkp, dsrt, is_biologisch, max(aant_gaaf) as max_aantal from ", 
                     schema, ".tbl_microbase_rdvl_def",
                     " where vsmd >= '", eerste_maand,
                     "' and vsmd < '", jrmnd$mnd1,
                     "' group by slnm, wrkp, dsrt, is_biologisch") 
    dsc_rdvl_2 <- dat_lees_db(sqlstr) |>
      # tel is_biologisch bij elkaar op
      dplyr::summarise(
        .by = c(slnm, wrkp, dsrt),
        max_aantal = sum(max_aantal)
      ) |>
      # excludeer de NVWA diersoort categorie "Volwassen runderen" en "Kalveren"
      dplyr::filter(!(dsrt %in% c("Volwassen runderen", "Kalveren")))
      
    #dsc_rdvl <- merge(dsc_rdvl_1, dsc_rdvl_2, by=c("slnm", "wrkp", "dsrt"), all=TRUE) 
    dsc_rdvl <- merge(dsc_rdvl_1, dsc_rdvl_2, by=c("wrkp", "dsrt"), all=TRUE) #aanpassing MDAK
    dsc_rdvl$slnm.x <- ifelse(is.na(dsc_rdvl$slnm.x), dsc_rdvl$slnm.y, dsc_rdvl$slnm.x) #aanpassing MDAK
    dsc_rdvl$slnm.y <- NULL #MDAK verwijderen kolom slnm.y
    dsc_rdvl <- plyr::rename(dsc_rdvl, c("slnm.x"="slnm")) #MDAK hernoemen kolom slnm.x
    dsc_rdvl <- dsc_rdvl[c(3,1,2,4,5)] #MDAK volgorde kolommen aanpassen
    dsc_rdvl <- dsc_rdvl[order(dsc_rdvl$slnm),] #MDAK sorteren op slnm
    dsc_rdvl <- subset(dsc_rdvl, select = -max_aantal)
    dsc_rdvl$aant_gaaf[is.na(dsc_rdvl$aant_gaaf)] <- 0
    
    mld <- "Verzamelen data microbase witvlees"
    setProgress(message=mld, value=0.75)
    logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
    sqlstr <- paste0("select slnm, wrkp, dsrt, aant_gaaf from ",
                     schema, ".", tbl_microbase_wtvl,
                     " where vwmd = '", jrmnd$vwmd,
                     "' and vsmd = '", jrmnd$mnd1, "'")
    dsc_wtvl_1 <- dat_lees_db(sqlstr)
    # haal op max_aantal in voorgaande 14 maanden uit tbl_microbase_wtvl_def
    # koppel aan dsc_wtvl, NA vervangen door 0
    sqlstr <- paste0("select slnm, wrkp, dsrt, max(aant_gaaf) as max_aantal from ",
                     schema, ".tbl_microbase_wtvl_def",
                     " where vsmd >= '", eerste_maand,
                     "' and vsmd < '", jrmnd$mnd1,
                     "' group by slnm, wrkp, dsrt")
    dsc_wtvl_2 <- dat_lees_db(sqlstr)
    dsc_wtvl <- merge(dsc_wtvl_1, dsc_wtvl_2, by=c("slnm", "wrkp", "dsrt"), all=TRUE)
    dsc_wtvl <- subset(dsc_wtvl, select = -max_aantal)
    dsc_wtvl$aant_gaaf[is.na(dsc_wtvl$aant_gaaf)] <- 0

    
    setProgress(message="Gereed", value=1.0)
  })

  logdebug(msg="Systeem is gereed met ophalen van de gegevens voor de output")
  return(list(stat=stat_incl_agg, dsc_rdvl=dsc_rdvl, dsc_wtvl=dsc_wtvl))
}
