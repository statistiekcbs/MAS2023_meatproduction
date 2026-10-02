#' Naam        : outputbestand_DSC_roodvlees.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om het outputbestand roodvlees voor DSC te maken.


#' Hercodeer dsrt van roodvlees voor DSC
#'
#' De intern gebruikte diersoort omschrijving dsrt wordt omgecodeerd naar
#' de DSC-benaming.
#'
#' @param dsrt string intern gebruikte diersoort omschrijving
#' @return string met DSC-benaming
#' @example
#' > hercodeer_rdvl_dsrt_dsc(ds="Kalveren")
#' "Kalf"
hercodeer_rdvl_dsrt_dsc <- function(ds) {
  if (ds == "Kalveren") {
    return ("Kalf")
  } else if (ds == "Volwassen runderen") {
    return ("Rund")
  } else if (ds == "Lammeren") {
    return ("Schaap jonger dan 1 jaar")
  } else if (ds == "Volwassen schapen") {
    return ("Schaap ouder dan 1 jaar")
  } else if (ds == "Varkens") {
    return ("Varken")
  } else if (ds == "Eenhoevige dieren") {
    return ("Eenhoevig dier")
  } else if (ds == "Geiten") {
    return ("Geit")
  }
}


#' Maak het outputbestand roodvlees voor DSC
#'
#' @param df dataframe met weg te schrijven data
#' @param outputbestand string met volledig pad incl naam van outputbestand
#' @return txt string met melding of maken outputbestand gelukt is of niet
maak_output_dsc_rdvl <- function(df, outputbestand,
                                 drivernaam=App$dbdrivernaam,
                                 servernaam=App$dbservernaam,
                                 databasenaam=App$databasenaam,
                                 schema=App$databaseschema,
                                 tabel_dsrt_en_dcat=App$tbl_dsrt_en_dcat) {
  logdebug(msg="Systeem is start met maken outputbestand roodvlees DSC")
  
  ########## Tijdelijke code; idealiter DSC op laagste categorie niveau ########
  
  ## TIJDELIJK: verdeel "Onbekend" om de oude DSC rund layout te behouden
  if ("Onbekend" %in% df$dsrt) {
    vec_runderen <- c('Stieren', 'Koeien', 'Vaarzen',
                      'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd')
    
    # filter de runderen (en de runderen + onbekend)
    df_runderen <- dplyr::filter(df, dsrt %in% vec_runderen) 
    df_onbekend <- dplyr::filter(df, dsrt == 'Onbekend')
    
    df_niet_runderen <- df |>
      dplyr::anti_join(dplyr::bind_rows(df_runderen, df_onbekend), 
                       by = join_by(dsrt)
      )
    df_verd <- df_runderen |>
      # berekend het totaal aantal runderen
      dplyr::summarise(
        .by = c(slnm, wrkp),
        tota_rund_gaaf = sum(aant_gaaf, na.rm = TRUE)
      ) |> 
      dplyr::left_join(
        # bereken het totaal aantal Onbekend
        df_onbekend |> dplyr::summarise(
          .by = c(slnm, wrkp),
          tota_onbekend_gaaf = sum(aant_gaaf, na.rm = TRUE)),
        by = c("slnm", "wrkp")
      ) |>
      # bereken de vermenigvuldigingsfactor
      dplyr::mutate(
        verd_factor_gaaf = 1 + (tota_onbekend_gaaf / tota_rund_gaaf)
      ) |>
      # vervang NA in verd_factor kolommen (door afwezigheid Onbekend)
      tidyr::replace_na(
        list(verd_factor_gaaf = 1)
      ) |>
      dplyr::select(slnm, wrkp, verd_factor_gaaf)
    
    # maak de df om mee verder te werken
    df_rdvl <- df_runderen |>
      # verdeel "Onbekend" over de runderen
      dplyr::left_join(df_verd, by=c("slnm", "wrkp")) |>
      dplyr::mutate(
        aant_gaaf = round(aant_gaaf * verd_factor_gaaf, 0)
      ) |>
      # voeg de runderen bij de rest van het roodvlees
      dplyr::bind_rows(df_niet_runderen) |>
      dplyr::select(-verd_factor_gaaf)
  } else {df_rdvl <- df}
  
  ## TIJDELIJK: agreggeer de RVO rundcategorieën DSC layout te behouden
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = drivernaam,
                        Server = servernaam,
                        Database = databasenaam
  )
  df <- con |> 
    dplyr::tbl(dbplyr::in_schema(schema, tabel_dsrt_en_dcat)) |> 
    dplyr::collect() |>
    dplyr::right_join(df_rdvl, by = c("dcat"="dsrt")) |>
    dplyr::summarise(
      .by = c(slnm, wrkp, dsrt),
      aant_gaaf = sum(aant_gaaf)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(slnm, dsrt)
  
  ########### Einde tijdelijke code #######################
  
  # hercodeer dsrt naar DSC-benaming
  df$dsrt_dsc <- apply(df[c("dsrt")], 1, function(x) hercodeer_rdvl_dsrt_dsc(x))

  # selecteer kolommen en zet in juiste volgorde
  df <- df[c("slnm", "dsrt_dsc", "aant_gaaf")]
  
  # browser()
  # maak df uniek (ontdubbelen MDAK n.a.v. dubbelen in dsc-bestand roodvlees)
  df <- unique(df)

  # output moet beginnen met BOM om utf-8 te forceren in editor  
  BOM <- charToRaw('\xEF\xBB\xBF')
  con <- file(description=outputbestand, open="wb")
  writeBin(BOM, con, endian="little")
  close(con)

  # schrijf data weg naar outputbestand
  write.table(x=df, file=outputbestand, append=TRUE, sep=";", quote=c(1),
              row.names=FALSE, col.names=FALSE, fileEncoding="utf-8")

  # verifieer of outputbestand gemaakt is
  if (file.exists(outputbestand)) {
    txt <- paste("bestand", basename(outputbestand), "is gemaakt")
  } else {
    txt <- paste("bestand", basename(outputbestand), "is niet gemaakt")
  }

  logdebug(msg="Systeem is gereed met maken outputbestand roodvlees DSC")
  return(txt)
}
