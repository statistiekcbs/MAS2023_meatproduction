#' Naam        : opslaan.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module bevat functies om de gevalideerde gegevens op te
#'               slaan in de microbase- en statbasetabellen.


#' Voeg dataframe met ruwe aantallen en dataframe met aantallen uit controle en
#' correctiesheets samen.
#'
#' Voor het opslaan van aantallen rood- en witvlees in de microbase dienen de
#' ruwe aantallen te worden samengevoegd met de gaafgemaakte aantallen uit de
#' controle en correctie-sheets.
#'
#' Uitgangspunten:
#' - df_ctcr en df_rw bevatten beide de kolommen "vwmd", "vsmd", "wrkp", "slnm",
#'   "dsrt" en "aant".
#' - Het koppelen van de ruwe en de gaafgemaakte aantallen gebeurt via een outer
#'   join.
#' - het resultaat heeft dezelfde kolommen in dezelfde volgorde als microbase.
#'
#' @param df_ruw dataframe met ruwe aantallen uit levering
#' @param df_ctcr dataframe met aantallen uit controle en correctiesheets
#' @return df dataframe met zowel ctcr-aantallen als ruwe aantallen
#' @example
#' > df_ruw
#'     vwmd   vsmd    wrkp                  slnm              dsrt aant
#' 1 201801 201708 0100082 BEDRIJFSNAAM Eenhoevige dieren    2
#' 3 201801 201708 0100082 BEDRIJFSNAAM    Volwassen rund  555
#' > df_ctcr
#'     vwmd   vsmd    wrkp                  slnm              dsrt aant
#' 1 201801 201708 0100082 BEDRIJFSNAAM Eenhoevige dieren    2
#' 2 201801 201708 0100082 BEDRIJFSNAAM          Kalveren    3
#' 3 201801 201708 0100082 BEDRIJFSNAAM    Volwassen rund  444
#' > ops_voeg_ruw_toe(df_ctcr, df_ruw)
#'     vwmd   vsmd    wrkp                  slnm              dsrt aant_ruw aant
#' 1 201801 201708 0100082 BEDRIJFSNAAM Eenhoevige dieren        2    2
#' 2 201801 201708 0100082 BEDRIJFSNAAM          Kalveren       NA    3
#' 3 201801 201708 0100082 BEDRIJFSNAAM    Volwassen rund      444  555
ops_voeg_ruw_ctcr_samen <- function (df_ruw, df_ctcr) {
    # tijdens het testen bleken de werkpleknummers in het ruwe bestand numeric te zijn en in het gave bestand character
    # allebei dus character maken zodat de merge lukt
    df_ruw$wrkp <- as.character(df_ruw$wrkp)
    df_ctcr$wrkp <- as.character(df_ctcr$wrkp)
    # outer join met df_ruw links, waardoor df_ruw.aant ook links komt te staan
    # (en df_ctcr dus rechts)
    df <- dplyr::full_join(
        df_ruw |>
          dplyr::select(-slnm) |> 
          # aantal ruw komt uit  het input bestand 
          dplyr::rename(aant_ruw = aant),
        # we nemen slachtnamen alleen uit het input bestand 
        df_ctcr,
        by = c("vwmd", "vsmd", "wrkp", "dsrt", "is_biologisch")
      ) |> 
        dplyr::select(
          vwmd, vsmd, wrkp, slnm, dsrt, is_biologisch, aant_ruw, aant
        ) |> 
        dplyr::arrange(slnm)
      
    return(df)
}

#' Schrijf de gevalideerde aantallen roodvlees uit het controle- en
#' correctiebestand samen met de ruwe aantallen uit de levering
#' weg naar de microbase. Schrijf ook de definitief geworden data weg.
#'
#' @param bst_aant_ruw string met pad naar levering aantallen roodvlees
#' @param bst_aant_gaaf string met pad naar ctcr-bestand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @param tabel string met naam microbasetabel waarnaar data wordt weggeschreven
#' @param tabel_def string met naam microbasetabel waarnaar definitieve data
#'                  wordt weggeschreven
#' @return niets
ops_updaten_micro_rdvl <- function (bst_aant_rdvl_nvwa_ruw,
                                    bst_aant_rdvl_rvo_ruw,
                                    bst_aant_gaaf,
                                    maanden_huidige_vm, jrmnd,
                                    tabel=App$tbl_microbase_rdvl,
                                    tabel_def=App$tbl_microbase_rdvl_def) {
    # lees levering en ctcr-bestand in
    # df_ruw  <- inl_lees_input_rdvl(inputbestand=bst_aant_ruw, jrmnd=jrmnd)
  df_ruw <- voeg_rvo_runderen_bij_nvwa(bst_nvwa=bst_aant_rdvl_nvwa_ruw, 
                                            bst_rvo=bst_aant_rdvl_rvo_ruw, jrmnd)
    if(!("is_biologisch" %in% colnames(df_ruw))) {
      df_ruw <- df_ruw |> 
        dplyr::mutate(
          is_biologisch = FALSE
        )
    }
    
    df_gaaf <- inl_lees_ctcr_aant_rdvl(bst_aant_gaaf=bst_aant_gaaf,
                                       maanden_huidige_vm=maanden_huidige_vm,
                                       jrmnd=jrmnd) 
      
    if(!("is_biologisch" %in% colnames(df_gaaf))) {
      df_gaaf <- df_gaaf |> 
        dplyr::mutate(
          is_biologisch = FALSE
        )
    } else {
      df_gaaf <- df_gaaf |> 
        dplyr::mutate(
          is_biologisch = as.logical(is_biologisch)
        )
    }
    # voeg ruwe aantallen en gaafgemaakte aantallen samen
    df_samen <- ops_voeg_ruw_ctcr_samen(df_ruw=df_ruw, df_ctcr=df_gaaf) |> 
      # selecteer kolommen en zet in juiste volgorde
      dplyr::select(
        vwmd, vsmd, wrkp, slnm, dsrt, aant_ruw, aant, is_biologisch
      ) |> 
      dplyr::mutate(
        is_biologisch = as.numeric(is_biologisch)
      )

    # schrijf alle records weg
    dat_schrijf_weg(df=df_samen, tabel=tabel, vwmd=jrmnd$vwmd)

    # log wat gedaan is
    logdebug(paste("Aantallen roodvlees weggeschreven naar", tabel , ":",
                  nrow(df_samen), "records"))

    # schrijf de definitieve geworden cijfers weg
    df_def <- df_samen |> 
      dplyr::filter(vsmd == jrmnd$mnd1) |> 
      # selecteer kolommen en zet in juiste volgorde
      dplyr::select(
        vsmd, wrkp, slnm, dsrt, aant, is_biologisch
      ) |> 
      dplyr::mutate(
        is_biologisch = as.numeric(is_biologisch)
      )
    
    dat_schrijf_weg(df=df_def, tabel=tabel_def, vwmd=jrmnd$vwmd,
                    def=TRUE, vsmd=jrmnd$mnd1)
    # log wat gedaan is
    logdebug(paste("Aantallen roodvlees weggeschreven naar", tabel_def , ":",
                  nrow(df_def), "records"))
}


#' Schrijf de gevalideerde aantallen witvlees uit het controle- en
#' correctiebestand samen met de ruwe aantallen uit de levering
#' weg naar de microbase. Schrijf ook de definitief geworden data weg.
#'
#' @param bst_aant_ruw string met pad naar levering aantallen witvlees
#' @param bst_aant_gaaf string met pad naar ctcr-bestand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @param tabel string met naam microbasetabel waarnaar data wordt weggeschreven
#' @param tabel_def string met naam microbasetabel waarnaar definitieve data
#'                  wordt weggeschreven
#' @return niets
ops_updaten_micro_wtvl <- function (bst_aant_ruw, bst_aant_gaaf,
                                    maanden_huidige_vm, jrmnd,
                                    tabel=App$tbl_microbase_wtvl,
                                    tabel_def=App$tbl_microbase_wtvl_def) {
  # lees levering en ctcr-bestand in
    df_ruw  <- inl_lees_input_wtvl(inputbestand=bst_aant_ruw, jrmnd=jrmnd)
    if(!("is_biologisch" %in% colnames(df_ruw))) {
      df_ruw <- df_ruw |> 
        dplyr::mutate(
          is_biologisch = FALSE
        )
    }
    df_gaaf <- inl_lees_ctcr_aant_wtvl(bst_aant_gaaf=bst_aant_gaaf,
                                       maanden_huidige_vm=maanden_huidige_vm,
                                       jrmnd=jrmnd)
    
    if(!("is_biologisch" %in% colnames(df_gaaf))) {
      df_gaaf <- df_gaaf |> 
        dplyr::mutate(
          is_biologisch = FALSE
        )
    } else {
      df_gaaf <- df_gaaf |> 
        dplyr::mutate(
          is_biologisch = as.logical(is_biologisch)
        )
    }

    # voeg ruwe aantallen en gaafgemaakte aantallen samen
    df_samen <- ops_voeg_ruw_ctcr_samen(df_ruw=df_ruw, df_ctcr=df_gaaf)  |> 
      # selecteer kolommen en zet in juiste volgorde
      dplyr::select(
        vwmd, vsmd, wrkp, slnm, dsrt, aant_ruw, aant, is_biologisch
      ) |> 
      dplyr::mutate(
        is_biologisch = as.numeric(is_biologisch)
      )

    # schrijf alle records weg
    dat_schrijf_weg(df=df_samen, tabel=tabel, vwmd=jrmnd$vwmd)

    # log wat gedaan is
    logdebug(paste("Aantallen witvlees weggeschreven naar", tabel , ":",
                  nrow(df_samen), "records"))

    # schrijf de definitieve geworden cijfers weg
    df_def <- df_samen |> 
      dplyr::filter(vsmd == jrmnd$mnd1) |> 
      # selecteer kolommen en zet in juiste volgorde
      dplyr::select(
        vsmd, wrkp, slnm, dsrt, aant, is_biologisch
      )  |> 
      dplyr::mutate(
        is_biologisch = as.numeric(is_biologisch)
      )
    
    dat_schrijf_weg(df=df_def, tabel=tabel_def, vwmd=jrmnd$vwmd,
                    def=TRUE, vsmd=jrmnd$mnd1)

    # log wat gedaan is
    logdebug(paste("Aantallen witvlees weggeschreven naar", tabel_def , ":",
                  nrow(df_def), "records"))
}


#' Schrijf de gevalideerde aantallenverdelingen uit het controle- en
#' correctiebestand weg naar de microbase. Schrijf ook de definitief geworden
#' data weg.
#'
#' @param bst_gaaf string met pad naar ctcr-bestand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @param tabel string met naam microbasetabel waarnaar data wordt weggeschreven
#' @param tabel_def string met naam microbasetabel waarnaar definitieve data
#'                  wordt weggeschreven
#' @return niets
ops_updaten_micro_verd <- function(bst_gaaf, maanden_huidige_vm,
                                   jrmnd, tabel=App$tbl_microbase_verd,
                                    tabel_def=App$tbl_microbase_verd_def) {
    # lees ctcr-bestand in
    df <- inl_lees_ctcr_verd(bst_gaaf=bst_gaaf,
                             maanden_huidige_vm=maanden_huidige_vm,
                             jrmnd=jrmnd)

    # schrijf alle records weg
    dat_schrijf_weg(df=df, tabel=tabel, vwmd=jrmnd$vwmd)

    # log wat gedaan is
    logdebug(paste("Aantallenverdelingen weggeschreven naar", tabel , ":",
                  nrow(df), "records"))

    # schrijf de definitieve geworden cijfers weg
    df_def <- df[ df$vsmd == jrmnd$mnd1, ]
    df_def <- df_def[c("vsmd", "dsrt",  # selecteer kolommen en
                       "dcat", "verd")] # zet in juiste volgorde
    dat_schrijf_weg(df=df_def, tabel=tabel_def, vwmd=jrmnd$vwmd,
                    def=TRUE, vsmd=jrmnd$mnd1)

    # log wat gedaan is
    logdebug(paste("Aantallenverdelingen weggeschreven naar", tabel_def , ":",
                  nrow(df_def), "records"))
}


#' Schrijf de gevalideerde gemiddelde gewichten uit het controle- en
#' correctiebestand weg naar de microbase. Schrijf ook de definitief geworden
#' data weg.
#'
#' @param bst_gaaf string met pad naar ctcr-bestand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @param tabel string met naam microbasetabel waarnaar data wordt weggeschreven
#' @param tabel_def string met naam microbasetabel waarnaar definitieve data
#'                  wordt weggeschreven
#' @return niets
ops_updaten_micro_gemg <- function(bst_gaaf, maanden_huidige_vm,
                                   jrmnd, tabel=App$tbl_microbase_gemg,
                                   tabel_def=App$tbl_microbase_gemg_def) {
    # lees ctcr-bestand in
    df <- inl_lees_ctcr_gemg(bst_gaaf=bst_gaaf,
                             maanden_huidige_vm=maanden_huidige_vm,
                             jrmnd=jrmnd)

    # schrijf alle records weg
    dat_schrijf_weg(df=df, tabel=tabel, vwmd=jrmnd$vwmd)

    # log wat gedaan is
    logdebug(paste("Gemiddelde gewichten weggeschreven naar", tabel , ":",
                  nrow(df), "records"))

    # schrijf de definitieve geworden cijfers weg
    df_def <- df[ df$vsmd == jrmnd$mnd1, ]
    df_def <- df_def[c("vsmd", "dcat", "gemg")] # selecteer kolommen en
                                                # zet in juiste volgorde
    dat_schrijf_weg(df=df_def, tabel=tabel_def, vwmd=jrmnd$vwmd,
                    def=TRUE, vsmd=jrmnd$mnd1)

    # log wat gedaan is
    logdebug(paste("Gemiddelde gewichten weggeschreven naar", tabel_def , ":",
                  nrow(df_def), "records"))
}


#' Haal de aantallen roodvlees en witvlees geaggregeerd op diersoort (dsrt) uit
#' de microbase op en schrijf de aggregaten weg naar statbase. Schrijf ook de
#' definitief geworden data weg.
#'
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @param tabel_micro_rdvl string met naam microbasetabel roodvlees
#' @param tabel_micro_wtvl string met naam microbasetabel witvlees
#' @param tabel_stat string met naam statbasetabel op dsrt-niveau
#' @param tabel_def string met naam microbasetabel waarnaar definitieve data
#'                  wordt weggeschreven
#' @param schema string met schema
#' @return niets
ops_updaten_stat_dsrt <- function(jrmnd,
                                  tabel_micro_rdvl=App$tbl_microbase_rdvl,
                                  tabel_micro_wtvl=App$tbl_microbase_wtvl,
                                  tabel_stat=App$tbl_statbase_dsrt,
                                  tabel_stat_def=App$tbl_statbase_dsrt_def,
                                  tabel_dsrt_en_dcat = App$tbl_dsrt_en_dcat,
                                  drivernaam=App$dbdrivernaam,
                                  servernaam=App$dbservernaam,
                                  databasenaam=App$databasenaam,
                                  schema=App$databaseschema) {
  
    con <- DBI::dbConnect(odbc::odbc(),
                          Driver = drivernaam,
                          Server = servernaam,
                          Database = databasenaam
    )
    vwmd_waarde <- jrmnd$vwmd
    
    # We krijgen eerst een tabel met alle mogelijke combinaties tussen vwmd,
    # vsmd en dsrt. Alle mogelijke diersoorten worden verkregen uit de 
    # tbl_diersoorten_en_diercategorieen-tabel.
    
    # We willen op deze manier alle mogelijke combinaties krijgen in plaats van
    # alleen te kijken naar alle diersoorten in een verwerkingsmaand van de
    # tbl_microbase tabellen voor het geval dat in een bepaalde maand niet alle
    # diersoorten worden gerapporteerd.
    df_dsrten <- con |> 
      dplyr::tbl(dbplyr::in_schema(schema, tabel_dsrt_en_dcat)) |> 
      dplyr::collect()
    
    df_maanden <- con |> 
      dplyr::tbl(dbplyr::in_schema(schema, tabel_micro_rdvl)) |> 
      dplyr::filter(vwmd == vwmd_waarde) |> 
      dplyr::distinct(vwmd, vsmd) |> 
      dplyr::collect()
    
    df_kader <- df_maanden |> 
      dplyr::cross_join(
        df_dsrten |> dplyr::distinct(dsrt)
      )
    
    # We verdelen de diersoort "Onbekend" over de rundcategoerieen op ratio:
    # Als er 1000 dieren zijn met bekende rundcategorie en 100 met import, 
    # dan worden de hoeveelheden van alle rundcategorieen met 
    # 1,1 vermenigvuldigd (1 + 100/1000). Hierdoor worden de onbekende dieren 
    # verdeeld over de bekende diersoorten, met hetzelfde rato per rundcategorie.
    df_alle_dcats <- con |> 
      dplyr::tbl(dbplyr::in_schema(schema, tabel_micro_rdvl)) |> 
      dplyr::filter(vwmd == vwmd_waarde) |> 
      dplyr::collect()
    
    if ("Onbekend" %in% df_alle_dcats$dsrt) {
      vec_runderen <- c('Stieren', 'Koeien', 'Vaarzen',
                        'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd')
      
      # filter de runderen (en de runderen + onbekend)
      df_runderen <- dplyr::filter(df_alle_dcats, dsrt %in% vec_runderen) 
      df_onbekend <- dplyr::filter(df_alle_dcats, dsrt == 'Onbekend')
      
      df_niet_runderen <- df_alle_dcats |>
        dplyr::anti_join(dplyr::bind_rows(df_runderen, df_onbekend), 
                         by = join_by(dsrt)
        )
    
      # bereken de vermenigvuldigingsfactor om "Onbekend" over de runderen te
      # verdelen, per verslagmaand
      df_verd <- df_runderen |>
        # berekend het totaal aantal runderen
        dplyr::summarise(
          .by = vsmd,
          tota_rund_ruw = sum(aant_ruw, na.rm = TRUE),
          tota_rund_gaaf = sum(aant_gaaf, na.rm = TRUE)
        ) |> 
        dplyr::left_join(
          # bereken het totaal aantal Onbekend
          df_onbekend |> dplyr::summarise(
            .by = vsmd,
            tota_onbekend_ruw = sum(aant_ruw, na.rm = TRUE),
            tota_onbekend_gaaf = sum(aant_gaaf, na.rm = TRUE)),
          by = "vsmd"
        ) |>
        # bereken de vermenigvuldigingsfactor
        dplyr::mutate(
          verd_factor_ruw = 1 + (tota_onbekend_ruw / tota_rund_ruw),
          verd_factor_gaaf = 1 + (tota_onbekend_gaaf / tota_rund_gaaf)
        ) |>
        # vervang NA in verd_factor kolommen (door afwezigheid Onbekend)
        tidyr::replace_na(
          list(verd_factor_ruw = 1, verd_factor_gaaf = 1)
        ) |>
        dplyr::select(vsmd, verd_factor_ruw, verd_factor_gaaf)

      # maak de df om mee verder te werken
      df_alle_dcats_rdvl <- df_runderen |>
        # verdeel "Onbekend" over de runderen
        dplyr::left_join(df_verd, by="vsmd") |>
        dplyr::mutate(
          aant_gaaf = round(aant_gaaf * verd_factor_gaaf, 0),
          aant_ruw = round(aant_ruw * verd_factor_ruw, 0)
        ) |>
        # voeg de runderen bij de rest van het roodvlees
        dplyr::bind_rows(df_niet_runderen) |>
        dplyr::select(-c(verd_factor_ruw, verd_factor_gaaf))
    } else {
      df_alle_dcats_rdvl <- df_alle_dcats
    }
    
    #################
    
    df_aggregaties <- df_alle_dcats_rdvl |> 
      # rvo diersoorten zijn eigenlijk diercategorieen (laagstse niveau), dus we
      # moeten van dcat naar dsrt. We gebruiken de
      # tbl_diersoorten_en_diercategorieen om ze te matchen. Voor de andere
      # diersoorten (NVWA data) diersoort en diercategorie zijn gelijk
      dplyr::rename(dcat = dsrt) |> 
      dplyr::left_join(
        df_dsrten,
        by = "dcat"
      ) |> 
      dplyr::select(-dcat) |> 
      dplyr::bind_rows(
        # Voor witvlees hebben we geen matching tussen dsrt en dcat nodig omdat
        # alles NVWA-gegevens zijn
        con |> 
          dplyr::tbl(dbplyr::in_schema(schema, tabel_micro_wtvl)) |> 
          dplyr::filter(vwmd == vwmd_waarde) |> 
          dplyr::collect()
      ) |> 
      # aggregaties hier
      dplyr::summarise(
        .by = c(vwmd, vsmd, dsrt, is_biologisch),
        tota_ruw = sum(aant_ruw, na.rm = TRUE),
        tota_gaaf = sum(aant_gaaf, na.rm = TRUE)
      ) |> # vervang totalen van 0 met NA (voor percentage bijschattingen/ pcbs)
      dplyr::mutate(
        tota_ruw = replace(tota_ruw, tota_ruw == 0, NA),
        tota_gaaf = replace(tota_gaaf, tota_gaaf == 0, NA)
      )
    
    stat_data <- dplyr::left_join(
        df_kader,
        df_aggregaties,
        by = c("vwmd", "vsmd", "dsrt")
      ) |> 
      dplyr::mutate(
        pcbs = dplyr::case_when(
          !is.na(tota_ruw) & !is.na(tota_gaaf) ~  100 * (tota_gaaf - tota_ruw) / tota_ruw,
          !is.na(tota_ruw) &  is.na(tota_gaaf) ~ -100,
          is.na(tota_ruw) & !is.na(tota_gaaf) ~  100,
          is.na(tota_ruw) &  is.na(tota_gaaf) ~  0,
          .default = NA
        )
      ) |> 
      dplyr::select(
        vwmd, vsmd, dsrt, tota_ruw, tota_gaaf, pcbs, is_biologisch
        ) |> 
      dplyr::arrange(vwmd, vsmd, dsrt) 

    DBI::dbDisconnect(con)
    
    if(nrow(stat_data) > 0){
        # schrijf alle records weg
        dat_schrijf_weg(df=stat_data, tabel=tabel_stat, vwmd=jrmnd$vwmd)

        # log wat gedaan is
        logdebug(paste("Aantallen weggeschreven naar", tabel_stat, ":",
                       nrow(stat_data), "records"))

        # schrijf de definitieve geworden cijfers weg
        df_def <- stat_data[ stat_data$vsmd == jrmnd$mnd1, ] |> 
          # selecteer kolommen en zet in juiste volgorde
          dplyr::select(
            vsmd, dsrt, tota_gaaf, pcbs, is_biologisch
          )
        
        dat_schrijf_weg(df=df_def, tabel=tabel_stat_def, vwmd=jrmnd$vwmd,
                        def=TRUE, vsmd=jrmnd$mnd1)

        # log wat gedaan is
        logdebug(paste("Aantallen weggeschreven naar", tabel_stat_def , ":",
                      nrow(df_def), "records"))
    }
}


#' Haal de aantallen roodvlees en witvlees en gewichten geaggregeerd op
#' diersoortcategorie (dcat) uit de microbase op en schrijf de aggregaten weg
#' naar statbase. Schrijf ook de definitief geworden data weg.
#'
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @param tabel_micro_rdvl string met naam microbasetabel roodvlees
#' @param tabel_micro_wtvl string met naam microbasetabel witvlees
#' @param tabel_micro_verd string met naam microbasetabel aantallenverdelingen
#' @param tabel_micro_gemg string met naam microbasetabel gemiddelde gewichten
#' @param tabel_micro_gemg_sort string met naam microbase tabel sortering
#'                              gemiddelde gewichten
#' @param tabel_stat string met naam statbasetabel op dsrt-niveau
#' @param schema string met schema
#' @return niets
ops_updaten_stat_dcat <- function(jrmnd,
                                  tabel_micro_rdvl=App$tbl_microbase_rdvl,
                                  tabel_micro_wtvl=App$tbl_microbase_wtvl,
                                  tabel_micro_gemg=App$tbl_microbase_gemg,
                                  tabel_stat=App$tbl_statbase_dcat,
                                  tabel_stat_def=App$tbl_statbase_dcat_def,
                                  tabel_dsrt_en_dcat = App$tbl_dsrt_en_dcat,
                                  drivernaam=App$dbdrivernaam,
                                  servernaam=App$dbservernaam,
                                  databasenaam=App$databasenaam,
                                  schema=App$databaseschema) {
    
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = drivernaam,
                        Server = servernaam,
                        Database = databasenaam
  )
  vwmd_waarde <- jrmnd$vwmd
  
  # We krijgen eerst een tabel met alle mogelijke combinaties tussen vwmd,
  # vsmd en dsrt. Alle mogelijke diersoorten worden verkregen uit de 
  # tbl_diersoorten_en_diercategorieen-tabel.
  
  # We willen op deze manier alle mogelijke combinaties krijgen in plaats van
  # alleen te kijken naar alle diersoorten in een verwerkingsmaand van de
  # tbl_microbase tabellen voor het geval dat in een bepaalde maand niet alle
  # diersoorten worden gerapporteerd.
  df_dsrten <- con |> 
    dplyr::tbl(dbplyr::in_schema(schema, tabel_dsrt_en_dcat)) |> 
    dplyr::collect()
  
  df_maanden <- con |> 
    dplyr::tbl(dbplyr::in_schema(schema, tabel_micro_rdvl)) |> 
    dplyr::filter(vwmd == vwmd_waarde) |> 
    dplyr::distinct(vwmd, vsmd) |> 
    dplyr::collect()
  
  df_kader <- df_maanden |> 
    dplyr::cross_join(
      df_dsrten |> dplyr::distinct(dcat)
    )
  
  # We verdelen de diersoort "Onbekend" over de rundcategoerieen op ratio:
  # Als er 1000 dieren zijn met bekende rundcategorie en 100 met import, 
  # dan worden de hoeveelheden van alle rundcategorieen met 
  # 1,1 vermenigvuldigd (1 + 100/1000). Hierdoor worden de onbekende dieren 
  # verdeeld over de bekende diersoorten, met hetzelfde rato per rundcategorie.
  df_alle_dcats <- con |> 
    dplyr::tbl(dbplyr::in_schema(schema, tabel_micro_rdvl)) |> 
    dplyr::filter(vwmd == vwmd_waarde) |> 
    dplyr::collect()
  
  if ("Onbekend" %in% df_alle_dcats$dsrt) {
    vec_runderen <- c('Stieren', 'Koeien', 'Vaarzen',
                      'Kalveren 0-8 mnd', 'Kalveren 8-12 mnd')
    
    # filter de runderen (en de runderen + onbekend)
    df_runderen <- dplyr::filter(df_alle_dcats, dsrt %in% vec_runderen) 
    df_onbekend <- dplyr::filter(df_alle_dcats, dsrt == 'Onbekend')
    
    df_niet_runderen <- df_alle_dcats |>
      dplyr::anti_join(dplyr::bind_rows(df_runderen, df_onbekend), 
                       by = join_by(dsrt)
      )
    
    # bereken de vermenigvuldigingsfactor om "Onbekend" over de runderen te
    # verdelen, per verslagmaand
    df_verd <- df_runderen |>
      # berekend het totaal aantal runderen
      dplyr::summarise(
        .by = vsmd,
        tota_rund_ruw = sum(aant_ruw, na.rm = TRUE),
        tota_rund_gaaf = sum(aant_gaaf, na.rm = TRUE)
      ) |> 
      dplyr::left_join(
        # bereken het totaal aantal Onbekend
        df_onbekend |> dplyr::summarise(
          .by = vsmd,
          tota_onbekend_ruw = sum(aant_ruw, na.rm = TRUE),
          tota_onbekend_gaaf = sum(aant_gaaf, na.rm = TRUE)),
        by = "vsmd"
      ) |>
      # bereken de vermenigvuldigingsfactor
      dplyr::mutate(
        verd_factor_ruw = 1 + (tota_onbekend_ruw / tota_rund_ruw),
        verd_factor_gaaf = 1 + (tota_onbekend_gaaf / tota_rund_gaaf)
      ) |>
      # vervang NA in verd_factor kolommen (door afwezigheid Onbekend)
      tidyr::replace_na(
        list(verd_factor_ruw = 1, verd_factor_gaaf = 1)
      ) |>
      dplyr::select(vsmd, verd_factor_ruw, verd_factor_gaaf)
    
    # maak de df om mee verder te werken
    df_alle_dcats_rdvl <- df_runderen |>
      # verdeel "Onbekend" over de runderen
      dplyr::left_join(df_verd, by="vsmd") |>
      dplyr::mutate(
        aant_gaaf = round(aant_gaaf * verd_factor_gaaf, 0),
        aant_ruw = round(aant_ruw * verd_factor_ruw, 0)
      ) |>
      # voeg de runderen bij de rest van het roodvlees
      dplyr::bind_rows(df_niet_runderen) |>
      dplyr::select(-c(verd_factor_ruw, verd_factor_gaaf))
  } else {
    df_alle_dcats_rdvl <- df_alle_dcats
  }
  
  df_aggregaties <- dplyr::bind_rows(
    df_alle_dcats_rdvl,
    con |> 
      dplyr::tbl(dbplyr::in_schema(schema, tabel_micro_wtvl)) |> 
      dplyr::filter(vwmd == vwmd_waarde) |> 
      dplyr::collect()
  ) |> 
    # rvo diersoorten zijn eigenlijk diercategorieen (laagstse niveau), 
    # dus we moeten heier NIET koppelen
    dplyr::rename(dcat = dsrt) |> 
    dplyr::summarise(
      .by = c(vwmd, vsmd, dcat, is_biologisch),
      tota_ruw = sum(aant_ruw, na.rm = TRUE),
      tota_gaaf = sum(aant_gaaf, na.rm = TRUE)
    )
  
  stat_data <- df_kader |> 
    dplyr::left_join(
      df_aggregaties,
      by = c("vwmd", "vsmd", "dcat")
    ) |> 
    dplyr::left_join(
      dplyr::tbl(con, dbplyr::in_schema(schema, tabel_micro_gemg)),
      copy = TRUE,
      by = c("vwmd", "vsmd", "dcat")
    ) |> 
    dplyr::mutate(
      totg = tota_gaaf * gemg
    ) |> 
    dplyr::select(
      vwmd, vsmd, dcat, tota_gaaf, totg, is_biologisch
      ) |> 
    dplyr::arrange(vwmd, vsmd, dcat) 

  DBI::dbDisconnect(con)
  
  if(nrow(stat_data) > 0){

      # schrijf alle records weg
      dat_schrijf_weg(df=stat_data, tabel=tabel_stat, vwmd=jrmnd$vwmd)

      # log wat gedaan is
      logdebug(paste("Aantallen weggeschreven naar", tabel_stat, ":",
                 nrow(stat_data), "records"))

      # schrijf de definitieve geworden cijfers weg
      df_def <- stat_data[ stat_data$vsmd == jrmnd$mnd1, ] |> 
        # selecteer kolommen en zet in juiste volgorde
        dplyr::select(
          vsmd, dcat, tota_gaaf, totg, is_biologisch
        )
      
      dat_schrijf_weg(df=df_def, tabel=tabel_stat_def, vwmd=jrmnd$vwmd,
                      def=TRUE, vsmd=jrmnd$mnd1)

      # log wat gedaan is
      logdebug(paste("Aantallen weggeschreven naar", tabel_stat_def , ":",
                    nrow(df_def), "records"))
  }
}


#' Schrijf de gevalideerde data uit de controle- en correctiebestanden weg naar
#' microbase en statbase.
#'
#' @param bst_aant_rdvl_ruw string met pad naar levering aantallen roodvlees
#' @param bst_aant_rdvl_gaaf string met pad naar ctcr-bestand roodvlees
#' @param bst_aant_wtvl_ruw string met pad naar levering aantallen witvlees
#' @param bst_aant_wtvl_gaaf string met pad naar ctcr-bestand witvlees
#' @param bst_verd_gaaf string met pad naar ctcr-bestand aantallenverdelingen
#' @param bst_verd_gaaf string met pad naar ctcr-bestand gemiddelde gewichten
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @return niets
opsl_opslaan_ctcr <- function(bst_aant_rdvl_nvwa_ruw, bst_aant_rdvl_rvo_ruw, 
                              bst_aant_rdvl_gaaf,
                              bst_aant_wtvl_ruw, bst_aant_wtvl_gaaf,
                              bst_verd_gaaf, bst_gemg_gaaf,
                              maanden_huidige_vm, jrmnd){
    withProgress(message="", value=0, {
        mld <- "Opslaan aantallen roodvlees in microbase"
        setProgress(message=mld, value=0.14)
        logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
        ops_updaten_micro_rdvl(bst_aant_rdvl_nvwa_ruw=bst_aant_rdvl_nvwa_ruw, 
                               bst_aant_rdvl_rvo_ruw=bst_aant_rdvl_rvo_ruw,
                               bst_aant_gaaf=bst_aant_rdvl_gaaf,
                               maanden_huidige_vm=maanden_huidige_vm,
                               jrmnd=jrmnd)

        mld <- "Opslaan aantallen witvlees in microbase"
        setProgress(message=mld, value=0.28)
        logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
        ops_updaten_micro_wtvl(bst_aant_ruw=bst_aant_wtvl_ruw,
                               bst_aant_gaaf=bst_aant_wtvl_gaaf,
                               maanden_huidige_vm=maanden_huidige_vm,
                               jrmnd=jrmnd)

        mld <- "Opslaan aantallenverdelingen in microbase"
        setProgress(message=mld, value=0.42)
        logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
        ops_updaten_micro_verd(bst_gaaf=bst_verd_gaaf,
                               maanden_huidige_vm=maanden_huidige_vm,
                               jrmnd=jrmnd)

        mld <- "Opslaan gemiddelde gewichten in microbase"
        setProgress(message=mld, value=0.56)
        logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
        ops_updaten_micro_gemg(bst_gaaf=bst_gemg_gaaf,
                               maanden_huidige_vm=maanden_huidige_vm,
                               jrmnd=jrmnd)

        mld <- "Opslaan totalen per diersoort in statbase"
        setProgress(message=mld, value=0.70)
        logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
        ops_updaten_stat_dsrt(jrmnd=jrmnd)

        mld <- "Opslaan totalen per diersoortcategorie in statbase"
        setProgress(message=mld, value=0.84)
        logdebug(msg=paste("Systeem meldt in voortgangsbalk:", mld))
        ops_updaten_stat_dcat(jrmnd=jrmnd)

        setProgress(message="Gereed", value=1.0)
    })
}
