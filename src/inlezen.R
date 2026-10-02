#' Naam        : inlezen.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module bevat functies om historische gegevens in de
#'               database, inputbestanden en controle- en correctiebestanden
#'               in te lezen.


##### generieke functies voor het inlezen #####################################
#' Maak van een platte tabel een draaitabel.
#'
#' In de controle- en correctiesheets is de data als draaitabel weergegeven.
#' Deze functie maakt een draaitabel van een platte tabel. Deze functie wordt
#' gebruikt bij het maken van de controle- en correctiesheets van aantallen
#' roodvlees en aantallen witvlees.
#'
#' Uitgangspunten:
#' - df bevat de kolommen vwmd, vsmd, slnm, wrkp, dsrt en een kolom met de
#'   waarde en in deze volgorde
#' - Het koppelen van data uit verschillende verslagmaanden gebeurt via een
#'   outer join
#' - Indien een waarde ontbreekt in een verslagmaand wordt een lege string
#'   teruggegeven voor die verslagmaand (we vervangen NA's door lege string)
#'
#' @param df dataframe
#' @param vsmd verslagmaanden die moeten worden opgehaald
#' @return df_draai met df als draaitabel
#' @example
#' > df[1:2,]
#'   vwmd   vsmd            slnm    wrkp           dsrt    aant
#' 201801 201708 BEDRIJFSNAAM 1 0101948   Vleeskuikens 1622318
#' 201801 201708   BEDRIJFSNAAM 2 0101959 Overige kippen  313600
#' > inl_maak_draaitabel(df, c('201708', '201709', '201710', '201711'))
#'   vwmd            slnm    wrkp           dsrt  201708  201709  201710  201711
#' 201801 BEDRIJFSNAAM 1 0101948   Vleeskuikens 1622318 1513320 1548388 1469133
#' 201801   BEDRIJFSNAAM 2 0101959 Overige kippen  313600  341471  487664  221238
inl_maak_draaitabel <- function(df_volledig, vsmd, extra_kolommen=c()) {
  
  # in vergelijking met witvlees, heeft roodvlees extra kolommen is_biologisch en bron
  # bron wordt op het einde weer toegevoegd
  if ("is_biologisch" %in% extra_kolommen) {
    df <- dplyr::select(df_volledig, !any_of(extra_kolommen[extra_kolommen != "is_biologisch"]))
  } else { # hernoem de df voor witvlees
    df <- df_volledig
  }
  
  # initialiseer df_draai
  df_draai <- df[df$vsmd == vsmd[1], ] # start met rijen uit eerste verslagmaand
  if (nrow(df_draai) == 0) {
      df_draai <- data.frame("vwmd"=character(), "vsmd"=character(),
                             "slnm"=character(), "wrkp"=character(),
                             "dsrt"=character(), "aant"=character(),
                             stringsAsFactors=FALSE)
  }
  df_draai$vsmd <- NULL # verwijder kolom vsmd
  df_draai <- plyr::rename(df_draai, c("aant"=vsmd[1])) # hernoem kolom aant in verslagmaand

  # merge de overige verslagmaanden aan df_draai als outer join
  if (length(vsmd) > 1) { # tijdens ontwikkeling nodig, in productie overbodig
      for (m in vsmd[2:length(vsmd)]) { # voor de overige verslagmaanden
          df_sel <- df[df$vsmd == m,]
          if (nrow(df_sel) == 0) {
              df_sel <- data.frame("vwmd"=character(), "vsmd"=character(),
                                   "slnm"=character(), "wrkp"=character(),
                                   "dsrt"=character(), "aant"=character(),
                                   stringsAsFactors=FALSE)
          }
          df_sel$vsmd <- NULL # verwijder kolom vsmd
          
          # merge ook op biologisch als die in de extra kolommen zit (nog niet geval voor witvlees)
          if ("is_biologisch" %in% extra_kolommen) {
            df_draai <- merge(x=df_draai, y=df_sel,
                        by=c("vwmd", "slnm", "wrkp", "dsrt", "is_biologisch"), all=TRUE)
          } else {
            df_draai <- merge(x=df_draai, y=df_sel,
                        by=c("vwmd", "slnm", "wrkp", "dsrt"), all=TRUE)
          }
            
          df_draai <- plyr::rename(df_draai, c("aant"=m))  # hernoem kolom aant in verslagmaand
      }
  }

  # # vervang NA door lege string in alle verslagmaandkolommen
  for (m in vsmd) {
      df_draai[, m][is.na(df_draai[, m])] <- ""
  }
  
  # voeg de extra kolommen weer toe; 
  # match op verwerkingsmaand, bedrijfnaam, werkpleknummer, diersoort
  if (!is.null(extra_kolommen)) {
    match_op <- c("vwmd", "slnm", "wrkp", "dsrt", "is_biologisch")
    df_extra_cols <- df_volledig |>
      dplyr::select(
        all_of(match_op), 
        any_of(extra_kolommen[extra_kolommen != "is_biologisch"])
    ) |>
      dplyr::distinct() # filter de herhalingen er uit
    # cast biologisch naar boolean
    
    if ("is_biologisch" %in% names(df_extra_cols)){
      df_extra_cols$is_biologisch <- as.logical(df_extra_cols$is_biologisch)
    }
    df_draai <- dplyr::left_join(df_draai, df_extra_cols, by=match_op)
  }

  return(df_draai)
}


#' Sla draaitabel plat.
#'
#' Voor het opslaan van data in draaitabelformaat dient de draaitabel eerst te
#' worden platgeslagen.
#'
#' Uitgangspunten:
#' - de namen van de kolommen die moeten worden platgeslagen bestaan uit zes
#'   cijfers, omdat het verslagmaanden zijn (jjjjmm)
#' - kols_res bevat de kolom vsmd (verslagmaand)
#' - de waarden komen in platgeslagen tabel in de laatste kolom
#'
#' NB. Wellicht deze functie omschrijven naar een apply-vorm
#' @param df dataframe met draaitabel
#' @param kols_res vector met kolomnamen van resultaattabel
#' @return df_plat dataframe met platgeslagen draaitabel
#' @example
#' > df[1:3,]
#'   vwmd    wrkp            slnm           dsrt 201708 201709 201710 201711
#' 201801 0200043 BEDRIJFSNAAM       Kalveren     NA     NA     NA     NA
#' 201801 0200043 BEDRIJFSNAAM Volwassen rund     NA      9     NA     NA
#' 201801 0200043 BEDRIJFSNAAM       Lammeren     NA      5     NA     NA
#' > inl_sla_draaitabel_plat(df, c("vwmd","vsmd","wrkp","slnm","dsrt","aant"))
#'    vwmd   vsmd    wrkp            slnm           dsrt aant
#' 201801 201709 0200043 BEDRIJFSNAAM Volwassen rund    9
#' 201801 201709 0200043 BEDRIJFSNAAM       Lammeren    5
#' 201801 201709 0200043 BEDRIJFSNAAM        Varkens    8
inl_sla_draaitabel_plat <- function(df, kols_res) {
    # initialiseer leeg dataframe voor platgeslagen resultaat
    df_plat <- as.data.frame(setNames(replicate(length(kols_res), character()),
                                      kols_res),
                             stringsAsFactors=FALSE)
    n <- 0 # aantal rijen in df_plat

    # vind kolomnamen van verslagmaanden (deze beginnen met 6 cijfers)
    vsmd <- names(df)[pattern=grep("^[0-9]{6}", x=names(df))]

    # vind elke waarde in de verslagmaandkolommen en sla plat
    for (rij in 1:nrow(df)) {
        for (kol_v in vsmd) {
            if (!is.na(df[rij, kol_v])){ # sla alleen niet NA-waarden plat
                n <- n + 1
                for (kol_k in intersect(kols_res, names(df))) {
                    df_plat[n, kol_k] <- df[rij, kol_k] # neem kenmerken over
                }
                df_plat[n, "vsmd"] <- kol_v # vul vsmd met verslagmaand
                df_plat[n, ncol(df_plat)] <- df[rij, kol_v] # vul laatste kolom met waarde
            }
        }
    }
    return(df_plat)
}


#' Voeg de diersoort toe op basis van de diersoortcategorie in een nieuwe kolom
#' dsrt.
#'
#' @param df dataframe met kolom dcat
#' @return df dataframe met kolommen dcat en dsrt
#' @example
#' > df
#'               dcat 201708 201709 201710 201711
#'            Stieren  11.72  13.71  10.66  11.08
#'             Koeien  85.95  83.96  87.01  86.59
#'            Vaarzen   2.33   2.33   2.33   2.33
#'   Kalveren 0-8 mnd  90.25  90.25  90.25  90.25
#'  Kalveren 8-12 mnd   9.75   9.75   9.75   9.75
#'> inl_voeg_dsrt_toe(df)
#'               dcat 201708 201709 201710 201711               dsrt
#'            Stieren  11.72  13.71  10.66  11.08 Volwassen runderen
#'             Koeien  85.95  83.96  87.01  86.59 Volwassen runderen
#'            Vaarzen   2.33   2.33   2.33   2.33 Volwassen runderen
#'   Kalveren 0-8 mnd  90.25  90.25  90.25  90.25           Kalveren
#'  Kalveren 8-12 mnd   9.75   9.75   9.75   9.75           Kalveren
inl_voeg_dsrt_toe <- function(df) {
    df$dsrt[df$dcat=="Stieren"]           <- "Volwassen runderen"
    df$dsrt[df$dcat=="Koeien"]            <- "Volwassen runderen"
    df$dsrt[df$dcat=="Vaarzen"]           <- "Volwassen runderen"
    df$dsrt[df$dcat=="Kalveren 0-8 mnd"]  <- "Kalveren"
    df$dsrt[df$dcat=="Kalveren 8-12 mnd"] <- "Kalveren"

    return(df)
}

##### historische aantallen roodvlees en witvlees uit de database #############
#' Inlezen aantallen roodvlees en witvlees uit database als draaitabel.
#'
#' Haal gegevens van vorige verwerkingsmaand op uit de microbasetabel in de
#' database met behulp van een select-query en lees deze uit als draaitabel.
#'
#' Er wordt verwacht dat de database een is_biologisch kolom heeft.
#'
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @param tabel string met databasenaam
#' @param schema string met schemanaam
#' @return hist_data dataframe met historische data als draaitabel
#' @example
#' > inl_lees_hist_aant(jrmnd, tabel)
#'                   slnm    wrkp               dsrt vvm_mnd1 vvm_mnd2 vvm_mnd3
#' BEDRIJFSNAAM 1646092  Eenhoevige dieren     <NA>     <NA>     <NA>
#' BEDRIJFSNAAM 1646092             Geiten     <NA>     <NA>     <NA>
#' BEDRIJFSNAAM 1646092           Kalveren     <NA>     <NA>     <NA>
inl_lees_hist_aant <- function(jrmnd, tabel, schema=App$databaseschema) {

  sqlstr <- paste0("select distinct z.slnm, z.wrkp, z.dsrt, z.is_biologisch, m1.aant_gaaf as 'vsmd1', m2.aant_gaaf as 'vsmd2', m3.aant_gaaf as 'vsmd3'",
                  " from (select distinct vwmd, slnm, wrkp, dsrt, is_biologisch from ", schema, ".", tabel, " where vwmd='", as.integer(jrmnd$vvwmd), "') as z",
                  " left join (select vwmd, slnm, wrkp, dsrt, is_biologisch, aant_gaaf from  ", schema, ".", tabel, " where vsmd='", as.integer(jrmnd$mnd1), "') as m1",
                  " on m1.vwmd = z.vwmd and m1.slnm = z.slnm and m1.wrkp = z.wrkp and m1.dsrt = z.dsrt and m1.is_biologisch = z.is_biologisch",
                  " left join (select vwmd, slnm, wrkp, dsrt, is_biologisch, aant_gaaf from ", schema, ".", tabel, " where vsmd='", as.integer(jrmnd$mnd2), "') as m2",
                  " on m2.vwmd = z.vwmd and m2.slnm = z.slnm and m2.wrkp = z.wrkp and m2.dsrt = z.dsrt and m2.is_biologisch = z.is_biologisch",
                  " left join (select vwmd, slnm, wrkp, dsrt, is_biologisch, aant_gaaf from ", schema, ".", tabel, " where vsmd='", as.integer(jrmnd$mnd3), "') as m3",
                  " on m3.vwmd = z.vwmd and m3.slnm = z.slnm and m3.wrkp = z.wrkp and m3.dsrt = z.dsrt and m3.is_biologisch = z.is_biologisch"
  )
  
  hist_data <- dat_lees_db(sqlstr)

  if(nrow(hist_data) > 0){
    colnames(hist_data) <- c("slnm","wrkp","dsrt", "is_biologisch", "vvm_mnd1","vvm_mnd2","vvm_mnd3")
    # verwijder regels met alleen aantallen in de maand voor jrmnd$mnd1
    hist_data <- hist_data[!(is.na(hist_data$vvm_mnd1)) | !(is.na(hist_data$vvm_mnd2)) | !(is.na(hist_data$vvm_mnd3)),]
  } else {
    logdebug(msg="Geen historische gegevens gevonden")
    # als query niets teruggeeft, geef leeg dataframe terug
    hist_data <- data.frame("slnm"=character(), "wrkp"=character(),
                            "dsrt"=character(), "vvm_mnd1"=character(),
                            "vvm_mnd2"=character(),"vvm_mnd3"=character(),
                            stringsAsFactors=FALSE)
  }
  # cast is_biologisch naar boolean
  hist_data$is_biologisch = ifelse(hist_data$is_biologisch == "1", TRUE, FALSE)
  
  return(hist_data)
}


##### historische aantallenverdelingen uit de database ########################
#' Inlezen aantallenverdelingen uit database als draaitabel
#'
#' Haal gegevens van vorige verwerkingsmaand op uit de microbasetabel in de
#' database met behulp van een select-query en lees deze uit als draaitabel.
#' In het resultaat zijn de rownames gezet om later in het proces op te kunnen
#' koppelen.
#'
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @param tabel string met databasenaam
#' @param schema string met schemanaam
#' @return hist_data dataframe met historische data als draaitabel
#' @example
#' > inl_lees_hist_verd(jrmnd)
#'              dcat vvm_mnd1 vvm_mnd2 vvm_mnd3
#'           Stieren    11.08    11.08    11.08
#'            Koeien    86.59    86.59    86.59
#'           Vaarzen     2.33     2.33     2.33
#'  Kalveren 0-8 mnd    95.00    95.00    95.00
#' Kalveren 8-12 mnd     5.00     5.00     5.00
inl_lees_hist_verd <- function(jrmnd,
                               tabel=App$tbl_microbase_verd,
                               schema=App$databaseschema) {
  sqlstr <- paste0("select z.dcat as dcat, m1.verd as vsmd1, m2.verd as vsmd2, m3.verd as vsmd3",
                   " from (select distinct dcat, dsrt, vwmd from ", schema, ".", tabel,
                   " where vwmd='", as.integer(jrmnd$vvwmd), "') as z ",
                   "left join (select dcat, vwmd, verd from ", schema, ".", tabel,
                   " where vsmd='", as.integer(jrmnd$mnd1), "') as m1 on z.dcat = m1.dcat and z.vwmd = m1.vwmd ",
                   "left join (select dcat, vwmd, verd from ", schema, ".", tabel,
                   " where vsmd='", as.integer(jrmnd$mnd2), "') as m2 on z.dcat = m2.dcat and z.vwmd = m2.vwmd ",
                   "left join (select dcat, vwmd, verd from ", schema, ".", tabel,
                   " where vsmd='", as.integer(jrmnd$mnd3), "') as m3 on z.dcat = m3.dcat and z.vwmd = m3.vwmd ",
                   "left join dbo.tbl_microbase_verd_sortering sort on z.dcat = sort.dcat order by sort.volgorde ")
  hist_data <- dat_lees_db(sqlstr)

  kolomkoppen_vvmd <- c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3")

  if(nrow(hist_data) > 0) {
    colnames(hist_data) <- c("dcat", kolomkoppen_vvmd)
    hist_data[, kolomkoppen_vvmd] <- sapply(hist_data[, kolomkoppen_vvmd], as.numeric) # maak van de waarden numerics
    # verwijder regels met alleen aantallen in de maand voor jrmnd$mnd1
    hist_data <- hist_data[!(is.na(hist_data$vvm_mnd1)) | !(is.na(hist_data$vvm_mnd2)) | !(is.na(hist_data$vvm_mnd3)),]
  } else {
    logdebug(msg="Geen historische gegevens gevonden")
    # geef leeg dataframe terug
    hist_data <- data.frame("dcat"=c("Stieren", "Koeien", "Vaarzen",
                                     "Kalveren 0-8 mnd", "Kalveren 8-12 mnd"),
                            "vvm_mnd1"=rep("",5),
                            "vvm_mnd2"=rep("",5),
                            "vvm_mnd3"=rep("",5),check.names=FALSE)
  }
  rownames(hist_data) <- hist_data$dcat

  return(hist_data)
}


##### historische gemiddelde gewichten uit de database ########################
#' Inlezen gemiddelde gewichten uit database als draaitabel.
#'
#' Haal gegevens van vorige verwerkingsmaand op uit de microbasetabel in de
#' database met behulp van een select-query en lees deze uit als draaitabel.
#'
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @param tabel string met databasenaam
#' @param schema string met schemanaam
#' @return hist_data dataframe met historische data als draaitabel
#' @example
#' > inl_lees_hist_gemg(jrmnd)
#'               dcat dsrt   vwmd vvm_mnd1 vvm_mnd2 vvm_mnd3
#'   Kalveren 0-8 mnd Kalf 201805    95.00    95.00    95.00
#'  Kalveren 8-12 mnd Kalf 201805     5.00     5.00     5.00
#'             Koeien Rund 201805    86.59    86.59    86.59
#'            Stieren Rund 201805    11.08    11.08    11.08
#'            Vaarzen Rund 201805     2.33     2.33     2.33
inl_lees_hist_gemg <- function(jrmnd,
                               tabel=App$tbl_microbase_gemg,
                               schema=App$databaseschema) {
  sqlstr <- paste0("select z.dcat as dcat, m1.gemg as vsmd1, m2.gemg as vsmd2, m3.gemg as vsmd3",
                   " from (select distinct dcat, vwmd from ", schema, ".", tabel,
                   " where vwmd='", as.integer(jrmnd$vvwmd), "') as z ",
                   "left join (select dcat, vwmd, gemg from ", schema, ".", tabel,
                   " where vsmd='", as.integer(jrmnd$mnd1), "') as m1 on z.dcat = m1.dcat and z.vwmd = m1.vwmd ",
                   "left join (select dcat, vwmd, gemg from ", schema, ".", tabel,
                   " where vsmd='", as.integer(jrmnd$mnd2), "') as m2 on z.dcat = m2.dcat and z.vwmd = m2.vwmd ",
                   "left join (select dcat, vwmd, gemg from ", schema, ".", tabel,
                   " where vsmd='", as.integer(jrmnd$mnd3), "') as m3 on z.dcat = m3.dcat and z.vwmd = m3.vwmd ",
                   "left join dbo.tbl_microbase_gemg_sortering sort on z.dcat = sort.dcat order by sort.volgorde ")
  hist_data <- dat_lees_db(sqlstr)

  kolomkoppen_vvwmd <- c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3")

  if(nrow(hist_data) > 0) {
    colnames(hist_data) <- c("dcat", kolomkoppen_vvwmd)
    hist_data[, kolomkoppen_vvwmd] <- sapply(hist_data[, kolomkoppen_vvwmd], as.numeric) # maak van de waarden numerics
    # verwijder regels met alleen aantallen in de maand voor jrmnd$mnd1
    hist_data <- hist_data[!(is.na(hist_data$vvm_mnd1)) | !(is.na(hist_data$vvm_mnd2)) | !(is.na(hist_data$vvm_mnd3)),]
  } else {
    logdebug(msg="Geen historische gegevens gevonden")
    # geef leeg dataframe terug
    hist_data <- data.frame("dcat"=character(), "vvm_mnd1"=character(),
                            "vvm_mnd2"=character(), "vvm_mnd3"=character(),
                            stringsAsFactors=FALSE)
  }
  rownames(hist_data) <- hist_data$dcat

  return(hist_data)
}

##### historische aantallen roodvlees en witvlees uit de database #############
#' Inlezen maximale aantal roodvlees en witvlees uit database voor 14 tot en 
#' met 4 maanden liggend voor verwerkingsmaand als draaitabel.
#' NB: deze 14 is momenteel de waarden van App$verschil_vwmd_historie_inicidenteel
#'
#' Haal gegevens uit definitieve microbasetabel in de database op met behulp 
#'van een select-query en lees deze uit als draaitabel.
#'
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @param tabel string met databasenaam
#' @param schema string met schemanaam
#' @param verleden negatief getal
#' @return hist_data dataframe met data als draaitabel
#' @example
#' > inl_lees_hist_max_aant_x_maand_terug(jrmnd, tabel)
#'                   slnm    wrkp               dsrt max_aantal
#' BEDRIJFSNAAM 1646092  Eenhoevige dieren         45
#' BEDRIJFSNAAM 1646092             Geiten        300
#' BEDRIJFSNAAM 1646092           Kalveren         20
inl_lees_hist_max_aant_x_maand_terug <- function(jrmnd, 
                                                 tabel, 
                                                 schema=App$databaseschema, 
                                                 maanden_terug=App$verschil_vwmd_historie_inicidenteel) {
  
  dt_verwerkingsmaand <- as.Date(paste(substr(jrmnd$vwmd,1,4), substr(jrmnd$vwmd,5,6), "01", sep='-'))
  dt_eerstemaand <- dt_verwerkingsmaand %m+% months(maanden_terug)
  eerste_maand <- paste0(substr(dt_eerstemaand,1,4), substr(dt_eerstemaand,6,7))
  
  sqlstr <- paste0("select  slnm, wrkp, dsrt, max(aant_gaaf) as max_aantal ",
                  " from ", schema, ".", tabel, 
                  " where vsmd>='", as.integer(eerste_maand), "'",
                  " and vsmd<'", as.integer(jrmnd$mnd1), "'",
                  " group by slnm, wrkp, dsrt " 
                  )
  hist_data <- dat_lees_db(sqlstr)

  if(nrow(hist_data) > 0){
    colnames(hist_data) <- c("slnm","wrkp","dsrt","max_aantal")
    # filter NVWA runderen er uit
    hist_data <- dplyr::filter(hist_data, dsrt != "Kalveren" & dsrt != "Volwassen runderen")
  } else {
    logdebug(msg="Geen gegevens gevonden in definitieve tabel")
    # als query niets teruggeeft, geef leeg dataframe terug
    hist_data <- data.frame("slnm"=character(), "wrkp"=character(),
                            "dsrt"=character(), "max_hist"=character(),
                            stringsAsFactors=FALSE)
  }

  return(hist_data)
}


##### inputbestand met aantallen roodvlees ####################################
#' Hercodeer soort omschrijving voor roodvlees.
#'
#' De soort omschrijving van een dier in de levering roodvlees wordt omgecodeerd
#' naar intern gebruikte diersoort omschrijvingen. Deze functie wordt gebruikt
#' in de inl_hercodeer_rdvl_SOORT_OMSCHRIJVING().
#' @param so string met soort omschrijving
#' @return intern gebruikte diersoort omschrijving
#' @example
#' > inl_hercodeer_rdvl_dsrt(so="Kalf")
#' "Kalveren"
inl_hercodeer_rdvl_dsrt <- function(so) {
    if (so == "Kalf") {
        return ("Kalveren")
    } else if (so == "Rund") {
        return ("Volwassen runderen")
    } else if (so %in% c("Kalveren 0-8 mnd",
                    "Kalveren 8-12 mnd",
                    "Stieren",
                    "Koeien",
                    "Vaarzen",
                    "Onbekend")) {
      return (so)
    } else if (so == "Schaap jonger dan 1 jaar") {
        return ("Lammeren")
    } else if (so == "Schaap ouder dan 1 jaar") {
        return ("Volwassen schapen")
    } else if (so == "Varken") {
        return ("Varkens")
    } else if (so == "Eenhoevig dier") {
        return ("Eenhoevige dieren")
    } else if (so == "Geit") {
        return ("Geiten")
    } else {
        melding <- paste("Fout! Aantal roodvlees: Onbekende Soort omschrijving", so)
        logerror(melding)
        return (so)
    }
}

#' Hercodeer kolom SOORT OMSCHRIJVING en zet deze in een nieuwe kolom dsrt.
#'
#' @param df dataframe met kolom SOORT OMSCHRIJVING
#' @return df dataframe met SOORT OMSCHRIJVING gehercodeerd in kolom dsrt
#' @example
#' > df
#'            NAAM WERKPLEK_NR       SOORT OMSCHRIJVING 201708 201709 201710 201711
#' BEDRIJFSNAAM     0200043                     Kalf     NA     NA     NA     NA
#' BEDRIJFSNAAM     0200043                     Rund     NA      9     NA     NA
#' BEDRIJFSNAAM     0200043 Schaap jonger dan 1 jaar     NA      5     NA     NA
#' > inl_hercodeer_rdvl_SOORT_OMSCHRIJVING(df)
#'            NAAM WERKPLEK_NR       SOORT OMSCHRIJVING 201708 201709 201710 201711               dsrt
#' BEDRIJFSNAAM     0200043                     Kalf     NA     NA     NA     NA           Kalveren
#' BEDRIJFSNAAM     0200043                     Rund     NA      9     NA     NA Volwassen runderen
#' BEDRIJFSNAAM     0200043 Schaap jonger dan 1 jaar     NA      5     NA     NA           Lammeren
inl_hercodeer_rdvl_SOORT_OMSCHRIJVING <- function(df) {
    # vind eerst alle dsrt's en zet deze in de vector col_herc
    col_herc <- c()
    for (i in 1:nrow(df)) {
        so <- df[i, "SOORT OMSCHRIJVING"]
        dsrt <- inl_hercodeer_rdvl_dsrt(so)
        col_herc <- c(col_herc, dsrt)
    }
    df$dsrt <- col_herc # voeg kolom dsrt toe
    return(df)
}

#' Lees levering aantallen roodvlees in en prepareer.
#'
#' Deze functie leest de levering aantallen roodvlees in en prepareert de
#' structuur zodat deze direct bruikbaar is. Deze functie wordt gebruikt bij het
#' maken van de controle- en correctiesheet roodvlees en bij het opslaan van de
#' controle- en correctiesheet om de ruwe data ook op te slaan in de microbase.
#'
#' @param inputbestand string met naam en pad van levering aantallen roodvlees
#' @param jrmd list met jrmnd$mnd1 en jrmnd$mnd4 als randen voor de mee te
#'             nemen verslagmaanden en jrmd$vwmd. Dit is de output van
#'             alg_vind_jaarmaanden()
#' @return df dataframe
#' @example
#' > inl_lees_input_rdvl(inputbestand, jrmnd)
#'   vwmd   vsmd    wrkp                         slnm              dsrt aant
#' 201801 201708 0100082 BEDRIJFSNAAM 1          Kalveren    2
#' 201801 201708 0101085      BEDRIJFSNAAM 2 Eenhoevige dieren    1
#' 201801 201708 0200327                     BEDRIJFSNAAM 3           Varkens    1
inl_lees_input_rdvl <- function(inputbestand, jrmnd) {
    df_input    <- as.data.frame(read_excel(path=inputbestand, sheet=1))

    # voeg biologisch kolom toe als die niet bestaat
    if (!("BIOLOGISCH" %in% colnames(df_input))) {
      df_input <- dplyr::mutate(
        df_input, 
        is_biologisch = FALSE)
    } else {
      df_input <- dplyr::rename(
        df_input, 
        is_biologisch = BIOLOGISCH)
    }

    df_sel      <- df_input[, c("Jaar", "Maand", "DIER_SOORT_OMSCHR",
                                "WERKPLEK_NR", "WERKPLEKNAAM", "AANTAL_AANGEBODEN",
                                "is_biologisch")]

    # aanpassen nieuw format levering roodvlees (MDAK)
    df_sel <- plyr::rename(df_sel, c("Jaar"="JAAR", "Maand"="MAAND", "DIER_SOORT_OMSCHR"="SOORT OMSCHRIJVING", "WERKPLEKNAAM"="NAAM", "AANTAL_AANGEBODEN"="AANGEBODEN"))
    df_sel$`SOORT OMSCHRIJVING` <- tolower(df_sel$`SOORT OMSCHRIJVING`) # van hoofdletters naar kleine letters (MDAK)
    capFirst <- function(s) {
      paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
    }
    df_sel$`SOORT OMSCHRIJVING` <- capFirst(df_sel$`SOORT OMSCHRIJVING`) # eerste letter naar hoofdletter (MDAK)
    df_sel      <- df_sel[rowSums(is.na(df_sel)) != ncol(df_sel), ] # rijen niet als alleen NA's
    df_sel$MAAND <- paste(formatC(df_sel$MAAND, width = 2, flag ="0"))   # voorloopnul toevoegen aan maanden (MDAK)
    df_sel$vsmd <- paste0(df_sel$JAAR, df_sel$MAAND) # voeg kolom vsmd toe
    df_sel      <- subset(df_sel, vsmd >= jrmnd$mnd1 & vsmd <= jrmnd$mnd4) # rijen uit relevante verslagmaand

    # hercodeer soort omschrijving in kolom dsrt
    df_herc     <- inl_hercodeer_rdvl_SOORT_OMSCHRIJVING(df=df_sel) # voeg kolom dsrt toe

    # pas structuur aan
    df_str      <- plyr::rename(df_herc, c("NAAM"="slnm", "WERKPLEK_NR"="wrkp",
                                     "AANGEBODEN"="aant")) # hernoem kolomnamen
    df_str$vwmd <- jrmnd$vwmd # voeg kolom vwmd toe
    df_str      <- df_str[c("vwmd", "vsmd", "wrkp", 
                            "slnm", "dsrt", "aant", 
                            "is_biologisch")]
    
    return(df_str)
}


##### inputbestand met aantallen witvlees #####################################
#' Hercodeer kolom MAAND en zet deze in een nieuwe kolom mnd.
#'
#' De maand in de levering witvlees wordt omgecodeerd naar intern gebruikte
#' maandcoderingen in een nieuwe kolom mnd.
#' @param df dataframe met kolom MAAND
#' @return df dataframe met MAAND gehercodeerd in kolom mnd
#' @example
#' > df
#'  JAAR     MAAND    DIERSOORT WERKPLEKNUMMER                         NAAM LEVEND AANGEVOERD
#'  2017 januari   Struisvogels        1100190 BEDRIJFSNAAM                 5
#'  2017 februari  Struisvogels        1100190 BEDRIJFSNAAM                 1
#' > inl_hercodeer_wtvl_MAAND(df)
#'  JAAR     MAAND    DIERSOORT WERKPLEKNUMMER                         NAAM LEVEND AANGEVOERD mnd
#'  2017 januari   Struisvogels        1100190 BEDRIJFSNAAM                 5  01
#'  2017 februari  Struisvogels        1100190 BEDRIJFSNAAM                 1  02
inl_hercodeer_wtvl_MAAND <- function(df) {
  df$mnd <- trimws(tolower(df$MAAND))
  df$mnd[df$mnd == "januari"]   <- "01"
  df$mnd[df$mnd == "februari"]  <- "02"
  df$mnd[df$mnd == "maart"]     <- "03"
  df$mnd[df$mnd == "april"]     <- "04"
  df$mnd[df$mnd == "mei"]       <- "05"
  df$mnd[df$mnd == "juni"]      <- "06"
  df$mnd[df$mnd == "juli"]      <- "07"
  df$mnd[df$mnd == "augustus"]  <- "08"
  df$mnd[df$mnd == "september"] <- "09"
  df$mnd[df$mnd == "oktober"]   <- "10"
  df$mnd[df$mnd == "november"]  <- "11"
  df$mnd[df$mnd == "december"]  <- "12"

  return(df)
}

#' Hercodeer diersoort voor witvlees.
#'
#' De diersoort van een dier in de levering witvlees wordt omgecodeerd
#' naar intern gebruikte diersoort omschrijvingen. Deze functie wordt
#' gebruikt in inl_hercodeer_wtvl_DIERSOORT().
#' @param ds string met soort omschrijving
#' @return intern gebruikte diersoort omschrijving
inl_hercodeer_wtvl_dsrt <- function(ds) {
    if (ds == "Struisvogels") {
        return ("Struisvogels")
    } else if (ds == "Duiven") {
        return ("Duiven")
    } else if (ds == "Eenden") {
        return ("Eenden")
    } else if (ds == "Kalkoenen") {
        return ("Kalkoenen")
    } else if (ds == "Kippen") { # eigenlijk enige hercodering
        return ("Overige kippen")
    } else if (ds == "Parelhoenders") {
        return ("Parelhoenders")
    } else if (ds == "Vleeskuikens") {
        return ("Vleeskuikens")
    } else if (ds == "Fazanten") {
        return ("Fazanten")
    } else if (ds == "Ganzen") {
        return ("Ganzen")
    } else if (ds == "Patrijzen") {
        return ("Patrijzen")
    } else {
        melding <- paste("Fout! Aantal witvlees: Onbekende diersoort", ds)
        logerror(melding)
        return (ds)
    }
}

#' Hercodeer kolom DIERSOORT en zet deze in een nieuwe kolom dsrt.
#'
#' @param df dataframe met kolom DIERSOORT
#' @return df dataframe met DIERSOORT gehercodeerd in kolom dsrt
#' @example
#' > df
#'            NAAM WERKPLEK_NR DIERSOORT 201708 201709 201710 201711
#' BEDRIJFSNAAM     0200043    Duiven     NA     NA     NA     NA
#' BEDRIJFSNAAM     0200043    Eenden     NA      9     NA     NA
#' BEDRIJFSNAAM     0200043    Kippen     NA      5     NA     NA
#' > inl_hercodeer_wtvl_DIERSOORT(df)
#'            NAAM WERKPLEK_NR DIERSOORT 201708 201709 201710 201711           dsrt
#' BEDRIJFSNAAM     0200043    Duiven     NA     NA     NA     NA         Duiven
#' BEDRIJFSNAAM     0200043    Eenden     NA      9     NA     NA         Eenden
#' BEDRIJFSNAAM     0200043    Kippen     NA      5     NA     NA Overige kippen
inl_hercodeer_wtvl_DIERSOORT <- function(df) {
    # vind eerst alle dsrt's en zet deze in de vector col_herc
    col_herc <- c()
    for (i in 1:nrow(df)) {
        ds <- df[i, "DIERSOORT"]
        dsrt <- inl_hercodeer_wtvl_dsrt(ds)
        col_herc <- c(col_herc, dsrt)
    }
    df$dsrt <- col_herc # voeg kolom dsrt toe
    return(df)
}

#' Lees levering aantallen witvlees in en prepareer tbv opslaan.
#'
#' Deze functie leest de levering aantallen witvlees in en prepareert de
#' structuur zodat deze direct bruikbaar is. Deze functie wordt gebruikt bij het
#' maken van de controle- en correctiesheet witvlees en bij het opslaan van de
#' controle- en correctiesheet om de ruwe data ook op te slaan in de microbase.
#'
#' @param inputbestand string met naam en pad van leveringn aantallen witvlees
#' @param jrmd list met jrmnd$mnd1 en jrmnd$mnd4 als randen voor de mee te
#'             nemen verslagmaanden en jrmd$vwmd. Dit is de output van
#'             alg_vind_jaarmaanden()
#' @return df
#' @example
#' > inl_lees_input_wtvl(inputbestand, jrmnd)
#'   vwmd   vsmd    wrkp                         slnm   dsrt aant
#' 201801 201708 0100082 BEDRIJFSNAAM 1 Eenden    2
#' 201801 201708 0101085      BEDRIJFSNAAM 2 Duiven    1
#' 201801 201708 0200327                     BEDRIJFSNAAM 3 Eenden    1
inl_lees_input_wtvl <- function(inputbestand, jrmnd) {
    df_input    <- as.data.frame(read_excel(path=inputbestand, sheet="SQL Results"))

    # selecteer relevante kolommen en rijen
    df_sel      <- df_input[, c("JAAR", "MAAND", "DIERSOORT",
                                "WERKPLEKNUMMER", "NAAM", "LEVEND AANGEVOERD")] # relevante kolommen
    df_sel      <- df_sel[rowSums(is.na(df_sel)) != ncol(df_sel), ] # rijen niet als alleen NA's
    df_sel      <- inl_hercodeer_wtvl_MAAND(df_sel) # voeg kolom mnd met maandcode
    df_sel$vsmd <- paste0(df_sel$JAAR, df_sel$mnd) # voeg kolom vsmd toe
    df_sel      <- subset(df_sel, vsmd >= jrmnd$mnd1 & vsmd <= jrmnd$mnd4) # rijen uit relevante verslagmaand

    # hercodeer diersoort in kolom dsrt
    df_herc     <- inl_hercodeer_wtvl_DIERSOORT(df=df_sel) # voeg kolom dsrt toe

    # pas structuur aan
    df_str      <- plyr::rename(df_herc, c("NAAM"="slnm", "WERKPLEKNUMMER"="wrkp",
                                     "LEVEND AANGEVOERD"="aant")) # hernoem kolomnamen
    df_str$vwmd <- jrmnd$vwmd # voeg kolom vwmd toe
    df_str      <- df_str[c("vwmd", "vsmd", "wrkp",  # selecteer kolommen en
                            "slnm", "dsrt", "aant")] # zet in juiste volgorde
    return(df_str)
}


##### inputbestand met aantallenverdelingen en gemiddelde gewichten ###########
#' lees de cellen die gebruikt moeten worden van het geleverde
#' bestand met aantallenverdelingen en gemiddelde gewichten
#' van roodvlees. Zet deze in twee aparte dataframes.
#'
#' kolom B tm D gaat naar aantallenverdelingen roodvlees
#' kolom G tm N gaat naar gemiddelde gewichten roodvlees
#'
#' @param jrmnd list of string
#' @param inputbestand string
#' @return df_verd dataframe met aantallenverdelingen roodvlees
#' @return df_gemg dataframe met gemiddelde gewichten roodvlees
#' @example
#' > inl_lees_input_verd_gemg(inputbestand, jrmnd)$df_verd
#'                    hvm_mnd1  hvm_mnd2  hvm_mnd3  hvm_mnd4 dcat
#' Stieren                11.0      11.0      11.0      11.0 Stieren
#' Koeien                 85.9      85.6      85.9      85.6 Koeien
#' Vaarzen                 4.1       4.1       4.1       4.1 Vaarzen
#' > inl_lees_input_verd_gemg(inputbestand, jrmnd)$df_gemg
#'                    hvm_mnd1  hvm_mnd2  hvm_mnd3  hvm_mnd4 dcat
#' Stieren               550.6     549.8     566.1     550.8 Stieren
#' Koeien                400.4     400.6     400.9     450.7 Koeien
#' Vaarzen               300.2     300.2     209.9     299.8 Vaarzen
#' Kalveren 0-8 mnd         NA        NA        NA        NA Kalveren 0-8 mnd
#' Kalveren 8-12 mnd     150.5     170.0     180.8     145.0 Kalveren 8-12 mnd
#' Lammeren               12.0      12.1      12.3      12.4 Lammeren
#' Varkens               100.0     100.0     100.0     100.0 Varkens
#' Volwassen schapen      30.0      30.0      30.0      30.0 Volwassen schapen
inl_lees_input_verd_gemg <- function(inputbestand, jrmnd) {
  mnd1 <- substr(jrmnd$mnd1, 5, 6)
  jr1  <- substr(jrmnd$mnd1, 1, 4)
  jr4  <- substr(jrmnd$mnd4, 1, 4)

  celbereik   <- "B7:K18"
  eerste_rij  <- as.numeric(mnd1)
  laatste_rij <- eerste_rij + 3
  kolomnamen  <- c("StierenVerd","KoeienVerd","VaarzenVerd","Stieren","Koeien",
                   "Vaarzen", "Kalveren 8-12 mnd","Lammeren","Varkens",
                   "Volwassen schapen")
  kolomnamen_verd <- kolomnamen[1:3]
  kolomnamen_gemg <- kolomnamen[4:10]

  # als mnd1 < 10 -> 1 tabblad
  if(mnd1<"10"){
    data <- as.data.frame(read_excel(path=inputbestand, sheet=jr1,
                                     range=celbereik, col_names=kolomnamen))
    df_data <- data[eerste_rij:laatste_rij, , drop=FALSE] # drop=FALSE behoudt de dimensie van het dataframe
  }
    # als mnd1 10, 11 of 12 -> 2 tabbladen
    else {
      data1 <- as.data.frame(read_excel(path=inputbestand, sheet=jr1,
                             range=celbereik, col_names=kolomnamen))
      data2 <- as.data.frame(read_excel(path=inputbestand, sheet=jr4,
                             range=celbereik, col_names=kolomnamen))

      if(mnd1 == "10") {
        df_data <- rbind(data1[10:12, , drop=FALSE], data2[1:1, , drop=FALSE])
      } else if(mnd1 == "11") {
        df_data <- rbind(data1[11:12, , drop=FALSE], data2[1:2, , drop=FALSE])
      } else if(mnd1 == "12") {
        df_data <- rbind(data1[12:12, , drop=FALSE], data2[1:3, , drop=FALSE])
      }
  }

  kolomkoppen_hvmd <- c("hvm_mnd1", "hvm_mnd2", "hvm_mnd3", "hvm_mnd4")
  df_data[, 1:10] <- sapply(df_data[, 1:10], as.numeric) # maak van de waarden numerics

  # stel df_verd met aantallenverdelingen samen
  df_verd <- as.data.frame(t(df_data[,c("StierenVerd","KoeienVerd","VaarzenVerd")]))
  colnames(df_verd) <- kolomkoppen_hvmd
  df_verd$dcat <- c("Stieren","Koeien","Vaarzen")

  # stel df_gemg met gemiddelde gewichten samen
  df_gemg <- as.data.frame(t(df_data[,c("Stieren","Koeien","Vaarzen",
                                        "Kalveren 8-12 mnd",
                                        "Lammeren","Varkens","Volwassen schapen")]))
  colnames(df_gemg) <- kolomkoppen_hvmd
  df_gemg$dcat <- c("Stieren","Koeien","Vaarzen",
                    "Kalveren 8-12 mnd","Lammeren","Varkens","Volwassen schapen")

  return(list(df_verd=df_verd, df_gemg=df_gemg))
}


##### inputbestand met gemiddelde slachtgewichten van kalveren 0-8 mnd ########
#' lees de cellen die gebruikt moeten worden van het geleverde
#' bestand met gemiddelde slachtgewichten van kalveren 0-8 mnd.
#'
#' @param inputmap string
#' @param inputbst string
#' @param jrmnd list of string
#' @return df_kalv dataframe met de cellen die gelezen moeten worden
#' @example
#' > inl_lees_input_gemg_kalv(inputbestand, inputbestand_vj, jrmnd)
#'                   hvmd_mnd1 hvm_mnd2 hvm_mnd3 hvm_mnd4             dcat
#' Kalveren 0-8 mnd     120.35   135.62   129.32  131.69  Kalveren 0-8 mnd
inl_lees_input_gemg_kalv <- function(inputbestand, inputbestand_vj, jrmnd) {
  mnd1    <- substr(jrmnd$mnd1, 5, 6)
  jr1     <- substr(jrmnd$mnd1, 1, 4)
  jr4     <- substr(jrmnd$mnd4, 1, 4)
  tab_jr1 <- paste("Slachtgewichten Kalveren", jr1)
  tab_jr4 <- paste("Slachtgewichten Kalveren", jr4)
  celbereik <- "B10:B21"
  eerste_rij <- as.numeric(mnd1)
  laatste_rij <- eerste_rij + 3
  kolomnamen <-  c("Kalveren 0-8 mnd")

  # als mnd1 < 10 -> 1 bestand/tabblad
  if(mnd1<"10"){
    df_data <- as.data.frame(read_excel(path=inputbestand, sheet=tab_jr1,
                                        range=celbereik, col_names=kolomnamen))
    df_data <- df_data[eerste_rij:laatste_rij, , drop=FALSE] # drop=FALSE behoudt de dimensie van het dataframe
  }
    # als mnd1 10, 11 of 12 -> 2 bestanden/tabbladen
    else {
    df_data1 <- as.data.frame(read_excel(path=inputbestand_vj, sheet=tab_jr1,
                                         range=celbereik, col_names=kolomnamen))
    df_data2 <- as.data.frame(read_excel(path=inputbestand, sheet=tab_jr4,
                                         range=celbereik, col_names=kolomnamen))

    if(mnd1 == "10") {
      df_data <- rbind(df_data1[10:12, , drop=FALSE], df_data2[1:1, , drop=FALSE])
    } else if(mnd1 == "11") {
      df_data <- rbind(df_data1[11:12, , drop=FALSE], df_data2[1:2, , drop=FALSE])
    } else if(mnd1 == "12") {
      df_data <- rbind(df_data1[12:12, , drop=FALSE], df_data2[1:3, , drop=FALSE])
    }
  }
  kolomkoppen_hvmd <- c("hvm_mnd1", "hvm_mnd2", "hvm_mnd3", "hvm_mnd4")
  df_data[, 1:1] <- sapply(df_data[, 1:1], as.numeric) # maak van de waarden numerics

  df_data <- as.data.frame(t(df_data))
  colnames(df_data) <- kolomkoppen_hvmd
  df_data$dcat <- c("Kalveren 0-8 mnd")

  return(df_kalv=df_data)
}


##### ctcr-bestand met aantallen roodvlees ####################################
#' Inlezen aantallen roodvlees uit controle- en correctiebestand als 
#' platte tabel. Verwacht dat het bestand een is_biologisch kolom heeft 
#'
#' Haal gevalideerde gegevens van deze verwerkingsmaand op uit het ctcr-bestand
#' en zet deze in een platte tabel.
#'
#' @param bst_aant_gaaf string met pad naar ctcr-bestand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @return df dataframe met platgeslagen data uit ctcr-bestand
#' @examples
#' > head(inl_lees_ctcr_aant_rdvl(bst_aant_gaaf, maanden_vorige_vm,
#'                                maanden_huidige_vm, jrmnd))
#'    vwmd   vsmd    wrkp         slnm         dsrt    aant
#'  201712 201712 1770400   BEDRIJFSNAAM     Vaarzen  5258122
#'  201712 201801 1770400   BEDRIJFSNAAM     Vaarzen  550000
#'  201712 201802 1770400   BEDRIJFSNAAM     Vaarzen  550000
inl_lees_ctcr_aant_rdvl <- function (bst_aant_gaaf, maanden_huidige_vm, jrmnd) {
    df <- as.data.frame(read_excel(path=bst_aant_gaaf,
                                   sheet="aantallen", col_names=FALSE, skip=2))
    maanden_vorige_vm <- c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3")
    colnames(df) <- c("slnm", "wrkp", "dsrt", "is_biologisch", maanden_vorige_vm,
                      maanden_huidige_vm, "bron", "ind1", "ind2", "ind3")
    
    # relevant kolommen
    df <- df[,c("slnm", "wrkp", "dsrt", "is_biologisch", maanden_huidige_vm)]
    
    # voeg kolom vwmd toe
    df$vwmd <- jrmnd$vwmd 
    
    # zet kolommen in juiste volgorde
    df <- df[c("vwmd", "wrkp", "slnm", "dsrt", "is_biologisch", maanden_huidige_vm)]
    
    df <- inl_sla_draaitabel_plat(
      df=df, 
      kols_res=c("vwmd", "vsmd", "wrkp", "slnm", "dsrt", "is_biologisch", "aant")
    )
    
    return(df)
}


##### ctcr-bestand met aantallen witvlees #####################################
#' Inlezen aantallen witvlees uit controle- en correctiebestand
#' als platte tabel.
#'
#' Haal gevalideerde gegevens van deze verwerkingsmaand op uit het ctcr-bestand
#' en zet deze in een platte tabel.
#'
#' @param bst_aant_gaaf string met pad naar ctcr-bestand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @return df dataframe met platgeslagen data uit ctcr-bestand
#' @examples
#' > head(inl_lees_ctcr_aant_wtvl(bst_aant_gaaf, maanden_vorige_vm,
#'                                maanden_huidige_vm, jrmnd))
#'    vwmd   vsmd    wrkp         slnm         dsrt    aant
#'  201712 201712 1770400   BEDRIJFSNAAM Vleeskuikens 5258122
#'  201712 201801 1770400   BEDRIJFSNAAM Vleeskuikens 5550000
#'  201712 201802 1770400   BEDRIJFSNAAM Vleeskuikens 5550000
inl_lees_ctcr_aant_wtvl <- function (bst_aant_gaaf, maanden_huidige_vm, jrmnd) {
  df <- as.data.frame(read_excel(path=bst_aant_gaaf,
                                 sheet="aantallen", col_names=FALSE, skip=2))
  maanden_vorige_vm <- c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3")
  colnames(df) <- c("slnm", "wrkp", "dsrt", maanden_vorige_vm,
                    maanden_huidige_vm, "ind1", "ind2", "ind3")
  df <- df[,c("slnm", "wrkp", "dsrt", maanden_huidige_vm)] # relevant kolommen
  df$vwmd <- jrmnd$vwmd # voeg kolom vwmd toe
  df <- df[c("vwmd", "wrkp", "slnm", "dsrt", maanden_huidige_vm)] # zet kolommen in juiste volgorde
  df <- inl_sla_draaitabel_plat(df=df, kols_res=c("vwmd", "vsmd", "wrkp",
                                                  "slnm", "dsrt", "aant"))
  return(df)
}


##### ctcr-bestand met aantallenverdelingen ###################################
#' Inlezen aantallenverdelingen uit controle- en correctiebestand als platte
#' tabel.
#'
#' Haal gevalideerde gegevens van deze verwerkingsmaand op uit het ctcr-bestand
#' en zet deze in een platte tabel.
#'
#' @param bst_gaaf string met pad naar ctcr-bestand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @return df dataframe met platgeslagen data uit ctcr-bestand
#' @examples
#' > head(inl_lees_ctcr_verd(bst_gaaf, maanden_vorige_vm, maanden_huidige_vm,
#'                           jrmnd))
#'   vwmd   vsmd               dsrt    dcat      verd
#' 201712 201712 Volwassen runderen Stieren      9.47
#' 201712 201801 Volwassen runderen Stieren  8.620308
#' 201712 201712 Volwassen runderen  Koeien      88.2
#' 201712 201801 Volwassen runderen  Koeien 89.049692
inl_lees_ctcr_verd <- function(bst_gaaf, maanden_huidige_vm, jrmnd) {
    df <- as.data.frame(read_excel(path=bst_gaaf, sheet="aantallenverdelingen",
                                   range="A3:H7", col_names=FALSE))
    maanden_vorige_vm <- c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3")
    colnames(df) <- c("dcat", maanden_vorige_vm, maanden_huidige_vm)
    df <- df[,c("dcat", maanden_huidige_vm)] # relevant kolommen
    df$vwmd <- jrmnd$vwmd # voeg kolom vwmd toe
    df <- inl_voeg_dsrt_toe(df=df) # voeg kolom dsrt toe obv dcat
    df <- df[c("vwmd", "dsrt", "dcat", maanden_huidige_vm)] # zet kolommen in juiste volgorde
    df <- inl_sla_draaitabel_plat(df=df, kols_res=c("vwmd", "vsmd", "dsrt",
                                                    "dcat", "verd"))
    return(df)
}


##### ctcr-bestand met gemiddelde gewichten ###################################
#' Inlezen gemiddelde gewichten uit controle- en correctiebestand als platte
#' tabel.
#'
#' Haal gevalideerde gegevens van deze verwerkingsmaand op uit het ctcr-bestand
#' en zet deze in een platte tabel.
#'
#' @param bst_gaaf string met pad naar ctcr-bestand
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param jrmnd list met verslagmaanden en verwerkingsmaand. Resultaat van
#'              alg_vind_jaarmaanden()
#' @return df dataframe met platgeslagen data uit ctcr-bestand
#' @examples
#' > head(inl_lees_ctcr_gemg(bst_gaaf, maanden_vorige_vm, maanden_huidige_vm,
#'                           jrmnd))
#'   vwmd   vsmd    dcat   gemg
#' 201712 201712 Stieren 450.78
#' 201712 201801 Stieren 450.78
#' 201712 201712  Koeien 299.98
inl_lees_ctcr_gemg <- function(bst_gaaf, maanden_huidige_vm, jrmnd) {
    df <- as.data.frame(read_excel(path=bst_gaaf, sheet="gemiddelde_gewichten",
                                   range="A3:H22", col_names=FALSE))
    maanden_vorige_vm <- c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3")
    colnames(df) <- c("dcat", maanden_vorige_vm, maanden_huidige_vm)
    df <- df[,c("dcat", maanden_huidige_vm)] # relevant kolommen
    df$vwmd <- jrmnd$vwmd # voeg kolom vwmd toe
    df <- df[c("vwmd", "dcat", maanden_huidige_vm)] # zet kolommen in juiste volgorde
    df <- inl_sla_draaitabel_plat(df=df, kols_res=c("vwmd", "vsmd",
                                                    "dcat", "gemg"))
    return(df)
}


##### analysebestand met tijdreeksen ###########################################
#' Inlezen voorlopige data uit database voor analysebestand tijdreeksen als
#' platte tabel.
#'
#' @param eerste_vlp_mnd string eerste voorlopige jaarmaand --- Wordt niet gebruikt dus kan weg?
#' @param vwmd string met verwerkingsmaand
#' @param is_biologisch integer waarde voor is_biologisch filter (0 of 1)
#' @param schema string met databaseschema
#' @return vlp_data dataframe met voorlopige data
#' @example
#' > inl_lees_vlp_tijdreeksen(eerste_vlp_mnd, vwmd)
#'   dcat   vsmd        totaal is_biologisch
#' Duiven 201712    223.000000 0
#' Duiven 201801    450.000000 0
#' Eenden 201712 678292.000000 0
#' Eenden 201801     40.000000 0
#' Eenden 201801      3.000000 1
inl_lees_vlp_tijdreeksen <- function(eerste_vlp_mnd, vwmd,
                                     schema=App$databaseschema) {

    sqlstr <- paste0("select dcat, vsmd, tota_gaaf as totaal, is_biologisch from ",
                     schema, ".tbl_statbase_dcat ",
                     "where vwmd = '", as.integer(vwmd), "' ",
                     "order by dcat, vsmd")
    vlp_data <- dat_lees_db(sqlstr)

    if (nrow(vlp_data) > 0) {
        colnames(vlp_data) <- c("dcat","vsmd","totaal","is_biologisch")
    } else {
        logdebug(msg="Geen historische gegevens gevonden")
        # als query niets teruggeeft, geef leeg dataframe terug
        vlp_data <- data.frame("dcat"=character(), "vsmd"=character(),
                               "totaal"=character(), "is_biologisch"=character(),
                               stringsAsFactors=FALSE)
    }

    return(vlp_data)
}


#' Inlezen definitieve data uit database voor analysebestand tijdreeksen als
#' platte tabel. De database kan voor eenzelfde dcat en vsmd mogelijk twee
#' rijen teruggeven: een rij voor wel en niet biologisch geslachte dieren
#'
#' @param eerste_def_mnd string januari van het eerste (definitieve) jaar wat
#'                       getoond wordt in het analysebestand
#' @param eerste_vlp_mnd string eerste voorlopige jaarmaand
#' @param schema string met databaseschema
#' @return hist_data dataframe met definitieve data
#' @example
#' > inl_lees_hist_tijdreeksen(eerste_def_mnd, eerste_vlp_mnd)
#'   dcat   vsmd     totaal is_biologisch
#' Duiven 201610 877.000000 0
#' Duiven 201611 584.000000 0
#' Duiven 201612 284.000000 0
#' Duiven 201612  40.000000 1
inl_lees_hist_tijdreeksen <- function(eerste_def_mnd, eerste_vlp_mnd,
                                     schema=App$databaseschema) {

    sqlstr <- paste0("select dcat, vsmd, tota_gaaf as totaal, is_biologisch from ",
                     schema, ".tbl_statbase_dcat_def ",
                     "where vsmd >= '", as.integer(eerste_def_mnd), "' ",
                     "and vsmd < '", as.integer(eerste_vlp_mnd), "' ", 
                     "order by dcat, vsmd")
    hist_data <- dat_lees_db(sqlstr)

    if (nrow(hist_data) > 0) {
        colnames(hist_data) <- c("dcat","vsmd","totaal","is_biologisch")
    } else {
        logdebug(msg="Geen historische gegevens gevonden")
        # als query niets teruggeeft, geef leeg dataframe terug
        hist_data <- data.frame("dcat"=character(), "vsmd"=character(),
                                "totaal"=character(), "is_biologisch"=character(),
                                stringsAsFactors=FALSE)
    }

    return(hist_data)
}

##### analysebestand met bijschattingen #######################################
#' Inlezen voorlopige en definitieve data uit database voor analysebestand
#' bijschattingen als draaitabel.
#'
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param schema string met databaseschema
#' @return hist_data dataframe met definitieve data
#' @example
#'               dsrt vvm_mnd1  vvm_mnd2  vvm_mnd3 hvm_mnd1 hvm_mnd2  hvm_mnd3  hvm_mnd4
#' Volwassen runderen 9.461894 16.994385 21.901467 9.343214 9.674087 12.361537 18.329840
#'           Kalveren 1.293780  1.445925  2.553697 1.233668 1.272843  1.425504  1.087142
#'           Lammeren 3.384201  1.277139  1.812203  .876781  .865854   .964420  2.826143
inl_lees_bijschattingen <- function(jrmnd, schema=App$databaseschema){
    kolomkoppen_vvmd <- c("vvm_mnd1", "vvm_mnd2", "vvm_mnd3")
    kolomkoppen_hvmd <- c("hvm_mnd1", "hvm_mnd2", "hvm_mnd3", "hvm_mnd4")

    sqlstr <- paste0("select z.dsrt as dsrt, m1.pcbs as vsmd1, m2.pcbs as vsmd2, m3.pcbs as vsmd3, ",
                     "hm1.pcbs as hvsmd1, hm2.pcbs as hvsmd2, hm3.pcbs as hvsmd3, hm4.pcbs as hvsmd4 ",
                     "from (select distinct dsrt, vwmd from ", schema, ".tbl_statbase_dsrt ",
                     "where vwmd='", as.integer(jrmnd$mnd3), "' and is_biologisch = 0) as z ",
                     "left join (select dsrt, vwmd, pcbs from ", schema , ".tbl_statbase_dsrt ",
                     "where vsmd='", as.integer(jrmnd$mnd1), "' and vwmd = '", as.integer(jrmnd$vvwmd), "' and is_biologisch = 0) as m1 on z.dsrt = m1.dsrt ",
                     "left join (select dsrt, vwmd, pcbs from ", schema , ".tbl_statbase_dsrt ",
                     "where vsmd='", as.integer(jrmnd$mnd2), "' and vwmd = '", as.integer(jrmnd$vvwmd), "' and is_biologisch = 0) as m2 on z.dsrt = m2.dsrt ",
                     "left join (select dsrt, vwmd, pcbs from ", schema , ".tbl_statbase_dsrt ",
                     "where vsmd='", as.integer(jrmnd$mnd3), "' and vwmd = '", as.integer(jrmnd$vvwmd), "' and is_biologisch = 0) as m3 on z.dsrt = m3.dsrt ",
                     "left join (select dsrt, vwmd, pcbs from ", schema , ".tbl_statbase_dsrt ",
                     "where vsmd='", as.integer(jrmnd$mnd1), "' and vwmd = '", as.integer(jrmnd$vwmd), "' and is_biologisch = 0) as hm1 on z.dsrt = hm1.dsrt ",
                     "left join (select dsrt, vwmd, pcbs from ", schema , ".tbl_statbase_dsrt ",
                     "where vsmd='", as.integer(jrmnd$mnd2), "' and vwmd = '", as.integer(jrmnd$vwmd), "' and is_biologisch = 0) as hm2 on z.dsrt = hm2.dsrt ",
                     "left join (select dsrt, vwmd, pcbs from ", schema , ".tbl_statbase_dsrt ",
                     "where vsmd='", as.integer(jrmnd$mnd3), "' and vwmd = '", as.integer(jrmnd$vwmd), "' and is_biologisch = 0) as hm3 on z.dsrt = hm3.dsrt ",
                     "left join (select dsrt, vwmd, pcbs from ", schema , ".tbl_statbase_dsrt ",
                     "where vsmd='", as.integer(jrmnd$mnd4), "' and vwmd = '", as.integer(jrmnd$vwmd), "' and is_biologisch = 0) as hm4 on z.dsrt = hm4.dsrt ",
                     "left join dbo.tbl_statbase_dsrt_sortering sort on z.dsrt = sort.dsrt order by sort.volgorde")
    data <- dat_lees_db(sqlstr)

    if (nrow(data) > 0) {
        colnames(data) <- c("dsrt", kolomkoppen_vvmd, kolomkoppen_hvmd)
    } else {
        logdebug(msg="Geen historische gegevens gevonden")
        # als query niets teruggeeft, geef leeg dataframe terug
        data <- data.frame("dcat"=character(), "vvm_mnd1"=character(),
                           "vvm_mnd2"=character(), "vvm_mnd3"=character(),
                           "hvm_mnd1"=character(), "hvm_mnd2"=character(),
                           "hvm_mnd3"=character(), "hvm_mnd4"=character(),
                           stringsAsFactors=FALSE)
    }

    return(data)
}


#' Verzamel de gegevens uit de database voor de output
#'
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden().
#' @param drivernaam String
#' @param servernaam String
#' @param databasenaam String
#' @param tabel String
#' @param schema String
#'
#' @return df_data dataframe met data die nodig zijn voor het genereren
#' van output.
#' @example.
#' > verzamel_data_output(jrmnd)
#            dcat vsmd1_aant vsmd1_totg vsmd2_aant vsmd2_totg vsmd3_aant vsmd3_totg vsmd4_aant vsmd4_totg
# Stieren Stieren   6818.521  3011809.0   5806.246  2617339.8   5103.567  2255632.9   5203.201  2330581.1
# Koeien   Koeien  53286.620 16021155.2  54077.184 16222073.7  52720.980 15789300.8  43218.655 13062060.4
# Vaarzen Vaarzen   1433.859   316581.7   1428.570   300442.5   1379.453   290515.6   1155.144   249615.1
inl_lees_statbase_output <- function(jrmnd,
                                     drivernaam=App$dbdrivernaam,
                                     servernaam=App$dbservernaam,
                                     databasenaam=App$databasenaam,
                                     tabel=App$tbl_statbase_dcat,
                                     schema=App$databaseschema){
  
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = drivernaam,
                        Server = servernaam,
                        Database = databasenaam
  )
  
  df_dcat <- con |> 
    dplyr::tbl(dbplyr::in_schema(schema, tabel)) |> 
    dplyr::filter(vwmd  == jrmnd$vwmd) |> 
    dplyr::collect()
  
  DBI::dbDisconnect(con)
  
  df_maanden <- data.frame(
    vsmd = c(jrmnd$mnd1, jrmnd$mnd2, jrmnd$mnd3, jrmnd$mnd4),
    nieuwe_kol_naam = c("vsmd1", "vsmd2", "vsmd3", "vsmd4")
    
  )
  
  data <- df_dcat |> 
    dplyr::left_join(
      df_maanden,
      by = "vsmd"
    ) |> 
    # tel is_biologisch bij elkaar op
    dplyr::summarise(
      .by = c(nieuwe_kol_naam, dcat),
      aant = sum(tota_gaaf),
      totg = sum(totg)
    ) |> 
    tidyr::pivot_wider(
      names_from = nieuwe_kol_naam,
      names_glue = "{nieuwe_kol_naam}_{.value}",
      values_from = c(aant, totg)
    ) |> 
    # kolommen in de juiste volgorde
    dplyr::select(dcat, vsmd1_aant, vsmd1_totg, vsmd2_aant, vsmd2_totg,
                  vsmd3_aant, vsmd3_totg, vsmd4_aant, vsmd4_totg)
  
  df_data <- as.data.frame(data)
  
  rownames(df_data) <- df_data$dcat
  
  return(df_data)
}


#' Een functie uitvoeren, zelfs als er waarschuwingen of fouten worden gegooid,
#' en alle outputs, errors, warnings of messages vastleggen. Gebruikt de
#' purrr-functies quietly en safely. Gebaseerd op https://github.com/tidyverse/purrr/issues/843
#'
#' @param fun De functie
#' @param ... Parameters voor fun
#'
#' @return een lijst
#' @export
veilig_en_stil <- function(fun, ...){
  veilig_fun <- purrr::quietly(purrr::safely(fun))
  out_veilig <- veilig_fun(...)
  out <- list(
      result = out_veilig$result$result,
      error = out_veilig$result$error,
      output = out_veilig$output,
      warnings = out_veilig$warnings,
      messages = out_veilig$messages)
  
  if(!is.null(out$error)){
    out$error <- conditionMessage(out$error)
  }
  
  return(out)
}

#' Lees csv bestand in met kolomnamen en types uit een configuratie bestand
#'
#' @param pad character, het bestandspad
#' @param kolommen_config list, bevat de mapping tussen de kolomnamen in het
#'   bestand en typen met de gewenste kolomnamen in de output. Elk element van
#'   de lijst stelt een kolom voor. De naam van het element staat voor de
#'   kolomnaam in de output. Elk element is een lijst met twee elementen. Het
#'   element `naam` bevat de kolomnaam in het bestand, het element `type` bevat
#'   het type dat de kolom moet hebben. De typen moeten overeenkomen met de
#'   types die zijn toegestaan door readr::cols
#' @param inlezen_functie function call. De functie die woordt gebruikt om de
#'   bestand in te lezen. Bijvoorbeeld `readr::read_csv`. De functie moet bij de
#'   `readr`-package horen
#' @param map_fouten character, pad naar de map waar het bestand met de
#'   waarschuwingen wordt opgeslagen.
#' @param stop_met_warnings bolean, bepaalt of de uitvoering wordt gestopt
#'   wanneer het lezen van de bestanden warnings geeft.
#' @param ... andere parameters voor `inlezen_functie`
#' @return een data.frame
#' @export
inl_lees_csv_bestand_met_kolommen_config <- function(
    pad,
    kolommen_config,
    inlezen_functie = readr::read_csv,
    map_fouten,
    stop_met_warnings = TRUE,
    ...) {
  
  inlezen_functie_namespace <- getNamespaceName(environment(inlezen_functie))
  
  if(inlezen_functie_namespace != "readr") {
    rlang::abort("De inlezen functie moet een functie van het readr package zijn.")
  }
  
  df_kolomnamen <- purrr::map_chr(kolommen_config,
                                  \(kol) purrr::pluck(kol, "naam")) |>
    tibble::enframe(name = "nieuwe_kolomnaam",
                    value = "oorspronkelijke_kolomnaam")
  
  
  # Transformeer input lijst naar readr cols object
  kolommen_spec_cols <-
    purrr::map(kolommen_config,
               \(kol) {
                 
                 kol_spec <- readr::cols()
                 
                 kol_config <- setNames(list(eval(kol$type)),
                                        nm = kol$naam)
                 
                 kol_spec <- append(kol_spec$cols, kol_config)
                 
                 return(kol_spec)
                 
               }
    ) |> 
    # we bewaren alleen de binnenste naam van elk element van de lijst, dus:
    #   $kolom_a
    #   $kolom_a$KOLOM_A
    #   type
    # wordt:
    #   $KOLOM_A
    #   type
    # in plaats van:
    #   $kolom_a_KOLOM_A
    #   type
    purrr::list_flatten(name_spec  = "{inner}")
  
  kolommen_spec <- readr::cols()
  
  kolommen_spec$cols <- kolommen_spec_cols
  
  
  inlees_poging <- veilig_en_stil(fun =  inlezen_functie, 
                                  file = pad,
                                  col_types = kolommen_spec, 
                                  ...)
  
  df <- inlees_poging$result
  
  # De readr functie geeft een foutmelding tijdens het lezen van het bestand
  if (length(inlees_poging$error) > 0) {
    rlang::abort(
      message = c(
        x = glue::glue("Het bestand kan niet worden gelezen.",
                       " Het fout is: '{inlees_poging$error}'")
      ))
  }
  
  # Aantal kolommen klopt niet
  if(ncol(df) != length(df_kolomnamen$nieuwe_kolomnaam)) {
    rlang::abort(
      "Het aantal kolommen in het bestand is anders dan het aantal kolommen in de config."
    )
  }
  
  # De readr functie geeft warning(s) tijdens het lezen van het bestand
  if(length(inlees_poging$warnings) > 0) {
    
    bericht <- c(
      glue::glue("We hebben een waarschuwing gedetecteerd tijdens het lezen van het bestand: {pad}")
    )
    
    df_pr <- readr::problems(df)
    
    # Voor warnings met betrekking tot parsing errors slaan we een
    # samengevatte versie op van de uitvoer van readr::problems
    if (nrow(df_pr) > 0) {
      
      bestandnaam_data <- basename(pad) |>
        stringr::str_remove(".csv")
      
      bestandnaam_fouten <- glue::glue("fouten_in_{bestandnaam_data}.csv")
      
      df_pr |>
        dplyr::summarise(
          .by = c(file, col, expected),
          rijen = stringr::str_c(row, collapse = ", ")
        ) |>
        readr::write_csv2(
          file = file.path(map_fouten, bestandnaam_fouten)
        )
      
      bericht <- c(
        bericht,
        "i" = "Deze werden veroorzaakt doordat in sommige kolommen onverwachte waarden werden gedetecteerd.",
        "i" = glue::glue(
          "Een bestand ({bestandnaam_fouten}) met de fouten was in {map_fouten} opgeslagd.")
      )
    } else {
      
      bericht <- c(
        bericht,
        "i" = glue::glue("Het warningsbericht is: {inlees_poging$warning}")
      )
    }
    
    if (stop_met_warnings) {
      bericht <- c(
        bericht,
        "x" = "De uitvoering werd dan gestopt."
      )
      rlang::abort(
        message = bericht
      )
    } else {
      rlang::warn(
        message = bericht
      )
    }
  }
  
  colnames(df) <- colnames(df) |>
    tibble::enframe(name = "idx",
                    value = "oorspronkelijke_kolomnaam") |>
    dplyr::left_join(df_kolomnamen,
                     by = dplyr::join_by(oorspronkelijke_kolomnaam)) |>
    dplyr::arrange(idx) |>
    dplyr::pull(nieuwe_kolomnaam)
  
  df
}

#' Lees excel bestand in met kolomnamen en types uit een configuratie bestand.
#' Dit is slechts een wrapper rond readxl::read_excel. Hiermee kunnen de namen
#' en types uit het configuratiebestand worden gebruikt
#'
#' @param pad character, het bestandspad
#' @param kolommen_config list, bevat de mapping tussen de kolomnamen in het
#'   bestand en typen met de gewenste kolomnamen in de output. Elk element van
#'   de lijst stelt een kolom voor. De naam van het element staat voor de
#'   kolomnaam in de output. Elk element is een lijst met twee elementen. Het
#'   element `naam` bevat de kolomnaam in het bestand, het element `type` bevat
#'   het type dat de kolom moet hebben. De typen moeten overeenkomen met de
#'   types die zijn toegestaan door readxl::read_excel
#' @param sheet numeric of character, het index of naam van het excelblad
#' @param map_fouten character, pad naar de map waar het bestand met de
#'   waarschuwingen wordt opgeslagen.
#' @param stop_met_warnings bolean, bepaalt of de uitvoering wordt gestopt
#'   wanneer het lezen van de bestanden warnings geeft.
#'
#' @return een data.frame
#' @export
inl_lees_excel_bestand_met_kolommen_config <- function(
    pad,
    kolommen_config,
    sheet = 1,
    map_fouten,
    stop_met_warnings = TRUE) {
  
  kolomnamen_in_output <- names(kolommen_config)
  oorspronkelijke_kolomnamen <- purrr::map_chr(kolommen_config, \(kol) kol$naam)
  kolomtypes <- purrr::map_chr(kolommen_config, \(kol) kol$type)
  
  
  if(!all(unique(kolomtypes) %in%  c("text", "numeric", "date", "logical"))) {
    rlang::abort(
      "Een van de types is niet geldig voor readxl::read_excel functie.
      Type moet zijn een van: 'text', 'numeric', 'date' of 'logical'")
  }
  
  inlees_poging <- veilig_en_stil(fun =  readxl::read_excel, 
                                  path = pad, sheet = sheet,
                                  col_types = kolomtypes)
  
  # read_excel geeft een foutmelding tijdens het lezen van het bestand
  if (length(inlees_poging$error) > 0) {
    rlang::abort(
      message = c(
        x = glue::glue("Het bestand kan niet worden gelezen.",
                       " Het fout is: '{inlees_poging$error}'")
      ))
  }
  
  # kolomnamen kloppen niet
  kolomnamen_bestand <- colnames(inlees_poging$result)
    
  diff_kolommen <- setdiff(oorspronkelijke_kolomnamen, kolomnamen_bestand)
  if (length(diff_kolommen) > 0) {
    
    idx <- which(oorspronkelijke_kolomnamen %in% diff_kolommen)
    rlang::abort(
      message = c(
        glue::glue("Fout bij het lezen van het Excel-bestand {pad}"),
        "x" = glue::glue(
          "De verwachte kolomnamen voor positie {idx} is",
          " {oorspronkelijke_kolomnamen[idx]} gebaseerd op het configuratiebestand.",
          " Maar in werkelijkheid is de naam in het Excel-bestand {kolomnamen_bestand[idx]}."
        ),
        "i" = "Corrigeer het configuratiebestand zodat het overeenkomt met de namen in het Excel-bestand."
        )
      )
    }
  
  # read_excel geeft warnings tijdens het lezen van het bestand
  if(length(inlees_poging$warnings) > 0) {
    
      bericht <- c(
        glue::glue("We hebben een waarschuwing gedetecteerd tijdens het lezen van het bestand: {pad}"),
        "x" = "De uitvoering werd dan gestopt."
      )

      bestandnaam_data <- basename(pad) |>
        stringr::str_remove(".xlsx|.xls")

      bestandnaam_fouten <- glue::glue("fouten_in_{bestandnaam_data}.csv")

      parsing_error_warnings <- inlees_poging$warnings |>
        stringr::str_subset(
          pattern = "Expecting [a-z]* in [A-Z]*[0-9]*"
        )
      
      # Als de waarschuwingen het gevolg zijn van parsingfouten, verwerken we de
      # waarschuwingsberichten, vatten we ze samen en slaan we de samenvatting
      # op in een csv-bestand
      if (length(parsing_error_warnings) > 0) {

        parsed_warnings <- stringr::str_match(
          parsing_error_warnings,
          pattern = "Expecting (?<expected>[a-z]*) in (?<kolrij>[A-Z]*[0-9]*)"
        )

        parsed_warnings |>
          as.data.frame() |>
          dplyr::select(expected, kolrij) |>
          tidyr::separate_wider_regex(
            cols = kolrij,
            patterns = c(col = "[A-Z]*", "", rij = "[0-9]*")) |>
          dplyr::summarise(
            .by = c(col, expected),
            rijen = stringr::str_c(rij, collapse = ", ")
          ) |>
          dplyr::mutate(
            file = pad
          ) |>
          dplyr::relocate(file, .before = col) |>
          readr::write_csv2(
            file = file.path(map_fouten, bestandnaam_fouten)
          )

        bericht <- c(
          bericht,
          "i" = glue::glue(
            "Een bestand ({bestandnaam_fouten}) met de fouten was in {map_fouten} opgeslagd."
            )
        )
      } else {
        bericht <- c(
          bericht, 
          "i" = glue::glue("Het warningsbericht is: {inlees_poging$warning}")
        )
      }
      if(stop_met_warnings) {
        rlang::abort(
          message = bericht
        )
      } else {
        rlang::warn(
          message = bericht
        )
      }
      
  }
  
  df <- inlees_poging$result
  
  df_kolomnamen <- data.frame(
    nieuwe_kolomnaam = kolomnamen_in_output,
    oorspronkelijke_kolomnaam = oorspronkelijke_kolomnamen
  ) 
 
  colnames(df) <- colnames(df) |>
    tibble::enframe(name = "idx",
                    value = "oorspronkelijke_kolomnaam") |>
    dplyr::left_join(df_kolomnamen,
                     by = dplyr::join_by(oorspronkelijke_kolomnaam)) |>
    dplyr::arrange(idx) |>
    dplyr::pull(nieuwe_kolomnaam)
  
  df
}

#' Controleer of een type een van de geldige opties is
#'
#' De functie controleert eenvoudig of een waarde in een verzameling mogelijke
#' waarden zit, maar voegt een aangepaste foutmelding toe.
#'
#' @param input_type vector van length 1. De type om te controleren.
#' @param geldige_types vector. De gelidge opties.
#' @param call environemnt. De omgeving van waaruit de functie wordt
#'   aangeroepen. Dit wordt gebruikt om de functienaam te bepalen die in het
#'   foutbericht wordt weergegeven.
#'
#' @return error call of niets.
#' @export
check_input_type <- function(
    input_type, 
    geldige_types = c("numeric", "character", "logical", "date"), 
    call = rlang::caller_env()) {
  
  if (!(input_type %in% geldige_types)) {
    opties <- paste0(geldige_types, collapse = ', ')
    rlang::abort(
      message = c(
      "x" = glue::glue("Input type ({input_type}) niet geldig."),
      "i" = glue::glue("Type moet een van de volgende zijn: {opties}.")
      ),
      call = call
    )    
  }
}

#' Het type van een vector wijzigen
#'
#' @param x vector
#' @param type string, het type waarin `x` moet worden gewijzigd. Kan een van de
#'   volgende zijn: numeric, logical, character, of date.
#' @param kol_naam string, optionele waarde die de naam van de kolom
#'   specificeert waar x wordt gevonden. Wordt alleen gebruikt in het
#'   warning bericht als er problemen met type verandering worden gevonden. Dit
#'   wordt gebruikt wanneer de functie wordt aangeroepen voor de kolommen van
#'   een data.frame.
#'
#' @return Als het type kan worden gewijzigd, retourneert een vector met
#'   dezelfde lengte als `x` maar met het juiste type. Als een van de waarden
#'   niet kan worden getransformeerd, worden deze teruggegeven als NA's en wordt
#'   hun positie in `x` gespecificeerd in een warning bericht.
#' @export
verander_type <- function(x, type, kol_naam = NULL) {
  
  check_input_type(input_type = type)
  
  nieuwe_waardes <- switch (type,
                         numeric = as.numeric(x),
                         logical = as.logical(x),
                         character = as.character(x),
                         date = lubridate::as_date(x)
  )
  
  is_na_in_x <- is.na(x)
  is_na_in_nieuwe_waardes <- is.na(nieuwe_waardes)
  
  if(sum(is_na_in_x) != sum(is_na_in_nieuwe_waardes)) {
    
    idx <- which(is_na_in_nieuwe_waardes == is_na_in_x)  |> 
      paste0(collapse = ", ")
    
    if (!is.null(kol_naam)) {
      msg <- glue::glue(
        "Probleem gevonden bij het parsen van kolom {kol_naam}.",
        " Het verwachte type is {type}, maar de kolom werd gelezen als {typeof(x)}.",
        " Bij het automatisch converteren naar type `{type}` werden problemen gevonden in rijen:",
        " {idx}."
      )
    } else {
      msg <- glue::glue(
        "Probleem gevonden bij het parsen van de input vector.",
        " Het verwachte type is {type}, maar de vector werd gelezen als {typeof(x)}.",
        " Bij het automatisch converteren naar type `{type}` werden problemen gevonden in posities:",
        " {idx}."
      )
    }
    
    warning(msg)
     
  }
  nieuwe_waardes
}


#' Lees excel bestand in met kolomnamen en types uit een configuratie bestand.
#' Dit is slechts een wrapper rond haven::read_sav maar het is ook mogelijk om
#' kolommen in te stellen op specifieke typen.. Hiermee kunnen de namen en types
#' uit het configuratiebestand worden gebruikt
#'
#' @param pad character, het bestandspad
#' @param kolommen_config list, bevat de mapping tussen de kolomnamen in het
#'   bestand en typen met de gewenste kolomnamen in de output. Elk element van
#'   de lijst stelt een kolom voor. De naam van het element staat voor de
#'   kolomnaam in de output. Elk element is een lijst met twee elementen. Het
#'   element `naam` bevat de kolomnaam in het bestand, het element `type` bevat
#'   het type dat de kolom moet hebben. De typen kunnen een van de volgende
#'   zijn: numeric, logical, character, of date.
#' @param map_fouten character, pad naar de map waar het bestand met de
#'   waarschuwingen wordt opgeslagen.
#' @param stop_met_warnings bolean, bepaalt of de uitvoering wordt gestopt
#'   wanneer het lezen van de bestanden warnings geeft.
#'
#' @return een data.frame
#' @export
inl_lees_sav_bestand_met_kolommen_config <- function(
    pad,
    kolommen_config,
    map_fouten,
    stop_met_warnings = TRUE) {
  
  #  transformeer kolommen list naar data.frame. Dan is het gemakkelijker om
  #  dingen eruit te halen
  df_kolommen_config <- tibble::enframe(kolommen_config, 
                  name = "nieuwe_kolomnaam",
                  value = "waarde") |> 
    dplyr::mutate(
      oorsps_kolomnaam = purrr::map_chr(waarde, "naam"),
      type = purrr::map_chr(waarde, "type")
    ) |> 
    dplyr::select(-waarde)
  # Probeer om de bestand in te lezen
  inlees_poging <- veilig_en_stil(
    fun = haven::read_sav,
    file = pad, 
    col_select = tidyselect::matches(df_kolommen_config$oorsps_kolomnaam)
    )
  
  if(length(inlees_poging$error) > 0 | length(inlees_poging$warning)) {
    
    error_en_warnings <- c(inlees_poging$error,
                           inlees_poging$warnings)
    
    msg <- glue::glue(
      "We hebben een fout gedetecteerd tijdens het lezen van het bestand: {pad}.",
      " Het oorspronkelijke bericht(en) dat is teruggestuurd door de functie haven::read_sav is:",
      " {error_en_warnings}",
      .null = "", .na = ""
      )
    
    rlang::abort(
      message = msg
    )
  }
  # Het bestand kon worden gelezen, dus controleren we nu de types van elke
  # kolom en wijzigen deze als ze niet overeenkomen met wat de gebruiker heeft
  # opgegeven.
  df <- inlees_poging$result
  
  output <- purrr::map(colnames(df), \(kol, df, config){
    
    verwacht_type <- df_kolommen_config |> 
      dplyr::filter(
        stringr::str_detect(kol, pattern = oorsps_kolomnaam)
      ) |> 
      dplyr::pull(type)
    
    check_input_type(input_type = verwacht_type, 
                     # Dit is n = 3 omdat we de naam van de functie
                     # inl_lees_sav_bestand_met_kolommen_config willen weergeven
                     # in de foutmelding, in plaats van purrr::map
                     call = rlang::caller_env(n = 3))
    
    nieuwe_naam <- df_kolommen_config |> 
      dplyr::filter(
        stringr::str_detect(kol, pattern = oorsps_kolomnaam)
      ) |> 
      dplyr::pull(nieuwe_kolomnaam)
    
    kol_waardes <- df[[kol]]
    
    is_verwachte_type <- switch (verwacht_type,
                                 numeric =  is.numeric(kol_waardes),
                                 logical = is.logical(kol_waardes),
                                 character = is.character(kol_waardes),
                                 date = lubridate::is.Date(kol_waardes)
    )
    
    if (!is_verwachte_type) {
      poging <- veilig_en_stil(fun = verander_type,
                                    x = kol_waardes, type = verwacht_type,
                                     kol_naam = kol)
      kol_waardes <- poging$result
      berichten <- c(poging$error, poging$warnings, poging$messages) |> 
        paste0(collapse = ", ")
    } else {
      berichten <- NA
    }
      
    df_niuewe_kol <- data.frame(
      x = kol_waardes
    )
    
    colnames(df_niuewe_kol) <- nieuwe_naam
      
    list(
      kolom = df_niuewe_kol,
      berichten = berichten
    )
    
  }, df = df, config = kolommen_config) 
  
  df_output <- purrr::map(output, "kolom") |> 
    purrr::list_cbind()
  
  df_berichten <- purrr::map(output, \(x) {
    data.frame(berichten = x$berichten)
    }) |> 
    purrr::list_rbind() |> 
    dplyr::filter(nchar(berichten) > 0)
  
  
  # parsing warnings tijdens het type verandering
  if(nrow(df_berichten) > 0) {
    
    bestandnaam_data <- basename(pad) |>
      stringr::str_remove(".sav")
    
    bestandnaam_fouten <- glue::glue("fouten_in_{bestandnaam_data}.csv")
    
    # Als de waarschuwingen het gevolg zijn van parsingfouten, verwerken we de
    # waarschuwingsberichten, vatten we ze samen en slaan we de samenvatting
    # op in een csv-bestand
    readr::write_csv2(
      x = df_berichten,
      file = file.path(map_fouten, bestandnaam_fouten)
    )
    
    bericht <- c(
      glue::glue("We hebben een waarschuwing gedetecteerd tijdens het lezen van het bestand: {pad}"),
      "i" = glue::glue(
        "Een bestand ({bestandnaam_fouten}) met de fouten was in {map_fouten} opgeslagd."
      )
    )
    if(stop_met_warnings) {
      rlang::abort(
        message = bericht
      )
    } else {
      rlang::warn(
        message = bericht
      )
    }
  } 
  df_output
}

