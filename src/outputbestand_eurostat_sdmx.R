#' Schrijf de Eurostat output uit volgens SDMX standaard. Split de output
#' wanneer jaarovergang wordt verwerkt.
#'
#' @param df data.frame waar per dier is aangegeven:
#'    - dier categorie (dcat)
#'    - aantallen voor verslagmaanden 1-4 (bijv. vsmd1_aant)
#'    - totaal gewicht voor verslagmaanden 1-4 (bijv. vsmd1_totg)
#' @param jrmnd Lijst met 6 items: mnd1-mnd4, vwmd(zelfde als mnd4),
#'   vvwmd (zelfde als mnd3)
#' @param output_map String met het pad waar de outputs worden verzameld.
#' @param dsnc_dataset String. Uit config: De Dataset ID die Eurostat verwacht
#' @param waarde_dataflow String. Uit config: Dataset ID met een versie 
#'   controle code
#' @param waarde_freq String. Uit config: Frequentie waarmee cijfers worden 
#'   gegenereerd. Maandelijks (M) of jaarlijks (A).
#' @param ... voor decimalen variabele om aan leid_data_af_eurostat mee te geven 
#'
#' @return String. Tekst met bericht of het maken van het bestand is gelukt.
maak_eurostat_sdmx_per_jaar <- function(df, jrmnd, output_map,
                                        waarde_dataflow,
                                        waarde_freq, ...) {
  # definieer de eurostat kolommen
  kolomnamen <- c(
    "dataflow", "freq", "ref_area", "agriprod", "stat_char", "unit_measure",
    "time_period", "obs_value", "obs_status", "conf_status",
    "obs_comment", "obs_period", "unit_mult", "decimals"
  )

  # alle data bij elkaar verzamelen
  df_eurostat <- leid_data_af_eurostat(
    df_input = df,
    jrmnd = jrmnd,
    kolomnamen = kolomnamen,
    waarde_dataflow = waarde_dataflow,
    waarde_freq = waarde_freq,
    ...
  )

  # vind de verwerkingsmaand én de decembermaand (als er een jaarovergang is)
  parsed <- lubridate::ym(jrmnd)
  vwmd <- which.max(parsed)
  december_of_vwmd <- which.max(lubridate::month(parsed))

  # indexeer jrmnd lijst om de laatste maand per jaar mee te geven.
  # zonder jaarovergang bevat vwmd en december_of_vwmd beide de verwerkingsmaand
  datum_voor_dsnc <- jrmnd[unique(c(vwmd, december_of_vwmd))]

  geslaagd_berichten <- c()

  # schrijf eurostat data.frame naar sdmx; één file per jaar
  for (datum in datum_voor_dsnc) {
    # selecteer de rijen die bij het jaar horen
    jaar <- toString(lubridate::year(lubridate::ym(datum)))
    df_eurostat_jaar <- df_eurostat |>
      dplyr::filter(stringr::str_detect(TIME_PERIOD, jaar))

    # maak een file naam in de Dataset Naming Convention
    filenaam <- construct_dsnc(datum,
      dsnc_freq = waarde_freq
    )
    file_pad_jaar <- file.path(output_map, filenaam)

    txt <- schrijf_sdmx_csv(
      df_eurostat = df_eurostat_jaar,
      file_pad = file_pad_jaar
    )

    # verzamel de berichten of het maken van output is geslaagd
    geslaagd_berichten <- append(geslaagd_berichten, txt)
  }

  txt_geslaagd <- paste(geslaagd_berichten, collapse = " ")

  return(txt_geslaagd)
}


#' Construeer een filename volgens de dataset naming convention (DSNC)
#' van Eurostat. Bevat de structuur:
#' DSNC = DATASET_FROM_YEAR_PERIOD_IGNORE.EXTENSION waar:
#' DATASET = de naam van de dataset waar de transmmissie voor wordt gemaakt
#' FROM = het land dat het bestand verstuurt
#' YEAR = het jaar waarvoor het bestand wordt verstuurd
#' PERIOD = de periode waarvoor het bestand wordt gestuurd
#'   “0001” to “0012” = the month for monthly transmissions
#'
#' @param verwerkingsmaand named List. Bevat een string met de verwerkingsmaand
#'   in het format yyyymm
#' @param dsnc_dataset String. De Dataset ID die Eurostat verwacht
#' @param dsnc_from String. Landcode.
#' @param dsnc_extensie String. bestand extentie (.csv)
#' @param padding_maand Numeric. Om de trailing zeros te maken voor de maand.
#' @param dsnc_freq 
#'   (bijv. 0004 of 0011)

construct_dsnc <- function(verwerkingsmaand,
                           dsnc_dataset = "ANIP_MTSLS",
                           dsnc_freq = "M",
                           dsnc_from = "NL",
                           dsnc_extensie = ".csv",
                           padding_maand = 4) {
  # extraheer de verwerkingsmaand en het jaar
  parsed <- lubridate::parse_date_time(verwerkingsmaand, "ym")
  maand <- lubridate::month(parsed)
  jaar <- lubridate::year(parsed)

  # pad maand met 0'en
  maand_padded <- stringr::str_pad(
    maand,
    width = padding_maand,
    side = "left",
    pad = "0"
  )

  # paste alles samen met _ en plak .csv er achter
  estat_dsnc <- paste0(
    paste(dsnc_dataset, dsnc_freq, dsnc_from, jaar, maand_padded, sep = "_"),
    dsnc_extensie
  )

  return(estat_dsnc)
}


#' Schrijf de data.frame naar csv met sdmx opmaak. Eisen voor SDMX:
#' - UTF-8 zonder BOM
#' - waarden gescheiden met ;
#' - carriage return & Line feed (CR LN, oftewel \r\n)
#' - punt als decimaal seperator
#' - en zonder rij indexes
#'
#' @param df_eurostat data.frame met alle verwachte Eurostat kolommen.
#' @param file_pad String met de locatie waar het bestand geschreven wordt.
#'
#' @return txt String met beschrijving of de operatie is geslaagd.
schrijf_sdmx_csv <- function(df_eurostat, file_pad) {
  # schrijf de csv in SDMX format
  write.table(df_eurostat,
    file = file_pad,
    sep = ";",
    eol = "\n",
    na = "",
    dec = ".",
    row.name = FALSE,
    fileEncoding = "UTF-8"
  )

  # verifieer of outputbestand gemaakt is
  if (file.exists(file_pad)) {
    txt <- paste("bestand", basename(file_pad), "is gemaakt.")
  } else {
    txt <- paste("bestand", basename(file_pad), "is niet gemaakt.")
  }

  return(txt)
}


#' Voor alle verwachte Eurostat kolommen, leidt de data af en zet dit in een
#' data.frame. Sorteer de rijen vervolgens op dier code, dan maand, dan unit.
#'
#' @param df_input data.frame met dieren in de rijen en de volgende kolommen:
#'   - dier categorie (dcat)
#'   - aantallen voor verslagmaanden 1-4 (bijv. vsmd1_aant)
#'   - totaal gewicht voor verslagmaanden 1-4 (bijv. vsmd1_totg)
#' @param jrmnd Lijst met 6 items: mnd1-mnd4, vwmd(zelfde als mnd4),
#'   vvwmd (zelfde als mnd3)
#' @param kolomnamen Vector met de kolommennamen die in de eurostat output
#'   worden verwacht, in lowercase.
#' @param dier_codes Tribble/data.frame met de dier categorieën en de
#'   bijbehorende Eurostat codes.
#' @param kg_per_eenheid Integer, voor de conversie van het gewicht naar de
#'   verwachte eenheid (1000 Tons)
#' @param heads_per_eenheid Integer, voor de conversie van het aantal naar de
#'   verwachte eenheid (1000 Heads)
#' @param eenheid_tons String, de verwachte eenheid voor gewicht (1000 Tons)
#' @param eenheid_heads String, de verwachte eenheid voor aantal (1000 Heads)
#' @param waarde_dataflow String, constante
#' @param waarde_freq String, de tijd granulariteit, jaarlijks of maandelijks
#' @param waarde_ref_area String, uit welk land de data komt (NL)
#' @param waarde_meatitem String, soort meat; import/export/slaughter (SL)
#' @param waarde_obs_status String, of de waarde voorwaardelijk is
#' @param waarde_conf_status String, of de waarde geheimhouding heeft
#' @param waarde_obs_comment String, kolom voor commentaar van de gaafmaker
#' @param afronden_decimalen Named vector, op hoeveel decimalen de waarden 
#'   voor aantallen en gewichten worden afgerond
#'
#' @return output_df
#' data.frame met de onderstaande kolommen
#'  DATAFLOW     constante "ESTAT:ANIP_MTSLS_M(1.0)"
#'  FREQ         constante "M"
#'  REF_AREA     constante "NL"
#'  AGRIPOD      -> is meat code
#'  STAT_CHAR    constante "SL"
#'  UNIT_MEASURE "THS_HD" (1000 heads) of "THS_T" (1000 tons)
#'  TIME_PERIOD  -> jrmnd in format "2023-09"
#'  OBS_VALUE    -> the actual value (in 1000 heads or tons)
#'  OBS_STATUS   P als voorlopige cijfers
#'  CONF_STATUS  C als confidential data
#'  OBS_COMMENT  -> keep empty for graafmaker comments
#'  OBS_PERIOD  moet leeg zijn
#'  UNIT_MULT   moet leeg zijn
#'  DECIMALS    constante "3"
leid_data_af_eurostat <- function(df_input, jrmnd,
                                  kolomnamen = c(
                                    "dataflow", "freq", "ref_area", "agriprod", "stat_char", "unit_measure",
                                    "time_period", "obs_value", "obs_status", "conf_status",
                                    "obs_comment", "obs_period", "unit_mult", "decimals"
                                  ),
                                  dier_codes = tibble::tribble(
                                    ~dcat, ~agriprod,
                                    "Stieren", "B1220",
                                    "Ossen", "B1210",
                                    "Koeien", "B1230",
                                    "Vaarzen", "B1240",
                                    "Kalveren 0-8 mnd", "B1110",
                                    "Kalveren 8-12 mnd", "B1120",
                                    "Runderen totaal", "B1000",
                                    "Lammeren", "B4110",
                                    "Schapen totaal", "B4100",
                                    "Volwassen schapen", "B4190",
                                    "Geiten", "B4200",
                                    "Varkens", "B3100",
                                    "Eenhoevige dieren", "B5000",
                                    "Eenden", "B7200",
                                    "Kalkoenen", "B7300",
                                    "Kippen totaal", "B7100"
                                  ),
                                  afronden_decimalen = c(aantal=3, gewicht=3),
                                  kg_per_eenheid = 1000000,
                                  heads_per_eenheid = 1000,
                                  eenheid_tons = "THS_T",
                                  eenheid_heads = "THS_HD",
                                  waarde_dataflow = "ESTAT:ANIP_MTSLS_M(1.0)",
                                  waarde_freq = "M",
                                  waarde_ref_area = "NL",
                                  waarde_stat = "SLAUGHT",
                                  waarde_obs_status = "P",
                                  waarde_conf_status = "C",
                                  waarde_obs_comment = NA,
                                  waarde_obs_period = NA,
                                  waarde_unit_mult = NA,
                                  waarde_dec = 3) {
  # format de datum
  parsed <- lubridate::parse_date_time(jrmnd, "ym")
  time_period <- format(parsed, format = "%Y-%m")

  # transform datum naar data.frame;
  # voeg eerst de maandlabels toe (mnd1-mnd4)
  df_datum <- data.frame(time_period) |>
    dplyr::mutate(
      maanden = names(jrmnd)
    ) |> # selecteer de verwerkingsmaanden
    dplyr::filter(stringr::str_detect(maanden, "^mnd")) |> # splits de cijfers van mnd1-mnd4, om later te kunnen koppelen
    tidyr::separate_wider_regex(
      cols = maanden,
      patterns = c("mnd", maand_getal = "\\d")
    )

  # transform de input naar een long format tabel
  df_data <- df_input |>
    tidyr::pivot_longer(
      cols = -dcat,
      names_to = c("maand", "aant_of_totg"),
      values_to = c("waarde"),
      names_pattern = "vsmd([0-9])_(.+)"
    )

  # voeg de af te leiden kolommen toe
  df_output <- df_data |> # verander "aant" en "totg" units
    dplyr::mutate(
      unit_measure = dplyr::case_when(
        aant_of_totg == "aant" ~ eenheid_heads,
        aant_of_totg == "totg" ~ eenheid_tons,
        .default = NA_character_
      )
    ) |> # vervang geobserveerde NA waarden met 0
    tidyr::replace_na(
      list(
        waarde = 0
      )
    ) |> # match de waarde met de eenheid & rond af op de juiste decimalen
    dplyr::mutate(
      obs_value = dplyr::case_when(
        unit_measure == eenheid_heads ~ sprintf("%.*f", 
                                        digits = getElement(afronden_decimalen, "aantal"),
                                        waarde / heads_per_eenheid),
        unit_measure == eenheid_tons ~ sprintf("%.*f",
                                      digits = getElement(afronden_decimalen, "gewicht"),
                                      waarde / kg_per_eenheid),
        .default = NA
      )
    ) |> # vul de formatted maanden in
    dplyr::left_join(
      y = df_datum,
      by = c("maand" = "maand_getal")
    ) |> # hercodeer dieren, right join drop dieren zonder eurostat code
    dplyr::right_join(
      y = dier_codes,
      by = "dcat"
    ) |> # markeer welke maanden voorlopige waarden hebben
    dplyr::mutate(
      obs_status = dplyr::if_else(
        condition = maand == 1,
        true = NA_character_,
        false = waarde_obs_status
      )
    ) |> # markeer de dier groepen die geheim gehouden moeten worden
    dplyr::mutate(
      conf_status = dplyr::case_when(
        dcat == "Eenden" ~ waarde_conf_status,
        dcat == "Kalkoenen" ~ waarde_conf_status,
        .default = NA_character_
      )
    ) |> # voeg de kolommen met constante waarden toe
    dplyr::mutate(
      dataflow = waarde_dataflow,
      freq = waarde_freq,
      ref_area = waarde_ref_area,
      stat_char = waarde_stat,
      obs_comment = waarde_obs_comment, # OBS_COMMENT blijft leeg
      obs_period = waarde_obs_period, # moet leeg zijn
      unit_mult = waarde_unit_mult, # moet leeg zijn
      decimals = waarde_dec
    ) |> # select de relevante kolommen voor eurostat
    dplyr::select(all_of(kolomnamen))

  # uppercase de kolomnamen
  colnames(df_output) <- toupper(kolomnamen)

  # zet de waarden op de verwachte eurostat volgorde:
  # eerst per dier code, dan per maand, vervolgens per unit
  df_eurostat <- dplyr::arrange(df_output, AGRIPROD, TIME_PERIOD, UNIT_MEASURE)

  return(df_eurostat)
}

