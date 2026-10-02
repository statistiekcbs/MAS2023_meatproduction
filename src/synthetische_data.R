maak_synthetische_slachting <- function(
    d_geboorte = NULL,
    d_import = NULL,
    d_afkalven = NULL,
    dst_code = NULL,
    d_slacht = NULL,
    d_ingang_0 = NULL,
    d_ingang_1 = NULL,
    d_ingang_2 = NULL,
    d_ingang_3 = NULL,
    d_einde_0 = NULL,
    d_einde_1 = NULL,
    d_einde_2 = NULL,
    d_einde_3 = NULL,
    geslacht = NULL,
    lnd_herkomst = NULL,
    lnd_oorsprong = NULL,
    eld_code = NULL,
    levens_nr = NULL,
    code_reden_einde = "SL",
    einde_oms = "Slacht",
    ubn_0 = NULL,
    ubn_1 = NULL,
    ubn_2 = NULL,
    ubn_3 = NULL,
    bvg_type_0 = NULL,
    bvg_type_1 = NULL,
    bvg_type_2 = NULL,
    bvg_type_3 = NULL) {
  if (dst_code == FALSE) {
   data.frame(
    ELD_CODE = ifelse(is.null(eld_code), character(0), eld_code),
    LEVENSNR = ifelse(is.null(levens_nr), character(0), levens_nr),
    datum_geboorte = ifelse(is.null(d_geboorte), character(0), datum_naar_rvo_formaat(d_geboorte)),
    datum_import = ifelse(is.null(d_import), character(0), datum_naar_rvo_formaat(d_import)),
    LND_LANDCODE_HERKOMST = ifelse(is.null(lnd_herkomst), character(0), lnd_herkomst),
    GESLACHT = ifelse(is.null(geslacht), character(0), geslacht),
    EERSTE_DATUM_AFKALVEN = ifelse(is.null(d_afkalven), character(0), datum_naar_rvo_formaat(d_afkalven)),
    LND_LANDCODE_OORSPRONG = ifelse(is.null(lnd_oorsprong), character(0), lnd_oorsprong),
    datum_einde = ifelse(is.null(d_slacht), character(0), datum_naar_rvo_formaat(d_slacht)),
    CODE_REDEN_EINDE = ifelse(is.null(code_reden_einde), character(0), code_reden_einde),
    EINDE_OMS = ifelse(is.null(einde_oms), character(0), einde_oms),
    UBN_0 = ifelse(is.null(ubn_0), character(0), ubn_0),
    BVG_TYPE_0 = ifelse(is.null(bvg_type_0), character(0), bvg_type_0),
    datum_ingang_0 = ifelse(is.null(d_ingang_0), character(0), datum_naar_rvo_formaat(d_ingang_0)),
    datum_einde_0 = ifelse(is.null(d_einde_0), character(0), datum_naar_rvo_formaat(d_einde_0)),
    UBN_3 = ifelse(is.null(ubn_3), character(0), ubn_3),
    BVG_TYPE_3 = ifelse(is.null(bvg_type_3), character(0), bvg_type_3),
    datum_ingang_3 = ifelse(is.null(d_ingang_3), character(0), datum_naar_rvo_formaat(d_ingang_3)),
    datum_einde_3 = ifelse(is.null(d_einde_3), character(0), datum_naar_rvo_formaat(d_einde_3)),
    UBN_2 = ifelse(is.null(ubn_2), character(0), ubn_2),
    BVG_TYPE_2 = ifelse(is.null(bvg_type_2), character(0), bvg_type_2),
    datum_ingang_2 = ifelse(is.null(d_ingang_2), character(0), datum_naar_rvo_formaat(d_ingang_2)),
    datum_einde_2 = ifelse(is.null(d_einde_2), character(0), datum_naar_rvo_formaat(d_einde_2)),
    UBN_1 = ifelse(is.null(ubn_1), character(0), ubn_1),
    BVG_TYPE_1 = ifelse(is.null(bvg_type_1), character(0), bvg_type_1),
    datum_ingang_1 = ifelse(is.null(d_ingang_1), character(0), datum_naar_rvo_formaat(d_ingang_1)),
    datum_einde_1 = ifelse(is.null(d_einde_1), character(0), datum_naar_rvo_formaat(d_einde_1))
  ) 
  } else {
    data.frame(
      ELD_CODE = ifelse(is.null(eld_code), character(0), eld_code),
      LEVENSNR = ifelse(is.null(levens_nr), character(0), levens_nr),
      DST_CODE = ifelse(is.null(dst_code), character(0), dst_code),
      datum_geboorte = ifelse(is.null(d_geboorte), character(0), datum_naar_rvo_formaat(d_geboorte)),
      datum_import = ifelse(is.null(d_import), character(0), datum_naar_rvo_formaat(d_import)),
      LND_LANDCODE_HERKOMST = ifelse(is.null(lnd_herkomst), character(0), lnd_herkomst),
      GESLACHT = ifelse(is.null(geslacht), character(0), geslacht),
      LND_LANDCODE_OORSPRONG = ifelse(is.null(lnd_oorsprong), character(0), lnd_oorsprong),
      datum_einde = ifelse(is.null(d_slacht), character(0), datum_naar_rvo_formaat(d_slacht)),
      CODE_REDEN_EINDE = ifelse(is.null(code_reden_einde), character(0), code_reden_einde),
      EINDE_OMS = ifelse(is.null(einde_oms), character(0), einde_oms),
      UBN_0 = ifelse(is.null(ubn_0), character(0), ubn_0),
      BVG_TYPE_0 = ifelse(is.null(bvg_type_0), character(0), bvg_type_0),
      datum_ingang_0 = ifelse(is.null(d_ingang_0), character(0), datum_naar_rvo_formaat(d_ingang_0)),
      datum_einde_0 = ifelse(is.null(d_einde_0), character(0), datum_naar_rvo_formaat(d_einde_0)),
      UBN_3 = ifelse(is.null(ubn_3), character(0), ubn_3),
      BVG_TYPE_3 = ifelse(is.null(bvg_type_3), character(0), bvg_type_3),
      datum_ingang_3 = ifelse(is.null(d_ingang_3), character(0), datum_naar_rvo_formaat(d_ingang_3)),
      datum_einde_3 = ifelse(is.null(d_einde_3), character(0), datum_naar_rvo_formaat(d_einde_3)),
      UBN_2 = ifelse(is.null(ubn_2), character(0), ubn_2),
      BVG_TYPE_2 = ifelse(is.null(bvg_type_2), character(0), bvg_type_2),
      datum_ingang_2 = ifelse(is.null(d_ingang_2), character(0), datum_naar_rvo_formaat(d_ingang_2)),
      datum_einde_2 = ifelse(is.null(d_einde_2), character(0), datum_naar_rvo_formaat(d_einde_2)),
      UBN_1 = ifelse(is.null(ubn_1), character(0), ubn_1),
      BVG_TYPE_1 = ifelse(is.null(bvg_type_1), character(0), bvg_type_1),
      datum_ingang_1 = ifelse(is.null(d_ingang_1), character(0), datum_naar_rvo_formaat(d_ingang_1)),
      datum_einde_1 = ifelse(is.null(d_einde_1), character(0), datum_naar_rvo_formaat(d_einde_1))
    ) 
  }
  
}

datum_naar_rvo_formaat <- function(datum) {
  if (is.na(datum)) {
    return(NA)
  }
  paste0(
    stringr::str_pad(string = lubridate::day(datum), width = 2, side = "left", pad = "0"),
    toupper(lubridate::month(datum, label = T, abbr = T, locale = "US")),
    lubridate::year(datum)
  )
}

maak_datum_slachting <- function(
    d_geboorte,
    d_slacht,
    n_locaties = 1,
    afkalven = T,
    import = F) {
  leeg_datum <- NA

  if (!lubridate::is.Date(d_geboorte)) {
    d_geboorte <- lubridate::as_date(d_geboorte)
  }

  if (!lubridate::is.Date(d_slacht)) {
    d_slacht <- lubridate::as_date(d_slacht)
  }
  leeftijd <- d_slacht - d_geboorte

  if (n_locaties == 1) {
    df_locaties <- data.frame(
      d_ingang_1 = d_geboorte,
      d_ingang_2 = leeg_datum,
      d_ingang_3 = leeg_datum,
      d_einde_1 = d_slacht,
      d_einde_2 = leeg_datum,
      d_einde_3 = leeg_datum
    )
  } else if (n_locaties == 2) {
    verblijfsduur <- leeftijd / 2

    n_dagen <- sample(1:verblijfsduur, 1)

    df_locaties <- data.frame(
      d_ingang_1 = d_geboorte + n_dagen,
      d_ingang_2 = d_geboorte,
      d_ingang_3 = leeg_datum,
      d_einde_1 = d_slacht,
      d_einde_2 = d_geboorte + n_dagen,
      d_einde_3 = leeg_datum
    )
  } else if (n_locaties == 3) {
    verblijfsduur <- leeftijd / 3

    n_dagen_eerste_loc <- sample(1:verblijfsduur, 1)
    n_dagen_tweede_loc <- verblijfsduur - n_dagen_eerste_loc

    df_locaties <- data.frame(
      d_ingang_1 = d_geboorte + n_dagen_eerste_loc + n_dagen_tweede_loc,
      d_ingang_2 = d_geboorte + n_dagen_eerste_loc,
      d_ingang_3 = d_geboorte,
      d_einde_1 = d_slacht,
      d_einde_2 = d_geboorte + n_dagen_eerste_loc + n_dagen_tweede_loc,
      d_einde_3 = d_geboorte + n_dagen_eerste_loc
    )
  }

  d_afkalven <- if (afkalven) {
    df_af <- data.frame(
      d_afkalven = d_geboorte + sample((365 * 2):(365 * 5), 1)
    )
  } else {
    df_af <- data.frame(
      d_afkalven = leeg_datum
    )
  }

  d_import <- if (import) {
    df_imp <- data.frame(
      d_import = d_geboorte + sample(1:leeftijd, 1)
    )
  } else {
    df_imp <- data.frame(
      d_import = leeg_datum
    )
  }

  df_vaste_datum <- data.frame(
    d_geboorte = d_geboorte,
    d_slacht =   d_slacht,
    d_ingang_0 = d_slacht,
    d_einde_0 =  d_slacht
  )
  dplyr::bind_cols(df_vaste_datum, df_locaties, df_af, df_imp)
}

maak_straatnamen <- function() {
  
  c("tevredenweg", "verwachtenstraat", "negenstraat", "zingenhof", "vaderweg",
  "modernstraat", "voorzichtighof", "straffenhof", "tweedeweg",
  "vergissenstraat", "werkhof", "wantstraat", "theehof", "drinkenhof",
  "basisweg", "afnameweg", "vliegweg", "drijvenweg", "zielweg", "maagstraat",
  "verfstraat", "mooiweg", "heetstraat", "brengenweg", "planthof",
  "bellenhof", "schoolstraat", "hardhof", "geboorteweg", "actiefweg",
  "rijzenstraat", "voetweg", "frisstraat", "varkenstraat", "genietenstraat",
  "vijandhof", "vannachthof", "lompweg", "maaltijdweg", "openstraat",
  "buurmanstraat", "grasstraat", "verstandstraat", "teenweg", "kunnenstraat",
  "opnemenstraat", "duwenstraat", "westweg", "warmhof", "enthousiastweg",
  "moordenstraat", "tekenstraat", "verzamelingstraat", "schetsenhof",
  "meerderehof", "grafhof", "eigenhof", "partnerstraat", "mevrouwhof",
  "plaathof", "zeepweg", "kennenweg", "wolfhof", "wijsstraat", "beidehof",
  "openlijkweg", "vriendweg", "vingerweg", "speciaalweg", "baanweg",
  "gaanweg", "bruiloftstraat", "telefoonstraat", "terwijlhof", "hoekweg",
  "sterkweg", "wildweg", "filmhof", "trekkenhof", "bevattenhof", "rotsstraat",
  "lerenstraat", "oosthof", "plaatjeweg", "rozehof", "brugstraat", "onzehof",
  "kennisweg", "opleidingstraat", "steenweg", "zevenstraat", "raamstraat",
  "mitsweg", "hertstraat", "manierweg", "ordestraat", "begripstraat",
  "ongelukhof", "treinweg", "verkopenweg")
}

#' Maak een aantal IDs van een bepaalde lengte
#'
#' @param n numeric, aantal ids
#' @param n_cijfers numeric, aantal cijfers in ID
#'
#' @return een character vector van lengte n
#' @export
#'
#' @examples
#'
#' maak_id(n = 10, n_cijfers = 2)
maak_id <- function(n, n_cijfers) {
  
  if(n > 10^n_cijfers) {
    glue::glue("Kan niet meer dan {10^n_cijfers} unieke id's genereren met et het",
               " opgegeven aantal cijfers ({n_cijfers}). n moet < 10^n_cijfers zijn.") |> 
      stop()
  }
  
  tot <- 10^(n_cijfers) - 1
  sample(0:tot, n, replace = FALSE) |> 
    stringr::str_pad(width = n_cijfers, side = "left", pad = 0)
}

#' Maak ubns voor synthetische slachtingen bestand.
#'
#' Deze functie wordt gebruikt binnen de functie
#' `maak_synthetische_slachtingen_levering` om de kolommen ubn_2 en ubn_3 in te
#' vullen met een actueel UBN-nummer, afhankelijk van het aantal locaties van
#' een bepaalde rij in de slachtingen-tabel. De functie maakt een data.frame met
#' twee kolommen, ubn_2 en ubn_3, en wijst een willekeurig UBN-nummer toe aan
#' beide kolommen (n_locaties = 3) of aan ubn_2 (n_locaties = 2). Met de
#' argumenten ubn_1 kunt je het UBN-nummer van de kolom ubn_1 opgeven, zodat dit
#' niet wordt herhaald in de kolommen ubn_2 of ubn_3.
#'
#' @param n_locaties numeric, aantal locaties. Kan 2 of 3 zijn.
#' @param verblijfplaatsen_ubn character vector, de te gebruiken UBN's. Kan ook
#'   NULL zijn, dan worden de UBN's willekeurig aangemaakt.
#' @param ubn_1 character, indien aanwezig, wordt deze UBN verwijderd uit de
#'   UBN's in `verblijfplaatsen_ubn`
#'
#' @return data.frame met twee kolommen, ubn_2 en ubn_3
#' @export
#'
#' @examples 
#' maak_ubns(n_locaties = 2, 
#'           verblijfplaatsen_ubn = c("000000", "999999", "202020"),
#'           ubn_1 = "000000" )
maak_ubns <- function(n_locaties, verblijfplaatsen_ubn = NULL, ubn_1 = NULL) {
  
  if(n_locaties > 3) {
    rlang::abort(
      message = c(
        "x" = "Het aantal locaties is groter dan verwacht.",
        "i" = "n_locaties moet tussen 1 en 3 liggen.")
    )
  }
  
  if(n_locaties == 1) {
    
    df <- data.frame(
      ubn_2 = NA,
      ubn_3 = NA
    )
    return(df)
  }
  max_ubns_n <- n_locaties - 1
  
  if(is.null(verblijfplaatsen_ubn)) {
    ubns <- maak_id(n = max_ubns_n, n_cijfers = 6)
  } else {
    ubns <- verblijfplaatsen_ubn
  }
  
  if(!is.null(ubn_1)) {

    if(length(ubn_1) != 1) {
      rlang::abort(
        message = "ubn_1 kan slechts één waarde bevatten."
      )
    }
  
    ubns <- verblijfplaatsen_ubn[ubns != ubn_1]
  }
  
  
  if(length(ubns) < n_locaties - 1) {
      rlang::abort(
        message = c(
          "Te weinig UBN's voorzien.",
          "x" = glue::glue(
            "Je hebt {n_locaties} locaties opgegeven, maar slechts {length(verblijfplaatsen_ubn)} ubns.",
            ),
          "i" = paste0(
            "Geef hetzelfde aantal UBN's of meer dan het aantal locaties,",
            " of geef geen UBN's en deze zullen willekeurig worden gegenereerd."
          )
          
        )
      )
    }

  df <- data.frame(
      ubn_2 = sample(ubns, size = 1, replace = TRUE),
      ubn_3 = NA
    )
  
  if (n_locaties == 3) {  
    
    ubns <- ubns[ubns != df$ubn_2]
    
    df <- df |> 
      dplyr::mutate(
        ubn_3 = sample(ubns, 1, replace = TRUE)
      )
  }
  return(df)
}

maak_synthetische_slachtingen_levering <- function(
    n_regels,
    diersoort,
    eerste_datum_slacht,
    laatste_datum_slacht,
    landcodes = c("NL", "BE", "DE", "FR", "DK"),
    slachthuizen_ubn = NULL,
    verblijfplaatsen_ubn = NULL) {
  
  if(is.null(slachthuizen_ubn)) {
    sl_ubns <- maak_id(n_regels, 6)
  } else {
    sl_ubns <- sample(slachthuizen_ubn, n_regels, replace = TRUE)
  }
  
  if(is.null(verblijfplaatsen_ubn)) {
    vb_ubns <- maak_id(n_regels, 6)
  } else {
    vb_ubns <- sample(verblijfplaatsen_ubn, n_regels, replace = TRUE)
  }
  
  if(diersoort == "rund") {
    rund_afkalven <- sample(c(F, T), n_regels, replace = TRUE)
    gaap_dst_code <- F
  } else {
    # gebruik afkalven om geslacht te kiezen
    rund_afkalven <- sample(c(F,T), n_regels, replace = TRUE)
    gaap_dst_code <- sample(c(3,4), n_regels, replace = T, prob = c(0.8, 0.2))
  }
  data.frame(
    d_geboorte = sample(
      seq(lubridate::ymd("2010-01-01"),
        lubridate::ymd("2023-01-01"),
        by = "day"
      ),
      n_regels,
      replace = TRUE
    ),
    d_slacht = sample(
      seq(lubridate::ymd(eerste_datum_slacht),
        lubridate::ymd(laatste_datum_slacht),
        by = "day"
      ),
      n_regels,
      replace = TRUE
    ),
    ubn_0 = sl_ubns,
    ubn_1 = vb_ubns,
    bvg_type_0 = "SP",
    bvg_type_1 = "VH",
    bvg_type_2 = NA,
    bvg_type_3 = NA,
    levens_nr = maak_id(n_regels, 10),
    n_locaties = sample(1:3, n_regels, replace = TRUE),
    afkalven = rund_afkalven,
    import = sample(c(F, T), n_regels, replace = TRUE, prob = c(0.6, 0.4)),
    dst_code = gaap_dst_code
  ) |>
    dplyr::mutate(
      data = purrr::pmap(
        .l = list(
          d_geboorte = d_geboorte, d_slacht = d_slacht,
          afkalven = afkalven, n_locaties = n_locaties,
          import = import
        ),
        .f = maak_datum_slachting
      ),
      geslacht = ifelse(afkalven, "V", "M"),
      ubns = purrr::map2(n_locaties, ubn_1, \(n, ubn_1) maak_ubns(
        n_locaties = n,
        verblijfplaatsen_ubn = verblijfplaatsen_ubn,
        ubn_1 = ubn_1
      )),
      bvg_type_2 = ifelse(n_locaties > 1, "VH", NA),
      bvg_type_3 = ifelse(n_locaties == 3, "VH", NA),
      eld_code = ifelse(import,
        sample(landcodes, n_regels, replace = T),
        "NL"
      ),
      lnd_herkomst = ifelse(import,
        sample(landcodes, n_regels, replace = T),
        NA
      ),
      lnd_oorsprong = ifelse(import,
        sample(landcodes, n_regels, replace = T),
        NA
      ),
    ) |> 
    dplyr::select(-c(d_geboorte, d_slacht, n_locaties, afkalven, import)) |>
    tidyr::unnest(data) |>
    tidyr::unnest(ubns)
}

fout_slacht_datum <- function(df, idx) {
  df |>
    dplyr::mutate(
      d_slacht = dplyr::if_else(dplyr::row_number() == idx, NA, d_slacht)
    )
}

fout_geboorte_datum <- function(df, idx = NULL,
                                n = 1,
                                dataset = "verwerkt",
                                datum_formaat = "%d%b%Y",
                                kol_geb_datum = datum_geboorte, 
                                kol_slacht_datum = datum_slacht) {
  
  if(is.null(idx)) {
    idx <- sample(1:nrow(df), n)
  }
  
  sf <- lubridate::stamp("01APR1999", orders = datum_formaat, locale = "US_en")
  
  if(dataset == "ruw") {
    
    df_output <- df |> 
      dplyr::mutate(
        {{ kol_geb_datum }} := dplyr::if_else(
          dplyr::row_number() %in% idx,
          toupper(sf(lubridate::parse_date_time({{ kol_slacht_datum }}, orders = datum_formaat) + lubridate::ddays(1))),
          {{kol_geb_datum}}
        )
      )
  } else if (dataset == "verwerkt") {
    df_output <- df |> 
      dplyr::mutate(
        {{ kol_geb_datum }} := dplyr::if_else(
          dplyr::row_number() %in% idx,
          {{ kol_slacht_datum }} + lubridate::ddays(1),
          {{kol_geb_datum}}
        )
      )
  } else {
    df_output <- df
  }
  
  return(df_output)
}

fout_locatie_1_datum <- function(df, idx) {
  df |>
    dplyr::mutate(
      d_ingang_1 = dplyr::if_else(dplyr::row_number() == idx,
        d_ingang_1 + (d_einde_1 - d_ingang_1) + 1,
        d_ingang_1
      )
    )
}

fout_geslacht_en_afkalven_datum <- function(df, idx) {
  df |>
    dplyr::mutate(
      d_afkalven = dplyr::case_when(
        dplyr::row_number() == idx & geslacht == "M" ~ d_ingang_0 - 10,
        .default = d_afkalven
      )
    )
}


fout_uniek_levens_nr <- function(df, idx = NULL) {
  
  if(is.null(idx)) {
    idx <- sample(1:nrow(df), 1)
  }
  
  df |>
    dplyr::bind_rows(
      df |> dplyr::slice(idx)
    )
}


maak_synthetische_slachthuizen <- function(
    ubn = NULL,
    bvg_type_bedrijfsvestiging = "SP",
    bvg_postcode_plaatscode = NULL,
    bvg_postcode_lettercode = NULL,
    bvg_huisnummer = NULL,
    bvg_huisnummer_toevoeging = NULL,
    bvg_straatnaam = NULL,
    bvg_plaatsnaam = NULL,
    x_coordinaat = NULL,
    y_coordinaat = NULL,
    status = NULL,
    datum_ingang = NULL,
    datum_einde = NULL,
    datum_ingang_dst = NULL,
    datum_einde_dst = NULL,
    brs_nummer = NULL,
    kvk_nr = NULL,
    naam = NULL,
    aantal = NULL) {
  data.frame(
    UBN = ifelse(is.null(ubn), character(0), ubn),
    BVG_TYPE_BEDRIJFSVESTIGING = ifelse(is.null(bvg_type_bedrijfsvestiging), character(0), bvg_type_bedrijfsvestiging),
    BVG_POSTCODE_PLAATSCODE = ifelse(is.null(bvg_postcode_lettercode), character(0), bvg_postcode_plaatscode),
    BVG_POSTCODE_LETTERCODE = ifelse(is.null(bvg_postcode_lettercode), character(0), bvg_postcode_lettercode),
    BVG_HUISNUMMER = ifelse(is.null(bvg_huisnummer), character(0), bvg_huisnummer),
    BVG_HUISNUMMER_TOEVOEGING = ifelse(is.null(bvg_huisnummer_toevoeging), character(0), bvg_huisnummer_toevoeging),
    BVG_STRAATNAAM = ifelse(is.null(bvg_straatnaam), character(0), bvg_straatnaam),
    BVG_PLAATSNAAM = ifelse(is.null(bvg_plaatsnaam), character(0), bvg_plaatsnaam),
    X_COORDINAAT = ifelse(is.null(x_coordinaat), character(0), x_coordinaat),
    Y_COORDINAAT = ifelse(is.null(y_coordinaat), character(0), y_coordinaat),
    STATUS = ifelse(is.null(status), character(0), status),
    datum_ingang = ifelse(is.null(datum_ingang), character(0), datum_naar_rvo_formaat(datum_ingang)),
    datum_einde = ifelse(is.null(datum_einde), character(0), datum_naar_rvo_formaat(datum_einde)),
    datum_ingang_dst = ifelse(is.null(datum_ingang_dst), character(0), datum_naar_rvo_formaat(datum_ingang_dst)),
    datum_einde_dst = ifelse(is.null(datum_einde_dst), character(0), datum_naar_rvo_formaat(datum_einde_dst)),
    BRS_NUMMER = ifelse(is.null(brs_nummer), character(0), brs_nummer),
    KVK_NR = ifelse(is.null(kvk_nr), character(0), kvk_nr),
    NAAM = ifelse(is.null(naam), character(0), naam),
    aantal = ifelse(is.null(aantal), numeric(0), aantal)
  )
}

maak_synthetische_verblijfsplaatsen <- function(
    ubn = NULL,
    relnr = NULL) {
  data.frame(
    UBN_NUMMER = ifelse(is.null(ubn), character(0), ubn),
    Relnr = ifelse(is.null(relnr), character(0), relnr)
  )
}

maak_synthetische_landbouwtelling_adressen <- function(
    relnr = NULL,
    pcnum = NULL,
    pclet = NULL,
    huisnr = NULL,
    hnrtoev = NULL,
    skalnr_lbt = NULL,
    jaar) {
  df <- data.frame(
    relnr = ifelse(is.null(relnr), character(0), relnr),
    pcnum = ifelse(is.null(pcnum), character(0), pcnum),
    pclet = ifelse(is.null(pclet), character(0), pclet),
    huisnr = ifelse(is.null(huisnr), character(0), huisnr),
    hnrtoev = ifelse(is.null(hnrtoev), character(0), hnrtoev),
    skalnr_lbt = ifelse(is.null(skalnr_lbt), character(0), skalnr_lbt)
  )

  kolomnamen <- colnames(df)
  kolomnamen_jaar <- c("relnr", paste0("J", jaar, kolomnamen[c(-1,-6)]), paste0("W", jaar[1], "ZZ.1000"))

  colnames(df) <- kolomnamen_jaar
  df
}

maak_synthetische_skal <- function(
    skalnummer = NULL,
    artikel = NULL,
    postcode = NULL,
    adres = NULL,
    restultaat_certificatie = NULL,
    geldig_tot = NULL,
    geldig_vanaf = NULL) {
  data.frame(
    Skalnummer = ifelse(is.null(skalnummer), character(0), skalnummer),
    Adres = ifelse(is.null(adres), character(0), adres),
    Postcode = ifelse(is.null(postcode), character(0), postcode),
    Artikel = ifelse(is.null(artikel), character(0), artikel),
    `Resultaat certificatie` = ifelse(is.null(restultaat_certificatie),
      character(0),
      restultaat_certificatie
    ),
    `Certificaat Geldig tot` = ifelse(is.null(geldig_tot),
      character(0),
      geldig_tot
    ),
    `Certificaat Geldig vanaf` = ifelse(is.null(geldig_vanaf),
      character(0),
      geldig_vanaf),
    check.names = FALSE
  )
}


#' Maak synthetische data voor alle bestanden die gerelateerd zijn aan locaties
#' voor alleen een locatie. Dit zijn RVO slachthuizen, landbouwtelling,
#' verblijfplaatsen en skal
#'
#' @inheritParams maak_locaties
#'
#' @param ubn character, het UBN nummer
#' @param relnr character, het relatie nummer
#' @param pcnum character, het postcode nummer
#' @param pclet character, het postcode letters
#' @param huisnr character, het huisnummer
#' @param hnrtoev character, het huisnummertoevoeging
#' @param soort character, het locatietype, SP (voor slachtplaats) of VH (voor
#'   veehouderij)
#' @param biologisch logical, TRUE indien biologisch, FALSE indien niet
#' @param skal_geldig_tot date, de datum tot wanneer het certificaat geldig is
#'   in de skal dataset
#' @param jaar_lbt het jaar (twee cijfers) dat wordt gevonden in de kolomnamen
#'   van het landbouwtelling dataset
#'
#' @return list met vier elementen. Elk element is een data.frame met een rij,
#'   dat overeenkomt met RVO slachthuizen, landbouwtelling, verblijfplaatsen en
#'   skal
#' @export
#'
#' @examples
maak_locatie <- function(ubn, 
                         relnr,
                         pcnum,
                         pclet,
                         huisnr,
                         hnrtoev,
                         soort,
                         biologisch = FALSE,
                         skal_geldig_tot = NULL,
                         jaar_lbt = 21,
                         straatnamen) {
  
  df_locatie <- data.frame(
    ubn = ubn,
    relnr = relnr,
    pcnum = pcnum,
    pclet = pclet,
    huisnr = huisnr,
    hnrtoev = hnrtoev
  )

  if (soort == "VH") {
    df_verblijfsplaatsen <- df_locatie |>
      dplyr::select(ubn, relnr) |>
      purrr::pmap(maak_synthetische_verblijfsplaatsen) |>
      purrr::list_rbind()
    
    df_landbouwtelling <- df_locatie |>
      dplyr::select(
        relnr, pcnum, pclet, huisnr, hnrtoev
      ) |>
      dplyr::mutate(
        jaar = jaar_lbt,
        skalnr_lbt = ifelse(biologisch, maak_id(1, 6), "")
      ) |>
      purrr::pmap(maak_synthetische_landbouwtelling_adressen) |>
      purrr::list_rbind()
    
  } else {
    df_verblijfsplaatsen <- data.frame()
    df_landbouwtelling <- data.frame()
  }

  if (biologisch) {
    
    df_skal <- df_locatie |>
      dplyr::mutate(
        skalnummer = ifelse(is.null(df_landbouwtelling$skalnr_lbt), maak_id(1, 6), df_landbouwtelling$skalnr_lbt),
        adres = glue::glue("{sample(straatnamen, 1)} {huisnr}{hnrtoev}", .na = ""),
        postcode = glue::glue("{pcnum} {pclet}"),
        restultaat_certificatie = "Biologisch",
        artikel = sample(c("01.42.1 runderen, vleesvee", "10.1 slachterijen en verwerking"), 1),
        geldig_tot = dplyr::if_else(is.null(skal_geldig_tot),
                            # laatste dag van het huidge jaar
                            lubridate::ymd_hms(
                              lubridate::floor_date(Sys.Date(), "year") + 
                                lubridate::dyears(1)),
                            NA
                            ),
        geldig_tot = format(geldig_tot, "%d-%m-%Y"),
        geldig_vanaf = lubridate::ymd_hms(
          lubridate::floor_date(Sys.Date(), "year") - 
            lubridate::dyears(5)),
        geldig_vanaf = format(geldig_vanaf, "%d-%m-%Y")
      ) |>
      dplyr::select(
        skalnummer, adres, postcode, restultaat_certificatie, artikel, geldig_tot, geldig_vanaf
      ) |> 
      purrr::pmap(maak_synthetische_skal) |> 
      purrr::list_rbind()
  } else {
    df_skal <- data.frame()
  }

  if(soort == "SP") {
    df_slachthuizen <- df_locatie |>
      dplyr::select(
        ubn,
        bvg_postcode_plaatscode = pcnum,
        bvg_postcode_lettercode = pclet,
        bvg_huisnummer = huisnr,
        bvg_huisnummer_toevoeging = hnrtoev
      ) |>
      dplyr::mutate(
        bvg_type_bedrijfsvestiging = soort
      ) |> 
      purrr::pmap(maak_synthetische_slachthuizen) |>
      purrr::list_rbind()
  } else {
    df_slachthuizen <- data.frame()
    
  }

  list(
    slachthuizen = df_slachthuizen,
    landbouwtelling = df_landbouwtelling,
    verblijfplaatsen = df_verblijfsplaatsen,
    skal = df_skal
  )
}

#' Maak synthetische data voor alle bestanden die gerelateerd zijn aan locaties.
#' Dit zijn RVO slachthuizen, landbouwtelling, verblijfplaatsen en skal
#'
#' @param n_locaties numeric, aantal locaties.
#' @param biologische_fractie numeric, moet tussen 0 en 1 liggen. Bepaalt het
#'   deel van de locaties dat wordt beschouwd als biologisch.
#' @param straatnamen character, namen van straten die in de adressen moeten
#'   worden gebruikt
#'
#' @return list met vier elementen. Elk element is een data.frame, dat
#'   overeenkomt met RVO slachthuizen, landbouwtelling, verblijfplaatsen en skal
#' @export
#'
#' @examples 
#' 
#' maak_locaties(100, 0.5, straatnamen = maak_straatnamen())

maak_locaties <- function(n_locaties, biologische_fractie, straatnamen) {
  
  
  locaties <- data.frame(
    ubn = maak_id(n_locaties, 7),
    soort = sample(c("SP", "VH"), n_locaties, replace = TRUE, prob = c(0.4, 0.6)),
    relnr = maak_id(n_locaties, 6),
    pcnum = maak_id(n_locaties, 4),
    pclet = paste0(sample(LETTERS, size = n_locaties / 2, replace = TRUE), 
                   sample(LETTERS, size = n_locaties / 2, replace = TRUE), 
                   sep = ""),
    huisnr = sample(1:1000, n_locaties, replace = TRUE),
    hnrtoev = sample(c(NA, LETTERS[1:5]), 
                     size = n_locaties, 
                     replace = TRUE, 
                     prob = c(0.8, rep(0.2 / 5, 5))),
    biologisch = sample(c(T, F), 
                        size = n_locaties,
                        replace = TRUE, 
                        prob = c(biologische_fractie,
                                 1 - biologische_fractie))
  ) |> 
      purrr::pmap(\(...)  maak_locatie(..., straatnamen = straatnamen))
    
    list(
      slachthuizen = purrr::map(locaties, \(l) purrr::pluck(l, "slachthuizen")) |>
        purrr::list_rbind(),
      landbouwtelling = purrr::map(locaties, \(l) purrr::pluck(l, "landbouwtelling")) |>
        purrr::list_rbind(),
      verblijfplaatsen = purrr::map(locaties, \(l) purrr::pluck(l, "verblijfplaatsen")) |>
        purrr::list_rbind(),
      skal = purrr::map(locaties, \(l) purrr::pluck(l, "skal")) |>
        purrr::list_rbind()
    )
}

#' Maak locaties en slachtingen dataset
#'
#' @inheritParams maak_locaties
#'
#' @param n_locaties numeric, aantal locaties
#' @param locaties list, optioneel, output van `maak_locaties()`. Indien niet
#'   opgegeven (of NULL) worden alle locaties vanaf nul aangemaakt, anders
#'   worden de locaties in de lijst gebruikt.
#' @param ... andere waarden voor de `maak_synthetische_slachtingen_levering`
#'   functie
#'
#' @return list met vijf elementen. Elk element is een data.frame, dat
#'   overeenkomt met RVO slachtingen, RVO slachthuizen, landbouwtelling,
#'   verblijfplaatsen en skal
#' @export
#'
#' @examples 
#' 
# synthetische_dataset <- maak_synthetische_dataset(
#     n_locaties = 100,
#     biologische_fractie = 0.5,
#     straatnamen = maak_straatnamen(),
#     n_regels = 1000,
#     eerste_datum_slacht = "2024-01-01",
#     laatste_datum_slacht = "2024-05-01"
#     )
maak_synthetische_dataset <- function(n_locaties, 
                                      biologische_fractie, 
                                      straatnamen,
                                      diersoort_rvo,
                                      locaties = NULL,
                                      ...) {
  slachtingen <- list()
  for (diersoort in diersoort_rvo) {  
    if(is.null(locaties)) {
    
      locaties <- maak_locaties(n_locaties = n_locaties, 
                                biologische_fractie = biologische_fractie,
                                straatnamen = maak_straatnamen())
        
    }
    
    ubn_sp <- locaties$slachthuizen$UBN
    ubn_vh = locaties$verblijfplaatsen$UBN_NUMMER
  

    if (diersoort == "rund") {
      df_slachtingen_rund <- maak_synthetische_slachtingen_levering(
        diersoort = "rund",
        verblijfplaatsen_ubn = ubn_vh, 
      slachthuizen_ubn = ubn_sp,
      ...)
      df_slachtingen_rund <- df_slachtingen_rund |>
      purrr::pmap(maak_synthetische_slachting) |>
      purrr::list_rbind()
      slachtingen <- append(slachtingen, 
                            list(df_slachtingen_rund = df_slachtingen_rund))
      if (!"gaap" %in% diersoort_rvo) {
        df_slachtingen_gaap <- NULL
        slachtingen <- append(slachtingen, 
                              list(df_slachtingen_gaap = df_slachtingen_gaap))
      }
    } else if (diersoort == "gaap") {
      df_slachtingen_gaap <- maak_synthetische_slachtingen_levering(
        diersoort = "gaap",
        verblijfplaatsen_ubn = ubn_vh, 
        slachthuizen_ubn = ubn_sp,
        ...)
      df_slachtingen_gaap <- df_slachtingen_gaap |> 
        purrr::pmap(maak_synthetische_slachting) |>
        purrr::list_rbind()
      slachtingen <- append(slachtingen, 
                            list(df_slachtingen_gaap = df_slachtingen_gaap))
      if (!"rund" %in% diersoort_rvo) {
        df_slachtingen_rund <- NULL
        slachtingen <- append(slachtingen,
                              list(df_slachtingen_rund = df_slachtingen_rund))
      }
    } else {
      slachtigen <- append(slachtigen,
                           list(df_slachtingen_rund = NULL,
                           df_slachtingen_gaap = NULL))
      
    }
  }
  
  list(
    slachthuizen = locaties$slachthuizen,
    landbouwtelling = locaties$landbouwtelling,
    verblijfplaatsen = locaties$verblijfplaatsen,
    skal = locaties$skal,
    slachtingen_rund = df_slachtingen_rund, 
    slachtingen_gaap = df_slachtingen_gaap
  )
}


#' Zet de synthetische input voor NVWA witvlees data in het juiste format qua
#' kolomnamen, datayype en weergave van de maanden
#'
#' @param jaar numeric. het jaar waarvoor een slachthuis cijfers aanlevert.
#' @param maand numeric. de maand waarvoor een slachthuis cijfers aanlevert. 
#' @param groep character. De diergroep; altijd "Pluimvee"
#' @param subgroep character. De dier subgroep; altijd "Pluimvee"
#' @param diersoort character. De diersoort die is gerapporteerd
#' @param werkpleknummer character. Het ID nummer dat NVWA dit slachthuis heeft 
#'   gegeven
#' @param naam character. De naam van het slachthuis
#' @param plaats character. De standplaats van het slachthuis
#' @param levend_aangevoerd numeric. Het aantal dieren dat is geslacht.
#' @param dood_aangevoerd numeric. 
#'
#' @return
#' @export
#'
#' @examples
synthetiseer_nvwa_witvlees <- function(
    jaar = NULL,
    maand = NULL, # januari - december
    groep = "Pluimvee",
    subgroep = "Pluimvee",
    diersoort = NULL, # Kippen, Vleeskuikens, Eenden, Duiven, Kalkoenen
    werkpleknummer = NULL,
    naam = NULL,
    plaats = NULL,
    levend_aangevoerd = NULL,
    dood_aangevoerd = NULL) {
  data.frame(
    JAAR = ifelse(is.null(jaar), character(0), jaar),
    MAAND = ifelse(is.null(maand), character(0), maand_naar_nvwa_witvlees_formaat(maand)),
    GROEP = ifelse(is.null(groep), character(0), groep),
    SUBGROEP = ifelse(is.null(subgroep), character(0), subgroep),
    DIERSOORT = ifelse(is.null(diersoort), character(0), diersoort),
    WERKPLEKNUMMER = ifelse(is.null(werkpleknummer), character(0), werkpleknummer),
    NAAM = ifelse(is.null(naam), character(0), naam),
    PLAATS = ifelse(is.null(plaats), character(0), plaats),
    "LEVEND AANGEVOERD" = ifelse(is.null(levend_aangevoerd), numeric(0), levend_aangevoerd),
    "DOOD AANGEVOERD" = ifelse(is.null(dood_aangevoerd), numeric(0), dood_aangevoerd),
    check.names = FALSE # om de spatie in de kolomnamen toe te laten
  )
}



#' Zet de synthetische input voor NVWA witvlees data in het juiste format qua
#' kolomnamen en datatype
#' 
#' @param jaar numeric. het jaar waarvoor een slachthuis cijfers aanlevert.
#' @param maand numeric. de maand waarvoor een slachthuis cijfers aanlevert. 
#' @param diersoort_code character. De naam van het diersoort in afkorting
#' @param diersoort_omschr character. De naam van het diersoort voluit.
#' @param werkpleknummer character. Het ID nummer dat NVWA dit slachthuis heeft 
#'   gegeven
#' @param werkpleknaam character. De naam van het slachthuis
#' @param werkplekplaats character. De standplaats van het slachthuis
#' @param aantal_aangeboden numeric. Het aantal dieren dat is geslacht.
#'
#' @return df data.frame met synthetisch aantal geslachte dieren per slachthuis
#'
#' @examples
synthetiseer_nvwa_roodvlees <- function(
    jaar = NULL,
    maand = NULL,
    diersoort_code = NULL,
    diersoort_omschr = NULL, 
    werkpleknummer = NULL, 
    werkpleknaam = NULL,
    werkplekplaats = NULL,
    aantal_aangeboden = NULL) {
  data.frame(
    Jaar = ifelse(is.null(jaar), character(0), jaar),
    Maand = ifelse(is.null(maand), character(0), maand),
    DIER_SOORT_CODE = ifelse(is.null(diersoort_code), character(0), diersoort_code),
    DIER_SOORT_OMSCHR	= ifelse(is.null(diersoort_omschr), character(0), diersoort_omschr),
    WERKPLEK_NR	= ifelse(is.null(werkpleknummer), character(0), werkpleknummer),
    WERKPLEKNAAM = ifelse(is.null(werkpleknaam), character(0), werkpleknaam),
    WERKPLEKPLAATS = ifelse(is.null(werkplekplaats), character(0), werkplekplaats),
    AANTAL_AANGEBODEN  = ifelse(is.null(aantal_aangeboden), numeric(0), aantal_aangeboden)
  )
}


#' Converteert een maand van getal naar woord
#'
#' @param maand_getal numeric. Het getal van een maand
#'
#' @return character. De label van een maand
maand_naar_nvwa_witvlees_formaat <- function(maand_getal){
    as.character(
      lubridate::month(maand_getal, label = TRUE, abbr = FALSE)
    )
}


#' Maak een synthetische levering van NWVA data; de inhoud voor roodvlees
#' op slachthuisniveau.
#'
#' @param n_regels numeric hoeveel regels/slachthuizen de datasets moeten 
#'   bevatten.
#' @param eerste_datum_verwerking character. De eerste maand van het 
#' tijdsinterval waar de data over moet rapporteren. in format yyyy-mm-01.
#' @param laatste_datum_verwerking character. De laatste maand waar van het 
#'   tijdsinterval waar de data over moet rapporteren. in format yyyy-mm-28.  
#' @param rdvl_code list. De roodvlees diersoort afkortingen. 
#' @param rdvl_omschrijving list. De roodvlees diersoorten voluit.
#' @param alleen_runderen boolean. TRUE om een bestand met alleen runderen en
#'   kalveren te genereren.
#'
#' @return data.frame met willekeurig gegenereerde inhoud voor de roodvlees 
#'   dataset
#'
#' @examples
synthetiseer_nvwa_roodvlees_levering <- function(
    n_regels,
    jaren = c("2022", "2023"),
    werkpleknummers = NULL,
    diersoort_nvwa = NULL,
    rdvl_code = c("KA", "RU", "ED", "GE", "SJ", "SO", "VA"),
    rdvl_omschrijving = c("KALF", "RUND", "EENHOEVIG DIER", "GEIT", 
                          "SCHAAP JONGER DAN 1 JAAR", "SCHAAP OUDER DAN 1 JAAR", 
                          "VARKEN")
    ) {
  set.seed(1)
  
  seq_maanden <- seq(lubridate::month(1) : lubridate::month(12))
  
  if (is.null(werkpleknummers)) {
    # generate de naam en nummer van het slachthuis
    werkpleknummer <- maak_id(n = n_regels, n_cijfers = 6)
  }
  werkpleknaam <- paste("Slachterij", werkpleknummer)
  
  if (is.null(diersoort_nvwa)) {
    diersoort_idx <-  sample(1:length(rdvl_code), n_regels, replace = TRUE)
  } else {
    diersoort_idx <- match(diersoort_nvwa, 
                           c("Kalveren", "Volwassen runderen", 
                             "eenhoevinge dieren", "Geiten",
                             "Lammeren",  "Schapen", "Varkens")
                          )
  }
  
  # voor elke werkpleknaam, genereer een diersoort 
  df <- data.frame(
    werkpleknummer = werkpleknummer,
    werkpleknaam = werkpleknaam,
    diersoort_idx = diersoort_idx
  ) |>
    dplyr::group_by(
      werkpleknummer,
      werkpleknaam, 
      diersoort_idx
  ) |> # in die combinatie, genereer aantallen per maand
    tidyr::expand(
      seq_maanden = seq_maanden,
      jaar = jaren
  ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      maand = lubridate::month(seq_maanden),
      diersoort_code = rdvl_code[diersoort_idx],
      diersoort_omschr = rdvl_omschrijving[diersoort_idx],
      aantal_aangeboden = sample(1:10000, length(seq_maanden), replace = TRUE)
  ) |>
    dplyr::select(-diersoort_idx, - seq_maanden)
}


#' Maak een synthetische levering van NWVA data; de inhoud voor witvlees
#' op slachthuisniveau.
#'
#' @param n_regels numeric hoeveel regels/slachthuizen de datasets moeten 
#'   bevatten.
#' @param eerste_datum_verwerking character. De eerste maand van het 
#' tijdsinterval waar de data over moet rapporteren. in format yyyy-mm-01.
#' @param laatste_datum_verwerking character. De laatste maand waar van het 
#'   tijdsinterval waar de data over moet rapporteren. in format yyyy-mm-28.  
#' @param wtvl_diersoort list. Bevat de witvlees diersoorten
#' @param ... zodat parameters voor roodvlees kunnen passeren zonder problemen
#'
#' @return data.frame met willekeurig gegenereerde inhoud voor de witvlees 
#'   dataset
#'
#' @examples
synthetiseer_nvwa_witvlees_levering <- function(
    n_regels,
    jaren = c("2022", "2023"),
    werkpleknummers = NULL,
    diersoort_nvwa = NULL,
    wtvl_diersoort = c("Vleeskuikens","Kippen","Eenden","Duiven","Kalkoenen"),
    ...
    ) {
  
  seq_maanden <- seq(lubridate::month(1) : lubridate::month(12))
  
  if (is.null(werkpleknummers)) {
    # generate de naam en nummer van het slachthuis
    werkpleknummer <- maak_id(n = n_regels, n_cijfers = 6)
  }
  naam <- paste("Slachterij", werkpleknummer)
  
  if (is.null(diersoort_nvwa)) {
    diersoort_nvwa <-  sample(wtvl_diersoort, n_regels, replace = TRUE)
  }
  
  # groepeer de werkpleknummers en -namen samen
  df <- data.frame(
    werkpleknummer = werkpleknummer,
    naam = naam,
    diersoort = diersoort_nvwa
  ) |>
    dplyr::group_by(
      werkpleknummer,
      naam,
      diersoort
    ) |> # in die combinatie, genereer aantallen per maand
    tidyr::expand(
      seq_maanden = seq_maanden,
      jaar = jaren
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      maand = lubridate::month(seq_maanden),
      groep = "Pluimvee",
      subgroep = "Pluimvee",
      levend_aangevoerd = sample(3000:400000, length(seq_maanden), replace = TRUE),
      dood_aangevoerd = sample(1:5000, length(seq_maanden), replace = TRUE)
    ) |>
    dplyr::select(-seq_maanden)

}


#' Maakt datasets van NVWA roodvlees en witvlees aantallen. Combineert de
#' eerder geschreven functies.
#'
#' @param ... om parameters binnen synthetiseer_nvwa_witvlees_levering en 
#'   synthetiseer_nvwa_roodvlees_levering te specificeren.
#'
#' @return list. Twee datasets; rood- en witvlees met aantallen per slachthuis
#'   per maand.
#'
#' @examples
#'   synthetiseer_nvwa_dataset(
    # n_regels = 1000,
    # jaren = c("2022", "2023")
#'   )
synthetiseer_nvwa_dataset <- function(...) {
    
    df_roodvlees <- synthetiseer_nvwa_roodvlees_levering(...)
    df_witvlees <- synthetiseer_nvwa_witvlees_levering(...) 
    
    nvwa_roodvlees <- df_roodvlees |> 
      purrr::pmap(synthetiseer_nvwa_roodvlees) |>
      purrr::list_rbind()
    nvwa_witvlees <- df_witvlees |> 
      purrr::pmap(synthetiseer_nvwa_witvlees) |>
      purrr::list_rbind()
    
    list(
      nvwa_roodvlees = nvwa_roodvlees,
      nvwa_witvlees = nvwa_witvlees
    )
}


#' Genereer synthetische gegevens voor de verdeling en gewichten van roodvlees.
#'
#' @param jaar character, het jaar waarvoor de data gegenereerd moet worden.
#' @param n_maanden numeric, de laatste maand dat data moet bevatten.
#' @param verd_vaarzen numeric, constante. Het percentage van de runderen dat 
#'   is opgemaakt uit vaarzen
#'
#' @return data.frame
synthetiseer_verd_gemg_inhoud <- function(
    jaar = "2021",
    n_maanden = 7,
    verd_vaarzen = 2.33,
    verd_kalf_0_8 = 90.25,
    verd_kalf_8_12 = 9.75
    ) 
  {
  set.seed(1)
  
  maand <- paste0(
    jaar, 
    stringr::str_pad(string = lubridate::month(seq(1:12)), width = 2, side = "left", pad = "0")
  )

  # sample runderen percentage verdeling
  verd_koeien <- sample(70:90, n_maanden, replace = TRUE)
  verd_stieren <- (100 - verd_koeien - verd_vaarzen)
  
  # TODO df met de sampled waarden, evenueel aanvullen met verwachte lege kolommen
  df <- as.data.frame(maand) |> dplyr::left_join(
      data.frame(
      maand = maand[1:n_maanden],
      verd_stieren = verd_stieren,
      verd_koeien = verd_koeien,
      verd_vaarzen = rep.int(verd_vaarzen, n_maanden),
      verd_kalf_0_8 = rep.int(verd_kalf_0_8, n_maanden),
      verd_kalf_8_12 = rep.int(verd_kalf_8_12, n_maanden),
      gemg_stieren = sample(420:490, n_maanden, replace=TRUE),
      gemg_koeien = sample(300:340, n_maanden, replace=TRUE),
      gemg_vaarzen = sample(220:260, n_maanden, replace=TRUE),
      gemg_kalf_0_8 = sample(140:160, n_maanden, replace=TRUE),
      gemg_kalf_8_12 = sample(180:220, n_maanden, replace=TRUE),
      gemg_varkens = sample(95:105, n_maanden, replace=TRUE),
      gemg_lammeren = sample(18:24, n_maanden, replace=TRUE),
      gemg_schapen = rep.int(30, n_maanden)
    ),
    by = "maand"
  )
  df
}


#' Maakt een excel worksheet met roodvlees gewichten. Wanneer meerdere jaren 
#' worden meegegeven, krijgt elk jaar een ander tabblad.
#'
#' @param jaren vector met characters. De jaren waar data voor gegenereerd wordt
#' @param max_maand numeric. de laatste maand waar data voor gemaakt moet worden
#' @param zwarte_balken boolean. TRUE als een excel gegenereerd moet worden met
#'   de twee kalveren kolommen bij 'verdeling' 
#'
#' @return workbook
opmaken_verd_gemg_excel <- function(
    jaren = c("2021", "2022"),
    max_maand = 12,
    zwarte_balken = FALSE
) {

  # create workbook
  wb <- openxlsx::createWorkbook()
  
  for (jaar in jaren) {
    # max maand geldt alleen voor het laatste jaar
    is_laatste_jaar <- match(jaar, jaren) == length(jaren)
    
    # make data
    df <- synthetiseer_verd_gemg_inhoud(
      jaar = jaar,
      n_maanden = ifelse(is_laatste_jaar, yes = max_maand, no = 12)
    )
    
    # add a worksheet
    openxlsx::addWorksheet(wb, sheetName = jaar)
    
    # moet de file geschreven worden met of zonder lege kolommen
    if (zwarte_balken == FALSE) {
    openxlsx::writeData(wb, 
                        sheet = match(jaar, jaren), 
                        x = df[, c("maand", 
                                   "verd_stieren", 
                                   "verd_koeien", 
                                   "verd_vaarzen",
                                   "gemg_stieren",
                                   "gemg_koeien",
                                   "gemg_vaarzen",
                                   "gemg_kalf_8_12",
                                   "gemg_lammeren",
                                   "gemg_varkens",
                                   "gemg_schapen")], 
                        startRow = 6, 
                        startCol = 1)
    } else {
      # schrijf de maand kolom en de verdeling 
      openxlsx::writeData(wb, 
                          sheet = match(jaar, jaren), 
                          x = df[, c("maand", 
                                     "verd_stieren", 
                                     "verd_koeien", 
                                     "verd_vaarzen")], 
                          startRow = 6, 
                          startCol = 1)
      # skip twee kolommen, schrijf daarna de gemg van stieren, koeien, vaarzen 
      openxlsx::writeData(wb, 
                          sheet = match(jaar, jaren), 
                          x = df[, c("gemg_stieren",
                                     "gemg_koeien",
                                     "gemg_vaarzen")], 
                          startRow = 6, 
                          startCol = 7)
      # skip een rij (kalveren < 8 mnd)
      openxlsx::writeData(wb, 
                          sheet = match(jaar, jaren), 
                          x = df[, c("gemg_kalf_8_12",
                                     "gemg_lammeren",
                                     "gemg_varkens",
                                     "gemg_schapen")], 
                          startRow = 6, 
                          startCol = 11)
    }
  }
  wb
}


#' Maakt een excel worksheet met kalveren gewichten.
#'
#' @param jaren vector met characters. De jaren waar data voor gegenereerd wordt
#' @param max_maand numeric. de laatste maand waar data voor gemaakt moet worden
#'
#' @return workbook 
opmaken_gemg_kalveren_excel <- function(
    jaar = "2021",
    max_maand = 12
) {

  # make data
  df <- synthetiseer_verd_gemg_inhoud(
    jaar = jaar,
    n_maanden = max_maand
  )

  # krijg de labels van maanden
  df$maanden <- lubridate::month(seq(1:12), label = TRUE)

  # schrijf de gewichten
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = glue::glue("Slachtgewichten Kalveren {jaar}"))
  openxlsx::writeData(wb,
                      sheet = glue::glue("Slachtgewichten Kalveren {jaar}"),
                      x = df[, c("maanden", 
                                 "gemg_kalf_0_8")],
                      startRow = 9, 
                      startCol = 1)
  wb
}


synthetiseer_rvo_runderen <- function(df_nvwa_roodvlees, start_rvo) {
  set.seed(1)
  
  # splits volwassen runderen in (vaarzen, stieren, koeien, onbekend rund)
  df_rvo <- df_nvwa_roodvlees |> 
    dplyr::group_by( # deze group_by werkt niet als meerdere diersoorten per bedrijf
      WERKPLEK_NR 
    ) |> # verwijder rijen die geen runderen bevatten
    dplyr::filter(
      DIER_SOORT_OMSCHR %in% c("KALF", "RUND")
    ) |> # switch naar een lagere granulariteit
    dplyr::mutate(
      DIER_SOORT_OMSCHR = dplyr::case_when(
        DIER_SOORT_OMSCHR == "KALF" ~ sample(c("Kalveren 0-8 mnd", "Kalveren 8-12 mnd"),
                                             size = 1,
                                             replace = TRUE,
                                             prob = c(.9, .1)),
        DIER_SOORT_OMSCHR == "RUND" ~ sample(c("Stieren", "Koeien", "Vaarzen", "Onbekend"), 
                                             size = 1,
                                             prob = c(.20, .60, .10, .10),
                                             replace = TRUE),
        .default = DIER_SOORT_OMSCHR),
    ) |> # bepaal of individuele slachthuizen biologisch zijn (kans van 50%)
    purrr::pmap(\(Jaar, Maand, DIER_SOORT_CODE,DIER_SOORT_OMSCHR, WERKPLEK_NR, WERKPLEKNAAM, WERKPLEKPLAATS, AANTAL_AANGEBODEN){
      if(sample(c(0, 1), size = 1) == 0) {
        
        # Als biologisch, dan verdelen we de hoeveelheid in bio en niet-bio binnen dat slachthuis
        nieuwe_bio_aantal <- round(AANTAL_AANGEBODEN * runif(n = 1, min = 0, max = 1))
        nieuwe_niet_bio_aantal <- AANTAL_AANGEBODEN - nieuwe_bio_aantal

        df_out <- data.frame(
          Jaar = Jaar,
          Maand = Maand,
          DIER_SOORT_CODE = DIER_SOORT_CODE,
          DIER_SOORT_OMSCHR = DIER_SOORT_OMSCHR,
          WERKPLEK_NR = WERKPLEK_NR,
          WERKPLEKNAAM = WERKPLEKNAAM,
          WERKPLEKPLAATS = WERKPLEKPLAATS,
          AANTAL_AANGEBODEN = c(nieuwe_bio_aantal, nieuwe_niet_bio_aantal),
          BIOLOGISCH = c(TRUE, FALSE)
        )

      } else {
        df_out <- data.frame(
          Jaar = Jaar,
          Maand = Maand,
          DIER_SOORT_CODE = DIER_SOORT_CODE,
          DIER_SOORT_OMSCHR = DIER_SOORT_OMSCHR,
          WERKPLEK_NR = WERKPLEK_NR,
          WERKPLEKNAAM = WERKPLEKNAAM,
          WERKPLEKPLAATS = WERKPLEKPLAATS,
          AANTAL_AANGEBODEN = AANTAL_AANGEBODEN,
          BIOLOGISCH = c(FALSE)
        )
      }
    }) |>
    purrr::list_rbind() |>
    dplyr::ungroup() |> 
    dplyr::mutate(
      AANTAL_AANGEBODEN = ifelse( # genereer een fout tov de NVWA waarde met een kans van 20%
        test = runif(min = 0, max = 1, n = length(AANTAL_AANGEBODEN)) < .20,
        yes = sample(1:1000, length(AANTAL_AANGEBODEN), replace = TRUE), # genereer fout
        no = AANTAL_AANGEBODEN # behoud waarde
      ),
      WERKPLEK_NR = paste0("UBN:", WERKPLEK_NR)
    ) 
  
  if (is.null(start_rvo)){
    return(df_rvo)
  } 
  else {
    # behoud alleen rvo na startpunt
    start_datum <- lubridate::ym(start_rvo)
    df_rvo$date <- lubridate::myd(paste(df_rvo$Maand, df_rvo$Jaar, "1"))
  
    df_rvo_gestart <- df_rvo |>
      dplyr::filter(date >= start_datum) |>
      dplyr::select(-date)
  }
}

#### FUNCTIE AANPASSEN VOOR GEITEN EN SCHAPEN
synthetiseer_rvo_gaap <- function(df_nvwa_roodvlees, start_rvo) {
  set.seed(1)
  
  # splits geiten en schapen in (geit, schaap en lam)
  df_rvo <- df_nvwa_roodvlees |> 
    dplyr::group_by( # deze group_by werkt niet als meerdere diersoorten per bedrijf
      WERKPLEK_NR 
    ) |> # verwijder rijen die geen runderen bevatten
    dplyr::filter(
      DIER_SOORT_OMSCHR %in% c("GEIT", "SCHAAP JONGER DAN 1 JAAR", "SCHAAP OUDER DAN 1 JAAR")
    ) |> # switch naar een lagere granulariteit
    dplyr::mutate(
      DIER_SOORT_OMSCHR = dplyr::case_when(
        DIER_SOORT_OMSCHR == "SCHAAP OUDER DAN 1" ~ sample(c("SCHAAP OUDER DAN 1 JAAR", "Onbekend"), 
                                             size = 1,
                                             prob = c(.90, .10),
                                             replace = TRUE),
        .default = DIER_SOORT_OMSCHR),
    ) |> # bepaal of individuele slachthuizen biologisch zijn (kans van 50%)
    purrr::pmap(\(Jaar, Maand, DIER_SOORT_CODE,DIER_SOORT_OMSCHR, WERKPLEK_NR, WERKPLEKNAAM, WERKPLEKPLAATS, AANTAL_AANGEBODEN){
      if(sample(c(0, 1), size = 1) == 0) {
        
        # Als biologisch, dan verdelen we de hoeveelheid in bio en niet-bio binnen dat slachthuis
        nieuwe_bio_aantal <- round(AANTAL_AANGEBODEN * runif(n = 1, min = 0, max = 1))
        nieuwe_niet_bio_aantal <- AANTAL_AANGEBODEN - nieuwe_bio_aantal
        
        df_out <- data.frame(
          Jaar = Jaar,
          Maand = Maand,
          DIER_SOORT_CODE = DIER_SOORT_CODE,
          DIER_SOORT_OMSCHR = DIER_SOORT_OMSCHR,
          WERKPLEK_NR = WERKPLEK_NR,
          WERKPLEKNAAM = WERKPLEKNAAM,
          WERKPLEKPLAATS = WERKPLEKPLAATS,
          AANTAL_AANGEBODEN = c(nieuwe_bio_aantal, nieuwe_niet_bio_aantal),
          BIOLOGISCH = c(TRUE, FALSE)
        )
        
      } else {
        df_out <- data.frame(
          Jaar = Jaar,
          Maand = Maand,
          DIER_SOORT_CODE = DIER_SOORT_CODE,
          DIER_SOORT_OMSCHR = DIER_SOORT_OMSCHR,
          WERKPLEK_NR = WERKPLEK_NR,
          WERKPLEKNAAM = WERKPLEKNAAM,
          WERKPLEKPLAATS = WERKPLEKPLAATS,
          AANTAL_AANGEBODEN = AANTAL_AANGEBODEN,
          BIOLOGISCH = c(FALSE)
        )
      }
    }) |>
    purrr::list_rbind() |>
    dplyr::ungroup() |> 
    dplyr::mutate(
      AANTAL_AANGEBODEN = ifelse( # genereer een fout tov de NVWA waarde met een kans van 20%
        test = runif(min = 0, max = 1, n = length(AANTAL_AANGEBODEN)) < .20,
        yes = sample(1:1000, length(AANTAL_AANGEBODEN), replace = TRUE), # genereer fout
        no = AANTAL_AANGEBODEN # behoud waarde
      ),
      WERKPLEK_NR = paste0("UBN:", WERKPLEK_NR)
    ) 
  
  if (is.null(start_rvo)){
    return(df_rvo)
  } 
  else {
    # behoud alleen rvo na startpunt
    start_datum <- lubridate::ym(start_rvo)
    df_rvo$date <- lubridate::myd(paste(df_rvo$Maand, df_rvo$Jaar, "1"))
    
    df_rvo_gestart <- df_rvo |>
      dplyr::filter(date >= start_datum) |>
      dplyr::select(-date)
  }
}

#' Synthetiseer_spek_bestanden:, maak synthetische data en sla bestanden op
#'
#' @param n_regels numeric hoeveel regels/slachthuizen de datasets moeten
#'   bevatten.
#' @param n_locaties numeric, aantal locaties
#' @param werkpleknummers character vector, de te gebruiken werkpleknummer's,
#'   indien leeg, worden deze willekeurig gegenereerd
#' @param diersoort character vector, de te gebruiken diersoorten, indien leeg,
#'   worden deze willekeurig gegenereerd
#' @param input_map_maand character string, de naam van de maand input map, dus
#'   de verwekingsmaand
#' @param input_map character string, pad naar de map waar bestanden worden opgeslagd
#' @param seed numeric, de seed voor set.seed functie
#' 
#' @example 
#' 
#' synthetiseer_spek_bestanden(
#'  n_regels = 100,
#'  n_locaties = 100,
#'  input_map_maand = "202404",
#'  input_map = "pad/naar/DIER/2024/202404")
synthetiseer_spek_bestanden <- function(
    n_regels = 100,
    n_locaties = 100,
    diersoort_rvo = c("rund", "gaap"),
    werkpleknummers = NULL,
    diersoort_nvwa = NULL, 
    input_map_maand,
    input_map,
    start_rvo = NULL,
    rvo_slachtingen_rund_kolommen_config = App$kolomnamen$i_en_r_runderen_slachtingen,
    rvo_slachtingen_gaap_kolommen_config = App$kolomnamen$i_en_r_gaap_slachtingen,
    seed = 1) {
  # maanden en jaren bepalen op basis van de maand van de input map
  datum_input_map_maand <- lubridate::ym(input_map_maand)
  nvwa_bestand_eerste_maand <- datum_input_map_maand + lubridate::days(31)
  nvwa_bestand_laatste_maand <- datum_input_map_maand - 
    lubridate::years(1) + 
    lubridate::days(62)
  nvwa_bestand_jaren <- seq(
    from = lubridate::year(nvwa_bestand_eerste_maand - lubridate::years(1)),
    to   = lubridate::year(nvwa_bestand_eerste_maand),
    by  = 1
  )
  
  nvwa_in_db_eerste_maand <- datum_input_map_maand - lubridate::days(31)
  nvwa_in_db_laatste_maand <- datum_input_map_maand - lubridate::years(1)
    
  nvwa_db_jaren <- seq(
    from = lubridate::year(nvwa_in_db_laatste_maand),
    to   = lubridate::year(nvwa_bestand_eerste_maand),
    by  = 1
  )
  
  set.seed(seed)
  
  # NVWA data
  synthetische_dataset <- synthetiseer_nvwa_dataset(
    n_regels = n_regels,
    jaren = nvwa_db_jaren,
    werkpleknummer = werkpleknummers,
    diersoort_nvwa = diersoort_nvwa
  )
  
  # Voor nvwa-gegevens slaan we twee bestanden op. Het eerste bestand wordt
  # opgeslagen in de invoermap en is bedoeld voor gebruik in de SPEK app. Het
  # tweede bestand wordt opgeslagen in een tijdelijke map en wordt gebruikt om
  # de database te vullen met de gegevens die het mogelijk maken om het eerste
  # bestand te verwerken in SPEK.
  

  # nvwa roodvlees ----------------------------------------------------------
  ## levering bestand
  ch_nvwa_rood_levering_pad <- file.path(
    input_map, "CBS_ROODVLEES.xlsx"
  )
  
  wb_rdvl <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_rdvl, sheetName = "CBS_ROODVLEES")
  
  synthetische_dataset$nvwa_roodvlees |> 
    dplyr::filter(
      lubridate::ym(paste(Jaar, Maand)) >=  nvwa_bestand_laatste_maand
    ) |> 
  openxlsx::writeData(wb_rdvl, 
                      sheet = "CBS_ROODVLEES", 
                      x = _, 
                      startRow = 1, 
                      startCol = 1)
  openxlsx::saveWorkbook(wb_rdvl, ch_nvwa_rood_levering_pad, TRUE)

  ## naar db bestand
  ch_nvwa_rood_db_pad <- tempfile(
    pattern = "CBS_ROODVLEES_DB",
    fileext = ".xlsx"
  )
  
  wb_rdvl <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_rdvl, sheetName = "CBS_ROODVLEES")
  
  openxlsx::writeData(wb_rdvl, 
                        sheet = "CBS_ROODVLEES", 
                        x = synthetische_dataset$nvwa_roodvlees, 
                        startRow = 1, 
                        startCol = 1)
  openxlsx::saveWorkbook(wb_rdvl, ch_nvwa_rood_db_pad, TRUE)

  # nvwa witvlees -----------------------------------------------------------
  ## levering
  ch_nvwa_wit_levering_pad <- file.path(
    input_map, "CBS_WITVLEES.xlsx"
  )
  
  wb_wtvl <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_wtvl, sheetName = "SQL Results")
  
  synthetische_dataset$nvwa_witvlees |> 
    dplyr::filter(
      lubridate::ymd(paste(JAAR, MAAND, "01")) >=  nvwa_bestand_laatste_maand
    ) |> 
    openxlsx::writeData(wb_wtvl, 
                        sheet = "SQL Results", 
                        x = _, 
                        startRow = 1, 
                        startCol = 1)
  openxlsx::saveWorkbook(wb_wtvl, ch_nvwa_wit_levering_pad, TRUE)
  
  ## naar database
  ch_nvwa_wit_db_pad <- tempfile(
    pattern = "CBS_WITVLEES",
    fileext = ".xlsx"
  )
  
  wb_wtvl <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_wtvl, sheetName = "SQL Results")
  openxlsx::writeData(wb_wtvl, 
                      sheet = "SQL Results", 
                      x = synthetische_dataset$nvwa_witvlees, 
                      startRow = 1, 
                      startCol = 1)
  openxlsx::saveWorkbook(wb_wtvl, ch_nvwa_wit_db_pad, TRUE)
  
  # baseer synth RVO data op NVWA roodvlees
  for (diersoort in diersoort_rvo) {
    if (diersoort == "rund") {
      df_rvo_rund <- synthetiseer_rvo_runderen(synthetische_dataset$nvwa_roodvlees,
                                            start_rvo)
    ch_rvo_rund_pad <- tempfile(
      pattern = "input_rvo_rund_slachtingen_AGG",
      fileext = ".xlsx"
    )

    wb_rvo <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb_rvo, sheetName = "SQL Results")
    openxlsx::writeData(wb_rvo, 
                        sheet = 1, 
                        x = df_rvo_rund, 
                        startRow = 1, 
                        startCol = 1)
    openxlsx::saveWorkbook(wb_rvo, ch_rvo_rund_pad, TRUE)
    } else if (diersoort == "gaap") {
      df_rvo_gaap <- synthetiseer_rvo_gaap(synthetische_dataset$nvwa_roodvlees,
                                          start_rvo)
    
      ch_rvo_gaap_pad <- tempfile(
        pattern = "input_rvo_gaap_slachtingen_AGG",
        fileext = ".xlsx"
      )
    
      wb_rvo <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb_rvo, sheetName = "SQL Results")
      openxlsx::writeData(wb_rvo, 
                          sheet = 1, 
                          x = df_rvo_gaap, 
                          startRow = 1, 
                          startCol = 1)
      openxlsx::saveWorkbook(wb_rvo, ch_rvo_gaap_pad, TRUE)
    }
  }
  
  # verdeling / gewichten ---------------------------------------------------
  ## genereer de synthetische verdeling / gewichten
  wb <- opmaken_verd_gemg_excel(
    jaren = max(nvwa_bestand_jaren),
    max_maand = lubridate::month(datum_input_map_maand),
    zwarte_balken = FALSE
  )
  ch_verd_gem_pad <- file.path(
    input_map, "Melding CBS per maand gem gewicht.xlsx"
  )
  openxlsx::saveWorkbook(wb, ch_verd_gem_pad, TRUE)
  
  # genereer de synthetische verdeling / gewichten voor kalveren
  
  if(lubridate::month(datum_input_map_maand) <= 3) {
    
    ch_gemg_kalveren_pad <- purrr::map_chr(nvwa_bestand_jaren, \(jaar) {
      
      if (jaar == max(nvwa_bestand_jaren)) {
        max_maand <- lubridate::month(datum_input_map_maand)
      } else {
        max_maand <- 12
      }
      bestandsnaam <- glue::glue("Slachtgewichten SBK kalveren {jaar}.xlsx")
      pad <- file.path(input_map, bestandsnaam)
      wb_kalf <- opmaken_gemg_kalveren_excel(
        jaar = jaar,
        max_maand = max_maand
      )
      
      openxlsx::saveWorkbook(wb_kalf, pad, TRUE)
      
      pad
    })
  } else {
    ch_gemg_kalveren_pad <- file.path(
      input_map, "Slachtgewichten SBK kalveren.xlsx"
    )
    
    wb_kalf <- opmaken_gemg_kalveren_excel(
      jaar = max(nvwa_bestand_jaren),
      max_maand = lubridate::month(datum_input_map_maand)
    )
    openxlsx::saveWorkbook(wb_kalf, ch_gemg_kalveren_pad, TRUE)
  }
  
  
  synthetische_dataset <- maak_synthetische_dataset(
    n_locaties = n_locaties,
    biologische_fractie = 0.5, straatnamen = maak_straatnamen(),
    n_regels = n_regels,
    diersoort_rvo = diersoort_rvo,
    eerste_datum_slacht = as.character(datum_input_map_maand - months(3)),
    laatste_datum_slacht = as.character(datum_input_map_maand + months(1))
  )

  # rvo slachtingen
  ch_slachtingen_rund_pad <- file.path(input_map,  "rvo_slachtingen_rund.csv")
  readr::write_csv(x = synthetische_dataset$slachtingen_rund, file = ch_slachtingen_rund_pad)
  df_slachtingen_rund <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  ch_slachtingen_rund_pad,
    kolommen_config = rvo_slachtingen_rund_kolommen_config,
    inlezen_functie = readr::read_csv
  )
  
  ch_slachtingen_gaap_pad <- file.path(input_map,  "rvo_slachtingen_gaap.csv")
  readr::write_csv(x = synthetische_dataset$slachtingen_gaap, file = ch_slachtingen_gaap_pad)
  df_slachtingen_gaap <- inl_lees_csv_bestand_met_kolommen_config(
    pad =  ch_slachtingen_gaap_pad,
    kolommen_config = rvo_slachtingen_gaap_kolommen_config,
    inlezen_functie = readr::read_csv
  )
  
  # rvo slachthuizen rund
  ch_slachthuizen_pad <- file.path(input_map, "rvo_slachthuizen_rund.csv")
  readr::write_csv(x = synthetische_dataset$slachthuizen, file = ch_slachthuizen_pad)
  
  # rvo slachthuizen gaap
  ch_slachthuizen_pad <- file.path(input_map, "rvo_slachthuizen_gaap.csv")
  readr::write_csv(x = synthetische_dataset$slachthuizen, file = ch_slachthuizen_pad)
  
  # skal bestand
  ch_skal_pad <- file.path(input_map, "Skal_bestand.xlsx")
  writexl::write_xlsx(synthetische_dataset$skal, path = ch_skal_pad)
  
  # lbt
  ch_lbt_pad <- file.path(input_map, "lbt_bestand.sav")
  haven::write_sav(synthetische_dataset$landbouwtelling, path = ch_lbt_pad)
  
  # verblijfplaatsen
  ch_verblijfplaatsen_pad <- file.path(input_map, "verblijfplaatsen_bestand.sav")
  haven::write_sav(synthetische_dataset$verblijfplaatsen, path = ch_verblijfplaatsen_pad)
  
  list(
    rdvl_lev_pad = ch_nvwa_rood_levering_pad,
    rdvl_db_pad = ch_nvwa_rood_db_pad,
    rdvl_rvo_rund_lev_pad = ch_slachtingen_rund_pad,
    rdvl_rvo_gaap_lev_pad = ch_slachtingen_gaap_pad,
    wtvl_lev_pad = ch_nvwa_wit_levering_pad,
    wtvl_db_pad = ch_nvwa_wit_db_pad,
    gemg_pad = ch_gemg_kalveren_pad,
    gemg_kalv_pad = ch_gemg_kalveren_pad
  )

}

#' Maak voor een bepaalde verwerkingsmaand de vereiste invoerbestanden om SPEK
#' te laten werken en vul de database met de vereiste gegevens om de verwerking
#' van die maand mogelijk te maken.
#'
#' Vooraf moet de functie temp_synth_naar_df geroepen worden
#'
#' @param verw_maand string, de verwerkingsmaand
#' @param db_schema string, de databaseschema waar de data moet worden ingelezen
#' @param is_rvo boolean, moet de data de laagste diersoort niveau voor runderen
#'   hebben? Dus simuleert RVO data in de database.
#' @param is_bio boolean, is de is_biologisch kolom aanwezig in de database?
#' @param ... andere argumenten voor synthetiseer_spek_bestanden functie.
#' @param con connection, een database verbinding object
#'
#' @example
#'
#' con <- DBI::dbConnect(odbc::odbc(), ...)
#'
#' synthetiseer_spek_bestanden_e_ini_db(con = con, verw_maand = input_map_maand,
#' input_map = test_map, db_schema = "unittest", rvo_slachtingen_kolommen_config
#' = App$kolomnamen$i_en_r_runderen_slachtingen)

#' 
synthetiseer_spek_bestanden_e_ini_db <- function(
    verw_maand = "202311",
    db_schema = "dbo",
    start_rvo = NULL,
    is_bio = FALSE,
    ...,
    con) {
  # INITIALISEER tbl_microbase_verd -----------------------------------------
  jrmnd <- alg_vind_jaarmaanden(verw_maand, verschil = -3)
  
  datum_verw_maand <- lubridate::ym(verw_maand)
  
  if(lubridate::month(datum_verw_maand) <= 3) {
    jaren <- c(lubridate::year(datum_verw_maand),
               lubridate::year(datum_verw_maand - lubridate::years(1))
               )
    df_verd_gemg_alles <- purrr::map(jaren, \(jaar) {
      synthetiseer_verd_gemg_inhoud(
        jaar = jaar,
        n_maanden = 12,
        verd_vaarzen = 2.33)
      
    }) |> purrr::list_rbind()
      
  } else {
    df_verd_gemg_alles <- synthetiseer_verd_gemg_inhoud(
        jaar = stringr::str_sub(verw_maand, 1, 4),
        n_maanden = lubridate::month(datum_verw_maand) - 1,
        verd_vaarzen = 2.33) 
    }
  
  vsmaanden_van_vorige_vwmd <- alg_vind_jaarmaanden(jrmnd$vvwmd, verschil = -3) 

  df_verd_gemg <- df_verd_gemg_alles |>
    dplyr::filter(
      maand %in% c(vsmaanden_van_vorige_vwmd$mnd1, 
                   vsmaanden_van_vorige_vwmd$mnd2,
                   vsmaanden_van_vorige_vwmd$mnd3,
                   vsmaanden_van_vorige_vwmd$mnd4)
  
        )
  # transpose de df om in de database structuur te passen
  df_voor_db_verd <- dplyr::select(df_verd_gemg, !starts_with("gemg_")
  ) |>
    dplyr::rename(
      Stieren = verd_stieren,
      Koeien = verd_koeien,
      Vaarzen = verd_vaarzen,
      "Kalveren 0-8 mnd" = verd_kalf_0_8,
      "Kalveren 8-12 mnd" = verd_kalf_8_12,
      vsmd = maand
    ) |>
    tidyr::pivot_longer(
      cols = -vsmd,
      names_to = "dcat",
      values_to = "verd"
    ) |>
    dplyr::mutate(
      dsrt = dplyr::if_else(
        condition = dcat %in% c("Kalveren 0-8 mnd", "Kalveren 8-12 mnd"),
        true = "Kalveren",
        false = "Volwassen runderen"
      )
    ) |> # voeg de verwerkingsmaand toe
    dplyr::mutate(
      vwmd = getElement(jrmnd, "mnd3")
    ) |>
    dplyr::relocate(vwmd, vsmd, dsrt, dcat, verd)
  
  glue::glue_sql(
    "DELETE FROM {`db_schema`}.tbl_microbase_verd WHERE vwmd IN ({verw_maand*})",
    .con = con
    ) |> 
    DBI::dbExecute(conn = con, 
                 statement = _)
  
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id(schema = db_schema, table = "tbl_microbase_verd"),
    value = df_voor_db_verd,
    append = TRUE
  )
  vsmaanden <- unique(df_voor_db_verd$vsmd)

  glue::glue_sql(
    "DELETE FROM {`db_schema`}.tbl_microbase_verd_def WHERE vsmd IN ({vsmaanden*})",
    .con = con
  ) |> 
    DBI::dbExecute(conn = con, 
                   statement = _)
  
  
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id(schema = db_schema, table = "tbl_microbase_verd_def"),
    value = dplyr::select(df_voor_db_verd, c(vsmd, dsrt, dcat, verd)),
    append = TRUE
  )
  
  # INITIALISEER tbl_microbase_gemg + def -----------------------------------
  df_voor_db_gemg <- dplyr::select(df_verd_gemg, !starts_with("verd_")
  ) |>
    dplyr::rename(
      vsmd = maand,
      Stieren = gemg_stieren,
      Koeien = gemg_koeien,
      Vaarzen = gemg_vaarzen,
      "Kalveren 0-8 mnd" = gemg_kalf_0_8,
      "Kalveren 8-12 mnd" = gemg_kalf_8_12,
      Lammeren = gemg_lammeren,
      Varkens = gemg_varkens,
      "Volwassen schapen" = gemg_schapen
    ) |>
      dplyr::mutate(
        Eenhoevigen = 225,
        Geiten = 13,
        Vleeskuikens = 1.65,
        "Overige kippen" = 2.15,
        Eenden = 2.1,
        Duiven = 0.4,
        Fazanten = 0.8,
        Ganzen = 6.8,
        Kalkoenen = 9.0,
        Parelhoenders = 1.3,
        Patrijzen = 0.4,
        Struisvogels = 130
    ) |>
    tidyr::pivot_longer(
      cols = -vsmd,
      names_to = "dcat",
      values_to = "gemg"
    )|> # voeg de verwerkingsmaand toe
  dplyr::mutate(
    vwmd = getElement(jrmnd, "mnd3")
  ) |>
    dplyr::relocate(vwmd, vsmd, dcat, gemg)
  
  
  glue::glue_sql(
    "DELETE FROM {`db_schema`}.tbl_microbase_gemg WHERE vwmd IN ({verw_maand*})",
    .con = con
  ) |> 
    DBI::dbExecute(conn = con, 
                   statement = _)
  
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id(schema = db_schema, table = "tbl_microbase_gemg"),
    value = df_voor_db_gemg,
    append = TRUE
  )
  vsmaanden <- unique(df_voor_db_gemg$vsmd)
  
  glue::glue_sql(
    "DELETE FROM {`db_schema`}.tbl_microbase_gemg_def WHERE vsmd IN ({vsmaanden*})",
    .con = con
  ) |> 
    DBI::dbExecute(conn = con, 
                   statement = _)
  
  DBI::dbWriteTable(
    conn = con,
    name = DBI::Id(schema = db_schema, table = "tbl_microbase_gemg_def"),
    value = dplyr::select(df_voor_db_gemg, c(vsmd, dcat, gemg)),
    append = TRUE
  )
  
  # maak de synthetische data voor twee jaar
  paden <- synthetiseer_spek_bestanden(
    input_map_maand = verw_maand,
    start_rvo = start_rvo,
    ...
  )

  # INITIALISEER tbl_microbase_rdvl + def  en tbl_microbase_wtvl + def --------
  
  # zoek pad naar de enorm lange data verzameling van twee jaar
  rdvl_nvwa_input <- paden$rdvl_db_pad
  rdvl_rvo_rund_input <- paden$rdvl_rvo_rund_lev_pad
  rdvl_rvo_gaap_input <- paden$rdvl_rvo_gaap_lev_pad

  # zoek pad naar de enorm lange data verzameling van twee jaar
  wtvl_input <- paden$wtvl_db_pad

  # vind de maanden waarvoor RVO data heeft
  # behoud alleen rvo na startpunt
  rvo_start_maand <- lubridate::ym(start_rvo)

  maanden <- seq(
    from = datum_verw_maand - lubridate::years(1),
    to   = lubridate::floor_date(datum_verw_maand - lubridate::days(28), unit = "month"),
    by  = "months"
  )
  
  purrr::walk(maanden, \(maand) {
    # begin met de jaarmaand met verwerking = januari
    pad_maand <- stringr::str_pad(lubridate::month(maand), 
                                  width = 2, 
                                  side = "left", pad = "0")
    verw_maand <- paste0(lubridate::year(maand),
                         pad_maand)
    
    # zoek verslagmaanden op
    jrmnd <- alg_vind_jaarmaanden(verw_maand, verschil = -3)
    nieuwste_vsmd <- lubridate::ym(jrmnd$mnd4)
    
    # lees de geformatte data in op basis van maandselectie
    df_rdvl <- inl_lees_input_rdvl(rdvl_nvwa_input, jrmnd)
    
    # als de verwerkingsmaand RVO data heeft, sla alleen RVO op in DB
    if(!is.null(start_rvo) && nieuwste_vsmd >= rvo_start_maand) {
      
      df_rdvl_rvo_rund <- inl_lees_input_rdvl(rdvl_rvo_rund_input, jrmnd)
      df_rdvl_rvo_gaap <- inl_lees_input_rdvl(rdvl_rvo_gaap_input, jrmnd)

      # splits de maanden voor de NVWA naar RVO overgangsperiode 
      jrmnd_as_vec <- unlist(jrmnd, use.names=FALSE)[1:4]
      if (start_rvo %in% jrmnd_as_vec) {
        mnd_na_start <- jrmnd_as_vec[which(jrmnd_as_vec == start_rvo) : 4]
        mnd_voor_start <- jrmnd_as_vec[1 : (which(jrmnd_as_vec == start_rvo) - 1)]
      } # anders, zijn de maanden volledig RVO 
      else {
        mnd_na_start <- jrmnd_as_vec
        mnd_voor_start <- c()
      }
      
      # haal de niet-rund roodvlees altijd uit NVWA
      df_voor_db_niet_rund <- df_rdvl |>
        dplyr::mutate(
          aant_ruw = aant,
          aant_gaaf = aant
        ) |>
        dplyr::select(-aant) |> 
        dplyr::filter(!dsrt %in% c("Kalveren", "Volwassen runderen", 
                                   "Lammeren", "Volwassen schapen", "Geiten"))
      
      # NVWA waarden vóór de RVO startdatum
      df_voor_db_rund_nvwa <- df_rdvl |>
        dplyr::filter(dsrt %in% c("Kalveren", "Volwassen runderen", 
                                  "Lammeren", "Volwassen schapen", "Geiten")) |> 
        dplyr::filter(
          vsmd %in% mnd_voor_start) |>
        dplyr::mutate(
          aant_ruw = aant,
          aant_gaaf = aant
        ) |>
        dplyr::select(-aant)
      
      # RVO waarden ná RVO startdatum
      df_voor_db_rund_rvo <- df_rdvl_rvo_rund |>
        dplyr::filter(
          vsmd %in% mnd_na_start) |>
        dplyr::mutate(
          aant_ruw = aant,
          aant_gaaf = aant
        ) |>
        dplyr::select(-aant)
      
      df_voor_db_gaap_rvo <- df_rdvl_rvo_gaap |>
        dplyr::filter(
          vsmd %in% mnd_na_start) |>
        dplyr::mutate(
          aant_ruw = aant,
          aant_gaaf = aant
        ) |>
        dplyr::select(-aant)
      
      df_voor_db_rdvl <- dplyr::bind_rows(
        df_voor_db_niet_rund |> dplyr::mutate(is_biologisch = FALSE),
        df_voor_db_rund_rvo,
        df_voor_db_gaap_rvo,
        df_voor_db_rund_nvwa
      )
      
    } else {
      df_voor_db_rdvl <- df_rdvl |>
        dplyr::mutate(
          aant_ruw = aant,
          aant_gaaf = aant
        ) |>
        dplyr::select(-aant)
    }
    
    
    # schrijf het naar de database 
    ## verwijder data voor huidige vwmd
    glue::glue_sql(
      "DELETE FROM {`db_schema`}.tbl_microbase_rdvl WHERE vwmd IN ({verw_maand*})",
      .con = con
    ) |> 
      DBI::dbExecute(conn = con, 
                     statement = _)
    
    
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = db_schema, table = "tbl_microbase_rdvl"),
      value = df_voor_db_rdvl,
      append = TRUE
    )
    
    vsmaanden <- unique(df_rdvl$vsmd)
    ## verwijder data voor huidige vwmd
    glue::glue_sql(
      "DELETE FROM {`db_schema`}.tbl_microbase_rdvl_def WHERE vsmd IN ({vsmaanden*})",
      .con = con
    ) |> 
      DBI::dbExecute(conn = con, 
                     statement = _)
    # alleen de eerste maand gaat naar de def tabel, als in de opslaan functies
    df_voor_db_rdvl |> 
      dplyr::select(-c(vwmd, aant_ruw)) |> 
      dplyr::filter(vsmd == jrmnd$mnd1) |> 
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = db_schema, table = "tbl_microbase_rdvl_def"),
      value = _,
      append = TRUE
    )
    

  # witvlees ----------------------------------------------------------------
    # lees de geformatte data in op basis van maandselectie
    df_wtvl <- inl_lees_input_wtvl(wtvl_input, jrmnd)
    
    # vul de rest van de waarden aan (aant)
    df_voor_db_wtvl <- df_wtvl |>
      dplyr::mutate(
        aant_ruw = aant,
        aant_gaaf = aant
      ) |>
      dplyr::select(-aant)
    
    # schrijf het naar de database 
    
    glue::glue_sql(
      "DELETE FROM {`db_schema`}.tbl_microbase_wtvl WHERE vwmd IN ({verw_maand*})",
      .con = con
    ) |> 
      DBI::dbExecute(conn = con, 
                     statement = _)
    
    
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = db_schema, table = "tbl_microbase_wtvl"),
      value = df_voor_db_wtvl,
      append = TRUE
    )
    
    vsmaanden <- unique(df_wtvl$vsmd)
    
    glue::glue_sql(
      "DELETE FROM {`db_schema`}.tbl_microbase_wtvl_def WHERE vsmd IN ({vsmaanden*})",
      .con = con
    ) |> 
      DBI::dbExecute(conn = con, 
                     statement = _)
    
    # alleen de eerste maand gaat naar de def tabel, als in de opslaan functies
    df_voor_db_wtvl |> 
      dplyr::select(-c(vwmd, aant_ruw)) |> 
      dplyr::filter(vsmd == jrmnd$mnd1) |> 
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = db_schema, table = "tbl_microbase_wtvl_def"),
      value = _,
      append = TRUE
    )
    
  # statbase ----------------------------------------------------------------
    df_statbase_dsrt <- dplyr::bind_rows(
      df_voor_db_rdvl,
      df_voor_db_wtvl
    ) |> 
      dplyr::summarise(
        .by = c(vwmd, vsmd, dsrt),
        tota_ruw = sum(aant_ruw),
        tota_gaaf = sum(aant_gaaf)
      ) |> 
      dplyr::mutate(
        pcbs = dplyr::case_when(
          !is.na(tota_ruw) & !is.na(tota_gaaf)  ~ 100 * (tota_gaaf - tota_ruw) / tota_ruw,
          !is.na(tota_ruw) &  is.na(tota_gaaf)  ~ -100,
           is.na(tota_ruw) & !is.na(tota_gaaf)  ~ 100,
           is.na(tota_ruw) &  is.na(tota_gaaf)  ~ 0
        )
      )
      
    glue::glue_sql(
      "DELETE FROM {`db_schema`}.tbl_statbase_dsrt WHERE vwmd IN ({verw_maand*})",
      .con = con
    ) |> 
      DBI::dbExecute(conn = con, 
                     statement = _)
    
    DBI::dbWriteTable(
      conn = con,
      name = DBI::Id(schema = db_schema, table = "tbl_statbase_dsrt"),
      value = df_statbase_dsrt,
      append = TRUE
    )
  })
  paden
}

