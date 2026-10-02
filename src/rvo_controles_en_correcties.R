#' Maak html rapport met de uitslag van de koppeling proces tussen RVO en Skal
#' locaties
#'
#' @param df_locaties_kv3 data.frame, kv3 versie van de tbl_locaties_kv2_kv3
#' @param df_locaties_zonder_adres data.frame, gemaakt bij
#'   `rvo_locaties_koppeling` functie
#' @param df_locaties_zonder_relnr data.frame, gemaakt bij
#'   `rvo_locaties_koppeling` functie
#' @param df_locaties_met_relnr_zonder_adres data.frame, gemaakt bij
#'   `rvo_locaties_koppeling` functie
#' @param df_locaties_zonder_adres_voor_norm data.frame, gemaakt bij
#'   `rvo_locaties_koppeling` functie
#' @param df_locaties_problematisch_norm_adressen data.frame, gemaakt bij
#'   `rvo_locaties_koppeling` functie
#' @param df_locaties_zonder_adres_vanvege_norm data.frame, gemaakt bij
#'   `rvo_locaties_koppeling` functie
#' @param df_locaties_zonder_skal data.frame, gemaakt bij
#'   `rvo_locaties_koppeling` functie
#' @param df_skal_problematisch_norm_adressen data.frame, gemaakt bij
#'   `rvo_locaties_koppeling` functie
#' @param df_skal_zonder_adres_vanvege_norm data.frame, gemaakt bij
#'   `rvo_locaties_koppeling` functie
#' @param pad_sjablon character, pad naar de sjablon voor de correcties rapport.
#' @param pad_outputmap character, pad naar de map waar de rapport wordt
#'   opgeslagen.
#' @param pad_tempmap character, pad naar temp map waar bestanden worden
#'   opgeslagen tijdens het rapport maken proces. Als deze NULL is, wordt deze
#'   automatisch aangemaakt
#'
#' @return
#' @export
rvo_locaties_koppeling_log <- function(
    df_locaties_kv3,
    df_locaties_zonder_adres,
    df_locaties_zonder_relnr,
    df_locaties_met_relnr_zonder_adres,
    df_locaties_meerdere_relnrs,
    df_locaties_zonder_adres_voor_norm,
    df_locaties_problematisch_norm_adressen,
    df_locaties_zonder_adres_vanvege_norm,
    df_locaties_zonder_skal,
    df_skal_zonder_adres,
    pad_sjablon, 
    pad_outputmap,
    pad_tempmap = NULL
    ) {
  
  if (is.null(pad_tempmap)) {
    pad_tempmap <- tempdir()
  } 
  # We gaan elke tabel als temp RDS bestand opslaan. We doen dit om data.frames
  # door te geven aan een parameterized quarto rapport, omdat dergelijke
  # rapporten geen data.frames als parameter accepteren
  dfs_naar_report <- list(
    "pad_df_locaties_zonder_relnr" = df_locaties_zonder_relnr,
    "pad_df_locaties_met_relnr_zonder_adres" = df_locaties_met_relnr_zonder_adres,
    "pad_df_locaties_meerdere_relnrs" = df_locaties_meerdere_relnrs,
    "pad_df_locaties_zonder_adres_voor_norm" = df_locaties_zonder_adres_voor_norm,
    "pad_df_locaties_problematisch_norm_adressen" = df_locaties_problematisch_norm_adressen,
    "pad_df_locaties_zonder_skal" = df_locaties_zonder_skal,
    "pad_df_skal_zonder_adres" = df_skal_zonder_adres
    # "pad_df_skal_problematisch_norm_adressen" = df_skal_problematisch_norm_adressen
  )
  
  params_paden_df <- purrr::imap(dfs_naar_report, \(df, naam) {
    pad <- tempfile(tmpdir = pad_tempmap, fileext = ".rds")
    saveRDS(df, pad)
    pad
  })
  
  n_locaties_totaal <- nrow(df_locaties_kv3)
  n_locaties_zonder_adres_voor_mergen <- nrow(df_locaties_zonder_adres)
  n_locaties_zonder_adres_voor_norm <- nrow(df_locaties_zonder_adres_voor_norm)
  n_locaties_zonder_adres_vanvege_norm <- nrow(df_locaties_zonder_adres_vanvege_norm)
  n_skal_zonder_adres <- nrow(df_skal_zonder_adres)
  
  params_n <- list(
    n_locaties_totaal = n_locaties_totaal,
    n_locaties_zonder_adres_voor_mergen = n_locaties_zonder_adres_voor_mergen,
    n_locaties_zonder_adres_voor_norm = n_locaties_zonder_adres_voor_norm,
    n_locaties_zonder_adres_vanvege_norm = n_locaties_zonder_adres_vanvege_norm,
    n_skal_zonder_adres = n_skal_zonder_adres
  ) 
  
  # render document
  quarto::quarto_render(
    input = pad_sjablon,
    execute_params = c(params_paden_df, params_n)
  )
  
  # Schoonmaken
  # Verplaats (en hernoem) bestand naar  het outputmap want het is niet 
  # moegelijk om de outputpad van het html bestand in Quarto te definieren
  pad_outputbestand <- stringr::str_replace(pad_sjablon, ".qmd", ".html")
  basenaam <- "kv3_koppeling"
  logbestandnaam <- paste(format(Sys.time(),'%Y%m%d_%H%M%S'), 
                          basenaam,
                          "log.html",
                          sep = "_")
  pad_logbestand <- file.path(pad_outputmap, logbestandnaam)
  fs::file_move(pad_outputbestand, pad_logbestand)
  
  pad_logbestand
}

#'Functies uit het validate package toepassen om controles uit te voeren. Maakt
#'het mogelijk om slechts een subset van controles uit te voeren van alle
#'controles in een validate::validator object.
#'
#'@param regels validate::validator object.
#'@param ... andere parameters voor het `een_controle_toepassen` functie
#'
#'@return tibble met een rij per controle en drie kolommen:
#'  - description = het 'description' veld in de regel. 
#'  - foutive_rijen = een data.frame met alle rijen die niet door de controle kwamen.
#'  - controles =  de samenvatting van de output van validate::confront
#'
#'@export
#'
#' @examples
#'
#' controles <- validate::validator(
#' onderwerp_a_ctrl_1 = x <= 5,
#' onderwerp_a_ctrl_2 = x < 6 & y > 6,
#' onderwerp_b = z == 15)
#'
#'df <- data.frame(
#'  x = 1:10,
#'  y = 6:15,
#'  z = 11:20
#')
#'
#'controles_toepassen(regels = controles,
#' df = df,
#' deel = "onderwerp_a")
controles_toepassen <- function(regels, ...) {
  purrr::map(names(regels), \(deel) {
    # We selecteren een subset van regels
    regels_deel <- regels[grep(deel, names(regels))]

    een_regel_toepassen(regel = regels_deel, ...)
  }) |>
    purrr::list_rbind()
}

#' Functie uit het validate package toepassen om  EEN controle uit te voeren.
#'
#'@param regel validate::validator object met een regel.
#'@param df data.frame. De tabel waar de controles moeten worden uitgevoerd.
#'@param alleen_gebruikt_kolommen logical. Geeft alleen de kolommen in de
#'  foutieve_df output terug die worden gebruikt in de controles. Default is
#'  FALSE.
#'@param id_kolommen string, als `alleen_gebruikt_kolommen` `TRUE` is, kunnen een
#'  of meer kolommen worden opgegeven om in de output te houden, zelfs als de
#'  regel er niet op worden toegepast. Dit kan handig zijn om snel
#'  een rij te identificeren waar een fout werd gevonden.
#'
#'@return tibble met een rij en drie kolommen:
#'  - description = het 'description' veld in de regel. 
#'  - foutive_rijen = een data.frame met alle rijen die niet door de controle kwamen.
#'  - controles =  de samenvatting van de output van validate::confront
#'
#'@export
#'
#' @examples
#'
#' controles <- validate::validator(
#' onderwerp_a_ctrl_1 = x <= 5
#'
#'df <- data.frame(
#'  x = 1:10,
#'  y = 6:15,
#'  z = 11:20
#')
#'
#'een_regel_toepassen(regel = controles,
#' df = df,
#' alleen_gebruikt_kolommen = TRUE,
#' id_kolommen = "x")
een_regel_toepassen <- function(regel, df, 
                                alleen_gebruikt_kolommen = FALSE,
                                id_kolommen = NULL) {
  
  if (length(regel) > 1) {
    rlang::abort(
      c(
        "x" = paste0("Het aantal regels in groter dan een. Dit functie wordt",
                     " gebruikt om maar een regel te verwerken."),
        "i" = paste0("Gebruik een andere functie (misschien regels_toepassen?)",
                     " of geef een enkelvoudige regel.")
      )
    )
  }
  
  # we voegen rij index toe om elke rij in de output te indetificeren
  df_met_index <- df |> 
    dplyr::mutate(
      rij_idx = dplyr::row_number()
    ) |> 
    dplyr::relocate(rij_idx, .before = 1)
  
  controles <- validate::confront(df_met_index, regel)
  
  fouten <- validate::summary(controles) |> 
    dplyr::filter(fails > 0) |> 
    dplyr::pull(name)
  
  foutive_regelnamen <- regel[fouten] |> 
    validate::description()
  
  df_foutive_rijen <- validate::violating(df_met_index, controles) 
  
  if(alleen_gebruikt_kolommen) {
    
    # Extraheer de kolomnamen die zijn gedefinieerd in de regels (expr in het
    # yml-bestand). We gebruiken deze om een subset van kolommen te selecteren
    gebruikt_kolommen <- purrr::map(
      regel, 
      \(regel_object){
        regel_object |> 
          validate::expr() |> 
          all.vars()
      }) |> 
      purrr::list_c()
    
    if(!is.null(id_kolommen)) {
      gebruikt_kolommen <- unique(c("rij_idx", id_kolommen, gebruikt_kolommen))
    }
    
    df_foutive_rijen <- df_foutive_rijen |> 
      dplyr::select(tidyselect::all_of(gebruikt_kolommen))
  }
  
  # beperk het aantal rijen bij veel fouten
  if (nrow(df_foutive_rijen) > 10) {
    df_foutive_rijen <- df_foutive_rijen |>
      dplyr::slice(1:10)
  }
  
  tibble::tibble(
    description = validate::description(regel),
    foutive_rijen = list(df_foutive_rijen),
    controles = list(validate::summary(controles))
  )
}


#' Pas controles toe aan een dataset en maak en html logbestand met de output
#' van de controles
#'
#' @param datasetnaam character, de naam van de dataset waarop de controles
#'   worden toegepast. Dit komt in de ondertitel van de koptekst in het
#'   html-bestand.
#' @param pad_inputbestand character, het pad naar het inputbestand waarop de
#'   controles worden toegepast. Dit wordt alleen gebruikt om het pad in het
#'   html-bestand af te drukken.
#' @param pad_sjablon character, het pad naar de quarto sjablon.
#' @param pad_outputmap character, het pad naar de map waar de html-bestand
#'   wordt opgeslagen.
#' @param pad_tempmap character, pad naar een tijdelijke map waar bestanden
#'   worden opgeslagen. Kan NULL zijn (standaard) en dan wordt de map
#'   automatisch aangemaakt.
#' @param ... parameters voor `controles_toepassen`
#'
#' @return het pad naar de html-bestand
#' @export
controles_log <- function(datasetnaam,
                          pad_inputbestand,
                          pad_sjablon, 
                          pad_outputmap,
                          pad_tempmap = NULL,
                          ...) {
  
  if (is.null(pad_tempmap)) {
    pad_tempmap <- tempdir()
  } 
  
  # We willen de output object van de controles naar de Quarto report doorgeven
  # Maar Quarto parameterized reports alleen eenvoudige objecten als strings of
  # numeric vectors accepteren. Dus we gaan de output van de controles (een
  # tibble met nested columns) opslaan in een temp rds bestand en de pad naar de
  # bestand aan Quarto doorgeven als character stringr
  pad_tempbestand <- tempfile(fileext = ".rds", 
                              tmpdir = pad_tempmap)
  df_controles <- controles_toepassen(...)
  saveRDS(df_controles, pad_tempbestand)
  
  # render document
  quarto::quarto_render(
    input = pad_sjablon,
    execute_params = list(datasetnaam = datasetnaam, 
                          pad_inputbestand = pad_inputbestand,
                          pad_controlesoutput = pad_tempbestand)
  )
  
  # Schoonmaken
  # Verplaats (en hernoem) bestand naar  het outputmap want het is niet 
  # moegelijk om de outputpad van het html bestand in Quarto te definieren
  pad_outputbestand <- stringr::str_replace(pad_sjablon, ".qmd", ".html")
  basenaam <- stringr::str_to_lower(datasetnaam) |> 
    stringr::str_replace_all(" ", "_")
  logbestandnaam <- paste(format(Sys.time(),'%Y%m%d_%H%M%S'), 
                          "kv2_controles",
                          basenaam,
                          "log.html",
                          sep = "_")
  pad_logbestand <- file.path(pad_outputmap, logbestandnaam)
  fs::file_move(pad_outputbestand, pad_logbestand)
  
  list(
    pad_logbestand = pad_logbestand,
    controles_result = df_controles |> dplyr::select(controles) |> tidyr::unnest(controles)
  )
  
}


#' Een set correcties toepassen op een dataset en de resultaten van de
#' correcties in een html rapport
#'
#' @param df data.frame, de tabel die gecorrigeerd moet worden
#' @param correcties modifier object (van dcmodify package), de regels en
#'   correcties
#' @param datasetnaam character, de naam van de dataset die wordt gecorrigeerd.
#'   Dit wordt toegevoegd aan de titel van het rapport
#' @param pad_sjablon character, pad naar de sjablon voor de correcties rapport.
#' @param pad_outputmap character, pad naar de map waar de rapport wordt
#'   opgeslagen.
#' @param pad_tempmap character, pad naar temp map waar bestanden worden
#'   opgeslagen tijdens het rapport maken proces. Als deze NULL is, wordt deze
#'   automatisch aangemaakt
#'
#' @return
#' @export
correcties_toepassen <- function(
    df,
    correcties,
    datasetnaam,
    pad_sjablon,
    pad_outputmap,
    pad_tempmap = NULL) {
  
  if(is.null(correcties)) {
    
    output <- list(
      correcties = FALSE,
      df = df,
      rapportpad = NULL
    )
    return(output)
  }
  
  if (class(correcties) != "modifier") {
    rlang::abort(
      "correcties object moet een modifier object zijn."
    )
  }
  
  if (is.null(pad_tempmap)) {
    pad_tempmap <- tempdir()
  } 
  
  df_aangepast <- dcmodify::modify(df, correcties)
  
  # gecorrigeerd regels
  df_gecorrigeerd <- dplyr::anti_join(
    df_aangepast,
    df,
    by = colnames(df))
  
  if(nrow(df_gecorrigeerd) > 0) {
    # correcties in bestand opslaan
    correctiesbestandpad <- tempfile(tmpdir = pad_tempmap, fileext = ".csv")
    write.csv(df_gecorrigeerd, correctiesbestandpad, row.names = FALSE)
    
    # render document
    quarto::quarto_render(
      input = pad_sjablon,
      execute_params = list(pad_df = correctiesbestandpad, 
                            datasetnaam = datasetnaam)
    )
    
    # Schoonmaken
    # Verplaats (en hernoem) bestand naar  het outputmap want het is niet 
    # moegelijk om de outputpad van het html bestand in Quarto te definieren
    pad_outputbestand <- stringr::str_replace(pad_sjablon, ".qmd", ".html")
    logbestandnaam <- paste(format(Sys.time(),'%Y%m%d_%H%M%S'), 
                            "kv3_correcties",
                            datasetnaam,
                            "log.html",
                            sep = "_")
    pad_logbestand <- file.path(pad_outputmap, logbestandnaam)
    fs::file_move(pad_outputbestand, pad_logbestand)
    
    output <- list(
      correcties = TRUE,
      df = df_gecorrigeerd,
      rapportpad = pad_logbestand
    )
    
  } else {
    output <- list(
      correcties = FALSE,
      df = df,
      rapportpad = NULL
    )
  }
  output
}