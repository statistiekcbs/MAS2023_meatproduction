#' Naam        : voer_testen_uit.R
#' Auteur(s)   : Nicolette de Bruijn (NBUN), Kenneth Chin-A-Fat (KCIT),
#'               Hugo Pineda Hernandez (HPEZ) en Valerie Sawirja (VSAA)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module voert de unittesten uit en toont de dekking
library(testthat)
library(covr)

root_map <- here::here()
src_map <- file.path(root_map, 'src')
test_map <- file.path(root_map, "unittesten")
testdata_map <- file.path(test_map, "testdata")
config_map <- file.path(root_map, "config")
# maak de sjablonen map relatief, om te omzeilen dat quarto niet met
# netwerkmappen werkt (\\cbsp.nl)
sjablonen_map <- file.path(".", "src", "sjablonen")

configbestand_pad <- file.path(config_map, "test_app.ini")

if(!file.exists(configbestand_pad)) {
  rlang::abort(
  "Het configuratiebestand voor de tests bestaat niet. Daarom kunnen de tests
  niet worden uitgevoerd. Maak een configuratiebestand aan met de naam
  'test_app.ini' in de 'config' map en voer dit bestand opnieuw uit.")
}

App <- yaml::read_yaml(
  file = configbestand_pad,
  eval.expr = TRUE
)
App$logmap <- withr::local_tempdir()
# Hack om het testen mogelijk te maken van functies die aanroepen bevatten naar
# de *Progress-functies van shiny wanneer er geen shiny app draait
incProgress <- function(...) {}
withProgress <- function(...) list(...)[["expr"]]
setProgress <-  function(...) {}

# toon alle ongedekte coderegels
dekking <- file_coverage(source_files=c(
                                        file.path(src_map, 'afleiden_indicatoren.R'),
                                        file.path(src_map, 'algemeen.R'),
                                        file.path(src_map, 'bijschatten.R'),
                                        file.path(src_map, 'inlezen.R'),
                                        file.path(src_map, 'outputbestand_eurostat_sdmx.R'),
                                        file.path(src_map, "synthetische_data.R"),
                                        file.path(src_map, "rvo_verwerken.R"),
                                        file.path(src_map, "rvo_controles_en_correcties.R"),
                                        file.path(src_map, "rvo_hulpfuncties.R"),
                                        file.path(src_map, "opslaan.R"),
                                        file.path(src_map, "ctcrbestand_aantallen_roodvlees.R"),
                                        file.path(src_map, "ctcrbestand_aantallen_witvlees.R"),
                                        file.path(src_map, "ctcrbestand_aantallenverdelingen.R"),
                                        file.path(src_map, "ctcrbestand_gemiddeldegewichten.R"),
                                        file.path(src_map, "analysebestand_tijdreeksen.R"),
                                        file.path(src_map, "analysebestand_bijschattingen.R"),
                                        file.path(src_map, "ophalen_outputdata.R"),
                                        file.path(src_map, "outputbestand_eurostat.R"),
                                        file.path(src_map, "outputbestand_eurostat_sdmx.R"),
                                        file.path(src_map, "outputbestand_Statline.R"),
                                        file.path(src_map, "outputbestand_DSC_roodvlees.R"),
                                        file.path(src_map, "outputbestand_DSC_witvlees.R"),
                                        file.path(src_map, "outputbestand_contactpersonen.R"),
                                        file.path(src_map, "database.R"),
                                        file.path(src_map, "bijschatten.R")
                                        ),
                         test_files=c(
                           # We comment dit tijdelijk totdat we
                           # besluiten dat de indicatoren worden bijgewerkt.
                           # Door deze regel te becommentariëren laten we de
                           # rest van de tests uitvoeren binnen de
                           # bestands_dekkingsfunctie 
                           # file.path(tst_map, 'test_afleiden_indicatoren.R'),
                                      file.path(test_map, 'test_algemeen.R'),
                                      file.path(test_map, 'test_bijschatten.R'),
                                      file.path(test_map, 'test_inlezen.R'),
                                      file.path(test_map, 'test_eurostat.R'),
                                      file.path(test_map, "test_synthetische_data.R"),
                                      file.path(test_map, "test_rvo_verwerken.R"),
                                      file.path(test_map, "test_rvo_controles_en_correcties.R"),
                                      file.path(test_map, "test_rvo_hulpfuncties.R")
                                      ),
                         function_exclusions= c('alg_vind_default_inputmap',
                                                'alg_vind_ctcr_bestanden',
                                                'alg_vind_analysebestanden',
                                                'alg_vind_outputbestanden',
                                                'alg_hercodeer_maand',
                                                'alg_vind_plaatje',
                                                'inl_lees_hist_aant',
                                                'inl_lees_hist_verd',
                                                'inl_lees_hist_gemg',
                                                'inl_lees_vlp_tijdreeksen',
                                                'inl_lees_hist_tijdreeksen',
                                                'inl_lees_bijschattingen',
                                                'inl_lees_statbase_output')
                         )
ongedekt <- subset(tally_coverage(dekking), value == 0)
rownames(ongedekt) <- NULL
print(ongedekt)
print(paste("De bovenstaande", nrow(ongedekt), "regels worden niet gedekt door de unittesten"))

# toon dekkingspercentage
print(dekking)

#' voer unittesten uit en toon resultaten
#' opties voor reporter (meest zijn hetzelfde):
#'      "check" (overzicht), "fail", "list", "minimal", "multi",
#'      "rstudio", "silent", "stop", "summary" (default),
#'      "tap" (uitgebreid), "teamcity" (heel uitgebreid)
test_dir(path=test_map, reporter=c("check"), stop_on_failure = FALSE)
