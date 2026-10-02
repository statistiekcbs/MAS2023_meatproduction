#' Naam        : start.R
#' Auteurs     : Nicolette de Bruijn (NBUN), Kenneth Chin-A-Fat (KCIT) en Hugo Pineda Hernandez (HPEZ)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module start het systeem.


# laad packages
# Als een pakket niet is geïnstalleerd, installeer het dan eerst
using <- function(...) {
  # https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them/44660688#44660688
  libs <- unlist(list(...))
  req <-unlist (lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}
using("shiny", "shinyalert", "openxlsx", "lubridate", "readxl", "plyr", 
      "logging", "RODBC", "stringr", "dplyr", "tidyr", "purrr", "yaml",
      "glue", "cli", "here", "rlang", "validate", "fs", "withr","ggplot2")

# adresnorm

if(require(adresnorm) == FALSE) {
  remotes::install_git(url = "https://gitea.cbsp.nl/KERS/adresnorm.git",
                       subdir = "R/adresnorm")
}

App <- yaml::yaml.load_file(input = file.path("config", "app.ini"),
                            eval.expr = TRUE)
# Maak paden relatieve aan de working dir
App$srcmap       <- file.path(getwd(), App$srcmap)
App$jpgmap       <- file.path(getwd(), App$jpgmap)
App$logmap       <- file.path(getwd(), App$logmap)
App$configmap    <- file.path(getwd(), App$configmap)
App$controlesmap <- file.path(getwd(), App$controlesmap)
# sjablon map is niet een aboluut pad want Quarto kan niet met UNC paden werken
App$sjablonenmap <- file.path(".", App$sjablonenmap)

# mappen aanmaken als ze niet bestaan
c(App$controlesmap, App$logmap) |> 
  purrr::walk(\(m) {
    if(!dir.exists(m)) {
      dir.create(m)
    }
  })
# Voeg quarto pad toe 
Sys.setenv(PATH = paste(";C:\\Program Files/RStudio/resources/app/bin/quarto/bin", 
                        Sys.getenv("PATH"), sep = ";"))


# source scripts
source(file.path(App$srcmap, "loggen.R"))
source(file.path(App$srcmap, "algemeen.R"))
source(file.path(App$srcmap, "bijschatten.R"))
source(file.path(App$srcmap, "inlezen.R"))
source(file.path(App$srcmap, "database.R"))
source(file.path(App$srcmap, "afleiden_indicatoren.R"))
source(file.path(App$srcmap, "opslaan.R"))
source(file.path(App$srcmap, "ctcrbestand_aantallen_roodvlees.R"))
source(file.path(App$srcmap, "ctcrbestand_aantallen_witvlees.R"))
source(file.path(App$srcmap, "ctcrbestand_aantallenverdelingen.R"))
source(file.path(App$srcmap, "ctcrbestand_gemiddeldegewichten.R"))
source(file.path(App$srcmap, "ctcrbestand_diertotalen.R"))
source(file.path(App$srcmap, "analysebestand_tijdreeksen.R"))
source(file.path(App$srcmap, "analysebestand_bijschattingen.R"))
source(file.path(App$srcmap, "ophalen_outputdata.R"))
source(file.path(App$srcmap, "outputbestand_eurostat.R"))
source(file.path(App$srcmap, "outputbestand_eurostat_sdmx.R"))
source(file.path(App$srcmap, "outputbestand_Statline.R"))
source(file.path(App$srcmap, "outputbestand_DSC_roodvlees.R"))
source(file.path(App$srcmap, "outputbestand_DSC_witvlees.R"))
source(file.path(App$srcmap, "outputbestand_contactpersonen.R"))
source(file.path(App$srcmap, "rvo_hulpfuncties.R"))
source(file.path(App$srcmap, "rvo_controles_en_correcties.R"))
source(file.path(App$srcmap, "rvo_verwerken.R"))
source(file.path(App$srcmap, "ophalen_skal.R"))


# start het systeem
options(scipen = 999) # turn off scientific notation for numbers
options(browser="C:/Program Files/Google/Chrome/Application/chrome.exe")
  shiny::runApp(launch.browser=TRUE, appDir = file.path(App$srcmap))


