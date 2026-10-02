#' Experimenting with S7 to make a generic synth data structure
#'
#'

library(S7)

#' wat moet synthetische data kunnen:
#'
#' attributen: 
#'  - uiteindelijke df met data
#'  
#' methoden:
#'  - formatteren van de input naar df met de juiste headers
#'  - opslaan van file (om hetzelfde aan te roepen, even though diffs between xls en csv)
#'  
#'  TODO: rebase synthetische data branch, installeer de S7 via het start bestand,
#'  test of de methode goed werkt, test of de constructor inderdaad gebruikt 
#'  kan worden als vervanging voor levering

synth_class <- new_class("synth data", 
  properties = list(
    # df_data, 
    # ORR all individual columns as their own properties??
    geboorte_datum = new_property(class_numeric, default = NA),
    bestandnaam,
  ),
  constructor = function(n_regels) {
    warning("de constructor om data te genereren is nog niet geïmplementeerd")
    new_object(
      S7_object(),
      
      geboorte_datum = sample(
        seq(lubridate::ymd("2010-01-01"),
            lubridate::ymd("2023-01-01"),
            by = "day"
        ),
        n_regels,
        replace = TRUE
      )
      # alle properties worden hier geplaatst         
    )
  }
)
# TODO methoden (genereer data moet ook fouten toestaan)
# genereer_data, schrijf_bestand, formateer_workbook, lees_bestand_in

schrijf_bestand <- new_generic("schrijf_synth_bestand", "x")
method(schrijf_bestand, synth_class) <- function(x) {
  
}




############ testing class #################