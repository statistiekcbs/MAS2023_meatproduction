#' Naam        : bijschatten.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Module om de bijschattingen in de controle- en
#'               correctiebestanden voor aantallen roodvlees en witvlees toe
#'               te voegen.


#' Maak van de waarde een mooi rond getal.
#'
#' De bijschattingen die op het 12-maandsgemiddelde zijn gebaseerd, worden
#' afgerond op een 'mooi' getal.
#'
#' @param w waarde
#' @return w afgerond naar een mooi getal
bij_maak_rond <- function(w) {
    if (is.nan(w)) {
        return(0)
    } else if (w < 10) {
        return(w)
    } else if (w < 100) {
        5*round(w/5) # naar dichtsbijzijnde 5-tal
    } else if (w < 500) {
        50*round(w/50) # naar dichtsbijzijnde 50-tal
    } else {
        100*round(w/100) # naar dichtsbijzijnde 100-tal
    }
}


#' Voeg bijschattingen toe
#'
#' Voeg bijschattingen toe indien nodig.
#' Uitgangspunten bijschatten:
#'   - schat alleen bij voor de vier verslagmaanden vd huidige verwerkingsmaand
#'   - schat kalkoenen nooit bij
#'   - als een 12-maandsgemiddelde wordt gebruikt voor een bijschatting, dan is
#'     dit gemiddelde gebaseerd op de laatste 12 maanden die al definitief
#'     zijn. Vb. Als de verslagmaanden 201801 t/m 201804 zijn, dan is het
#'     12-maandsgemiddelde gebaseerd op de definitieve maanden 201701 t/m 201712
#'   - een 0 blijft staan (en wordt niet overschreven met een bijschatting)
#'
#' Methode bijschatten:
#'   - als een waarde in verslagmaanden m-3 t/m m-1 ontbreek, neem dan de waarde
#'     van de vorige verwerkingsmaand (vorige levering)
#'   - als een waarde in laatste verslagmaand ontbreekt, neem dan het
#'     12-maandsgemiddelde als deze groter of gelijk is aan 100
#'
#' @param df dataframe met inhoud uit levering in vorm van ctcr-bestand
#' @param df_def dataframe met waarden uit laatste 12 maanden die definitief zijn
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @return df dataframe met inhoud uit levering evt aangevuld met
#'              bijschattingen in vorm van ctcr-bestand
#' @return df_bijschatting dataframe met alle bijschattingen om straks in het
#'                         ctcr-bestand te kunnen markeren
#' @example
#' > data
#'                    slnm    wrkp               dsrt vvm_mnd1 vvm_mnd2 vvm_mnd3 201711 201712 201801 201802 ind1 ind2 ind3
#' BEDRIJFSNAAM 1 0901354           Kalveren     1500     1500     1500     NA     NA     NA     NA
#' BEDRIJFSNAAM 1 0901354 Volwassen runderen     5000     5000     5000     NA     NA     NA     NA
#'  BEDRIJFSNAAM 2 1646092  Eenhoevige dieren        1        6        1      1      6      1      2
#' > bij_voeg_bijschattingen_toe(df, df_def, maanden_huidige_vm)$df
#'                    slnm    wrkp               dsrt vvm_mnd1 vvm_mnd2 vvm_mnd3 201711 201712 201801 201802 ind1 ind2 ind3
#' BEDRIJFSNAAM 1 0901354           Kalveren     1500     1500     1500   1500   1500   1500   1600
#' BEDRIJFSNAAM 1 0901354 Volwassen runderen     5000     5000     5000   5000   5000   5000   7200
#'  BEDRIJFSNAAM 2 1646092  Eenhoevige dieren        1        6        1      1      6      1      2
#' > bij_voeg_bijschattingen_toe(df, df_def, maanden_huidige_vm)$df_bijschatting
#'                    slnm    wrkp               dsrt   vsmd aant
#' BEDRIJFSNAAM 1 0901354           Kalveren 201711 1500
#' BEDRIJFSNAAM 1 0901354 Volwassen runderen 201711 5000
#' BEDRIJFSNAAM 1 0901354           Kalveren 201712 1500
bij_voeg_bijschattingen_toe <- function(df, df_def, maanden_huidige_vm) {
  # initaliseer dataframe om alle bijschattingen bij te houden
  
  df_bijschatting <- data.frame("slnm"=character(), "wrkp"=character(),
                                "dsrt"=character(), "vsmd"=character(),
                                "aant"=character(), stringsAsFactors=FALSE)

  for (i in 1:nrow(df)) {
    rij <- df[i, ]
    if (rij$dsrt != 'Kalkoenen') { # kalkoenen niet bijschatten
      
      # verslagmaanden m-3 t/m m-1: pak waarden vorige verwerkingsmaand
      for (v_kol in match(maanden_huidige_vm[1:3], names(df))) { # verslagmaanden m-3 t/m m-1
        if (alg_is_leeg(rij[, v_kol])) { # als waarde ontbreekt
          bs <- rij[, v_kol-3] # bijschatting is waarde vorige verwerkingsmaand
            if (!is.na(bs)) {
              vsmd <- names(df)[v_kol] # vind de uitgeschreven verslagmaand
              rec_bs <- c(rij$slnm, rij$wrkp, rij$dsrt, vsmd, bs) #MDAK
              #rec_bs <- c(rij$wrkp, rij$dsrt, vsmd, bs)
              # voeg toe aan df_bijschatting
              df_bijschatting <- rbind(df_bijschatting, rec_bs,
                                       stringsAsFactors=FALSE)
              }
          }
       colnames(df_bijschatting) <- c("slnm", "wrkp", "dsrt","vsmd", "aant") # weer goedzetten #MDAK

      }

      # laatste verslagmaand m: pak 12-maandsgemiddelde definitieve maanden
      # als deze groter is dan 100 (mooi getal)
      vsmd <- maanden_huidige_vm[4] # laatste verslagmaand
      vsmd_index <- match(vsmd, names(df))
      if (alg_is_leeg(rij[, vsmd_index])) {
        
        # bijschatting is 12-maandsgemiddelde
        # as.numeric want df_def$aant_gaaf komt als character uit de database
        # (dit is vanwege het gebruik van as.is in dat_lees_db -> sqlQuery)
        bs <- mean(as.numeric(df_def[df_def$wrkp == rij$wrkp &
                          df_def$dsrt == rij$dsrt, 'aant_gaaf']))
        if (!is.nan(bs)) {
          bs <- as.integer(bs) # bijschatting kan alleen integer zijn
          if (bs >= 100) {
            bs_rond <- bij_maak_rond(bs)
            rec_bs <- c(rij$slnm, rij$wrkp, rij$dsrt, vsmd, bs_rond)
            # voeg toe aan df_bijschatting
            df_bijschatting <- rbind(df_bijschatting, rec_bs,
                                     stringsAsFactors=FALSE)
          }
        }
        colnames(df_bijschatting) <- c("slnm", "wrkp", "dsrt", "vsmd", "aant") # weer goedzetten #MDAK
      }
    }
  }

#browser()
  # voeg de bijschattingen toe aan de data obv df_bijschatting
  if (nrow(df_bijschatting) > 0) {
      for (i in 1:nrow(df_bijschatting)) {
          df_bs <- df_bijschatting[i, ]
          df[df$slnm == df_bs$slnm & df$wrkp == df_bs$wrkp & 
             df$dsrt == df_bs$dsrt, df_bs$vsmd] <- df_bs$aant
      }
  }

  df <- df |> 
    dplyr::mutate(
      # maak van de waarden numerics
      dplyr::across(tidyselect::matches("[0-9]{6}|vvm_mnd[1-3]"), as.numeric)
    )
  
  return(list(df=df, df_bijschatting=df_bijschatting))
}
