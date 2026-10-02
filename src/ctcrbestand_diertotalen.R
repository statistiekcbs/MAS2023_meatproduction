#' Verzamel de gegevens uit de levering en de historische gegevens en
#' stel op basis hiervan een dataframe samen met diertotalen.
#'
#' @param jrmnd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param levering_bst_nvwa string met volledig pad naar inputbestand van nvwa
#' @param levering_bst_rvo string met volledig pad naar verwerkte inputbestand van rvo
#' @param maanden_huidige_vm list met verslagmaanden horend bij verwerkingsmaand
#' @param schema string met databaseschema
#' @return dataframe met de diertotalen van huidige en vorige verwerkingsmaand
verzamel_data_ctcr_diertot <- function(jrmnd, levering_bst_nvwa, levering_bst_rvo,
                                    maanden_huidige_vm,
                                    schema=App$databaseschema,
                                    tabel=App$tbl_microbase_rdvl,
                                    tabel_def=App$tbl_microbase_rdvl_def){
  
  # voeg de rvo en nvwa data samen
  df_lev_rdvl <- voeg_rvo_runderen_bij_nvwa(bst_nvwa=levering_bst_nvwa, 
                                            bst_rvo=levering_bst_rvo, jrmnd)
  
  # haal levering op en zet om in draaitabel
  df_lev      <- inl_maak_draaitabel(df=df_lev_rdvl, vsmd=maanden_huidige_vm,
                                     extra_kolommen=c("is_biologisch", "bron"))
  df_lev      <- subset(df_lev, select=-c(vwmd)) # verwijder kolom vwmd
  
  # zet de aantallenkolommen om naar numeriek
  df_lev[c(unlist(jrmnd[1]),unlist(jrmnd[2]),unlist(jrmnd[3]),unlist(jrmnd[4]))] <- as.numeric(unlist(df_lev[c(unlist(jrmnd[1]),unlist(jrmnd[2]),unlist(jrmnd[3]),unlist(jrmnd[4]))]))
  
  # haal het "UBN:" gedeelte uit de wrkp
  df_lev$wrkp <- str_remove_all(df_lev$wrkp, "UBN:")
  
  totalen_lev <- df_lev %>%
    group_by(dsrt) %>%
    summarise(assign(unlist(jrmnd[1]), sum(get(unlist(jrmnd[1])),na.rm=T)),
              assign(unlist(jrmnd[2]), sum(get(unlist(jrmnd[2])),na.rm=T)),
              assign(unlist(jrmnd[3]), sum(get(unlist(jrmnd[3])),na.rm=T)),
              assign(unlist(jrmnd[4]), sum(get(unlist(jrmnd[4])),na.rm=T)))
  
  colnames(totalen_lev) <- c("dsrt",jrmnd[1:4])
  
  totalen_lev <- totalen_lev %>%
    pivot_longer(cols = c(2:5), names_to = "maand", values_to = "aant_lev")
  
  # haal de historische data op uit de database
  df_hist     <- inl_lees_hist_aant(jrmnd=jrmnd, tabel=App$tbl_microbase_rdvl)
  
  # haal het "UBN:" gedeelthe uit de wrkp
  df_hist$wrkp <- str_remove_all(df_hist$wrkp, "UBN:")
  
  # haal hist data met runderen weg (gebruiken nu gesplitste onderverdelingen)
  df_hist_zonder_runderen <- dplyr::filter(df_hist, dsrt != "Kalveren" & dsrt != "Volwassen runderen")
  
  colnames(df_hist_zonder_runderen)[5:7] <- jrmnd[1:3]
  
  # bereken de totalen per maand
  totalen_hist <- df_hist_zonder_runderen %>%
    group_by(dsrt) %>%
    summarise(assign(unlist(jrmnd[1]), sum(get(unlist(jrmnd[1])),na.rm=T)),
              assign(unlist(jrmnd[2]), sum(get(unlist(jrmnd[2])),na.rm=T)),
              assign(unlist(jrmnd[3]), sum(get(unlist(jrmnd[3])),na.rm=T)))
  
  colnames(totalen_hist) <- c("dsrt",jrmnd[1:3])
  
  # zet de tabel om naar long format
  totalen_hist <- totalen_hist %>%
    pivot_longer(cols = c(2:4), names_to = "maand", values_to = "aant_hist")
  
  totalen <- full_join(totalen_lev,totalen_hist)
  
  return(totalen)
}

#' Maak het controle- en correctiebestand voor de vergelijking van diertotalen.
#'
#' @param jrmd list met verslagmaanden, verwerkingsmaand en vorige
#'             verwerkingsmaand. Output van alg_vind_jaarmaanden()
#' @param levering_bst_nvwa string met volledig pad naar inputbestand van nvwa
#' @param levering_bst_rvo string met volledig pad naar verwerkte inputbestand van rvo
#' @param outputbestand string met volledig pad naar outputbestand
#' @return txt. string met tekst die naar het scherm wordt geschreven
maak_ctcr_dier_tot <- function(jrmnd, levering_bst_nvwa, levering_bst_rvo,
                                maanden_huidige_vm,
                                schema=App$databaseschema,
                                tabel=App$tbl_microbase_rdvl,
                                tabel_def=App$tbl_microbase_rdvl_def, outputbestand) {
  
  totalen <- verzamel_data_ctcr_diertot(jrmnd, levering_bst_nvwa, levering_bst_rvo,
                                     maanden_huidige_vm,
                                     schema,tabel,tabel_def)
  
  # bepaal de kleur van de grafieken
  legend_colors <- c("aant_lev" = "blue", "aant_hist" = "red")

  ggplot(data = totalen, mapping = aes(x = maand, y = aant_lev, group = dsrt, color = "aant_lev")) + geom_point() + geom_line() + 
    geom_point(mapping = aes(x = maand, y = aant_hist, group = dsrt, color = "aant_hist")) + geom_line(mapping = aes(x = maand, y = aant_hist, group = dsrt, color = "aant_hist")) + 
    scale_y_continuous(limits = c(0, NA)) + facet_wrap("dsrt", scales = "free", ncol = 3) + labs(color = "legenda") + 
    scale_color_manual(values = legend_colors) + 
    theme_bw()
  
  ggsave(outputbestand, units = "px", width = 2750, height = 2750)
}
