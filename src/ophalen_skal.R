
#' Haal de SKAL certificaten data op uit de SKAL database voor de I&R bio
#' verwerken
#' @param qry_pad string, pad naar de query die de data uit de skal database ophaalt
#' @param hoofsbi string, de hoofdsbi van het certificaten dat moeten worden
#'   opgehaald uit de database. Deaufault waarde komt uit config bestand
#' @param dbdrivernaam  string, drivernaam van database. Default waarde komt uit
#'   config bestand
#' @param dbservernaam  string, servernaam van database. Default waarde komt uit
#'   config bestand
#' @param databasenaam  string, databasenaam van database. Default waarde komt
#'   uit config bestand
#' @param dbschema  string, schema van database. Default waarde komt uit config
#'   bestand
#' @param tbl_activiteiten string, naam van de activiteiten tabel in database.
#'   Default waarde komt uit config bestand
#' @param tbl_locaties string, naam van de locaties tabel in database.
#'   Default waarde komt uit config bestand
#' @param tbl_certificaten string, naam van de certificaten tabel in database.
#'   Default waarde komt uit config bestand
#' @param tbl_bedrijven string, naam van de bedrijven tabel in database.
#'   Default waarde komt uit config bestand
#'
#' @return data.frame met kolommen:
#'  - skalnummer
#'  - datum_certificatie_geldigheid
#'  - datum_geldig_vanaf
#'  - huisnummer
#'  - postcode
#'  - hoofdsbi
#'
#' @export
oph_haal_skal_op <- function(qry_pad,
                             hoofsbi = App$rund_skal_hoofdsbi,
                             dbdrivernaam=App$skal_dbdrivernaam,
                             dbservernaam=App$skal_dbservernaam,
                             databasenaam=App$skal_databasenaam,
                             dbschema=App$skal_databaseschema,
                             tbl_activiteiten = App$tbl_skal_activiteiten,
                             tbl_locaties = App$tbl_skal_locaties,
                             tbl_certificaten = App$tbl_skal_certificaten,
                             tbl_bedrijven = App$tbl_skal_bedrijven
                             ) {
  
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = dbdrivernaam,
                        Server = dbservernaam,
                        Database = databasenaam
  )
  
  tbl_sc_cert <- DBI::Id(schema = dbschema, table = tbl_certificaten)
  tbl_sc_actv <- DBI::Id(schema = dbschema, table = tbl_activiteiten)
  tbl_sc_loca <- DBI::Id(schema = dbschema, table = tbl_locaties)
  tbl_sc_bedr <- DBI::Id(schema = dbschema, table = tbl_bedrijven)
  
  df <- readr::read_lines(qry_pad) |>
    paste(collapse = "\n") |> 
    glue::as_glue() |> 
    glue::glue_sql(.con = con) |> 
    DBI::dbGetQuery(conn = con, statement = _)

  DBI::dbDisconnect(con)
  
  df
}