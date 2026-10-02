#' Naam        : database.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module bevat functies om de database te lezen en te
#'               schrijven.


#' Escape single quotes door er nog een single quote voor te zetten.
#'
#' Zet een single quote voor een single quote omdat dit moet in sql-queries.
#' Normaliter is dit alleen nodig voor de slachthuisnaam.
#'
#' @param df dataframe
#' @param kols vector met kolomnamen die moeten worden ge-escaped
#' @return df met enkele quotes uit kols ge-escaped
#' @example
#' > df
#' >   kol1 slnm
#' 1 'allo  ho'
#' 2 hallo  hoi
#' 3  <NA> <NA>
#' > escape_quotes(df)
#' >   kol1 slnm
#' 1 ''allo  ho'
#' 2  hallo  hoi
#' 3   <NA> <NA>
dat_escape_quotes <- function (df, kols=c("slnm")) {
    for (i in 1:length(kols)) {
        for (j in 1:nrow(df)) {
            df[j, kols[i]] <- gsub("'", "''", df[j, kols[i]])
        }
    }
    return(df)
}


#' Haal gegevens op uit de database.
#'
#' @param sqlstr. Volledige sqlquery
#' @return data dataframe
dat_lees_db <- function(sqlstr,
                        drivernaam=App$dbdrivernaam,
                        servernaam=App$dbservernaam,
                        databasenaam=App$databasenaam) {
  logdebug(msg="Databasequery wordt uitgevoerd")

  connectstr <- paste0("Driver={", drivernaam, "}",
                       ";Server=", servernaam,
                       ";Database=", databasenaam,
                       ";trusted_connection=yes")
  dbconnection <- odbcDriverConnect(connectstr)
  data <- sqlQuery(channel=dbconnection, query=sqlstr, as.is=TRUE)
  odbcClose(dbconnection)

  if (is.character(data)){
    logerror(msg=paste("Databasequery is mislukt", sqlstr))
  } else {
    logdebug(msg="Databasequery is geslaagd")
  }

  return(data)
  }


#' Schrijf gegevens naar de database.
#'
#' @param sqlstr. Volledige sqlquery
#' @return sql_info string
dat_schrijf_db <- function(sqlstr,
                           drivernaam=App$dbdrivernaam,
                           servernaam=App$dbservernaam,
                           databasenaam=App$databasenaam) {
  #even een test met een andere database om te zien of de connectie tot stand wordt gebracht
  connectstr <- paste0("Driver={", drivernaam, "}",
                       ";Server=", servernaam,
                       ";Database=", databasenaam,
                       ";trusted_connection=yes")
  dbconnection <- odbcDriverConnect(connectstr)
  sql_info <- sqlQuery(dbconnection, sqlstr)
  odbcClose(dbconnection)
  print(sql_info) #niet erg informatief

  return(sql_info)
}


#' Verwijder gegevens en sla vervangende gegevens op.
#'
#' KCIT: gaan we deze functie gebruiken of dat_schrijf_weg() of beide?
#' @param sqlstr_verwijderen Volledige sqlquery
#' @param sqlstr_opslaan Volledige sqlquery
#' @return sql_info string
dat_updaten_db <- function(sqlstr_verwijderen,
                           sqlstr_opslaan,
                           drivernaam=App$dbdrivernaam,
                           servernaam=App$dbservernaam,
                           databasenaam=App$databasenaam) {
# beide acties moeten binnen een TRAN worden geplaatst
#   BEGIN TRY
#   BEGIN TRAN
#
#   acties
#
#   COMMIT TRAN
#   END TRY
#   BEGIN CATCH
#   PRINT 'De transactie is teruggedraaid fout: ' + cast(error_message() as nvarchar(max))
#   ROLLBACK TRAN
#   END CATCH

  #voorbeeld
  sqlstr_verwijderen <- paste0("DELETE FROM [ont].[tbl_microbase_verd] ",
                        "WHERE vwmd='201905' AND vsmd IN ('201812','201901','2019021','201903')")

  #voorbeeld
  sqlstr_opslaan <- paste0("INSERT INTO [ont].[tbl_microbase_verd](vwmd,vsmd,dsrt,dcat,verd) ",
                      "VALUES('201906','201812','Rund','Stieren',11.08), ",
                      "('201906','201812','Rund','Koeien',86.59), ",
                      "('201906','201812','Rund','Vaarzen',2.33), ",
                      "('201906','201812','Kalf','Kalveren 0-8 mnd',95), ",
                      "('201906','201812','Kalf','Kalveren 8-12 mnd',5)")

  sqlstr <- paste("BEGIN TRY    BEGIN TRAN ",
    sqlstr_verwijderen,
    sqlstr_opslaan,
    "COMMIT TRAN     END TRY      BEGIN CATCH      PRINT 'De transactie is teruggedraaid fout: ' + cast(error_message() as nvarchar(max))     ROLLBACK TRAN    END CATCH")
  print(sqlstr ) #ziet er niet uit, werkt wel

  #even een test met een andere database om te zien of de connectie tot stand wordt gebracht
  connectstr <- paste0("Driver={", drivernaam, "}",
                       ";Server=", servernaam,
                       ";Database=", databasenaam,
                       ";trusted_connection=yes")
  dbconnection <- odbcDriverConnect(connectstr)
  sql_info <- sqlQuery(dbconnection, sqlstr)
  #sql_info_verwijderen <- sqlQuery(dbconnection, sqlstr_verwijderen)
  #sql_info_opslaan <- sqlQuery(dbconnection, sqlstr_opslaan)

  odbcClose(dbconnection)
  print(sql_info)
  #print(sql_info_verwijderen) #niet erg informatief
  #print(sql_info_opslaan) #niet erg informatief
  #return(list(sql_info_verwijderen=sql_info_verwijderen,sql_info_opslaan=sql_info_opslaan))
  return(sql_info=sql_info)
}


#' Maak insert sqlquery.
#'
#' Uitgangspunten:
#' - zorg ervoor dat de elementen van de vector exact overeenkomen met (en in
#'   dezelfde volgorde staan als) de kolommen in de databasetabel
#' - alle waarden worden omgezet naar strings, maar dat wordt goed weggeschreven
#' - NA in de vector wordt omgezet naar een NULL in de sqlquery (zonder quotes)
#' @param record dataframe met 1 record of een vector
#' @param tabel string met databasetabel waarnaar record moet worden weggeschreven
#' @param schema string met databaseschema van tabel
#' @return niets
#' @example
#' > rec
#'     vwmd              vsmd              wrkp     idbs              aant
#' "201801"          "201709"         "0200043"      "0"          "     9"
#' > maak_insert_sqlquery(record=rec, tabel='dbo.tbl_microbase_rdvl')
#'[1] "insert into dbo.tbl_microbase_rdvl values ( '201801', '201709', '0200043', '0', '     9')"
dat_maak_insert_sqlquery <- function(record, tabel, schema=App$databaseschema) {

    # stel query samen
    qry <- paste0("insert into ", schema, ".", tabel, " values (")
    for (i in 1:length(record)) {
        if (is.na(record[i])) {
            qry <- paste0(qry, 'NULL') # NA wordt NULL zonder extra quotes eromheen
        } else {
            qry <- paste0(qry, "'", record[i], "'")
        }

        if (i < length(record)) {
            qry <- paste0(qry, ", ")
        }
    }
    qry <- paste0(qry, ")")
    return(qry)
}


#' Schrijf record weg naar de databasetabel.
#'
#' Deze functie stelt een insert-query samen obv het record en voert de query
#' uit op de database.
#' @param record vector met names
#' @param tabel string met databasetabel waarnaar record wordt weggeschreven
#' @param dbconn string met databaseconnectie
#' @return niets
dat_schrijf_record_weg <- function(record, tabel, dbconn) {
    qry <- dat_maak_insert_sqlquery(record=record, tabel=tabel)

    # voer query uit
    sql_res <- sqlQuery(dbconn, qry)
    if (!identical(sql_res, character(0))){
        print(paste("Databasequery is mislukt:", qry, sql_res))
        logerror(paste("Databasequery is mislukt:", qry, sql_res))
        xxx # forceer crash
    }
}


#' Schrijf dataframe weg naar de databasetabel.
#'
#' Deze functie schrijft de inhoud van een dataframe weg naar een
#' databasetabel. We gaan ervan uit dat het dataframe exact dezelfde
#' structuur heeft als de databasetabel. We verwijderen eerst alle records
#' uit de databasetabel van de meegegeven verwerkingsmaand.
#' @param df dataframe met weg te schrijven data
#' @param tabel string met databasetabel waarnaar record wordt weggeschreven
#' @param vwmd string met verwerkingsmaand
#' @param def boolean die aangeeft of de databasetabel definitieve cijfers bevat
#'            (oftewel, eindigt op def) of niet. Als def=TRUE, dan vsmd meegeven
#' @param vsmd string met verslagmaand. Alleen nodig als def=TRUE
#' @param drivernaam string
#' @param servernaam string
#' @param databasenaam string

#' @return niets
dat_schrijf_weg <- function(df, tabel, vwmd,
                            def=FALSE,
                            vsmd='',
                            drivernaam=App$dbdrivernaam,
                            servernaam=App$dbservernaam,
                            databasenaam=App$databasenaam,
                            schema=App$databaseschema) {

    # maak verbindingstring met database
    connectstr <- paste0("Driver={", drivernaam, "}",
                         ";Server=", servernaam,
                         ";Database=", databasenaam,
                         ";trusted_connection=yes")
    dbconn <- odbcDriverConnect(connectstr)

    # verwijder bestaande data van verwerkingsmaand of verslagmaand
    if (!def) {
        qry_del <- paste0("delete from ", schema, ".", tabel, " where vwmd='", vwmd, "'")
    } else {
        qry_del <- paste0("delete from ", schema, ".", tabel, " where vsmd='", vsmd, "'")
    }
    sql_res <- sqlQuery(dbconn, qry_del)
    # geeft foutmelding terug als niets te deleten
    # bv [RODBC] ERROR: Could not SQLExecDirect 'delete from ont.tbl_hist_microbase_rdvl where vwmd='201801''
    #if (!identical(sql_res, character(0))){
    #    print(paste("Databasequery is mislukt:", qry_del, sql_res))
    #    logerror(paste("Databasequery is mislukt:", qry_del, sql_res))
    #    xxx # forceer crash
    #}
    # schrijf data weg
    if ("slnm" %in% colnames(df)) {
        df <- dat_escape_quotes(df=df)
        }
    apply(df, 1, dat_schrijf_record_weg, tabel=tabel, dbconn=dbconn)

    odbcClose(dbconn)
}
