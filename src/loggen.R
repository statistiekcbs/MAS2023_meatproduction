#' Naam        : loggen.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module initialiseert de logging mbv een logbestand. Het is
#'               mogelijk om ook logging naar het scherm te krijgen.
#' Gebruik     : loginfo(msg="infomelding") voor handelingen van gebruiker (in
#'               server.R)
#'               logwarn(msg="warningmelding")
#'               logerror(msg="errormelding")
#'               logdebug(msg="debugmelding")

# bepaal logbestand met naamopmaak logging_<user>_<datumtijd>.txt
user         <- Sys.info()['login']
datumtijd    <- format(Sys.time(), format="%Y%m%d_%H%M%S")
bestandsnaam <- paste0("logging", "_", user, "_", datumtijd, ".txt")
if (!dir.exists(App$logmap)) {
  dir.create(App$logmap)
  message(glue::glue("Log map aangemaakt in {App$logmap}"))
}
logbestand   <- paste(App$logmap, bestandsnaam, sep="\\")

# start logging
#basicConfig(level='FINEST') # decommentarieer om logging ook naar dosbox te schrijven
addHandler(handler=writeToFile, file=logbestand, level='DEBUG')
setLevel('FINEST')
logdebug(msg="Start logging")

init_msg <- glue::glue(
  "\n ## Mappen gebruikt in huidige sessie:",
  "\n- Pad naar Input map is: {App$datamap}",
  "\n- Pad naar Werk map is: {App$werkmap}",
  "\n- Pad naar Output map is: {App$outputmap}",
  "\n- Pad naar src map (code) is: {App$srcmap}",
  "\n- Pad naar www map (css, plaatjes) is: {App$jpgmap}",
  "\n ## Database voor huidige sessie:",
  "\n- Databasenaam is: {App$databasenaam}",
  "\n- Servernaam is: {App$dbservernaam}",
  "\n- Schema is: {App$databaseschema}"
)

logdebug(msg=init_msg)
