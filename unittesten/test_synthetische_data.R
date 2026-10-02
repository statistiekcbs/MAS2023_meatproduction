library(testthat)


# source het te testen script
root_map <- here::here()
src_map <- file.path(root_map, 'src')
source(file.path(src_map, "synthetische_data.R"))

test_that("maak_id functie maakt geen gedupliceerd ids", {
  
  expect_length(
    unique(maak_id(n = 10, n_cijfers = 1)), 
    10)
  
  expect_length(
    unique(maak_id(n = 100, n_cijfers = 2)), 
    100)

  })

test_that("fouten in maak_id functie", {
  # Alle id's moeten uniek zijn, dus als we een aantal id's aanvragen dat groter
  # is dan het laatst mogelijke aantal id's, krijgen we een foutmelding.
  expect_error(
    maak_id(11, 1)
  )
})


test_that("maak_ubns functie", {
  
  # Deze functie maakt de UBN's aan voor verblijfplaatsen 2 en 3. Hij wordt
  # gebruikt bij het aanmaken van het slachtingenbestand en de uitvoer ervan zal
  # kolommen UBN_2 en UBN_3 aanmaken. Deze id's kunnen willekeurig gegenereerd
  # worden of gekozen worden uit een reeds bestaande reeks id's, meegegeven in
  # het argument verblijfplaatsen_ubn
  
  expect_named(maak_ubns(n_locaties = 1), c("ubn_2", "ubn_3"))
  
  # Als het aantal locaties 1 is, dan moeten locaties 2 en 3 leeg zijn:
  df_output <- maak_ubns(n_locaties = 1)
  expect_true(is.na(df_output$ubn_2))
  expect_true(is.na(df_output$ubn_3))
  
  # Als het aantal locaties 2 is, dan moet locatie 3 leeg zijn:
  df_output <- maak_ubns(n_locaties = 2)
  expect_false(is.na(df_output$ubn_2))
  expect_true(is.na(df_output$ubn_3))
  
  # Als het aantal locaties 1 is, dan moeten locaties 2 en 3 niet leeg zijn:
  df_output <- maak_ubns(n_locaties = 3)
  expect_false(is.na(df_output$ubn_2))
  expect_false(is.na(df_output$ubn_3))
  
  # Als UBN'subns worden opgegeven in `verblijfplaatsen_ubn`, dan moet de inhoud van
  # de kolommen een van deze UBN's zijn
  ubns <- c("a", "b", "c")
  df_output <- maak_ubns(n_locaties = 3, verblijfplaatsen_ubn = ubns)
  
  expect_true(df_output$ubn_2 %in% ubns)
  expect_true(df_output$ubn_3 %in% ubns)
  
 
})

test_that("maak_ubns functie", {
  
  # retourneer data.frame
  expect_s3_class(
    maak_ubns(2),
    "data.frame"
  )
  # kolomnamen kloppen
  expect_named(
    maak_ubns(2),
    c("ubn_2", "ubn_3")
  )
  
  #  de meegeleverde UBN's worden gebruikt
  df_output <- maak_ubns(n_locaties = 3, 
                         verblijfplaatsen_ubn = c("b", "c"))
  
  expect_equal(
    sort(c(df_output$ubn_2, df_output$ubn_3)),
    c("b", "c")
  )
  
  # ubn_1 is verwijderd 
    df_output <- maak_ubns(n_locaties = 3, 
                         verblijfplaatsen_ubn = c("a", "b", "c"),
                         ubn_1 = "a")
  
  expect_equal(
    sort(c(df_output$ubn_2, df_output$ubn_3)),
    c("b", "c")
  )
  
})

test_that("Fouten in maak_ubns functie", {
  
  # De functie zou moeten falen als het aantal locaties groter is dan 3 of als
  # er minder UBN's zijn verstrekt maar het aantal kleiner is dan het aantal
  # locaties.
  expect_error(
    maak_ubns(n_locaties = 4)
  )
  
  expect_error(
    maak_ubns(n_locaties = 3, 
              verblijfplaatsen_ubn = "a")
  )
  
  expect_error(
    maak_ubns(n_locaties = 3, 
              verblijfplaatsen_ubn = c("a", "b"),
              ubn_1 = "a")
  )
})


test_that("het synthetische excel bestand voor verdeling en gewichten correct is", {
  set.seed(1)
  
  verw <- list(
    df_verd = data.frame(
      stringsAsFactors = FALSE,
      row.names = c("StierenVerd", "KoeienVerd", "VaarzenVerd"),
      hvm_mnd1 = c(24.67, 73, 2.33),
      hvm_mnd2 = c(21.67, 76, 2.33),
      hvm_mnd3 = c(27.67, 70, 2.33),
      hvm_mnd4 = c(26.67, 71, 2.33),
      dcat = c("Stieren", "Koeien", "Vaarzen")
    ),
    df_gemg = data.frame(
      stringsAsFactors = FALSE,
      row.names = c("Stieren","Koeien","Vaarzen",
                    "Kalveren 0-8 mnd","Kalveren 8-12 mnd","Lammeren",
                    "Varkens","Volwassen schapen"),
      hvm_mnd1 = c(473, 327, 221, NA, 212, 21, 101, 30),
      hvm_mnd2 = c(426, 319, 237, NA, 200, 24, 97, 30),
      hvm_mnd3 = c(456, 322, 241, NA, 210, 24, 96, 30),
      hvm_mnd4 = c(456, 305, 233, NA, 217, 22, 104, 30),
      dcat = c("Stieren","Koeien","Vaarzen",
               "Kalveren 0-8 mnd","Kalveren 8-12 mnd","Lammeren",
               "Varkens","Volwassen schapen")
    )
  )
  
  # genereer de synthetische data em sla dit op
  wb <- opmaken_verd_gemg_excel(
    jaren = c("2021", "2022"),
    max_maand = 7,
    zwarte_balken = TRUE
  )
  tmp <- withr::local_tempfile(
    pattern = "Melding CBS per maand gem gewicht",
    fileext = ".xlsx"
  )
  openxlsx::saveWorkbook(wb, tmp, TRUE)
  
  # test of het gemaakte bestand kan worden ingelezen door inl_lees_input_verd_gemg
  jrmnd <- list(mnd1 = "202101", mnd2 = "202102", 
             mnd3 = "202103", mnd4 = "202104")
  res <- inl_lees_input_verd_gemg(tmp, jrmnd)
  
  expect_equal(res, verw)
  
})


test_that("het excel voor kalveren gewichten correct wordt aangemaakt", {
set.seed(0)

verw <- data.frame(
  stringsAsFactors = FALSE,
  row.names = c("Kalveren 0-8 mnd"),
  hvm_mnd1 = c(144),
  hvm_mnd2 = c(148),
  hvm_mnd3 = c(153),
  hvm_mnd4 = c(144),
  dcat = c("Kalveren 0-8 mnd")
)

# schrijf de synthetische data
wb_kalf <- opmaken_gemg_kalveren_excel(
  jaar = c(2021),
  max_maand = 4
)
tmp <- withr::local_tempfile(
  pattern = "Slachtgewichten SBK kalveren",
  fileext = ".xlsx"
)
openxlsx::saveWorkbook(wb_kalf, tmp, TRUE)

# test of het gemaakte bestand kan worden ingelezen door inl_lees_input_gemg_kalv
jrmnd <- list(mnd1 = "202101", mnd2 = "202102", 
              mnd3 = "202103", mnd4 = "202104")
res <- inl_lees_input_gemg_kalv(tmp, jrmnd = jrmnd)

expect_equal(res, verw)

})

