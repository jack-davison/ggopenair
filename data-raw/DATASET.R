## code to prepare `marylebone` dataset goes here

marylebone <-
  openair::importAURN(site = "my1",
                      year = 2015:2020) |>
  dplyr::select(date, ws, wd, air_temp,
                nox, no2, o3, pm2.5, pm10)

usethis::use_data(marylebone, overwrite = TRUE)
