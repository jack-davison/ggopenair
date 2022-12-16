## code to prepare `world` dataset goes here

world <- rnaturalearth::ne_countries(returnclass = "sf",scale = "medium")

usethis::use_data(world, overwrite = TRUE, internal = TRUE)
