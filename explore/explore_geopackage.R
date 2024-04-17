library(dplyr)
library(sf)
library(purrr)
library(readxl)

# Kiinteistörajat
# https://asiointi.maanmittauslaitos.fi/karttapaikka/tiedostopalvelu/kiinteistorekisterikartta_vektori

file <- here::here("data/kiinteistorekisterikartta.gpkg")
st_layers(file)

query <- "SELECT * from PalstanSijaintitiedot WHERE kiinteistotunnuksenEsitysmuoto = '931-408-8-26'"

kiinteistorajat <- st_read(file,
                           layer = "PalstanSijaintitiedot",
                           query = query)

plot(kiinteistorajat)


# Hila-aineistot
# https://www.metsakeskus.fi/fi/avoin-metsa-ja-luontotieto/aineistot-paikkatieto-ohjelmille/paikkatietoaineistot

# meta
meta_xlsx_url <- "https://www.metsakeskus.fi/sites/default/files/document/hila-koodisto-ja-tietokantakuvaus.xlsx"

download.file(meta_xlsx_url, destfile = here::here("data/meta.xlsx"), mode = "wb")

meta_attributes <- read_xlsx(here::here("data/meta.xlsx"), sheet = "Tietokantakuvaus", skip = 2,
                             col_names = c("table_name", "id", "attribute", "description", "type", "code_set", "note"))

meta_codes <- read_xlsx(here::here("data/meta.xlsx"), sheet = "Koodisto", skip = 2,
                        col_names = c("code_set", "attribute", "code", "label_fi", "label_sv"))

# data
hila_files <- here::here(glue::glue("data/Hila_P4413{LETTERS[1:8]}.gpkg"))

hila_list <- purrr::map(hila_files, function(file) {st_read(file, layer = "gridcell")}, .progress = TRUE)

hila <- bind_rows(hila_list)

hila_vuorisalo <- st_join(hila, kiinteistorajat, largest = TRUE, left = FALSE)

plot(hila_vuorisalo["volume"])
plot(hila_vuorisalo["meanheight"])
