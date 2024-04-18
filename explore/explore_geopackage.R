library(dplyr)
library(sf)
library(purrr)
library(readxl)
library(ggplot2)
library(thlGraphs)

# Kiinteistörajat
# https://asiointi.maanmittauslaitos.fi/karttapaikka/tiedostopalvelu/kiinteistorekisterikartta_vektori

file <- here::here("data/kiinteistorekisterikartta.gpkg")
st_layers(file)

query <- "SELECT * from PalstanSijaintitiedot WHERE kiinteistotunnuksenEsitysmuoto = '931-408-8-26'"

kiinteistorajat <- st_read(file,
                           layer = "PalstanSijaintitiedot",
                           query = query)


# Hila-aineistot
# https://www.metsakeskus.fi/fi/avoin-metsa-ja-luontotieto/aineistot-paikkatieto-ohjelmille/paikkatietoaineistot

# meta
meta_xlsx_url <- "https://www.metsakeskus.fi/sites/default/files/document/hila-koodisto-ja-tietokantakuvaus.xlsx"

download.file(meta_xlsx_url, destfile = here::here("data/meta.xlsx"), mode = "wb")

meta_attributes <- read_xlsx(here::here("data/meta.xlsx"), sheet = "Tietokantakuvaus", skip = 2,
                             col_names = c("table_name", "id", "attribute", "description", "type", "code_set", "note"))

meta_codes <- read_xlsx(here::here("data/meta.xlsx"), sheet = "Koodisto", skip = 2,
                        col_names = c("code_set", "attribute", "code", "label_fi", "label_sv")) %>%
  mutate(code = as.numeric(code))

# data
hila_files <- here::here(glue::glue("data/Hila_P4413{LETTERS[1:8]}.gpkg"))

hila_list <- purrr::map(hila_files, function(file) {st_read(file, layer = "gridcell")}, .progress = TRUE)

hila <- bind_rows(hila_list) %>%
  select(-starts_with("sample")) %>%
  mutate(across(c(maingroup, subgroup, fertilityclass, soiltype, drainagestate, harvestaccessibility, growthplacedatasource, developmentclass, maintreespecies), as.character))

hila_vuorisalo <- st_join(hila, kiinteistorajat, largest = TRUE, left = FALSE)

# explore

# maingroup
hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "maingroup"), by = c("maingroup" = "code"))

maingroup_palette <- c("#CA369C", "#31AA35", "#005A1E")

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = label_fi), color = NA) +
  scale_fill_manual(values = maingroup_palette)


# subgroup
hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "subgroup"), by = c("subgroup" = "code"))

subgroup_palette <- c("#005A1E", "#31AA35", "#0096FA", "#0B57CE", "#31AA35")

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = label_fi), color = NA) +
  scale_fill_manual(values = subgroup_palette)

# fertilityclass

hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "fertilityclass"), by = c("fertilityclass" = "code"))

fertilityclass_palette <- c("#949494", "#9C9288", "#005A1E", "#007A1E", "#318A35", "#319A35")

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = label_fi), color = NA) +
  scale_fill_manual(values = fertilityclass_palette)

