library(dplyr)
library(sf)
library(purrr)
library(readxl)
library(ggplot2)
library(viridis)

# Kiinteistörajat
# https://asiointi.maanmittauslaitos.fi/karttapaikka/tiedostopalvelu/kiinteistorekisterikartta_vektori

file <- here::here("data/kiinteistorekisterikartta.gpkg")
st_layers(file)

query <- "SELECT * from PalstanSijaintitiedot WHERE kiinteistotunnuksenEsitysmuoto = '931-408-8-26'"

kiinteistorajat <- st_read(file,
                           layer = "PalstanSijaintitiedot",
                           query = query)
# Maastotietokanta

file <- here::here("data/maastotietokanta_kaikki.gpkg")
st_layers(file)

tieviiva <- st_read(file,
                    layer = "tieviiva")
# Maastotietokanta

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

hila_list <- hila_files %>%
  purrr::set_names() %>%
  purrr::map(function(file) {
    st_read(file, layer = "gridcell", quiet = TRUE)
    }, .progress = interactive())

hila <- bind_rows(hila_list, .id = "file") %>%
  select(-starts_with("sample")) %>%
  mutate(across(c(maingroup, subgroup, fertilityclass, soiltype, drainagestate, harvestaccessibility, growthplacedatasource, developmentclass, maintreespecies), as.character))

hila_vuorisalo <- st_join(hila, kiinteistorajat, largest = TRUE, left = FALSE)
hila_vuorisalo <- st_join(hila, kiinteistorajat, largest = TRUE, left = FALSE)

# explore

thl_test_palette <- c("#31AA35", "#005A1E", "#0B57CE", "#CA369C", "#0096FA", "#FFA03C", "#ED8CAE")

# maingroup
hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "maingroup"), by = c("maingroup" = "code"))

palette <- c("#CA369C", "#31AA35", "#005A1E")

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = label_fi), color = NA) +
  scale_fill_manual(values = palette)


# subgroup
hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "subgroup"), by = c("subgroup" = "code"))

palette <- c("#005A1E", "#31AA35", "#0096FA", "#0B57CE", "#31AA35")

ggplot() +
  geom_sf(data = hila_vuorisalo.rep, aes(fill = label_fi), color = NA) +
  geom_sf(data = tieviiva) +
  scale_fill_manual(values = palette)

# fertilityclass

hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "fertilityclass"), by = c("fertilityclass" = "code"))

palette <- c("#949494", "#9C9288", "#005A1E", "#007A1E", "#318A35", "#319A35")

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = label_fi), color = NA) +
  scale_fill_manual(values = palette)


# soiltype

hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "soiltype"), by = c("soiltype" = "code"))

palette <- c("#c4cc93", "#979c93", "#647a42", "#5a5a5a", "#a2b362", "#625b2f")

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = label_fi), color = NA) +
  scale_fill_manual(values = palette)

# drainagestate         

hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "drainagestate"), by = c("drainagestate" = "code"))

palette <- c("#0096FA", "#647a42", "#649a42", "#31AA35", "#005A1E", "#0B57CE", "#625b2f")

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = label_fi), color = NA) +
  scale_fill_manual(values = palette)

# ditchingyear          

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = ditchingyear), color = NA) +
  scale_fill_manual(values = thl_test_palette)

# harvestaccessibility

hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "harvestaccessibility"), by = c("harvestaccessibility" = "code")) %>%
  mutate(label_fi = factor(label_fi, labels = c("Talvi","Kelirikko", "Normaali kesä, turvemaa", "Normaali kesä, kivennäismaa", "Kuiva kesä, turvemaa", "Kuiva kesä, kivennäismaa")))

palette <- c("#798234","#a3ad62","#d0d3a2","#fdfbe4","#f0c6c3","#df91a3","#d46780")


ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = label_fi), color = NA) +
  scale_fill_manual(values = palette)

# growthplacedatasource 

hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "growthplacedatasource"), by = c("growthplacedatasource" = "code"))

palette <- c("#798234","#a3ad62","#d0d3a2","#fdfbe4","#f0c6c3","#df91a3","#d46780")


ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = label_fi), color = NA) +
  scale_fill_manual(values = palette)

# growthplacedate       

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = growthplacedate), color = NA) +
  scale_fill_viridis()

# developmentclass     

hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "developmentclass"), by = c("developmentclass" = "code"))

palette <- rev(c("#3d5941","#778868","#b5b991","#f6edbd","#edbb8a","#de8a5a","#ca562c"))

gplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = label_fi), color = NA) +
  scale_fill_manual(values = palette)

# maintreespecies       

hila_vuorisalo.rep <- hila_vuorisalo %>%
  left_join(filter(meta_codes, attribute == "growthplacedatasource"), by = c("growthplacedatasource" = "code"))

palette <- c("#798234","#a3ad62","#d0d3a2","#fdfbe4","#f0c6c3","#df91a3","#d46780")

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = label_fi), color = NA) +
  scale_fill_manual(values = palette)

# laserheight           

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = laserheight), color = NA) +
  scale_fill_viridis()

# laserdensity          

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = laserdensity), color = NA) +
  scale_fill_viridis()

# treedatadate         

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = treedatadate), color = NA) +
  scale_fill_viridis()

# agepine

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = agepine), color = NA) +
  scale_fill_viridis(direction = -1)

# basalareapine         

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = basalareapine), color = NA) +
  scale_fill_viridis(direction = -1)

# stemcountpine         

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = stemcountpine), color = NA) +
  scale_fill_viridis(direction = -1)

# meandiameterpine     

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = meandiameterpine), color = NA) +
  scale_fill_viridis(direction = -1)

# meanheightpine        

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = meanheightpine), color = NA) +
  scale_fill_viridis(direction = 1)

# volumepine            

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = volumepine), color = NA) +
  scale_fill_viridis(direction = -1)

# agespruce             

# basalareaspruce      

# stemcountspruce       

# meandiameterspruce    

# meanheightspruce      

# volumespruce         

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = volumespruce), color = NA) +
  scale_fill_viridis(direction = -1)

# agedeciduous          

# basalareadeciduous    

# stemcountdeciduous    

# meandiameterdeciduous

# meanheightdeciduous   

# volumedeciduous

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = volumedeciduous), color = NA) +
  scale_fill_viridis(direction = -1)

# age                   

# basalarea            

# stemcount             

# meandiameter          

# meanheight            

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = meanheight), color = NA) +
  scale_fill_viridis(direction = 1)

# dominantheight       

# volume                

hila_vuorisalo.rep <- hila_vuorisalo

ggplot(hila_vuorisalo.rep) +
  geom_sf(aes(fill = volume), color = NA) +
  scale_fill_viridis(direction = -1)

# creationtime          

# updatetime            

# geometry 

