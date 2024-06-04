library(tidyverse)
library(sf)
library(mapview)
library(aspe)
library(tod)

# ======================================================================================================
# Sélection des points pops qui nous intéressent
# ======================================================================================================
# il s"'agit de la région + des BV partiellement sur la région
# on utilise donc deux sélections qui sont 
# - les pts dans l'emprise de la région adminbistrative (4 depts + un buffer), couche data.gouv.fr
# - les pts dans les SAGEs qui mordent sur la région d'après la couche de la Dreal accessible par flux

# Géométrie de la région
#---------------------------
# téléchargement du shapefile des contours des départements
url <- "https://www.data.gouv.fr/fr/datasets/r/3096e551-c68d-40ce-8972-a228c94c0ad1"
depts <- osm_depts_tod(url = url, repertoire = "raw_data", crs_sortie = 2154)

# Création du polygone (classe sf) de la région
region_bzh <- osm_creer_polygone_region(departements_sf = depts,
                                        departements_selectionnes = c("22", "29", "35", "56"),
                                        distance_buffer = 0.01,
                                        intitule_region = "Bretagne") %>% 
  `st_crs<-`(2154)

mapview(region_bzh)

# Géométrie des SAGEs
#---------------------------
wms <- "https://geobretagne.fr/geoserver/dreal_b/sage_dreal/wms?SERVICE=WMS&REQUEST=GetCapabilities" #image
wfs <- "https://geobretagne.fr/geoserver/dreal_b/sage_dreal/wfs?SERVICE=WFS&REQUEST=GetCapabilities" #data

# m <- mapview(region_bzh)
# m@map <- m@map %>%
#   addWMSTiles(group = "sage_dreal",
#               baseUrl = wms,
#               layers = "sage_dreal")                                                                               )
# 
# m

sages <- st_read(wfs)

# pb pas moyen de manipuler cet objet => solution sur
# https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12/389854#389854
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  gdalUtilities::ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

sages <- ensure_multipolygons(sages)

mapview::mapview(sages)

# Sélection des identifiants
#---------------------------
data <- aspe::misc_nom_dernier_fichier(repertoire = "../../../../ASPE/raw_data/rdata",
                                       pattern = "^tables")
load(file = data)

pops <- point_prelevement %>%
  left_join(y = ref_type_projection,
            by = c("pop_typ_id" = "typ_id"))

# coordonnées
coords <- aspe::geo_convertir_coords_df(
  df = pops,
  var_x = pop_coordonnees_x,
  var_y = pop_coordonnees_y,
  var_crs_initial = typ_code_epsg,
  crs_sortie = 2154 # Lambert 93
)

# création d'un objet géographique
pops_geo <- pops %>%
  cbind(coords) %>%
  sf::st_as_sf(coords = c("X", "Y"),
               crs = 2154)

mapview::mapview(pops_geo)

# 
# pops <- point_prelevement %>% 
#   geo_ajouter_coords_pop() %>% 
#   geo_convertir_coords_df(var_x = "pop_coordonnees_x",
#                           var_y = "pop_coordonnees_y",
#                           var_id = "pop_id",
#                           var_crs_initial = "typ_code_epsg",
#                           crs_sortie = 2154) %>% 
#   st_as_sf(coords = c("X", "Y"),
#            crs = 2154)

pops_bzh <- pops_geo %>% 
  st_join(region_bzh) %>% 
  filter(!is.na(region))

mapview::mapview(pops_bzh)

pops_sages <- pops_bzh %>% 
  st_join(sages) %>% 
  filter(!is.na(cd_sage))

pops_id <- c(pops_bzh$pop_id, pops_sages$pop_id) %>% 
  unique()

# ======================================================================================================
# Mise en forme
# ======================================================================================================
passerelle <- mef_creer_passerelle() %>% 
  filter(pop_id %in% pops_id) %>% 
  mef_ajouter_ope_date() %>% 
  mef_ajouter_libelle() %>% 
  mef_ajouter_lots() %>% 
 # mef_ajouter_esp_code_alternatif() %>% 
  left_join(y = data_passerelle_taxo %>% 
              select(esp_code_alternatif,
                     esp_nom_latin,
                     esp_code_taxref) %>% 
              # doublons
              unique() %>% 
              filter(!is.na(esp_code_taxref)) %>% 
              group_by(esp_code_alternatif) %>% 
              # si plusieurs codes taxref pour un même code aspe, on retient le plus récent (= à jour)
              filter(esp_code_taxref == max(esp_code_taxref, na.rm = TRUE))) 

# table peuplement
# --------------------------
passerelle <- passerelle %>% 
  left_join(station %>% 
              select(sta_id, sta_code_sandre)) %>% 
  left_join(point_prelevement %>% 
              select(pop_id, pop_code_sandre)) %>% 
  mutate(Code_station = ifelse(!is.na(sta_code_sandre), sta_code_sandre, pop_code_sandre)) # création d'un code point
 
peuplement_bzh <- passerelle %>% 
 select(Annee = annee,
         Num_operation = ope_id,
         date_peche = ope_date,
         Code_station,
         Code_espece_onema = esp_code_alternatif,
         Code_INPN = esp_code_taxref,
         Effectif_peche = lop_effectif) %>% 
  group_by(across(c(-Effectif_peche))) %>% 
    summarise(Effectif_peche = sum(Effectif_peche)) %>% 
  ungroup() %>% 
  distinct()


# table operation
# --------------------------
# operation_bzh <- passerelle %>% 
#   mef_ajouter_moyen_prospection() %>% 
#   mef_ajouter_surf_calc() %>% 
#   mef_ajouter_passage() %>% 
#   mef_ajouter_type_protocole() %>% 
#   right_join(y = pops_geo %>% 
#               select(pop_id,
#                      pop_coordonnees_x,
#                      pop_coordonnees_y,
#                      typ_code_epsg) %>% 
#               distinct())






operation_bzh <- passerelle %>% 
  mef_ajouter_moyen_prospection() %>% 
  mef_ajouter_surf_calc() %>% 
  mef_ajouter_passage() %>% 
  mef_ajouter_type_protocole() %>% 
  left_join(y = point_prelevement %>% 
              select(pop_id,
                     pop_coordonnees_x,
                     pop_coordonnees_y,
                     pop_typ_id)) %>% 
  # geo_ajouter_coords_pop()
  left_join(y = ref_type_projection,
            by = c("pop_typ_id" = "typ_id"))

# passage des coordonnées en WGS84
coords <- operation_bzh %>% 
  select(pop_id,
         pop_coordonnees_x,
         pop_coordonnees_y,
         typ_code_epsg) %>% 
  distinct() %>% 
  geo_convertir_coords_df(var_crs_initial = typ_code_epsg,
                          var_id = pop_id,
                          var_x = pop_coordonnees_x,
                          var_y = pop_coordonnees_y)

# assemblage
operation_bzh <- operation_bzh %>% 
  left_join(coords) %>% 
  mutate(Cd_Rdd = NA,
         Lb_Rdd = NA) %>% 
  group_by(across(-pas_numero)) %>% 
    summarise(Nombre_passage = max(pas_numero)) %>% 
  ungroup() %>% 
  select(Annee = annee,
         Num_operation = ope_id,
         date_peche = ope_date,
         Code_station,
         Lb_station = pop_libelle,
         Coord_X = X,
         Coord_Y = Y,
         Cd_Rdd,
         Lb_Rdd,
         Methode_prospection = pro_libelle,
         Mode_prospection = mop_libelle,
         Nombre_passage,
         Surface_prospectee = ope_surface_calculee) %>% 
  distinct()


# table IPR
# --------------------------

ipr_bzh <- passerelle %>%
  select(annee, Code_station, ope_id) %>% 
  distinct() %>% 
  mef_ajouter_ipr() %>% 
  mef_ajouter_metriques() %>% 
  filter(!is.na(ipr)) %>% 
  select(-(cli_id:dti_observe)) %>% 
  pivot_longer(cols = ner:dti,
               names_to = "type_metrique",
               values_to = "valeur_metrique")


ipr_bzh <- ipr_bzh %>% 
  mutate(classe_metrique = cut(valeur_metrique,
                               breaks = c(-99, 7, 16, 25, 36, 1e6) / 7,
                               labels = c("Excellent", "Bon", "Médiocre", "Mauvais", "Très mauvais")))



write.csv2(peuplement_bzh, file = "processed_data/peuplement.csv")
write.csv2(operation_bzh, file = "processed_data/operation.csv")
write.csv2(ipr_bzh, file = "processed_data/ipr.csv")

# export en Excel si ça a un intérêt
ma_liste <- list("peuplement" = peuplement_bzh,
                 "operation" = operation_bzh,
                 "ipr" = ipr_bzh)

openxlsx::write.xlsx(ma_liste, file = "processed_data/aspe_202_06.xlsx",
                     overwrite = T)




