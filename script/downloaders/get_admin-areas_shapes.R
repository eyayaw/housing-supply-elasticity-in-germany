library(data.table)
library(sf)

source("./script/helpers/base_helpers.R")
source("./script/helpers/helpers.R")

### This script ------------------------------------------------------------ ###
# 1) downloads shapes of Germany at all admin levels
# 2) cleans it and produce shape files for states, districts and municipalities
# 3) creates administrative levels:
# district and municipality names and codes for the entire analysis in the paper
### ------------------------------------------------------------------------ ###

# Downloading ------------------------------------------------------------------

## Territorial Codes References, Administrative areas -------
## Federal Agency for Cartography and Geodesy (gdz.BKG-bund.de)
### website: https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete.html?___store=default
### Administrative areas 1:250 000 (levels), as of December 31st (VG250 31.12.)
### It is based on the territorial definition of 2019 (end of the year).

furl = "https://daten.gdz.bkg.bund.de/produkte/vg/vg250_kompakt_1231/2019/vg250_12-31.utm32s.shape.kompakt.zip"

dname = "data/raw/admin-areas"
zfpath = sprintf("%s/%s", dname, basename(furl))

try(download_file(furl, zfpath))

# unzipping
unzip(zfpath, exdir = dname, overwrite = F)

fpath = sprintf(
  "%s/%s/dokumentation/struktur_und_attribute_vg250.xls", dname, file.stem(zfpath)
)
dname = sprintf("%s/%s", dname, file.stem(zfpath)) # the extracted folder

## States, Bundesländer ---------------------
# source:https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Glossar/bundeslaender.html

states = read.csv(
  textConnection('
state_code state_abb state
01 SH Schleswig-Holstein
02 HH Hamburg
03 NI Niedersachsen
04 HB Bremen
05 NW Nordrhein-Westfalen
06 HE Hessen
07 RP Rheinland-Pfalz
08 BW Baden-Württemberg
09 BY Bayern
10 SL Saarland
11 BE Berlin
12 BB Brandenburg
13 MV Mecklenburg-Vorpommern
14 SN Sachsen
15 ST Sachsen-Anhalt
16 TH Thüringen'
),
sep = " ", colClasses = rep("character", 3L)
)


# Cleaning ---------------------------------------------------------------

## administrative units ----
admin_areas = readxl::read_excel(fpath, sheet = "VG250")
ibz = readxl::read_excel(fpath, sheet = "VG250_IBZ") # attribute table
setDT(admin_areas)
setDT(ibz)

admin_areas = admin_areas[, c(
  "ADE", "ARS", "AGS", "GEN", "IBZ", "SN_L", "SN_R", "SN_K", "SN_V1", "SN_V2",
  "SN_G", "ARS_0", "AGS_0", "DEBKG_ID"
), with = FALSE]

setnames(
  admin_areas,
  c("SN_L", "SN_R", "SN_K", "SN_V1", "SN_V2", "SN_G"),
  c("state", "admin_district", "district", "admin_assoc_frontpart", "admin_assoc_rearpart", "municipality")
)
setnames(
  admin_areas, c("ADE", "GEN", "IBZ"),
  c("admin_level", "name", "admin_unit")
)

ibz = ibz[, .(admin_unit = IBZ, BEZ)]
ibz = unique(ibz, by = c("admin_unit", "BEZ"))

admin_areas = merge(admin_areas, ibz, "admin_unit")

## districts ----
districts = admin_areas[admin_level == 4L, !"admin_level"] # Kreis

## since we kept only ADE == 4 (i.e. districts), ARS should be the same as AGS
if (with(districts, all(AGS == ARS))) {
  districts[, `:=`(did = ARS, AGS = NULL, ARS = NULL)]
  setcolorder(districts, "did")
}

districts = districts[, .(did, name, state, admin_unit = BEZ)]


# merge with the states data.frame for state_abb
districts = districts[states[, c("state_code", "state_abb")], on = "state==state_code"][, !"state"]

setnames(districts, "state_abb", "state")
setcolorder(districts, "state", after = "name")

# municipalities
municipals = admin_areas[
  admin_level == 6L,
  .(ARS,
    mid = AGS, name,
    did = paste0(state, admin_district, district),
    admin_unit
  )
]

municipals = municipals[ibz, on = "admin_unit", nomatch = 0]
municipals[, admin_unit := NULL]
setnames(municipals, "BEZ", "admin_unit")

# merge with the districts data.frame for district id and name
municipals = municipals[districts[, .(did, district = name, state)], on = "did"]
setcolorder(municipals, c("district", "state"), after = "did")


## Shapes ---------------------------------------------------------------------
# The compact version comes redundancy-free i.e. the shapes are the lower admin
# levels i.e. municipalities. For districts and states, we need to aggregate
# the shapes to the respective levels.

shape_path = sprintf(
  "%s/VG250_F.shp", dir(dname, "vg250_kompakt_[0-9]{4}", full.names = T)
)
municipality_shape = st_read(shape_path)

## Filter by GF
# GF = Geofactor : Survey of values
# 1 = Waters without structures
# 2 = Waters with structures
# 3 = Land without structure
# 4 = Land with structure

municipality_shape = subset(municipality_shape, GF == 4L, select = c("ARS", "geometry"))
municipality_shape = merge(municipality_shape, municipals[, !"admin_unit"], "ARS")

## aggregate to the district and state levels
district_shape = aggregate(
  municipality_shape[, "geometry"],
  list(did = municipality_shape$did), length
) |>
  merge(districts[, .(did, name, state)], "did")

state_shape = aggregate(
  district_shape[, "geometry"],
  list(state = district_shape$state), length
)

# write to disk
dir_out = "data/processed/admin-areas"
dir.create(dir_out, recursive = T, showWarnings = F)
## admin area names, codes
fwrite(districts, file.path(dir_out, "districts_bkg.csv"))
fwrite(states, file.path(dir_out, "states.csv"))
fwrite(municipals, file.path(dir_out, "municipalities_bkg.csv"))

## admin area shapes
dir_out = "data/processed/admin-areas/admin-shapes/"
dir.create(dir_out, recursive = T, showWarnings = F)
st_write(
  municipality_shape, file.path(dir_out, "municipalities_shape.shp"),
  append = F
)
st_write(
  district_shape, file.path(dir_out, "districts_shape.shp"),
  append = F
)
st_write(
  state_shape, file.path(dir_out, "states_shape.shp"),
  append = F
)
