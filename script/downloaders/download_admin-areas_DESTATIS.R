source("script/helpers/base_helpers.R")

# Gemeinden in Deutschland nach Fläche, Bevölkerung und Postleitzahl am 31.12.2019 (Jahr) ----

durl = "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GVAuszugJ/31122019_Auszug_GV.xlsx?__blob=publicationFile"

fname = sub("[?].+", "", basename(durl))
fpath = sprintf("data/raw/admin-areas/%s", fname)

try(download_file(durl, fpath))

## Taken from the header notes of the excel file
## Satzart -----------

record_type = tibble::tribble(
  ~code,                 ~text,              ~text_en,
    10L,                "Land",              "country",
    20L,    "Regierungsbezirk",       "admin district",
    30L,  "Region (nur in BW)",  "region (only in BW)",
    40L,               "Kreis",             "district",
    50L,     "Gemeindeverband",    "admin association",
    60L,            "Gemeinde",         "municipality"
  )


## Textkennzeichen -----

text_mark = tibble::tribble(
~code,                             ~text,                            ~text_en,
  41L,                "Kreisfreie Stadt",                "district-free city",
  42L,          "Stadtkreis (nur in BW)",        "dity district (only in BW)",
  43L,                           "Kreis",                          "district",
  44L,                       "Landkreis",                    "rural district",
  45L,     "Regionalverband (nur im SL)", "regional association (only in SL)",
  50L,          "Verbandsfreie Gemeinde",        "association-free community",
  51L,                             "Amt",                 "government office",
  52L,                    "Samtgemeinde",                "joint municipality",
  53L,                "Verbandsgemeinde",                  "Verbandsgemeinde",
  54L,         "Verwaltungsgemeinschaft",          "administrative community",
  55L,         "Kirchspielslandgemeinde",                "parish land parish",
  56L,              "Verwaltungsverband",        "administrative association",
  58L,             "Erfüllende Gemeinde",                 "Fulfilling Church",
  60L,                           "Markt",                            "market",
  61L,                "Kreisfreie Stadt",                "district-free city",
  62L,          "Stadtkreis (nur in BW)",        "city district (only in BW)",
  63L,                           "Stadt",                              "city",
  64L,        "Kreisangehörige Gemeinde",             "district municipality",
  65L,   "gemeindefreies Gebiet-bewohnt",  "municipality-free area-inhabited",
  66L, "gemeindefreies Gebiet-unbewohnt","municipality-free uninhabited area",
  67L,                "Große Kreisstadt",               "large city-district"
)


## clean --------
admin_areas = readxl::read_excel(fpath,
  sheet = "Onlineprodukt_Gemeinden_311219", col_names = F, skip = 6
)

header = c(
  "Satz-art", "Text-kenn-zeichen",
  "Land", "Regierungsbezirk (RB)", "Kreis", "Gemeindeverband (VB)",
  "Gemeinde (Gem)", # Amtlicher Regionalschlüssel (ARS)
  "Gemeindename", "Fläche km2",
  paste("Bevölkerung", c("insgesamt", "männlich", "weiblich", "je km2")),
  "Post-leit-zahl", "Längengrad", "Breitengrad", # Geografische Mittelpunktkoordinaten
  paste0("Reisegebiete ", c("Schlüssel", "Bezeichnung")), # Reisegebiete
  paste0("Grad der Verstädterung", c("Schlüssel", "Bezeichnung")) # Grad der Verstädterung
)

header_en = c(
  "Record-type", "Text mark",
  "Land", "Administrative Region", "District",
  "Adminstrative Association",
  "Municipality", # Official regional key (ARS)
  "Parish name", "Area km2",
  paste0("Population_", c("total", "male", "female", "per km2")),
  "Postal code", "Longitude", "Latitude", # Geographic center point coordinates
  paste0("travel areas ", c("Key", "Description")), # travel areas
  paste0("urban level ", c("Key", "Description")) # Degree of urbanization
)
names(admin_areas) = gsub("[ -]{1,}", "_", tolower(header_en))


# subset municipalities (Gemiende)
municipals = subset(admin_areas, `record_type` == 60L, select = -1L)

# create AGS Official municipality key which we call `mid` municipality id and
# `did` district id. AGS = ARS without the key number of the administrative association
municipals = within(municipals,{
  did = paste0(land, administrative_region, district)
  mid = paste0(did, municipality)
  longitude = as.numeric(gsub(",", ".", longitude))
  latitude = as.numeric(gsub(',', '.', latitude))
  name = parish_name
  parish_name = NULL
  state_code = land
  land = NULL
  })

municipals$municipality_type =
  with(text_mark,
       paste0(text[match(municipals$text_mark, code)], ' (', municipals$text_mark, ')'))

municipals = municipals[, c(
  "mid", "name", "did", "state_code", "municipality_type",
  "area_km2", "population_total", "postal_code", "longitude", "latitude",
  "travel_areas_key", "travel_areas_description",
  "urban_level_key", "urban_level_description"
)]

# subset districts (Kreise)
districts = subset(admin_areas, `record_type` == 40L, select = -1)
districts = transform(districts,
  did = paste0(land, administrative_region, district),
  name = parish_name,
  state_code = land,
  parish_name = NULL,
  land = NULL
)

districts$district_type = with(text_mark,
  paste0(text[match(districts$text_mark, code)], " (", districts$text_mark, ")")
)

districts = districts[, c("did", "name", "state_code", "district_type")]

states = read.csv("./data/processed/admin-areas/states.csv", colClasses = rep("character", 3L))

districts = merge(districts, states, "state_code", all.x = TRUE)
districts = districts[, c("did", "name", "district_type", "state_code", "state_abb", "state_name")]

data.table::fwrite(municipals, "data/processed/admin-areas/municipalities_destatis.csv")
data.table::fwrite(districts, "data/processed/admin-areas/districts_destasis.csv")


# district/city municipalities i.e. districts that have 1 municipality
# which means they're themselves the municipalities

dmunicipals = data.frame(table(municipals$did)) |>
  setNames(c("did", "N")) |>
  subset(N == 1L, select = "did") |>
  merge(municipals[, c("did", "mid", "name", "municipality_type")], "did")

rm(record_type, text_mark, admin_areas, header, header_en)



# notes ------------------------------------------------------------------------

## Satzart: -----------
# 10=Land
# 20=Regierungsbezirk
# 30=Region (nur in Baden-Württemberg)
# 40=Kreis
# 50=Gemeindeverband
# Erläuterung der Kürzel:
#   Land 08 Baden-Württemberg: VVG=Vereinbarte Verwaltungsgemeinschaft
#                              GVV=Gemeinde Verwaltungsverband
#   Land 09 Bayern: Vgem=Verwaltungsgemeinschaft
# 60=Gemeinde

## Textkennzeichen: ----
# 41=Kreisfreie Stadt
# 42=Stadtkreis (nur in Baden-Württemberg)
# 43=Kreis
# 44=Landkreis
# 45=Regionalverband (nur im Saarland)
# 50=Verbandsfreie Gemeinde
# 51=Amt
# 52=Samtgemeinde
# 53=Verbandsgemeinde
# 54=Verwaltungsgemeinschaft
# 55=Kirchspielslandgemeinde
# 56=Verwaltungsverband
# 58=Erfüllende Gemeinde
# 60=Markt
# 61=Kreisfreie Stadt
# 62=Stadtkreis (nur in Baden-Württemberg)
# 63=Stadt
# 64=Kreisangehörige Gemeinde
# 65=gemeindefreies Gebiet-bewohnt
# 66=gemeindefreies Gebiet-unbewohnt
# 67=Große Kreisstadt

## Land: ----
# 01 Schleswig-Holstein
# 02 Hamburg
# 03 Niedersachsen
# 04 Bremen
# 05 Nordrhein-Westfalen
# 06 Hessen
# 07 Rheinland-Pfalz
# 08 Baden-Württemberg
# 09 Bayern
# 10 Saarland
# 11 Berlin
# 12 Brandenburg
# 13 Mecklenburg-Vorpommern
# 14 Sachsen
# 15 Sachsen-Anhalt
# 16 Thüringen

## -----
# RB:
#   Regierungsbezirk
# VB:
#   Gemeindeverband
# Gem:
#   Gemeinde


## Fläche in km2: -----
# Fläche im Land Rheinland-Pfalz: Einschließlich des Gebietes "Gemeinsames deutsch-luxemburgisches Hoheitsgebiet" von 6,20 km2.
#
# Abweichungen bei den Flächenangaben sind durch Runden der Zahlen möglich.

# ABWEICHUNGEN
# sind bei den Gemeinden möglich, deren Fläche und/oder Bevölkerung im Zuge einer Gebietsänderung verändert wurde(n).


## Postleitzahl (PLZ): ----
# Die Gemeinde kann mehrere Postleitzahlen haben;
# im Gemeindeverzeichnis wird nur die PLZ des Verwaltungssitzes der Gemeinde geführt.

## Quelle: ----
# Bundesamt für Kartographie und Geodäsie (BKG), Frankfurt am Main.
# - World Geodetic System 1984 (WGS 84) -

## Reisegebiete: ----
# - Hamburg
# - Bremen
# - Bremerhaven
# - Berlin
# haben keine Zuordnung zu Reisegebieten

## Quelle: ----
# Eurostat
# - The New Degree of Urbanisation (DEGURBA) - (Neuklassifizierung ab 31.12.2011)
# LINK:
# http://ec.europa.eu/eurostat/ramon/miscellaneous/index.cfm?TargetUrl=DSP_DEGURBA

