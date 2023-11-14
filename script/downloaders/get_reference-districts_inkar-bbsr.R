source("script/helpers/base_helpers.R")
source("script/helpers/helpers.R")

# get the reference file for 31.12.2019
# checked on Nov 2, 2023, they following url takes to 31.12.2020
# you can find the archived version here: https://web.archive.org/web/20220414081208/https://www.inkar.de/documents/Referenz%20Gemeinden,%20Kreise,%20NUTS.xlsx
furl = "https://www.inkar.de/documents/Referenz%20Gemeinden,%20Kreise,%20NUTS.xlsx"
stem = file.stem(gsub(",?%20", " ", basename(furl)))
fpath = file.path("data/raw/INKAR", paste0(stem, "_ 31.12.2019.xlsx"))
download_file(furl, ensure_dir(fpath))

reference = readxl::read_excel(fpath, "Kreise", skip=1L)

reference = reference[, c(
  "Kreise Kennziffer", "Kreise Name", "Bundesländer Kennziffer", "Bundesländer Name",
  "Arbeitsmarktregionen Kennziffer", "Arbeitsmarktregionen Name", "West-Ost Kennziffer",
  "West-Ost Name", "Raumtyp Bezug Lage (Kreise) Kennziffer",
  "Raumtyp Bezug Lage (Kreise) Name", "Städtischer-Ländlicher Raum Kennziffer",
  "Städtischer-Ländlicher Raum Name", "Städtischer-Ländlicher Raum (West-Ost) Kennziffer",
  "Städtischer-Ländlicher Raum (West-Ost) Name", "Wachsend-Schrumpfend (Kreise) Kennziffer",
  "Wachsend-Schrumpfend (Kreise) Name"
)]

names(reference) = make_names(c(
  "did", "name", "state code", "state name",
  "Labour Market Region Code", "Labour Market Region Name", "West-East Code",
  "West-East Name", "Room Type Reference Location (districts) Code",
  "Area type reference location (districts) Name", "urban-rural area code",
  "urban-rural area name", "urban-rural area (west-east) code",
  "urban-rural area (West-East) Name", "Growing-Shrinking (districts) Code",
  "Growing-Shrinking (districts) Name"
))

ids = c("did", "name", "state_code", "state_name")
nms = c(ids, grepv("name", setdiff(names(reference), ids)))
reference = reference[, nms]
names(reference)[!(nms %in% ids)] = gsub("_name", "", setdiff(names(reference), ids))

reference = within(reference, {
  did=did %/% 1000L
  did=appendLeadingZeros(did)
})


data.table::fwrite(reference, "data/processed/admin-areas/districts_inkar_ 31.12.2019.csv")
