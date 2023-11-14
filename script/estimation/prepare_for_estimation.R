library(data.table)
source("script/helpers/helpers.R")

## read in ----
districts = fread_keepzeros(
  "data/processed/admin-areas/districts_inkar_ 31.12.2019.csv",
  drop = c("name", "state_name")
)

### main ----
main_wide = fread_utf8("data/processed/main/main_wide.csv", keepLeadingZeros = TRUE)

### add controls -----
#### physical constraints ----
area = fread_utf8("data/processed/main/territorial-area_qkm.csv", keepLeadingZeros = TRUE)
tri = fread_utf8(
  "data/processed/geodata/TRI_wilson_riley.csv",
  select = c("did", "mean_riley"), col.names = c("did", "tri"),
  keepLeadingZeros = TRUE
)

constraints = fread("data/processed/geodata/constraints.csv")
constraints[, did := appendLeadingZeros(did)]

# keep only those which I constructed with `ww` (wetlands and water bodies) masked
constraints[, (grep("_aww", names(constraints), value = TRUE)) := NULL]
setnames(
  constraints,
  grep("_ww", names(constraints), value = TRUE),
  sub("_ww", "", grep("_ww", names(constraints), value = TRUE))
)

constraints = constraints[,
  c(
    "did", "year", "area_avail", "area_gr_15", "area_masked",
    "slope_mean", "slope_range", "unavail_frac"
  ),
  with = FALSE
]

### land cover ----
clc_classes = fread(
  "data/raw/geodata/corine-land-cover/clc-classification.csv",
  select = c("clc_code", "classname", "classname2", "classname3")
)

frac_dev = fread_utf8(
  "data/processed/geodata/land-cover_area_by-clc-class1.csv",
  keepLeadingZeros = TRUE
) |>
  subset(code %/% 100 == 1L) # keep artificial surfaces class

frac_dev = frac_dev[
  clc_classes[, .(
    code = clc_code,
    label1 = classname3,
    label2 = classname2,
    label3 = classname
  )],
  on = "code", nomatch = NULL
]


# frac_dev1 = frac_dev[, .(area=sum(area)), .(did, year, label1)]
frac_dev2 = frac_dev[, .(area = sum(area)), .(did, year, label2)]
# frac_dev3 = frac_dev[, .(did, year, area, code, label3)]
frac_dev <-
  frac_dev[, .(artificial_surfaces = sum(area)), .(did, year)][frac_dev2[label2 == "urban fabric", .(did, year, urban_fabric = area)],
    on = c("did", "year")
  ]
## area I calculated from district shapes
# constraints[, .(did, year, area=area + fifelse(is.na(na), 0, na))]
## area from DESTATIS
frac_dev = area[, .(did, year, area = 1e6 * territorial_area_qkm)][frac_dev, on = c("did", "year")]
frac_dev[, `:=`(dev_frac = artificial_surfaces / area), .(did, year)]

# reshaping
constraints <- dcast(constraints,
  did ~ year,
  value.var = setdiff(names(constraints), c("did", "year"))
)
frac_dev <- dcast(
  frac_dev, did ~ year,
  value.var = setdiff(names(frac_dev), c("did", "year"))
)

area = area[
  year %in% c(2008, 2019),
  .(did, year, area = 1e6 * territorial_area_qkm)
] |>
  reshape(direction = "wide", idvar = "did", timevar = "year", sep = "_")


# construction costs 2019 --------------------------------------------------
ccosts = fread(
  "data/processed/main/statista_construction-costs-by-states_2019.csv",
  select = c("state_code", "cost_euro_sqm_2019")
)


# merge ----
mainDF = merge(main_wide, districts, "did") |>
  merge(area, "did") |>
  merge(constraints, "did") |>
  merge(frac_dev, "did") |>
  merge(tri, "did")

mainDF[, state_code := as.integer(substr(did, 1, 2))]
mainDF = mainDF[ccosts, on = "state_code"]

mainDF[, west_east := fifelse(west_east == "Ost", "East", west_east)]
mainDF[, urban_rural_area := fcase(
  urban_rural_area == "Städtischer Raum", "Urban",
  urban_rural_area == "Ländlicher Raum", "Rural"
)]

setcolorder(mainDF, unique(c("did", "name", names(districts))))
fwrite(mainDF, "data/processed/main/model-data.csv")
rm(main_wide, area, districts, frac_dev, frac_dev2, tri, clc_classes, constraints)
