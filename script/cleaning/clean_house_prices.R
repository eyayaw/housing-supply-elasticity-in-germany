library(data.table)
library(sf)
source("script/helpers/helpers.R")
source("script/cleaning/variable_list.R")
# source("script/cleaning/variable_and_value_labels.R") # not used, look at around L175 for heating_type labs

# cleaning ---------------------------------------------------------------------
homes = fread(
  "data/raw/RWI-GEO-RED/HK_SUF_ohneText_Combined_Main_Vars.csv",
  colClasses = list(character = "kid2019")
)

# rename all vars into English
old_names = copy(names(homes)) # names in German
new_names = main_vars$var_en[match(old_names, main_vars$var_de)] # new names in English
setnames(homes, old_names, new_names)

# write homes with English column names to disk
fwrite(homes, "data/raw/RWI-GEO-RED/HK_SUF_ohneText_Combined_Main_Vars_en.csv")

setnames(homes, "unique_id", "unique_obid")
homes[, obid := NULL]
# reorder variables
order_of_vars = c(
  "unique_obid", "kid2019", "state", "year", "ad_begin_year", "ad_begin_mon",
  "ad_end_mon", "price", "floor_space", "usable_floor_space", "plot_size",
  "num_rooms", "num_bedrooms", "num_bathrooms", "num_floors", "house_type",
  "protected_building", "holiday_house", "heating_type", "basement",
  "guest_washroom", "constr_year", "renov_year", "constr_phase",
  "equipment", "condition", "parking_space", "parking_space_price",
  "grid_id", "lab_mrkt_reg", "kid2015"
)
setcolorder(homes, c(order_of_vars, setdiff(names(homes), order_of_vars)))

setnames(homes, 'kid2019', 'did') # rename kid2019 to `did` (district id)

## dealing with missing values of many forms -----------------------------------

### drop observations with missing district identifier ----
count_missing = list(did = homes[did < 0, .N])
homes = homes[did > 0, ]

### drop obs with missing 1km grid identifier, i.e., that are annonymized
count_missing$grid_id = homes[grid_id<0, ]
homes = homes[grid_id > 0, ]

### drop rows if price is missing ----
count_missing$price = homes[price <= 0, .N]
homes = homes[price > 0, ]

### drop if missing floor space ----
count_missing$floor_space = homes[floor_space <= 0, .N]
homes = homes[floor_space > 0, ]

### plot area ----
count_missing$plot_size = homes[plot_size <= 0, .N]
homes = homes[plot_size > 0, ]

### number of rooms -----
count_missing$num_rooms = homes[num_rooms < 0, .N]
homes = homes[num_rooms > 0, ]

### house type ----
count_missing$house_type = homes[house_type < 0, .N]
homes[, house_type := fcase(
  house_type == -7 | house_type == -9, 0L,
  house_type == 1 | house_type == 2, 1L,
  house_type == 11 | house_type == 12, 2L,
  house_type == 3L, 3L, # semi-detached
  between(house_type, 4, 6), 4L,
  house_type == 13 | house_type == 15, 5L,
  between(house_type, 7, 10) | house_type == 14, 6L
)]
homes[, house_type := factor(
  house_type, 0L:6L,
  c(
    "na",            # -9 (Sonstiges Missing) + -7 (Keine Angabe)
    "single-family", #  1 Single-family house (detached) + 2 Single-family house
    "two-family",    # 11 two-family houses + 12 block of flats
    "semi-detached", # itself, 3 semi-detached
    "terraced",      # 4 terraced + 5 terraced (middle unit) + 6 terraced (end unit)
    "other",         # 13 other property for living + 15 other
    "special"        # 7 Bungalow + 8 Farmhouse + 9 Castle + 10 Mansion + 14 Special property
  )
)]


### condition of the object ----
count_missing$condition = homes[condition < 0, .N]
homes[condition < 0, condition := 0L]
homes[, condition := factor(
  condition,
  0L:10L,
  c(
    "na", "First occupancy", "First occupancy after reconstruction", "Like new",
    "Reconstructed", "Modernised", "Completely renovated", "Well kempt",
    "Needs renovation", "By arrangement", "Dilapidated"
  )
)]


### number of bedrooms ----
count_missing$num_bedrooms = homes[num_bedrooms <= 0, .N]
homes[, num_bedrooms := fcase(
  num_bedrooms <= 0, 0L,
  num_bedrooms >= 7, 7L,
  rep_len(TRUE, length(num_bedrooms)), num_bedrooms
)]

homes[, num_bedrooms := factor(
  num_bedrooms,
  0:7,
  c("na or 0", 1:6, "7+")
)]


### number of bathrooms ----
count_missing$num_bathrooms = homes[num_bathrooms <= 0, .N]
homes[, num_bathrooms := fcase(
  num_bathrooms <= 0, 0L,
  num_bathrooms >= 4, 4L,
  rep_len(TRUE, length(num_bathrooms)), num_bathrooms
)]

homes[, num_bathrooms := factor(
  num_bathrooms,
  0:4,
  c("na or 0", 1:3, "4+")
)]


### total number of floors, create 5 categories ----
homes[, num_floors := fcase(
  between(num_floors, -11, 0), 0L,
  between(num_floors, 4, max(num_floors)), 4L,
  rep_len(TRUE, length(num_floors)), num_floors
)]

homes[, num_floors := factor(num_floors, 0:4, c("na", 1:3, "4+"))]

### facilities of the house, create categories ----
homes[between(equipment, -11, 0), equipment := 0]
homes[, equipment := factor(
  equipment,
  0:4,
  c("na", "Simple", "Normal", "Sophisticated", "Deluxe")
)]

### year of construction, year of renovation ----
homes[, c("constr_year_cat", "renov_year_cat") := lapply(.SD, function(x) {
  fcase(
    x <= 0, 0,
    x < 1900, 1,
    between(x, 1900, 1945), 2,
    between(x, 1946, 1959), 3,
    between(x, 1960, 1969), 4,
    between(x, 1970, 1979), 5,
    between(x, 1980, 1989), 6,
    between(x, 1990, 1999), 7,
    between(x, 2000, 2009), 8,
    between(x, 2010, 2020), 9
  )
}), .SDcols = c("constr_year", "renov_year")]

homes[, c("constr_year_cat", "renov_year_cat") := lapply(.SD,
  factor,
  levels = 0:9,
  labels = c(
    "na", "<1900", "1900-1945", "1946-1959", "1960-1969", "1970-1979",
    "1980-1989", "1990-1999", "2000-2009", "2009+"
  )
), .SDcols = c("constr_year_cat", "renov_year_cat")]


### Type of heating ----
homes[heating_type < 0, heating_type := 0]
# heating_type_labs = get_value_labels('heating_type', TRUE)
# datapasta::vector_paste(heating_type_labs$label) may help
homes[, heating_type := factor(
  heating_type,
  0L:13L,
  c("na", "Cogeneration/combined heat and power plant", "Electric heating",
    "Self-contained central heating", "District heating", "Floor heating",
    "Gas heating", "Wood pellet heating", "Night storage heaters", "Heating by stove",
    "Oil heating", "Solar heating", "Thermal heat pump", "Central heating")
)]


### construction phase ----
homes[constr_phase <= 0, constr_phase := 0L]
homes[, constr_phase := factor(
  constr_phase,
  0L:3L,
  c("na", "House in process of planning", "House in process of building", "House built")
)]

### binary variables -----
# Following ('Klick & Schaffner' 2019, p. 12), for binary variables, we replace
# missing values by 0, i.e., by absence of the feature. Absence is denoted by `Nein` (== no ==0) in binary variables.

binary_vars = c("basement", "protected_building", "guest_washroom", "holiday_house")

# check if absence of the info in a binary variable is denoted by 0
check_for_0 = logical(length(binary_vars))
for (i in seq_along(binary_vars)) {
  check_for_0[[i]] =
    all(0 %in% unique(homes[[binary_vars[[i]]]]))
}

for (i in seq_along(binary_vars)) {
  if (check_for_0[[i]]) {
    homes[, (binary_vars[[i]]) := lapply(.SD, function(v) {
      fcase(v == -9 | v == -7, 0L,
            between(v, -11, -1), NA_integer_,
            rep_len(TRUE, length(v)), v)
      # replace '-9'--other missing or '-7'--not specified, by '0'.
    }),
    .SDcols = binary_vars[[i]]
    ]
  } else {
    warning(sprintf(
      "Variable `%s` does not have `0` in its levels/categories.",
      binary_vars[[i]]
    ))
  }
}
homes[, (binary_vars) := lapply(.SD, as.factor), .SDcols = binary_vars]


# house keeping
rm(main_vars, missing_vals, selected_vars, selected_vars_house_buy,common_vars,
   files, i, translate_names,replace_missing_label_by_value,
   check_for_0, old_names, new_names, binary_vars, order_of_vars
)


# remove protected buildings
homes = homes[protected_building == 0, ]
homes[, protected_building := NULL]

# keep only districts that are defined in BKG end of the year i.e. 2019.12.31
homes[, did := as.integer(did)]
districts = fread("data/processed/admin-areas/districts_destasis.csv",
  select = "did", colClasses = "integer"
)
homes = merge(homes, districts, "did") # two districts c('3152', '3156') will be dropped


# compute distance to the CBD -----
grid1km = st_read("data/raw/RWI-GEO-RED/2020/Raster_shp/ger_1km_rectangle.shp")[, c("idm", "geometry")]
cbds = st_read("data/processed/geodata/CBDs.shp")[, c("did", "geometry")]
cbds$geometry = st_centroid(cbds[, "geometry"])$geometry
cbds = st_transform(cbds, st_crs(grid1km))
cbds$did = as.integer(cbds$did)

# geometry now is the centroid of the grid cell
grid1km$geometry = st_centroid(grid1km[, "geometry"])$geometry
names(grid1km)[names(grid1km) == "idm"] = "grid_id"
grid1km = merge(grid1km, unique(homes[, .(grid_id, did)]), by="grid_id")

dids = unique(cbds$did)
dist2cbd = vector("list", length(dids))
for (did in dids) {
  grid_ids = which(grid1km$did == did)
  dist2cbd[[did]] = data.frame(
    grid_id = grid1km[grid_ids, ]$grid_id,
    did=did,
    dist2cbd = units::set_units(st_distance(grid1km[grid_ids, ], cbds[cbds$did == did, ]), km)
  )
}
dist2cbd = rbindlist(dist2cbd, use.names=TRUE)
homes = merge(homes, dist2cbd, c("grid_id", "did"))


## import consumer price index (CPI) for inflation adjustment ----
cpi = fread("data/raw/consumer-price-index_61121-0001_flat.csv")
cpi = cpi[, grep("^time$|preis", names(cpi), ignore.case=TRUE, value=TRUE), with=FALSE]
names(cpi) = c("year", "cpi")
if (!file.exists("data/processed/consumer-price-index_base-year-2015.csv")) {
  fwrite(cpi, "data/processed/consumer-price-index_base-year-2015.csv")
}

## adjust by the GDP deflator (CPI) --------
homes = merge(homes, cpi[year > 2006, ], "year")
homes[, price := price / (cpi / 100)] # divide by the deflator
homes[, cpi := NULL]

# Define new vars ----
homes[, c("lnprice", "price_sqm") := .(log(price), price / floor_space)]
homes[, lnprice_sqm := log(price_sqm)]
setkeyv(homes, c("did", "year"))

# optional: further cleaning for  hedonic model-----

# few obs in 2007, 2020 is a diabolical year :)
homes = homes[year > 2007 & year < 2020, ]

# problematic construction and renovation years
homes[constr_year < 0, constr_year := NA][renov_year < 0, renov_year := NA]

# perhaps houses not finished or built yet
max_year = min(2019, homes[, max(year)])
homes[constr_year > max_year, constr_year := (max_year)]
# If renovated before built, swap construction year with renovation year
homes[renov_year < constr_year,
      `:=`(renov_year = constr_year, constr_year = renov_year)]

# keep houses built since 1900
homes = homes[constr_year >= 1900 | is.na(constr_year), ]


# imputation of NAs with the overall median value by house type

# fill missing construction year by renovation year:
# could be that not renovated yet, thus construction year == renovation year
homes[is.na(constr_year) & !is.na(renov_year), constr_year := renov_year]
homes[is.na(renov_year) &  !is.na(constr_year), renov_year := constr_year]

homes[, `:=`(
  constr_year = fifelse(
    is.na(constr_year),
    as.integer(median(constr_year, na.rm = TRUE)),
    constr_year
  )
), house_type]

# impute renovation year by the median value,
# if not, replace it with construction year
homes[, `:=`(
  renov_year = fifelse(
    is.na(renov_year),
    max(constr_year, as.integer(median(renov_year, na.rm = TRUE))),
    renov_year
  )
), house_type]


# sanity checks
if (any(idx <- homes[, constr_year < 1900])) {
  message(sprintf("The imputation produced %i very old houses: built before 1900. Removing them...", sum(idx)))
  homes = homes[!idx, ]
}

if (any(homes[, renov_year > max_year])) {
  message("The imputation produced for some homes renov.year > max.year possible.\
          Replaced them by the max.year")
  homes[renov_year > max_year, renov_year := (max_year)]
}

# compute age of houses, and renovation speed
homes[, age0 := max_year - constr_year]
homes[, age1 := renov_year - constr_year]

# drop not-finished houses: House in process of planning or building
# homes = homes[!(constr_phase %like% "(House in process of )?(planning|building)"), ]

# write to disk ----
if (file.exists("data/processed/homes_ready.csv")) {
  warning("File has been overwritten!")
  fwrite(homes, "data/processed/homes_ready.csv", showProgress=TRUE)
}

## single family homes only ----
fwrite(
  homes[house_type == "single-family", !"house_type"],
  "data/processed/homes_single-family_ready.csv"
)
