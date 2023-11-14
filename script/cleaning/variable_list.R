files <- dir("data/raw/RWI-GEO-RED/2020/HiDrive/csv/HK_SUF_csv/",
             pattern = "(H|W)(K|M)SUF[0-9][.]csv$", full.names = TRUE)

# collect variable names in each data set
dir_path = "output/var-names"
dir.create(dir_path, showWarnings = FALSE)

if (FALSE) {
  vars_in_datasets <- lapply(files, function(f) {
    fname <- basename(f)
    nms <- system(paste("head -n 1", f), intern = TRUE) # read the 1st line
    nms <- sub("\r", "", nms) # remove a carriage return at the end of the text
    nms <- strsplit(nms, ",")[[1]]
    writeLines(nms, paste0(
      dir_path, "/var-names-",
      sub("csv", "txt", fname)
    ))
    return(nms)
  })
} else {
  vnlist <- dir(dir_path, "var-names-.*[.]txt$", full.names = TRUE)
  vars_in_datasets <- lapply(vnlist, readLines)
  names(vars_in_datasets) <- regmatches(basename(vnlist),
                                regexpr("(H|W)(K|M)SUF\\d", basename(vnlist)))
}


# The list of variables that are available every year (common to all data sets)
common_vars <- Reduce(intersect, vars_in_datasets)
# list of vars that appear at least once in the data sets
all_vars = Reduce(union, vars_in_datasets)

# a matrix showing whether a variable is available or not in each data set
data_record = matrix(nrow = length(all_vars), ncol = length(vars_in_datasets))
dimnames(data_record) = list(all_vars, names(vars_in_datasets))
for (v in seq_along(all_vars)) {
  for (i in seq_along(vars_in_datasets)) {
    data_record[v, i] =  as.integer(all_vars[[v]] %in% vars_in_datasets[[i]])
  }
}

data_record <- cbind(data_record, sum = rowSums(data_record))
# house keeping
rm(all_vars, vars_in_datasets, data_record, vnlist, v)

# Selected variables and their availability across all real estate types ----
## availability is encoded 1 available 0 not available
## xxxx: # first digit for house-miete
         # second   -  -   house-kauf
         # third    -  -   wohnung-miete
         # fourth   -  -   wohnung-kauf

selected_vars <- tibble::tribble(
  ~var_de, ~var_en, ~label, ~availability,
  "obid", "obid", "Object identifier", "1111",
  "kid2019", "kid2019", "District identifier (AGS, 2019)", "1111",
  'jahr', 'year', NA, '1111',
  "kaufpreis", "price", "Purchase price in Euro", "0101",
  "kategorie_Haus", "house_type", "House type", "1100",
  "grundstuecksflaeche", "plot_size", "Plot area of the object", "1100",
  "wohnflaeche", "floor_space", "Living space in sqm", "1111",
  "nutzflaeche", "usable_floor_space", "Usable floor space", "1111",
  "schlafzimmer", "num_bedrooms", "Number of bedrooms", "1111",
  "zimmeranzahl", "num_rooms", "Number of rooms", "1111",
  "badezimmer", "num_bathrooms", "Number of bathrooms", "1111",
  "baujahr", "constr_year", "Year of construction", "1111",
  "letzte_modernisierung", "renov_year", "Year of last modernization", "1111",
  "anzahletagen", "num_floors", "Number of floors", "1111",
  "ausstattung", "equipment", "Facilities of the object", '1111',
  "immobilientyp", "type", "Type of real estate, hm,hk,wm,wk", "1111",
  "balkon", "balcony", "Balcony", "0011",
  "garten", "garden", "Garden", "0011",
  "keller", "basement", "Basement", "1111",
  "heizungsart", "heating_type", "Type of heating", "1111",
  "mietekalt", "rent", "Rent net of utilities", "1010",
  "nebenkosten", "utilities", "Monthly costs for utilites in Euro", "1010",
  "etage", "floor", "Floor location of apartment", "0011",
  "einbaukueche", "kitchen", NA, "1011",
  "aufzug", "elevator", NA, "1011",
  "kategorie_Wohnung", "flat_type", "Flat type", "0011",
  "uniqueID_gen", "unique_id", "Unique object identifier (generated)", "1111",
  "ajahr", "ad_begin_year", "Beginning of ad, year", "1111",
  "amonat", "ad_begin_mon", "Beginning of ad, month", "1111",
  "ejahr", "ad_end_year", "Ending of ad, year", "1111",
  "emonat", "ad_end_mon", "Ending of ad, month", "1111",
  "denkmalobjekt", "protected_building", "Protected historic building", "0101",
  "ferienhaus", "holiday_house", "Usable as holiday house", "0101",
  "gaestewc", "guest_washroom", "Guest toilet", "1111",
  "parkplatz", "parking_space", "Garage/parking space available", "1111",
  "parkplatzpreis", "parking_space_price", "Price of parking space (euro)", "1111",
  "bauphase", "constr_phase", "Construction phase", "0100",
  "einliegerwohnung", "granny_flat", "Granny flat in the object", "0100",
  "rollstuhlgerecht", "wheelchair_accessible", "Wheelchair accessible, no steps", "1111",
  "objektzustand", "condition", "Condition of object", "1111",
  "blid", "state", "German state", "1111",
  "erg_amd", "lab_mrkt_reg", "Local labour market (Kosfeld and Werner, 2012)", "1111",
  # `r1_id` another name for `ergg_1km`
  "r1_id", "grid_id", "1-sqkm raster cell following INSPIRE", "1111",
  "gid2015", "gid2015", "Municipality Identifier (AGS, 2015)", "1111",
  "ags2019", "ags2019", "Municipality Identifier (AGS, 2019)", "1111",
  "kid2015", "kid2015", "District identifier (AGS, 2015)", "1111",
  "plz", "postcode", "Address: postcode", "1111"
)

# selected variables for house purchases (hk) with available data
selected_vars_house_buy <- subset(selected_vars, grepl("[01]1[01]{2}", availability))

# main variables ---------------------------------------------------------------
main_vars <- tibble::tribble(
  ~var_de, ~coltype,
  "obid", "integer",
  "jahr", "integer",
  "ajahr", "integer",
  "amonat", "integer",
  "emonat", "integer",
  "kaufpreis", "double",
  "kategorie_Haus", "character",
  "wohnflaeche", "double",
  "nutzflaeche", "usable_floor_space",
  "grundstuecksflaeche", "double",
  "badezimmer", "integer",
  "schlafzimmer", "integer",
  "zimmeranzahl", "double",
  "baujahr", "integer",
  "letzte_modernisierung", "integer",
  "bauphase", "character",
  "anzahletagen", "integer",
  "keller", "character",
  "gaestewc", "character",
  "parkplatz", "character",
  "parkplatzpreis", "double",
  "heizungsart", "character",
  "ausstattung", "character",
  "denkmalobjekt", "character",
  "ferienhaus", "character",
  "objektzustand", "character",
  "erg_amd", "character",
  "kid2015", "character",
  "kid2019", "character",
  "r1_id", "character",
  "blid", "character",
  "uniqueID_gen", "integer"
)

main_vars = merge(main_vars, selected_vars_house_buy, by = 'var_de', all.x = TRUE)

