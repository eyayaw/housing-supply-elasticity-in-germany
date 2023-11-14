library(data.table)

municipals = fread(
  "data/processed/admin-areas/municipalities_destatis.csv", encoding = "UTF-8",
  select = setNames(
    rep("character", 4L), c("mid", "name", "did", "municipality_type")
  )
)

districts = fread(
  "data/processed/admin-areas/districts_destasis.csv",
  colClasses = list("character" = "did"), encoding = "UTF-8"
)


# population at the municipality level, includes for higher levels too
pop = fread("data/raw/Regional-Atlas/12-Population/municipalities/12411-01-01-5_flat.csv",
  select = c(
    "Zeit", "1_Auspraegung_Code", "1_Auspraegung_Label",
    "2_Auspraegung_Label", "BEVSTD__Bevoelkerungsstand__Anzahl"
  ),
  na.strings = c(".", "/", "...", "-"), encoding = "Latin-1"
)

setnames(pop, c("year", "mid", "name", "gender", "pop_level"))

pop[, year := as.integer(sub("\\d{2}\\.\\d{2}\\.(\\d{4})", "\\1", year))]

setcolorder(pop, c("mid", "name", "year"))
setkey(pop, mid, year)

# reshape pop_level to wide
pop = dcast(pop, ... ~ gender, value.var = "pop_level")
setnames(
  pop, c("Insgesamt", "männlich", "weiblich"),
  sprintf("pop_%s", c("tot", "male", "female"))
)


higher_levels = pop[
  mid == "DG" | # Germany
  mid %in% sprintf("%02d", 1:16) | # 16 states
  nchar(mid) == 5L, # districts
  which = T
]
aggregates = pop[higher_levels, ] # values for the states and country Germany
pop = pop[-higher_levels, ]      # keep only lower levels

# municipality population
pop_municipals = merge(municipals, pop[, !"name"], "mid")

# these are possibly district-municipalities, one municipality in the district
distr_municipals = municipals[!pop, on = "mid"] # anti-join, -> not matching district-municipalities
## we can check with
# distr_mun = municipals[municipals[, .N, did][N==1L, !'N'], on='did']
# distr_municipals[distr_mun, on=c('mid', 'did', 'name')]

# add population data
distr_municipals = distr_municipals[aggregates[, !"name"], on = "did==mid", nomatch = NULL]

## city-states with only 1 municipality, Berlin and Hamburg
pattern = quote(mid %like% "^(02|11)" & name %like% "Berlin|Hamburg")
city_states = municipals[i, env = list(i = pattern)]

city_states[, state_code := sub("^(02|11)\\d+$", "\\1", mid)]

city_states = city_states[aggregates[i, env = list(i = pattern), !"name"], on = "state_code==mid"
][, !"state_code"]


# all municipalities
pop_municipals = list(pop_municipals, distr_municipals, city_states) |>
  rbindlist(use.names = TRUE)

setcolorder(pop_municipals, "name", after = "did")
setkey(pop_municipals, mid, year)


# drop municipalities that are unoccupied i.e.
# `municipality_type == gemeindefreies Gebiet-unbewohnt (66)`
pop_municipals = pop_municipals[!municipality_type %like% "Gebiet-unbewohnt \\(66\\)", ]

# fill na values with the mean value
cols = sprintf("pop_%s", c("tot", "male", "female"))
pop_municipals[, paste0("filled_", cols) :=
  lapply(.SD, \(x) nafill(x, type = "const", fill = round(mean(x, na.rm = T)))),
.SDcols = cols, mid
]

pop_municipals[, grep("male|female", names(pop_municipals), value = TRUE, invert = TRUE), with = FALSE] |>
  fwrite("data/processed/main/population_municipality.csv")

## extra ----
# former municipalities but later merged or dissolved
old_municipals = pop[!municipals, on = "mid", .N, .(mid, name)]
