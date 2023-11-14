library(data.table)
source("script/cleaning/clean_regional-atlas_helpers.R")
source("script/cleaning/territorial_changes.R")

#' -------------------------------------------------------------------------
# In this script, we 'read in' the `_flat.csv` data sets downloaded from the
# regional database and process them and output tidy data sets.
# The processed data sets are saved in `data/processed/main/`
#' -------------------------------------------------------------------------

dir_out = "data/processed/main/"
dir.create(dir_out, recursive = T, showWarnings = F)
# housing stock-floorspace --------

# Note
# floorspace does not come in the *_flat.csv housing stock files.
# But, if we download the .csv (not flat.csv in Regional Atlas naming),
# housing stock files include floorspace info. So, in this script only floorspace
# is an exception. Once we have that, we can 'even' skip housing stock below since
# housing stock is included in .csv file as well.

# This one: ('data/processed/main/residential_stock-floorspace.csv')

# housing stock -----------------------------------------------------------

# comes from two different data sets
# one until 2010, another from 2011 and on wards
fpath = file.path(
  "data/raw/Regional-Atlas/31-Building-and-Housing/",
  # until 2010, after 2010
  c("31-231/31231-01-02-4_flat.csv", "31-231/31231-02-01-4_flat.csv")
)

## up until 2010 ------------
stock_upto_2010 = tidy_atlas(fpath[[1]])
stock_upto_2010 = stock_upto_2010[!(is.na(feature_code_2) & is.na(feature_label_2)), ]

# keep only residential building, by the number of apartments
stock_upto_2010 = stock_upto_2010[
  "Buildings by number of apartments", on = "feature_label_2",
    !"number_of_apartments_in_residential_and_non_residential_buildings"
  ]

stock_upto_2010 = stock_upto_2010[, keep_vars(stock_upto_2010), with=F] |>
  dcast(... ~ expression_label_2, value.var = "number_of_residential_buildings")

setnames(stock_upto_2010, tidy_names(stock_upto_2010))
stock_upto_2010[, year := year(as.Date(year, "%d.%m.%Y"))]
setnames(stock_upto_2010, "all_in_all", "total_buildings")

## after 2010 ------------
stock_after_2010 = tidy_atlas(fpath[[2]])
stock_after_2010 = stock_after_2010[!(is.na(feature_code_2) & is.na(feature_label_2)), ]

# keep only residential building, by the number of apartments
stock_after_2010 = stock_after_2010[
  "Residential buildings by number of apartments", on = "feature_label_2",
  !"number_of_apartments_in_residential_and_non_residential_buildings"
]

stock_after_2010 = stock_after_2010[, keep_vars(stock_after_2010), with=F] |>
  dcast(... ~ expression_label_2, value.var = "number_of_residential_buildings")

setnames(stock_after_2010, tidy_names(stock_after_2010))
stock_after_2010[, year := year(as.Date(year, "%d.%m.%Y"))]
setnames(stock_after_2010, "all_in_all", "total_buildings")

setcolorder(
  stock_after_2010,
  c(setdiff(names(stock_after_2010), "total_buildings"), "total_buildings")
)

setnames(stock_after_2010,
  c("residential_building_with_1_apartment", "residential_building_with_2_apartments",
    "residential_building_with_3_and_more_apartments"),
  c("apartment_1", "apartments_2", "apartments_3_or_more")
)

### bind the two tables together ----
stock = list(stock_upto_2010, stock_after_2010) |>
  rbindlist(use.names = T, fill = T)

setcolorder(stock, put_last(stock, "total_buildings"))

cols = c("apartment_1", "apartments_2", "dormitories", "apartments_3_or_more", "total_buildings")

stock = vmake_consistent(stock, cols, "sum", na.rm = TRUE)

# writing to disk
fwrite(stock,
  file.path(dir_out, "house-stock_residential-buildings_by-number-of-apartments.csv"),
  encoding = "UTF-8"
)

# house keeping
rm(stock_after_2010, stock_upto_2010)

# land prices -------------------------------------------------------------

fpath = atlas_path("61-Prices/61-511/61511-01-03-4_flat.csv")
land_price = tidy_atlas(fpath)
# keep only land sold for construction purposes
land_price = land_price[
  expression_label_2 %like% "land.?ready.?for.?construction",
  !c("expression_code_2", "expression_label_2")
]
land_price[, `:=`(
  land_sold_area = 1000 * sold_building_land_area_1000_sqm,
  land_sold_value = 1000 * purchase_price_thousand_eur,
  sold_building_land_area_1000_sqm = NULL,
  purchase_price_thousand_eur = NULL
  ), ]

setnames(land_price,
  c("cases_of_sale_of_building_land_number", "average_purchase_value_per_square_meter_of_eur"),
  c("land_sold_count", "pland")
)

# correct inconsistencies
cols = c("land_sold_count", "land_sold_area", "land_sold_value", "pland")
fns = c("sum", "sum", "sum", "mean")

land_price = vmake_consistent(land_price, cols, fns, na.rm = TRUE)
# better to recompute price of land (eur per sqm) from the total value / total area
# the i0dea is that make_consistent works for values to be summed than averaged.
# land_price[, pland := land_sold_value/land_sold_area]

fwrite(land_price, file.path(dir_out, "land-prices.csv"))


# completions -------

# 31121-01-02-4:	Completion of new residential buildings and apartments in residential buildings according to the number of apartments - annual total - regional depth: districts and regional cities

completions = tidy_atlas(atlas_path("31-Building-and-Housing/31-121/31121-01-02-4_flat.csv"))

completions = completions[!(is.na(feature_label_3) & is.na(expression_label_3)), ]

## buildings completions, by the number of apartments -----
completions_buildings = completions[
  "(according to the number of apartments)",
  on = .(feature_label_3), !"number_of_apartments"
]

completions_buildings <-
  completions_buildings[, keep_vars(completions_buildings), with = F] |>
   dcast(... ~ expression_label_3, value.var = "number_of_buildings")
setnames(completions_buildings, tidy_names(completions_buildings))

setnames(completions_buildings, "all_in_all", "total_buildings_completions")
setcolorder(
  completions_buildings,
  put_last(completions_buildings, "total_buildings_completions")
)

# testing whether the constituents make up the total
incols = c("apartment_1", "apartments_2", "with_3_and_more_apartments")
try(if (completions_buildings[
  !(is.na(apartment_1) |
    is.na(apartments_2) |
    is.na(with_3_and_more_apartments)),
  all.equal(total_buildings_completions, rowSums(.SD)),
  .SDcols = incols
]) {
  message("Yeeeey!!! The `sum of the parts` is equal to the `total`!")
})
cols = c(incols, "total_buildings_completions")
completions_buildings = vmake_consistent(completions_buildings, cols, "sum", na.rm = TRUE)
rm(incols, cols)

## apartments completions ---------
completions_apartments = completions[
  "in residential buildings", on = .(feature_label_3), !"number_of_buildings"
  ]

completions_apartments <-
  completions_apartments[, keep_vars(completions_apartments), with = F] |>
  dcast(... ~ expression_label_3, value.var = "number_of_apartments")
setnames(completions_apartments, tidy_names(completions_apartments))

setnames(completions_apartments, "all_in_all", "total_apartments_completions")
setcolorder(
  completions_apartments,
  put_last(completions_apartments, "total_apartments_completions")
)

# testing whether the constituents make up the total
incols = c("apartment_1", "apartments_2", "with_3_and_more_apartments")
try(if (completions_apartments[
  !(is.na(apartment_1) |
    is.na(apartments_2) |
    is.na(with_3_and_more_apartments)),
  all.equal(total_apartments_completions, rowSums(.SD)),
  .SDcols = incols
]) {
  message("Yeeeey!!! The `sum of the parts` is equal to the `total`!")
})

cols = c(incols, "total_apartments_completions")
completions_apartments = vmake_consistent(completions_apartments, cols, "sum", na.rm = TRUE)
rm(incols, cols)

# write to disk
fwrite(completions_buildings, file.path(dir_out, "completions-buildings.csv"))
fwrite(completions_apartments, file.path(dir_out, "completions-apartments.csv"))

# house keeping
rm(completions, completions_buildings, completions_apartments)

# permits --------

# 31111-01-02-4: Permits for the construction of new residential buildings and apartments in residential buildings according to the number of apartments - annual total - regional depth: districts and regional cities

permits = tidy_atlas(atlas_path("31-Building-and-Housing/31-111/31111-01-02-4_flat.csv"))

permits = permits[!(is.na(feature_label_3) & is.na(expression_label_3)), ]

## buildings permits, by the number of apartments -----
permits_buildings = permits[
  "(according to the number of apartments)", on = .(feature_label_3),
  !"number_of_apartments_in_residential_buildings"
]

permits_buildings <-
  permits_buildings[, keep_vars(permits_buildings), with = F] |>
  dcast(... ~ expression_label_3, value.var = "number_of_residential_buildings")
setnames(permits_buildings, tidy_names(permits_buildings))

setnames(permits_buildings, "all_in_all", "total_buildings_permits")
setcolorder(
  permits_buildings,
  put_last(permits_buildings, "total_buildings_permits")
)

# testing whether the constituents make up the total
incols = c("apartment_1", "apartments_2", "with_3_and_more_apartments")
try(if (na.omit(permits_buildings[, c(incols, "total_buildings_permits"), with=F])[,
  all.equal(total_buildings_permits, rowSums(.SD)),
  .SDcols = incols
]) {
  message("Yeeeey!!! The `sum of the parts` is equal to the `total`!")
})

cols = c(incols, "total_buildings_permits")
permits_buildings = vmake_consistent(permits_buildings, cols, "sum", na.rm = T)
rm(incols, cols)

## apartments permits ---------
permits_apartments = permits[
  "in residential buildings", on = .(feature_label_3), !"number_of_residential_buildings"
  ]

permits_apartments <-
  permits_apartments[, keep_vars(permits_apartments), with = F] |>
  dcast(... ~ expression_label_3, value.var = "number_of_apartments_in_residential_buildings")
setnames(permits_apartments, tidy_names(permits_apartments))

setnames(permits_apartments, "all_in_all", "total_apartments_permits")
setcolorder(
  permits_apartments, put_last(permits_apartments, "total_apartments_permits")
)


# testing whether the constituents make up the total
incols = c("apartment_1", "apartments_2", "with_3_and_more_apartments")
try(if (permits_apartments[
  !(is.na(apartment_1) |
    is.na(apartments_2) |
    is.na(with_3_and_more_apartments)),
  all.equal(total_apartments_permits, rowSums(.SD)),
  .SDcols = incols
]) {
  message("Yeeeey!!! The `sum of the parts` is equal to the `total`!")
})

cols = c(incols, "total_apartments_permits")
permits_apartments = vmake_consistent(permits_apartments, cols, "sum", na.rm = TRUE)
rm(incols, cols)

# write to disk
fwrite(permits_buildings, file.path(dir_out, "permits-buildings.csv"))
fwrite(permits_apartments, file.path(dir_out, "permits-apartments.csv"))

# house keeping
rm(permits, permits_buildings, permits_apartments)

# population --------------------------------------------------------------

# 12411-01-01-4:	Population by gender - reference date 31.12. - regional depth: counties and regional cities	https://www.regionalstatistik.de:443/genesis//online?operation=table&code=12411-01-01-4&bypass=true&levelindex=0&levelid=1622726026433

pop = tidy_atlas(atlas_path("12-Population/12411-01-01-4_flat.csv"))
pop = pop[!(is.na(expression_code_2) & is.na(expression_label_2)), ]
pop[, year := year(as.Date(year, "%d.%m.%Y"))]

pop = pop[, keep_vars(pop), with = F] |>
  dcast(... ~ expression_label_2, value.var = "population_level_number")

setnames(pop, tidy_names(pop))
setnames(pop, "all_in_all", "tot")
setcolorder(pop, put_last(pop, "tot"))

setnames(pop, c("tot", "female", "male"), paste0(c("tot", "female", "male"), "_pop"))

cols = paste0(c("female", "male", "tot"), "_pop")
pop = vmake_consistent(pop, cols, "sum", na.rm = T)

fwrite(pop, file.path(dir_out, "population_by-gender_district.csv"))


# labor market -----------------------------------------------------------

## Statistics of employees subject to social security contributions (13111) -----
# Content : Statistics of employees subject to social security contributions
# * 13111-01-03-4:  by place of work
# * 13111-02-02-4:  by place of residence

### by place of  work, residence ------
emp = c(
  atlas_path("13-Labor-Market/13-111/13111-01-03-4_flat.csv"),
  atlas_path("13-Labor-Market/13-111/13111-02-02-4_flat.csv")
) |>
  lapply(tidy_atlas)

emp = lapply(emp, \(x) x[!(is.na(expression_label_2) & is.na(expression_label_3)), ])
emp = merge(emp[[1]], emp[[2]], intersect(names(emp[[1]]), names(emp[[2]])))

emp[, year := year(as.Date(year, "%d.%m.%Y"))]
setnames(emp,
  c("employees_subject_to_social_security_(place_of_work)_number",
    "employees_subject_to_social_insurance_(place_of_residence)_number"),
  c("emp_work", "emp_resid")
)

emp = emp[, !c("expression_code_2", "expression_code_3")] |>
  dcast(... ~ expression_label_2 + expression_label_3,
    value.var = c("emp_work", "emp_resid")
  )

setnames(emp, tidy_names(emp))
setnames(emp, sub("all_in_all", "total", names(emp)))
emp[, c("emp_work_foreigners_female", "emp_work_foreigners_male",
        "emp_resid_foreigners_female", "emp_resid_foreigners_male") := NULL]

setnames(emp, gsub("_?total", "", names(emp)))

emp = vmake_consistent(emp, grep("^emp_work|resid", names(emp), value = TRUE), "sum", na.rm=TRUE)

fwrite(emp, file.path(dir_out, "employment_by-place-of-work--residence_by-gender-nationality.csv"))


## Employment accounts of the federal government & countries (WZ2008) (13312) ----
# * 13312-01-05-4	(Erwerbstätige nach Wirtschaftszweigen) Employed persons by branch of industry - annual average - regional depth: counties and Krfr. Cities
# * 13312-02-03-4	(Arbeitnehmer nach Wirtschaftszweigen) Employees by branch of industry - annual average - regional depth: counties and regional cities

### employment by industry composition --------

# Erwerbstätige includes self-employed, Arbeitnehmer does not
#### county-level ----
share_inc = tidy_atlas(atlas_path("13-Labor-Market/13-312/13312-01-05-4_flat.csv"))
share_exc = tidy_atlas(atlas_path("13-Labor-Market/13-312/13312-02-03-4_flat.csv"))

setnames(share_inc, "employed_1000_on_average_per_year", "emp_1000_inc_selfemp")
setnames(share_exc, "annual_average_employees_1000", "emp_1000_exc_selfemp")

share = merge(share_inc, share_exc, intersect(names(share_inc), names(share_exc)))
share = share[!(is.na(expression_code_2) & is.na(expression_label_2)), ]

share = share[, !"expression_code_2"] |>
  dcast(... ~ expression_label_2, value.var = c("emp_1000_inc_selfemp", "emp_1000_exc_selfemp"))

setnames(share, tidy_names(share))
setnames(share, sub("all_in_all", "total", names(share)))

rm(share_inc, share_exc)

share = vmake_consistent(share, grep("^emp_1000_exc|inc", names(share), value = TRUE), "sum", na.rm = TRUE)

fwrite(share,
  file.path(dir_out, "employment-share_by-industry_inc-exc-self--employed_yearly-average-1000.csv")
)

#### national-level ------

share_inc = tidy_atlas(atlas_path("13-Labor-Market/13-312/13312-01-05-4_flat.csv"), return_agg = T)[[2]]
share_exc = tidy_atlas(atlas_path("13-Labor-Market/13-312/13312-02-03-4_flat.csv"), return_agg = T)[[2]]

setnames(share_inc, "employed_1000_on_average_per_year", "emp_1000_inc_selfemp")
setnames(share_exc, "annual_average_employees_1000", "emp_1000_exc_selfemp")

share = merge(share_inc, share_exc, intersect(names(share_inc), names(share_exc)))
share = share[!(is.na(expression_code_2) & is.na(expression_label_2)), ]

share = share[, !"expression_code_2"] |>
  dcast(... ~ expression_label_2, value.var = c("emp_1000_inc_selfemp", "emp_1000_exc_selfemp"))

setnames(share, tidy_names(share))
setnames(share, sub("all_in_all", "total", names(share)))

rm(share_inc, share_exc)
# national data does need inconsistency correction :D
fwrite(
  share,
  file.path(dir_out, "employment-share_by-industry_inc-exc-self--employed_yearly-average-1000_national.csv")
)


# National account, GDP ---------------------------------------------------
# 82111	National accounts of the federal states: production calculation

## GDP --------------------------------------------------------------------
# 82111-01-05-4	Gross domestic product / gross value added according to economic sectors - annual total - regional depth: districts and cities	https://www.regionalstatistik.de:443/genesis//online?operation=table&code=82111-01-05-4&bypass=true&levelindex=0&levelid=1622821880890

# The main variable here is 'BWS zu Herstellungspreisen in jeweiligen Preisen Tsd. EUR'
# 'GVA at basic prices in current prices EUR thousand'
gdp = tidy_atlas(atlas_path("82-National-Accounts/82-111/82111-01-05-4_flat.csv"))

gdp = gdp[!(is.na(expression_code_2) & is.na(expression_label_2)), ]

gdp = gdp[, keep_vars(gdp), with=F] |>
  dcast(... ~ expression_label_2,
        value.var = "gva_at_basic_prices_in_current_prices_in_eur_thousand" )

setnames(gdp, tidy_names(gdp))
setnames(gdp, "all_in_all", "tot")
setcolorder(gdp, put_last(gdp, "tot"))

gdp = vmake_consistent(gdp, setdiff(names(gdp), c("did", "year", "name")), "sum", na.rm=TRUE)

fwrite(gdp, file.path(dir_out, "GVA_in-current-prices_EUR-Tsd.csv"))

## Private disposable income ------------------

income = tidy_atlas(atlas_path("82-National-Accounts/82-411/82411-01-03-4_flat.csv"))

income = vmake_consistent(income, grep("income_", names(income), value = TRUE), "mean", na.rm=TRUE)

fwrite(income,
  file.path(dir_out, "disposable-income_private-HH.csv"), encoding = "UTF-8"
)


# Territory, area ----

area = tidy_atlas(atlas_path("11-Territory/11111-01-01-4_flat.csv"))
area[, year := year(as.Date(year, "%d.%m.%Y"))]

area = vmake_consistent(area, "territorial_area_qkm", "sum", na.rm = T)

fwrite(area, file.path(dir_out, "territorial-area_qkm.csv"))


# merge all ---------------------------------------------------------------
flist = list.files("data/processed/main", ".+[.]csv$", full.names = T)
flist = grep("construction-costs-index", flist, value = T, invert = T)
name_list = lapply(flist, fread, nrows = 1L) |> lapply(names)
names(name_list) = tools::file_path_sans_ext(basename(flist))
names(flist) = names(name_list)

# does not run
if (FALSE) {
  Reduce(
    \(x, y) merge.data.table(x, y, by = c("did", "name", "year")),
    lapply(flist, fread)
  )
}
