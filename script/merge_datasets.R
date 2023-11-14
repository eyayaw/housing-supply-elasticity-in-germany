library(data.table)
source("script/helpers/helpers.R")

# brings all the datasets together
# produces main.csv which is used for the analysis


districts = fread_keepzeros(
  "data/processed/admin-areas/districts_destasis.csv",
  encoding = "UTF-8", select = c("did", "name")
)

select = c(
  "did", "year", "price", "floor_space", "plot_size", "house_type",
  "constr_year", "renov_year", "constr_phase", "equipment", "condition",
  "lnprice_sqm"
)

homes = fread(
  "data/processed/homes_ready.csv",
  select = select, colClasses = list("integer" = "year") # did does not have leading zeros
)

homes = homes[year > 2007 & year < 2020, ] # few observations in these years
homes[, did := appendLeadingZeros(did)] # add leading zero
rm(select)

cpi = fread_keepzeros("data/processed/consumer-price-index_base-year-2015.csv")

# descriptive stats -------------------------------------------------------

# descriptive statistics, from the rwi-immoscout data
hometypes = c("all", "_single_fam" = "single-family")
desc_stats = homes[, .(
  count_immo = .N, # total num of houses advertised yearly in each district
  price_sqm = mean(price / floor_space),
  lnprice_sqm = mean(lnprice_sqm)
  # med_lnprice_sqm = median(lnprice_sqm),
  # sd_lnprice_sqm = sd(lnprice_sqm),
  # floor_space_immo = mean(floor_space),
  # plot_size_immo = mean(plot_size)
), by = .(did, year)]


desc_stats_single = homes[house_type == hometypes[[2]],
  .(
    count_immo = .N,
    price_sqm = mean(price / floor_space),
    lnprice_sqm = mean(lnprice_sqm)
    # med_lnprice_sqm = median(lnprice_sqm),
    # sd_lnprice_sqm = sd(lnprice_sqm),
    # floor_space_immo = mean(floor_space),
    # plot_size_immo = mean(plot_size)
  ),
  by = .(did, year)
]


desc_stats = merge(
  desc_stats, desc_stats_single, c("did", "year"),
  suffixes = names(hometypes)
)

rm(desc_stats_single)
setkey(desc_stats, did, year)

# District Hedonic Price Index, yearly -----
flist = sprintf("data/processed/hedonic/fe-fixest_%s-homes.csv", hometypes)
names(flist) = hometypes
# sub("(?:.*)fe-fixest_(?:(.*)-homes)[.]csv$", "\\1", flist)

HPI = lapply(flist, fread_keepzeros,
  select = c("did", "year", "eff"), col.names = c("did", "year", "hpi")
)
HPI = merge(HPI[[1]], HPI[[2]], c("did", "year"), suffixes = names(hometypes))
# hpi comes in log from the hedonic estimation
HPI[, paste0("hpi", names(hometypes)) :=
  lapply(.SD, exp), .SDcols = paste0("hpi", names(hometypes))]

HPI[, did := appendLeadingZeros(did)]
rm(flist)

# housing stock-floorspace, from the regional atlas -----------------------------

stock_fspace = fread_keepzeros(
  "data/processed/main/residential_stock-floorspace.csv",
  drop = "name"
)
hstock = fread_keepzeros(
  "data/processed/main/house-stock_residential-buildings_by-number-of-apartments.csv",
  select = c("did", "year", "total_buildings")
)

hstock = merge(stock_fspace, hstock, c("did", "year"), suffixes = c("_k", "_d"))

# both the `floorspace` & `hstock` dfs have the same housing stock data
try(
  if (with(hstock, all.equal(total_buildings_k, total_buildings_d))) {
    message("Good!")
  },
  silent = TRUE
)
hstock[, total_buildings_d := NULL]
setnames(hstock, "total_buildings_k", "total_buildings")
rm(stock_fspace)

# completions and permits, apartments & buildings ----
part = c(
  "total_buildings",
  "1-apart-building", "2-apart-building", "3ormore-apart-building",
  "total_apartments", "3ormore-apart-apartment", "floorspace"
)
vs = c("permits", "completions")


permits = fread_keepzeros(sprintf("data/processed/main/%s.csv", vs[[1]]),
  select = c("did", "year", sprintf("%s_%s", vs[[1]], part))
)
completions = fread_keepzeros(sprintf("data/processed/main/%s.csv", vs[[2]]),
  select = c("did", "year", sprintf("%s_%s", vs[[2]], part))
)


setkeyv(permits, c("did", "year"))
setkeyv(completions, c("did", "year"))

## shift completions by a year down because 2010's completions are 2009's permits?
if (FALSE) {
  completions[, (sprintf("%s_%s", vs[[2]], part)) := lapply(.SD, \(x) shift(x, 1, type = "lead")),
    did,
    .SDcols = sprintf("%s_%s", vs[[2]], part)
  ]

  # the last year (2020) completion becomes NA as a result
  completions[year %in% c(2019, 2020),
    (sprintf("%s_%s", vs[[2]], part)) := lapply(.SD, nafill, type = "locf"),
    did,
    .SDcols = sprintf("%s_%s", vs[[2]], part)
  ]
}

## fill NA with the previous year's value
permits[, (sprintf("%s_%s", vs[[1]], part)) := lapply(.SD, nafill, type = "locf"),
  did,
  .SDcols = sprintf("%s_%s", vs[[1]], part)
]
completions[, (sprintf("%s_%s", vs[[2]], part)) := lapply(.SD, nafill, type = "locf"),
  did,
  .SDcols = sprintf("%s_%s", vs[[2]], part)
]

permit_completion = merge(permits, completions, c("did", "year"))
fwrite(
  permit_completion,
  "data/processed/main/total_apartments-buildings_completions-permits.csv"
)

# population, gdp, income, land price ----
pop = fread_keepzeros("data/processed/main/population_by-gender_district.csv",
  select = c("did", "year", "tot_pop")
)
pop[, tot_pop := nafill(tot_pop, "locf"), did]

gdp = fread_keepzeros("data/processed/main/GVA_in-current-prices_EUR-Tsd.csv",
  select = c("did", "year", "tot")
)
gdp[, c("gdp_euro", "tot") := .(1000 * tot, NULL)]
income = fread_keepzeros("data/processed/main/disposable-income_private-HH.csv",
  select = c("did", "year", "household_income_per_inhabitant_eur")
)
setnames(income, "household_income_per_inhabitant_eur", "hhinc")

rent = fread_keepzeros("data/processed/main/land-prices.csv", drop = "name")
cols = c("land_sold_count", "land_sold_area", "land_sold_value", "pland")

rent[, (cols) := lapply(.SD, \(x) fifelse(x <= 0, NA_real_, x)), .SDcols = cols]
rent[, (cols) := lapply(.SD, nafill, type = "locf"), did, .SDcols = cols]
# adjust for inflation
rent = rent[cpi, on = "year", nomatch = NULL]
rent[, pland := pland * 100 / cpi][, cpi := NULL]

controls = list(hstock, permit_completion, pop, gdp, income, rent) |>
  Reduce(f = \(x, y) merge(x, y, all = TRUE))

# fill missing (NA) by last observation carried forward? -----
in_cols = setdiff(names(controls), c("did", "year"))
controls[, (in_cols) := lapply(.SD, nafill, type = "locf"), .(did), .SDcols = in_cols]
# fill missing (NA) by NOCB (next observation carried backward) ------------------
controls[, (in_cols) := lapply(.SD, nafill, type = "nocb"), .(did), .SDcols = in_cols]


# merging ------
main = merge(HPI, desc_stats, by = c("did", "year"))
main = merge(main, controls, by = c("did", "year"))

keys = c("did", "year")
setcolorder(main, keys)

fwrite(main, "data/processed/main/main_in-levels.csv")


# changes 2008-2019 -----
# you may keep 2009 value if 2008 value is an outlier because of NA filling

main_wide = main[
  year %in% c(2008, 2019),
  .(
    did, year, hpi, hpi_single_fam, count_immo, count_immo_single_fam,
    total_buildings,
    floorspace,
    single_fam = `1-apart-building`,
    single_fam_floorspace = floorspace * `1-apart-building` / total_buildings,
    permits_total_buildings,
    single_fam_permits = `permits_1-apart-building`,
    single_fam_permits_floorspace = permits_floorspace * `permits_1-apart-building` / permits_total_buildings,
    permits_total_apartments,
    permits_floorspace,
    completions_total_buildings,
    single_fam_completions = `completions_1-apart-building`,
    single_fam_completions_floorspace = completions_floorspace * `completions_1-apart-building` / completions_total_buildings,
    completions_total_apartments,
    completions_floorspace,
    pop = tot_pop, gdp_euro, hhinc,
    land_sold_count, land_sold_area, land_sold_value, pland
  )
]
main_vars = setdiff(names(main_wide), c("did", "year"))
main_wide = reshape(main_wide,
  direction = "wide", idvar = "did",
  timevar = "year", v.names = main_vars, sep = "_"
)

# log transform -> compute long difference
## using `computing on the language`
## https://rdatatable.gitlab.io/data.table/articles/datatable-programming.html

x1 = sprintf("%s_%d", main_vars, 2019L)
x0 = sprintf("%s_%d", main_vars, 2008L)
j = substitute(j,
  env = list(j = setNames(
    Map(
      \(e, s) call("-", call("log", e), call("log", s)),
      lapply(x1, as.name), lapply(x0, as.name)
    ),
    paste0("dln_", main_vars)
  ))
)

j[["did"]] = quote(did)

changes = main_wide[, jj, env = list(jj = j)]
main_wide = merge(main_wide, changes, "did")

# labor demand shock / shift-share ----
labor_demand = fread_keepzeros("data/processed/main/bartick-shocks.csv")
labor_demand[, `:=`(
  ln_emp_act_2008 = log(emp_act_2008), emp_act_2008 = NULL,
  ln_emp_act_2019 = log(emp_act_2019), emp_act_2019 = NULL,
  did = appendLeadingZeros(did)
)]
labor_demand[, dln_emp_act := ln_emp_act_2019 - ln_emp_act_2008]
setnames(labor_demand, c("bartik_loot", "bartik_loog"), c("bartik", "bartik_1"))

main_wide = merge(main_wide, labor_demand, "did") |>
  merge(districts, by = "did")
setcolorder(main_wide, c("did", "name"))
fwrite(main_wide, "data/processed/main/main_wide.csv")

## house keeping ---
rm(
  hstock, permit_completion, pop, gdp, income, rent, changes, main, homes, HPI,
  desc_stats, controls, districts, in_cols, keys
)
gc()
