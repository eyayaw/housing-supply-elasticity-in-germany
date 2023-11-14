library(data.table)

# employment data ----
# regional employment data by industry over time
regional = fread(
  "data/processed/main/regional-employment_by-industry_inc-self-employment.csv",
  key = c("did", "sector", "year")
)

# national employment by industry over time
# in the absence of it, we could construct it from the regional data,
# i.e., aggregate over districts
nat = fread(
  "data/processed/main/national-employment_by-industry_inc-self-employment.csv",
  key = c("sector", "year")
)


## params
sectors = unique(regional$sector) |>
  {
    \(.x) data.frame(sector = .x, label = paste0("k_", seq_along(.x)))
  }()
K = nrow(sectors) # number of industries (should be 7)
base_year = 2008L # some initial period (base year)
end_year = min(2019L, max(regional$year))


# wrangling ----
regional[, sector := factor(sector, sectors$sector, sectors$label)]
nat[, sector := factor(sector, sectors$sector, sectors$label)]

# fill NAs, if any
regional[, value := nafill(value, "locf"), .(did, sector)
         ][, value := nafill(value, "nocb"), .(did, sector)]
# keep from base year on
regional = regional[year >= base_year, ]
nat = nat[year >= base_year, ]
# exclude the construction sector?
# regional = regional[sector != sectors["Construction" == sectors$sector,]$label, ]

# leave-one-out total -----------------------------------------------------
## loot = the nation-wide employment in industry k excluding the district i
## refer Baum-snow & Han 2019
regional[, share := value / sum(value), .(did, year)]

regional[, loot := sum(value) - value, .(sector, year)]
regional[year %in% c(base_year, end_year), ] |>
  reshape(
    direction = "wide", idvar = c("did", "sector"),
    timevar = "year", v.names = c("value", "share", "loot"), sep = "_"
  ) -> regional_wide


b_loot = regional_wide[, .(
  bartik_loot = sum(share_2008 * log(loot_2019 / loot_2008)),
  # bartik_loot = sum(value_2008 * (loot_2019 / loot_2008)),
  emp_act_2008 = sum(value_2008),
  emp_act_2019 = sum(value_2019)
), did]

## alternatively following GPSS 2020
regional[, loog := loot / shift(loot) - 1, .(did, sector)]
regional[, share_base := share[year == base_year], .(did, sector)]
regional[, value_base := value[year == base_year], .(did, sector)]

regional[, .(
  bartik_loog = sum(share_base * loog),
  # bartik_loog = sum(value_base * (loog + 1)),
  emp_act = sum(value)
), .(did, year)] -> b_loog

bartik_loo = merge(b_loot, b_loog[year == end_year, .(did, bartik_loog)])
setcolorder(bartik_loo, "bartik_loot", before = "bartik_loog")

fwrite(bartik_loo, "data/processed/main/bartik-shocks.csv")
