library(data.table)
states = fread("data/processed/admin-areas/states.csv")

# construction costs ------------------------------------------------------
ccosts = readxl::read_excel(
  "data/raw/statista construction costs by states 2019.xlsx",
  sheet = "Daten", col_names = c("state_name", "cost_euro_sqm_2019"), skip = 4
)

cpi = read.csv("data/processed/consumer-price-index_base-year-2015.csv") |>
  subset(year == 2019, select = "cpi") |> `[.data.frame`(, "cpi")
# convert to 2015 prices
ccosts$cost_euro_sqm_2019 = ccosts$cost_euro_sqm_2019 / (cpi / 100)
ccosts = merge(states[, !"state_abb"], ccosts, "state_name")
setkey(ccosts, state_code)

fwrite(ccosts, "data/processed/main/statista_construction-costs-by-states_2019.csv")

## abandoned because there is no bpi for all states
# ## baupreisindex, since 2010
# bpi = fread(
#   "data/processed/main/construction-costs-index.csv",
#   select = c("state_code", "year", "cpi_ave")
# )
#
# bpi[, `:=`(cpi_2019=100*cpi_ave/cpi_ave[year==2019]), state_code]
#
# ccosts = merge(bpi, ccosts[, !"state_name"], "state_code")
# ccosts[, ccosts:=cpi_2019*cost_euro_sqm_2019
#    ][, ccosts:=ccosts/cpi_ave[year==2015], state_code]
# ccosts[, `:=`(cpi_2019=NULL, cost_euro_sqm_2019=NULL)]
#
# # complete cases
# comp = with(ccosts, CJ(state_code = unique(state_code), year=unique(year)))
# absent = comp[!ccosts[, .(state_code, year)], on = .(state_code, year)]
# ccosts = rbind(ccosts, absent, fill = TRUE)
#
# ccosts[, c("cpi_ave", "ccosts") := lapply(.SD, nafill, type="locf"), state_code, .SDcols=c("cpi_ave", "ccosts")]
