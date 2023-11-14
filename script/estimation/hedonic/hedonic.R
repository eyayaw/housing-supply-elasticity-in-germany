library(fixest)
library(data.table)

# tidy fixed effects from fixest::fixef()
tidy_fixeff <- function(fixeff) {
  stopifnot(inherits(fixeff, "fixest.fixef"))
  fe = data.frame(id = names(fixeff[[1]]), eff = fixeff[[1]])
  fe[, c("did", "year")] = do.call("rbind", strsplit(fe$id, "_"))
  fe[, c("id", "did", "year", "eff")]
}

setDTthreads(getDTthreads() - 1L)

homes <- fread("data/processed/homes_ready.csv",
  drop = c(
    "unique_obid", "ad_begin_year", "kid2015", "parking_space", "lab_mrkt_reg",
    "parking_space_price", "ad_begin_mon", "state", "usable_floor_space", "renov_year_cat",
    "constr_year_cat"
  )
)

# factor variables
cats = c(
  "did", "year", "ad_end_mon", "num_bedrooms", "num_bathrooms", "num_floors",
  "house_type", "holiday_house", "heating_type", "basement", "guest_washroom",
  "constr_phase", "equipment", "condition"
)


# Estimation -------------------------------------------------------------------
homes[, (cats) := lapply(.SD, as.factor), .SDcols = cats]

# Estimate the fixed effects model with dummy variable estimator
part = "lnprice_sqm ~ 0 + dist2cbd + floor_space + plot_size + num_rooms +
num_floors + num_bedrooms + num_bathrooms + house_type + heating_type + age0 +
age1 + constr_phase + condition + equipment + basement + guest_washroom + holiday_house"

## all homes ----
### using `fixest` package ---------------------------------------------------------

form_feols_all = sprintf("%s | did^year", part) |> as.formula()

hedonic_all = feols(form_feols_all, homes, combine.quick = FALSE)
fe_all = fixef(hedonic_all) # extract fixed effects
gc()

## single-family homes -------
form_feols_single = sprintf("%s|did^year", sub("house_type\\s*\\+", "", part)) |>
  as.formula()

hedonic_single = feols(form_feols_single,
  homes[house_type == "single-family", !"house_type"], combine.quick = FALSE
)
fe_single = fixef(hedonic_single)

# write to disk
dir_write = "data/processed/hedonic/"
fwrite(tidy_fixeff(fe_all),
  file.path(dir_write, "fe-fixest_all-homes.csv")
)
fwrite(tidy_fixeff(fe_single),
  file.path(dir_write, "fe-fixest_single-family-homes.csv")
)

# export regression output
etable(hedonic_all, hedonic_single,
  depvar = FALSE,
  headers = c("All homes", "Single-family homes"),
  tex = TRUE,
  title = "Hedonic Price Index",
  label = "hedonic",
  file = file.path("hedonic-output_fixest.tex"),
  style.df = style.df("aer"),
  replace = TRUE
)
