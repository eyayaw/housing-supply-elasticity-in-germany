#### <--------------------------------------------------------------------> ####
library(data.table)
library(ivreg)
library(texreg)
source("script/helpers/estimation_helpers.R")

# robust standard errors, p values
het <- function(obj, type = "HC1", ...) {
  se = diag(sandwich::vcovHC(obj, type = type, ...))^.5
  df = df.residual(obj)
  .coef = coef(obj)
  tval = .coef / se
  pval = 2 * pt(abs(tval), df = df, lower.tail = FALSE)
  list(se = se, pval = pval)
}

my_texreg <- function(l, ..., caption = NULL, digits = 3, fontsize = "normalsize", float.pos = "H") {
  texreg(l,
    caption = sprintf("\\textbf{%s}", caption),
    caption.above = TRUE,
    stars = c(1, 5, 10) / 100,
    include.adjrs = FALSE,
    digits = digits,
    fontsize = fontsize,
    float.pos = float.pos,
    booktabs = TRUE,
    threeparttable = TRUE,
    use.packages = FALSE,
    ...
  )
}
#### <--------------------------------------------------------------------> ####

# import data ----
main = fread("data/processed/main/model-data.csv")
construction_emp = fread(
  "data/processed/main/regional-employment_by-industry_inc-self-employment.csv"
) |>
  subset(
    year %in% c(2008, 2019) & sector == "Construction",
    select = c("did", "year", "value")
  ) |>
  reshape(
    direction = "wide", idvar = "did", timevar = "year", v.names = "value",
    sep = "_"
  )


main = merge(main,
  construction_emp[, .(
    did, ln_cemp_2008 = log(value_2008), ln_cemp_2019 = log(value_2019)
    )], "did"
)
main[, dln_cemp := ln_cemp_2019 - ln_cemp_2008]
main[, west_east := factor(west_east, c("East", "West"))]
main[, urban_rural := factor(urban_rural_area, c("Rural", "Urban"))]
main[, ln_pop_den := log(pop_2008 / (1e-6 * area_2008))]
main[, unavail_frac := unavail_frac_2006][, dev_frac := dev_frac_2006]
incols = sprintf("%s_%d", c("pop", "pland", "hhinc"), 2008)
main[, sprintf("ln_%s", incols) := lapply(.SD, log), .SDcols = incols]
main[, ln_ccost := log(cost_euro_sqm_2019)]

# IV estimation ----

## all homes -------
# dependent variable: `dln_floorspace`

## case 1 ----
mod1 = ivreg(
  dln_floorspace ~ dln_hpi + dev_frac + unavail_frac + urban_rural + west_east
  + ln_ccost + ln_cemp_2008 | . + ln_hhinc_2008 + ln_pop_2008
    - dln_hpi + bartik,
  data = main
)

## case 2 ----
mod2 = ivreg(
  dln_floorspace ~ dln_hpi + dln_hpi:dev_frac + unavail_frac + urban_rural + west_east
    + ln_ccost + ln_cemp_2008 | . + ln_hhinc_2008 + ln_pop_2008
    - dln_hpi - dln_hpi:dev_frac
    + bartik + bartik:dev_frac,
  data = main
)

## case 3 ----
mod3 = ivreg(
  dln_floorspace ~ dln_hpi + dln_hpi:unavail_frac + dev_frac + urban_rural + west_east
    + ln_ccost + ln_cemp_2008 | . + ln_hhinc_2008 + ln_pop_2008
    - dln_hpi - dln_hpi:unavail_frac
    + bartik + bartik:unavail_frac,
  data = main
)

## case 4 ----
mod4 = ivreg(
  dln_floorspace ~ dln_hpi + dln_hpi:dev_frac + dln_hpi:unavail_frac + urban_rural + west_east
    + ln_ccost + ln_cemp_2008 | . + ln_hhinc_2008 + ln_pop_2008
    - dln_hpi - dln_hpi:dev_frac - dln_hpi:unavail_frac
    + bartik + bartik:dev_frac + bartik:unavail_frac,
  data = main
)


## for generating tables -----
coef.nms = list(
  "dln_hpi" = "$\\Delta\\ln P$",
  "dln_hpi:dev_frac" = "$\\Delta\\ln P\\times{\\text{Developed}}$",
  "dln_hpi:unavail_frac" = "$\\Delta\\ln P\\times{\\text{Unavail}}$",
  "dev_frac" = "Developed",
  "unavail_frac" = "Unavail",
  # "west_eastWest" = "West",
  # "urban_ruralUrban" = "Urban",
  # "ln_ccost" = "$\\ln \\text{Constr. costs}$",
  # "ln_cemp_2008" = "$\\ln \\text{Constr. labor}$",
  "(Intercept)" = "Constant"
)

custom.note = "Regressions include the log of construction costs in 2019, the log of construction labor in 2008, urban vs. rural, and west vs. east district classifications as defined by BBSR (2021), and the log of population and household income in 2008. The fraction of land developed (Developed) and unavailable (Unavail) are for 2006. Robust standard errors are in parentheses."
custom.note = sprintf("\\item %%stars. \\\\ \\textit{Notes:} %s", custom.note)


### iterate over dependent variables
deps = c(
  Floorspace = "dln_floorspace",
  Units = "dln_total_buildings",
  Permits = "dln_permits_total_buildings",
  Completions = "dln_completions_total_buildings"
)

mods = list(mod1, mod2, mod3, mod4)

out = vector("list", length(deps))
names(out) = names(deps)
mods_all = out
mods_all$Floorspace = mods
for (dep in deps[-1]) {
  mods_all[[names(deps)[deps == dep]]] <-
    lapply(mods, \(m) update(m, as.formula(paste(dep, " ~ ."))))
}

for (i in seq_along(deps)) {
  my_texreg(mods_all[[i]],
    custom.coef.map = coef.nms,
    omit.coef = setdiff(getUniqueCoefNames(mods_all[[i]]), coef.nms),
    override.se = lapply(mods_all[[i]], \(m) het(m)$se),
    override.pvalues = lapply(mods_all[[i]], \(m) het(m)$pval),
    custom.model.names = sprintf("(%d)", seq_along(mods_all[[i]])),
    custom.header = setNames(
      list(seq_along(mods_all[[i]])),
      sprintf("Housing Supply Growth: $\\Delta\\ln(H)$ (%s)", names(deps)[[i]])
    ),
    caption = sprintf("IV Results: Housing Supply Elasticity Estimates (%s)", names(deps)[[i]]),
    custom.note = custom.note,
    no.margin = TRUE
  ) -> out[[i]]
  write(out[[i]], sprintf("output/iv-main-results_%s_checked.tex", names(deps)[[i]]))
}

all.coef.nms = getUniqueCoefNames(mods)
### wide table showing floorspace and units main results ----
main_results = with(mods_all, c(Floorspace, Units))
nmods = length(main_results)
tab_main_results = my_texreg(main_results,
  custom.coef.map = coef.nms,
  override.se = lapply(main_results, \(m) het(m)$se),
  override.pvalues = lapply(main_results, \(m) het(m)$pval),
  omit.coef = setdiff(all.coef.nms, coef.nms),
  custom.header = setNames(list(seq_len(nmods/2), (nmods/2 + 1):nmods), bold(c("Floorspace", "Units"))),
  custom.model.names = paste0("(", rep(seq_len(nmods/2), 2), ")"),
  caption = "IV Results: Housing Supply Elasticity Estimates",
  label = "tab:iv-main-results-checked",
  custom.note = custom.note,
  fontsize = "footnotesize",
  no.margin = TRUE
  # sideways = TRUE,float.pos = "hbtp!"
)

write(tab_main_results, "output/iv-main-results_checked.tex")

## collect estimates ----
coefs = mods_all |>
  lapply(
    \(m) rbindlist(lapply(m, \(x) as.list(coef(x))), idcol = "case", fill = TRUE)
  ) |>
  rbindlist(use.names = TRUE, idcol = "qmeasure")
coefs[, case := paste0("case.", case)]

fwrite(coefs, "output/elasticity-estimates_checked.csv")

## single-family homes -----

# renaming is a trick: for brevity of avoiding replacing `dln_hpi` by `dln_hpi_single_fam`
## everywhere in the formula
main[, dln_hpi_backup := dln_hpi
     ][, dln_hpi := dln_hpi_single_fam]
mods_single_floorspace = lapply(
  list(mod1, mod2, mod3, mod4),
  \(m) update(m, dln_single_fam_floorspace ~ .)
)

# run the model for units, permits, and completions
deps_single = c(
  Floorspace = "dln_single_fam_floorspace",
  Units = "dln_single_fam",
  Permits = "dln_single_fam_permits",
  Completions = "dln_single_fam_completions"
)

mods_single = list(Floorspace = mods_single_floorspace)
for (dep in deps_single[-1]) {
  mods_single[[dep]] <-
    lapply(mods_single_floorspace, \(m) update(m, as.formula(paste(dep, " ~ ."))))
}
rm(mods_single_floorspace)
names(mods_single) = names(deps_single)

out_single = vector("list", length(mods_single))
names(out_single) = names(deps_single)

for (i in seq_along(deps_single)) {
  my_texreg(mods_single[[i]],
    custom.coef.map = coef.nms,
    omit.coef = setdiff(getUniqueCoefNames(mods_single[[i]]), coef.nms),
    override.se = lapply(mods_single[[i]], \(m) het(m)$se),
    override.pvalues = lapply(mods_single[[i]], \(m) het(m)$pval),
    custom.model.names = sprintf("(%d)", seq_along(mods_single[[i]])),
    custom.header = setNames(
      list(seq_along(mods_single[[i]])),
      sprintf("Housing Supply Growth: $\\Delta\\ln(H)$ (%s--single-family)", names(deps_single)[[i]])
    ),
    caption = "IV Results: Housing Supply Elasticity Estimates",
    custom.note = custom.note,
    label = sprintf("tab:iv-results-%s--single-family-checked", names(deps_single)[[i]]),
    fontsize = "scriptsize"
  ) -> out_single[[i]]
  write(
    out_single[[i]],
    sprintf("output/iv-results_%s--single-family_checked.tex", names(deps_single)[[i]])
  )
}

## collect estimates ----
coefs_single = mods_single |>
  lapply(
    \(m) rbindlist(lapply(m, \(x) as.list(coef(x))), idcol = "case", fill = TRUE)
  ) |>
  rbindlist(use.names = TRUE, idcol = "qmeasure")
coefs_single[, case := paste0("case.", case)]

fwrite(coefs_single, "output/elasticity-estimates-single-family_checked.csv")

### wide table showing floorspace and units results ----
results = with(mods_single, c(Floorspace, Units))
nmods = length(results)
tab_results = my_texreg(results,
  custom.coef.map = coef.nms,
  override.se = lapply(results, \(m) het(m)$se),
  override.pvalues = lapply(results, \(m) het(m)$pval),
  omit.coef = setdiff(all.coef.nms, coef.nms),
  custom.header = setNames(list(seq_len(nmods/2L), (nmods/2+1):nmods), bold(c("Floorspace", "Units"))),
  custom.model.names = paste0("(", rep(seq_len(nmods/2), 2), ")"),
  caption = "IV Results: Housing Supply Elasticity Estimates (single-family homes)",
  label = "tab:iv-results-single-family-checked",
  custom.note = custom.note,
  fontsize = "footnotesize",
  no.margin = TRUE
)

write(tab_results, "output/iv-results_single-family_checked.tex")

main[, dln_hpi := dln_hpi_backup
     ][, dln_hpi_backup := NULL] # bring back real value

# first-stage ------------------------------------------------

fstage = lm(
  dln_hpi ~ bartik + ln_pop_2008 + ln_hhinc_2008 + urban_rural + west_east, main
)

fstage = list(all = fstage, single = update(fstage, dln_hpi_single_fam ~ .))

coef.nms = vmake.label(getUniqueCoefNames(fstage))
coef.nms = sub("_{2008}", "", coef.nms, fixed = TRUE)
if ("(Intercept)" %in% names(coef.nms)) {
  coef.nms[["(Intercept)"]] = "Constant"
  # coef.nms = c(coef.nms[-1L], coef.nms[1])
  loc = match("(Intercept)", names(coef.nms))
  coef.nms = c(coef.nms[-loc], coef.nms[loc])
}

custom.note = "The dependent variable is change in the log of house prices (of all-type, including single-family homes) in the first column, and of only single-family homes in the second column. Regressions include urban vs. rural and west vs. east district classifications as defined by BBSR (2021), the log of population and household income in 2008. Robust standard errors are in parentheses."
custom.note = sprintf("\\item %%stars. \\\\ \\textit{Notes:} %s", custom.note)

my_texreg(fstage,
  custom.coef.map = as.list(coef.nms),
  override.se = lapply(fstage, \(m) het(m)$se),
  override.pvalues = lapply(fstage, \(m) het(m)$pval),
  custom.header = setNames(list(1, 2), bold(c("All homes", "Single-family homes"))),
  custom.model.names = paste0("(", seq_along(fstage), ")"),
  caption = "First-stage Results",
  label = "tab:first-stage-results-checked",
  include.fstatistic = TRUE,
  custom.note = custom.note,
  fontsize = "footnotesize"
  # no.margin = TRUE
  # sideways = TRUE
) -> tab_fs_results

write(tab_fs_results, "output/first-stage-results_checked.tex")


# OLS -----------------------

ols_all = lm(
  dln_floorspace ~ dln_hpi + dev_frac + unavail_frac + ln_ccost + ln_cemp_2008 + ln_hhinc_2008 + ln_pop_2008 + urban_rural + west_east, main
)

ols_all = list(
  Floorspace = ols_all,
  Units = update(ols_all, dln_total_buildings ~ .),
  Permits = update(ols_all, dln_permits_total_buildings ~ .),
  Completions = update(ols_all, dln_completions_total_buildings ~ .)
)

ols_single = lm(dln_single_fam_floorspace ~ dln_hpi_single_fam + dev_frac +
  unavail_frac + urban_rural + west_east + ln_pop_2008 +
  ln_ccost + ln_cemp_2008 + ln_hhinc_2008, main)

ols_single = list(
  Floorspace = ols_single,
  Units = update(ols_single, dln_single_fam ~ .),
  Permits = update(ols_single, dln_single_fam_permits ~ .),
  Completions = update(ols_single, dln_single_fam_completions ~ .)
)


custom.note = "The dependent variables are changes in the log of total residential floorspace, buildings, permits, and completions. Regressions include the log of construction costs in 2019, the log of construction labor in 2008, urban vs. rural, and west vs. east district classifications as defined by BBSR (2021), and the log of population and household income in 2008. The fraction of land developed (Developed) and unavailable (Unavail) are for 2006. Robust standard errors are in parentheses."
custom.note = sprintf("\\item %%stars. \\\\ \\textit{Notes:} %s", custom.note)

ols = list(all = ols_all, "single-family" = ols_single)

ols_out = vector("list", length(ols))
names(ols_out) = names(ols)
for (i in seq_along(ols_out)) {
  coef.nms = vmake.label(getUniqueCoefNames(ols[[i]]))
  coef.nms = sub("_{2008}", "", coef.nms, fixed = TRUE)
  coef.nms = coef.nms[!(names(coef.nms) %in% sprintf("ln_%s_2008", c('pop', 'pland', 'hhinc')))]
  if ("(Intercept)" %in% names(coef.nms)) {
    coef.nms[["(Intercept)"]] = "Constant"
    loc = match("(Intercept)", names(coef.nms))
    coef.nms = c(coef.nms[-loc], coef.nms[loc])
  }

  my_texreg(ols[[i]],
    custom.coef.map = as.list(coef.nms),
    omit.coef = setdiff(getUniqueCoefNames(ols[[i]]), coef.nms),
    override.se = lapply(ols[[i]], \(m) het(m)$se),
    override.pvalues = lapply(ols[[i]], \(m) het(m)$pval),
    custom.model.names = sprintf("(%d)", seq_along(ols[[i]])),
    custom.header = setNames(as.list(seq_along(ols[[i]])), names(ols[[i]])),
    caption = "OLS Results: Housing Supply Elasticity Estimates",
    custom.note = if (names(ols_out)[[i]]=="single-family") sub("completions", "completions, all for single-family homes", custom.note) else custom.note,
    label = sprintf("tab:ols-results--%s-checked", names(ols)[[i]]),
    fontsize = "footnotesize"
  ) -> ols_out[[i]]
  write(ols_out[[i]], sprintf("output/OLS-results_%s_checked.tex", names(ols)[[i]]))
}

## collect estimates ----
coefs_ols = list(all = ols_all, single = ols_single) |>
  lapply(\(h) rbindlist(lapply(h, \(m) as.list(coef(m))), idcol = "dep_var")) |>
  rbindlist(use.names = TRUE, fill = TRUE, idcol = "home_type")

fwrite(coefs_ols, "output/elasticity-estimates-ols_checked.csv")
