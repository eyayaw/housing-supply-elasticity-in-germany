library(data.table)

## deal with territorial inconsistencies -----
# missing data b/c of changes of territorial definitions
##

## when did territorial definitions go into effect?
went_to_effect =
  readxl::read_excel(
    "data/raw/admin-areas/vg250_12-31.utm32s.shape.kompakt/dokumentation/struktur_und_attribute_vg250.xls", "VG250"
  ) |>
  subset(ADE == 4L, select = c("AGS", "GEN", "WSK")) |>
  setNames(c("did", "name", "date")) |>
  # transform(did = as.integer(did)) |>
  as.data.table()

## Referenzschlüssel Kreise von 1990 bis 2019
fpath = "data/raw/ref-kreise-umrech-2019-1990-2018.xlsx"
s = 1990
e = 2019
sheets = readxl::excel_sheets(fpath)
sheets = sub("\\d{4}$", e, sheets)
ref = lapply(sheets, \(x) {
  yr = as.integer(strsplit(x, "-")[[1]])
  # c('Kreise',"Kreisname", "flächen", "bevölkerungs", "Kreise", "kreisname")
  nms = c("did_s", "name_s", "area_prop", "pop_prop", "did_e", "name_e")
  readxl::read_excel(fpath, x) |>
    (\(d) d[, grep("(kreis)|((fl.chen|bev.lkerungs)[-].*proportionaler.*)",
      names(d),
      ignore.case = TRUE
    )])() |>
    setNames(nms) |>
    # subset(did_s != did_e) |>
    within({
      year = yr[[1]]
      # rm 3 trailing zeros
      did_s = did_s / 1000
      did_e = did_e / 1000
      did_s = ifelse(nchar(did_s) == 4L, paste0("0", did_s), did_s)
      did_e = ifelse(nchar(did_e) == 4L, paste0("0", did_e), did_e)
    })
}) |>
  do.call(what = rbind) |>
  as.data.table()

# those districts (parent) into which at least one district (child) is merged
if (FALSE) {
  merged_districts =
    aggregate(did_s ~ did_e + year, ref, \(x) paste(x, collapse = "+")) |>
    setNames(c("parent", "year", "child")) |>
    merge(went_to_effect[, .(did, date)], by.x = "parent", by.y = "did") |>
    as.data.table()
}

# did is read as integer by fread
districts = fread("data/processed/admin-areas/districts_destasis.csv",
  key = "did", select = c("did" = "character", "name" = "character")
)

# we need `districts`, and `ref` data.tables called in the environment
# districts will be used to keep only districts under the 2019 territorial definition
make_consistent <- function(df, col, fun, ..., use_2009_for_2008 = TRUE) {
  df = copy(df)[, c("did", "year", col), with = FALSE]
  setkey(df, did, year)

  # what if the correction makes 2008 value so distorted?
  # this is the first period of the study. Take the 2009 value instead?
  if (use_2009_for_2008) {
    df[year %in% c(2008, 2009), x := nafill(x, "nocb"), did, env = list(x = col)]
  }

  # sanity checks
  stopifnot(length(col) == length(fun)) # not vectorized over args
  stopifnot(all(
    c(typeof(df$did), typeof(districts$did), typeof(ref$did_s), typeof(ref$did_e)) %in% "character"
  ))

  stopifnot(all(c(mode(df$year), mode(ref$year)) %in% "numeric"))

  ref = copy(ref)

  ## districts that do not exist anymore under the 2019 territorial definitions
  # we will make use of the population and area weights
  merged = ref[!districts, on = "did_s==did", .(did_s, did_e, year, area_prop, pop_prop)]

  # data values for those districts, up until they're dissolved (or have NA after that)
  children_vals = df[merged, on = c("did==did_s", "year")]

  # parents (did_e): districts which we try to make consistent data for
  # data values for these parents including the ones to be corrected or not
  affected = df[merged[, .(did = unique(did_e))], on = "did", ]

  # the date (year) in which the territorial change went into effect
  # when=affected[ , .(did=unique(did))][went_to_effect[, .(did, year = year(date))], on='did', nomatch=0L]
  when = merged[, .(cyear = max(year) + 1L), .(did = did_e)]
  # not ok values: values (potentially all NA in these years) that we are after
  before = affected[when[, .(did, cyear)], on = "did"][year < cyear, !"cyear"]
  # ok values: value once the changes went into effect
  after = affected[when[, .(did, cyear)], on = "did"][year >= cyear, !"cyear"]
  rm(affected, merged)
  # make sure the NAs are b/c of territorial changes not data unavailability
  # append data values to `after` that aren't NA, though they're `before territorial change` values
  after = rbindlist(list(before[!is.na(x), env = list(x = col)], after), use.names = TRUE)
  before = before[is.na(x), env = list(x = col)] # really due to the change

  # get the values (of dissolved districts)
  children_vals = children_vals[before[, .(did_e = did, year)], on = c("did_e", "year")]
  # formula: `fun` can be a summary function such as `sum` or `mean` depending on the
  # nature of the variable col`. We can not sum prices up, so we may rather use mean
  # in such cases. We can sum up population size or houses.
  fun = match.fun(fun)
  out = children_vals[, .(v = fun(0.5 * (area_prop + pop_prop) * v, ...)), .(did = did_e, year), env = list(v = col)]
  # bring values together, before (constructed) and after (untouched): now the data for the districts are made consistent
  out = rbindlist(list(out, after), use.names = TRUE)
  # add those districts that do not need correction
  out = rbindlist(list(df[!out[, .(did, year)], on = c("did", "year")], out), use.names = TRUE)
  # removes those dissolved (we called children)
  merge(districts[, .(did, name)], out, "did")
}

# vectorize over `col` and `fn`
vmake_consistent <- function(df, cols, fns, ...) {
  Reduce(
    \(x, y) merge(x, y, c("did", "year", "name")),
    Map(\(x, f) make_consistent(df, x, f, ...), cols, fns)
  )
}
