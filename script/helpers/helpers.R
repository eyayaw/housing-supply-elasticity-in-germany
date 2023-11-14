# miscellaneous functions

fread_utf8 <- function(...) data.table::fread(..., encoding = "UTF-8")
fread_keepzeros <- function(file, ..., keepLeadingZeros = TRUE) {
  data.table::fread(file, ..., keepLeadingZeros = keepLeadingZeros)
}
# make did 5-digit long
appendLeadingZeros <- function(x) {
  ifelse(grepl("^[1-9][0-9]{3}$", x), paste0("0", x), x)
}

# create dir on the fly if needed for a file path
ensure_dir <- function(path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  path
}

# the golden ratio
gold = (1 + 5^0.5) / 2

is.outlier <- function(x, cutoff = 4, ...) {
  m = mean(x, ...)
  s = sd(x, ...)
  (x < m - cutoff * s) | (x > m + cutoff * s)
}

# growth rate
grate <- function(x, n = 1) {
  (x / data.table::shift(x, n = n)) - 1
}

round1 <- function(x) {
  round(x, digits = 1L)
}
round2 <- function(x) {
  round(x, digits = 2L)
}
round3 <- function(x) {
  round(x, digits = 3L)
}

# round + format
formatNum <- function(x, digits = 0L, nsmall = digits, ...) {
  format(round(x, digits = digits), big.mark = ",", trim = TRUE, nsmall = nsmall, ...)
}


# dln(x) = lnx_1 - lnx_0 = ln(x_1/x_0)
# g = x_1/x_0 - 1
ldiff2grate <- function(x, percent = FALSE) {
  grate = exp(x) - 1
  if (percent) {
    return(grate * 100)
  }
  grate
}


## landcover/constraints helpers

# set a unit of x to
m2 <- function(x) units::set_units(x, m^2) # meter square (m^2)
unity <- function(x) units::set_units(x, 1) # unity (e.g. m^2/m^2 = 1 [1])

# get factor id (in categorical raster r) given the clc code
get_id <- function(r, code) {
  Cats = cats(r)[[1]]
  names(Cats) = tolower(names(Cats))
  names(Cats)[grep("^code_", names(Cats))] = "code"
  Cats[match(as.integer(code), as.integer(Cats$code)), ]$id
}

# inverse of get_id
get_code <- function(r, id) {
  Cats = cats(r)[[1]]
  names(Cats) = tolower(names(Cats))
  names(Cats)[grep("^code_", names(Cats))] = "code"
  Cats[match(as.integer(id), as.integer(Cats$id)), ]$code
}

## convert radians to slope percent, https://geogra.uah.es/patxi/gisweb/DEMModule/DEM_T_Sl.htm
rad2perc = \(r) tan(r) * 100
rad2deg = \(r) 180 * r / pi
deg2rad = \(d) (d * pi) / 180



# translate German var names into English (predefined in `table`)
translate_names <- function(var_de, table = NULL) {
  if (is.data.frame(var_de)) {
    stop("Expecting a character vector, not a data.frame!
         Perhaps, you want to translate the names of `var_de`?", call. = FALSE)
  }

  if (is.null(table)) {
    source("script/cleaning/variable_list.R", local = TRUE)
    table = selected_vars[, c("var_de", "var_en")]
  }
  idx = match(var_de, table$var_de)
  out = table$var_en[idx]
  not_translated = which(is.na(out))
  out[not_translated] = var_de[not_translated]
  out
}

translate_mon <- function(monat) {
  mons = c(
    "Januar", "Februar", "März", "April", "Mai", "Juni",
    "Juli", "August", "September", "Oktober", "November", "Dezember"
  )
  abb = c(
    "Jan", "Feb", "März", "Apr", "Mai", "Juni",
    "Juli", "Aug", "Sept", "Okt", "Nov", "Dez"
  )
  if (all(nchar(setdiff(monat, "Mai")) >= 4L)) {
    out = month.name[match(tolower(monat), tolower(mons))]
  } else {
    out = month.name[match(tolower(monat), tolower(abb))]
  }
  out[which(is.na(out))] = monat[which(is.na(out))]
  out
}

# tidy packed rows, i.e. columns with multiple variable.
# For example, year and month in one column: 2020 Feb Jan ... 2019 Feb Jan ...
sep_rows <- function(x, col, pattern) {
  stopifnot(col %in% names(x))
  parent_rows = which(grepl(pattern, x[[col]]))
  parents = x[[col]][parent_rows]
  nchild_rows = diff(parent_rows)
  nchild_rows[length(nchild_rows) + 1] = nrow(x) - tail(parent_rows, 1) + 1
  child_rows = unlist(Map(function(x, n) rep(x, n), parents, nchild_rows), use.names = FALSE)
  newname = paste0(col, "_", "sep")
  x[[newname]] = child_rows
  if (inherits(x, "data.table")) {
    x[, c(col, newname, setdiff(names(x), c(col, newname))), with = FALSE]
  } else {
    x[, c(col, newname, setdiff(names(x), c(col, newname)))]
  }
}


# count cases per categories of cols
freq <- function(df, cols) {
  stopifnot("`df` needs to be a `data.table`." = is.data.table(df))
  types = vapply(df[, ..cols], class, character(1L))
  facs = cols[types %in% c("factor", "integer")]
  chars = cols[types %in% "character"]
  ignored = setdiff(cols, union(facs, chars))
  if (length(ignored) != 0) {
    warning("Ignoring non factor and/or character vars: ",
      paste0("`", ignored, "`", collapse = ", "),
      call. = FALSE
    )
  }
  cols = union(facs, chars)
  lapply(cols, function(x) df[, .N, by = x])
}
