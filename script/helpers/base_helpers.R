## Helper base-R Functions ----

# It’s useful as a way of providing a default value in case the output of
# another function is NULL:
# http://adv-r.had.co.nz/Functions.html#special-calls
`%||%` <- function(lhs, rhs) if (!is.null(lhs)) lhs else rhs

# safely compare two numeric (floating) vectors, taken from dplyr::near
near <- function (x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

# base select, for use in pipeOp
bselect = `[.data.frame`


# a practical cut
my_cut <- function(x, ..., include.lowest = TRUE, right = FALSE) {
  cut(x, ..., include.lowest = include.lowest, right = right)
}

# taken from adv-r 1st ed http://adv-r.had.co.nz/Functions.html#return-values
in_dir <- function(dir, code) {
  old = setwd(dir)
  on.exit(setwd(old))
  force(code)
}


# always round 0.5 up, taken from the internet
.round <- function(x, digits=0) {
  posneg = sign(x)
  z = abs(x) * 10^digits
  z = z + 0.5
  z = trunc(z)
  z = z / 10^digits
  z * posneg
}


# get the stem of a file (basename of a file without the ext)
# e.g. data/foo.csv -> foo
file.stem <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

# Non-overwriting download operation
download_file <- function(url, destfile, ...) {
  if (!file.exists(destfile)) {
    download.file(url, destfile, ...)
  } else {
    warning('File already exists. Download operation skipped.', call.=FALSE)
  }
}


# grep returns value
grepv <- function(pattern, x, ...) {
  grep(pattern = pattern, x = x, value = TRUE, ...)
}


# makes tidy names
make_names <- function(names) {
  names = gsub("[ ]{2,}", " ", names) # rm two or more white spaces anywhere
  patt = "[[:punct:] ]+"
  names = gsub(patt, "_", trimws(tolower(names)))
  names = gsub(patt, "_", names)
  gsub("(^_)|(_$)", "", names)
}


# rearrange columns in a data.frame
col_reorder <- function(x, col = NULL) {
  stopifnot(inherits(x, "data.frame"))
  cols = seq_len(ncol(x))
  col = switch(class(col),
    "character" = match(col, names(x)),
    "numeric" = if (all(col %in% cols)) col else stop(call. = FALSE)
  )
  x[, c(col, cols[-col])]
}


# # simplify to data.frame --------------------------------------------------
# ## mimicking purrr's style
#
# bind_rows <- function(.l, ...) {
#   as.data.frame(do.call(rbind, .l, ...))
# }
#
# bind_cols <- function(.l, ...) {
#   as.data.frame(do.call(cbind, .l, ...))
# }
#
# map_dfr <- function(l, f, ...) {
#   as.data.frame(do.call(rbind, lapply(l, f, ...)))
# }
#
# map_dfc <- function(l, f, ...) {
#   as.data.frame(do.call(cbind, lapply(l, f, ...)))
# }
#
# pmap_dfr <- function(..., f) {
#   as.data.frame(do.call(rbind, Map(f, ...)))
# }
#
# pmap_dfc <- function(..., f) {
#   as.data.frame(do.call(cbind, Map(f, ...)))
# }


# reshaping -----------
# reshape_wide <- function(data, ...) {
#   reshape(data, ..., direction = 'wide')
# }
#
# reshape_long <- function(data, ...) {
#   reshape(data, ..., direction = 'long')
# }
