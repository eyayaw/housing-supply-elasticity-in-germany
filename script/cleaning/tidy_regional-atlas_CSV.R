library(data.table)
source("script/cleaning/territorial_changes.R")

na.strings = c("-", ".", "...", "/", "x")
encoding = "Latin-1"
dec = ","
sep = ";"

tidy_atlas_csv <-
  function(path, skip, header = FALSE, return_districts_only = TRUE, ...) {
  if (missing(skip)) {
    skip = grep(
      r"{^(?P<day>0[1-9]|[12][0-9]|3[01])(?P<delimiter>[- /.])(?P<month>0[1-9]|1[012])\2(?P<year>(?:19|20)\d\d)|(?P<Year>(?:19|20)\d\d);DG;Deutschland;}",
      readLines(path, encoding = encoding, n = 20L), perl = TRUE
      )[[1]] - 1L
    if (header) skip = skip - 1L
  }

  out = fread(path,
    header = header, skip = skip, sep = sep, encoding = encoding,
    dec = dec, na.strings = na.strings, ...
  )
  names(out)[1:3] = c("year", "did", "name")
  out[, year :=
        as.integer(sub(r"{^(?:(\d\d[/.-]\d\d[/.-])?(\d{4}))$}", "\\2", year, perl = TRUE))
      ]
  if (return_districts_only) {
    out[name %in% c("Hamburg", "Berlin"), did := paste0(did, "000")]
    out[nchar(did) == 5L, ]
  } else {
    out
  }
}

# fixes NA that should be explicitly zero or filled with total-(sum of the parts)
fix_NA_parts_from_total <- function(x, total_var, part_vars) {
  incols = c(total_var, part_vars)
  stopifnot(all(incols %in% names(x)))

  where = x[, apply(.SD, 1L, \(x) sum(is.na(x))) == 1, .SDcols = incols]
  message(sum(where), " fixable found.")
  if (sum(where) == 0) {
    return(x)
  }
  fixable = x[where, incols, with = FALSE]
  fixable[, incols] <-
    fixable[,
    transpose(apply(.SD, 1L, \(.x) replace(.x, is.na(.x), .x[[1]] - sum(na.omit(.x[-1]))), simplify = F)),
    .SDcols = incols
    ]
  x = copy(x)
  x[where, (incols) := fixable]
  x
}


# permits and completions -----

## permits -----
permits = tidy_atlas_csv("data/raw/Regional-Atlas/31-Building-and-Housing/31-111/31111-01-02-4.csv")

percomp_nms = c(
  "total_buildings", "1-apart-building", "2-apart-building",
  "3ormore-apart-building", "total_apartments", "1-apart-apartment",
  "2-apart-apartment", "3ormore-apart-apartment", "floorspace_1000sqm"
)

setnames(permits, -c(1,2,3), paste0("permits_", percomp_nms))

permits[, permits_floorspace:=permits_floorspace_1000sqm * 1000][,permits_floorspace_1000sqm:=NULL]

## completions -----

completions = tidy_atlas_csv("data/raw/Regional-Atlas/31-Building-and-Housing/31-121/31121-01-02-4.csv")

setnames(completions, -c(1, 2, 3),paste0("completions_", percomp_nms))

completions[,completions_floorspace:=completions_floorspace_1000sqm * 1000][, completions_floorspace_1000sqm:=NULL]

### make consistent -----
part_build = c(
  "total_buildings", "1-apart-building", "2-apart-building", "3ormore-apart-building"
  )
part_apart = c(
  "total_apartments","1-apart-apartment", "2-apart-apartment", "3ormore-apart-apartment"
  )


pc = list(permits=permits, completions=completions)
pc = Map(pc, names(pc),
         f = \(.x, .n) fix_NA_parts_from_total(
           .x,
          sprintf("%s_%s", .n, part_build[1]),
          sprintf("%s_%s", .n, part_build[-1])
          ) |>
  fix_NA_parts_from_total(
    sprintf("%s_%s", .n, part_apart[1]),
    sprintf("%s_%s", .n, part_apart[-1]))
  )

pc = lapply(pc, \(x) vmake_consistent(x, names(x)[-(1:3)], 'sum', na.rm=TRUE))

## writing ----
fwrite(pc$permits, "data/processed/main/permits.csv")
fwrite(pc$completions, "data/processed/main/completions.csv")

rm(permits, completions, percomp_nms, part_apart, part_build)


# stock: residential and non-residential ----
fpaths = sprintf(
  "data/raw/Regional-Atlas/31-Building-and-Housing/31-231/31231-%s.csv",
  c("01-02-4", "02-01-4")
)
names(fpaths) = c("before", "after")
fpaths = as.list(fpaths)


## before 2010: values for before 2011 -----

stock_before = tidy_atlas_csv(fpaths$before, select=1:7)
setnames(
  stock_before,
  c(
    "year", "did", "name", "total_buildings", "1-apart-building",
    "2-apart-building", "floorspace_1000sqm"
  )
)


## after 2010: values for 2011 and on wards ------
stock_after = tidy_atlas_csv(fpaths$after, select=1:9)
setnames(
  stock_after,
  c(
    "year", "did", "name", "total_buildings", "1-apart-building",
    "2-apart-building", "3ormore-apart-building", "dorm", "floorspace_1000sqm"
  )
)


stock_fspace = rbindlist(list(stock_after, stock_before), use.names=TRUE, fill=TRUE)

# fix NAs that should be explicit zero
stock_fspace = fix_NA_parts_from_total(
  stock_fspace, "total_buildings",
  c("1-apart-building", "2-apart-building", "3ormore-apart-building", "dorm")
)


stock_fspace[, `3ormore-apart+dorm-building`:= total_buildings-(`1-apart-building`+`2-apart-building`)]
stock_fspace[, `:=`(`3ormore-apart-building`=NULL, dorm=NULL)]
setcolorder(stock_fspace, "3ormore-apart+dorm-building", before="floorspace_1000sqm")

stock_fspace[, floorspace := 1000 * floorspace_1000sqm][, floorspace_1000sqm := NULL]

stock_fspace = vmake_consistent(
  stock_fspace,
  c("total_buildings", "1-apart-building", "2-apart-building", "3ormore-apart+dorm-building", "floorspace"),
  rep("sum", 5L), na.rm = TRUE
)

fwrite(stock_fspace, "data/processed/main/residential_stock-floorspace.csv")

rm(stock_before, stock_after, fpaths)
