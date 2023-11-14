library(readxl)
library(googleLanguageR)
library(readr)

# helpers ----------------------------------------------------------------------
get_year <- function(x) {
  sub("^(.*)((19)|(20)\\d{2})$", "\\2", x)
}

# gives us dir + path
full_path <- function(file.name, dir) paste0(dir, file.name)

# removes file ext from basename
file.stem <- function(path) {
  tools::file_path_sans_ext(basename(path))
}
# makes tidy names
clean_name <- function(name) {
  gsub("[-. ]", "_", trimws(name))
}


read_assign <- function(file.name, fun, ..., env = globalenv()) {
  nm <- paste0("df_", clean_name(file.stem(file.name)))
  assign(nm, fun(file.name, ...), envir = env)
}


## tidying  helpers ------------------------------------------------------------
# takes out values of a variable which are combined together with other values # in a single column
# example

mock <- tibble::tribble(
  ~id, ~name, ~var, ~val,
  "2019", NA, NA, NA,
  "DE", "Germany", "gdp", 80,
  NA, NA, "pop", 80,
  NA, NA, "emp", 30,
  "01001", "Flensburg", "gdp", 1.5,
  NA, NA, "pop", 1.1,
  NA, NA, "emp", 0.5,
  "01002", "Kiel", "gdp", 1.3,
  NA, NA,  "pop", 2,
  NA, NA, "emp", 1,
  "2018", NA, NA, NA,
  "DE", "Germany", "gdp", 70,
  NA, NA, "pop", 81,
  NA, NA, "emp", 31,
  "01001", "Flensburg", "gdp", 2,
  NA, NA, "pop", 2,
  NA, NA, "emp", 1,
  "01002", "Kiel", "gdp", 1,
  NA, NA,  "pop", 2,
  NA, NA, "emp", 1,
)

# what this function does is take  `year` vals out of column 1 and
# put it in a separate column.
# f(df, 'id', '^((19)|(20))\\d{2}$', 'year')

tidy_packedrows <- function(df, col, pattern, newname, drop = TRUE) {
  if (is.character(col)) {
    stopifnot(`unknown column name` = col %in% names(df))
    pos <- match(col, names(df))
  } else if (is.numeric(col)) {
    stopifnot(`unknown column num` = col %in% seq_along(df))
    pos <- col
  }
  # drop footnote lines at the bottom of the excel file
  discard <- which(grepl("_{3,}", df[[pos]]))
  if (length(discard) != 0) {
    df <- df[1:(discard - 1), ]
  }

  ii <- which(grepl(pattern, df[[pos]], perl = TRUE)) # row indices of of the values
  if (length(ii) == 0) {
    stop(sprintf("No match found in col `%s` with pattern = `%s`", col, pattern),
      call. = FALSE
    )
  }
  # message(sprintf("(%s) -> ", paste(ii, collapse = ", ")),
  #       sprintf("(%s)", paste((ii - seq_along(ii)) + 1, collapse = ", ")))
  reps <- diff(ii) # construct replications, for each element of ii
  reps[length(reps) + 1] <- nrow(df) - sum(reps) # add the rep of the last instance
  value <- unlist(Map(rep, df[[pos]][ii], reps))
  if (missing(newname)) newname <- deparse1(substitute(col))
  df <- setNames(data.frame(df, value), c(names(df), newname))
  if (drop) {
    df <- df[-ii, ] # remove the old instances
  }
  # relocate the new var from the last col to just after `col`
  nms <- names(df)
  newpos <- match(c(nms[pos], newname), nms)
  df[, c(nms[newpos], setdiff(nms, nms[newpos]))]
}

# tidy the data.frame if there are multiple vars stacked in one column,
# i.e., in 'Var'. `df` is the same passed to `tidy_packedrows(...)`
# the example data frame is what we typically want to tidy with this function.
# f(df, 'id', '^((19)|(20))\\d{2}$', 'year', 'var')

tidy_packedrows_with_a_mult_vars_col <-
  function(df, col, pattern, newname, Var = NULL) {
    nms = names(df)
    if (is.character(col)) {
      stopifnot(`unknown column name` = col %in% nms)
      pos <- match(col, nms)
    } else if (is.numeric(col)) {
      stopifnot(`unknown column num` = col %in% seq_along(df))
      pos <- col
    }
    # drop footnote lines at the bottom of the excel file
    discard <- which(grepl("_{3,}", df[[pos]]))
    if (length(discard) != 0) {
      df <- df[1:(discard - 1), ]
    }

    ii <- which(grepl(pattern, df[[pos]], perl = TRUE)) # row indices of of the values
    if (length(ii) == 0) {
      stop(sprintf("No match found in col `%s` with pattern = `%s`", col, pattern),
        call. = FALSE
      )
    }

    reps <- diff(ii) # construct replications, for each element of ii
    reps[length(reps) + 1] <- nrow(df) - sum(reps) # add the rep of the last instance
    value <- unlist(Map(rep, df[[pos]][ii], reps))
    if (missing(newname)) newname <- deparse1(substitute(col))
    df <- setNames(data.frame(df, value), c(nms, newname))
    df <- df[-ii, ] # remove the old instances
    # relocate the new var from the last col to just after `col`
    nms <- names(df)
    newpos <- match(c(nms[pos], newname), nms)
    tidy_df <- df[, c(nms[newpos], setdiff(nms, nms[newpos]))]
    rm(df)

    # if there are multiple variables in the data set, then do more tidying
    if (!is.null(Var)) {
      if (!Var %in% names(tidy_df)) {
        stop(sprintf("Var = `%s` is not found", Var), call. = FALSE)
      }
      N <- nrow(tidy_df)
      len <- length(unique(tidy_df[[Var]]))
      main_ids <- which(!is.na(tidy_df$id))
      to_be_filled <- setdiff(seq_len(N), main_ids)
      n <- length(main_ids)
      gap <- diff(main_ids)
      gap <- c(gap, N - main_ids[n] + 1L) # append the gap of the last element of main_ids
      problematic <- main_ids[which(gap != len)]
      if (!all(gap == len)) {
        warning(
          "Unbalanced panel? The following rows seem to have fewer values: ",
          paste(problematic, collapse = ", "),
          call. = FALSE
        )
        message(sprintf("Expecting one unique length for all cases, but (`%s`).",
                        unique(gap)))
      }
      tidy_df["NEW_ID"] <- NA
      tidy_df["NEW_NAME"] <- NA
      newnms <- c("NEW_ID", "NEW_NAME")
      tidy_df[main_ids, newnms] <- tidy_df[main_ids, c("id", "name")]
      tidy_df[to_be_filled, "NEW_ID"] <-
        unlist(Map(function(x, t) rep(x, t - 1), tidy_df$id[main_ids], gap))
      tidy_df[to_be_filled, "NEW_NAME"] <-
        unlist(Map(function(x, t) rep(x, t - 1), tidy_df$name[main_ids], gap))

      tidy_df <- tidy_df[, setdiff(names(tidy_df), c("id", "name"))]
      tidy_df <- tidy_df[c(newnms, setdiff(names(tidy_df), newnms))]
      tidy_df <- setNames(tidy_df, c("id", "name", names(tidy_df)[-c(1, 2)]))
    }
    tidy_df
  }



tidy_it <- function(dir,
                    file.name,
                    pattern_tidy_packedrows,
                    newname_tidypackedrows = "year",
                    write_it = TRUE,
                    na = c("-", ".", "...", "/", "x"),
                    skip = 1L,
                    Var = NULL, # if multiple vars in a col of the data set
                    ...) {
  # districts <- read_csv("data/processed/admin-areas/districts_bkg.csv",
  #                       col_select='did')
  xx <- read_excel(full_path(file.name, dir), skip = skip, na = na, ...)

  # xx <- xx[!duplicated(xx), ]

  nms <- names(xx)[-c(1:2)]
  # since translation will change the name of the df
  # we need the location of the supplied Var in the names of the data frame
  var.ix <- if (!is.null(Var)) {
    match(Var, names(xx))
  }

  trans.path <- sprintf(
    "%s-names-translated.csv",
    file.stem(file.name)
  )
  if (file.exists(full_path(trans.path, dir))) {
    names_trans <- read_csv(full_path(trans.path, dir))
  } else {
    names_trans <- gl_translate(nms, target = "en", source = "de")
    write_csv(names_trans, full_path(trans.path, dir))
  }

  names(xx)[-c(1, 2)] <- names_trans$translatedText
  if (length(var.ix) != 0) {
    Var <- names(xx)[var.ix]
  }


  if (!is.null(Var)) {
    xx <- tidy_packedrows_with_a_mult_vars_col(
      df = xx,
      col = "id", # or 1L
      pattern = pattern_tidy_packedrows,
      newname = newname_tidypackedrows,
      Var = Var
    )
  } else {
    xx <- tidy_packedrows(
      df = xx,
      col = "id", # or 1L
      pattern = pattern_tidy_packedrows,
      newname = newname_tidypackedrows
    )
  }


  xx[, newname_tidypackedrows] <- xx[[newname_tidypackedrows]]

  # xx <- merge(
  #   districts,
  #   xx,
  #   by.x = "did",
  #   by.y = "id",
  #   all.y = TRUE
  # )

  # xx <- with(xx, xx[order(did, year), ])

  # xx <- xx[!duplicated(xx), ]

  if (write_it) {
    write_csv(
      xx,
      full_path(
        file.name = paste0(file.stem(file.name), "-cleaned.csv"),
        dir = dir
      )
    )
  }
  xx
}

# cleaning ---------------------------------------------------------------------


# 33-Land-use ------------------
## 33111-Survey of area according to type of actual use -----------------
.dir <- "data/raw/Regional-Atlas/33-Land-Use/"
.file.name <- c("33111-01-01-4.xlsx", "33111-01-02-4.xlsx")

Map(.file.name,
  f = function(ff) {
    tidy_it(
      dir = .dir,
      file.name = ff,
      pattern_tidy_packedrows = "^\\d{2}[.]\\d{2}[.]\\d{4}$"
    )
  }
)



# 31-Building-and-Housing -----------------------

## 31-111-Statistics of building permits (Statistik der Baugenehmigungen) ----

# house keeping
rm(list = ls(
  pattern = "([.]dir.*)|([.]file.*)",
  all.names = TRUE
))

.dir <- "data/raw/Regional-Atlas/31-Building-and-Housing/31-111/"
.file.name <- list.files(
  path = .dir,
  pattern = "\\d{5}-\\d{2}-\\d{2}-\\d{1}[.]xlsx$"
)

Map(.file.name, list(NULL, NULL, NULL, "Gebäudearten"),
  f = function(ff, v) {
    tidy_it(
      dir = .dir,
      file.name = ff,
      pattern_tidy_packedrows = "^((19)|(20))\\d{2}$",
      Var = v
    )
  }
)


## 31-121-Statistics of construction completions (Statistik der Baufertigstellungen) -----

# house keeping
rm(list = ls(
  pattern = "(^[.]dir$)|(^[.]file[.]name$)",
  all.names = TRUE
))

.dir <- "data/raw/Regional-Atlas/31-Building-and-Housing/31-121/"
.file.name <- list.files(
  path = .dir,
  pattern = "^\\d{5}-\\d{2}-\\d{2}-\\d{1}(-B)?[.]xlsx$"
)

Map(.file.name, list(NULL, NULL, NULL, "Gebäudearten"),
  f = function(ff, v) {
    tidy_it(
      dir = .dir,
      file.name = ff,
      pattern_tidy_packedrows = "^((19)|(20))\\d{2}$",
      Var = v
    )
  }
)




## 31-231-update d. Residential u. Housing stock (Fortschreib. d. Wohngebäude- u. Wohnungsbestandes) -----

# house keeping
rm(list = ls(
  pattern = "(^[.]dir$)|(^[.]file[.]name$)",
  all.names = TRUE
))

.dir <- "data/raw/Regional-Atlas/31-Building-and-Housing/31-231/"
.file.name <- list.files(
  path = .dir,
  pattern = "^\\d{5}-\\d{2}-\\d{2}-\\d{1}[.]xlsx$"
)

Map(.file.name,
  f = function(ff) {
    tidy_it(
      dir = .dir,
      file.name = ff,
      pattern_tidy_packedrows = "\\d{2}[.]\\d{2}[.]\\d{4}"
    )
  }
)


# 82-National accounts of the federal states (Volkswirtschaftliche Gesamtrechnungen der Länder) ----------------------

## 82-111-National accounts of the federal states: production calculation (VGR der Länder: Entstehungsrechnung) ------------

# house keeping
rm(list = ls(
  pattern = "(^[.]dir$)|(^[.]file[.]name$)",
  all.names = TRUE
))

.dir <- "data/raw/Regional-Atlas/82-National-Accounts/82-111/"
.file.name <- list.files(
  path = .dir,
  pattern = "^\\d{5}-\\d{2}-\\d{2}-\\d{1}[.]xlsx$"
)

Map(.file.name,
  f = function(ff) {
    tidy_it(
      dir = .dir,
      file.name = ff,
      pattern_tidy_packedrows = "^((19)|(20))\\d{2}$"
    )
  }
)

## 82-411-National accounts of the federal states: redistribution calculation (VGR der Länder: Umverteilungsrechnung) ---------

# house keeping
rm(list = ls(
  pattern = "(^[.]dir$)|(^[.]file[.]name$)",
  all.names = TRUE
))

.dir <- "data/raw/Regional-Atlas/82-National-Accounts/82-411/"
.file.name <- list.files(
  path = .dir,
  pattern = "^\\d{5}-\\d{2}-\\d{2}-\\d{1}[.]xlsx$"
)

Map(.file.name,
  f = function(ff) {
    tidy_it(
      dir = .dir,
      file.name = ff,
      pattern_tidy_packedrows = "^((19)|(20))\\d{2}$"
    )
  }
)





# 61-Prices (Preise) ------------------------------------------------------

## 61-511-Statistics of purchase values for building land (Statistik der Kaufwerte für Bauland) ----------


# house keeping
rm(list = ls(
  pattern = "(^[.]dir$)|(^[.]file[.]name$)",
  all.names = TRUE
))

.dir <- "data/raw/Regional-Atlas/61-Prices/61-511/"
.file.name <- list.files(
  path = .dir,
  pattern = "^\\d{5}-\\d{2}-\\d{2}-\\d{1}[.]xlsx$"
)

Map(.file.name, "type",
  f = function(ff, v) {
    tidy_it(
      dir = .dir,
      file.name = ff,
      pattern_tidy_packedrows = "^((19)|(20))\\d{2}$",
      Var = v
    )
  }
)





# 11-Territory (Gebiet) ---------------------------------------------------

# house keeping
rm(list = ls(
  pattern = "(^[.]dir$)|(^[.]file[.]name$)",
  all.names = TRUE
))

.dir <- "data/raw/Regional-Atlas/11-Territory/"
.file.name <- list.files(
  path = .dir,
  pattern = "^\\d{5}-\\d{2}-\\d{2}-\\d{1}[.]xlsx$"
)

Map(.file.name,
  f = function(ff) {
    tidy_it(
      dir = .dir,
      file.name = ff,
      pattern_tidy_packedrows = "\\d{2}[.]\\d{2}[.]\\d{4}"
    )
  }
)




# 12-Population (Bevölkerung) -----------------------------------------------

# house keeping
rm(list = ls(
  pattern = "(^[.]dir$)|(^[.]file[.]name$)",
  all.names = TRUE
))

.dir <- "data/raw/Regional-Atlas/12-Population//"
.file.name <- list.files(
  path = .dir,
  pattern = "^\\d{5}-\\d{2}-\\d{2}-\\d{1}[.]xlsx$"
)

Map(.file.name,
  f = function(ff) {
    tidy_it(
      dir = .dir,
      file.name = ff,
      pattern_tidy_packedrows = "\\d{2}[.]\\d{2}[.]\\d{4}",
      Var = "Altersgruppe (unter 3 bis 75 u.m.)"
    )
  }
)

# 13-Labor market (Arbeitsmarkt) ----------------------------------------------

# house keeping
rm(list = ls(
  pattern = "(^[.]dir$)|(^[.]file[.]name$)",
  all.names = TRUE
))

.dir <- "data/raw/Regional-Atlas/13-Labor-Market/"
.file.name <- list.files(
  path = .dir,
  pattern = "^\\d{5}-\\d{2}-\\d{2}-\\d{1}[.]xlsx$"
)

Map(.file.name,
  f = function(ff) {
    tidy_it(
      dir = .dir,
      file.name = ff,
      pattern_tidy_packedrows = "\\d{2}[.]\\d{2}[.]\\d{4}"
    )
  }
)
