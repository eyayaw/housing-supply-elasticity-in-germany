library(data.table)
source("script/helpers/helpers.R")
source("script/translate_atlas.R")

# prepend to file names the dir name of regional atlas
atlas_path <- function(x) {
  sprintf("data/raw/Regional-Atlas/%s", x)
}


# get the main variables (measures) from the data set
get_measure_vars = function(x, translated = F) {
  if (is.data.frame(x)) {
    message("You passed a data frame, `evaluating names(x)` instead.")
    x = names(x)
  }
  pattern = "^([0-9]_[a-z]+[_])((code)|(label))$"
  if (translated) {
    pattern = "^((expression)|(feature))_((code)|(label))_\\d+$"
  }
  # identify which ones are variable labels or codes
  m = grepl(pattern, x, ignore.case = T)
  if (all(!m)) {
    message("Column names don't follow this pattern: ", sprintf(r"{'`%s`'}", pattern))
    return(x)
    }

  label_and_code = x[m]
  last_non_main = tail(label_and_code, 1L) # pick the last element

  if (!(last_non_main %in% x)) {
    stop("Fail to know where the `non main vars` end.", call. = FALSE)
  }
  x[(1 + match(last_non_main, x)):length(x)]
}

# replace the two German words and make clean names thereafter
tidy_names = function(x, lower_case = TRUE) {
  if (is.data.frame(x)) x = names(x)
  p = c("Auspraegung", "Merkmal")
  r = c("Expression", "Feature")
  for (i in seq_along(p))
    x = sub(p[[i]], r[[i]], x, ignore.case = TRUE)
  x = sub("^(\\d+)[_. ]*(.*)", "\\2_\\1", x)
  x = gsub("[ .-]+", "_", x)
  if (lower_case) x = tolower(x)
  x
}


# tidy regional atlas data sets
# path: path to the flat csv file
# destfile: a path to save the tidied file? will be implemented later

# tidy regional atlas data sets  ------------------------------------------------

tidy_atlas <- function(
    path, drop = NULL, dec = ",", header = T, return_agg = F,
    encoding = "Latin-1", na.strings = c("-", ".", "...", "/", "x"), ...
    ) {

  if (!grepl("[_]flat[.]csv$", path)) {
    stop(
      "This function accepts the `_flat.csv` version of the regional atlas data set.",
      call. = FALSE
    )
  }

  if (missing(drop)) {
  drop <- c(
    "Statistik_Code", "Statistik_Label", "Zeit_Code",
    "Zeit_Label", "1_Merkmal_Code", "1_Merkmal_Label"
    )
  }

  atlas <- fread(path,
  drop = drop, na.strings = na.strings, dec = dec, encoding = encoding,
  header = header, colClasses = list("character" = "1_Auspraegung_Code"),
  ...
  )
  # for completeness: since "DG" is in `Expression_Code_1` it'll be `character` class


  # tidying and renaming columns
  new_nms = c(year = "Zeit", did = "Expression_Code_1", name = "Expression_Label_1")
  setnames(atlas, tidy_names(names(atlas), FALSE))
  setnames(atlas, new_nms, names(new_nms))

  # identify variables that are categorical, and drop them if they include only one
  # label
  cats_vars = names(atlas)[grepl("((Feature)|(Expression))", names(atlas), ignore.case = T)]
  cats = lapply(atlas[, cats_vars, with=F], unique)
  cats_len = vapply(cats, length, 1)
  on.exit(setattr(atlas, "labels", cats)) # if needed for checking later
  # not relevant, since they include only one category/label
  one_cats = cats_vars[cats_len==1]
  if (length(one_cats))
    atlas[, (one_cats) := NULL]

  main_vars <- get_measure_vars(names(atlas), T) # the quantitative vars we are after
  # save a bit of character translation by removing the stat code at the beginning of
  # the main vars
  main_vars_trans <-
    strsplit(main_vars, "__") |>
    vapply(function(x) {
      if (length(x) > 1) {
        paste0(x[1], "__", translate_atlas(paste0(x[-1], collapse = "__"))$to)
      } else {
        x
      }
    }, "")
  main_vars_trans = tidy_names(main_vars_trans)
  main_vars_trans = sub("^([a-z0-9]+)__([a-z0-9_]+)", "\\2", main_vars_trans)
  setnames(atlas, main_vars, main_vars_trans)
  setnames(atlas, tolower(names(atlas)))
  setkey(atlas, did, year) # sorting

  # translate the values of variables that include important texts
  ## iterate over variables that include "label" in their name --> llabels
  ## find the unique values of each variable --> measures
  ## translate them --> trans_measures
  ## replace the variable values by the translated values

  llabels <- names(atlas)[grepl("_label", names(atlas))]
  measures <- lapply(atlas[, ..llabels], unique)
  trans_measures <- lapply(measures, translate_atlas, underscore_rm = F)
  for (i in seq_along(llabels)) {
    for (j in seq_along(measures[[i]])) {
      atlas[measures[[i]][[j]],
        on = (llabels[[i]]),
        (llabels[[i]]) := trans_measures[[i]][["to"]][[j]]
      ]
    }
  }


  # if a column includes all NA, drop it.
  v <- get_measure_vars(names(atlas), translated=TRUE)
  all_na <- vapply(atlas[, v, with = F], \(x) all(is.na(x)), logical(1L))
  v <- v[all_na] # all na vars
  n <- length(v)
  if (n > 0L) {
    atlas[, (v) := NULL]
    message(sprintf(ngettext(
      n, "%d variable's been dropped b/c it includes all missing.",
      "%d variables've been dropped b/c they include all missing."
    ), n))
    message(sprintf("\nDropped vars:\n--------------\n%s", paste0(v, collapse = "\n")))
  }

  # values for states, and for Germany
  agg_rows = atlas[grepl("^(DG)$|^([0-9]{1,2})$", did), which=TRUE]
  if (return_agg) {
    atlas_agg = atlas[agg_rows, ]
  }

  # keep only data for the districts, to do so, right-join w/ `districts` data set,
  # but does not work b/c of territorial changes so we may need to keep all with all=T
  #
  ## City states Berlin and Hamburg need care,
  ## Bremen is fine (data comes at each admin level for it)
  city_state_rows = atlas[did %like% "(^11$)|(^02$)", which=TRUE]
  city_states = atlas[city_state_rows, paste0(did, "000")] # make them districts
  # rm agg values and append newly created city-states (with 5 digit ids)
  atlas = list(
    atlas[-agg_rows, ], data.table(did=city_states, atlas[city_state_rows, !"did"])
    ) |>
    rbindlist(use.names=TRUE)
  # let's keep only districts, their ids are 5 digit long

  atlas = atlas[nchar(did) >=4 & nchar(did) <= 5, ]
  # if did is integer, we can do, did<17000 (since there are 16 states)

  # rm Berlin districts b/c we have Berlin---the city state, itself as district
  # atlas = atlas[!grepl("^11(?!000)\\d+", did, perl = T), ]
  atlas = atlas[!grepl("^Berlin[-].+$", name), !"name"]  # delete name col too
  # atlas = merge(atlas, districts, 'did', all=T)
  setcolorder(atlas, c("did", "year"))
  if (return_agg) {
    list(atlas, atlas_agg[, !"name"])
  } else {
    atlas
  }
}


# for cleaning after calling tidy_atlas helpers --------------------------------

# reorder columns--- take col to the end
put_last <- function(x, col) {
  stopifnot(is.data.frame(x) && all(col %in% names(x)))
  c(setdiff(names(x), col), col)
}


## get variables names that should be kept
# these are variables that do not include labels and codes,
# exception being the last expression_label_* variable
# keep_code = T additionally keeps expression_code_*
keep_vars <- function(x, keep_code = F) {
  if (is.data.frame(x)) x <- names(x)
  patt <- "(expression_[a-z_]+)|(feature_[a-z_]+)"
  exlb <- x[grepl(patt, x)]
  mx <- max(as.numeric(regmatches(exlb, regexpr("\\d$", exlb))))
  exlb <- if (keep_code) {
    exlb[!grepl(sprintf("expression_((label)|(code))_%s", mx), exlb)]
  } else {
    exlb[!grepl(sprintf("expression_label_%s", mx), exlb)]
  }
  setdiff(x, exlb)
}

