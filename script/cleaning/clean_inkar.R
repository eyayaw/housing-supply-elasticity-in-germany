library("tidyr")
library("readr")
source("script/translate.R")
# real_names <- function(df) {
#   colnames(df)[!grepl("\\d+", colnames(df))]
# }

# avoid overwriting existing files
if_doesnot_exist_write <- function(.fun, x, path, ...) {
  if (file.exists(path)) {
    message("File exists: `", path, "`\n")
    ans <- readline("Do you wanna overwrite it? [y/n] ")
    if (tolower(ans) == "y") {
      .fun(x, path, ...)
    } else if (tolower(ans) == "n") {
      message("You chose not to overwrite the existing file, out of caution, we chose to save it under new name.")
      .fun(x, sprintf(
        "%s-%s%s",
        sub("[.][a-zA-Z]{3}$", "", path),
        Sys.time(),
        sub("(.*)([.][a-zA-Z]{3}$)", "\\2", path), ...
      ))
    } else {
      stop(sprintf("Cannot proceed because your choice (`%s`) is not either `y` or ` n`", ans), call. = FALSE)
    }
  } else {
    .fun(x, path, ...)
  }
}

read_in <- function(file,
                    delim = ";",
                    skip = 0,
                    loc = locale(
                      decimal_mark = ",",
                      grouping_mark = "."
                    ), ...) {
  read_delim(file = file, delim = delim, locale = loc, skip = skip, ...)
}


# get the time series dim len of the variables
meta_info <- function(x) {
  freq <- table(nms <- sub("[_.]\\d+", "", names(x)))
  freq <- as.data.frame(freq)
  names(freq) <- c("var", "freq")
  # since `table` annoyingly orders the result
  freq$var <- factor(freq$var, levels = unique(nms))
  freq <- freq[order(freq$var), ]
  freq
}


correct_names <- function(x) {
  nms <- colnames(x)
  nms <- sub("[_.]\\d+", "", nms) # remove any digit following a . or _
  firstrow <- unlist(x[1, ]) # first row holds years
  firstrow <- ifelse(is.na(firstrow), "", paste0("YEAR", firstrow))
  paste0(nms, firstrow)
}

# varying for reshape
# freq = from meta_info, the freq of vars
# a0 = from meta_info the len of vars that are ids
construct_varying <- function(freq, a0) {
  to <- vector("list", length(freq))
  for (i in seq_along(to)) {
    to[[i]] <- a0 + sum(freq[seq_len(i)])
  }
  from <- lapply(seq_along(to), function(i) {
    if (i == 1) {
      return(a0 + 1)
    } else {
      to[[i - 1]] + 1
    }
  })
  Map(seq, from, to)
}

clean_separate <- function(x) {
  static.vars = grepl("([Ee]ntwicklung)|([Vv]eränderung)", names(x))
  if (any(static.vars)) {
    message(
      paste0(names(x)[static.vars], collapse = "\n"),
      " are dropped. We suspect they are static or hard to combine with the rest of the vars."
    )
  }
  x = x[, !static.vars]
  good.names <- correct_names(x)
  meta <- meta_info(x)
  # nms <- as.character(meta[which(meta$freq > 1), "var"])
  idvar <- c("Kennziffer", "Raumeinheit", "Aggregat")
  stopifnot(all(idvar %in% meta$var))
  vary <- meta[!(meta$var %in% idvar), "freq"]
  vary <- construct_varying(vary, length(idvar))
  # times = range(na.omit(as.integer(x[1, ])))
  x <- x[-1, ]
  x <- setNames(x, good.names)
  # reshape(x,
  #         direction = 'long',
  #         varying = vary,
  #         idvar = idvar,
  #         timevar = "year",
  #         times = seq(times[[1]], times[[2]]),
  #         v.names = nms)
  pivot_longer(x, cols = unlist(vary)) %>%
    separate(col = name, into = c("var", "year"), sep = "YEAR") # %>%
  # pivot_wider(names_from = "var", values_from = "value")
}

# nest the data by var and then write each var to disk
nest_write_each <- function(x, dir = NULL) {
  df <- tidyr::nest(x, data = -var)
  by = vapply(df[["data"]], function(x) x[["Aggregat"]][[1]], "")
  by = gsub("\\s|[.]", "", by)
  varname = gsub("/", "-or-", trimws(df[["var"]])) # / is dir sep must be removed
  path <- sprintf("%s/%s-by-%s.csv", dir, varname, by)
  lapply(1:nrow(df), function(i) {
    if_doesnot_exist_write(write_csv, df$data[[i]], path[[i]])
  })
}

# read in the .csv file with `read_in`, `clean_separate` and `nest_write_each`
## then read in the metadata from the .xls (sheet = Metadaten),
## translate it
## write it to a file with name Meta-*
## append the first two rows of meta_trans to columns-list.csv
## write each var to separated/*
readin_clean_nestwrite <- function(path) {
  .dir <- dirname(path)
  xls.path <- sub("[.][A-Za-z]{3}$", ".xls", path)
  meta.path <- sprintf("%s/Metadata-%s", .dir, basename(path))
  if (file.exists(meta.path)) {
    message("File exists: `", meta.path, "`\n")
    message("No need to translate the metadata. It seems it already is.\n")
    meta_trans = read_csv(meta.path)
  } else {
    meta_trans <- translate_metadata(read_metadata_xls(xls.path))
    names(meta_trans) <- make_metadata_names(meta_trans)
    write_csv(meta_trans, meta.path)
  }
  .table <- read_csv("data/raw/INKAR/column-names.csv")
  if (any(newones <- !(meta_trans$Indikator %in% .table$de))) {
    write_csv(with(meta_trans[newones, ], data.frame(de = Indikator, en = Indicator)),
      "data/raw/INKAR/column-names.csv",
      append = TRUE
    )
  }

  .table <- read_csv("data/raw/INKAR/column-names.csv")
  if (!dir.exists(new.dir <- paste0(.dir, "/separated"))) {
    dir.create(new.dir)
  }
  df_cleaned <- clean_separate(read_in(path))
  nest_write_each(df_cleaned, new.dir)
  if_doesnot_exist_write(
    write_tsv,
    translate_colnames(nest(df_cleaned, data = -var)$var, .table),
    paste0(new.dir, "/readme.txt")
  )
}

# Kreise reference
Kreise_codes <- readxl::read_xlsx("data/raw/INKAR/Referenz Gemeinden-GVB-Kreise_NUTS.xlsx", sheet = "KRS")

names(Kreise_codes)[names(Kreise_codes) == "...12"] <- "ktyp4_label"
Kreise_codes <- Kreise_codes[-1, c(
  "krs17", "krs17name", "ksitz", "kslk", "kreg17",
  "kreg17name", "st_kreg", "kslk_kreg", "ktyp4",
  "ktyp4_label"
)]

labels <- c(
  "Kreise2017", "Kreise2017", "Sitz der Kreisverwaltung",
  "Kreisfreie Stadt=1/Landkreis=2 - Ebene Kreise", "Kreisregion", "Kreisregion",
  "Status der Kreisregion (1=ja 2=nein)", "Kreisfreie Stadt=1/Landkreis=2 - Ebene Kreisregion",
  "siedlungsstruktureller Kreistyp 2017", "siedlungsstruktureller Kreistyp 2017"
)

attr(Kreise_codes, "label") = labels
