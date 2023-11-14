export = FALSE

# INKAR ---------------------------------------------------------

# translate the column names of data frames downloaded from
# INKAR, `table` has de vs en pairs of words.

translate_colnames <- function(x, .table) {
  .table = read.csv("data/raw/INKAR/column-names.csv")
  nms = if (is.data.frame(x)) names(x) else x
  trimmed <- tolower(trimws(nms))
  .df = data.frame(from = nms, to = with(.table, en[match(trimmed, tolower(de))]))
  within(.df, {
    to = ifelse(is.na(to), to, to)
  })
}

# read in the second sheet of xls files downloaded from inkar
read_metadata_xls <-
  function(path, sheet = "Metadaten", skip = 1, assign = FALSE, name_repair = "minimal", ...) {
    if (assign) {
      name <- sub("[.].*", "", basename(path))
      name <- gsub("[ ()-]", "_", tolower(name))
      assign(name, readxl::read_xls(path, sheet = sheet, skip = skip, ...), envir = globalenv())
    } else {
      readxl::read_xls(path, sheet = sheet, skip = skip, .name_repair = name_repair, ...)
    }
  }


# translate metadata from German(de) to English(en)
# dependencies: googleLanguageR and cld2

translate_metadata <- function(x, from = "de", to = "en", exclude_cols = "Quelle", detect.from = FALSE) {
  oldnms <- names(x)
  exclude <- x[, (oldnms %in% exclude_cols)] # we will append it after translation
  x <- x[, !(oldnms %in% exclude_cols)]
  oldnms <- names(x)

  if (detect.from) {
    .source <- unlist(lapply(x, function(txt) {
      unique(na.omit(cld2::detect_language(txt, lang_code = TRUE)))
    }))

    if (all(.source == to)) {
      message(sprintf("lang code `%s` is detected in `%s`, the supplied source = `%s`.\n", .source, oldnms, from))
    }
  }

  if (interactive())
    ans <- readline("Do you want to proceed with the translation? [y/n]|> ")
  if (tolower(ans) == "y") {
    trans <- Map(googleLanguageR::gl_translate,
      t_string = x, source = from, target = to
    )
    trans <- lapply(trans, rev) # source txt then target txt
    trans <- as.data.frame(do.call("cbind", trans))
    names(trans) <- sub("[.]text", "", names(trans))
    names(trans) <- sub("[.]translatedText", paste0("_", to), names(trans))
    trans <- data.frame(trans, exclude)
  } else {
    message("No translation has been made.")
    return(data.frame())
  }
  trans
}

# make metadata names once after they are translated
# What it does is: indikator indikator_en -> indikator indicator
make_metadata_names <- function(x) {
  .table = read.csv("data/raw/INKAR/column-names.csv")
  nms = if (is.data.frame(x)) names(x) else x
  .which = grepl("_en$", nms)
  nms[.which] = translate_colnames(sub("_en", "", nms[.which]), .table)$to
  sub(" ", "_", nms)
}
