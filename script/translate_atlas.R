# Regional Atlas ----------------------------------------------------------
# colnames and additional info translation for data sets from regional atlas
# requires google cloud translation api services

get_dict <- function(path_dict = "data/dictionary.txt") {
  if (file.exists(path_dict)) {
    return(read.table("data/dictionary.txt", header = T, row.names = NULL))
  }
  dict = list.files(
    "data/raw/Regional-Atlas/", pattern = "names-translated[.]csv$",
    recursive = T, full.names = T
  ) |>
    lapply(read.csv, header = T) |>
    (\(x) do.call("rbind", x))() |>
    rev() |>
    setNames(c('from', 'to'))

  write.table(dict, path_dict, row.names = F)
  dict
}


# translate a text vector in German to English
translate_atlas = function(x, underscore_rm = T, add_to_dict = T) {
  if (is.data.frame(x)) {
    stop("Expecting a character vector, not a data.frame!
         Perhaps, you want to translate the names of `x`?", call. = F)
  }
  if (underscore_rm) {
    x = gsub("_", " ", x)
  }

  is_translated = function(term) {
    if (tolower(term) %in% tolower(dict$from)) T else F
  }
  dict = get_dict()

  already_translated = vapply(x, is_translated, logical(1))

  res = data.frame(from = x,  to = x)
  res[already_translated, ] = dict[match(tolower(x[already_translated]), tolower(dict$from)), ]

  if (!requireNamespace("googleLanguageR", quietly = T) || is.null(googleLanguageR::gl_auto_auth())) {
    warning(
      "Either {{googleLanguageR}} is not installed, or the google translation api services have not been set up..", call. = F
      )
    return(res)
  }

# continues with translations for untranslated ones if the api is set up correctly
  if (any(!already_translated)) {
    for (i in which(!already_translated)) {
      # no need to translate i+1 if it is the same as i when translated
      res[i, ] = if (is_translated(x[[i]])) {
        dict[match(tolower(x[[i]]), tolower(dict$from)), ]
      } else {
        # gtranslate return the trans in 1st col,i.e. `translatedText`
        rev(googleLanguageR::gl_translate(x[[i]], target = "en", source = "de"))
      }
      dict = rbind.data.frame(dict, res[i, ])
    }
    if (add_to_dict) {
      write.table(res[!already_translated, ],
        "data/dictionary.txt",
        append = add_to_dict,
        row.names = F,
        col.names = F
      )
      message("Updated the dictionary.\n")
    }
  }

  res
}
