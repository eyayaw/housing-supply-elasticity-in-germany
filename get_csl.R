# A helper function to download a style file.
# You may want to change the `citation_package` option to, for example, `natbib`, for the "bst" to work.

get_style_file <- function(style, ext, base_url) {
  style_file = paste0(tools::file_path_sans_ext(style), ext)

  if (!file.exists(style_file)) {
    url = file.path(base_url, style_file)
    tryCatch(
      download.file(url, basename(url)),
      error = function(e) {
        message = paste("Failed to download style file from ", url)
        stop(message)
      }
    )
  }

  return(style_file)
}

get_csl <- function(style = "apa-no-ampersand", bst = F) {
  if ((missing(style) || style == "") && bst) {
    stop("You should provide a non-empty `bst` style name.", call. = F)
  }

  if (bst) {
    bst_url_base = "https://www.ctan.org/tex-archive/biblio/bibtex/contrib/economic"
    return(get_style_file(style, ".bst", bst_url_base))
  } else {
    csl_url_base = "https://raw.githubusercontent.com/citation-style-language/styles/master"
    return(get_style_file(style, ".csl", csl_url_base))
  }
}
