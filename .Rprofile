source("renv/activate.R")

options(
  prompt = "R> ",
  continue = ". ",
  max.print = 1000L,
  max = 10L,
  example.ask = TRUE,
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE,
  repos = c(rspm = "https://packagemanager.rstudio.com/all/latest")
)

# data.table options
options(
  datatable.print.class = TRUE,
  datatable.print.keys = TRUE,
  datatable.print.nrows = TRUE,
  datatable.warnredundantby = TRUE,
  datatable.print.topn = FALSE,
  datatable.print.trunc.cols = TRUE,
  datatable.print.trunc.cols = TRUE,
  datatable.print.rownames = TRUE,
  datatable.print.colnames = "auto"
)


local({
  # taken from https://github.com/tidyverse/modelr/blob/main/R/na-warn.R
  na.warn <- function(object) {
    missing <- sum(!stats::complete.cases(object))
    if (missing > 0) {
      warning("Dropping ", missing, " rows with missing values", call. = FALSE)
    }
    stats::na.exclude(object)
  }

  options(na.action = na.warn)
})

# a styler that does not replace = by <-
.eq_assign_style <- function(...) {
  transformers = styler::tidyverse_style(...)
  transformers$token$force_assignment_op = NULL
  transformers
}

options(
  styler.addins_style_transformer = ".eq_assign_style()",
  styler.quiet = TRUE,
  styler.cache_root = "styler"
)
