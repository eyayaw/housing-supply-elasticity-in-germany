source("script/helpers/helpers.R") # for translate_names()

# get value labels of a factor variable
## include_missing is to exclude -5 to -11 which are special missing values
get_value_labels <- function(var_name, include_missing = TRUE) {
  x <- LABELS[[var_name]]
  if (include_missing) {
    x
  } else if (!include_missing) {
    x[which(x$value > -1), ]
  }
}

# house purchases labels
ll = readLines(unz('data/raw/RWI-GEO-RED/2020/HiDrive.zip',
'Dokumentation/Labels/Labels_Immoscout_HK_en.txt'), 13015)[-c(1:6)]

index = which(ll == "") # each variable's info is separated by 'empty line' in the file
n = length(index)
out = vector("list", n)
s = 1
for (i in 1:n) {
  out[[i]] = c(s, index[[i]]-1)
  s = index[[i]] + 1
}

lbls = lapply(out, function(x) ll[x[[1]]:x[[2]]])
varnames = vapply(lbls, function(x) sub(".*\\((\\w+)\\)$", "\\1", x[[1]]), "")
names(lbls) = varnames

LABELS = lapply(varnames, function(var_de) {
  x = lbls[[var_de]]
  attribute = x[c(1,2)]
  x = trimws(x[-c(1,2)], 'left')
  n = length(x)
  x = strcapture("(^-?\\d{1,}) (.+)", x,
             data.frame(value=integer(n), label=character(n)))
  attr(x, 'info') <- attribute
  x
})
names(LABELS) = varnames
LABELS = Filter(function(x) nrow(x) > 0, LABELS) # filter out vars with no labels


# house keeping
rm(out, s, i, n, varnames, index)

# variable labels
 var_labs = lapply(lbls, `[[`, 1L)
 # rm `Variable: ` at the beginning of the string
 var_labs <- sub("Variable: ", "", var_labs)

  var_labs <- data.frame(
    name = sub(x = var_labs, "(.*)\\s(\\((.*)\\))$", "\\3", perl = TRUE),
    label = sub(x = var_labs, "(.*)\\s(\\(.*\\))$", "\\1", perl = TRUE)
  )

  # replace empty names or labels with the corresponding name or label.
  var_labs <- within(var_labs, {
    label <- ifelse(label == "", name, label)
    name <- ifelse(name == "", label, name)
  })


names(LABELS) = translate_names(names(LABELS))

# house keeping
rm(ll, lbls)
