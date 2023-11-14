

pvalues <- function(obj, vcov = NULL) {
  if (!missing(vcov)) {
    stopifnot(`vcov must be a matrix` = is.matrix(vcov))
  } else {
    vcov = vcov(obj)
  }
  coef. = coef(obj)
  se = diag(vcov)^.5
  stopifnot(`length does not match` = length(coef.) == length(se))
  nms = intersect(names(coef.), names(se))
  se = se[names(se) %in% nms]
  coef. = coef.[names(coef.) %in% nms]
  tval = coef. / se
  df = df.residual(obj)
  2 * pt(abs(tval), df = df, lower.tail = FALSE)
}



lm_labels <- function(dat, x, y, xlab = 'x', ylab = 'y') {
  form = as.formula(paste(y, '~', x))
  mod = lm(form, data = dat)
  se = summary(mod)$coefficients[2, 2]
  formula = sprintf(
    "italic(widehat(%s)) == %.2f %+.2f(%.2f) * italic(%s)",
    ylab, coef(mod)[1], coef(mod)[2], se, xlab)
  formula = sub("(?<!== )([-+])", '~\\1~', formula, perl = T)
  r2 = cor(dat[[x]], dat[[y]])**2
  r2 = sprintf("italic(R^2) == %.2f", r2)
  data.frame(formula = formula, r2 = r2)
}



eq.label <- function(obj, digits = 2) {
  getSign <- function(x) if (sign(x) == -1) "-" else "+"

  betas = coef(obj)
  names = names(betas)
  if (names[1] == "(Intercept)") names[1] = ""
  signs = sapply(betas, getSign)
  fmt.float = paste0("%.", digits, "f")

  slopes = paste0(
    sprintf(paste0("%s ", fmt.float, "%s"), signs[-1], abs(betas[-1]), names[-1]),
    collapse = " "
    )
  sprintf(paste0(fmt.float, "%s %s"), betas[1], names[1], slopes)
}


make_form <- function(y, x, const = TRUE, env = parent.frame()) {
  stopifnot(is.logical(const))
  if (const)
    as.formula(paste0(y, ' ~ ', paste(x, collapse = " + ")), env = env)
  else
    as.formula(paste0(y, ' ~ 0 + ', paste(x, collapse = " + ")), env = env)
}

make_form_iv <-
  function(y, x, z, const.2nd = TRUE, const.1st = TRUE, env = parent.frame()) {
  # y ~ x | z + x[-1]
  form_2nd = make_form(y, x, const = const.2nd, env = env)
  form_1st = make_form(NULL, c(z, x[-1L]), const.1st, env = env)
  list(f = form_2nd, iv.f = form_1st)
}

# iv construction: y ~ x1 + x2 + c1 | z1 + z2 + c1 + c2
iv_parts <- function(y, x, z, c1 = NULL, c2 = NULL) {
  rhs = paste0(c(x, c1), collapse = '+')
  stopifnot(length(rhs) == 1L && is.character(rhs))
  c(rhs, paste0(c(iv, strsplit(rhs, "[+]")[[1L]][-1L], c2), collapse = "+"))
}

# parse iv parts given rhs of the second stage:
# e.g. e = "x + c1" with c2 additionally supplied, -> ["x + c1", "z + c1 + c2"]
parse_iv_parts <- function(e, z = NULL, ctrl.1st.stage = NULL) {
  stopifnot(is.character(e) && length(e) == 1L && !is.null(e) && e != "")
  ctrl.1st.stage = paste0(ctrl.1st.stage, collapse = " + ")
  if (length(ctrl.1st.stage) == 1L && trimws(ctrl.1st.stage) == "")
    ctrl.1st.stage = NULL
  if (!missing(z))
    if (length(z) == 1L && trimws(z) == "")
      z = NULL
  nz = length(z)
  if (nz == 0L)
    parts = c(xs=e, zs=paste0(c(e, ctrl.1st.stage), collapse = " + "))
  else {
    xs = c(strsplit(e, "[+]")[[1L]][-seq_len(nz)], ctrl.1st.stage)
    part_1st.stage = paste0(c(paste0(z, collapse = ' + '), xs), collapse = " + ")
    parts = c(xs=e, zs=part_1st.stage)
  }
  gsub(" {2,}", " ", parts)
}


# post-process notes in the latex output
append_note <- function(out, ncol = 2L, common_note = NULL) {
  note = sprintf(
    r"[\multicolumn{%d}{l}{\parbox[t]{\textwidth}{\textit{Notes:} %s}} \\]",
    ncol, paste0(common_note, collapse = " "))
  out[grepl('Note:', out)] = note
  out
}


bold <- function(x) {
  sprintf('\\textbf{%s}', x)
}

italic <- function(x) {
  sprintf('\\textit{%s}', x)
}

fixed <- function(x) {
  charset = strsplit('[]\\*+.{}()?^$', "")[[1]]
  charset = charset[vapply(charset, \(p) grepl(p, x, fixed = TRUE), NA)]
  if (length(charset) == 0)
    return(x)
  if ("\\" %in% charset)
    x = gsub(r"{\}", r"{\\}", x, fixed = TRUE)
  if (any(c("[", "]") %in% charset)) {
    x = gsub("[", "\\[", x, fixed = TRUE)
    x = gsub("]", "\\]", x, fixed = TRUE)
  }
  charset = setdiff(charset, c("\\", "[","]"))
  out = character(length(charset))
  for (i in seq_along(out)) {
    x = gsub(charset[[i]], paste0("\\", charset[[i]]), x, fixed = TRUE)
  }
  x
}


# make variable label ----
dict = tibble::tribble(
  ~vars,                       ~labs,
  "hpi",                        "P",
  "total_buildings",            "Units",
  "floorspace",                 "Floorspace",
  "permits",                    "Permits",
  "completions",                "Completions",
  'permits_total_buildings',    'Permits (buildings)',
  "permits_apartments",         "Permits (apartments)",
  "dev_frac",                   "Developed",
  "unavail_frac",               "Unavail",
  #"pland",                      "Price of land",
  "hhinc",                      "HH income",
  "pop",                        "Pop",
  "pop_den",                    "Pop density",
  # "emp_pred",                   "Bartik",
  "bartik",                     "Bartik",
  "bartik_1",                   "Bartik",
  "emp_act",                    "employment growth (actual)",
  "west_east",                  "West",
  "west_eastEast",              "East",
  "west_eastWest",              "West",
  "urban_rural",                "Urban",
  "urban_ruralUrban",           "Urban",
  "urban_ruralRural",           "Rural",
  "single_fam",                 "Single-family",
  "(Intercept)",                "(Intercept)",
  "tri",                        "TRI",
  "ccost",                      "Constr. costs",
  "cemp",                      "Constr. labor"
)

# remove I()
rmI <- function(x) {
 trimws(gsub(r"{(\s+)?I\1?\(\1?(?<keep>.+)\1?\)\1?}", "\\2", trimws(x), perl=TRUE))
}

make.label <- function(v, split = "_", math=TRUE) {
  stopifnot(length(v) == 1L && length(split) == 1L)
  v = trimws(v)
  isSingleFam = grepl("[-_]?single[-_]fam(ily)?", v)
  if (isSingleFam)
    v = sub("[-_]?single[-_]fam(ily)?", "", v)

  vs = strsplit(v, split, fixed=TRUE)[[1]]
  vname = setdiff(vs, c('dln', 'ln', grep("\\d{4}", vs, value = TRUE)))
  if (length(vname) == 0L || (length(vname) == 1L && vname == ""))
    return(v)
  if (length(vname) > 1L)
    vname = paste0(vname, collapse = split)
  lab = with(dict, labs[grep(paste0('^', fixed(vname), '$'), vars, ignore.case=TRUE)])
  if (isSingleFam)
    lab = paste0(lab, "--", 'single-family')
  if (length(lab) == 0L || lab == "")
    stop('`', vname, '` has no match in `dict` so cannot be parsed!', call.=FALSE)

  # in change log
  if (grepl("^dln", v))
    lab = sprintf("\\Delta\\ln{\\text{%s}}", lab)
  # in log
  else if (grepl("^ln", v))
    lab = sprintf("\\ln{\\text{%s}}", lab)
  else
    lab = sprintf("\\text{%s}", lab)
  # has year
  if (grepl("_\\d{4}$", v)) {
    .year = regmatches(v, regexpr("\\d{4}$", v))
    lab = sub("text\\{(.+[^}])\\}", sprintf("text{\\1}_{%s}", .year), lab)
    }
  if (math)
    return(paste0('$', lab, '$'))
  lab
}


# split and parse label: e.g. I(dln_hpi * dev_frac) -> .\times.
MakeLabelSplit <- function(x, split = "*", fixed=TRUE, sep="\\times") {
  x = trimws(strsplit(rmI(x), split, fixed)[[1]])
  x = setdiff(x, '1')
  x = vapply(x, make.label, NA_character_, math=FALSE)
  paste0('$', paste0(x, collapse=sep), '$')
}

## vectorize ----

vfixed <- Vectorize(fixed, SIMPLIFY = TRUE)
vmake.label <- Vectorize(make.label, SIMPLIFY = TRUE)
vMakeLabelSplit <- Vectorize(MakeLabelSplit, SIMPLIFY = TRUE)

# customized stargazer -----
my_stargazer <- function(...,
                         type = 'latex',
                         title = "",
                         column.labels = NULL,
                         omit = NULL,
                         omit.stat = c('adj.rsq', 'ser'),
                         notes = NULL,
                         font.size = 'small',
                         table.placement = "H",
                         intercept.bottom = TRUE,
                         column.sep.width = "0pt"
                         ) {
  stargazer::stargazer(...,
            header=FALSE,
            type=type,
            column.labels =
              if (type == 'latex') bold(column.labels) else column.labels,
            title=bold(title),
            omit=omit,
            omit.stat=omit.stat,
            notes=notes,
            notes.align='l',
            notes.append=FALSE,
            font.size=font.size,
            align=TRUE,
            no.space=TRUE,
            table.placement=table.placement,
            intercept.bottom=intercept.bottom,
            column.sep.width = column.sep.width
  )
}


# get names of coefficients of a model object ----

getCoefNames <- function(obj) {
  stopifnot('coefficients' %in% names(obj))
  names(obj$coefficients)
}
# expecting multiple model objects
getUniqueCoefNames <- function(objs) {
  unique(unlist(lapply(objs, getCoefNames)))
}

# try whether LaTeX tables can compile
tryCompile <- function(x) {
  template = c(
    "\\documentclass{article}\n\\usepackage{dcolumn, float, multirow, amsmath, rotating}\n\\begin{document}",
"\\end{document}"
  )
  tmp = tempfile(fileext = ".tex")
  write(paste0(c(template[[1]], x, template[[2]]), collapse = "\n"), tmp)
  rstudioapi::navigateToFile(tmp)
}
