library(data.table)
library(rvest)
library(readxl)
library(pdftools)

source("script/helpers/helpers.R")
source("script/helpers/base_helpers.R")

# helper in this script to translate German col names (months) into English
trans_nms <- function(x) {
  nms = names(copy(x))
  tnms = translate_mon(nms)
  nms[which(!is.na(tnms))] = na.omit(tnms)
  nms
}

# Baupreiseindex (a proxy for construction costs), at the state level ----
## source : https://www.sprengnetter.de/produkte-services/marktdaten-karten-unterlagen-service/indizes-und-zeitreihen/baupreisindex/

# States -----------------------------------------------------------------------

## Baden-Württemberg (BW) -----------------------
# https://www.statistik-bw.de/GesamtwBranchen/KonjunktPreise/BPI-LR.jsp?form=csv

fpath = "data/raw/Regional-Atlas/Construction-costs/BPI-LR.csv"
furl = "https://www.statistik-bw.de/GesamtwBranchen/KonjunktPreise/BPI-LR.jsp?form=csv"
dir.create(dirname(fpath), recursive = TRUE, showWarnings = FALSE)

# construction price index
if (!file.exists(fpath)) {
  bpi_bw = readLines(furl, encoding = "UTF-8")
  fname = sub("([[:alnum:]]+)[.](.*)", "\\1", basename(furl))
} else {
  bpi_bw = readLines(fpath, encoding = "UTF-8")
  fname = tools::file_path_sans_ext(basename(fpath))
}

bpi_bw = bpi_bw[which(!bpi_bw %in% enc2utf8(c(
  "*) Einschließlich Mehrwertsteuer.",
  "Datenquelle: Statistik der Bauleistungspreise.",
  "© Statistisches Landesamt Baden-Württemberg, 2020", ";2015 = 100"
)))]


bpi_bw = paste0(bpi_bw, collapse = "\n")

fpath_new = sprintf("%s/%s-edited.csv", dirname(fpath), fname)
if (!file.exists(fpath_new)) {
  writeLines(bpi_bw, fpath_new)
}

bpi_bw = fread(bpi_bw,
  dec = ",", sep = ";", skip = 1,
  na.strings = enc2utf8(c("…", ".", "·")),
  encoding = "UTF-8"
)

setnames(bpi_bw, "Jahr/Monat", "year_mon")

bpi_bw[grepl("^JD.*(\\d{4})$", year_mon), year_mon := sub("^JD.*(\\d{4})$", "\\1", year_mon)]


bpi_bw[grepl("\\D", year_mon), year_mon := translate_mon(year_mon)]

# keep the the first column and a column which is for buildings (Wohngebäude)
setnames(bpi_bw, "Wohngebäude", "cpi_resid_building")
bpi_bw = bpi_bw[, c("year_mon", "cpi_resid_building"), with = FALSE]
bpi_bw[, state := rep_len("BW", .N)] # add column for the state name

# separate year and month from the first column
bpi_bw = sep_rows(bpi_bw, "year_mon", "\\d{4}")[grepl("\\D", year_mon), ]

# keep obs since 1990
bpi_bw = bpi_bw[as.integer(year_mon_sep) >= 1990L, ]

bpi_bw = dcast(bpi_bw, state + year_mon_sep ~ year_mon, value.var = "cpi_resid_building")
# compute rowMeans (of all months) -> yearly ave
bpi_bw[, cpi_ave := rowMeans(bpi_bw[, !c("state", "year_mon_sep")], na.rm = TRUE)]
setnames(bpi_bw, "year_mon_sep", "year")
fwrite(
  bpi_bw,
  "data/raw/Regional-Atlas/Construction-costs/bpi_cleaned/construction-index_BW.csv"
)


## Berlin (BE), Brandenburg (BB) ------

furl = sprintf("https://www.statistik-berlin-brandenburg.de/publikationen/stat_berichte/2021/SB_M01-04-00_2021q01_%s.xlsx", c("BE", "BB"))

fname = sprintf(
  "data/raw/Regional-Atlas/Construction-costs/%s",
  basename(furl)
)


Map(furl, fname, f = function(x, p) {
  if (!file.exists(p)) {
    download.file(x, p)
    message(p, " successfuly downloaded!")
  }
})

bpi_bb_be = lapply(
  fname,
  function(x) {
    # read nothing but get the variable names
    df = read_excel(x, sheet = "Tab.3", skip = 1, n_max = 1)
    nms = names(df)
    df = read_excel(x, sheet = "Tab.3", skip = 3, n_max = 32, na = enc2native("…"))
    names(df) = nms
    df$state = sub(
      "(?:.*)_([A-Z]{2})([.][a-z]{4})",
      "\\1", basename(x)
    )
    df
  }
)

bpi_bb_be = rbindlist(bpi_bb_be)
setnames(bpi_bb_be, c("Durch-\r\nschnitt", "Jahr"), c("cpi_ave", "year"))

bpi_bb_be[, cpi_ave := rowMeans(.SD[, !c("state", "year", "cpi_ave")], na.rm = T)]


# translate German months and rename cols
setnames(bpi_bb_be, names(bpi_bb_be), trans_nms(bpi_bb_be))

# relocate cols
setcolorder(bpi_bb_be, c("state", "year"))

fwrite(
  bpi_bb_be,
  sprintf(
    "%s/bpi_cleaned/construction-index_%s.csv",
    dirname(fname[[1]]),
    bpi_bb_be[, paste0(unique(state), collapse = "-")]
  )
)

## Bayern (BY) -----------------------

# https://www.statistikdaten.bayern.de/genesis/online?language=de&sequenz=statistikTabellen&selectionname=61261

# direct downlod:  https://www.statistikdaten.bayern.de/genesis/online/data/61261-121.csv?operation=ergebnistabelleDownload&levelindex=2&levelid=1621537405884&option=dcsv

furl = "https://www.statistikdaten.bayern.de/genesis//online/data?operation=table&code=61261-121&levelindex=0&levelid=1621540375347"

bpi_by = read_html(furl) %>%
  html_nodes("#ET") %>%
  html_table() %>%
  .[[1]]

names(bpi_by) = unlist(bpi_by[1, ])
bpi_by = bpi_by[-1, ]
names(bpi_by)[[1]] = "Jahresdurchschnitt"
bpi_by = bpi_by[, c("Jahresdurchschnitt", "Wohngebäude")]

names(bpi_by) = c("year", "cpi_ave")

bpi_by = transform(bpi_by,
  cpi_ave = as.numeric(sub(",", ".", cpi_ave)), state = "BY"
)

# The below process gets us quarterly bauprice index
# remember we are doing this because we may need the quarterly data
# but it goes to 2014 only, so we need the yearly data too for after 2014
furl = "https://www.statistikdaten.bayern.de/genesis//online/data?operation=table&code=61261-122&levelindex=0&levelid=1621605513388"

furl %>%
  read_html() %>%
  html_nodes("#ET") %>%
  html_table() %>%
  .[[1]] -> bpi_by_mon
names(bpi_by_mon) = unlist(bpi_by_mon[1, ])
bpi_by_mon = bpi_by_mon[-1, 1:3]
names(bpi_by_mon) = c("year", "mon", "cpi")
bpi_by_mon = sep_rows(bpi_by_mon, "year", "\\d{4}")
bpi_by_mon = dcast(as.data.table(bpi_by_mon)[, !"year"],
  year_sep ~ mon,
  value.var = "cpi"
)
setnames(bpi_by_mon, names(bpi_by_mon), c("year", translate_mon(names(bpi_by_mon)[-1])))
bpi_by_mon[, (names(bpi_by_mon)[-1]) := lapply(.SD, \(df_pdf) as.numeric(sub(",", ".", df_pdf))), .SDcols = !"year"]

bpi_by_mon[, c("state", "cpi_ave") := .(
  rep_len("BY", .N),
  rowMeans(bpi_by_mon[, !"year"], na.rm = TRUE)
)]

setcolorder(bpi_by_mon, c("state", "year"))
# merge the yearly and quarterly
bpi_by = bpi_by_mon[bpi_by, on = .(state, year)]
bpi_by[, `:=`(cpi_ave = i.cpi_ave, i.cpi_ave = NULL)]
rm(bpi_by_mon)

fwrite(
  bpi_by,
  "data/raw/Regional-Atlas/Construction-costs/bpi_cleaned/construction-index_BY.csv"
)


## Hesse (HE) --------------

furl = "https://statistik.hessen.de/sites/statistik.hessen.de/files/Bauindizes_Wohngebaeude_1968-2021_08042021.xlsx"

fpath = strsplit(basename(furl), "_")[[1]]
fpath = sprintf(
  "%s/%s_Hesse_%s",
  "data/raw/Regional-Atlas/Construction-costs/",
  paste0(fpath[1:2], collapse = "_"),
  paste0(fpath[-c(1, 2)], collapse = "_")
)

download_file(furl, fpath)
bpi_he = read_excel(fpath, skip = 3)[, 1:3]

# remove the footer
bpi_he = bpi_he[1:(which(grepl("_{3, }", bpi_he[[1]])) - 1), ]
names(bpi_he) = c("year", "mon", "cpi")
setDT(bpi_he)
bpi_he[mon != "D", ] <- sep_rows(bpi_he[mon != "D", ], "year", "\\d{4}")[, !"year"]
bpi_he = dcast(bpi_he, year ~ mon, value.var = "cpi")

bpi_he[, cpi_ave := rowMeans(bpi_he[, !"year"], na.rm = TRUE)]
bpi_he[, D := NULL][, state := rep_len("HE", .N)]
setnames(bpi_he, names(bpi_he), trans_nms(bpi_he))
setcolorder(bpi_he, c("state", "year"))
fwrite(
  bpi_he,
  "data/raw/Regional-Atlas/Construction-costs/bpi_cleaned/construction-index_HE.csv"
)

## Niedersachsen (NI) ----------------
furl = "https://www.statistik.niedersachsen.de/download/79666"
fpath <- dir("data/raw/Regional-Atlas/Construction-costs",
  pattern = "^[^~].*niedersachsen",
  full.names = T, ignore.case = T
)


download_file(furl, fpath)

bpi_ni = read_excel(fpath, skip = 1)[, 1:2]
bpi_ni = bpi_ni[1:(which(grepl("Niedersachsen", bpi_ni[[1]])) - 1), ]
names(bpi_ni) = c("year_mon", "cpi")
setDT(bpi_ni)

temp1 = bpi_ni[grepl("^\\d{4}D$", year_mon), ]
temp1[, year_mon := sub("(\\d{4})(D)", "\\1_\\2", year_mon)][, c("year", "mon") := tstrsplit(year_mon, split = "_")]
temp1[, year_mon := NULL]
setcolorder(temp1, c("year", "mon", "cpi"))

temp2 = sep_rows(bpi_ni[!grepl("^\\d{4}D$", year_mon), ], "year_mon", "\\d{4}")[, c("year_mon_sep", "year_mon", "cpi"), with = FALSE]
temp2 = temp2[!grepl("\\d{4}", year_mon), ]

setnames(temp2, c("year", "mon", "cpi"))

bpi_ni = rbindlist(list(temp1, temp2))
rm(temp1, temp2)
bpi_ni = dcast(bpi_ni, year ~ mon, value.var = "cpi")

bpi_ni[, cpi_ave := rowMeans(
  bpi_ni[,
    lapply(.SD, as.numeric),
    .SDcols = !c("year", "D")
  ],
  na.rm = T
)]
bpi_ni[, D := NULL][, state := "NI"]
setnames(bpi_ni, names(bpi_ni), trans_nms(bpi_ni))
setcolorder(bpi_ni, c("state", "year"))

fwrite(
  bpi_ni,
  "data/raw/Regional-Atlas/Construction-costs/bpi_cleaned/construction-index_NI.csv"
)

## North Rhine-Westphalia (NW) ---------------------

furl = "https://webshop.it.nrw.de/gratis/M149%20202141.pdf"
fpath = sprintf("data/raw/Regional-Atlas/Construction-costs/BPI_NRW_%s", sub("%", "_", basename(furl)))
# extract page 23
text = if (file.exists(fpath)) {
  pdf_text(fpath)[[23]]
} else {
  pdf_text(furl)[[23]]
}

part1 = gsub("(.*)(1968.*)(2017.*Februar.*)(Preisindizes.*)", "\\2", text)
part1 = fread(part1, dec = ",", select = 1:3, header = FALSE, col.names = c("year", "mon", "cpi"))

# there is quarterly data from 2017-2021

part2 = gsub(
  "(.*)(1968.*)(2017.*Februar.*)(Preisindizes.*)", "\\3",
  text
)
part2 = fread(part2, dec = ",", fill = T, select = 1:3, header = FALSE, na.strings = "…")

part2[grepl("\\D+", V1, perl = T), mon := V1]
part2[is.na(mon), mon := V2]
part2[grepl("[[:alpha:]]+", V2), V2 := V3]
part2[grepl("[[:alpha:]]+", V1), V1 := ""]

part2[, V3 := NULL]
part2 = part2[!apply(part2, 1, \(x) all(x == "")), ]
part2[, V2 := as.numeric(sub(",", ".", V2))]

part2 = sep_rows(part2, "V1", "\\d{4}")[, !"V1"]
setnames(part2, c("year", "cpi", "mon"))
bpi_nw = rbindlist(list(part1, part2), use.names = T)

bpi_nw = dcast(bpi_nw, year ~ mon, value.var = "cpi")
bpi_nw[, `:=`(
  state = "NW",
  cpi_ave = rowMeans(.SD, na.rm = T)
), .SDcols = !"year"]
bpi_nw[, D := NULL]
setcolorder(bpi_nw, c("state", "year"))
setnames(bpi_nw, trans_nms(bpi_nw))

fwrite(
  bpi_nw,
  "data/raw/Regional-Atlas/Construction-costs/bpi_cleaned/construction-index_NW.csv"
)


## Saarland (SL) ------------

furl = "https://www.statistischebibliothek.de/mir/servlets/MCRFileNodeServlet/SLHeft_derivate_00006831/SL%20Bericht%20MI4%202021-01-vj.pdf"

fpath = sprintf(
  "data/raw/Regional-Atlas/Construction-costs/%s",
  gsub("%\\d{2}", " ", basename(furl))
)
fpath = sub("SL", "Saarland", fpath)


try(download_file(furl, fpath))

# the price index table is on page 5
txt = pdf_text(fpath)[[5]]
txt = gsub("^(.*)(2000 D.*)\\d\\).*$", "\\2", txt)
txt = gsub("(((?:19)[5-9]{2})|((?:20)[0-9]{2})) D", "\\1", txt)
txt = strsplit(txt, "\n\n")[[1]]

bpi_sl = fread(text = txt, sep = " ", na.strings = ".", dec = ",", fill = TRUE, blank.lines.skip = TRUE)[, c("V1", "V2")]

names(bpi_sl) = c("year_mon", "cpi")

bpi_sl_quarterly = sep_rows(bpi_sl[22:.N, ], "year_mon", "\\d{4}")[grepl("\\D", year_mon), ]
names(bpi_sl_quarterly) = c("mon", "year", "cpi")

bpi_sl = bpi_sl[1:21, ]
setnames(bpi_sl, "year_mon", "year")
bpi_sl[, mon := rep("D", .N)]
bpi_sl = rbindlist(list(bpi_sl, bpi_sl_quarterly), use.names = TRUE)
bpi_sl = dcast(bpi_sl, year ~ mon, value.var = "cpi")

bpi_sl[, cpi_ave := ifelse(!is.na(D), D, rowMeans(bpi_sl[, !c("year", "D")], na.rm = TRUE))]
bpi_sl[, D := NULL][, state := "SL"]
setnames(bpi_sl, names(bpi_sl), trans_nms(bpi_sl))
setcolorder(bpi_sl, c("state", "year", "February"))

fwrite(
  bpi_sl,
  "data/raw/Regional-Atlas/Construction-costs/bpi_cleaned/construction-index_SL.csv"
)


## Sachsen-Anhalt (ST) -----------

furl = "https://statistik.sachsen-anhalt.de/fileadmin/Bibliothek/Landesaemter/StaLa/startseite/Themen/Preise__Verdienste/Tabellen/Preise/Baupreisindizes_Jahresdurchschnitte_ab_1991.xlsx"

fpath = sprintf(sub("_", "_%s_", basename(furl)), "ST")
fpath = paste0("data/raw/Regional-Atlas/Construction-costs/", fpath)
try(download_file(furl, fpath))

bpi_st = read_excel(fpath, skip = 3)[, 1:2] |>
  setNames(c("year", "cpi_ave"))
bpi_st = head(bpi_st, -2)
bpi_st = within(bpi_st, {
  state = "ST"
  year = sub("(\\d{4}).*JD", "\\1", year)
  cpi_ave = as.numeric(gsub("[.,]", ".", cpi_ave))
})[c(3, 1, 2)]

fwrite(
  bpi_st,
  "data/raw/Regional-Atlas/Construction-costs/bpi_cleaned/construction-index_ST.csv"
)

## Sachsen (SN) -------------

furl = "https://www.statistik.sachsen.de/download/preise/statistik-sachsen_mI_zr_preisindizes-bauleistungen-neubau.xlsx"

fpath = sprintf(
  "data/raw/Regional-Atlas/Construction-costs/%s",
  basename(furl)
)
try(download_file(furl, fpath))

bpi_sn = read_excel(fpath, skip = 35)[1:3] |>
  setNames(c("year", "mon", "cpi_ave"))

bpi_sn$year = c(rep(1995:2020, each = 4), 2021)
bpi_sn = bpi_sn |>
  as.data.table() |>
  dcast(year ~ mon, value.var = "cpi_ave")
bpi_sn[, `:=`(state = "SN", cpi_ave = rowMeans(.SD, na.rm = T)), .SDcols = !"year"]
setcolorder(bpi_sn, c("state"))
setnames(bpi_sn, names(bpi_sn), trans_nms(bpi_sn))

fwrite(
  bpi_sn,
  "data/raw/Regional-Atlas/Construction-costs/bpi_cleaned/construction-index_SN.csv"
)


## Thüringen ----------------

# yearly
furl = "https://statistik.thueringen.de/datenbank/download/rad17DB0.csv"
fpath = sprintf("data/raw/Regional-Atlas/Construction-costs//BPI_TH_%s", basename(furl))
try(download_file(furl, fpath))

bpi_th = fread(fpath, encoding = "Latin-1", skip = 2L, dec = ",", sep = ";", select = 1:2, col.names = c("year", "cpi"))
bpi_th[, c("year", "mon") := tstrsplit(year, " ")]

# quarterly data

furl = "https://statistik.thueringen.de/datenbank/download/rad5B5D3.csv"
fpath = sprintf("data/raw/Regional-Atlas/Construction-costs//BPI_TH_%s", basename(furl))

try(download_file(furl, fpath))

bpi_th_quart = fread(fpath, sep = ";", dec = ",", encoding = "Latin-1", skip = 1)[1, `Feb 2010`:`Feb 2021`]

bpi_th_quart = data.table(
  mon = names(bpi_th_quart),
  cpi = unlist(transpose(bpi_th_quart))
)
bpi_th_quart[, c("mon", "year") := tstrsplit(mon, " ")]

# bind yearly and quarterly
bpi_th = rbindlist(list(bpi_th, bpi_th_quart), use.names = TRUE)
bpi_th = dcast(bpi_th, year ~ mon, value.var = "cpi")
bpi_th[, `:=`(
  state = "TH",
  cpi_ave = rowMeans(.SD, na.rm = T)
), .SDcols = !"year"]
bpi_th[, JD := NULL]
setcolorder(bpi_th, c("state", "year"))
setnames(bpi_th, names(bpi_th), trans_nms(bpi_th))

fwrite(
  bpi_th,
  "data/raw/Regional-Atlas/Construction-costs/bpi_cleaned/construction-index_TH.csv"
)

## No data for the remaining states



# combine all together ------------

bpi = dir("data/raw/Regional-Atlas/Construction-costs/bpi_cleaned/",
  pattern = "^construction-index_([A-Z]{2})|(BB-BE).csv$", full.names = T
) |>
  lapply(fread) |>
  rbindlist(use.names = T, fill = T)

setkeyv(bpi, c("state", "year"))

setnames(
  bpi, grep("^[A-Z][a-z]+$", names(bpi), value = T),
  \(x) gsub(x = tolower(x), "(.{3})(.*)", "cpi_\\1")
)


States = fread("data/processed/admin-areas/states.csv",
  drop = "state_name",
  colClasses = list("character" = "state_code")
)

bpi = merge(bpi, States, by.x = "state", by.y = "state_abb")[, !"state"]
setcolorder(bpi, "state_code")
setkey(bpi, state_code, year)

fwrite(bpi, "data/processed/main/construction-costs-index.csv")
