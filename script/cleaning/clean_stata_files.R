library(data.table)
library(haven)
source("script/cleaning/variable_list.R")

# create `HK_SUF_ohneText_Combined_Main_Vars.csv` from `HK_SUF_ohneText_*.dta` files
## The entire analysis is based on the `.dta` files of house sales, specifically,
## the files without text labels for categorical variables
## (since the data files with text are huge). Thus, we need to bring individual chunks of
## .dta files into one big file and work with that thereafter.
## That is what this script does.

# extract to temp dir, get the list of file paths within the zip files
rootDir = "data/raw/RWI-GEO-RED/"
mainZip = file.path(rootDir, "2020/HiDrive.zip")
zfile = "Stata/HK_SUF_ohneText.zip"
realname = basename(zfile)
tmpdir = tempdir()
path_to_zipfile = file.path(tmpdir, realname)
dirName = tools::file_path_sans_ext(path_to_zipfile)

unzip(mainZip, zfile, exdir = tmpdir, junkpaths=T) # 1st extraction
unzip(path_to_zipfile, exdir = tmpdir) # 2nd extraction

flist = dir(dirName, "^.*[2-8][.]dta$", full.names=T)

# read in the first file
hk_suf_ohnetext_1 = read_dta(
  file.path(dirName, "HK_SUF_ohneText1.dta"), col_select = main_vars$var_de
)
# read in 2nd-8th files and keep only selected variables
# NB: assign the 2-8 to hk instead if your machine does not have enough RAM
# I like giving intuitive names for objects
hk_suf_ohnetext_2_8 = lapply(flist, haven::read_dta)
hk_suf_ohnetext_2_8 = rbindlist(hk_suf_ohnetext_2_8, use.names=T, fill=T)
hk_suf_ohnetext_2_8 = hk_suf_ohnetext_2_8[, main_vars$var_de, with=F]


# bind rows together and write to disk as one big data set---
# for the entire sample time period
hk = rbindlist(list(hk_suf_ohnetext_1, hk_suf_ohnetext_2_8), use.names=T, fill=T)
rm(hk_suf_ohnetext_1, hk_suf_ohnetext_2_8) # house keeping

fwrite(hk, paste0(rootDir, "HK_SUF_ohneText_Combined_Main_Vars.csv"))
# write_dta(hk, paste0(rootDir, "HK_SUF_ohneText_Combined_Main_Vars.dta"))

# get the labels of each labelled or factor variable
are_labelled = sapply(hk, function(x) "haven_labelled" %in% class(x))
labs = lapply(hk[, ..are_labelled], attr, "labels")
labs = lapply(labs, function(x) data.table(label = names(x), value = x))

dir.create(paste0(rootDir, "labels"))
for (v in names(labs)) {
  write.table(labs[[v]],
    sprintf("%s/labels/labels-%s.txt", rootDir, v), row.names=F
  )
}

unlink(tmpdir)


# Additionally, unzip Dokumentation and Raster_shp dirs

zipfile = "data/raw/RWI-GEO-RED/2020/HiDrive.zip"
lfiles = grep("Dokumentation|Raster_shp", unzip(zipfile, list=T)[[1]], value=T)
unzip(zipfile, lfiles, overwrite=T, exdir=dirname(mainZip))
