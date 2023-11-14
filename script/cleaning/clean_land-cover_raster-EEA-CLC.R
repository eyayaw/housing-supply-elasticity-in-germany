library(sf)
library(terra)


tmpdir = tempdir()
dname = "data/raw/geodata/corine-land-cover/raster/"
dir.create(dname, recursive = TRUE, showWarnings = FALSE)

## extract the land cover of Germany from the EU-wide raster

# read in the file which we use to crop out Germany's land cover
dtm_path = "data/raw/geodata/Digital-Terrain-Model/dgm200.utm32s.gridascii/dgm200/dgm200_utm32s.asc"
clipper = rast(dtm_path)

# read in EU level corine land cover raster data
dir_clc = paste0(tmpdir, "/clc")
dir.create(dir_clc, recursive = TRUE, showWarnings = FALSE)
years = c(1990, 2000, 2006, 2012, 2018)

for (y in years) {
  pattern = sprintf("^clc-eu-%d.zip$", y)
  fname_clc = dir(dname, pattern, full.names = TRUE)
  unzip(fname_clc, exdir = dir_clc, overwrite = FALSE)
  zipfile = dir(dir_clc,
    sprintf("clc%d_v2020_20u1_raster100m[.]zip$", y),
    full.names = TRUE)
  unzip(zipfile, exdir = dir_clc, overwrite = F)
}

# land cover rasters
fraster = list.files(dir_clc, "20u1[.]tif$", full = TRUE, recursive = TRUE)
clc = lapply(fraster, rast)
names(clc) = years

dir.create(
  "data/processed/geodata/clc-germany", recursive = T, showWarnings = F
)

for (i in seq_along(clc)) {
  # re-projection
  clc[[i]] = tryCatch(
    project(clc[[i]], clipper, method = "near", mask = TRUE),
    error = function(e) {
      message(e$message)
      crs(clc[[i]]) = "EPSG:3035"
      project(clc[[i]], clipper, method = "near", mask = TRUE)
    }
  )
  clc[[i]] = mask(crop(clc[[i]], ext(clipper)), clipper)
  tbl = data.frame(table(values(clc[[i]]))) |> setNames(c("id", "count"))
  tbl$id = as.integer(levels(tbl$id))[tbl$id]
  Cats = cats(clc[[i]])[[1]]
  names(Cats) = tolower(names(Cats))
  tbl = merge(tbl, Cats[, setdiff(names(Cats), "count")], by.x="id", by.y="value")
  levels(clc[[i]])[[1]] = tbl
  activeCat(clc[[i]]) = grep("^label", names(tbl)) - 1 # col 1 is id, not counted

  # write to disk
  dirName = sprintf("data/processed/geodata/clc-germany/clc-%d", years[[i]])
  dir.create(dirName, recursive = T, showWarnings = F)
  writeRaster(
    clc[[i]], sprintf("%s/%s", dirName, basename(fraster)[[i]]), overwrite = T
  )
}

unlink(tmpdir)
