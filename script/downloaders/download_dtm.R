source("./script/helpers/base_helpers.R")

# Download Digital terrain model grid width 200 m (DGM200)
url = "https://daten.gdz.bkg.bund.de/produkte/dgm/dgm200/aktuell/dgm200.utm32s.gridascii.zip"

# create new directory
newdir = "data/raw/geodata/Digital-Terrain-Model/"
dir.create(newdir, recursive = TRUE, showWarnings = FALSE)

downloadFileName = file.path(newdir, basename(url))

# download file
download_file(url, destfile = downloadFileName)

# unzip the files
unzip(downloadFileName, exdir = newdir, overwrite = FALSE)
