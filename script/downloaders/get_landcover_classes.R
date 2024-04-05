library(data.table)

# land cover classes ----

## https://land.copernicus.eu/user-corner/technical-library/corine-land-cover-nomenclature-guidelines/html/index.html
## parse classes that are relevant to Germany from the pdf file
pdf_path = "data/raw/geodata/corine-land-cover/clc5_2018.pdf"
if (!file.exists(pdf_path)) {
  download.file(
    "https://sg.geodatenzentrum.de/web_public/gdz/dokumentation/deu/clc5_2018.pdf", pdf_path
  )
}

# classes = pdftools::pdf_text(pdf_path)[[7]] |>
#   gsub(pattern = "(.*)(CLC-Code.*Meere und Ozeane)(.*)", replacement = "\\2") |>
#   strsplit("\n") |>
#   unlist() |>
#   sub(pattern = "^ ", replacement = "") |>
#   gsub(pattern = "^ {6,}", replacement = "...") |>
#   gsub(pattern = " {4,}", replacement = ";")
#
# classes = data.table::fread(text = classes, sep = ";", fill = T)

## Then edit by hand. datapasta may help

classes <- data.frame(
  clc_code = c(
    111L, 112L, 121L, 122L, 123L,
    124L, 131L, 132L, 133L, 141L, 142L, 211L, 221L, 222L, 231L,
    242L, 243L, 311L, 312L, 313L, 321L, 322L, 324L, 331L,
    332L, 333L, 334L, 335L, 411L, 412L, 421L, 423L, 511L, 512L,
    521L, 522L, 523L
  ),
  classname = c(
    "Continuous urban fabric",
    "Discontinuous urban fabric",
    "Industrial, commercial and public units",
    "Road and rail networks and associated land",
    "Port area",
    "Airports",
    "Mineral extraction sites",
    "Dump sites",
    "Construction sites",
    "Green urban area",
    "Sport and leisure facilities",
    "Non-irrigated arable land",
    "Vineyards",
    "Fruit tree and berry plantations",
    "Pasture, meadows and other permanent grasslands under agricultural use",
    "Complex cultivation patterns",
    "Land principally occupied by agriculture, with significant areas of natural vegetation",
    "Broad-leaved forest",
    "Coniferous forest",
    "Mixed forest",
    "Natural grassland",
    "Moors and heathland",
    "Transitional woodland/shrub",
    "Beaches, dunes and sand plains",
    "Bare rock",
    "Sparsely vegetated areas",
    "Burnt areas",
    "Glaciers and perpetual snow",
    "Inland marshes",
    "Peatbogs",
    "Coastal salt marshes",
    "Intertidal flats",
    "Water courses",
    "Water bodies",
    "Coastal lagoons",
    "Estuaries",
    "Sea and ocean"
  ),
  klassname = c(
    "Durchgängig städtische Prägung", "Nicht durchgängig städtische Prägung",
    "Industrie und Gewerbeflächen, öffentliche Einrichtungen",
    "Straßen-, Eisenbahnnetze und funktionell zugeordnete Flächen", "Hafengebiete", "Flughäfen", "Abbauflächen",
    "Deponien und Abraumhalden", "Baustellen",
    "Städtische Grünfläche", "Sport- und Freizeitanlagen",
    "Nicht bewässertes Ackerland", "Weinbauflächen",
    "Obst- und Beerenobstbestände", "Wiesen und Weiden grasslands",
    "Komplexe Parzellenstruktur",
    "Landwirtschaftlich genutztes Land mit Flächen natürlicher Bodenbedeckung von signifikanter Größe",
    "Laubwälder", "Nadelwälder", "Mischwälder",
    "Natürliches Grünland", "Heiden und Moorheiden",
    "Wald-Strauch-Übergangsstadien", "Strände, Dünen und Sandflächen",
    "Felsen ohne Vegetation",
    "Flächen mit spärlicher Vegetation", "Brandflächen",
    "Gletscher und Dauerschneegebiete", "Sümpfe", "Torfmoore", "Salzwiesen",
    "In der Gezeitenzone liegende Flächen", "Gewässerläufe",
    "Wasserflächen", "Lagunen", "Mündungsgebiete", "Meere und Ozeane"
  )
)



colorcodes <-
  "222,0,0
255,16,66

156,0,156
156,156,156
173,181,198
99,99,99

156,99,0
156,49,0
156,156,156

99,255,0
255,99,49

255,255,132

255,99,49
255,206,156

99,206,49

255,206,156
206,206,99


0,206,0
0,156,99
0,156,0


156,206,99
206,206,0
255,255,0

255,255,206
255,255,255
156,206,156
16,16,16
247,255,255

156,99,255
132,132,222

206,156,206
255,239,255

0,99,255
0,156,255

0,206,255
99,206,255
156,206,255"


colorcodes <-
  fread(text = colorcodes, blank.lines.skip = T, col.names = c("r", "g", "b"))

colorcodes[, cols1 := rgb(r / 255, g / 255, b / 255)]
scales::show_col(colorcodes[["cols1"]])


classes2 <- tibble::tribble(
  ~classname2, ~classname3,
  "urban fabric", "artificial surfaces",
  "urban fabric", "artificial surfaces",
  "industrial, commercial and transport units", "artificial surfaces",
  "industrial, commercial and transport units", "artificial surfaces",
  "industrial, commercial and transport units", "artificial surfaces",
  "industrial, commercial and transport units", "artificial surfaces",
  "mines, dumps and construction sites", "artificial surfaces",
  "mines, dumps and construction sites", "artificial surfaces",
  "mines, dumps and construction sites", "artificial surfaces",
  "artificial non-agriculatural vegetated areas", "artificial surfaces",
  "artificial non-agriculatural vegetated areas", "artificial surfaces",
  "arable land", "agricultural areas",
  "permanent crops", "agricultural areas",
  "permanent crops", "agricultural areas",
  "pastures", "agricultural areas",
  "heterogeneous agricultural areas", "agricultural areas",
  "heterogeneous agricultural areas", "agricultural areas",
  "forests", "forest and seminatural area",
  "forests", "forest and seminatural area",
  "forests", "forest and seminatural area",
  "scrubs and/or herbaceous vegetation", "forest and seminatural area",
  "scrubs and/or herbaceous vegetation", "forest and seminatural area",
  "scrubs and/or herbaceous vegetation", "forest and seminatural area",
  "open spaces with little or no vegetation", "forest and seminatural area",
  "open spaces with little or no vegetation", "forest and seminatural area",
  "open spaces with little or no vegetation", "forest and seminatural area",
  "open spaces with little or no vegetation", "forest and seminatural area",
  "open spaces with little or no vegetation", "forest and seminatural area",
  "inland wetlands", "wetlands",
  "inland wetlands", "wetlands",
  "costal wetlands", "wetlands",
  "costal wetlands", "wetlands",
  "inland waters", "water bodies",
  "inland waters", "water bodies",
  "marine waters", "water bodies",
  "marine waters", "water bodies",
  "marine waters", "water bodies"
)


classes <- cbind(classes, classes2, colorcodes[, .(cols1)])
names(classes) <- tolower(names(classes))


colorcodes_3 <- data.table(
  classname3 = c(
    "artificial surfaces", "agricultural areas",
    "forest and seminatural area", "wetlands", "water bodies"
  ),
  rgb3 = c(
    "255,153,153",
    "255,255,204",
    "204,255,153",
    "204,204,255",
    "204,255,255"
  )
)

colorcodes_3[, c("r", "g", "b") := lapply(tstrsplit(rgb3, ","), as.integer)]
colorcodes_3[, cols3 := rgb(r / 255, g / 255, b / 255)]

classes <- classes[,
  colorcodes_3[, .(classname3, cols3)], on = "classname3"
]

fwrite(classes, "data/raw/geodata/corine-land-cover/clc-classification.csv")
rm(colorcodes, classes2, colorcodes_3)
