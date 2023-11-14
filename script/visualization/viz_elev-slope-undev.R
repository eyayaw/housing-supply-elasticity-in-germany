library(ggplot2)
library(sf)
library(terra)
library(tmap)
library(data.table)
source("script/helpers/helpers.R")


districts = st_read("data/processed/admin-areas/admin-shapes/districts_shape.shp")


# land cover, wet lands and water bodies
## corine land cover classes
clc_classes = fread("data/raw/geodata/corine-land-cover/clc-classification.csv") |>
  subset( # grepl("^(1|4|5)", clc_code),
    select = c("clc_code", "classname", "classname3", "cols1", "cols3")
  )

clc_classes = cbind(clc_classes, t(col2rgb(clc_classes$cols1, TRUE)))

## covers
clc = rast("data/processed/geodata/clc-germany/clc-2006/U2012_CLC2006_V2020_20u1.tif")
# if (crs(clc, proj = TRUE) != crs_dtm)
#   clc = project(clc, dtm, mask = TRUE)

Cats = cats(clc)[[1]] # categories of the raster (factor) values

## separate some land cover classes
maskers = c(
  `artificial-agri-forest` = "^(1|2|3)",
  `wetlands-water` = "^(4|5)",
  `artificial` = "^1"
)
clc_sub = vector("list", length(maskers))
for (i in seq_along(maskers)) {
  fac_ids = get_id(clc, subset(clc_classes, grepl(maskers[[i]], clc_code))$clc_code)
  m = with(Cats, cbind(id, ifelse(id %in% fac_ids, id, NA)))
  clc_sub[[i]] = classify(clc, m)
}

names(clc_sub) = names(maskers)

tm_shape(clc_sub$artificial) +
  tm_raster(
    style = "cat",
    palette = clc_classes[classname3 == "artificial surfaces", cols1],
    labels = clc_classes[classname3 == "artificial surfaces", classname],
    title = "Developed area - Artificial surfaces"
  ) +
  tm_layout(
    frame = F,
    legend.width = -0.65,
    legend.position = c(.9, 0.04),
    legend.text.size = 0.65,
    legend.title.fontface = "bold",
    inner.margins = rep(0.01, 4),
    outer.margins = c(rep(0, 3), .275)
  ) -> pa



tm_shape(clc_sub$`wetlands-water`) +
  tm_raster(
    style = "cat",
    palette = clc_classes[classname3 %like% "wetland|water", cols1],
    labels = clc_classes[classname3 %like% "wetland|water", classname],
    title = "Undevelopable area\n(Wetlands and Water bodies)"
  ) +
  tm_layout(
    frame = F,
    legend.width = -0.5,
    legend.position = c(.9, 0.04),
    legend.text.size = 0.65,
    legend.title.fontface = "bold",
    inner.margins = rep(0.01, 4),
    outer.margins = c(rep(0, 3), .25)
  ) -> pww


tm_shape(clc_sub$`artificial-agri-forest`) +
  tm_raster(
    style = "cat",
    palette = clc_classes[classname3 %like% "arti|agri|forest", cols1],
    labels = clc_classes[classname3 %like% "arti|agri|forest", classname],
    title = "Developable area - Artificial, Agri, and Forests"
  ) +
  tm_layout(
    frame = F,
    legend.width = -1,
    legend.position = c(1, 0.02),
    legend.text.size = .65,
    legend.title.fontface = "bold",
    inner.margins = rep(0.01, 4),
    outer.margins = c(rep(0, 3), .5)
  ) -> paaf



nms = c("developed-land", "undevelopable-land", "developable-land")
for (i in seq_along(nms)) {
  tmap_save(list(pa, pww, paaf)[[i]],
    sprintf("output/figs/%s.png", nms[[i]]),
    width = 8, height = 6, outer.margins = NA
  )
}



## artificial surfaces, wetlands and water bodies ------
aww = classify(clc,
  with(Cats, cbind(id, ifelse(id %in% get_id(clc, subset(clc_classes, grepl("^(1|4|5)", clc_code))$clc_code), id, NA)))
)

tm_shape(aww) +
  tm_raster(
    style = "cat",
    palette = clc_classes[classname3 %in%
      c("artificial surfaces", "wetlands", "water bodies"), cols1],
    labels = clc_classes[classname3 %in% c("artificial surfaces", "wetlands", "water bodies"), classname],
    legend.is.portrait = TRUE, title = "Classes"
  ) +
  tm_layout(
    title = "Artificial surfaces, wetlands & water bodies",
    inner.margins = c(0.05, 0.05, 0.02, 0),
    outer.margins = c(0.05, 0.05, 0.02, 0),
    title.fontface = "bold",
    title.position = c(0.05, 0.05),
    frame = F,
    title.snap.to.legend = TRUE,
    legend.stack = "horizontal",
    legend.position = c(-0.2, .4),
    legend.title.fontface = "bold"
  ) -> p

tmap_save(p, filename = "output/figs/aww.png", width = 11, height = 8)


# TRI ----
tri = fread("data/processed/geodata/TRI_wilson_riley.csv",
  select = c("did", "mean_riley", "mean_wilson"),
  colClasses = c(did = "character")
)

merge(districts, tri, "did") |>
  tm_shape() +
  tm_fill(paste0("mean_", c("wilson", "riley")),
    style = "cont", palette = "-viridis",
    border.alpha = .05, lwd = .05,
    title = c("Wilson et.al (2007)", "Riley et.al (1999)")
  ) +
  tm_layout(
    frame = F,
    main.title = "TRI: Terrain Heterogeneity",
    main.title.position = "center"
  ) +
  tm_facets(free.coords = F)

tmap_save(
  filename = "output/figs/terrain-heterogeneity_TRI.png",
  width = 11, height = 9
)
