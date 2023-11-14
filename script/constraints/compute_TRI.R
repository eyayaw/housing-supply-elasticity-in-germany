library(sf)
library(terra) # supercedes raster
library(tmap)

# faster raster extraction
# remotes::install_github('hunzikp/velox')
if (!requireNamespace("exactextractr")) {
  install.packages("exactextractr")
} else {
  library(exactextractr)
}

source("./script/helpers/helpers.R")

# summary function
summ_f = function(x, w = 1L, na.rm = T) {
  list(
    min = min(x, na.rm = na.rm),
    mean = weighted.mean(x, w = w, na.rm = na.rm),
    med = median(x, na.rm = na.rm),
    max = max(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm),
    n = length(x) # number of pixel cells in each polygon i.e. district
  )
}

# import data ----
# import German districts shape data, i.e. administrative borders
districts_shape = st_read("data/processed/admin-areas/admin-shapes/districts_shape.shp")

# import digital terrain model of Germany, resolution 200m
dtm = rast("data/raw/geodata/Digital-Terrain-Model/dgm200.utm32s.gridascii/dgm200/dgm200_utm32s.asc")
png("output/figs/Elevation_base-plot.png")
plot(dtm, main = "Elevation")
dev.off()

# raster extraction with geometric vector object: here polygons
## compute mean elevation at the district level from extracted raster cells
# crs = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
districts_shape = st_transform(
  districts_shape, terra::crs(dtm) # we use this as a selector
)

# elevation ----
elev = exact_extract(dtm, districts_shape)
elev_agg = data.frame(
  did = districts_shape$did,
  do.call(rbind, lapply(elev, \(x) summ_f(x$value, x$coverage_fraction, na.rm = TRUE))),
  range = sapply(elev, \(x) diff(range(x, na.rm = TRUE))),
  wn = sapply(elev, \(x) sum(x$coverage_fraction)) # weighted num of cells
)

data.table::fwrite(elev_agg, "data/processed/geodata/elevation.csv")

st_geometry(elev_agg) = st_geometry(districts_shape)

# Compute TRI terrain ruggedness index from elevation data ----
# 1) according to Wilson et al. (2007), as in gdaldem.
# TRI (Terrain Ruggedness Index) is the mean of the absolute differences between the value of a cell and the value of its 8 surrounding cells.
# 2) Riley et al. (1999)

## Wilson et al. (2007) ----
tri_wilson = terra::terrain(dtm, "TRI")

png("output/figs/TRI_wilson_base-plot.png")
plot(tri_wilson, main = "Terrain Ruggedness Index - Wilson et al. (2007)")
dev.off()

# TRI values need to be averaged over district for a TRI at the district level
# we use raster extraction: by district polygons
tri_wilson_agg = exact_extract(tri_wilson, districts_shape)
tri_wilson_agg = tri_wilson_agg |>
  (\(d) data.frame(
    do.call("rbind.data.frame", Map(\(x) summ_f(x$value, x$coverage_fraction, na.rm = T), d)),
    wn = sapply(d, \(x) sum(x$coverage_fraction)) # weighted num of cells
  ))()

st_geometry(tri_wilson_agg) = st_geometry(districts_shape)


## Riley et al. (1999) -----
## see https://www.researchgate.net/publication/259011943
tri_riley <- focal(dtm,
  w = matrix(1, nrow = 3, ncol = 3),
  fun = function(x) sqrt(sum((x[-5] - x[5])**2))
)

png("output/figs/TRI_riley_base-plot.png")
plot(tri_riley, main = "Terrain Ruggedness Index - Riley et al. (1999)")
dev.off()


# TRI values need to be averaged over district for a TRI at the district level

tri_riley_agg = exact_extract(tri_riley, districts_shape)
tri_riley_agg = tri_riley_agg |>
  (\(d) data.frame(
    do.call("rbind.data.frame", Map(\(x) summ_f(x$value, x$coverage_fraction, T), d)),
    wn = sapply(d, \(x) sum(x$coverage_fraction)) # weighted num of cells
  ))()

st_geometry(tri_riley_agg) = st_geometry(districts_shape)


# classification: using "equal area" classification method to group continuous
# ranges of TRI values into seven classes of unequal range, but equal area.

cats = "range;label
0-80;level
81-116;nearly level
117-161;slightly rugged
162-239;intermediately rugged
240-497;moderately rugged
498-958;highly rugged
959-Inf;extremely rugged"

cats = read.csv(text = cats, sep = ";", header = TRUE)
cats$upper = as.numeric(sub("(.*)-(.+)$", "\\2", cats$range))
tri_riley_agg$tri = cut(tri_riley_agg$mean, breaks = c(0, cats$upper), label = cats$label)

# show tri heterogeneity across space
tm_shape(tri_riley_agg) +
  tm_fill(
    "tri", style = "cat", title = "class", alpha = .5, drop.levels = TRUE, showNA = T
  ) +
  tm_borders(lwd = 0.1) +
  tm_layout(
    frame = F,
    legend.position = c(.6, .25),
    main.title = "Terrain Ruggedness Index (TRI) - Riley et al. (1999)",
    outer.margins = 0,
    inner.margins = c(0.01, 0, .1, 0.4),
    main.title.size = 1.2,
    main.title.fontface = "bold"
  ) + tm_credits(
    "Classification: using \"equal area\" classification method to group\ncontinuous ranges of TRI values into seven classes of unequal range, but equal area.",
    align = "left",
    position = c("left", "top")
  )

tmap_save(
  tmap_last(), ensure_dir("output/figs/TRI_by-cats.png"),
  width = 11, height = 9
)

# custom classification
# tri_riley_agg |>
#   transform(tri = cut(mean, 7)) |>
#   tm_shape() +
#   tm_fill("mean", n = 6, style = "cont", drop.levels = T, title = "TRI") +
#   tm_borders(lwd = 1 / 10) +
#   tm_layout(frame = F, legend.outside = T) +
#   tm_scale_bar(pos = c("RIGHT", "BOTTOM"))

tri_riley_agg$tri <- NULL

## merge TRIs from the two approaches
nms = names(tri_riley_agg)
mc = match("geometry", nms)
names(tri_wilson_agg)[-mc] = paste0(nms[-mc], "_wilson")
names(tri_riley_agg)[-mc] = paste0(nms[-mc], "_riley")

tri = cbind(tri_wilson_agg, st_drop_geometry(tri_riley_agg))
# tri = tri[, grep('^min|max|sd', names(tri), value = T, invert = T)]

tri$did = districts_shape$did
tri = tri[, c("did", setdiff(names(tri), "did"))]

# compare TRI (Wilson) and TRI (Riley)
cor(tri$mean_wilson, tri$mean_riley) # > .99 perfect correlation

tm_shape(tri) +
  tm_fill(paste0("mean_", c("wilson", "riley")),
    style = "cont", palette = "-viridis",
    border.alpha = .05, lwd = .05,
    title = c("Wilson et.al (2007)", "Riley et.al (1999)")
  ) +
  tm_layout(
    frame = FALSE,
    main.title = "TRI: Terrain Heterogeneity",
    main.title.position = "center"
  ) +
  tm_facets(free.coords = FALSE) -> p_tri

tmap_save(
  p_tri,
  ensure_dir("output/figs/terrain-heterogeneity_TRI.png"),
  width = 11, height = 9
)

data.table::fwrite(
  st_drop_geometry(tri), "data/processed/geodata/TRI_wilson_riley.csv"
)

