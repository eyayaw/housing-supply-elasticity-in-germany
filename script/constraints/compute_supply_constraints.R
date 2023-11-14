library(sf)
library(terra) # supersedes raster
library(exactextractr) # faster raster extraction
# library(units)         # setting and getting units
library(data.table)


# compute developable and undevelopable land fractions ------------------------
## extract the exact fraction of land that is or is not developable
## undevelopable area =  built-up, water, wetlands, and slope > 15%


## helpers ----
source("script/helpers/helpers.R")

select_base <- `[` # base select for uses in pipes

# safely compare floating point numbers, taken from dplyr::near
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}


# import data ----

# digital terrain model of Germany, resolution 200m
dtm = rast("data/raw/geodata/Digital-Terrain-Model/dgm200.utm32s.gridascii/dgm200/dgm200_utm32s.asc")
# crs_dtm = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
crs_dtm = terra::crs(dtm, proj = TRUE)


# German districts' shapes, i.e. administrative borders
## we'll use shapes (districts) to extract elev/slope from the dtm raster below
shapes = st_read("data/processed/admin-areas/admin-shapes/districts_shape.shp") |>
  select_base(j = c("did", "geometry")) |>
  st_transform(crs_dtm)


# param
pix_area = m2(prod(res(dtm))) # area of a pixel (in m^2), res (200x200)

# aggregation function
agg_fun = function(val, cfrac) {
  masked = near(val, maskval_perc)
  nan = is.nan(val)
  list(
    # weighted num pixels
    # wnpix = sum(cfrac, na.rm = TRUE), # pixel_area * wnpix == admin_area of district

    ## stock of land avail for development: agri + forest + (artificial if not a masker)
    # pixel_area * wn, wn = weighted number of cells, excludes cells w/ NaN slope
    area_avail = pix_area * sum(cfrac[!(nan | masked)]),

    # pixel_area * sum of weighted fraction of the masked cells, potentially, equals
    # [artificial + wet + water] or [wet + water] => (the masked part above)
    area_masked = pix_area * sum(cfrac[masked], na.rm = TRUE),

    # weighted NA|NaN count
    area_nan = pix_area * sum(cfrac[nan], na.rm = TRUE),

    # pixel_area * count of cells with slope > 15 %
    area_gr_15 = pix_area * sum(cfrac * (val > 15), na.rm = TRUE)
  )
}


## Corine land cover classes
# 37 classes at disaggregated, 5 classes at aggregated level
clc_classes = fread("data/raw/geodata/corine-land-cover/clc-classification.csv") |>
  select_base(j = c("clc_code", "classname", "classname3", "cols1", "cols3"))

clc_classes = cbind(clc_classes, t(col2rgb(clc_classes$cols1, TRUE)))

## covers
fpaths = c(
  "clc-2006/U2012_CLC2006_V2020_20u1.tif",
  "clc-2018/U2018_CLC2018_V2020_20u1.tif"
)
names(fpaths) = substr(fpaths, 1, 8)

# compute the fraction of area with slope > 15 % ----

# computation -----
## steps ----
## step 0: compute slope ----
slope = terrain(dtm, "slope", unit = "radians", neighbors = 8)


## step 1: construct maskers -----

CLCs <- lapply(fpaths, \(fpath) {
  clc <- rast(sprintf("data/processed/geodata/clc-germany/%s", fpath))
  if (crs(clc, proj = TRUE) != crs_dtm) {
    clc <- project(clc, dtm, mask = TRUE)
  }

  Cats <- cats(clc)[[1]] # categories of the raster (factor) values
  Cats <- Cats[!is.na(Cats[, grep("^code_", names(Cats), value = TRUE)]), ]
  list(clc = clc, Cats = Cats)
})

for (i in seq_along(CLCs)) {
  ## area covered by land cover classes classes
  # extract values by districts
  clc_extract = exact_extract(CLCs[[i]]$clc, shapes, include_cols = "did") |>
    rbindlist(use.names = TRUE)
  clc_extract = clc_extract[,
    .(area = sum(coverage_fraction) * pix_area),
    .(did, code = as.integer(get_code(CLCs[[i]]$clc, value)))
  ]
  # aggregate by district and land cover class
  clc_extract_agg = clc_extract[, .(area = sum(area)), .(did, code3 = code %/% 100)]
  clc_extract_agg = dcast(clc_extract_agg, did ~ code3, value.var = "area")
  setnames(clc_extract_agg,
    c("NA", 1:5),
    c("na", "artificial", "agri", "forest", "wetlands", "water"))

  ## separate some land cover classes
  maskers = c(`artificial-wetlands-water` = "^(1|4|5)", `wetlands-water` = "^(4|5)")
  for (j in seq_along(maskers)) {
    fac_ids = get_id(CLCs[[i]]$clc, subset(clc_classes, grepl(maskers[[j]], clc_code))$clc_code)
    m = with(CLCs[[i]]$Cats, cbind(id, ifelse(id %in% fac_ids, id, NA)))
    clc_masker = classify(CLCs[[i]]$clc, m)


    ## step 3: clip or mask the slope raster by the masker raster ----

    # first, keep the slope raster aligning with district shapes,
    # then, remove polygons that are masked (given by the selected masker classes)

    # replace the slope value of the masked cells with
    maskval_rad = -pi / 4 # rads (-45 degree) -> slope -100
    maskval_perc = rad2perc(maskval_rad)

    # mask by selected masker classes above and keep the unmasked part
    slope_clip = mask(slope, clc_masker, updatevalue = maskval_rad, inverse = TRUE)
    values(slope_clip) = rad2perc(values(slope_clip)) # slope_% = tan(slope_rad)*100

    ## step 4: aggregate pixel level slope to the district level -----

    # This involves extracting slope values from the raster that are covered by the
    # district polygons: using a summary function (e.g. mean) then, we can aggregate
    # pixel level slope values to the district level.
    #
    # Detailed steps:
    # First, extract the area covered by each district in the slope raster.
    # This area of the district excludes the area covered with (1) wet land,
    # (2) water bodies, and (3) artificial surfaces, depending on the selected maskers.
    # We achieve this by masking cells in the slope raster with Corine Land Cover
    # raster with these three categories.
    # Next, for each district, we find the sum of the area that exhibits slope greater
    # than 15%--out of the area that is developable (i.e., not masked).

    # We define the total developable stock as the area of the district that is not
    # covered by wetlands and water bodies(, and artificial surfaces).
    # We are interested in computing what fraction of this quantity exhibits
    # slope greater than 15%.
    # Then, the ratio of (the sum of wetlands, water, and 15%>) =: total undevelopable land
    # to total land gives us the fraction of land that is undevelopable.
    #

    slope_extract = exact_extract(slope_clip, shapes, include_cols = "did")
    # bind data frames, the above returns a list of data frames one for each district
    slope_extract = rbindlist(slope_extract, use.names = TRUE)

    setnames(slope_extract, c("value", "coverage_fraction"), c("val", "cfrac"))

    # min, mean, med, max, sd, n
    slope_agg = slope_extract[!(near(val, maskval_perc) | is.nan(val)),
      .(slope_mean = weighted.mean(val, cfrac), slope_range = diff(range(val))),
      did
    ]
    slope_agg = merge(slope_agg, slope_extract[, agg_fun(val, cfrac), did], "did")

    slope_agg[, area := rowSums(cbind(area_avail, area_masked, area_nan), na.rm = TRUE)]

    # the above area should be equal to the administrative area
    merge(slope_agg, data.frame(did = shapes$did, area_admin = st_area(shapes)), "did") |>
      with(stopifnot(all.equal(area, as.numeric(area_admin))))

    # what fraction of the developable stock exhibits slope > 15%?
    # frac_area_gr_15_dev = area_gr_15/area_avail, where area_avail = wn * prod(res(dtm))
    # slope_agg[, "frac_area_gr_15_dev" := area_gr_15 / area_avail]


    ## step 5: compute undevelopable fraction ----

    # what fraction of the total area of a district is undevelopable?
    constraints = merge(slope_agg, clc_extract_agg, "did")
    # undevelopable fraction according to our definition above
    constraints[, unavail_frac := (1 / area) * rowSums(cbind(area_gr_15, area_masked), na.rm = TRUE)]

    # check if undevelopable fraction is above 1, it should not be, of course!
    stopifnot(max(constraints$unavail_frac) <= 1L)

    fwrite(constraints,
      sprintf("data/processed/geodata/constraints_%s_%s_masked.csv",
        names(fpaths)[[i]], names(maskers)[[j]]
      )
    )

  }

  fwrite(clc_extract, sprintf(
    "data/processed/geodata/land-cover_area_%s_by-clc-class1.csv",
    names(fpaths)[[i]]
  ))

}


# merge ----

## land cover area by class 1 ----

cover_area1 = sprintf(
  "data/processed/geodata/land-cover_area_%s_by-clc-class1.csv",
  names(fpaths)
) |>
  setNames(substr(fpaths, 5, 8)) |>
  lapply(fread, keepLeadingZeros = TRUE) |>
  rbindlist(use.names = TRUE, idcol = "year")

fwrite(cover_area1, "data/processed/geodata/land-cover_area_by-clc-class1.csv")

const_aww <-
  sprintf(
    "data/processed/geodata/constraints_%s_artificial-wetlands-water_masked.csv",
    names(fpaths)
  ) |>
  setNames(substr(fpaths, 5, 8)) |>
  lapply(fread, keepLeadingZeros = TRUE) |>
  rbindlist(use.names = TRUE, idcol = "year")

const_ww <- sprintf(
  "data/processed/geodata/constraints_%s_wetlands-water_masked.csv",
  names(fpaths)
) |>
  setNames(substr(fpaths, 5, 8)) |>
  lapply(fread, keepLeadingZeros = TRUE) |>
  rbindlist(use.names = TRUE, idcol = "year")

selected = c("area", "artificial", "agri", "forest", "wetlands", "water", "na")
constraints <- merge(const_aww[, !..selected], const_ww,
  by = c("did", "year"), suffixes = c("_aww", "_ww")
)

setcolorder(constraints, c(
  "did", "year", selected,
  "area_avail_aww", "area_avail_ww", "area_gr_15_aww", "area_gr_15_ww",
  "area_masked_aww", "area_masked_ww", "area_nan_aww", "area_nan_ww",
  "slope_mean_aww", "slope_range_aww", "slope_mean_ww", "slope_range_ww",
  "unavail_frac_aww", "unavail_frac_ww"
))

fwrite(constraints, "data/processed/geodata/constraints.csv")
