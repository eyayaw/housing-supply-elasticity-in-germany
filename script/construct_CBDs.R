
#==============================================================================#
# Construct central business districts based on the most populous municipality in the district
#==============================================================================#
library(data.table)
library(sf)
source('./script/helpers/helpers.R')

# list of municipalities
municipals = fread("data/processed/admin-areas/municipalities_bkg.csv",
  select = setNames(rep("character", 3L), c("mid", "name", "did"))
)

municipal_shape = st_read("data/processed/admin-areas/admin-shapes/municipalities_shape.shp")


# import municipality population ----
pop = fread("data/processed/main/population_municipality.csv",
  colClasses = list("character" = "mid", "character" = "did")
)
setkey(pop, mid, year) # order by municipality id and year

# CBDs based on max average population, over the entire period 2008-2019
CBDs =
  pop[, .(pop_ave = round(mean(filled_pop_tot))), mid] |> # ave pop over years by mid
  merge(municipals, by="mid", all.x=TRUE) |> # get the district id `did`
  DT(, .(CBD_mid = mid[which.max(pop_ave)]), did) # the largest municipality in the district

CBDs = CBDs[municipals, on=c("CBD_mid==mid", "did"), nomatch=NULL]
setnames(CBDs, "name", "CBD_name")

# CBDs per year in each district based on yearly
cbds = pop[, .(CBD_mid = mid[which.max(filled_pop_tot)]), .(did, year)]

# bring shapes of municipalities
CBDs = CBDs[municipal_shape[, c("mid", "geometry")], on="CBD_mid==mid", nomatch=0] |>
  st_as_sf()


st_write(CBDs, ensure_dir("data/processed/geodata/CBDs.shp"), append = F)
