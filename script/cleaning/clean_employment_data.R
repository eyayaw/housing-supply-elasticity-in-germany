library(data.table)

## industry classifications -----
## look at script/economic_classification_WZ2008.R
## there are 7 groups of industries
industry_class <- data.frame(
  sector = c(
    "agriculture,_forestry,_fishing_(a)",
    "manufacturing_industry_excluding_construction_(b_e)",
    "manufacturing_industry_(c)",
    "construction_industry_(f)",
    "trade,_transport,_hospitality,_information_/_communication",
    "fin_,_vers_,_unt_dienstl_,_landst_/_housing",
    "public_and_other_services,_education,_health"
  ),
  sector_label = c(
    "Agri, forestry, and fishing",
    "Mining & quarrying, energy, and water, sewage & waste",
    "Manufacturing",
    "Construction",
    "Trade, transport, hospitality, and info & communication",
    "Finance & insurance, and real estate",
    "Public & other services, educ, and health"
  ),
  sector_id = paste0("k_", 1:7)
)

label_industry <- function(x) {
  factor(x, industry_class$sector_id, industry_class$sector_label)
}

# cleans employment data
## (national or regional) and (including or excluding self employment)
## data comes in wide format: reshapes to long
    # each industry employment being a column to long: c(id, sector, value)
## in addition, it removes total from industry category
tidy_df <- function(df) {
  stopifnot(is.data.table(df))
  ind_ids = paste0("k_", 1:7) # seven industries/sector

  names(df)[grepl("emp_1000_(inc|exc)_selfemp_.*[^total]$", names(df))] = ind_ids
  names(df)[grepl("total$", names(df))] = "total"
  df = df[, !"total"] # drop the total category

  # `Produzierendes Gewerbe ohne Baugewerbe (B-E)` includes C (i.e., manufacturing)
  #  which by itself is separately given so we need to subtract C off from B-E group.
  #  We can check that by comparing the total with the sum of sector 1-7.
  #  The latter is greater than the former by the size of C --> double counting.
  #  Compare `total` and `tot`:
  #  df[, .(total,tot=rowSums(df[, 5:11]), k_3)][,.(.SD, diff=tot-total)]

  df$k_2 <- df$k_2 - df$k_3 # remove C from B-E
  ## reshape to long
  id_vars = c("did", "year")
  df = melt(df, id.vars = id_vars, variable.name = "sector")

  df[, value := value * 1000] # the employment values come in 1000s
  df[value == 0, value := 1]  # avoid explicit zeros
  df[, sector := label_industry(sector)]
  df
}


## file paths
fpath = "data/processed/main/employment-share_by-industry_inc-exc-self--employed_yearly-average-1000%s.csv"

select = c(
  "did", "year",
  paste0("emp_1000_%s_selfemp_", c(industry_class$sector, "total"))
)

level = c("nat", "reg") # national vs regional
type = c("inc", "exc")  # includes vs excludes self-employment
emp_data_raw = vector("list", length(type))
names(emp_data_raw) = type

for (i in seq_along(type)) {
  ## national
  nat = fread(sprintf(fpath, "_national"),
    select = sprintf(select, type[[i]]), encoding = "Latin-1"
  )
  nat = nat[did == "DG", ]
  nat = tidy_df(nat)[, !"did"]
  ## regional
  reg = fread(sprintf(fpath, ""), select = sprintf(select, type[[i]]), encoding = "UTF-8")
  reg = tidy_df(reg)
  emp_data_raw[[i]] = list(nat = nat, reg = reg)
}


# write disk
for (x in c("inc", "exc")) {
  for (y in c("nat", "reg")) {
    fwrite(
      emp_data_raw[[x]][[y]],
      sprintf(
        "data/processed/main/%s-employment_by-industry_%s-self-employment.csv",
        grep(y, c("national", "regional"), value=TRUE),
        x
      )
    )
  }
}
