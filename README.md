
<!-- README.md is generated from README.qmd. Please edit that file -->

# Replication code for my paper:

[**Geographic Constraints and the Housing Supply Elasticity in Germany**
— Ruhr Economic Papers
\#1003](https://www.rwi-essen.de/en/publications/scientific/ruhr-economic-papers/detail/geographic-constraints-and-the-housing-supply-elasticity-germany)

> You can find the latest version
> [here](https://uni-duisburg-essen.sciebo.de/s/km7tQzcygRQjwp6).

<div>

> **Warning**
>
> `RWI-GEO-RED` data is not included in this repository. Thus, you need
> access to the data to reproduce this project.

</div>

# Requirements

- `R` 4.3.2

- `R packages`:

  - `data.table` devel 1.14.9
  - more packages listed in [`renv.lock`](./renv.lock) file

  <details>

  This project depends heavily on the awesome
  [`data.table`](https://github.com/Rdatatable/data.table) package.

  <div id="tbl-top-packages">

  | pkg          | num_files |
  |:-------------|----------:|
  | `data.table` |        29 |
  | `rmarkdown`  |        15 |
  | `knitr`      |        10 |
  | `readxl`     |        10 |
  | `sf`         |         9 |
  | `tools`      |         8 |
  | `bookdown`   |         5 |
  | `tibble`     |         5 |
  | `terra`      |         4 |
  | `ivreg`      |         3 |

  Table 1: Dependencies - Top packages

  </div>

  </details>

# Usage

1.  Data

    1.  Housing price: Accessed the data from the [RWI FDZ at
        Ruhr](https://fdz.rwi-essen.de/en/doi-detail/id-107807immoredhksufv3)

    2.  Geodata:

        - [administrative
          boundaries](https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete.html?___store=default)
          from the [Federal Agency for Cartography and
          Geodesy](https://gdz.BKG-bund.de),
          [destatis.de](https://destatis.de) and
          [inkar.de](https://inkar.de)

        - land cover data from the [Copernicus Land Monitoring
          Service](https://land.copernicus.eu/en/dataset-catalog?b_size=10&query=%5B%7B%22i%22:%22portal_type%22,%22o%22:%22paqo.selection.any%22,%22v%22:%5B%22DataSet%22%5D%7D,%7B%22i%22:%22SearchableText%22,%22o%22:%22paqo.string.contains%22,%22v%22:%22land%20cover%22%7D%5D)

        - [Digital Terrain Model of Germany
          (DGM200)](https://daten.gdz.bkg.bund.de/produkte/dgm/dgm200/aktuell/dgm200.utm32s.gridascii.zip)
          from BKG

    3.  regional data from [Regionaldatenbank
        Deutschland](https://www.regionalstatistik.de/genesis/online)

2.  R scripts: The order in which the scripts should be run is provided
    in [`main.R`](main.R).

3.  The `.Rmd` files generate the manuscript.

    - Render it with `Cmd+Shift+B` in RStudio. Or, run the following
      code in `R` console:

      ``` r
      rmarkdown::render_site(encoding = "UTF-8")
      ```

## The price elasticity of housing supply estimates

The main results are provided in
`Table 2: IV Results: Housing Supply Elasticity Estimates` of the
manuscript.

If you want to calculate elasticity estimates for each county (kreis),
compute “developed fraction” and “undevelopable fraction” at the county
level. Then, use the coefficients estimated from the regression output
for each variable to obtain estimates at the county level. It is
important to note that the variation across counties is solely
attributable to these two variables.

$$
\begin{aligned}
&\Delta\ln H_i^s = \varepsilon^S\Delta\ln P_i + \beta^{\text{Developed}} \Delta \ln P_i\times\text{Developed}_i +
\beta^{\text{Unavail}} \Delta \ln P_i\times\text{Unavail}_i + \boldsymbol{\beta}\mathbf{X}_i^S + u^S_i\\
&\implies \varepsilon^S_i = \varepsilon^S + \beta^{\text{Developed}}\text{Developed}_i + \beta^{\text{Unavail}}\text{Unavail}_i\,
\end{aligned}
$$

where $i$ can be any spatial scale.

Please look at the
[`script/constraints/compute_supply_constraints.R`](./script/constraints/compute_supply_constraints.R)
for constructing constraints at the county level.

------------------------------------------------------------------------

**Please email me if you find any mistakes or have any questions about
the project.**

#### Citation

BibTeX entry:

``` tex
@book{beze_2023,
    address = {DE},
    title = {Geographic {Constraints} and the {Housing} {Supply} {Elasticity} in {Germany}},
    copyright = {All rights reserved},
    isbn = {978-3-96973-169-7},
    url = {https://doi.org/10.4419/96973169},
    publisher = {RWI},
    author = {Beze, Eyayaw},
    year = {2023},
}
```

For attribution of this repository, please cite the paper.
