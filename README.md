

<!-- README.md is generated from README.qmd. Please edit that file -->

# Replication code for my paper:

[**Geographic Constraints and the Housing Supply Elasticity in Germany**
— Ruhr Economic Papers
\#1003](https://www.rwi-essen.de/en/publications/scientific/ruhr-economic-papers/detail/geographic-constraints-and-the-housing-supply-elasticity-germany)

> You can find the <span style="color: firebrick;">latest version</span>
> in this repo
> [Beze_2023_the-Price-Elasticity-of-Housing-Supply-in-Germany.pdf](Beze_2023_the-Price-Elasticity-of-Housing-Supply-in-Germany.pdf).
> View it
> [here](https://docs.google.com/viewer?url=https://github.com/eyayaw/housing-supply-elasticity-in-germany/raw/main/Beze_2023_the-Price-Elasticity-of-Housing-Supply-in-Germany.pdf).

> [!WARNING]
>
> `RWI-GEO-RED` data is not included in this repository. Thus, you need
> access to the data to reproduce this project.

# Requirements

- `R` 4.3.3

- `R packages`:

  - `data.table` devel 1.15.99
  - more packages listed in [`renv.lock`](./renv.lock) file

  <details>

  This project depends heavily on the awesome
  [`data.table`](https://github.com/Rdatatable/data.table) package.

  <div class="cell-output-display">

  | pkg          | num_files |
  |:-------------|----------:|
  | `rmarkdown`  |        31 |
  | `data.table` |        30 |
  | `knitr`      |        13 |
  | `sf`         |        10 |
  | `readxl`     |        10 |
  | `tools`      |         9 |
  | `bookdown`   |         5 |
  | `tibble`     |         5 |
  | `ivreg`      |         4 |
  | `ggplot2`    |         4 |

  </div>

  </details>

- [quarto](https://quarto.org) to render the manuscript

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

3.  The `index.qmd` file generates the manuscript.

    - Render it with `Cmd+Shift+K` in RStudio/VS Code. Or, run the
      following code in the terminal:

      ``` bash
      quarto render index.qmd
      # move figure notes out of the caption
      python3 tidy_figure_notes.py index.tex Beze_2023_the-Price-Elasticity-of-Housing-Supply-in-Germany.pdf
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

``` bibtex
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
