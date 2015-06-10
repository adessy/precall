precall
=======

`precall` is a simple R package to plot precision-recall curves and to compute the Area Under Precision-Recall curves (AUPR).

You can install the package with:

``` r
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("hadley/assertthat")
devtools::install_github("adessy/precall")
```

To go through a few examples, you can read the vignette : `vignette("precall", package = "precall")`
