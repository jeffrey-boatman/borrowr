# borrowr

R package for estimating the population average treatment effect using a primary data source with borrowing from supplemental data sources.

To install from source and build vignettes:

devtools::install_github("jeffrey-boatman/borrowr", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"), force = TRUE)

To do:

  - pate fails if data = subset(...) and doesn't work with global
    variables. Fixed.
    
  - update pate to include adjustment for confounding due to noncompliance.
