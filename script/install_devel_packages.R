
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")

## projections package
missing_devel_projections <-
  !("projections" %in% installed.packages()[, "Package"]) ||
  packageVersion("projections") < "0.4.1"

if (missing_devel_projections) {
  remotes::install_github("reconhub/projections")
}



## incidence package
missing_devel_incidence <-
  !("incidence" %in% installed.packages()[, "Package"]) ||
  packageVersion("incidence") < "1.7.1"

if (missing_devel_incidence) {
  remotes::install_github("reconhub/incidence@issue_119")
}
