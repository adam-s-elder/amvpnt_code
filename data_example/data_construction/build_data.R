## Check if the needed packages are installed:
installed_packs <- rownames(installed.packages())
needed_packs <- setdiff(c("amp", "HVTN505", "kyotil",
                          "ggrepel", "tidyverse", "cowplot"),
                        installed_packs)

## If the package with the paper's method has not been
## downloaded yet, do so.
if ("amp" %in% needed_packs) {
  if (!("remotes" %in% installed_packs)) {
    install.packages("remotes")
  }
  cat("\n Installing the required test function package \n")
  remotes::install_github("https://github.com/adam-s-elder/amp")
}

## If the needed data has not been downloaded yet, do so
if ("HVTN505" %in% needed_packs) {
  package_loc <- "https://atlas.scharp.org/cpas/_webdav/HVTN%20Public%20Data/HVTN%20505/%40files/correlates%20analysis/HVTN505_2020-8-4.tar.gz?contentDisposition=attachment"
  cat("\n Installing the required data package \n")
  download.file(package_loc, destfile = "hvtn505.tar.gz")
  install.packages("hvtn505.tar.gz", repos = NULL)
}

cran_packs <- setdiff(needed_packs, c("HVTN505", "amp"))

if (length(cran_packs) > 0) {
  cat("The following packages need to be installed from cran : ",
      paste0(cran_packs, "\n"),
      "These packages can be installed using the ",
      "install.packages function")
}

## Library the required packages:
library(tidyverse)
library(ggrepel)
library(kyotil)
library(amp)
library(HVTN505)

## Load in the needed functions:
source("helper_funs.R")

## Create the data for each of the
## desired biomarkers

assays <- unique(var.super$assay)
antigens <- unique(var.super$antigen)

# 1
data_igg_igga <- make_data(c("IgG", "IgA"), exclude = "IgG3")
# 2
data_igg3 <- make_data("IgG3")
# 3
data_Tcells <- make_data(c("CD4", "CD8"))
# 4 Fx Ab (all antigens)
data_Fx_ab <- make_data(c("phago", "R2a", "R3a"))
# 5. 1+2
data_igg_3_igga <- make_data(c("IgG", "IgA", "IgG3"))
# 6. 1 + 3
data_igg_iga_t <- make_data(c("IgG", "IgA", "CD4", "CD8"), exclude = "IgG3")
# 7. 1 + 2 + 3
data_iga_igg_3_t <- make_data(c("IgG", "IgA", "IgG3", "CD4", "CD8"))
# 8. 1+2+3+5
data_igg_igga_pc_Fx <- make_data(c("IgG", "IgA", "IgG3", "phago", "R2a", "R3a"))
# 9. 1+4+5
data_t_pc_Fx <- make_data(c("CD4", "CD8", "phago", "R2a", "R3a"))
data_all <- make_data(colnames(HVTN505::dat.505))

all_data <- list(
  "IgG + IgA" = data_igg_igga,
  "IgG3 (Immuno Globulin G3 Group)" = data_igg3,
  "T Cells" = data_Tcells,
  "Fx Ab" = data_Fx_ab,
  "IgG + IgA + IgG3" = data_igg_3_igga,
  "IgG + IgA + T Cells" = data_igg_iga_t,
  "IgG + IgA + IgG3 + T Cells" = data_iga_igg_3_t,
  "IgG + IgA + IgG3 + Fx Ab" = data_igg_igga_pc_Fx,
  "T Cells + Fx Ab" = data_t_pc_Fx,
  "All markers" = data_all
)

saveRDS(all_data, "all_data.RDS")
