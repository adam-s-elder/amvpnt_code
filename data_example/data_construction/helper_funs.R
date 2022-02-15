#################################################
## Code for Data example of variable screening ##
## Brian Williamson wrote most of the code used #
## in the this script. It has been modified #####
## slightly to fit our needs. ###################
#################################################

get_nms_group_all_antigens <- function(X, assays, assays_to_exclude = "") {
  ## set all vars to be false
  vars <- rep(FALSE, ncol(X))
  ## set vars with assay in name to be true
  ## may be more than one
  for (i in seq_along(assays)) {
    if (assays_to_exclude != "") {
      vars[grepl(assays[i], names(X)) &
             !grepl(assays_to_exclude, names(X))] <- TRUE
    } else {
      vars[grepl(assays[i], names(X))] <- TRUE
    }
    if (assays[i] == "phago") {
      vars[grepl("ADCP1", names(X))] <- TRUE
    }

  }
  return(vars)
}

## ---------------------------------------------------------------------------------
## pre-process the data
## ---------------------------------------------------------------------------------
## read in the full dataset
make_data <- function(group_of_vars = "IgG3", exclude = "") {
  data("dat.505", package = "HVTN505")
  ## read in the super learner variables
  suppressWarnings(data("var.super", package = "HVTN505"))
  # even if there is a warning message, it still exists
  ## note that "var.super" contains individual vars for vaccine-matched antigens,
  ## and for vaccine-mismatched antigens, has either individual var (if only one)
  ## or PC1 and/or MDW (only PC1 if cor(PC1, MDW) > 0.9)

  ## scale vaccine recipients to have mean 0, sd 1 for all vars
  for (a in var.super$varname) {
    dat.505[[a]] <- scale(
      dat.505[[a]],
      center = mean(dat.505[[a]][dat.505$trt == 1]),
      scale = sd(dat.505[[a]][dat.505$trt == 1])
      )
    dat.505[[a %.% "_bin"]] <- scale(
      dat.505[[a %.% "_bin"]],
      center = mean(dat.505[[a %.% "_bin"]][dat.505$trt == 1]),
      scale = sd(dat.505[[a %.% "_bin"]][dat.505$trt == 1])
    )
  }

  ## Adjusting the baseline covariates
  for (a in c("age", "BMI", "bhvrisk")) {
    dat.505[[a]] <- scale(
      dat.505[[a]],
      center = mean(dat.505[[a]][dat.505$trt == 1]),
      scale = sd(dat.505[[a]][dat.505$trt == 1]))
  }

  ## set up X, Y for super learning
  X_markers <- dat.505 %>%
    dplyr::select(var.super$varname, paste0(var.super$varname, "_bin"))

  ## Get all of the names of matching biomarkers in the dataset.
  var_set <- get_nms_group_all_antigens(X_markers,
                                        assays = group_of_vars,
                                        assays_to_exclude = exclude)

  ## Select all of the columns with any of these names
  X_markers_varset <- X_markers %>% dplyr::select(names(X_markers)[var_set])

  ## Create a dataset with all baseline covariates (they are not
  ## used in this analysis, but are in the original analysis)
  X_exposure <- dat.505 %>% dplyr::select(age, BMI, bhvrisk)

  X <- data.frame(trt = dat.505$trt,# X_exposure,
                  X_markers_varset)

  ## Create a weight vector where weight is the inverse of the
  ## probability of being sampled (all cases are sampled).
  weights <- dat.505$wt
  Y <- dat.505$case
  ## check the above statement :
  ## dat.505[1:20, c("wt", "case")]

  ## Only keep data those who received treatment.
  vaccinees <- cbind.data.frame("y" = Y,
                                "wt" = weights,
                                "w" = X) %>%
    filter(w.trt == 1) %>% dplyr::select(-w.trt)
  return(vaccinees)
}
