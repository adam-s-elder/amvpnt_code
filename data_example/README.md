# Data Example

This folder contains the scripts for the

## Construction of the data (`data_construction`)

In this folder, the main script, `build_data.R` constructs the 10 different groupings of biomarkers for which we run a test.  Constructing the data requires the package `hvtn_505`.  The `helper_funs.R` script contains some of the functions used in `build_data.R`.  The package containing the data used in this analysis can be found [here](https://atlas.scharp.org/cpas/_webdav/HVTN%20Public%20Data/HVTN%20505/%40files/correlates%20analysis/HVTN505_2020-8-4.tar.gz?contentDisposition=attachment)

## Running the test (`data_analysis`)

In this folder, the main script conduct the test on each of the 10 different groupings of biomarkers created in the `data_construction` folder.  

## Plotting the results (`plot_results`)

Last, this folder provides the code used to create the plots and table for the data example in the paper.
