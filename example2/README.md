# Example 2 Simulations

This folder contains the scripts to recreate simulation results from example 2.

- `run_exp2.R`: This is the main script used to recreate the analysis.  The script is short, but has all of the required code to run the test in a single setting.  To run all simulation settings, we recommend using code from this script and iterating over all possible simulation settings.

- `exp2_funs.R`: This script contains defines functions used to create the data for example 2.

- plot: This folder contains the code used to create the plot for example 2.  The file `cleaned_res_examp2.csv` inside this folder contains the cleaned test results used to create the plots, though you may also use the code from `run_exp2.R` to evaluate test performance.