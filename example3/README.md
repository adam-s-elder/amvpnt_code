# Example 3 Simulations

This folder contains the scripts to recreate simulation results from example 3.

- `run_exp3.R`: This is the main script used to recreate the analysis.  The script is short, but has all of the required code to run the test in a single setting.  To run all simulation settings, we recommend using code from this script and iterating over all possible simulation settings.

- `exp3_funs.R`: This script contains defines functions used to create the data for example 3.

- plot: This folder contains the code used to create the plot for example 3.  The file `cleaned_res_examp3.csv` inside this folder contains the cleaned test results used to create the plots, though you may also use the code from `run_exp3.R` to evaluate test performance.