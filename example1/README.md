# Example 1 Simulations

This folder contains the scripts to recreate simulation results from example 1.

- `run_exp1.R`: This is the main script used to recreate the analysis.  The script is short, but has all of the required code to run the test in a single setting.  To run all simulation settings, we recommend using code from this script and iterating over all possible simulation settings.

- small_pvals: This folder contains the scripts used to investigate (and plot) the large type one errors observed in example 1.  

- main_plots: This folder contains the code used to create the main analysis plots for example 1.  The file `cleaned_res_examp1.csv`  inside this folder contains the cleaned test results used to create the plots, though you may also use the code from `run_exp1.R` to evaluate test performance.
