# Order of files:

## Simulation:

1. jobs.sh can be run with SLURM, make sure dependencies are installed, and
hpc_functions.R and blk1_jobs.R are in the same directory. Make sure there is
an out directory and a data directory in whatever environment you are working 
in. If you do not have a similar high performance computing environment 
available, you can try running blk1_jobs.R locally with nrep = 10,000, but
it may take very long.

2. After code has run, run data_processing.R to get reformatted data.

3. Then run plots.R and tables.R to obtain figures and tables, respectively.

## Real Data:

1. Run sp500.R to get table.

2. We reformatted the table manually in LaTeX.

# What each file does:
copula.R: shows how we computed the lag-1 autocorrelations for normal and gamma
series

tacvfARMA.R: True Auto-Covariance Function of an ARMA Process

ksfitted.R: functions for semi-parametric bootstrap

jobs.sh: shell script to submit to SLURM Scheduler

data_processing.R: process results after running jobs

tables.R: produce tables after processing data

hpc_functions.R: functions used in SLURM jobs

functions.R: functions used to produce figures locally

plots.R: produce figures after processign data

sp500.R: applied data analysis of S\&P 500 data

blk1_jobs.R: jobs which are submitted with jobs.sh, can also be run locally

blk2004_analysis.R: analysis of Politis 2004 automatic procedure results

test_functions.R: functions to be used on R Compute server to run Politis 2004


