# Order of files:

## Simulation:

1. jobs.sh can be run with SLURM, make sure dependencies are installed, and
hpc_functions.R and blk1_jobs.R are in the same directory. Make sure there is
an out directory and a data directory in whatever environment you are working 
in.

2. After code has run, run data_processing.R to get reformatted data.

3. Then run plots.R and tables.R to obtain figures and tables, respectively.

## Real Data:

1. Run sp500.R to get table.

2. We reformatted the table manually in LaTeX.
