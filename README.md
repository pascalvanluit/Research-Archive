# Research Archive for Research Master Thesis

This is the Research Archive for my Research Master Thesis titled:
Improving Generalizability of Structural Equation Models through Cross-Validated Model Modifications

The repository is organized as follows:
  - Methods folder: Folder containing all the functions used by the different SEM modification and specification methods.
    The folder contains:
      - mod_adj_chisq_cv.R: Algorithm 3
      - mod_adj_mi.R: Algorithm 1
      - mod_adj_mi_cv.R: Algorithm 2
      - mod_no_adj.R: function to fit the baseline model (No Mod).
      - modindices_cv.R: function to obtain k OOS MIs.
      - modindices_train.R: function to obtain MIs which lead to mean significant OOS chi-square fit.
      
  - Simulation study folder: Folder containing all the script to run the simulation study.
    The folder contains:
      - 00_conditions.rds: data frame with conditions for simulating datasets.
      - 00_create_conditions_grid.R: script for creating conditions grid (00_conditions.rds).
      - 01_simulation.R: script for creating simulated datasets:
      - 01_sim_data.rds: all simulated datasets.
      - 02_Modelling.R: script for running all 7 methods on each simulated dataset. Creating models and lavaan fit objects.
      - 03a_Analysis.R: script for computing all MSE's of the POI and distances of modelled covariance matrices to true covariance matrix.
      - 03b_Analysis.R: script for compiling all results from all methods.
      - 04_Tables_and_Figures.R: script for creating all results tables and figures found in the paper.
      - Functions folder: A folder which contains functions used in the simulation study analysis.
          - create_true_covmat: function to create the true covariance matrix for each condition.
          
