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
      - 00_conditions.rds: 
