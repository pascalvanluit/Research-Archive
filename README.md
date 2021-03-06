# Research Archive for Research Master Thesis

This is the Research Archive for my Research Master Thesis titled:
Improving Generalizability of Structural Equation Models through Cross-Validated Model Modifications

## Abstract
Structural equation modelling (SEM) is a popular modelling tool in the behavioral and social sciences. SEM models are often modified with the aid of modification indices; which are computed based on a sample dataset. This practice comes with a risk for overfitting to the sample dataset. To prevent overfitting, this paper proposes SEM model modification methods in combination with cross-validation. A simulation study is run to assess the performance of the standard modification method with the proposed cross-validation methods. Results indicate that cross-validating model modifications in SEM can be effective in obtaining a lower Mean Squared Error of estimating a parameter of interest.

## Archive description
The Research Archive is available at https://github.com/pascalvanluit/Research-Archive. The GitHub repository is public and open-source. The owner is Pascal van Luit (p.j.vanluit@uu.nl). Please contact Pascal for any questions or comments regarding the repository. A copy of this repository has been stored in the archives of Utrecht University. The data will be stored for a minimal duration of ten years.

The repository contains two main folders ("Methods" and "Simulation Study") and the two following files:
  - van Luit,PJ - MSBBSS.pdf: thesis manuscript.
  - approval.pdf: ethical approval for the project.
    - This study is approved by the Ethics Committee of the Faculty of Social and Behavioural Sciences of Utrecht University, filed under number 19-222.

### Methods folder:
Folder containing all the functions used by the different SEM modification and specification methods.
The folder contains:
   - mod_adj_chisq_cv.R: Algorithm 3
   - mod_adj_mi.R: Algorithm 1
   - mod_adj_mi_cv.R: Algorithm 2
   - mod_no_adj.R: function to fit the baseline model (No Mod).
   - modindices_cv.R: function to obtain k OOS MIs.
   - modindices_train.R: function to obtain MIs which lead to mean significant OOS chi-square fit.
      
### Simulation Study folder:
Folder containing all scripts to run the simulation study. The simulation study can be replicated by re-running all scripts in the order as they are listed below.
The folder contains:
   - 00_conditions.rds: data frame with conditions for simulating datasets.
   - 00_create_conditions_grid.R: script for creating conditions grid (00_conditions.rds).
   - 01_simulation.R: script for creating simulated datasets:
   - 01_sim_data.rds: all simulated datasets.
   - 02_Modelling.R: script for running all 7 methods on each simulated dataset. Creating models and lavaan fit objects.
   - 03a_Analysis.R: script for computing all MSE's of the POI and covariance matrix distances.
   - 03b_Analysis.R: script for compiling all results from all methods.
   - 04_Tables_and_Figures.R: script for creating all results tables and figures found in the paper.
   - *Functions folder*: Folder containing functions used in the simulation study analysis.
      - create_true_covmat.R: function to create the true covariance matrix for each condition.
      - poi.R: function to find the estimated value of the parameter of interest for each iteration.
   - *Results folder*: Folder containing all results which are used in analyses.
      - 02_results_mod_adj_chisq_cv_10.rds: results of Algorithm 3 (minimum MI = 10)
      - 02_results_mod_adj_chisq_cv_4.rds: results of Algorithm 3 (minimum MI = 4)
      - 02_results_mod_adj_mi_10.rds: results of Algorithm 1 (minimum MI = 10)
      - 02_results_mod_adj_mi_4.rds: results of Algorithm 1 (minimum MI = 4)
      - 02_results_mod_adj_mi_cv_10.rds: results of Algorithm 2 (minimum MI = 10)
      - 02_results_mod_adj_mi_cv_4.rds: results of Algorithm 2 (minimum MI = 4)
      - 02_results_mod_no_adj.rds: results of No Mod method.
      - all_results.rds: Results of all 7 methods.
          
