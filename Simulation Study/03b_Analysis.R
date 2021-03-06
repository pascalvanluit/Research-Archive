library(tidyverse)

#########################################################
## Script for compiling all results from all 7 methods ##
#########################################################

# Loading all results:
res_mod_no_adj          <- read_rds("Simulation study/Results 01-05/02_results_mod_no_adj.rds") 
res_mod_adj_mi_4        <- read_rds("Simulation study/Results 01-05/02_results_mod_adj_mi_4.rds")  
res_mod_adj_mi_10       <- read_rds("Simulation study/Results 01-05/02_results_mod_adj_mi_10.rds")  
res_mod_adj_mi_cv_4     <- read_rds("Simulation study/Results 01-05/02_results_mod_adj_mi_cv_4.rds")  
res_mod_adj_mi_cv_10    <- read_rds("Simulation study/Results 01-05/02_results_mod_adj_mi_cv_10.rds")
res_mod_adj_chisq_cv_4  <- read_rds("Simulation study/Results 01-05/02_results_mod_adj_chisq_cv_4.rds")
res_mod_adj_chisq_cv_10 <- read_rds("Simulation study/Results 01-05/02_results_mod_adj_chisq_cv_10.rds")

# Adding a method column:
res_mod_no_adj          <- res_mod_no_adj          %>% mutate(method = "mod_no_adj")
res_mod_adj_mi_4        <- res_mod_adj_mi_4        %>% mutate(method = "mod_adj_mi_4")
res_mod_adj_mi_10       <- res_mod_adj_mi_10       %>% mutate(method = "mod_adj_mi_10")
res_mod_adj_mi_cv_4     <- res_mod_adj_mi_cv_4     %>% mutate(method = "mod_adj_mi_cv_4")
res_mod_adj_mi_cv_10    <- res_mod_adj_mi_cv_10    %>% mutate(method = "mod_adj_mi_cv_10")
res_mod_adj_chisq_cv_4  <- res_mod_adj_chisq_cv_4  %>% mutate(method = "mod_adj_chisq_cv_4")
res_mod_adj_chisq_cv_10 <- res_mod_adj_chisq_cv_10 %>% mutate(method = "mod_adj_chisq_cv_10")

# Compiling all results:
all_results <- bind_rows(res_mod_no_adj, res_mod_adj_mi_4, res_mod_adj_mi_10, res_mod_adj_mi_cv_4, res_mod_adj_mi_cv_10, res_mod_adj_chisq_cv_4, res_mod_adj_chisq_cv_10)

# Saving separate rds file with all_results:
write_rds(all_results, path = "Simulation study/Results/all_results.rds")