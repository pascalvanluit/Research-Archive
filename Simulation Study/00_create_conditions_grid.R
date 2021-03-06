library(tidyverse)

#####################################################################
### Script to create the conditions grid for the simulation study ###
#####################################################################

# Number of replications:
replications <- 50 # Used in script 01_simulation.R

# Creating conditions grid
conditions_grid <- expand_grid(lambda = c(0.1, 0.3, 0.5), rho = c(0.1, 0.3, 0.5), delta = c(0.1, 0.3, 0.5), n = c(100, 200, 500))

# Creating .rds file with all conditions:
write_rds(conditions_grid, path = "Simulation study/00_conditions.rds")