library(tidyverse)
library(xtable)
library(gmodels)
library(tables)
library(gridExtra)

# Loading all the results:
all_res <- read_rds("Simulation study/Results 01-05/all_results.rds")

# Finding the mean and median MSE of each method:
general <- all_res %>% select(mses, method, mean_distcov) %>% 
  unnest(mses) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE), "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>%
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

general
print(xtable(general, caption = "General Results", digits = 4), include.rownames = FALSE)

##########################################################
# Finding the median MSE and Covdist per sample size (n):#
##########################################################

# For n = 100:
n_100 <- all_res %>% select(mses, method, mean_distcov, n) %>% 
  unnest(mses) %>% 
  filter(n == 100) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE), "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# For n = 200:
n_200 <- all_res %>% select(mses, method, mean_distcov, n) %>% 
  unnest(mses) %>% 
  filter(n == 200) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE), "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# For n = 500:
n_500 <- all_res %>% select(mses, method, mean_distcov, n) %>% 
  unnest(mses) %>% 
  filter(n == 500) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE), "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# binding all tables with rownames as a column
by_n <- rbind(
  data.frame(n_100, station = "n = 100", what = factor(rownames(n_100), levels = rownames(n_100)), row.names = NULL, check.names = FALSE),
  data.frame(n_200, station = "n = 200", what = factor(rownames(n_200), levels = rownames(n_200)), row.names = NULL, check.names = FALSE),
  data.frame(n_500, station = "n = 500", what = factor(rownames(n_500), levels = rownames(n_500)), row.names = NULL, check.names = FALSE)
  )

# Finalizing the table:
table_by_n <- tabular(Heading()*what ~ station*(`Median MSE` +`Mean CovDist`)*Heading()*(identity),data = by_n)

# Printing table in LaTex format:
latex(table_by_n)


# Making a plot by n for median MSE:
plot_n_mse_data <- all_res %>% select(n, mses, method) %>% mutate(n = as.factor(n)) %>% unnest(mses) %>% mutate(mses = unlist(mses)) %>% filter(method == "mod_no_adj" | method == "mod_adj_mi_10" | method == "mod_adj_mi_cv_10" | method == "mod_adj_chisq_cv_10") %>% group_by(method, n) %>%  dplyr::summarize(Median = median(mses, na.rm = TRUE)) %>% arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_10", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_10")))

plot_n_mse <- ggplot(plot_n_mse_data, aes(x = n, y = Median, fill = method)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(fill = "Method") + scale_fill_discrete(name = "Method", labels = c("CHI-CV10", "MI10", "MI-CV10", "No Mod")) + labs(y = "Median MSE", x = "Sample Size", title = "(a) Median MSE of methods with different n")

# Making a plot by n for mean distcov:
plot_n_distcov_data <- all_res %>% select(n, distcov, method) %>% mutate(n = as.factor(n)) %>% unnest(distcov) %>% mutate(distcov = as.numeric(distcov)) %>% filter(method == "mod_no_adj" | method == "mod_adj_mi_10" | method == "mod_adj_mi_cv_10" | method == "mod_adj_chisq_cv_10") %>% group_by(method, n) %>% dplyr::summarize(Mean = mean(distcov, na.rm = TRUE))

plot_n_distcov <- ggplot(plot_n_distcov_data, aes(x = n, y = Mean, fill = method)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(fill = "Method") + scale_fill_discrete(name = "Method", labels = c("CHI-CV10", "MI10", "MI-CV10", "No Mod")) + labs(y = expression(paste("Mean ", Sigma)), x = "Sample Size", title = expression(paste("(b) Mean ", Sigma, " of methods with different n")))

grid.arrange(plot_n_mse, plot_n_distcov)





##############################################
# Finding the median MSE and Covdist per rho:#
##############################################

# for rho = 0.1
rho_0.1 <- all_res %>% select(mses, method, mean_distcov, rho) %>% 
  unnest(mses) %>% 
  filter(rho == 0.1) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE),  "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# for rho = 0.3
rho_0.3 <- all_res %>% select(mses, method, mean_distcov, rho) %>% 
  unnest(mses) %>% 
  filter(rho == 0.3) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE),  "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# for rho = 0.5:
rho_0.5 <- all_res %>% select(mses, method, mean_distcov, rho) %>% 
  unnest(mses) %>% 
  filter(rho == 0.5) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE),  "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# binding all tables with rownames as a column
by_rho <- rbind(
  data.frame(rho_0.1, station = "rho = 0.1", what = factor(rownames(rho_0.1), levels = rownames(rho_0.1)), row.names = NULL, check.names = FALSE),
  data.frame(rho_0.3, station = "rho = 0.3", what = factor(rownames(rho_0.3), levels = rownames(rho_0.3)), row.names = NULL, check.names = FALSE),
  data.frame(rho_0.5, station = "rho = 0.5", what = factor(rownames(rho_0.5), levels = rownames(rho_0.5)), row.names = NULL, check.names = FALSE)
)

# Finalizing the table:
table_by_rho <- tabular(Heading()*what ~ station*(`Median MSE` +`Mean CovDist`)*Heading()*(identity),data = by_rho)

# Printing table in LaTex format:
latex(table_by_rho)

# Making a plot by rho for median MSE:
plot_rho_mse_data <- all_res %>% select(rho, mses, method) %>% mutate(rho = as.factor(rho)) %>% unnest(mses) %>% mutate(mses = unlist(mses)) %>% filter(method == "mod_no_adj" | method == "mod_adj_mi_10" | method == "mod_adj_mi_cv_10" | method == "mod_adj_chisq_cv_10") %>% group_by(method, rho) %>%  dplyr::summarize(Median = median(mses, na.rm = TRUE))

plot_rho_mse <- ggplot(plot_rho_mse_data, aes(x = rho, y = Median, fill = method)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(fill = "Method") + scale_fill_discrete(name = "Method", labels = c("CHI-CV10", "MI10", "MI-CV10", "No Mod")) + labs(y = expression(paste("Median MSE")), x = expression(rho), title = expression(paste("(a) Median MSE of methods with different ", rho)))

# Making a plot by rho for mean distcov:
plot_rho_distcov_data <- all_res %>% select(rho, distcov, method) %>% mutate(rho = as.factor(rho)) %>% unnest(distcov) %>% mutate(distcov = as.numeric(distcov)) %>% filter(method == "mod_no_adj" | method == "mod_adj_mi_10" | method == "mod_adj_mi_cv_10" | method == "mod_adj_chisq_cv_10") %>% group_by(method, rho) %>% dplyr::summarize(Mean = mean(distcov, na.rm = TRUE))

plot_rho_distcov <- ggplot(plot_rho_distcov_data, aes(x = rho, y = Mean, fill = method)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(fill = "Method") + scale_fill_discrete(name = "Method", labels = c("CHI-CV10", "MI10", "MI-CV10", "No Mod")) + labs(y = expression(paste("Mean ", Sigma)), x = expression(rho), title = expression(paste("(b) Mean ", Sigma, " of methods with different ", rho)))

grid.arrange(plot_rho_mse, plot_rho_distcov)



################################################
# Finding the median MSE and Covdist per delta:#
################################################

# for rho = 0.1
delta_0.1 <- all_res %>% select(mses, method, mean_distcov, delta) %>% 
  unnest(mses) %>% 
  filter(delta == 0.1) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE),  "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# for rho = 0.3
delta_0.3 <- all_res %>% select(mses, method, mean_distcov, delta) %>% 
  unnest(mses) %>% 
  filter(delta == 0.3) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE),  "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# for rho = 0.5:
delta_0.5 <- all_res %>% select(mses, method, mean_distcov, delta) %>% 
  unnest(mses) %>% 
  filter(delta == 0.5) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE),  "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# binding all tables with rownames as a column
by_delta <- rbind(
  data.frame(delta_0.1, station = "delta = 0.1", what = factor(rownames(delta_0.1), levels = rownames(rho_0.1)), row.names = NULL, check.names = FALSE),
  data.frame(delta_0.3, station = "delta = 0.3", what = factor(rownames(delta_0.3), levels = rownames(delta_0.3)), row.names = NULL, check.names = FALSE),
  data.frame(delta_0.5, station = "delta = 0.5", what = factor(rownames(delta_0.5), levels = rownames(delta_0.5)), row.names = NULL, check.names = FALSE)
)

# Finalizing the table:
table_by_delta <- tabular(Heading()*what ~ station*(`Median MSE` +`Mean CovDist`)*Heading()*(identity),data = by_delta)

# Printing table in LaTex format:
latex(table_by_delta)




plot_delta_mse_data <- all_res %>% select(delta, mses, method) %>% mutate(delta = as.factor(delta)) %>% unnest(mses) %>% mutate(mses = unlist(mses)) %>% filter(method == "mod_no_adj" | method == "mod_adj_mi_10" | method == "mod_adj_mi_cv_10" | method == "mod_adj_chisq_cv_10") %>% group_by(method, delta) %>%  dplyr::summarize(Median = median(mses, na.rm = TRUE))

plot_delta_mse <- ggplot(plot_delta_mse_data, aes(x = delta, y = Median, fill = method)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(fill = "Method") + scale_fill_discrete(name = "Method", labels = c("CHI-CV10", "MI10", "MI-CV10", "No Mod")) + labs(y = "Median MSE", x = expression(delta), title = expression(paste("(a) Median MSE of methods with different ", delta)))

plot_delta_distcov_data <- all_res %>% select(delta, distcov, method) %>% mutate(delta = as.factor(delta)) %>% unnest(distcov) %>% mutate(distcov = as.numeric(distcov)) %>% filter(method == "mod_no_adj" | method == "mod_adj_mi_10" | method == "mod_adj_mi_cv_10" | method == "mod_adj_chisq_cv_10") %>% group_by(method, delta) %>%  dplyr::summarize(Mean = mean(distcov, na.rm = TRUE))

plot_delta_distcov <- ggplot(plot_delta_distcov_data, aes(x = delta, y = Mean, fill = method)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(fill = "Method") + scale_fill_discrete(name = "Method", labels = c("CHI-CV10", "MI10", "MI-CV10", "No Mod")) + labs(y = expression(paste("Mean ", Sigma)), x = expression(delta), title = expression(paste("(b) Mean ", Sigma, " of methods with different ", delta)))

grid.arrange(plot_delta_mse, plot_delta_distcov)



#################################################
# Finding the median MSE and Covdist per lambda:#
#################################################

# for rho = 0.1
lambda_0.1 <- all_res %>% select(mses, method, mean_distcov, lambda) %>% 
  unnest(mses) %>% 
  filter(lambda == 0.1) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE),  "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# for rho = 0.3
lambda_0.3 <- all_res %>% select(mses, method, mean_distcov, lambda) %>% 
  unnest(mses) %>% 
  filter(lambda == 0.3) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE),  "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# for rho = 0.5:
lambda_0.5 <- all_res %>% select(mses, method, mean_distcov, lambda) %>% 
  unnest(mses) %>% 
  filter(lambda == 0.5) %>% 
  group_by(method) %>% 
  mutate(mses = unlist(mses)) %>% 
  dplyr::summarize("Median MSE" = median(mses, na.rm = TRUE),  "Mean CovDist" = mean(mean_distcov, na.rm = TRUE)) %>% 
  arrange(factor(.$method, levels = c("mod_no_adj", "mod_adj_mi_4", "mod_adj_mi_10", "mod_adj_mi_cv_4", "mod_adj_mi_cv_10", "mod_adj_chisq_cv_4", "mod_adj_chisq_cv_10")))

# binding all tables with rownames as a column
by_lambda <- rbind(
  data.frame(lambda_0.1, station = "lambda = 0.1", what = factor(rownames(delta_0.1), levels = rownames(rho_0.1)), row.names = NULL, check.names = FALSE),
  data.frame(lambda_0.3, station = "lambda = 0.3", what = factor(rownames(delta_0.3), levels = rownames(delta_0.3)), row.names = NULL, check.names = FALSE),
  data.frame(lambda_0.5, station = "lambda = 0.5", what = factor(rownames(delta_0.5), levels = rownames(lambda_0.5)), row.names = NULL, check.names = FALSE)
)

# Finalizing the table:
table_by_lambda <- tabular(Heading()*what ~ station*(`Median MSE` +`Mean CovDist`)*Heading()*(identity),data = by_lambda)

# Printing table in LaTex format:
latex(table_by_lambda)




plot_lambda_mse_data <- all_res %>% select(lambda, mses, method) %>% mutate(lambda = as.factor(lambda)) %>% unnest(mses) %>% mutate(mses = unlist(mses)) %>% filter(method == "mod_no_adj" | method == "mod_adj_mi_10" | method == "mod_adj_mi_cv_10" | method == "mod_adj_chisq_cv_10") %>% group_by(method, lambda) %>%  dplyr::summarize(Median = median(mses, na.rm = TRUE))

plot_lambda_mse <- ggplot(plot_lambda_mse_data, aes(x = lambda, y = Median, fill = method)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(fill = "Method") + scale_fill_discrete(name = "Method", labels = c("CHI-CV10", "MI10", "MI-CV10", "No Mod")) + labs(y = "Median MSE", x = expression(lambda), title = expression(paste("(a) Median MSE of methods with different ", lambda)))

plot_lambda_distcov_data <- all_res %>% select(lambda, distcov, method) %>% mutate(lambda = as.factor(lambda)) %>% unnest(distcov) %>% mutate(distcov = as.numeric(distcov)) %>% filter(method == "mod_no_adj" | method == "mod_adj_mi_10" | method == "mod_adj_mi_cv_10" | method == "mod_adj_chisq_cv_10") %>% group_by(method, lambda) %>%  dplyr::summarize(Mean = mean(distcov, na.rm = TRUE))

plot_lambda_distcov <- ggplot(plot_lambda_distcov_data, aes(x = lambda, y = Mean, fill = method)) + geom_bar(stat = "identity", position = "dodge") + theme_minimal() + labs(fill = "Method") + scale_fill_discrete(name = "Method", labels = c("CHI-CV10", "MI10", "MI-CV10", "No Mod")) + labs(y = expression(paste("Mean ", Sigma)), x = expression(lambda), title = expression(paste("(b) Mean ", Sigma, " of methods with different ", lambda)))

grid.arrange(plot_lambda_mse, plot_lambda_distcov)



