# Crear una lista vacía
mi_lista <- list()

# Número de elementos a agregar
n <- 5

# Usar un bucle for para agregar elementos a la lista
for (i in 1:n) {
  mi_lista[[length(mi_lista) + 1]] <- paste("Elemento", i)
}

# Mostrar la lista resultante
print(mi_lista)



# Significant variables:
# Pooling:
hitter_vars_1 <- c("X_Home_runs_2_t")
# Within
hitter_vars_2 <- c("X_Home_runs_2_t")
# Random effects
hitter_vars_3 <- c("X_Home_runs_2_t",
                   "X_Home_runs_t_1")
# First Differences
hitter_vars_4 <- c("X_Home_runs_2_t",
                   "X_Porcentaje_On_base_plus_slugging_2_t_1",
                   "X_WAR_t")


# Pooling:
formula <- paste(control_vars,
                 hitter_vars_1,
                 sep = " + ")
# Create a model to store the results
hitter_stimation_1 <- plm(formula, data = hitters_panel,
                          model = "pooling",
                          index = c("id", "Anio_ref"))
# To store the results
hitter_results_stimation_1 <- coeftest(hitter_stimation_1,
                                       vcov = vcovHC(hitter_stimation_1,
                                                     type = "HC1",
                                                     cluster = "group"))
# Within:
formula <- paste(control_vars,
                 hitter_vars_2,
                 sep = " + ")
# Create a model to store the results
hitter_stimation_2 <- plm(formula, data = hitters_panel,
                          model = "within",
                          index = c("id", "Anio_ref"))
# To store the results
hitter_results_stimation_2 <- coeftest(hitter_stimation_2,
                                       vcov = vcovHC(hitter_stimation_2,
                                                     type = "HC1",
                                                     cluster = "group"))
# Random:
formula <- paste(control_vars,
                 hitter_vars_3[[1]],
                 sep = " + ")
formula <- paste(formula,
                 hitter_vars_3[[2]],
                 sep = " + ")
# Create a model to store the results
hitter_stimation_3 <- plm(formula, data = hitters_panel,
                          model = "random",
                          index = c("id", "Anio_ref"))
# To store the results
hitter_results_stimation_3 <- coeftest(hitter_stimation_3,
                                       vcov = vcovHC(hitter_stimation_3,
                                                     type = "HC1",
                                                     cluster = "group"))
# First Differences:
formula <- paste(control_vars,
                 hitter_vars_4[[1]],
                 sep = " + ")
formula <- paste(formula,
                 hitter_vars_4[[2]],
                 sep = " + ")
# Create a model to store the results
hitter_stimation_4 <- plm(formula, data = hitters_fd_panel,
                          model = "random",
                          index = c("id", "Anio_ref"))
# To store the results
hitter_results_stimation_4 <- coeftest(hitter_stimation_4,
                                       vcov = vcovHC(hitter_stimation_4,
                                                     type = "HC1",
                                                     cluster = "group"))

# Modelos
hitter_models <- list(pooling = hitter_results_stimation_1,
                      within = hitter_results_stimation_2,
                      random = hitter_results_stimation_3,
                      fd = hitter_results_stimation_4)
# Modelos
hitter_end_models <- list(pooling = hitter_stimation_1,
                          within = hitter_stimation_2,
                          random = hitter_stimation_3,
                          fd = hitter_stimation_4)


# Print the third block of results
stargazer(hitter_models,
          no.space = TRUE,
          align = TRUE,
          type = "latex",
          title = "Bateadores: Comparación de los modelos",
          column.labels = c("Pooling", "Within", 
                            "RE","FD"))