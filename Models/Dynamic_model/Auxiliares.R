# create some sample data
x <- c(1, 2, 3, 4, 5)
y <- c(3, 5, 7, 9, 11)

# fit a linear model and store the output
model <- lm(y ~ x)

# extract the p-values and store them in a list
p_values <- summary(model)$coefficients[, 4]

# print the list of p-values to the console
p_values


# extract the coefficient for the 'x' variable
x_coef <- coef(model)["x"]

# extract the p-value for the 'x' variable
x_pvalue <- summary(model)$coefficients["x", "Pr(>|t|)"]

# print the p-value for the 'x' variable to the console
x_pvalue

# create a list to store the results
d_hitter_results_1 <- list()

# loop over the variables in var_hitter_list
for (i in 1:length(hitter_var_suf)){
  
  # run linear regression with grouped errors by country and robust errors
  formula <- paste("Y_Sueldo_regular_norm_t ~ ", hitter_var_suf[[i]])
  model <- lm(formula, data = d_hitter_data)
  model_robust <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  
  # store the results in the list
  d_hitter_results_1[[i]] <- list(variable = names(hitter_var_suf[i]),
                                  model = model_robust)
  
  # show the results
  print(d_hitter_results_1[[i]])
}

```{r hitter_stimation_simple}
# create a list to store the results
#hitter_results_1 <- list()

# loop over the variables in var_hitter_list
for (i in 1:length(X_hitter_var_suf)){
  
  # run linear regression with grouped errors by country and robust errors
  formula <- paste(base_vars,
                   X_hitter_var_suf[[i]],
                   sep = " + ")
  
  h_m_pooled <- lm(formula,  data = hitter_data)
  
  my_lm_cluster <- coeftest(h_m_pooled,
                            vcov = vcovHC(h_m_pooled,
                                          cluster = "Jugador"))
  #h_m_pooled <- plm(formula, data = hitter_data,
  #                  model = "pooling",
  #                  index = c("Jugador", "Anio_t"))
  
  # Group errors:
  # Get clustered standard errors
  #coeftest(h_m_pooled, vcov = vcovHC(h_m_pooled, cluster = "Jugador"))
  
  #model_robust <- coeftest(h_m_pooled,
  #                         vcov = vcovHC(h_m_pooled,
  #                                       type = "HC1"))
  # store the results in the list
  #hitter_results_1[[i]] <- list(variable = names(X_hitter_var_suf[i]),
  #                              model = model_robust)
  
  # show the results
  #print(hitter_results_1[[i]])
  print(my_lm_cluster)
}