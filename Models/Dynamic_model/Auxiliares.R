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


### Short Stop

# loop over the variables in var_hitter_list
for (i in 1:length(X_fielder_var_suf)){
  
  # run linear regression with grouped errors by country and robust errors
  formula <- paste(vars,
                   X_fielder_var_suf[[i]],
                   sep = " + ")
  
  s_m_pooled <- lm(formula,  data = shorts_data)
  
  my_lm_cluster <- coeftest(s_m_pooled,
                            vcov = vcovHC(s_m_pooled,
                                          cluster = "Jugador"))
  print(my_lm_cluster)
}

# Poder de negociación antes y después de ser agente libre

Compararemos un periodo antes de convertirse agente con el primer periodo como agente libre. Importemos las bases de datos correspondientes

```{r import_panel_change}
setwd("~/Documentos/Github/Proyectos/MLB_HN/")
hitters_panel_change <- read.csv('ETL_Data/Panel/Cumulative/Bargaining_change/panel_hitters_cum_ch.csv')
fielders_panel_change <- read.csv('ETL_Data/Panel/Cumulative/Bargaining_change/panel_fielders_cum_ch.csv')
```

Por otro lado, se mostrarán las dimensiones de cada pánel
```{r panel_dimentions_change}
print("Bateadores: ")
print(dim(hitters_panel_change))
print("")
print("Fildeadores: ")
print(dim(fielders_panel_change))
```

```{r position_split_change, include=FALSE}
# split the data frame by category
split_hitter <- split(hitters_panel_change,
                      f = hitters_panel$Posicion_t)
split_fielder <- split(fielders_panel_change,
                       f = fielders_panel$Posicion_t)
```

```{r group_category_split_change, include=FALSE}
# Whole panel:
# Offensive:
h_category_1 <- split_hitter[["SP"]]
h_category_2 <- split_hitter[["RP"]]
h_category_3 <- split_hitter[["RP/CL"]]
h_category_4 <- split_hitter[["C"]]
h_category_5 <- split_hitter[["1B"]]
h_category_6 <- split_hitter[["2B"]]
h_category_7 <- split_hitter[["3B"]]
h_category_8 <- split_hitter[["SS"]]
h_category_9 <- split_hitter[["RF"]]
h_category_10 <- split_hitter[["CF"]]
h_category_11 <- split_hitter[["LF"]]
d_hitter_data <- split_hitter[["DH"]]

# Defensive:
starting_data <- split_fielder[["SP"]]
b_category_2 <- split_fielder[["RP"]]
b_category_3 <- split_fielder[["RP/CL"]]
shorts_data <- split_fielder[["SS"]]
```

```{r group_concat_change, include=FALSE}
# All panels:
# Hitters:
# Concatenate the two categories
hitter_data <- rbind(h_category_1, h_category_2, h_category_3, h_category_4,
                     h_category_5, h_category_6, h_category_7, h_category_8,
                     h_category_9, h_category_10, h_category_11)
hitter_data <- unique(hitter_data)

# Fielders:
# Concatenate the two categories
relief_pitcher_data <- rbind(b_category_2, b_category_3)
relief_pitcher_data <- unique(relief_pitcher_data)
```

## Pooling 

### Bateadores

Se obtendrán las estimaciones de las variables referentes a estadísticas deportivas sin controles

```{r hitter_stimation_simple_pooling_change}
# loop over the variables in var_hitter_list
for (i in 1:length(stat_hitter_t_1)){
  # run linear regression with grouped errors by country and robust errors
  base_vars_h  <- paste(vars, stat_hitter_t[[i]],
                        sep = '+')
  formula <- paste(base_vars_h,
                   stat_hitter_t_1[[i]],
                   sep = " + ")
  
  h_m_pooled <- plm(formula, data = hitter_data,
                    model = "pooling",
                    index = c("id", "Anio_ref"))
  
  my_lm_cluster <- coeftest(h_m_pooled,
                            vcov = vcovHC(h_m_pooled,
                                          type = "HC1",
                                          cluster = "group"))
  
  print(my_lm_cluster)
}
```


### Starting pitcher

```{r starting_stimation_pooling_change}
# loop over the variables in var_hitter_list
for (i in 1:length(stat_fielder_t_1)){
  # run linear regression with grouped errors by country and robust errors
  base_vars_s  <- paste(vars, stat_fielder_t[[i]],
                        sep = '+')
  formula <- paste(base_vars_s,
                   stat_fielder_t_1[[i]],
                   sep = " + ")
  
  s_m_pooled <- plm(formula, data = starting_data,
                    model = "pooling",
                    index = c("id", "Anio_ref"))
  
  my_lm_cluster <- coeftest(s_m_pooled,
                            vcov = vcovHC(s_m_pooled,
                                          type = "HC1",
                                          cluster = "group"))
  
  print(my_lm_cluster)
}
```

## Efectos fijos

### Bateadores

Se obtendrán las estimaciones de las variables referentes a estadísticas deportivas sin controles

```{r hitter_stimation_simple_within_change}
# loop over the variables in var_hitter_list
for (i in 1:length(stat_hitter_t_1)){
  # run linear regression with grouped errors by country and robust errors
  base_vars_h  <- paste(vars, stat_hitter_t[[i]],
                        sep = '+')
  formula <- paste(base_vars_h,
                   stat_hitter_t_1[[i]],
                   sep = " + ")
  
  h_m_fix_ef <- plm(formula, data = hitter_data,
                    model = "within",
                    index = c("id", "Anio_ref"))
  
  my_lm_cluster <- coeftest(h_m_fix_ef,
                            vcov = vcovHC(h_m_fix_ef,
                                          type = "HC1",
                                          cluster = "group"))
  
  print(my_lm_cluster)
}
```

### Starting pitcher

```{r starting_stimation_within_change}
# loop over the variables in var_hitter_list
for (i in 1:length(stat_fielder_t_1)){
  # run linear regression with grouped errors by country and robust errors
  base_vars_s  <- paste(vars, stat_fielder_t[[i]],
                        sep = '+')
  formula <- paste(base_vars_s,
                   stat_fielder_t_1[[i]],
                   sep = " + ")
  
  s_m_fix_ef <- plm(formula, data = starting_data,
                    model = "within",
                    index = c("id", "Anio_ref"))
  
  my_lm_cluster <- coeftest(s_m_fix_ef,
                            vcov = vcovHC(s_m_fix_ef,
                                          type = "HC1",
                                          cluster = "group"))
  
  print(my_lm_cluster)
}
```

## Efectos aleatorios

### Bateadores

Se obtendrán las estimaciones de las variables referentes a estadísticas deportivas sin controles

```{r hitter_stimation_simple_random_change}
# loop over the variables in var_hitter_list
for (i in 1:length(stat_hitter_t_1)){
  # run linear regression with grouped errors by country and robust errors
  base_vars_h  <- paste(vars, stat_hitter_t[[i]],
                        sep = '+')
  formula <- paste(base_vars_h,
                   stat_hitter_t_1[[i]],
                   sep = " + ")
  
  h_m_random <- plm(formula, data = hitter_data,
                    model = "random",
                    index = c("id", "Anio_ref"))
  
  my_lm_cluster <- coeftest(h_m_random,
                            vcov = vcovHC(h_m_random,
                                          type = "HC1",
                                          cluster = "group"))
  
  print(my_lm_cluster)
}
```

### Starting pitcher

```{r starting_stimation_random_change}
# loop over the variables in var_hitter_list
for (i in 1:length(stat_fielder_t_1)){
  # run linear regression with grouped errors by country and robust errors
  base_vars_s  <- paste(vars, stat_fielder_t[[i]],
                        sep = '+')
  formula <- paste(base_vars_s,
                   stat_fielder_t_1[[i]],
                   sep = " + ")
  
  s_m_random <- plm(formula, data = starting_data,
                    model = "random",
                    index = c("id", "Anio_ref"))
  
  my_lm_cluster <- coeftest(s_m_random,
                            vcov = vcovHC(s_m_random,
                                          type = "HC1",
                                          cluster = "group"))
  
  print(my_lm_cluster)
}
```

## First Differences

### Bateadores

Se obtendrán las estimaciones de las variables referentes a estadísticas deportivas sin controles

```{r hitter_stimation_simple_fd_change}
# loop over the variables in var_hitter_list
for (i in 1:length(stat_hitter_t_1)){
  # run linear regression with grouped errors by country and robust errors
  base_vars_h  <- paste(vars, stat_hitter_t[[i]],
                        sep = '+')
  formula <- paste(base_vars_h,
                   stat_hitter_t_1[[i]],
                   sep = " + ")
  
  h_m_first_d <- plm(formula, data = hitter_data,
                     model = "fd",
                     index = c("id", "Anio_ref"))
  
  my_lm_cluster <- coeftest(h_m_first_d,
                            vcov = vcovHC(h_m_first_d,
                                          type = "HC1",
                                          cluster = "group"))
  
  print(my_lm_cluster)
}
```

### Starting pitcher

```{r starting_stimation_fd_change}
# loop over the variables in var_hitter_list
for (i in 1:length(stat_fielder_t_1)){
  # run linear regression with grouped errors by country and robust errors
  base_vars_s  <- paste(vars, stat_fielder_t[[i]],
                        sep = '+')
  formula <- paste(base_vars_s,
                   stat_fielder_t_1[[i]],
                   sep = " + ")
  
  s_m_first_d <- plm(formula, data = starting_data,
                     model = "fd",
                     index = c("id", "Anio_ref"))
  
  my_lm_cluster <- coeftest(s_m_first_d,
                            vcov = vcovHC(s_m_first_d,
                                          type = "HC1",
                                          cluster = "group"))
  
  print(my_lm_cluster)
}
```