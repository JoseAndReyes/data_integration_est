construir_formula_dinamica <- function(formula_inicial, data_joined) {
  # Verificar que la fórmula se extrae correctamente
  formula_inicial <- as.formula(formula_inicial)
  if (is.null(formula_inicial) || length(all.vars(formula_inicial)) == 0) {
    stop("La fórmula inicial no es válida.")
  }
  
  # Extraer el lado izquierdo (variable dependiente) y el lado derecho (variables independientes)
  variable_estudio <- all.vars(formula_inicial)[1]
  predictores <- all.vars(formula_inicial)[-1]
  
  # Verificar que la variable de estudio fue extraída correctamente
  if (is.na(variable_estudio)) {
    stop("No se pudo extraer la variable dependiente de la fórmula.")
  }

  # Crear una lista vacía para almacenar los nombres de las variables corregidas
  variables_actualizadas <- c()

  for (predictor in predictores) {
    # Comprobar si la variable tiene una versión con sufijo _A
    if (paste0(predictor, "_A") %in% names(data_joined)) {
      # Si existe la versión con sufijo _A, la usamos
      variables_actualizadas <- c(variables_actualizadas, paste0(predictor, "_A"))
    } else if (paste0(predictor, "_B") %in% names(data_joined)) {
      # Si solo existe la versión _B, la usamos (en caso de que _A no esté presente)
      variables_actualizadas <- c(variables_actualizadas, paste0(predictor, "_B"))
    } else if (predictor %in% names(data_joined)) {
      # Si no tiene sufijos pero existe, la usamos tal cual
      variables_actualizadas <- c(variables_actualizadas, predictor)
    } else {
      stop(paste("La variable", predictor, "no se encontró en los datos."))
    }
  }
  
  # Crear una nueva fórmula con los nombres actualizados de las variables
  nueva_formula <- as.formula(paste(variable_estudio, "~", paste(variables_actualizadas, collapse = " + ")))
  return(nueva_formula)
}

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

PC_Estimator <- function(data = NULL,
                         data_A = NULL,
                         data_B = NULL,
                         id_var_A = NULL,        # Variable identificadora en la muestra A
                         id_var_B = NULL,        # Variable identificadora en la muestra B
                         ind_var_A = NULL,       # Variable indicadora para la muestra A
                         ind_var_B = NULL,       # Variable identificadora para la muestra B
                         y_A_col = NULL,
                         y_B_col = NULL,
                         aux_vars = NULL,
                         N_total = NULL,
                         weights_A = NULL,       # Pesos para la muestra A
                         weights_B = NULL,       # Pesos para la muestra B
                         outcome_model = NULL,   # Fórmula del modelo de resultado
                         model_type = "normal",  # Tipo de modelo ("normal", "logístico")
                         scenario = 1,
                         eval_model_performance = FALSE,
                         verbose = FALSE
) {
    # Verificar si los datos se proporcionan directamente
    data_direct <- !is.null(data)
    
    if (!data_direct && (is.null(data_A) || is.null(data_B))) {
        stop("Debe proporcionar 'data' o ambos 'data_A' y 'data_B'.")
    }
    
    if (!data_direct && !is.null(data_A) && !is.null(data_B)) {
        # Asegurar que las variables identificadoras estén especificadas
        if (is.null(id_var_A) || is.null(id_var_B)) {
            stop("Debe especificar 'id_var_A' y 'id_var_B' cuando proporcione 'data_A' y 'data_B'.")
        }
        
        # Verificar que las variables identificadoras existan en data_A y data_B
        if (!(id_var_A %in% names(data_A))) {
            stop(paste("La columna", id_var_A, "no se encuentra en 'data_A'."))
        }
        if (!(id_var_B %in% names(data_B))) {
            stop(paste("La columna", id_var_B, "no se encuentra en 'data_B'."))
        }

        # Renombrar las variables identificadoras a un nombre común 'id'
        data_A <- data_A %>% rename(id = !!sym(id_var_A))
        data_B <- data_B %>% rename(id = !!sym(id_var_B))
     
        # Unir data_A y data_B usando 'id'
        data_combined <- full_join(data_A, data_B, by = "id", suffix = c("_A", "_B"))

        # *** Aquí añadimos la lógica condicional para las variables indicadoras ***
        if (y_A_col == y_B_col) {
            # Si los nombres son iguales, seguimos con los sufijos
            data_combined <- data_combined %>%
                mutate(
                    ind_var_A = ifelse(!is.na(.[[paste0(y_A_col, "_A")]]), 1, 0),
                    ind_var_B = ifelse(!is.na(.[[paste0(y_B_col, "_B")]]), 1, 0)
                )
        } else {
            # Si los nombres son diferentes, usamos los nombres originales
            data_combined <- data_combined %>%
                mutate(
                    ind_var_A = ifelse(!is.na(.[[y_A_col]]), 1, 0),
                    ind_var_B = ifelse(!is.na(.[[y_B_col]]), 1, 0)
                )
        }

        # Actualizar los nombres de las variables indicadoras
        ind_var_A <- "ind_var_A"
        ind_var_B <- "ind_var_B"
        
        data <- data_combined
    }

    if (!data_direct) {
        # Ajustar los nombres de las columnas para las muestras A y B solo cuando data_direct = FALSE
        if (y_A_col == y_B_col) {
        y_A_col <- paste0(y_A_col, "_A")
        y_B_col <- paste0(y_B_col, "_B")
        }
        if (!is.null(aux_vars)) {
            aux_vars_A <- paste0(aux_vars, "_A")
            aux_vars_B <- paste0(aux_vars, "_B")
        }
    } else {
        # Si data_direct = TRUE, no ajustar nombres, usar tal como están
        aux_vars_A <- aux_vars
        aux_vars_B <- aux_vars
    }

    # Calcular N_total si no se proporciona
    if (is.null(N_total)) {
        if (data_direct) {
            N_total <- nrow(data)
        } else {
            if (is.null(weights_A)) {
                stop("Para aproximar N_total, debe proporcionar los pesos para la muestra A ('weights_A').")
            }
            if (!(weights_A %in% names(data))) {
                stop(paste("La columna especificada en 'weights_A' no se encuentra en 'data'."))
            }
            N_total <- sum(data[[weights_A]][data[[ind_var_A]] == 1], na.rm = TRUE)
        }
    }

    # Asignar pesos para la muestra A
    if (!is.null(weights_A)) {
        if (!(weights_A %in% names(data))) {
            stop(paste("La columna especificada en 'weights_A' no se encuentra en 'data'."))
        }
        data$d_i_A <- ifelse(data[[ind_var_A]] == 1, data[[weights_A]], 0)
    } else if (!is.null(N_total)) {
        data$d_i_A <- ifelse(data[[ind_var_A]] == 1, N_total / sum(data[[ind_var_A]] == 1, na.rm = TRUE), 0)
        weights_A <- "d_i_A"
    } else {
        stop("Debe proporcionar 'weights_A' o definir 'N_total'.")
    }

    # Asignar pesos para S_B
    if (!is.null(weights_B)) {
        if (!(weights_B %in% names(data))) {
            stop(paste("La columna especificada en 'weights_B' no se encuentra en 'data'."))
        }
        data$d_i_B <- ifelse(data[[ind_var_B]] == 1, data[[weights_B]], 0)
    } else {
        # Asignar pesos temporales basados en N_total
        data$d_i_B <- ifelse(data[[ind_var_B]] == 1, N_total / sum(data[[ind_var_B]] == 1, na.rm = TRUE), 0)
    }

    # 5. Calcular tamaños de muestra A y B
    size_A <- sum(data[[ind_var_A]] == 1, na.rm = TRUE)
    size_B <- sum(data[[ind_var_B]] == 1, na.rm = TRUE)
    
    if (size_A == 0) stop("No hay unidades en la muestra A.")
    if (size_B == 0) stop("No hay unidades en la muestra B.")
    
    # 6. Calcular los totales poblacionales de variables auxiliares
    if (!is.null(aux_vars)) {
        if (data_direct) {
            # Si 'data' representa la población completa, sumar directamente
            pop_totals <- sapply(aux_vars, function(var) {
                sum(data[[var]], na.rm = TRUE)
            })
            names(pop_totals) <- aux_vars
        } else if (!is.null(weights_A)) {
            # Estimar 'pop_totals' usando Horvitz-Thompson basado en la muestra A
            design_S_A <- svydesign(ids = ~1, data = subset(data, ind_var_A == 1), weights = ~d_i_A)
            # Estimar los totales poblacionales de las variables auxiliares desde la muestra A
            pop_totals_estimated <- svytotal(as.formula(paste("~", paste(aux_vars_A, collapse = " + "))), design_S_A)
            pop_totals <- coef(pop_totals_estimated)
        } else {
            stop("Para calcular 'pop_totals' se requiere 'data' completo o 'weights_A' proporcionados con 'aux_vars'.")
        }
    } else {
        pop_totals <- NULL
    }

    # 7. Crear fórmula de calibración
    if (!is.null(aux_vars)) {
        # Si data_direct = TRUE, usar las variables auxiliares directamente
        calibration_formula <- if (data_direct) {
            as.formula(paste("~0 +", paste(aux_vars, collapse = " + ")))
        } else {
            # Si data_direct = FALSE, usar las versiones renombradas (aux_vars_B)
            as.formula(paste("~0 +", paste(aux_vars_B, collapse = " + ")))
        }
        if (verbose) print(paste("Fórmula de calibración:", deparse(calibration_formula)))
    } else {
        calibration_formula <- NULL
    }

    # 8. Calibrar pesos para S_B
    design_S_B <- svydesign(ids = ~1, data = data[data[[ind_var_B]] == 1,], weights = ~d_i_B)
    if (!is.null(aux_vars)) {
        if (is.null(pop_totals)) {
            stop("Los totales poblacionales de variables auxiliares deben ser proporcionados o estimados.")
        }
        suppressWarnings(calibrated_design_S_B <- tryCatch({
            quiet(calibrate(
                design = design_S_B,
                formula = calibration_formula,
                population = pop_totals,
                calfun = "linear"
            ))
        }, error = function(e) {
            stop("Error en la calibración de pesos: ", e$message)
        }))
        weights_calibrated_B <- weights(calibrated_design_S_B)
        # Asignar pesos calibrados solo a las unidades de S_B
        data$d_i_B[data[[ind_var_B]] == 1] <- weights_calibrated_B
    } else {
        weights_calibrated_B <- weights(design_S_B)
        data$d_i_B[data[[ind_var_B]] == 1] <- weights_calibrated_B
    }

    # 9. Escenarios específicos
    if (scenario == 1) {
        # Escenario 1: Variable objetivo observada en S_B
        if (is.null(y_B_col)) {
            stop("Para el Escenario 1, 'y_B_col' no puede ser NULL.")
        }
        mean_PC <- svymean(as.formula(paste("~", y_B_col)), calibrated_design_S_B)
        est_mean_PC <- coef(mean_PC)
        SE_PC <- SE(mean_PC)
        
        return(list(
            estimator = est_mean_PC,
            SE = SE_PC,
            weights_B = weights_calibrated_B
        ))
    
    } else if (scenario == 2) {
        # Escenario 2: Variable objetivo no observada en S_B, se necesita predecir y_i
        if (is.null(outcome_model)) stop("Debe proporcionar 'outcome_model' para el modelo de predicción.")
        if (is.null(y_A_col)) stop("Debe proporcionar 'y_A_col' para el modelo de predicción.")
        
        # Construir el modelo de predicción utilizando únicamente S_A ∩ S_B
        data_intersect <- data[data[[ind_var_A]] == 1 & data[[ind_var_B]] == 1, ]
        if (nrow(data_intersect) == 0) {
            stop("No hay unidades en la intersección de S_A y S_B para estimar el modelo.")
        }
        
        # *** Aquí integramos la lógica de la fórmula dinámica ***
        prediction_formula <- construir_formula_dinamica(outcome_model, data_intersect)

        if (verbose) print(paste("Construyendo el modelo de predicción con la fórmula:", deparse(prediction_formula)))
        
        if (model_type == "normal") {
            model <- lm(prediction_formula, data = data_intersect)
        } else if (model_type == "logistic") {
            model <- glm(prediction_formula, data = data_intersect, family = binomial)
        } else {
            stop("Tipo de modelo no soportado.")
        }

        # Predecir y_i en S_B
        data$hat_y_i_S_B <- ifelse(data[[ind_var_B]] == 1, predict(model, newdata = data, type = "response"), NA)
        
        # Predecir y_i en S_A
        data$hat_y_i_S_A <- ifelse(data[[ind_var_A]] == 1, predict(model, newdata = data, type = "response"), NA)
        
        # Calcular residuos para S_A
        data$residuals <- ifelse(data[[ind_var_A]] == 1, data[[y_A_col]] - data$hat_y_i_S_A, NA)
        
        # Calcular el estimador PC
        term1 <- sum(data$d_i_B * data$hat_y_i_S_B, na.rm = TRUE)
        term2 <- sum(data$d_i_A * data$residuals, na.rm = TRUE)
        estimator_PC <- (term1 + term2) / N_total  # Dividir por N para obtener la media
        
        return(list(
            estimator = estimator_PC,
            weights_B = weights_calibrated_B,
            model = model
        ))
    
    } else if (scenario == 3) {
        # Escenario 3: Violación del supuesto MAR, implementar estimador DR1
        if (is.null(outcome_model)) stop("Debe proporcionar 'outcome_model' para el modelo de predicción.")
        if (is.null(y_A_col)) stop("Debe proporcionar 'y_A_col' para el modelo de predicción.")
        if (is.null(y_B_col)) stop("Para el Escenario 3, 'y_B_col' no puede ser NULL.")

        # Construir el modelo de predicción utilizando únicamente S_A con la fórmula corregida
        prediction_formula <- construir_formula_dinamica(outcome_model, data[data[[ind_var_A]] == 1,])
        if (model_type == "normal") {
            model <- lm(prediction_formula, data[data[[ind_var_A]] == 1,])

        } else if (model_type == "logistic") {
            model <- glm(prediction_formula, data = data[data[[ind_var_A]] == 1,], family = binomial)

        } else {
            stop("model_type debe ser 'normal' o 'logistic'.")
        }

        # Predecir y_i en toda la población
        data$hat_y_i <- predict(model, newdata = data, type = "response")
        sum_hat_y_U <- sum(data$hat_y_i, na.rm = TRUE)
        
        # Calcular los residuos en S_A
        data$residuals <- ifelse(data[[ind_var_A]] == 1, data[[y_A_col]] - data$hat_y_i, NA)
        
        # Calcular term1_DR1
        term1_DR1 <- sum(data$d_i_B * (ifelse(data[[ind_var_B]] == 1, data[[y_B_col]], 0) - data$hat_y_i), na.rm = TRUE)
        
        # Calcular el estimador DR1
        estimator_DR1 <- (term1_DR1 + sum_hat_y_U) / N_total  # Dividir por N para obtener la media
        
        # Evaluar el rendimiento del modelo (opcional)
        if (eval_model_performance) {
            # Calcular el RMSE en S_A
            RMSE <- sqrt(mean((data$residuals[data[[ind_var_A]] == 1])^2, na.rm = TRUE))
            
            # Calcular el R^2
            SST <- sum((data[[y_A_col]][data[[ind_var_A]] == 1] - mean(data[[y_A_col]][data[[ind_var_A]] == 1], na.rm = TRUE))^2, na.rm = TRUE)
            SSR <- sum((data$residuals[data[[ind_var_A]] == 1])^2, na.rm = TRUE)
            R2 <- 1 - (SSR / SST)
            
            
            return(list(
                estimator_DR1 = estimator_DR1,
                weights_B = weights_calibrated_B,
                model = model,
                RMSE = RMSE,
                R2 = R2
            ))
        } else {
            return(list(
                estimator_DR1 = estimator_DR1,
                weights_B = weights_calibrated_B,
                model = model
            ))
        }
    } else {
        stop("Escenario inválido. Debe ser 1, 2 o 3.")
    }
}
