# Cargar librerías necesarias
library(survey)
library(dplyr)

RegDI2 <- function(data = NULL,
                   data_A = NULL,
                   data_B = NULL,
                   id_var_A = NULL,         # Variable identificadora en la muestra A
                   id_var_B = NULL,         # Variable identificadora en la muestra B
                   ind_var_A = NULL,        # Variable indicadora para la muestra A
                   ind_var_B = NULL,        # Variable indicadora para la muestra B
                   y_A_col,
                   y_B_col,
                   aux_vars = NULL,
                   N_total = NULL,          # Tamaño total de la población
                   weights_A = NULL,        # Pesos para la muestra A
                   weights_B = NULL,        # Pesos para la muestra B (no se usa directamente)
                   outcome_model = NULL,
                   model_type = "normal",   # Tipo de modelo de resultado ("normal", "logístico")
                   correction = 0,          # 0: Sin corrección, 1: Corrección por calibración, 2: Corrección por error de medición, 3: Doble robusto
                   eval_model_performance = FALSE    # Evaluar métricas de desempeño del modelo
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
    
    # Calcular el tamaño de las muestras A y B
    size_A <- sum(data[[ind_var_A]] == 1, na.rm = TRUE)
    size_B <- sum(data[[ind_var_B]] == 1, na.rm = TRUE)
    
    if (size_A == 0) stop("No hay unidades en la muestra A.")
    if (size_B == 0) stop("No hay unidades en la muestra B.")

    # Crear variables auxiliares delta_
    data$uno <- 1
    data$delta_i <- ifelse(data[[ind_var_B]] == 1, 1, 0)
    data$delta_yi <- ifelse(data[[ind_var_B]] == 1, data[[y_B_col]], 0)
    
    # Crear variables delta_ para las variables auxiliares
    if (!is.null(aux_vars)) {
        if (data_direct) {
            for (z in aux_vars) {
                data[[paste0("delta_", z)]] <- ifelse(data[[ind_var_B]] == 1, data[[z]], 0)
            }
        } else {
            for (z_B in aux_vars_B) {
                data[[paste0("delta_", z_B)]] <- ifelse(data[[ind_var_B]] == 1, data[[z_B]], 0)
            }
        }
    }
    
    # Preparar los totales de calibración
    if (data_direct) {
        totals <- c(
            uno = sum(data$uno, na.rm = TRUE),
            delta_i = sum(data$delta_i, na.rm = TRUE),
            delta_yi = sum(data$delta_yi, na.rm = TRUE)
        )
    } else {
        totals <- c(
            uno = N_total,
            delta_i = sum(data$delta_i, na.rm = TRUE),
            delta_yi = sum(data$delta_yi, na.rm = TRUE)
        )    
    }
    nrow(totals)
    
    if (!is.null(aux_vars)) {
        if (data_direct) {
            aux_totals <- sapply(aux_vars, function(z) sum(data[[paste0("delta_", z)]], na.rm = TRUE))
            names(aux_totals) <- paste0("delta_", aux_vars)
        } else {
            aux_totals <- sapply(aux_vars_B, function(z_B) sum(data[[paste0("delta_", z_B)]], na.rm = TRUE))
            names(aux_totals) <- paste0("delta_", aux_vars_B)
        }
        totals <- c(totals, aux_totals)
    }
    
    # Crear fórmula de calibración
    if (!is.null(aux_vars)) {
        delta_aux_vars <- if (data_direct) paste0("delta_", aux_vars) else paste0("delta_", aux_vars_B)
        calibration_formula <- as.formula(
            paste("~0 + uno + delta_i + delta_yi +", paste(delta_aux_vars, collapse = " + "))
        )
    } else {
        calibration_formula <- ~0 + uno + delta_i + delta_yi
    }

    # Crear el diseño de encuesta con pesos iniciales
    design <- svydesign(
        ids = ~1, 
        data = data[data[[ind_var_A]] == 1, ],  # Sustitución de subset()
        weights = ~d_i_A
    )
    
    # Realizar la calibración
    calibrated_design <- calibrate(
        design,
        formula = calibration_formula,
        population = totals,
        aggregate.stage = 1
    )
    
    # Función para calcular el estimador Doble Robusto
    calculate_DR_RegDI <- function(data, calibrated_design, y_A_col, aux_vars, N_total, outcome_model, eval_model_performance) {
        # 1. Ajustar el modelo de resultado usando la muestra A
        if (is.null(outcome_model)) {
            stop("Debe especificar el modelo de resultado usando el parámetro 'outcome_model'.")
        }
        
        # Ajustar fórmula para datos con nombres de columnas ajustados
        if (!data_direct && !is.null(aux_vars)) {
            aux_vars_A <- paste0(aux_vars, "_A")
        } else {
            aux_vars_A <- aux_vars
        }
        prediction_formula <- as.formula(outcome_model)
        outcome_fit <- lm(prediction_formula, data = data[data[[ind_var_A]] == 1, ])  # Sustitución de subset()
        
        # Predecir valores para todas las unidades
        data$Y_pred <- predict(outcome_fit, newdata = data)
        
        # Calcular el estimador DR
        weights_calibrated <- weights(calibrated_design)
        residuals_weighted <- (data[[y_A_col]][data[[ind_var_A]] == 1] - data$Y_pred[data[[ind_var_A]] == 1]) * weights_calibrated
        sum_weights_residuals <- sum(residuals_weighted, na.rm = TRUE)
        sum_Y_pred <- sum(data$Y_pred, na.rm = TRUE)
        
        # Estimador DR-RegDI
        T_DR_RegDI <- (sum_weights_residuals + sum_Y_pred) / N_total
        # Estimación de la varianza
        var_weights_residuals <- var(residuals_weighted, na.rm = TRUE) / length(residuals_weighted)
        var_Y_pred <- var(data$Y_pred, na.rm = TRUE) / N_total
        V_DR_RegDI <- var_weights_residuals + var_Y_pred
        
        # Evaluar el desempeño del modelo si se solicita
        if (eval_model_performance) {
            residuals <- data[[y_A_col]][data[[ind_var_A]] == 1] - data$Y_pred[data[[ind_var_A]] == 1]
            RMSE <- sqrt(mean(residuals^2, na.rm = TRUE))
            
            # Calcular R²
            SST <- sum((data[[y_A_col]][data[[ind_var_A]] == 1] - mean(data[[y_A_col]][data[[ind_var_A]] == 1], na.rm = TRUE))^2, na.rm = TRUE)
            SSR <- sum(residuals^2, na.rm = TRUE)
            R2 <- 1 - (SSR / SST)
            
            return(list(mean_DR_RegDI = T_DR_RegDI, var_DR_RegDI = V_DR_RegDI, RMSE = RMSE, R2 = R2))
        } else {
            return(list(mean_DR_RegDI = T_DR_RegDI, var_DR_RegDI = V_DR_RegDI))
        }
    }
    
    # Aplicar corrección según el parámetro 'correction'
    if (correction == 1) {
        # Corrección por calibración
        svymean_result <- svymean(as.formula(paste("~", y_A_col)), calibrated_design)
        T_RegDI <- as.numeric(svymean_result[1])
        V_RegDI <- as.numeric(attr(svymean_result, "var")[1])
        
    } else if (correction == 2) {
        # Corrección por calibración y error de medición
        validation_indices <- which(data[[ind_var_A]] == 1 & data[[ind_var_B]] == 1)
        
        if (length(validation_indices) < 2) {
            stop("No hay suficientes datos de validación en la muestra A para ajustar el modelo de error de medición.")
        }
        
        fit <- lm(as.formula(paste(y_A_col, "~", y_B_col)), 
                  data = data[validation_indices, ])
        
        beta_0 <- coef(fit)[1]
        beta_1 <- coef(fit)[2]
        
        data$y_corrected <- data[[y_B_col]]  
        data$y_corrected[data[[ind_var_A]] == 1] <- (data[[y_A_col]][data[[ind_var_A]] == 1] - beta_0) / beta_1
        
        data$delta_yi_corrected <- ifelse(data[[ind_var_B]] == 1, data$y_corrected, 0)
        
        totals_corrected <- c(
            uno = totals["uno"],
            delta_i = totals["delta_i"],
            delta_yi_corrected = sum(data$delta_yi_corrected, na.rm = TRUE)
        )
        
        if (!is.null(aux_vars)) {
            totals_corrected <- c(totals_corrected, totals[setdiff(names(totals), c("uno", "delta_i", "delta_yi"))])
        }
        
        # Crear la fórmula de calibración corregida
        if (!is.null(aux_vars)) {
            delta_aux_vars <- if (data_direct) paste0("delta_", aux_vars) else paste0("delta_", aux_vars_B)
            calibration_formula_corrected <- as.formula(
                paste("~0 + uno + delta_i + delta_yi_corrected +", paste(delta_aux_vars, collapse = " + "))
            )
        } else {
            calibration_formula_corrected <- ~0 + uno + delta_i + delta_yi_corrected
        }
    
        # Crear el diseño de encuesta corregido
        design_corrected <- svydesign(
            ids = ~1,
            data = data[data[[ind_var_A]] == 1, ],  # Sustitución de subset()
            weights = ~d_i_A
        )
        
        # Realizar la calibración corregida
        calibrated_design_corrected <- calibrate(
            design_corrected,
            formula = calibration_formula_corrected,
            population = totals_corrected,
            aggregate.stage = 1
        )
        
        # Calcular la media corregida
        svymean_result <- svymean(~y_corrected, calibrated_design_corrected)
        T_RegDI <- as.numeric(svymean_result[1])
        V_RegDI <- as.numeric(attr(svymean_result, "var")[1])
        
    } else if (correction == 3) {
        # Estimador Doble Robusto (DR-RegDI)
        DR_results <- calculate_DR_RegDI(data, calibrated_design, y_A_col, aux_vars, N_total, outcome_model, eval_model_performance)
        T_RegDI <- DR_results$mean_DR_RegDI
        V_RegDI <- DR_results$var_DR_RegDI
        
        if (eval_model_performance) {
            RMSE <- DR_results$RMSE
            R2 <- DR_results$R2
        }
        
    } else {
        # Sin corrección
        svymean_result <- svymean(as.formula(paste("~", y_A_col)), calibrated_design)
        T_RegDI <- as.numeric(svymean_result[1])
        V_RegDI <- as.numeric(attr(svymean_result, "var")[1])
    }

    # Devolver los resultados incluyendo métricas de desempeño si aplicable
    if (correction == 3 && eval_model_performance) {
        return(list(mean_RegDI = T_RegDI, var_RegDI = V_RegDI, RMSE = RMSE, R2 = R2))
    } else {
        return(list(mean_RegDI = T_RegDI, var_RegDI = V_RegDI))
    }
}

                                 