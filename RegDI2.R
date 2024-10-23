# Cargar librerías necesarias
library(survey)
library(dplyr)

RegDI2 <- function(data = NULL,
                   data_A = NULL,
                   data_B = NULL,
                   ind_var_A = "ind_A",      # Variable indicadora para muestra A
                   ind_var_B = "ind_B",      # Variable indicadora para muestra B
                   y_A_col,
                   y_B_col,
                   aux_vars = NULL,
                   N_total = NULL,
                   weights_A = NULL,         # Pesos para muestra A
                   weights_B = NULL,         # Opcional para RegDI2, no se usa directamente
                   outcome_model = NULL,
                   model_type = "normal",    # Tipo de modelo ("normal", "logistic")
                   correction = 0,           # 0: Sin corrección, 1: Corrección A, 2: Corrección B, 3: Doble Robusto
                   eval_model_performance = FALSE    # Nuevo parámetro para evaluar el rendimiento del modelo
                   ) {
        
    # 1. Validar que al menos se proporcione 'data' o ambas 'data_A' y 'data_B'
    if (is.null(data) && (is.null(data_A) || is.null(data_B))) {
        stop("Debe proporcionar 'data' o ambas 'data_A' y 'data_B'.")
    }
    
    # 2. Combinar data_A y data_B si se proporcionan por separado
    if (!is.null(data_A) || !is.null(data_B)) {
        if (is.null(data_A) || is.null(data_B)) {
            stop("Si se proporciona 'data_A' o 'data_B', ambos deben ser proporcionados.")
        }
        
        # Verificar que ambas muestras tengan una columna 'id' para la unión
        if (!("id" %in% names(data_A)) || !("id" %in% names(data_B))) {
            stop("Ambas muestras deben contener una columna 'id' para realizar la unión.")
        }
        
        # Realizar la unión completa para manejar intersecciones
        data_combined <- full_join(
            data_A %>% mutate(!!ind_var_A := 1),
            data_B %>% mutate(!!ind_var_B := 1),
            by = "id"
        ) %>%
            mutate(
                !!ind_var_A := ifelse(is.na(!!sym(ind_var_A)), 0, !!sym(ind_var_A)),
                !!ind_var_B := ifelse(is.na(!!sym(ind_var_B)), 0, !!sym(ind_var_B))
            )
        
        data <- data_combined
    }
    
    # 3. Definir los pesos de muestreo
    # Asignar pesos para S_A
    if (!is.null(weights_A)) {
        if (!(weights_A %in% names(data))) {
            stop(paste("La columna especificada en 'weights_A' no se encuentra en 'data'."))
        }
        data$d_i_A <- ifelse(data[[ind_var_A]] == 1, data[[weights_A]], 0)
    } else {
        # Asignar pesos basados en N_total
        if (is.null(N_total)) {
            N_total <- nrow(data)
        }
        data$d_i_A <- ifelse(data[[ind_var_A]] == 1, N_total / sum(data[[ind_var_A]] == 1, na.rm = TRUE), 0)
    }
    
    # Asignar pesos para S_B (no se usa directamente en RegDI2, pero se deja para consistencia)
    if (!is.null(weights_B)) {
        if (!(weights_B %in% names(data))) {
            stop(paste("La columna especificada en 'weights_B' no se encuentra en 'data'."))
        }
        data$d_i_B <- ifelse(data[[ind_var_B]] == 1, data[[weights_B]], 0)
    } else {
        data$d_i_B <- ifelse(data[[ind_var_B]] == 1, N_total / sum(data[[ind_var_B]] == 1, na.rm = TRUE), 0)
    }
    
    # 4. Calcular N_total si no se proporciona
    if (is.null(N_total)) {
        N_total <- nrow(data)
    }
    
    # 5. Calcular tamaños de muestra A y B
    size_A <- sum(data[[ind_var_A]] == 1, na.rm = TRUE)
    size_B <- sum(data[[ind_var_B]] == 1, na.rm = TRUE)
    
    if (size_A == 0) stop("No hay unidades en la muestra A.")
    if (size_B == 0) stop("No hay unidades en la muestra B.")
    
    # 6. Calcular los totales poblacionales automáticamente
    if (!is.null(aux_vars)) {
        pop_totals <- sapply(aux_vars, function(var) {
            sum(data[[var]], na.rm = TRUE)
        })
        names(pop_totals) <- aux_vars
    } else {
        pop_totals <- NULL
    }
    
    # 7. Crear variables auxiliares delta_
    data$uno <- 1
    data$delta_i <- ifelse(data[[ind_var_B]] == 1, 1, 0)
    data$delta_yi <- ifelse(data[[ind_var_B]] == 1, data[[y_B_col]], 0)
    
    if (!is.null(aux_vars)) {
        for (z in aux_vars) {
            data[[paste0("delta_", z)]] <- ifelse(data[[ind_var_B]] == 1, data[[z]], 0)
        }
    }
    
    # 8. Preparar los totales poblacionales para calibración
    totals <- c(
        uno = sum(data$uno, na.rm = TRUE),
        delta_i = sum(data$delta_i, na.rm = TRUE),
        delta_yi = sum(data$delta_yi, na.rm = TRUE)
    )
    
    if (!is.null(aux_vars)) {
        aux_totals <- sapply(aux_vars, function(z) sum(data[[paste0("delta_", z)]], na.rm = TRUE))
        names(aux_totals) <- paste0("delta_", aux_vars)
        totals <- c(totals, aux_totals)
    }
    
    # 9. Crear la fórmula de calibración
    if (!is.null(aux_vars)) {
        delta_aux_vars <- paste0("delta_", aux_vars)
        calibration_formula <- as.formula(
            paste("~0 + uno + delta_i + delta_yi +", paste(delta_aux_vars, collapse = " + "))
        )
    } else {
        calibration_formula <- ~0 + uno + delta_i + delta_yi
    }
    
    # 10. Crear el diseño de encuesta con los pesos iniciales
    design <- svydesign(
        ids = ~1, 
        data = subset(data, data[[ind_var_A]] == 1), 
        weights = ~d_i_A
    )
    
    # 11. Realizar la calibración
    calibrated_design <- calibrate(
        design,
        formula = calibration_formula,
        population = totals,
        aggregate.stage = 1  # Agregar aggregate.stage para prevenir el warning
    )
    
    # 12. Función interna para calcular el estimador Doble Robusto
    calculate_DR_RegDI <- function(data, calibrated_design, y_A_col, aux_vars, N_total, outcome_model, eval_model_performance) {
        # 1. Estimar el modelo de resultado usando las muestras A
        if (is.null(outcome_model)) {
            stop("Debe especificar el modelo de resultado a través del parámetro 'outcome_model'.")
        }
        
        # Ajustar el modelo de resultado usando solo las observaciones con Y observado
        prediction_formula <- as.formula(outcome_model)
        outcome_fit <- lm(prediction_formula, data = subset(data, data[[ind_var_A]] == 1))
        
        # Calcular los valores predichos para toda la población
        data$Y_pred <- predict(outcome_fit, newdata = data)
        
        # Calcular el estimador Doble Robusto
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
        
        # Evaluar el rendimiento del modelo si se solicita
        if (eval_model_performance) {
            residuals <- data[[y_A_col]][data[[ind_var_A]] == 1] - data$Y_pred[data[[ind_var_A]] == 1]
            RMSE <- sqrt(mean(residuals^2, na.rm = TRUE))
            
            # Calcular R^2
            SST <- sum((data[[y_A_col]][data[[ind_var_A]] == 1] - mean(data[[y_A_col]][data[[ind_var_A]] == 1], na.rm = TRUE))^2, na.rm = TRUE)
            SSR <- sum(residuals^2, na.rm = TRUE)
            R2 <- 1 - (SSR / SST)
            
            return(list(mean_DR_RegDI = T_DR_RegDI, var_DR_RegDI = V_DR_RegDI, RMSE = RMSE, R2 = R2))
        } else {
            return(list(mean_DR_RegDI = T_DR_RegDI, var_DR_RegDI = V_DR_RegDI))
        }
    }
    
    # 13. Aplicar la corrección según el valor de 'correction'
    if (correction == 1) {
        # Corrección por calibración
        svymean_result <- svymean(as.formula(paste("~", y_A_col)), calibrated_design)
        T_RegDI <- as.numeric(svymean_result[1])
        V_RegDI <- as.numeric(attr(svymean_result, "var")[1])
        
    } else if (correction == 2) {
        # Corrección por calibración y errores de medición
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
            uno = sum(data$uno, na.rm = TRUE),
            delta_i = sum(data$delta_i, na.rm = TRUE),
            delta_yi_corrected = sum(data$delta_yi_corrected, na.rm = TRUE)
        )
        
        if (!is.null(aux_vars)) {
            aux_totals_corrected <- sapply(aux_vars, function(z) sum(data[[paste0("delta_", z)]], na.rm = TRUE))
            names(aux_totals_corrected) <- paste0("delta_", aux_vars)
            totals_corrected <- c(totals_corrected, aux_totals_corrected)
        }
        
        # Crear la fórmula de calibración corregida
        if (!is.null(aux_vars)) {
            delta_aux_vars <- paste0("delta_", aux_vars)
            calibration_formula_corrected <- as.formula(
                paste("~0 + uno + delta_i + delta_yi_corrected +", paste(delta_aux_vars, collapse = " + "))
            )
        } else {
            calibration_formula_corrected <- ~0 + uno + delta_i + delta_yi_corrected
        }
    
        # Crear el diseño de encuesta corregido
        design_corrected <- svydesign(
            ids = ~1,
            data = subset(data, data[[ind_var_A]] == 1),
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

    # Devolver resultados incluyendo métricas de rendimiento si corresponde
    if (correction == 3 && eval_model_performance) {
        return(list(mean_RegDI = T_RegDI, var_RegDI = V_RegDI, RMSE = RMSE, R2 = R2))
    } else {
        return(list(mean_RegDI = T_RegDI, var_RegDI = V_RegDI))
    }
}
