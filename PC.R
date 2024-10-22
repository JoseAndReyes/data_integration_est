library(survey)

# Cargar librerías necesarias
library(survey)

PC_Estimator <- function(data = NULL,
                         data_A = NULL,
                         data_B = NULL,
                         ind_var_A = "ind_A",      # Variable indicadora para muestra A
                         ind_var_B = "ind_B",      # Variable indicadora para muestra B
                         y_A_col = NULL,
                         y_B_col = NULL,
                         aux_vars = NULL,
                         N_total = NULL,
                         weights_A = NULL,         # Pesos para muestra A
                         weights_B = NULL,         # Pesos para muestra B (solo para PC)
                         outcome_model = NULL,     # Modelo de resultado (similar a RegDI2)
                         model_type = "normal",    # Tipo de modelo ("normal", "logistic")
                         scenario = 1,
                         eval_model_performance = FALSE,
                         verbose = FALSE
                         ) {
    
    # 1. Validar que al menos se proporcione 'data' o ambas 'data_A' y 'data_B'
    if (is.null(data) && (is.null(data_A) || is.null(data_B))) {
        stop("Debe proporcionar 'data' o ambas 'data_A' y 'data_B'.")
    }
    
    # 2. Combinar data_A y data_B si se proporcionan por separado
    if (!is.null(data_A) | !is.null(data_B)) {
        if (is.null(data_A) | is.null(data_B)) {
            stop("Si se proporciona 'data_A' o 'data_B', ambos deben ser proporcionados.")
        }
        
        # Verificar que ambas muestras tengan una columna 'id' para la unión
        if (!("id" %in% names(data_A)) | !("id" %in% names(data_B))) {
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
    
    # 3. Verificar y Calcular N_total antes de asignar pesos temporales
    if (is.null(N_total)) {
        if (!is.null(data)) {
            # Si 'data' representa la población completa
            N_total <- nrow(data)
        } else if (!is.null(weights_A)) {
            # Si 'weights_A' está proporcionado, estimar N_total usando Horvitz-Thompson
            # Crear un diseño de encuesta para la muestra A
            design_S_A <- svydesign(ids = ~1, data = subset(data, data[[ind_var_A]] == 1), weights = ~get(weights_A))
            
            # Estimar N_total
            est_N_total <- svytotal(~1, design_S_A)
            N_total <- coef(est_N_total)
        } else {
            stop("Debe proporcionar 'N_total', 'weights_A' o el dataframe completo 'data' para estimar 'N_total'.")
        }
    } else {
        NULL
    }
    
    # 4. Definir los pesos de muestreo
    # Asignar pesos para S_A
    if (!is.null(weights_A)) {
        if (!(weights_A %in% names(data))) {
            stop(paste("La columna especificada en 'weights_A' no se encuentra en 'data'."))
        }
        data$d_i_A <- ifelse(data[[ind_var_A]] == 1, data[[weights_A]], 0)
    } else {
        # Asignar pesos temporales basados en N_total
        data$d_i_A <- ifelse(data[[ind_var_A]] == 1, N_total / sum(data[[ind_var_A]] == 1, na.rm = TRUE), 0)
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
        if (!is.null(data)) {
            # Si 'data' representa la población completa, sumar directamente
            pop_totals <- sapply(aux_vars, function(var) {
                sum(data[[var]], na.rm = TRUE)
            })
            names(pop_totals) <- aux_vars
        } else if (!is.null(weights_A)) {
            # Estimar 'pop_totals' usando Horvitz-Thompson basado en la muestra A
            # Crear un diseño de encuesta para la muestra A
            design_S_A <- svydesign(ids = ~1, data = subset(data, data[[ind_var_A]] == 1), weights = ~d_i_A)
            
            # Estimar los totales poblacionales de las variables auxiliares
            pop_totals_estimated <- svytotal(as.formula(paste("~", paste(aux_vars, collapse = " + "))), design_S_A)
            pop_totals <- coef(pop_totals_estimated)
        } else {
            stop("Para calcular 'pop_totals' se requiere 'data' completo o 'weights_A' proporcionados con 'aux_vars'.")
        }
    } else {
        pop_totals <- NULL
    }
    
    # 7. Crear fórmula de calibración
    if (!is.null(aux_vars)) {
        calibration_formula <- as.formula(paste("~0 +", paste(aux_vars, collapse = " + ")))
        if (verbose) print(paste("Fórmula de calibración:", deparse(calibration_formula)))
    } else {
        calibration_formula <- NULL
    }
    
    # 8. Calibrar pesos para S_B
    design_S_B <- svydesign(ids = ~1, data = subset(data, data[[ind_var_B]] == 1), weights = ~d_i_B)
    
    if (!is.null(aux_vars)) {
        if (is.null(pop_totals)) {
            stop("Los totales poblacionales de variables auxiliares deben ser proporcionados o estimados.")
        }
        calibrated_design_S_B <- tryCatch({
            calibrate(
                design = design_S_B,
                formula = calibration_formula,
                population = pop_totals,
                calfun = "linear"
            )
        }, error = function(e) {
            stop("Error en la calibración de pesos: ", e$message)
        })
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
        data_intersect <- subset(data, data[[ind_var_A]] == 1 & data[[ind_var_B]] == 1)
        if (nrow(data_intersect) == 0) {
            stop("No hay unidades en la intersección de S_A y S_B para estimar el modelo.")
        }
        
        prediction_formula <- as.formula(outcome_model)
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
        
        # Construir el modelo de predicción utilizando únicamente S_A
        prediction_formula <- as.formula(outcome_model)
        if (model_type == "normal") {
            model <- lm(prediction_formula, data = subset(data, data[[ind_var_A]] == 1))

        } else if (model_type == "logistic") {
            model <- glm(prediction_formula, data = subset(data, data[[ind_var_A]] == 1), family = binomial)

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
