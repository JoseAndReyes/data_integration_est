library(survey)

PC_Estimator <- function(data, y_B_col, sample_B_col,
                                   aux_vars, pop_totals,
                                   initial_weight = NULL, weights_B_col = NULL) {
  
  # Subconjunto de datos para S_B
  data_S_B <- data[data[[sample_B_col]] == 1, ]
  n_B <- nrow(data_S_B)
  
  if (n_B == 0) stop("No hay unidades en la muestra S_B.")
  
  # Asignar pesos iniciales
  if (!is.null(weights_B_col)) {
    data_S_B$d_i_B <- data_S_B[[weights_B_col]]
  } else if (is.null(initial_weight)) {
    data_S_B$d_i_B <- N / n_B
  } else if (is.numeric(initial_weight)) {
    data_S_B$d_i_B <- initial_weight
  } else if (initial_weight == "uniform") {
    data_S_B$d_i_B <- 1
  } else {
    stop("initial_weight debe ser numérico o 'uniform'.")
  }
  
  # Crear fórmula de calibración
  calibration_formula <- as.formula(paste("~0+", paste(aux_vars, collapse = " + ")))
  
  # Crear diseño de encuesta
  design_S_B <- svydesign(ids = ~1, data = data_S_B, weights = ~d_i_B)
  
  # Calibrar pesos
  calibrated_design <- calibrate(
    design = design_S_B,
    formula = calibration_formula,
    population = pop_totals,
    calfun = "linear"
  )
  
  # Estimar la media
  mean_estimated <- svymean(as.formula(paste("~", y_B_col)), calibrated_design)
  mean_PC <- coef(mean_estimated)
  
  # Estimar el SE
  SE_PC <- SE(mean_estimated)
  
  # Obtener pesos calibrados
  weights_calibrated <- weights(calibrated_design)
  
  return(list(
    mean_PC = mean_PC,
    SE_PC = SE_PC,
    weights = weights_calibrated
  ))
}