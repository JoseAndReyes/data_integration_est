## Introducción
Este proyecto implementa dos estimadores principales: el estimador **RegDI** y el estimador **PC**. Ambos estimadores están pensados en resolver el problema de integración de muestras probabilísticas y no probabilísticas como lo serían las muestras de Big Data. En las siguientes secciones se describe cada estimador junto con la referencia teórica y la representación matemática correspondiente.

### Estimador RegDI
El estimador **RegDI** se basa en la integración de datos mediante calibración, utilizando los totales conocidos de la muestra de big data, como el tamaño de esta o el total de la variable de interés, para reducir el sesgo de selección en la estimación de la población finita. Este método puede emplear otras variables auxiliares y minimiza una función objetivo cuadrática, sujeta a restricciones de calibración, para obtener pesos ajustados que coincidan con los totales poblacionales conocidos. La técnica permite, además, manejar errores de medición en la muestra de big data y adaptarse para tratar errores de medición en muestras probabilísticas, como en encuestas desactualizadas.

La estimación basada en RegDI está dada por:

$$
\widehat{T}\_{\text{RegDI}} = \sum_{i \in A} w_i y_i
$$

donde los pesos $w_i$ están determinados para minimizar una función objetivo, sujeta a las siguientes restricciones de calibración:

$$
\sum_{i \in A} w_i x_{i} = \sum_{i=1}^{N} x_{i} 
$$

Aquí, $x_{i}$ representa las variables auxiliares conocidas en la población basados en la muestra no probablística, como el tamaño de la muestra de big data $N_b$ y el total de la variable de interés en la muestra de Big Data. 

- **Referencia:** Kim, J.-K., & Tam, S.-M. (2021). Data integration by combining big data and survey sample data for finite population inference. *International Statistical Review, 89*(2), 382–401. [Wiley Online Library](https://onlinelibrary.wiley.com/doi/10.1111/insr.12345)

### Estimador PC
El estimador **PC** o de pseudo-calibración utiliza una combinación de ponderaciones ajustadas y datos auxiliares para estimar parámetros de la población a partir de muestras no probabilísticas. Este método emplea un modelo de propensión implícito y al igual que RegDI, ajustanda los pesos, esta vez basados en la muestra no probabilística, para que cumplan con restricciones de calibración, las cuales se basan en totales conocidos de una encuesta de referencia. Este enfoque permite corregir el sesgo de selección y adaptar el estimador para poblaciones donde la muestra de big data puede tener una sub-cobertura o no seguir un esquema de probabilidad.

La estimación basada en PC está dada por:

$$
\widehat{Y}\_{\text{PC}} = \sum_{i \in \mathcal{S}\_{B}} w_{i}^{B} y_{i}
$$

donde los pesos $w_{i}^{B}$ se determinan para minimizar una función de distancia, sujeta a las siguientes restricciones de calibración:

$$
\sum_{i \in \mathcal{S}\_{B}} w_{i}^{B} x_{i} = \mathbf{X}^{*}
$$

Aquí, $\mathbf{X}^{*}$ representa los totales conocidos o estimados de la población, obtenidos de una encuesta de referencia o un censo, para variables auxiliares relevantes en la estimación.

- **Referencia:** Golini, N., & Righi, P. (2024). Integrating probability and big non-probability samples data to produce Official Statistics. *Statistical Methods & Applications, 33*(2), 555–580. [Springer](https://link.springer.com/article/10.1007/s10260-024-00456-8)
- **Representación matemática:** [Espacio para incluir la forma matemática]

## Simulaciones
El archivo `Simulaciones_Clean.ipynb` incluye todas las simulaciones realizadas en el marco de este proyecto de tesis. Estas simulaciones abarcan tanto aquellas realizadas bajo la metodología de [Referencia PC] como algunas simulaciones propias diseñadas para explorar distintos escenarios y supuestos. Las simulaciones incluidas son:

- **Simulación de muestreo estratificado**
- **Simulación para violación del supuesto MAR**
- **Simulación para probar la doble robustez**
- **Simulación con múltiples covariables** [En desarrollo]

Adicionalmente, se incluye una aplicación con datos reales (fuente de datos a definir) para evaluar el rendimiento de los estimadores en situaciones del mundo real.

## Características de los Estimadores

### Características del Estimador RegDI
- Soporte para múltiples restricciones de calibración.
- Soporte para modelos de error de medición en el caso de medición de la muestra probabilística de la forma $\widehat{T}\_{RegDI}=\sum_{i \in A} w_{i} \widehat{y}\_{i}$, donde $\widehat{y}\_{i}$ se obtiene mediante un modelo de error de medición de la forma $\widehat{y_{i}}=\beta_{0}+\beta_{1} y_{i}+e_{i}$
- Estimación de varianza mediante [falta método].

### Características del Estimador PC
- Soporte para múltiples restricciones de calibración.
- Soporte para el uso de modelo doble robusto en el caso de errores de medición en la muestra B de la forma: $\hat{Y}\_{P C}^{D}=\sum_{i \in \mathcal{S}\_{B}} w_{i}^{B} \tilde{y}\_{i}+\sum_{i \in \mathcal{S}\_{A}} d_{i}^{A}\left(y_{i}-\hat{y}_{i}\right)$ donde $\tilde{y}\_{i}$ está obtenido por un modelo de predicción o de error de medición. 
- Soporte para el uso de modelo doble robusto en el caso de violación del supuesto MAR de la forma: $\hat{Y}\_{D R 2}=\sum_{i \in \mathcal{S}\_{B}} w_{i}^{B}\left(y_{i}-\hat{y}\_{i}\right)+\sum_{i \in \mathcal{S}\_{A}} d_{i}^{A}$. Nuevamente $\hat{y}$ es un modelo ajustado, pero esta vez con la muestra probablística.
- Estimación de varianza mediante [falta método].
