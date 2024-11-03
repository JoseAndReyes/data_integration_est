# Proyecto de Tesis

## Introducción
Este proyecto de tesis implementa dos estimadores principales: el estimador **RegDI** y el estimador **PC**. Ambos estimadores están pensados en resolver el problema de integración de muestras probabilísticas y no probabilísticas como lo serían las muestras de Big Data. En las siguientes secciones se describe cada estimador junto con la referencia teórica y la representación matemática correspondiente.

### Estimador RegDI
El estimador **RegDI** está desarrollado para ofrecer soporte en la estimación de datos bajo restricciones específicas de calibración. Este estimador es capaz de manejar múltiples restricciones y está diseñado para modelos de error de medición en casos donde se requiere medición de la muestra probabilística.

- **Referencia:** [Espacio para incluir referencia]
- **Representación matemática:** [Espacio para incluir la forma matemática]

### Estimador PC
El estimador **PC** permite un enfoque robusto en el análisis de datos, incluyendo soporte para modelos doble robusto y gestión de errores de medición en la muestra B. Este estimador también es útil en escenarios donde el supuesto de **MAR (Missing at Random)** no es válido.

- **Referencia:** [Espacio para incluir referencia]
- **Representación matemática:** [Espacio para incluir la forma matemática]

## Simulaciones
El archivo `Simulaciones_Clean.ipynb` incluye todas las simulaciones realizadas en el marco de este proyecto de tesis. Estas simulaciones abarcan tanto aquellas realizadas bajo la metodología de [Referencia PC] como algunas simulaciones propias diseñadas para explorar distintos escenarios y supuestos. Las simulaciones incluidas son:

- **Simulación de muestreo estratificado**
- **Simulación para violación del supuesto MAR**
- **Simulación para probar la doble robustez**
- **Simulación con múltiples covariables**

Adicionalmente, se incluye una aplicación con datos reales (fuente de datos a definir) para evaluar el rendimiento de los estimadores en situaciones del mundo real.

## Características de los Estimadores

### Características del Estimador RegDI
- Soporte para múltiples restricciones de calibración.
- Soporte para modelos de error de medición en el caso de medición de la muestra probabilística.
- Estimación de varianza mediante [falta método].

### Características del Estimador PC
- Soporte para múltiples restricciones de calibración.
- Soporte para el uso de modelo doble robusto en el caso de errores de medición en la muestra B.
- Soporte para el uso de modelo doble robusto en el caso de violación del supuesto MAR.
