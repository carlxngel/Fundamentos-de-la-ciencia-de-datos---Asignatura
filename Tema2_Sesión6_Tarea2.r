# ========================================
# VISUALIZACIÓN AVANZADA - STUDENTS PERFORMANCE
# ========================================

library(ggplot2)
library(dplyr)

# Cargar datos
students <- read.csv("Carga de datos/StudentsPerformance.csv")
colnames(students) <- c("gender", "race_ethnicity", "parental_education", 
                        "lunch", "test_prep_course", "math_score", 
                        "reading_score", "writing_score")

cat("================================================================================\n")
cat("PREGUNTA 1: ¿CÓMO APLICAR FACETING EN LA COMPARACIÓN DE VARIABLES?\n")
cat("================================================================================\n\n")

cat("DEFINICIÓN DE FACETING:\n")
cat("----------------------\n")
cat("Faceting (o facetas) es la técnica de dividir un gráfico en múltiples\n")
cat("paneles (subgráficos) basados en los valores de una o más variables categóricas.\n")
cat("Permite comparar patrones entre diferentes grupos de manera simultánea.\n\n")

cat("TIPOS DE FACETING EN GGPLOT2:\n")
cat("-----------------------------\n")
cat("1. facet_wrap() - Organiza paneles en una cuadrícula flexible (1 variable)\n")
cat("2. facet_grid() - Organiza paneles en filas y columnas específicas (2 variables)\n\n")

# EJEMPLO 1: facet_wrap por una variable
cat("EJEMPLO 1: FACET_WRAP POR GÉNERO\n")
cat("---------------------------------\n")

plot1a <- ggplot(students, aes(x = reading_score, y = writing_score)) +
  geom_point(alpha = 0.4, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  facet_wrap(~ gender, nrow = 1) +
  labs(
    title = "Faceting por Género (facet_wrap)",
    subtitle = "Comparación lado a lado del rendimiento por género",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11)
  )

print(plot1a)

cat("\nCÓDIGO:\n")
cat("facet_wrap(~ gender, nrow = 1)\n\n")

cat("VENTAJAS:\n")
cat("- Comparación visual directa entre grupos\n")
cat("- Misma escala en todos los paneles (por defecto)\n")
cat("- Fácil identificación de diferencias de patrones\n\n\n")

# EJEMPLO 2: facet_wrap por otra variable
cat("EJEMPLO 2: FACET_WRAP POR TIPO DE ALMUERZO\n")
cat("-------------------------------------------\n")

plot1b <- ggplot(students, aes(x = reading_score, y = writing_score, color = gender)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  facet_wrap(~ lunch) +
  labs(
    title = "Faceting por Tipo de Almuerzo",
    subtitle = "Comparación del rendimiento según nivel socioeconómico",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura",
    color = "Género"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("female" = "#E91E63", "male" = "#2196F3"),
    labels = c("female" = "Femenino", "male" = "Masculino")
  )

print(plot1b)

cat("\nCÓDIGO:\n")
cat("facet_wrap(~ lunch)\n\n")

# EJEMPLO 3: facet_grid con dos variables
cat("EJEMPLO 3: FACET_GRID CON DOS VARIABLES (GÉNERO × ALMUERZO)\n")
cat("-----------------------------------------------------------\n")

plot1c <- ggplot(students, aes(x = reading_score, y = writing_score)) +
  geom_point(alpha = 0.4, size = 1.5, color = "#667eea") +
  geom_smooth(method = "lm", se = TRUE, color = "#f093fb", linewidth = 1) +
  facet_grid(gender ~ lunch) +
  labs(
    title = "Faceting con facet_grid: Género × Tipo de Almuerzo",
    subtitle = "4 paneles: cada combinación de género y almuerzo",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold", size = 10)
  )

print(plot1c)

cat("\nCÓDIGO:\n")
cat("facet_grid(gender ~ lunch)\n")
cat("         (filas ~ columnas)\n\n")

cat("APLICACIONES DEL FACETING:\n")
cat("-------------------------\n")
cat("✓ Comparar grupos demográficos (género, edad, región)\n")
cat("✓ Analizar efectos de intervenciones (con/sin programa)\n")
cat("✓ Visualizar patrones temporales (por año, mes, día)\n")
cat("✓ Explorar segmentación de datos (alto/medio/bajo rendimiento)\n\n")

cat("CUÁNDO USAR CADA TIPO:\n")
cat("---------------------\n")
cat("facet_wrap: Una variable categórica, distribución flexible de paneles\n")
cat("facet_grid: Dos variables categóricas, estructura de filas × columnas\n\n\n")


cat("================================================================================\n")
cat("PREGUNTA 2: ¿POR QUÉ INCLUIR TÍTULOS Y ETIQUETAS? ¿QUÉ IMPACTO TIENEN?\n")
cat("================================================================================\n\n")

cat("IMPORTANCIA DE TÍTULOS Y ETIQUETAS:\n")
cat("----------------------------------\n\n")

# EJEMPLO MALO: Sin títulos ni etiquetas claras
cat("EJEMPLO MALO: GRÁFICO SIN CONTEXTO\n")
cat("----------------------------------\n")

plot2a <- ggplot(students, aes(x = reading_score, y = writing_score)) +
  geom_point() +
  theme_minimal()

print(plot2a)

cat("\nPROBLEMAS:\n")
cat("❌ No se sabe qué representan los ejes\n")
cat("❌ No hay contexto del análisis\n")
cat("❌ Difícil de interpretar sin explicación adicional\n")
cat("❌ No es autoexplicativo\n\n")

# EJEMPLO BUENO: Con títulos y etiquetas completos
cat("EJEMPLO BUENO: GRÁFICO CON CONTEXTO COMPLETO\n")
cat("--------------------------------------------\n")

plot2b <- ggplot(students, aes(x = reading_score, y = writing_score, color = gender)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  labs(
    title = "Relación entre Puntuaciones de Lectura y Escritura",
    subtitle = "Análisis de 1000 estudiantes de secundaria por género",
    x = "Puntuación en Lectura (0-100)",
    y = "Puntuación en Escritura (0-100)",
    color = "Género",
    caption = "Fuente: Students Performance Dataset | Análisis: 2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    axis.title = element_text(face = "bold", size = 11),
    legend.title = element_text(face = "bold")
  ) +
  scale_color_manual(
    values = c("female" = "#E91E63", "male" = "#2196F3"),
    labels = c("female" = "Femenino", "male" = "Masculino")
  )

print(plot2b)

cat("\nVENTAJAS:\n")
cat("✓ AUTOEXPLICATIVO: Cualquier persona puede entenderlo sin contexto adicional\n")
cat("✓ PROFESIONAL: Apto para presentaciones, reportes e informes\n")
cat("✓ TRAZABILIDAD: Incluye fuente y fecha del análisis\n")
cat("✓ CLARIDAD: Elimina ambigüedades sobre qué se está mostrando\n\n")

cat("IMPACTO EN LA INTERPRETACIÓN:\n")
cat("----------------------------\n")
cat("1. TÍTULO: Comunica el mensaje principal o pregunta de investigación\n")
cat("2. SUBTÍTULO: Proporciona contexto adicional (muestra, periodo, grupo)\n")
cat("3. EJES: Indican claramente las variables y sus unidades de medida\n")
cat("4. LEYENDA: Explica la codificación de colores, formas o tamaños\n")
cat("5. CAPTION: Da crédito a la fuente y documenta el análisis\n\n")

cat("ELEMENTOS CLAVE DE UN BUEN TÍTULO:\n")
cat("---------------------------------\n")
cat("✓ Específico (no genérico como 'Gráfico 1')\n")
cat("✓ Descriptivo (dice QUÉ se está mostrando)\n")
cat("✓ Conciso (idealmente menos de 15 palabras)\n")
cat("✓ Informativo (comunica el hallazgo principal si es posible)\n\n\n")


cat("================================================================================\n")
cat("PREGUNTA 3: ¿CÓMO AYUDA LA INCLUSIÓN DE TEXTO EN LOS GRÁFICOS?\n")
cat("================================================================================\n\n")

cat("TIPOS DE TEXTO EN GRÁFICOS:\n")
cat("--------------------------\n")
cat("1. Anotaciones: Destacar puntos específicos de interés\n")
cat("2. Etiquetas de datos: Valores numéricos directos en el gráfico\n")
cat("3. Texto explicativo: Notas que guían la interpretación\n")
cat("4. Referencias: Líneas de referencia con etiquetas\n\n")

# EJEMPLO 1: Gráfico con anotaciones
cat("EJEMPLO 1: ANOTACIONES PARA DESTACAR PUNTOS IMPORTANTES\n")
cat("-------------------------------------------------------\n")

# Identificar outliers o puntos de interés
outliers <- students %>%
  mutate(diferencia = abs(writing_score - reading_score)) %>%
  arrange(desc(diferencia)) %>%
  head(3)

plot3a <- ggplot(students, aes(x = reading_score, y = writing_score)) +
  geom_point(alpha = 0.3, size = 2, color = "gray50") +
  geom_point(data = outliers, aes(x = reading_score, y = writing_score),
             color = "red", size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 1) +
  annotate("text", x = 95, y = 20, 
           label = "Zona de\nrendimiento atípico", 
           color = "red", size = 4, fontface = "bold") +
  annotate("curve", x = 90, y = 25, xend = outliers$reading_score[1], 
           yend = outliers$writing_score[1],
           arrow = arrow(length = unit(0.3, "cm")), 
           color = "red", curvature = 0.3) +
  annotate("segment", x = 20, y = 20, xend = 100, yend = 100,
           linetype = "dashed", color = "green", linewidth = 0.8) +
  annotate("text", x = 85, y = 90, 
           label = "Línea ideal\n(Lectura = Escritura)", 
           color = "green", size = 3.5, fontface = "italic") +
  labs(
    title = "Identificación de Casos Atípicos con Anotaciones",
    subtitle = "Estudiantes con mayor diferencia entre lectura y escritura",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(plot3a)

cat("\nCÓDIGO CLAVE:\n")
cat("annotate('text', x = 95, y = 20, label = 'Zona de rendimiento atípico')\n")
cat("annotate('curve', ..., arrow = arrow(...))  # Flechas\n")
cat("annotate('segment', ..., linetype = 'dashed')  # Líneas de referencia\n\n")

cat("VENTAJAS:\n")
cat("✓ Dirige la atención a hallazgos importantes\n")
cat("✓ Contextualiza valores extremos o inusuales\n")
cat("✓ Reduce la necesidad de explicaciones externas\n\n")

# EJEMPLO 2: Etiquetas de estadísticas
cat("EJEMPLO 2: INCLUIR ESTADÍSTICAS CLAVE EN EL GRÁFICO\n")
cat("---------------------------------------------------\n")

# Calcular estadísticas
correlacion <- cor(students$reading_score, students$writing_score)
n_estudiantes <- nrow(students)

plot3b <- ggplot(students, aes(x = reading_score, y = writing_score)) +
  geom_point(alpha = 0.4, size = 2, color = "#667eea") +
  geom_smooth(method = "lm", se = TRUE, color = "#f093fb", linewidth = 1.2) +
  annotate("label", x = 25, y = 95, 
           label = sprintf("r = %.3f\nn = %d estudiantes", correlacion, n_estudiantes),
           size = 4.5, fontface = "bold", 
           fill = "white", color = "black", alpha = 0.9) +
  labs(
    title = "Correlación entre Lectura y Escritura",
    subtitle = "Con estadísticas clave incluidas en el gráfico",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(plot3b)

cat("\nCÓDIGO:\n")
cat("annotate('label', x = 25, y = 95,\n")
cat("         label = sprintf('r = %.3f\\nn = %d', correlacion, n),\n")
cat("         fill = 'white', alpha = 0.9)\n\n")

cat("VENTAJAS:\n")
cat("✓ El lector obtiene información cuantitativa inmediata\n")
cat("✓ No necesita buscar estadísticas en tablas separadas\n")
cat("✓ Facilita la toma de decisiones basada en datos\n\n")

# EJEMPLO 3: Texto explicativo por faceta
cat("EJEMPLO 3: TEXTO EXPLICATIVO POR GRUPO\n")
cat("--------------------------------------\n")

# Calcular promedios por género
promedios <- students %>%
  group_by(gender) %>%
  summarise(
    avg_reading = mean(reading_score),
    avg_writing = mean(writing_score),
    .groups = "drop"
  )

plot3c <- ggplot(students, aes(x = reading_score, y = writing_score)) +
  geom_point(alpha = 0.4, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  facet_wrap(~ gender) +
  geom_text(data = promedios, 
            aes(x = 30, y = 90, 
                label = sprintf("Promedio:\nLectura: %.1f\nEscritura: %.1f", 
                               avg_reading, avg_writing)),
            size = 3.5, fontface = "bold", color = "navy") +
  labs(
    title = "Rendimiento con Promedios por Género",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura"
  ) +
  theme_minimal()

print(plot3c)

cat("\nBENEFICIOS DEL TEXTO EN GRÁFICOS:\n")
cat("--------------------------------\n")
cat("1. GUÍA LA INTERPRETACIÓN: Destaca lo que el autor quiere que veas\n")
cat("2. AHORRA TIEMPO: El lector no tiene que calcular o buscar valores\n")
cat("3. CUENTA UNA HISTORIA: Transforma datos en narrativa\n")
cat("4. AUMENTA PRECISIÓN: Reduce malentendidos o interpretaciones erróneas\n")
cat("5. HACE GRÁFICOS AUTOSUFICIENTES: No requieren explicación verbal\n\n\n")


cat("================================================================================\n")
cat("PREGUNTA 4: ¿QUÉ VENTAJAS TIENE LIMITAR LOS EJES A UN RANGO ESPECÍFICO?\n")
cat("================================================================================\n\n")

cat("CONTROL DE ESCALAS EN EJES:\n")
cat("--------------------------\n\n")

# EJEMPLO 1: Sin límites (automático)
cat("EJEMPLO 1: EJES AUTOMÁTICOS (SIN LÍMITES)\n")
cat("-----------------------------------------\n")

plot4a <- ggplot(students, aes(x = reading_score, y = writing_score)) +
  geom_point(alpha = 0.4, size = 2, color = "#667eea") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Gráfico con Ejes Automáticos",
    subtitle = "ggplot2 ajusta automáticamente basándose en los datos",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura"
  ) +
  theme_minimal()

print(plot4a)

cat("\nRANGOS AUTOMÁTICOS:\n")
cat("X:", range(students$reading_score), "\n")
cat("Y:", range(students$writing_score), "\n\n")

cat("PROBLEMA: El rango se ajusta solo a los datos, no al contexto real\n\n")

# EJEMPLO 2: Con límites establecidos (0-100)
cat("EJEMPLO 2: EJES LIMITADOS AL RANGO REAL DE PUNTUACIONES (0-100)\n")
cat("---------------------------------------------------------------\n")

plot4b <- ggplot(students, aes(x = reading_score, y = writing_score)) +
  geom_point(alpha = 0.4, size = 2, color = "#667eea") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(
    title = "Gráfico con Ejes Limitados (0-100)",
    subtitle = "Muestra el contexto completo de las puntuaciones posibles",
    x = "Puntuación en Lectura (0-100)",
    y = "Puntuación en Escritura (0-100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

print(plot4b)

cat("\nCÓDIGO:\n")
cat("coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))\n\n")

cat("VENTAJAS:\n")
cat("✓ Muestra el CONTEXTO REAL de las puntuaciones\n")
cat("✓ Evita EXAGERAR diferencias visuales\n")
cat("✓ Permite COMPARACIÓN justa entre múltiples gráficos\n\n")

# EJEMPLO 3: Zoom en región de interés
cat("EJEMPLO 3: ZOOM EN REGIÓN ESPECÍFICA DE INTERÉS\n")
cat("-----------------------------------------------\n")

plot4c <- ggplot(students, aes(x = reading_score, y = writing_score, color = gender)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  coord_cartesian(xlim = c(60, 100), ylim = c(60, 100)) +
  labs(
    title = "Zoom en Estudiantes de Alto Rendimiento (60-100)",
    subtitle = "Análisis detallado del segmento superior",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura",
    color = "Género"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("female" = "#E91E63", "male" = "#2196F3"),
    labels = c("female" = "Femenino", "male" = "Masculino")
  )

print(plot4c)

cat("\nVENTAJAS DEL ZOOM:\n")
cat("✓ Mayor DETALLE en región específica\n")
cat("✓ Mejor visibilidad de DIFERENCIAS SUTILES\n")
cat("✓ Enfoque en POBLACIÓN DE INTERÉS\n\n")

cat("COMPARACIÓN: coord_cartesian() vs scale_x/y_continuous():\n")
cat("---------------------------------------------------------\n")
cat("coord_cartesian(xlim = c(a, b)):\n")
cat("  ✓ Hace ZOOM sin eliminar datos\n")
cat("  ✓ Los cálculos (líneas de tendencia) usan TODOS los datos\n")
cat("  ✓ Recomendado para EXPLORACIÓN\n\n")
cat("scale_x_continuous(limits = c(a, b)):\n")
cat("  ✓ ELIMINA datos fuera del rango\n")
cat("  ✓ Los cálculos usan SOLO datos visibles\n")
cat("  ✓ Puede generar ADVERTENCIAS\n\n")

cat("VENTAJAS GENERALES DE LIMITAR EJES:\n")
cat("----------------------------------\n")
cat("1. CONTEXTO: Muestra la escala real de medición\n")
cat("2. COMPARABILIDAD: Permite comparar múltiples gráficos de forma justa\n")
cat("3. EVITA ENGAÑOS: Previene exageración visual de diferencias pequeñas\n")
cat("4. ENFOQUE: Permite concentrarse en rangos de interés\n")
cat("5. ESTANDARIZACIÓN: Facilita la creación de dashboards consistentes\n\n")

cat("CUÁNDO USAR CADA ENFOQUE:\n")
cat("------------------------\n")
cat("✓ Ejes automáticos: Exploración inicial, patrones generales\n")
cat("✓ Rango completo (0-100): Presentaciones, contexto real\n")
cat("✓ Zoom específico: Análisis detallado de subgrupos\n\n")

# EJEMPLO 4: Comparación visual de escalas
cat("EJEMPLO 4: DEMOSTRACIÓN DEL IMPACTO DE LAS ESCALAS\n")
cat("--------------------------------------------------\n")

# Crear subset con diferencias pequeñas
subset_similar <- students %>%
  filter(reading_score >= 70 & reading_score <= 80)

plot4d1 <- ggplot(subset_similar, aes(x = reading_score, y = writing_score)) +
  geom_point(alpha = 0.5, size = 2, color = "darkgreen") +
  labs(
    title = "ESCALA AUTOMÁTICA: Parece gran variación",
    x = "Lectura", y = "Escritura"
  ) +
  theme_minimal()

plot4d2 <- ggplot(subset_similar, aes(x = reading_score, y = writing_score)) +
  geom_point(alpha = 0.5, size = 2, color = "darkgreen") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(
    title = "ESCALA FIJA (0-100): Variación real es pequeña",
    x = "Lectura", y = "Escritura"
  ) +
  theme_minimal()

print(plot4d1)
print(plot4d2)

cat("\nCONCLUSIÓN:\n")
cat("La MISMA DATA puede verse muy diferente según la escala.\n")
cat("Limitar ejes al rango real previene interpretaciones engañosas.\n\n")

cat("================================================================================\n")
cat("RESUMEN FINAL\n")
cat("================================================================================\n\n")

cat("1. FACETING: Divide gráficos en paneles para comparar grupos\n")
cat("2. TÍTULOS/ETIQUETAS: Hacen gráficos autoexplicativos y profesionales\n")
cat("3. TEXTO: Guía interpretación, destaca hallazgos, cuenta historias\n")
cat("4. LÍMITES DE EJES: Proveen contexto, evitan engaños, facilitan comparación\n\n")

cat("PRINCIPIO FUNDAMENTAL:\n")
cat("Un buen gráfico debe contar su historia SIN necesidad de explicación verbal.\n")