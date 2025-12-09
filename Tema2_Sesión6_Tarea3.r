# ========================================
# ANÁLISIS EXPLORATORIO - STUDENTS PERFORMANCE
# ========================================

library(ggplot2)
library(dplyr)

# Cargar datos
students <- read.csv("Carga de datos/StudentsPerformance.csv")
colnames(students) <- c("gender", "race_ethnicity", "parental_education", 
                        "lunch", "test_prep_course", "math_score", 
                        "reading_score", "writing_score")

cat("================================================================================\n")
cat("PREGUNTA 1: GRÁFICO DE BARRAS - VARIABLE LUNCH\n")
cat("================================================================================\n\n")

# Calcular frecuencias
tabla_lunch <- table(students$lunch)
df_lunch <- as.data.frame(tabla_lunch)
colnames(df_lunch) <- c("Tipo_Almuerzo", "Frecuencia")

# Calcular porcentajes
df_lunch$Porcentaje <- round((df_lunch$Frecuencia / sum(df_lunch$Frecuencia)) * 100, 1)

cat("TABLA DE FRECUENCIAS:\n")
cat("--------------------\n")
print(df_lunch)
cat("\n")

# Crear gráfico de barras
plot1 <- ggplot(df_lunch, aes(x = Tipo_Almuerzo, y = Frecuencia, fill = Tipo_Almuerzo)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Frecuencia, "\n(", Porcentaje, "%)")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  labs(
    title = "Distribución de Estudiantes por Tipo de Almuerzo",
    subtitle = "Análisis de 1000 estudiantes",
    x = "Tipo de Almuerzo",
    y = "Número de Estudiantes",
    fill = "Categoría"
  ) +
  scale_fill_manual(
    values = c("free/reduced" = "#E53935", "standard" = "#43A047"),
    labels = c("free/reduced" = "Gratuito/Reducido", "standard" = "Estándar")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 11),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(df_lunch$Frecuencia) * 1.15)

print(plot1)
ggsave("grafico_pregunta1.png", plot1, width = 10, height = 6, dpi = 300)

cat("\nRESPUESTA PREGUNTA 1:\n")
cat("--------------------\n")
cat("¿Qué tipo de almuerzo es más común?\n")
if (df_lunch$Frecuencia[df_lunch$Tipo_Almuerzo == "standard"] > 
    df_lunch$Frecuencia[df_lunch$Tipo_Almuerzo == "free/reduced"]) {
  cat("→ ALMUERZO ESTÁNDAR es más común\n")
} else {
  cat("→ ALMUERZO GRATUITO/REDUCIDO es más común\n")
}
cat("\n¿Cuántas instancias tiene cada opción?\n")
cat("→ Almuerzo Estándar:", df_lunch$Frecuencia[df_lunch$Tipo_Almuerzo == "standard"], 
    "estudiantes (", df_lunch$Porcentaje[df_lunch$Tipo_Almuerzo == "standard"], "%)\n")
cat("→ Almuerzo Gratuito/Reducido:", df_lunch$Frecuencia[df_lunch$Tipo_Almuerzo == "free/reduced"], 
    "estudiantes (", df_lunch$Porcentaje[df_lunch$Tipo_Almuerzo == "free/reduced"], "%)\n")
cat("\nINTERPRETACIÓN:\n")
cat("La mayoría de estudiantes (64.5%) recibe almuerzo estándar, lo que sugiere\n")
cat("que la muestra proviene mayoritariamente de familias con mejor situación socioeconómica.\n")
cat("El 35.5% restante recibe almuerzo gratuito/reducido, indicando necesidad económica.\n\n\n")


cat("================================================================================\n")
cat("PREGUNTA 2: HISTOGRAMA - DISTRIBUCIÓN DE WRITING_SCORE\n")
cat("================================================================================\n\n")

# Estadísticas descriptivas
media_writing <- mean(students$writing_score)
mediana_writing <- median(students$writing_score)
sd_writing <- sd(students$writing_score)

cat("ESTADÍSTICAS DESCRIPTIVAS - WRITING SCORE:\n")
cat("------------------------------------------\n")
cat("Media:", round(media_writing, 2), "\n")
cat("Mediana:", mediana_writing, "\n")
cat("Desviación Estándar:", round(sd_writing, 2), "\n")
cat("Mínimo:", min(students$writing_score), "\n")
cat("Máximo:", max(students$writing_score), "\n")
cat("Rango:", max(students$writing_score) - min(students$writing_score), "\n\n")

# Crear histograma
plot2 <- ggplot(students, aes(x = writing_score)) +
  geom_histogram(aes(y = after_stat(count)), 
                 bins = 20, 
                 fill = "#667eea", 
                 color = "white", 
                 alpha = 0.8) +
  geom_vline(aes(xintercept = media_writing), 
             color = "red", 
             linetype = "dashed", 
             linewidth = 1.2) +
  geom_vline(aes(xintercept = mediana_writing), 
             color = "blue", 
             linetype = "solid", 
             linewidth = 1.2) +
  annotate("text", x = media_writing + 8, y = max(table(cut(students$writing_score, 20))) * 0.9,
           label = paste("Media =", round(media_writing, 1)), 
           color = "red", fontface = "bold", size = 4) +
  annotate("text", x = mediana_writing - 8, y = max(table(cut(students$writing_score, 20))) * 0.85,
           label = paste("Mediana =", mediana_writing), 
           color = "blue", fontface = "bold", size = 4) +
  labs(
    title = "Distribución de Puntuaciones en Escritura",
    subtitle = "Histograma con media y mediana",
    x = "Puntuación en Escritura (0-100)",
    y = "Frecuencia (Número de Estudiantes)",
    caption = "Línea roja = Media | Línea azul = Mediana"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 10, color = "gray50"),
    axis.title = element_text(face = "bold", size = 12)
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10))

print(plot2)
ggsave("grafico_pregunta2.png", plot2, width = 10, height = 6, dpi = 300)

cat("\nRESPUESTA PREGUNTA 2:\n")
cat("--------------------\n")
cat("¿La distribución parece estar centrada o sesgada?\n\n")

sesgo <- (media_writing - mediana_writing) / sd_writing

if (abs(sesgo) < 0.1) {
  cat("→ DISTRIBUCIÓN APROXIMADAMENTE SIMÉTRICA (CENTRADA)\n")
} else if (sesgo > 0.1) {
  cat("→ DISTRIBUCIÓN CON SESGO POSITIVO (HACIA LA DERECHA)\n")
} else {
  cat("→ DISTRIBUCIÓN CON SESGO NEGATIVO (HACIA LA IZQUIERDA)\n")
}

cat("\nANÁLISIS DETALLADO:\n")
cat("------------------\n")
cat("Media (", round(media_writing, 2), ") vs Mediana (", mediana_writing, "):\n", sep="")
cat("Diferencia:", round(media_writing - mediana_writing, 2), "\n")
cat("Coeficiente de sesgo aproximado:", round(sesgo, 3), "\n\n")

cat("INTERPRETACIÓN:\n")
if (abs(media_writing - mediana_writing) < 2) {
  cat("La distribución es APROXIMADAMENTE SIMÉTRICA ya que la media y mediana\n")
  cat("son muy similares. Los datos se concentran alrededor del centro (~68-70).\n")
  cat("La forma es similar a una distribución normal.\n\n")
  cat("IMPLICACIONES:\n")
  cat("- La mayoría de estudiantes tienen rendimiento promedio\n")
  cat("- Hay pocos valores extremos (muy altos o muy bajos)\n")
  cat("- Los estudiantes están relativamente homogéneos en escritura\n")
} else if (media_writing > mediana_writing) {
  cat("La distribución tiene SESGO POSITIVO (cola a la derecha) ya que\n")
  cat("la media es mayor que la mediana. Hay algunos valores muy altos\n")
  cat("que jalan la media hacia arriba.\n")
} else {
  cat("La distribución tiene SESGO NEGATIVO (cola a la izquierda) ya que\n")
  cat("la media es menor que la mediana. Hay algunos valores muy bajos\n")
  cat("que jalan la media hacia abajo.\n")
}
cat("\n\n")


cat("================================================================================\n")
cat("PREGUNTA 3: GRÁFICO DE DENSIDAD - READING_SCORE POR TEST_PREPARATION_COURSE\n")
cat("================================================================================\n\n")

# Calcular estadísticas por grupo
stats_prep <- students %>%
  group_by(test_prep_course) %>%
  summarise(
    n = n(),
    media = round(mean(reading_score), 2),
    mediana = median(reading_score),
    sd = round(sd(reading_score), 2),
    .groups = "drop"
  )

cat("ESTADÍSTICAS POR GRUPO:\n")
cat("----------------------\n")
print(stats_prep)
cat("\n")

# Crear gráfico de densidad
plot3 <- ggplot(students, aes(x = reading_score, fill = test_prep_course)) +
  geom_density(alpha = 0.6, linewidth = 1) +
  geom_vline(data = stats_prep, aes(xintercept = media, color = test_prep_course),
             linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Distribución de Puntuaciones en Lectura",
    subtitle = "Comparación por Curso de Preparación (Gráfico de Densidad)",
    x = "Puntuación en Lectura (0-100)",
    y = "Densidad",
    fill = "Curso de Preparación",
    color = "Media del Grupo",
    caption = "Líneas verticales indican la media de cada grupo"
  ) +
  scale_fill_manual(
    values = c("completed" = "#4CAF50", "none" = "#FF5722"),
    labels = c("completed" = "Completado", "none" = "No realizó")
  ) +
  scale_color_manual(
    values = c("completed" = "#2E7D32", "none" = "#D84315"),
    labels = c("completed" = "Completado", "none" = "No realizó")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 10, color = "gray50"),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10))

print(plot3)
ggsave("grafico_pregunta3.png", plot3, width = 10, height = 6, dpi = 300)

cat("\nRESPUESTA PREGUNTA 3:\n")
cat("--------------------\n")
cat("¿Existe alguna diferencia entre las distribuciones?\n\n")

diferencia_medias <- stats_prep$media[stats_prep$test_prep_course == "completed"] - 
                     stats_prep$media[stats_prep$test_prep_course == "none"]

cat("→ SÍ, EXISTE DIFERENCIA NOTABLE ENTRE LAS DISTRIBUCIONES\n\n")

cat("ANÁLISIS COMPARATIVO:\n")
cat("--------------------\n")
cat("Estudiantes que COMPLETARON el curso:\n")
cat("  • Media:", stats_prep$media[stats_prep$test_prep_course == "completed"], "\n")
cat("  • Mediana:", stats_prep$mediana[stats_prep$test_prep_course == "completed"], "\n")
cat("  • Cantidad:", stats_prep$n[stats_prep$test_prep_course == "completed"], "estudiantes\n\n")

cat("Estudiantes que NO realizaron el curso:\n")
cat("  • Media:", stats_prep$media[stats_prep$test_prep_course == "none"], "\n")
cat("  • Mediana:", stats_prep$mediana[stats_prep$test_prep_course == "none"], "\n")
cat("  • Cantidad:", stats_prep$n[stats_prep$test_prep_course == "none"], "estudiantes\n\n")

cat("DIFERENCIA DE MEDIAS:", round(diferencia_medias, 2), "puntos\n\n")

cat("INTERPRETACIÓN DEL GRÁFICO DE DENSIDAD:\n")
cat("--------------------------------------\n")
cat("1. POSICIÓN: La curva VERDE (completaron curso) está desplazada hacia la DERECHA\n")
cat("   → Indica puntuaciones más altas en promedio\n\n")
cat("2. FORMA: Ambas distribuciones tienen forma similar (aproximadamente normales)\n")
cat("   → La diferencia es en la UBICACIÓN, no en la variabilidad\n\n")
cat("3. SOLAPAMIENTO: Hay considerable solapamiento entre ambas curvas\n")
cat("   → Algunos que no hicieron el curso rinden igual o mejor que quienes sí lo hicieron\n")
cat("   → El curso ayuda pero no garantiza alto rendimiento\n\n")
cat("4. PICOS: Ambas distribuciones tienen picos similares (altura)\n")
cat("   → La variabilidad es similar en ambos grupos\n\n")

cat("CONCLUSIÓN:\n")
cat("El curso de preparación tiene un EFECTO POSITIVO en las puntuaciones de lectura,\n")
cat("aumentando la media en aproximadamente", round(diferencia_medias, 1), "puntos.\n")
cat("Sin embargo, no es el único factor determinante del rendimiento.\n\n\n")


cat("================================================================================\n")
cat("PREGUNTA 4: BOXPLOT - READING_SCORE POR TEST_PREPARATION_COURSE\n")
cat("================================================================================\n\n")

# Calcular cuartiles
quartiles_prep <- students %>%
  group_by(test_prep_course) %>%
  summarise(
    Q1 = quantile(reading_score, 0.25),
    Q2 = median(reading_score),
    Q3 = quantile(reading_score, 0.75),
    IQR = IQR(reading_score),
    min = min(reading_score),
    max = max(reading_score),
    .groups = "drop"
  )

cat("ESTADÍSTICAS DE CUARTILES:\n")
cat("-------------------------\n")
print(quartiles_prep)
cat("\n")

# Crear boxplot
plot4 <- ggplot(students, aes(x = test_prep_course, y = reading_score, fill = test_prep_course)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 2) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 0.8, color = "gray30") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "yellow", color = "black") +
  labs(
    title = "Distribución de Puntuaciones en Lectura por Curso de Preparación",
    subtitle = "Boxplot con puntos individuales y media (rombo amarillo)",
    x = "Curso de Preparación",
    y = "Puntuación en Lectura (0-100)",
    fill = "Grupo",
    caption = "Puntos rojos = Valores atípicos | Rombo amarillo = Media | Línea en caja = Mediana"
  ) +
  scale_x_discrete(labels = c("completed" = "Completado", "none" = "No realizó")) +
  scale_fill_manual(
    values = c("completed" = "#4CAF50", "none" = "#FF5722"),
    labels = c("completed" = "Completado", "none" = "No realizó")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray50"),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "none",
    axis.text.x = element_text(size = 11, face = "bold")
  ) +
  coord_cartesian(ylim = c(0, 105))

print(plot4)
ggsave("grafico_pregunta4.png", plot4, width = 10, height = 6, dpi = 300)

cat("\nRESPUESTA PREGUNTA 4:\n")
cat("--------------------\n")
cat("¿En qué ayuda el boxplot a complementar el análisis anterior?\n\n")

cat("VENTAJAS DEL BOXPLOT vs GRÁFICO DE DENSIDAD:\n")
cat("--------------------------------------------\n\n")

cat("1. VISUALIZACIÓN DE CUARTILES:\n")
cat("   • El boxplot muestra EXPLÍCITAMENTE Q1, Q2 (mediana) y Q3\n")
cat("   • Permite ver dónde se concentra el 50% central de los datos (la caja)\n")
cat("   • Densidad solo muestra forma general\n\n")

cat("   Completed: Q1=", quartiles_prep$Q1[1], " | Q2=", quartiles_prep$Q2[1], 
    " | Q3=", quartiles_prep$Q3[1], "\n")
cat("   None: Q1=", quartiles_prep$Q1[2], " | Q2=", quartiles_prep$Q2[2], 
    " | Q3=", quartiles_prep$Q3[2], "\n\n")

cat("2. IDENTIFICACIÓN DE VALORES ATÍPICOS (OUTLIERS):\n")
cat("   • Boxplot marca CLARAMENTE outliers como puntos individuales (en rojo)\n")
cat("   • Gráfico de densidad los 'oculta' en las colas de la distribución\n")
cat("   • Facilita identificar estudiantes con rendimiento excepcional o muy bajo\n\n")

cat("3. RANGO INTERCUARTÍLICO (IQR):\n")
cat("   • La ALTURA de la caja = IQR = Q3 - Q1\n")
cat("   • Muestra la DISPERSIÓN del 50% central de los datos\n")
cat("   • Completed IQR:", quartiles_prep$IQR[1], 
    " | None IQR:", quartiles_prep$IQR[2], "\n")
cat("   → Ambos grupos tienen dispersión similar\n\n")

cat("4. COMPARACIÓN MÁS DIRECTA:\n")
cat("   • Los boxplots están uno al lado del otro\n")
cat("   • Más fácil comparar medianas, rangos y dispersión\n")
cat("   • No hay solapamiento visual (como en densidad)\n\n")

cat("5. VALORES EXTREMOS:\n")
cat("   • Los 'bigotes' muestran el rango de datos (excluyendo outliers)\n")
cat("   • Completed: Mín=", quartiles_prep$min[1], " | Máx=", quartiles_prep$max[1], "\n")
cat("   • None: Mín=", quartiles_prep$min[2], " | Máx=", quartiles_prep$max[2], "\n\n")

cat("6. SIMETRÍA/SESGO:\n")
cat("   • Si mediana está centrada en la caja → distribución simétrica\n")
cat("   • Si mediana está cerca de Q1 o Q3 → distribución sesgada\n")
cat("   • Fácil de ver visualmente en boxplot\n\n")

cat("COMPLEMENTARIEDAD:\n")
cat("-----------------\n")
cat("Densidad: Muestra la FORMA completa de la distribución\n")
cat("Boxplot: Muestra ESTADÍSTICAS específicas y outliers\n\n")
cat("MEJOR PRÁCTICA: Usar AMBOS juntos para análisis completo\n")
cat("  → Densidad para entender la forma\n")
cat("  → Boxplot para cuantificar diferencias y detectar outliers\n\n\n")


cat("================================================================================\n")
cat("PREGUNTA 5: GRÁFICO DE SECTORES (PIE CHART) - TEST_PREPARATION_COURSE\n")
cat("================================================================================\n\n")

# Calcular frecuencias
tabla_prep <- table(students$test_prep_course)
df_prep <- as.data.frame(tabla_prep)
colnames(df_prep) <- c("Curso", "Frecuencia")
df_prep$Porcentaje <- round((df_prep$Frecuencia / sum(df_prep$Frecuencia)) * 100, 1)
df_prep$Etiqueta <- paste0(df_prep$Curso, "\n", df_prep$Frecuencia, " estudiantes\n(", 
                            df_prep$Porcentaje, "%)")

cat("TABLA DE FRECUENCIAS:\n")
cat("--------------------\n")
print(df_prep)
cat("\n")

# Crear gráfico de sectores (pie chart)
plot5 <- ggplot(df_prep, aes(x = "", y = Frecuencia, fill = Curso)) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 2) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            position = position_stack(vjust = 0.5),
            size = 8, fontface = "bold", color = "white") +
  labs(
    title = "Proporción de Estudiantes según Curso de Preparación",
    subtitle = "Distribución de 1000 estudiantes",
    fill = "Estado del Curso"
  ) +
  scale_fill_manual(
    values = c("completed" = "#4CAF50", "none" = "#FF5722"),
    labels = c("completed" = paste0("Completado\n(", 
                                     df_prep$Frecuencia[df_prep$Curso == "completed"], 
                                     " estudiantes)"),
               "none" = paste0("No realizó\n(", 
                              df_prep$Frecuencia[df_prep$Curso == "none"], 
                              " estudiantes)"))
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray30", margin = margin(b = 20)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11)
  )

print(plot5)
ggsave("grafico_pregunta5_pie.png", plot5, width = 8, height = 8, dpi = 300)

cat("\nRESPUESTA PREGUNTA 5:\n")
cat("--------------------\n")
cat("¿Cuál tiene mayor proporción?\n\n")

if (df_prep$Frecuencia[df_prep$Curso == "none"] > 
    df_prep$Frecuencia[df_prep$Curso == "completed"]) {
  cat("→ MAYOR PROPORCIÓN: NO REALIZÓ EL CURSO\n\n")
  cat("La MAYORÍA de estudiantes (", df_prep$Porcentaje[df_prep$Curso == "none"], 
      "%) NO realizó el curso de preparación.\n")
  cat("Solo el ", df_prep$Porcentaje[df_prep$Curso == "completed"], 
      "% completó el curso.\n\n")
} else {
  cat("→ MAYOR PROPORCIÓN: COMPLETÓ EL CURSO\n\n")
  cat("La MAYORÍA de estudiantes (", df_prep$Porcentaje[df_prep$Curso == "completed"], 
      "%) SÍ completó el curso de preparación.\n")
  cat("Solo el ", df_prep$Porcentaje[df_prep$Curso == "none"], 
      "% no lo realizó.\n\n")
}

cat("DISTRIBUCIÓN EXACTA:\n")
cat("-------------------\n")
cat("Completaron el curso:", df_prep$Frecuencia[df_prep$Curso == "completed"], 
    "estudiantes (", df_prep$Porcentaje[df_prep$Curso == "completed"], "%)\n")
cat("No realizaron el curso:", df_prep$Frecuencia[df_prep$Curso == "none"], 
    "estudiantes (", df_prep$Porcentaje[df_prep$Curso == "none"], "%)\n\n")

cat("INTERPRETACIÓN:\n")
cat("--------------\n")
diferencia_prop <- abs(df_prep$Porcentaje[1] - df_prep$Porcentaje[2])
cat("Diferencia:", round(diferencia_prop, 1), "puntos porcentuales\n\n")

if (df_prep$Porcentaje[df_prep$Curso == "none"] > 60) {
  cat("IMPLICACIONES:\n")
  cat("• La mayoría de estudiantes NO tiene acceso o no participa en cursos de preparación\n")
  cat("• Esto puede indicar:\n")
  cat("  - Falta de recursos económicos para pagar cursos adicionales\n")
  cat("  - Falta de información sobre la disponibilidad de estos cursos\n")
  cat("  - Los cursos no son parte del programa escolar estándar\n")
  cat("• Dado que vimos que el curso MEJORA el rendimiento (Pregunta 3),\n")
  cat("  sería beneficioso ampliar el acceso a estos programas\n")
} else {
  cat("IMPLICACIONES:\n")
  cat("• La mayoría de estudiantes SÍ tiene acceso a cursos de preparación\n")
  cat("• Esto sugiere una escuela con buenos recursos o programas de apoyo\n")
}

cat("\nNOTA SOBRE GRÁFICOS DE SECTORES:\n")
cat("--------------------------------\n")
cat("VENTAJAS:\n")
cat("• Muestra claramente PROPORCIONES del total\n")
cat("• Intuitivo para audiencias no técnicas\n")
cat("• Ideal para presentaciones ejecutivas\n\n")
cat("LIMITACIONES:\n")
cat("• Difícil comparar sectores similares en tamaño\n")
cat("• No recomendado para más de 5-6 categorías\n")
cat("• Gráfico de barras suele ser más preciso\n\n")

cat("ALTERNATIVA: Gráfico de barras para misma información\n")

# Crear versión alternativa con barras
plot5_alt <- ggplot(df_prep, aes(x = Curso, y = Porcentaje, fill = Curso)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Porcentaje, "%\n(", Frecuencia, " estudiantes)")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  labs(
    title = "Proporción de Estudiantes (Versión Barras)",
    x = "Curso de Preparación",
    y = "Porcentaje (%)"
  ) +
  scale_x_discrete(labels = c("completed" = "Completado", "none" = "No realizó")) +
  scale_fill_manual(values = c("completed" = "#4CAF50", "none" = "#FF5722")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none"
  ) +
  ylim(0, 100)

print(plot5_alt)
ggsave("grafico_pregunta5_barras.png", plot5_alt, width = 10, height = 6, dpi = 300)