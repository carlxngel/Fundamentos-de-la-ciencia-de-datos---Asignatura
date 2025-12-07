# ========================================
# ANÁLISIS DE RENDIMIENTO ACADÉMICO
# ========================================

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Cargar el dataset
students <- read.csv("Carga de datos/StudentsPerformance.csv")

# Limpiar nombres de columnas (eliminar espacios y caracteres especiales)
colnames(students) <- c("gender", "race_ethnicity", "parental_education", 
                        "lunch", "test_prep_course", "math_score", 
                        "reading_score", "writing_score")

cat("========================================\n")
cat("DATASET CARGADO\n")
cat("========================================\n")
cat("Dimensiones:", nrow(students), "filas x", ncol(students), "columnas\n\n")

# Mostrar primeras filas
cat("Primeras filas del dataset:\n")
print(head(students))
cat("\n")

# Resumen estadístico de las variables de interés
cat("========================================\n")
cat("RESUMEN ESTADÍSTICO\n")
cat("========================================\n")
cat("\nPuntuaciones de Escritura (writing_score):\n")
print(summary(students$writing_score))

cat("\nPuntuaciones de Lectura (reading_score):\n")
print(summary(students$reading_score))

cat("\nDistribución por Género:\n")
print(table(students$gender))

cat("\nDistribución por Tipo de Almuerzo:\n")
print(table(students$lunch))
cat("\n\n")

# ========================================
# PREGUNTA 1 y 2: GRÁFICO DE DISPERSIÓN BÁSICO
# ========================================

cat("========================================\n")
cat("PREGUNTA 1 y 2: GRÁFICO DE DISPERSIÓN\n")
cat("========================================\n\n")

# Gráfico básico de dispersión
plot1 <- ggplot(students, aes(x = reading_score, y = writing_score)) +
  geom_point(alpha = 0.5, size = 2, color = "steelblue") +
  labs(
    title = "Relación entre Puntuaciones de Lectura y Escritura",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura",
    subtitle = "Datos de 1000 estudiantes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

print(plot1)

cat("RESPUESTA PREGUNTA 1:\n")
cat("- Variable X (independiente): reading_score (Puntuación en Lectura)\n")
cat("- Variable Y (dependiente): writing_score (Puntuación en Escritura)\n")
cat("- Tipo de gráfico: Gráfico de Dispersión (Scatter Plot)\n\n")

cat("RESPUESTA PREGUNTA 2:\n")
cat("SÍ, existe una relación POSITIVA Y FUERTE entre las puntuaciones.\n")
cat("Los puntos siguen una tendencia lineal ascendente, lo que indica que\n")
cat("los estudiantes que obtienen buenas calificaciones en lectura tienden\n")
cat("a obtener buenas calificaciones en escritura también.\n\n\n")

# ========================================
# PREGUNTA 3: TRANSPARENCIA Y TAMAÑO
# ========================================

cat("========================================\n")
cat("PREGUNTA 3: IMPORTANCIA DE TRANSPARENCIA\n")
cat("========================================\n\n")

cat("RESPUESTA PREGUNTA 3:\n")
cat("La transparencia (alpha) y el tamaño de puntos son útiles porque:\n\n")
cat("1. TRANSPARENCIA (alpha = 0.5):\n")
cat("   - Permite visualizar la DENSIDAD de datos\n")
cat("   - Muestra dónde se concentran más estudiantes\n")
cat("   - Evita el 'overplotting' (superposición de puntos)\n")
cat("   - Áreas más oscuras = mayor concentración de estudiantes\n\n")
cat("2. TAMAÑO DE PUNTOS:\n")
cat("   - Puntos más pequeños permiten ver mejor patrones en datasets grandes\n")
cat("   - Reduce la superposición visual\n")
cat("   - Facilita identificar la distribución general\n\n\n")

# ========================================
# PREGUNTA 4: ANÁLISIS POR GÉNERO Y ALMUERZO
# ========================================

cat("========================================\n")
cat("PREGUNTA 4: ANÁLISIS MULTIVARIABLE\n")
cat("========================================\n\n")

# Gráfico con facetas por género y color por almuerzo
plot4 <- ggplot(
  students,
  aes(x = reading_score, y = writing_score, color = lunch)
) +
  geom_point(alpha = 0.6, size = 2) +
  facet_wrap(~ gender) +
  labs(
    title = "Rendimiento Académico por Género y Tipo de Almuerzo",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura",
    color = "Tipo de Almuerzo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold", size = 11)
  ) +
  scale_color_manual(
    values = c("standard" = "#2E7D32", "free/reduced" = "#C62828")
  )

print(plot4)
ggsave("grafico_pregunta4.png", plot4, width = 10, height = 6, dpi = 300)

# Calcular estadísticas por grupo
estadisticas_grupos <- students %>%
  group_by(gender, lunch) %>%
  summarise(
    n = n(),
    promedio_lectura = mean(reading_score),
    promedio_escritura = mean(writing_score),
    .groups = "drop"
  )

cat("\nEstadísticas por Grupo:\n")
print(estadisticas_grupos)

cat("\nRESPUESTA PREGUNTA 4:\n")
cat("CONCLUSIONES sobre rendimiento académico:\n\n")
cat("1. TIPO DE ALMUERZO (Factor socioeconómico):\n")
cat("   - Estudiantes con almuerzo 'standard' (verde) tienen puntuaciones\n")
cat("     consistentemente MÁS ALTAS que aquellos con 'free/reduced' (rojo)\n")
cat("   - Esto sugiere que el nivel socioeconómico afecta el rendimiento\n\n")
cat("2. GÉNERO:\n")
cat("   - Las mujeres (female) tienden a tener puntuaciones ligeramente\n")
cat("     más altas en lectura y escritura comparadas con los hombres\n\n")
cat("3. RELACIÓN GENERAL:\n")
cat("   - La correlación positiva entre lectura y escritura se mantiene\n")
cat("     INDEPENDIENTEMENTE del género o tipo de almuerzo\n")
cat("   - Esto indica que las habilidades de lectura y escritura están\n")
cat("     fuertemente relacionadas en todos los grupos\n\n\n")

# ========================================
# PREGUNTA 5: LÍNEA DE TENDENCIA
# ========================================

cat("========================================\n")
cat("PREGUNTA 5: LÍNEA DE TENDENCIA\n")
cat("========================================\n\n")

# Gráfico con línea de tendencia
plot5 <- ggplot(students, aes(x = reading_score, y = writing_score)) +
  geom_point(alpha = 0.4, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.2) +
  labs(
    title = "Relación entre Lectura y Escritura con Línea de Tendencia",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura",
    subtitle = "Línea de regresión lineal con intervalo de confianza del 95%"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13)
  )

print(plot5)
ggsave("grafico_pregunta5.png", plot5, width = 10, height = 6, dpi = 300)

# Calcular correlación y modelo de regresión
correlacion <- cor(students$reading_score, students$writing_score)
modelo <- lm(writing_score ~ reading_score, data = students)

cat("\nAnálisis Estadístico:\n")
cat("Correlación de Pearson:", round(correlacion, 4), "\n")
cat("\nModelo de Regresión Lineal:\n")
print(summary(modelo))

cat("\nRESPUESTA PREGUNTA 5:\n")
cat("La línea de tendencia (regresión lineal) aporta:\n\n")
cat("1. DIRECCIÓN: Confirma una relación POSITIVA (pendiente ascendente)\n")
cat("2. FUERZA: La correlación de", round(correlacion, 3), "indica una relación\n")
cat("   MUY FUERTE entre ambas variables\n")
cat("3. PREDICCIÓN: Por cada punto adicional en lectura, se espera un\n")
cat("   aumento de aproximadamente", round(coef(modelo)[2], 3), "puntos en escritura\n")
cat("4. AJUSTE: El R² de", round(summary(modelo)$r.squared, 3), "significa que el",
    round(summary(modelo)$r.squared * 100, 1), "%\n")
cat("   de la variabilidad en escritura se explica por la puntuación en lectura\n")
cat("5. BANDA DE CONFIANZA: El área sombreada muestra la incertidumbre\n")
cat("   de la predicción (intervalo de confianza del 95%)\n\n\n")

# ========================================
# PREGUNTA 6: GRÁFICO COLOREADO POR GÉNERO
# ========================================

cat("========================================\n")
cat("PREGUNTA 6: ANÁLISIS POR GÉNERO\n")
cat("========================================\n\n")

# Gráfico coloreado por género con líneas de tendencia separadas
plot6 <- ggplot(students, aes(x = reading_score, y = writing_score, color = gender)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  labs(
    title = "Relación Lectura-Escritura por Género",
    x = "Puntuación en Lectura",
    y = "Puntuación en Escritura",
    color = "Género",
    subtitle = "Con líneas de tendencia individuales por género"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "top"
  ) +
  scale_color_manual(
    values = c("female" = "#E91E63", "male" = "#2196F3"),
    labels = c("female" = "Femenino", "male" = "Masculino")
  )

print(plot6)
ggsave("grafico_pregunta6.png", plot6, width = 10, height = 6, dpi = 300)

# Análisis por género
analisis_genero <- students %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    promedio_lectura = mean(reading_score),
    promedio_escritura = mean(writing_score),
    correlacion = cor(reading_score, writing_score)
  )

cat("\nEstadísticas por Género:\n")
print(analisis_genero)

# Modelos separados por género
modelo_female <- lm(writing_score ~ reading_score, 
                    data = filter(students, gender == "female"))
modelo_male <- lm(writing_score ~ reading_score, 
                  data = filter(students, gender == "male"))

cat("\nCoeficientes de Regresión:\n")
cat("Mujeres - Pendiente:", round(coef(modelo_female)[2], 3), 
    "| R²:", round(summary(modelo_female)$r.squared, 3), "\n")
cat("Hombres - Pendiente:", round(coef(modelo_male)[2], 3), 
    "| R²:", round(summary(modelo_male)$r.squared, 3), "\n")

cat("\nRESPUESTA PREGUNTA 6:\n")
cat("DIFERENCIAS Y PATRONES POR GÉNERO:\n\n")
cat("1. POSICIÓN VERTICAL:\n")
cat("   - Los puntos ROSAS (mujeres) están ligeramente más arriba\n")
cat("   - Las mujeres tienen puntuaciones promedio más altas en escritura\n")
cat("   - Promedio escritura mujeres:", round(analisis_genero$promedio_escritura[1], 1), "\n")
cat("   - Promedio escritura hombres:", round(analisis_genero$promedio_escritura[2], 1), "\n\n")
cat("2. RELACIÓN LECTURA-ESCRITURA:\n")
cat("   - AMBOS géneros muestran una correlación muy fuerte y similar\n")
cat("   - Las líneas de tendencia son prácticamente PARALELAS\n")
cat("   - Esto indica que la relación entre lectura y escritura es\n")
cat("     CONSISTENTE independientemente del género\n\n")
cat("3. DISPERSIÓN:\n")
cat("   - Ambos grupos muestran dispersión similar alrededor de su línea\n")
cat("   - No hay diferencias significativas en la variabilidad\n\n")
cat("4. CONCLUSIÓN PRINCIPAL:\n")
cat("   - Aunque las MUJERES tienen puntuaciones promedio ligeramente\n")
cat("     más altas, la RELACIÓN entre lectura y escritura es igualmente\n")
cat("     fuerte en ambos géneros\n")
cat("   - El género NO modifica la naturaleza de la relación entre las\n")
cat("     dos habilidades académicas\n\n")

cat("========================================\n")
cat("ANÁLISIS COMPLETADO\n")
cat("========================================\n")
