# ========================================
# PARTE 1: CREACIÓN DE LA MATRIZ
# ========================================

# 1. Definir la matriz con nombres de filas y columnas
calificaciones <- matrix(
  c(85, 90, 78, 92, 88,    # Matemáticas
    80, 85, 82, 88, 86,    # Ciencias
    78, 83, 85, 80, 90),   # Lenguaje
  nrow = 5,
  ncol = 3,
  byrow = FALSE,
  dimnames = list(
    c("Estudiante1", "Estudiante2", "Estudiante3", "Estudiante4", "Estudiante5"),
    c("Matemáticas", "Ciencias", "Lenguaje")
  )
)

cat("MATRIZ DE CALIFICACIONES:\n")
print(calificaciones)
cat("\n")

# 2. Calificaciones del segundo estudiante
cat("Calificaciones del Estudiante 2:\n")
print(calificaciones[2, ])
cat("\n")

# 3. Notas de ciencias de todos los estudiantes
cat("Notas de Ciencias:\n")
print(calificaciones[, "Ciencias"])
cat("\n")

# 4. Promedio de calificaciones en lenguaje
cat("Promedio en Lenguaje:\n")
print(mean(calificaciones[, "Lenguaje"]))
cat("\n\n")

# ========================================
# PARTE 2: CREACIÓN DEL DATAFRAME
# ========================================

# 1. Definir el dataframe
desempeño <- data.frame( # nolint
  Matemáticas = c(85, 90, 78, 92, 88),
  Ciencias = c(80, 85, 82, 88, 86),
  Lenguaje = c(78, 83, 85, 80, 90),
  Edad = c(15, 16, 15, 14, 16),
  Género = c("F", "M", "F", "M", "F"),
  row.names = c("Estudiante1", "Estudiante2", "Estudiante3", "Estudiante4", "Estudiante5")
)

cat("DATAFRAME DE DESEMPEÑO:\n")
print(desempeño)
cat("\n")

# 2. Estudiantes con nota en Matemáticas superior a 85
cat("Estudiantes con Matemáticas > 85:\n")
print(desempeño[desempeño$Matemáticas > 85, ])
cat("\n")

# 3. Filtrar estudiantes mujeres
cat("Estudiantes Mujeres:\n")
print(desempeño[desempeño$Género == "F", ])
cat("\n\n")

# ========================================
# PARTE 3: CREACIÓN DE LA LISTA
# ========================================

# 1. Definir el vector de materias y la lista
materias <- c("Matemáticas", "Ciencias", "Lenguaje")

informe <- list(
  calificaciones = calificaciones,
  desempeño = desempeño,
  materias = materias
)

cat("LISTA INFORME CREADA CON 3 ELEMENTOS\n\n")

# 2. Promedio general de todas las calificaciones
cat("Promedio General de Todas las Calificaciones:\n")
promedio_general <- mean(as.matrix(informe$desempeño[, 1:3]))
print(promedio_general)
cat("\n")

# 3. Último elemento del vector de materias
cat("Último Elemento del Vector Materias:\n")
print(informe$materias[length(informe$materias)])
cat("\n\n")

# ========================================
# ANÁLISIS ADICIONAL
# ========================================

cat("RESUMEN ESTADÍSTICO POR MATERIA:\n")
cat("Matemáticas - Promedio:", mean(desempeño$Matemáticas), "\n")
cat("Ciencias - Promedio:", mean(desempeño$Ciencias), "\n")
cat("Lenguaje - Promedio:", mean(desempeño$Lenguaje), "\n")