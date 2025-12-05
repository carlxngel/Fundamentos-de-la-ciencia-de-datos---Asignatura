# ========================================
# SISTEMA DE ANÁLISIS DE NOTAS
# ========================================

# Datos iniciales
estudiantes <- list(
  nombres = c("Ana", "Carlos", "María", "Luis", "Elena"),
  asignaturas = c("Matemáticas", "Ciencias", "Historia", "Arte", "Literatura"),
  calificaciones = c(85, 92, 78, 60, 89)
)

cat("===========================================\n")
cat("DATOS DE ESTUDIANTES\n")
cat("===========================================\n")
print(estudiantes)
cat("\n\n")

# ========================================
# a) CLASIFICACIÓN APROBADO/REPROBADO
# ========================================

cat("===========================================\n")
cat("a) CLASIFICACIÓN APROBADO/REPROBADO\n")
cat("===========================================\n")

nota_minima <- 70

# Clasificar cada estudiante
estado <- ifelse(estudiantes$calificaciones >= nota_minima, "APROBADO", "REPROBADO")

# Mostrar resultado
resultado_clasificacion <- data.frame(
  Nombre = estudiantes$nombres,
  Asignatura = estudiantes$asignaturas,
  Calificación = estudiantes$calificaciones,
  Estado = estado
)

print(resultado_clasificacion)
cat("\n\n")

# ========================================
# b) MENSAJE PERSONALIZADO PARA CADA ESTUDIANTE
# ========================================

cat("===========================================\n")
cat("b) MENSAJES PERSONALIZADOS\n")
cat("===========================================\n")

for (i in 1:length(estudiantes$nombres)) {
  estado_estudiante <- ifelse(estudiantes$calificaciones[i] >= nota_minima, 
                               "APROBÓ", 
                               "REPROBÓ")
  
  mensaje <- paste("Estudiante:", estudiantes$nombres[i], 
                   "| Asignatura:", estudiantes$asignaturas[i],
                   "| Calificación:", estudiantes$calificaciones[i],
                   "| Estado:", estado_estudiante)
  
  cat(mensaje, "\n")
}
cat("\n\n")

# ========================================
# c) REVISIÓN CON WHILE (break y next)
# ========================================

cat("===========================================\n")
cat("c) REVISIÓN DE NOTAS CON WHILE\n")
cat("===========================================\n")

i <- 1
while (i <= length(estudiantes$calificaciones)) {
  nota_actual <- estudiantes$calificaciones[i]
  nombre_actual <- estudiantes$nombres[i]
  
  # Si la nota es mayor o igual a 90, felicitar y terminar
  if (nota_actual >= 90) {
    cat("¡FELICITACIONES", nombre_actual, "! Obtuviste una nota excelente:", nota_actual, "\n")
    cat("Proceso terminado.\n")
    break
  }
  
  # Si la nota es menor a 50, indicar que es muy baja y continuar
  if (nota_actual < 50) {
    cat("Nota muy baja para", nombre_actual, ":", nota_actual, "\n")
    i <- i + 1
    next
  }
  
  # En caso contrario, imprimir la nota actual
  cat("Nota de", nombre_actual, ":", nota_actual, "\n")
  
  i <- i + 1
}
cat("\n\n")

# ========================================
# d) FUNCIONES APPLY, LAPPLY Y SAPPLY
# ========================================

cat("===========================================\n")
cat("d) ANÁLISIS CON FUNCIONES APPLY\n")
cat("===========================================\n")

# APPLY: Calcular el promedio de las calificaciones
# Nota: apply se usa típicamente con matrices, pero podemos convertir el vector
matriz_calificaciones <- matrix(estudiantes$calificaciones, nrow = 1)
promedio <- apply(matriz_calificaciones, 1, mean)

cat("APPLY - Promedio de calificaciones:\n")
cat("Promedio:", promedio, "\n\n")

# LAPPLY: Convertir calificaciones a escala de 0 a 10
# Fórmula: (calificación / 100) * 10
calificaciones_escala_10 <- lapply(estudiantes$calificaciones, function(x) {
  return((x / 100) * 10)
})

cat("LAPPLY - Calificaciones en escala de 0 a 10:\n")
for (i in 1:length(estudiantes$nombres)) {
  cat(estudiantes$nombres[i], ":", 
      estudiantes$calificaciones[i], "sobre 100 =", 
      calificaciones_escala_10[[i]], "sobre 10\n")
}
cat("\n")

# SAPPLY: Evaluar si cada estudiante está por encima del promedio
encima_promedio <- sapply(estudiantes$calificaciones, function(x) {
  return(x > promedio)
})

cat("SAPPLY - Estudiantes por encima del promedio (", promedio, "):\n", sep = "")
for (i in 1:length(estudiantes$nombres)) {
  estado_promedio <- ifelse(encima_promedio[i], 
                             "SÍ está por encima", 
                             "NO está por encima")
  cat(estudiantes$nombres[i], "con", estudiantes$calificaciones[i], 
      "puntos:", estado_promedio, "del promedio\n")
}
cat("\n\n")

# ========================================
# RESUMEN FINAL
# ========================================

cat("===========================================\n")
cat("RESUMEN FINAL\n")
cat("===========================================\n")
cat("Total de estudiantes:", length(estudiantes$nombres), "\n")
cat("Promedio general:", promedio, "\n")
cat("Aprobados:", sum(estudiantes$calificaciones >= nota_minima), "\n")
cat("Reprobados:", sum(estudiantes$calificaciones < nota_minima), "\n")
cat("Nota más alta:", max(estudiantes$calificaciones), 
    "(", estudiantes$nombres[which.max(estudiantes$calificaciones)], ")\n")
cat("Nota más baja:", min(estudiantes$calificaciones), 
    "(", estudiantes$nombres[which.min(estudiantes$calificaciones)], ")\n")