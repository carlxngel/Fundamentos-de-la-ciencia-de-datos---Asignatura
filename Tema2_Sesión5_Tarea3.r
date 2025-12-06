# ========================================
# ANÁLISIS DE VENTAS POR CATEGORÍA
# ========================================

# Cargar librería necesaria
library(dplyr)

# Crear el dataset
SalesData <- data.frame(
  Product = c("Laptop", "Smartphone", "Chair", "Desk", "TV"),
  Category = c("Electronics", "Electronics", "Furniture", "Furniture", "Electronics"),
  Sales = c(1000, 800, 150, 300, 600),
  Units_Sold = c(5, 8, 3, 2, 4),
  Discount = c(0.10, 0.05, 0.15, 0.00, 0.20)
)

cat("========================================\n")
cat("DATASET ORIGINAL\n")
cat("========================================\n")
print(SalesData)
cat("\n\n")

# ========================================
# CÁLCULO DE INGRESOS REALES
# ========================================

cat("========================================\n")
cat("PASO 1: CALCULAR INGRESO REAL\n")
cat("========================================\n")
cat("Fórmula: Ingreso_Real = Sales * (1 - Discount)\n\n")

# Agregar columna de ingreso real
SalesData_con_ingreso <- SalesData %>%
  mutate(Ingreso_Real = Sales * (1 - Discount))

print(SalesData_con_ingreso)
cat("\n\n")

# ========================================
# ANÁLISIS POR CATEGORÍA
# ========================================

cat("========================================\n")
cat("ANÁLISIS AGRUPADO POR CATEGORÍA\n")
cat("========================================\n\n")

# Sentencia completa con pipes
analisis_por_categoria <- SalesData %>%
  mutate(Ingreso_Real = Sales * (1 - Discount)) %>%
  group_by(Category) %>%
  summarise(
    Total_Unidades_Vendidas = sum(Units_Sold),
    Promedio_Ventas = mean(Sales),
    Total_Ingresos_Reales = sum(Ingreso_Real)
  )

cat("SENTENCIA COMPLETA:\n")
cat("analisis_por_categoria <- SalesData %>%\n")
cat("  mutate(Ingreso_Real = Sales * (1 - Discount)) %>%\n")
cat("  group_by(Category) %>%\n")
cat("  summarise(\n")
cat("    Total_Unidades_Vendidas = sum(Units_Sold),\n")
cat("    Promedio_Ventas = mean(Sales),\n")
cat("    Total_Ingresos_Reales = sum(Ingreso_Real)\n")
cat("  )\n\n")

cat("RESULTADO:\n")
print(analisis_por_categoria)
cat("\n\n")

# ========================================
# RESULTADO FORMATEADO
# ========================================

cat("========================================\n")
cat("RESULTADO FORMATEADO\n")
cat("========================================\n\n")

for (i in 1:nrow(analisis_por_categoria)) {
  cat("Categoría:", analisis_por_categoria$Category[i], "\n")
  cat("  - Total Unidades Vendidas:", analisis_por_categoria$Total_Unidades_Vendidas[i], "\n")
  cat("  - Promedio de Ventas: $", round(analisis_por_categoria$Promedio_Ventas[i], 2), "\n", sep="")
  cat("  - Total Ingresos Reales: $", round(analisis_por_categoria$Total_Ingresos_Reales[i], 2), "\n\n", sep="")
}

# ========================================
# DESGLOSE DETALLADO POR PRODUCTO
# ========================================

cat("========================================\n")
cat("DESGLOSE DETALLADO POR PRODUCTO\n")
cat("========================================\n\n")

detalle_productos <- SalesData %>%
  mutate(
    Ingreso_Real = Sales * (1 - Discount),
    Descuento_Aplicado = Sales * Discount
  ) %>%
  select(Product, Category, Sales, Discount, Descuento_Aplicado, Ingreso_Real, Units_Sold)

print(detalle_productos)
cat("\n\n")

# ========================================
# ANÁLISIS COMPARATIVO
# ========================================

cat("========================================\n")
cat("ANÁLISIS COMPARATIVO\n")
cat("========================================\n\n")

# Categoría con mayor ingreso real
categoria_mayor_ingreso <- analisis_por_categoria %>%
  filter(Total_Ingresos_Reales == max(Total_Ingresos_Reales))

cat("Categoría con mayor ingreso real:", categoria_mayor_ingreso$Category, "\n")
cat("Ingreso real: $", round(categoria_mayor_ingreso$Total_Ingresos_Reales, 2), "\n\n", sep="")

# Categoría con mayor promedio de ventas
categoria_mayor_promedio <- analisis_por_categoria %>%
  filter(Promedio_Ventas == max(Promedio_Ventas))

cat("Categoría con mayor promedio de ventas:", categoria_mayor_promedio$Category, "\n")
cat("Promedio: $", round(categoria_mayor_promedio$Promedio_Ventas, 2), "\n\n", sep="")

# Totales generales
cat("TOTALES GENERALES:\n")
cat("- Total unidades vendidas (todas las categorías):", sum(analisis_por_categoria$Total_Unidades_Vendidas), "\n")
cat("- Total ingresos reales (todas las categorías): $", round(sum(analisis_por_categoria$Total_Ingresos_Reales), 2), "\n", sep="")
cat("- Descuento promedio aplicado:", round(mean(SalesData$Discount) * 100, 2), "%\n")

# ========================================
# VISUALIZACIÓN CON TABLA MEJORADA
# ========================================

cat("\n========================================\n")
cat("TABLA RESUMEN FINAL\n")
cat("========================================\n\n")

tabla_final <- analisis_por_categoria %>%
  mutate(
    Promedio_Ventas = round(Promedio_Ventas, 2),
    Total_Ingresos_Reales = round(Total_Ingresos_Reales, 2)
  )

print(tabla_final)