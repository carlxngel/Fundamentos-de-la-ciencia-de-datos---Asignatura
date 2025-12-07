# ============================================================
# ANÁLISIS DE VENTAS MENSUALES 2024 EN R
# ============================================================

# 1. DECLARAR EL VECTOR Ventas_2024
# ============================================================
Ventas_2024 <- c(12.5, 15.3, 14.8, 13.2, 16.4, 18.0, 20.1, 19.5, 17.3, 15.0, 14.2, 13.7)

cat("1. Vector Ventas_2024 declarado:\n")
print(Ventas_2024)
cat("\n")


# 2. TOTAL DE VENTAS EN EL AÑO
# ============================================================
total_ventas <- sum(Ventas_2024)

cat("2. Total de ventas en el año:\n")
cat("   Sentencia: total_ventas <- sum(Ventas_2024)\n")
cat("   Resultado:", total_ventas, "miles de euros\n\n")


# 3. PROMEDIO DE VENTAS EN EL AÑO
# ============================================================
promedio_ventas <- mean(Ventas_2024)

cat("3. Promedio de ventas en el año:\n")
cat("   Sentencia: promedio_ventas <- mean(Ventas_2024)\n")
cat("   Resultado:", promedio_ventas, "miles de euros\n\n")


# 4. MES DE MAYOR Y MENOR VENTA
# ============================================================
# Mes de mayor venta
mes_mayor <- which.max(Ventas_2024)
valor_mayor <- max(Ventas_2024)

# Mes de menor venta
mes_menor <- which.min(Ventas_2024)
valor_menor <- min(Ventas_2024)

cat("4. Mes de mayor y menor venta:\n")
cat("   Sentencia mayor: mes_mayor <- which.max(Ventas_2024)\n")
cat("   Sentencia menor: mes_menor <- which.min(Ventas_2024)\n")
cat("   Resultado:\n")
cat("     - Mayor venta: Mes", mes_mayor, "con", valor_mayor, "miles de euros\n")
cat("     - Menor venta: Mes", mes_menor, "con", valor_menor, "miles de euros\n\n")


# 5. VENTAS SUPERIORES O IGUALES A 15
# ============================================================
ventas_mayores_15 <- Ventas_2024[Ventas_2024 >= 15]
meses_mayores_15 <- which(Ventas_2024 >= 15)

cat("5. Ventas superiores o iguales a 15:\n")
cat("   Sentencia: ventas_mayores_15 <- Ventas_2024[Ventas_2024 >= 15]\n")
cat("   Resultado (valores):", ventas_mayores_15, "\n")
cat("   Sentencia meses: meses_mayores_15 <- which(Ventas_2024 >= 15)\n")
cat("   Resultado (meses):", meses_mayores_15, "\n")
cat("   Total de meses con ventas >= 15:", length(ventas_mayores_15), "\n\n")


# 6. EXTRAER LOS MESES 1, 3, 5 Y 10
# ============================================================
meses_seleccionados <- Ventas_2024[c(1, 3, 5, 10)]

cat("6. Extraer los meses 1, 3, 5 y 10:\n")
cat("   Sentencia: meses_seleccionados <- Ventas_2024[c(1, 3, 5, 10)]\n")
cat("   Resultado:\n")
cat("     - Mes 1:", Ventas_2024[1], "\n")
cat("     - Mes 3:", Ventas_2024[3], "\n")
cat("     - Mes 5:", Ventas_2024[5], "\n")
cat("     - Mes 10:", Ventas_2024[10], "\n")
cat("   Vector completo:", meses_seleccionados, "\n\n")


# ============================================================
# RESUMEN VISUAL
# ============================================================
cat("============================================================\n")
cat("RESUMEN DE RESULTADOS\n")
cat("============================================================\n")
cat("Total de ventas anuales:    ", total_ventas, "miles de euros\n")
cat("Promedio mensual:           ", promedio_ventas, "miles de euros\n")
cat("Mejor mes:                   Mes", mes_mayor, "(", valor_mayor, ")\n")
cat("Peor mes:                    Mes", mes_menor, "(", valor_menor, ")\n")
cat("Meses con ventas >= 15:     ", length(ventas_mayores_15), "meses\n")
cat("============================================================\n")


# OPCIONAL: Gráfico simple de las ventas
if (interactive()) {
  barplot(Ventas_2024, 
          names.arg = 1:12,
          main = "Ventas Mensuales 2024",
          xlab = "Mes",
          ylab = "Ventas (miles de euros)",
          col = "steelblue",
          border = "white")
  abline(h = promedio_ventas, col = "red", lwd = 2, lty = 2)
  legend("topright", legend = "Promedio", col = "red", lty = 2, lwd = 2)
}
