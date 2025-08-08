#funcion 1

box_summary <- function(numeric_var, group_var = NULL, return.graph = TRUE) {
  
  # --------------------- Verificar e informar si faltan paquetes ---------------------
  required_pkgs <- c("ggplot2", "dplyr", "gt", "knitr", "readr", "viridis")
  missing_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
  
  if (length(missing_pkgs) > 0) {
    message("⚠️ Los siguientes paquetes no están instalados:\n")
    cat(paste0("📦 ", missing_pkgs, collapse = "\n"), "\n\n")
    message("Puedes instalarlos ejecutando:\n")
    cat("install.packages(c(\"", paste(missing_pkgs, collapse = "\", \""), "\"))\n", sep = "")
    stop("⛔ Por favor instala los paquetes requeridos y vuelve a correr la función.")
  }
  
  # --------------------- Cargar librerías ---------------------
  library(ggplot2)
  library(dplyr)
  library(gt)
  library(knitr)
  library(readr)
  library(viridis)
  
  # --------------------- Verificar tipo de variable ---------------------
  if (!is.numeric(numeric_var)) {
    stop("El primer argumento debe ser una variable numérica.")
  }
  
  # --------------------- Si se solicita gráfico ---------------------
  if (return.graph) {
    if (is.null(group_var)) {
      # Boxplot simple
      df <- data.frame(Value = numeric_var)
      p <- ggplot(df, aes(x = "", y = Value)) +
        geom_boxplot(fill = "#1f77b4", color = "black", size = 1.2, 
                     outlier.shape = 16, outlier.color = "red") +
        labs(title = "Boxplot of the Numeric Variable",
             x = NULL,
             y = "Value") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.y = element_text(face = "bold"),
          axis.text = element_text(face = "bold")
        )
    } else {
      # Boxplot por grupos
      df <- data.frame(Group = as.factor(group_var), Value = numeric_var)
      p <- ggplot(df, aes(x = Group, y = Value, fill = Group)) +
        geom_boxplot(color = "black", size = 1.2,
                     outlier.shape = 16, outlier.color = "red") +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Grouped Boxplot by Category",
             x = "Category",
             y = "Value") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          legend.position = "none"
        )
    }
    print(p)
    
    # --------------------- Si se solicita tabla resumen ---------------------
  } else {
    if (is.null(group_var)) {
      summary_table <- data.frame(
        Minimum = min(numeric_var, na.rm = TRUE),
        Q1 = quantile(numeric_var, 0.25, na.rm = TRUE),
        Median = median(numeric_var, na.rm = TRUE),
        Q3 = quantile(numeric_var, 0.75, na.rm = TRUE),
        Maximum = max(numeric_var, na.rm = TRUE)
      )
      cat("\n--- Table: Statistical Summary of the Variable ---\n")
      print(knitr::kable(summary_table, row.names = FALSE, digits = 3))
      
      return(
        summary_table %>%
          gt() %>%
          tab_header(title = "Statistical Summary of the Variable") %>%
          fmt_number(columns = everything(), decimals = 2) %>%
          cols_label(
            Minimum = "Minimum",
            Q1 = "Q1 (25%)",
            Median = "Median (Q2)",
            Q3 = "Q3 (75%)",
            Maximum = "Maximum"
          )
      )
      
    } else {
      df <- data.frame(Group = as.factor(group_var), Value = numeric_var)
      summary_table <- df %>%
        group_by(Group) %>%
        summarise(
          Minimum = min(Value, na.rm = TRUE),
          Q1 = quantile(Value, 0.25, na.rm = TRUE),
          Median = median(Value, na.rm = TRUE),
          Q3 = quantile(Value, 0.75, na.rm = TRUE),
          Maximum = max(Value, na.rm = TRUE),
          .groups = "drop"
        )
      cat("\n--- Table: Statistical Summary by Group ---\n")
      print(knitr::kable(summary_table, row.names = FALSE, digits = 3))
      
      return(
        summary_table %>%
          gt() %>%
          tab_header(title = "Statistical Summary by Group") %>%
          fmt_number(columns = where(is.numeric), decimals = 2) %>%
          cols_label(
            Group = "Group",
            Minimum = "Minimum",
            Q1 = "Q1 (25%)",
            Median = "Median (Q2)",
            Q3 = "Q3 (75%)",
            Maximum = "Maximum"
          )
      )
    }
  }
}

# funcion 2
normality_qqplot <- function(y,
                             datax = FALSE,
                             distribution = qnorm,
                             probs = c(0.25, 0.75),
                             qtype = 7,
                             transformation = "none",  # solo una
                             save = FALSE,
                             save_path = ".",
                             filename_prefix = "qqplot",
                             save_format = "png",
                             dpi = 300,
                             width = 6,
                             height = 6) {
  
  # --------- 1. Verificar paquetes necesarios ---------
  required_pkgs <- c("ggplot2", "car", "bestNormalize", "scales")
  missing_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
  if (length(missing_pkgs) > 0) {
    message("⚠️ Paquetes faltantes:\n")
    cat(paste0("📦 ", missing_pkgs, collapse = "\n"), "\n\n")
    message("Puedes instalarlos con:\n")
    cat("install.packages(c(\"", paste(missing_pkgs, collapse = "\", \""), "\"))\n", sep = "")
    stop("⛔ Por favor instala los paquetes requeridos antes de continuar.")
  }
  
  # Cargar paquetes
  library(ggplot2)
  library(car)
  library(bestNormalize)
  library(scales)
  
  # --------- 2. Función auxiliar de transformación ---------
  apply_transformation <- function(data, method) {
    switch(method,
           "none" = data,
           "log" = log(data),
           "sqrt" = sqrt(data),
           "inverse" = 1 / data,
           "yeojohnson" = {
             result <- yeojohnson(data, standardize = FALSE)
             as.numeric(result$x.t)
           },
           "zscore" = as.numeric(scale(data)),
           stop("❌ Transformación no reconocida.")
    )
  }
  
  # --------- 3. Función auxiliar para crear Q-Q plot ---------
  create_qqplot <- function(data, title_suffix) {
    df <- data.frame(y = sort(data),
                     x = sort(distribution(ppoints(length(data)))))
    if (datax) {
      names(df) <- c("x", "y")
    }
    
    # Línea de referencia
    q <- quantile(data, probs = probs, type = qtype, na.rm = TRUE)
    theo_q <- distribution(probs)
    slope <- diff(q) / diff(theo_q)
    intercept <- q[1L] - slope * theo_q[1L]
    
    ggplot(df, aes(x = x, y = y)) +
      geom_point(size = 2, color = "blue") +
      geom_abline(slope = slope, intercept = intercept, color = "red", size = 1) +
      labs(title = paste("Normal Q-Q Plot -", title_suffix),
           x = ifelse(datax, "Sample Quantiles", "Theoretical Quantiles"),
           y = ifelse(datax, "Theoretical Quantiles", "Sample Quantiles")) +
      theme_bw() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold")
      )
  }
  
  # --------- 4. Validación de entrada ---------
  y <- na.omit(y)
  if (!is.numeric(y)) stop("El vector de datos debe ser numérico.")
  
  # Verificar valores positivos si es necesario
  if (transformation %in% c("log", "sqrt", "inverse") && any(y <= 0)) {
    stop("❌ Para aplicar log, sqrt o inverse, los datos deben ser positivos.")
  }
  
  # --------- 5. Aplicar transformación y generar gráfico ---------
  transformed <- tryCatch(apply_transformation(y, transformation), error = function(e) {
    message("⚠️ Error en transformación: ", conditionMessage(e))
    return(NULL)
  })
  
  if (is.null(transformed)) return(NULL)
  
  p <- create_qqplot(transformed, transformation)
  print(p)
  
  if (save) {
    fname <- paste0(filename_prefix, "_", transformation, ".", save_format)
    ggsave(filename = fname, plot = p, path = save_path,
           dpi = dpi, width = width, height = height)
    message(paste0("✅ Gráfico guardado en: ", file.path(save_path, fname)))
  }
}

# funcion 3

scatter_with_marginals <- function(x,
                                   y,
                                   add_trend = TRUE,
                                   conf_band = FALSE,     # <- Nuevo parámetro
                                   add_marginals = TRUE,
                                   point_size = 1,
                                   title = NULL,
                                   xlab = NULL,
                                   ylab = NULL) {
  
  # -------------------- 0) Verificar paquetes --------------------
  required_pkgs <- c("ggplot2", "ggExtra")
  missing_pkgs <- setdiff(required_pkgs, rownames(utils::installed.packages()))
  if (length(missing_pkgs) > 0) {
    message("⚠️ Paquetes faltantes:\n  ", paste(missing_pkgs, collapse = ", "))
    message("\nInstálalos con:\n  install.packages(c(\"",
            paste(missing_pkgs, collapse = "\", \""), "\"))")
    stop("⛔ Por favor instala los paquetes requeridos y vuelve a ejecutar la función.")
  }
  
  suppressPackageStartupMessages({
    library(ggplot2)
    library(ggExtra)
  })
  
  # -------------------- 1) Validaciones --------------------
  if (!is.numeric(x)) {
    stop("⛔ El argumento 'x' debe ser un vector numérico.")
  }
  if (!is.numeric(y)) {
    stop("⛔ El argumento 'y' debe ser un vector numérico.")
  }
  if (length(x) != length(y)) {
    stop("⛔ 'x' e 'y' deben tener la misma longitud.")
  }
  
  # Crear data.frame interno
  df <- data.frame(x = x, y = y)
  
  if (is.null(title)) title <- "Scatterplot"
  if (is.null(xlab))  xlab  <- "X"
  if (is.null(ylab))  ylab  <- "Y"
  
  # -------------------- 2) Scatterplot base --------------------
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(alpha = 0.9, size = point_size) +
    labs(title = title, x = xlab, y = ylab) +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      axis.title = element_text(face = "bold", size = 14),
      axis.text  = element_text(face = "bold")
    )
  
  # -------------------- 3) Línea de tendencia (lm por defecto) --------------------
  if (add_trend) {
    p <- p + geom_smooth(method = "lm", se = conf_band)
    
    xr <- range(df$x, na.rm = TRUE)
    yr <- range(df$y, na.rm = TRUE)
    x_pos <- xr[1] + 0.02 * diff(xr)
    y_pos <- yr[2] - 0.05 * diff(yr)
    
    p <- p + annotate(
      "text",
      x = x_pos, y = y_pos,
      label = "",
      hjust = 0, vjust = 1, fontface = "bold"
    )
  }
  
  # -------------------- 4) Boxplots marginales azules --------------------
  if (add_marginals) {
    m <- ggMarginal(
      p,
      type = "boxplot",
      margins = "both",
      xparams = list(fill = "blue", color = "black"),
      yparams = list(fill = "blue", color = "black")
    )
    return(m)
  } else {
    return(p)
  }
}





