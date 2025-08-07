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






