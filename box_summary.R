# Install if necessary
# install.packages(c("ggplot2", "dplyr", "gt", "knitr", "viridis"))

# Main function
library(readr)
library(ggplot2)
library(dplyr)
library(gt)
library(knitr)

box_summary <- function(numeric_var, group_var = NULL, return.graph = TRUE) {
  # Check if numeric_var is numeric
  if (!is.numeric(numeric_var)) {
    stop("The first argument must be a numeric variable.")
  }
  # If return.graph = TRUE → generate boxplots
  if (return.graph) {
    if (is.null(group_var)) {
      # Simple boxplot without grouping
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
      # Grouped boxplot by categorical variable
      df <- data.frame(Group = as.factor(group_var),
                       Value = numeric_var)
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
    print(p)  # Show plot
  } else {
    # return.graph = FALSE → generate summary tables
    if (is.null(group_var)) {
      summary_table <- data.frame(
        Minimum = min(numeric_var, na.rm = TRUE),
        Q1 = quantile(numeric_var, 0.25, na.rm = TRUE),
        Median = median(numeric_var, na.rm = TRUE),
        Q3 = quantile(numeric_var, 0.75, na.rm = TRUE),
        Maximum = max(numeric_var, na.rm = TRUE)
      )
      # Version 1: Console table
      cat("\n--- Table: Statistical Summary of the Variable ---\n")
      print(knitr::kable(summary_table, row.names = FALSE, digits = 3))
      # Version 2: Visual table
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
      df <- data.frame(Group = as.factor(group_var),
                       Value = numeric_var)
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
      # Version 1: Console table
      cat("\n--- Table: Statistical Summary by Group ---\n")
      print(knitr::kable(summary_table, row.names = FALSE, digits = 3))
      # Version 2: Visual table
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


# Cargar los datos

dataset <- read_csv("Pathname")

# Example 1: Only numeric variable (simple boxplot)
box_summary(dataset$avg_glucose_level)

# Example 2: Numeric and categorical variable (grouped boxplot)
box_summary(dataset$avg_glucose_level, dataset$work_type)

# Example 3: Only numeric variable, summary table
box_summary(dataset$avg_glucose_level, return.graph = FALSE)

# Example 4: Numeric and categorical variable, grouped summary table
box_summary(dataset$avg_glucose_level, dataset$work_type, return.graph = FALSE)




