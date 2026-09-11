# MPG exploratory analysis
#
# Run from the project root:
#   Rscript "r project.R"
# Or choose a different output directory:
#   Rscript "r project.R" results
#
# The script uses ggplot2's built-in `mpg` data set and writes tables and plots
# to the selected output directory. Generated files are intentionally ignored by
# Git so the analysis can be rerun without creating working-tree noise.

required_packages <- c("dplyr", "ggplot2")
missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]

if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install them with install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    ")) and run the script again.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

get_output_dir <- function() {
  arguments <- commandArgs(trailingOnly = TRUE)

  if (length(arguments) > 1) {
    stop(
      "Provide at most one argument: the output directory.",
      call. = FALSE
    )
  }

  if (length(arguments) == 0) "output" else arguments[[1]]
}

validate_mpg_data <- function(data) {
  required_columns <- c(
    "manufacturer", "model", "displ", "year", "trans", "drv", "cty", "hwy"
  )
  absent_columns <- setdiff(required_columns, names(data))

  if (length(absent_columns) > 0) {
    stop(
      "The mpg data set is missing required column(s): ",
      paste(absent_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(data) == 0) {
    stop("The mpg data set contains no observations.", call. = FALSE)
  }

  invisible(data)
}

prepare_mpg_data <- function(data) {
  data %>%
    mutate(
      transmission = case_when(
        grepl("^auto", trans, ignore.case = TRUE) ~ "Automatic",
        grepl("^manual", trans, ignore.case = TRUE) ~ "Manual",
        TRUE ~ "Other"
      ),
      transmission = factor(transmission, levels = c("Automatic", "Manual", "Other")),
      drivetrain = recode(
        drv,
        "f" = "Front-wheel drive",
        "r" = "Rear-wheel drive",
        "4" = "Four-wheel drive",
        .default = "Other"
      ),
      drivetrain = factor(
        drivetrain,
        levels = c("Front-wheel drive", "Rear-wheel drive", "Four-wheel drive", "Other")
      ),
      year = as.integer(year),
      year_label = factor(year)
    )
}

save_plot <- function(plot, output_dir, filename, width, height) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

write_table <- function(data, output_dir, filename) {
  write.csv(
    data,
    file = file.path(output_dir, filename),
    row.names = FALSE,
    na = ""
  )
}

model_coefficients <- function(model) {
  coefficients <- as.data.frame(summary(model)$coefficients)
  coefficients <- cbind(term = rownames(coefficients), coefficients)
  rownames(coefficients) <- NULL
  coefficients
}

main <- function(output_dir = get_output_dir()) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  raw_mpg <- ggplot2::mpg
  validate_mpg_data(raw_mpg)
  mpg_data <- prepare_mpg_data(raw_mpg)

  manufacturer_summary <- mpg_data %>%
    group_by(manufacturer) %>%
    summarise(
      vehicles = n(),
      average_city_mpg = mean(cty, na.rm = TRUE),
      average_highway_mpg = mean(hwy, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(average_highway_mpg), manufacturer)

  drivetrain_summary <- mpg_data %>%
    group_by(drivetrain) %>%
    summarise(
      vehicles = n(),
      average_city_mpg = mean(cty, na.rm = TRUE),
      average_highway_mpg = mean(hwy, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(average_highway_mpg))

  transmission_summary <- mpg_data %>%
    group_by(transmission) %>%
    summarise(
      vehicles = n(),
      average_city_mpg = mean(cty, na.rm = TRUE),
      average_highway_mpg = mean(hwy, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(average_highway_mpg))

  year_summary <- mpg_data %>%
    group_by(year) %>%
    summarise(
      vehicles = n(),
      average_city_mpg = mean(cty, na.rm = TRUE),
      average_highway_mpg = mean(hwy, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(year)

  write_table(manufacturer_summary, output_dir, "manufacturer_summary.csv")
  write_table(drivetrain_summary, output_dir, "drivetrain_summary.csv")
  write_table(transmission_summary, output_dir, "transmission_summary.csv")
  write_table(year_summary, output_dir, "year_summary.csv")

  # The overall model quantifies the displacement/highway-MPG relationship.
  # A second model permits that relationship to differ by transmission type.
  displacement_model <- lm(hwy ~ displ, data = mpg_data)
  transmission_model <- lm(hwy ~ displ * transmission, data = mpg_data)

  write_table(
    model_coefficients(displacement_model),
    output_dir,
    "displacement_model_coefficients.csv"
  )
  write_table(
    model_coefficients(transmission_model),
    output_dir,
    "transmission_model_coefficients.csv"
  )

  report_lines <- c(
    "MPG analysis report",
    paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Source: ggplot2::mpg (", nrow(mpg_data), " vehicles)", sep = ""),
    "",
    "Key findings from this sample:",
    paste(
      "Highest average highway MPG manufacturer:",
      manufacturer_summary$manufacturer[[1]],
      sprintf("(%.1f MPG)", manufacturer_summary$average_highway_mpg[[1]])
    ),
    paste(
      "Highest average highway MPG drivetrain:",
      as.character(drivetrain_summary$drivetrain[[1]]),
      sprintf("(%.1f MPG)", drivetrain_summary$average_highway_mpg[[1]])
    ),
    paste(
      "Highest average highway MPG transmission:",
      as.character(transmission_summary$transmission[[1]]),
      sprintf("(%.1f MPG)", transmission_summary$average_highway_mpg[[1]])
    ),
    sprintf(
      "Displacement model adjusted R-squared: %.3f",
      summary(displacement_model)$adj.r.squared
    ),
    "",
    "Interpretation note: this is a descriptive sample, not a causal study."
  )
  writeLines(report_lines, con = file.path(output_dir, "analysis_report.txt"))

  project_theme <- theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey30"),
      axis.title.y = element_text(margin = margin(r = 8)),
      panel.grid.minor = element_blank()
    )

  manufacturer_plot <- ggplot(
    manufacturer_summary,
    aes(x = reorder(manufacturer, average_highway_mpg), y = average_highway_mpg)
  ) +
    geom_col(fill = "#2C7FB8", width = 0.72) +
    coord_flip() +
    labs(
      title = "Average Highway MPG by Manufacturer",
      subtitle = "Unweighted average across vehicles in ggplot2::mpg",
      x = NULL,
      y = "Average highway MPG"
    ) +
    project_theme

  drivetrain_plot <- ggplot(
    drivetrain_summary,
    aes(x = reorder(drivetrain, average_highway_mpg), y = average_highway_mpg)
  ) +
    geom_col(fill = "#41AB5D", width = 0.65) +
    coord_flip() +
    labs(
      title = "Average Highway MPG by Drivetrain",
      subtitle = "Drivetrain codes have been expanded for readability",
      x = NULL,
      y = "Average highway MPG"
    ) +
    project_theme

  displacement_plot <- ggplot(mpg_data, aes(x = displ, y = hwy, colour = transmission)) +
    geom_point(alpha = 0.72, size = 2) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
    scale_colour_manual(
      values = c("Automatic" = "#D95F0E", "Manual" = "#2C7FB8", "Other" = "#756BB1"),
      drop = FALSE
    ) +
    labs(
      title = "Engine Displacement and Highway MPG",
      subtitle = "Fitted linear trends are shown separately by transmission",
      x = "Engine displacement (litres)",
      y = "Highway MPG",
      colour = "Transmission"
    ) +
    project_theme

  year_plot <- ggplot(mpg_data, aes(x = year_label, y = hwy)) +
    geom_boxplot(fill = "#9ECAE1", colour = "#2171B5", width = 0.55, outlier.alpha = 0.5) +
    geom_jitter(width = 0.1, alpha = 0.22, colour = "#252525") +
    labs(
      title = "Highway MPG Distribution by Model Year",
      subtitle = "The data include only the 1999 and 2008 model years",
      x = "Model year",
      y = "Highway MPG"
    ) +
    project_theme

  save_plot(manufacturer_plot, output_dir, "manufacturer_highway_mpg.png", 8, 6)
  save_plot(drivetrain_plot, output_dir, "drivetrain_highway_mpg.png", 8, 4.5)
  save_plot(displacement_plot, output_dir, "displacement_highway_mpg.png", 8, 5.5)
  save_plot(year_plot, output_dir, "year_highway_mpg.png", 7, 5)

  message(
    "Analysis complete. Wrote ",
    normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  )
}

main()
