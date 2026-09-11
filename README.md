# MPG Exploratory Analysis

A reproducible R analysis of the built-in `ggplot2::mpg` data set. The project summarizes city and highway fuel economy and creates publication-ready charts for manufacturer, drivetrain, transmission, engine displacement, and model year.

## What it produces

Running the analysis writes the following generated files to `output/` by default:

| File | Description |
| --- | --- |
| `manufacturer_summary.csv` | Vehicle count plus average city and highway MPG for each manufacturer |
| `drivetrain_summary.csv` | Fuel-economy summary by expanded drivetrain name |
| `transmission_summary.csv` | Fuel-economy summary by automatic/manual transmission grouping |
| `year_summary.csv` | Fuel-economy summary for each available model year |
| `displacement_model_coefficients.csv` | Coefficients from the overall `hwy ~ displ` linear model |
| `transmission_model_coefficients.csv` | Coefficients from the displacement/transmission interaction model |
| `analysis_report.txt` | Timestamped, concise report with sample-level highlights |
| `*.png` | Four high-resolution analysis charts (300 DPI) |

Generated results are excluded from Git and can be recreated at any time.

## Requirements

- R 4.1 or newer
- The R packages `ggplot2` and `dplyr`

Install the packages once if necessary:

```r
install.packages(c("ggplot2", "dplyr"))
```

## Run the analysis

From the repository root:

```sh
Rscript "r project.R"
```

To write results to a different directory:

```sh
Rscript "r project.R" results
```

The script checks that required columns and packages are available before doing any work. It will create the requested output directory if it does not exist.

## Analysis choices

- **Transmission cleanup:** source labels such as `auto(l4)` and `manual(m5)` are grouped by their prefix into `Automatic` and `Manual`. This avoids the incomplete, hard-coded recoding in the original exploratory script.
- **Drivetrain labels:** `f`, `r`, and `4` are expanded to front-wheel, rear-wheel, and four-wheel drive for readable outputs.
- **Year comparison:** the data contains only 1999 and 2008 observations, so the project uses a distribution plot rather than presenting a two-point linear trend as a time-series result.
- **Model interpretation:** the linear models describe associations within this sample. They do not support causal claims about engine size or transmission.

## Project layout

```text
.
├── r project.R            # Reproducible analysis entry point
├── My First Project.Rproj # RStudio project configuration
├── README.md              # Project instructions and methodology
└── .gitignore             # Keeps generated/local files out of Git
```
