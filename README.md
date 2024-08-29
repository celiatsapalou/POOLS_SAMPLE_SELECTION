# Genomic Data Processing Pipeline

This repository contains a script that processes and analyzes genomic data, integrating multiple datasets, filtering samples based on specific criteria, and generating visualizations of population and ancestry distributions.

## Table of Contents
- [Introduction](#introduction)
- [Data Sources](#data-sources)
- [Installation](#installation)
- [Usage](#usage)
- [Workflow](#workflow)
- [Output Files](#output-files)
- [License](#license)

## Introduction
The script provided here is designed to manage and process large-scale genomic data, focusing on integrating datasets from different sources, filtering specific populations, and analyzing coverage across different ancestry groups. This process is crucial for ensuring data quality and preparing the dataset for downstream genomic analyses.

## Data Sources
The script uses the following input files:
- `1k_ont_data_overview.xlsx`: The main ONT dataset.
- `compare.xlsx`: Data used to filter out certain samples from the ONT dataset.
- `Eichler_2107_LAST.xlsx`: Dataset from Eichler's study, used to remove overlapping samples.
- `1kgp_samples_pops_threecol.tsv`: Contains population and superpopulation data for samples.
- `/Users/tsapalou/Downloads/U24_Subpopulations_Colors.csv`: Defines color schemes for populations.

## Installation
To run the script, you need to have R installed with the following packages:
- `readxl`
- `dplyr`
- `stringr`
- `openxlsx`
- `ggplot2`
- `ggbeeswarm`

You can install these packages using the following commands:
```R
install.packages(c("readxl", "dplyr", "stringr", "openxlsx", "ggplot2", "ggbeeswarm"))
