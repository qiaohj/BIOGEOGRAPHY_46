# Spatiotemporal Mismatches in Macroecology and Biogeography

## Overview
This repository contains the data and analytical code for the manuscript: **"Spatiotemporal Mismatches in Macroecology and Biogeography: The Global Divide between Knowledge Producers and Knowledge Subjects"**. 

The study analyzes research articles published in four major journals (*Diversity and Distributions*, *Ecography*, *Global Ecology and Biogeography*, and *Journal of Biogeography*) to quantify structural geographic disparities in knowledge production. It utilizes a Large Language Model (LLM) for automated metadata extraction from full-text PDFs to map the global distribution of research leadership and highlight mismatches between knowledge producers and study areas.

## Computational Environment & Prerequisites
The analyses were entirely conducted in the **R programming environment** (v4.5 or higher recommended).

To reproduce the analyses, ensure you have the following key R packages installed:
* **Data Manipulation & Spatial:** `data.table`, `sf`, `terra`, `httr`
* **Statistical Modeling:** `glmmTMB`, `strucchange`
* **Visualization & Others:** `ggplot2`, `jsonlite`

## Directory Structure
The repository is organized functionally to mirror the chronological steps of the analysis pipeline.

* **`API.KEYS/`**: Contains configuration scripts for API keys (`gemini.keys`, `tokens.r`). *Note: Users must provide their own API keys to replicate the data downloading and LLM parsing steps.*
* **`Configurations/`**: Contains global configuration files, such as custom color palettes (`color.r`) used across all visualizations.
* **`Download.PDF/`**: Scripts responsible for programmatically interacting with the Web of Science Starter API and the Wiley TDM API to fetch metadata and download full-text PDFs.
* **`Prompt/`**: Markdown files containing the exact prompts submitted to the Gemini API for metadata extraction (`extract.info.prompt.md`, `affiliation_2_iso.md`, `geo_2_country.md`).
* **`Download_and_Parse_PDF/`**: Scripts that process the downloaded PDFs, interface with the LLM API to extract authorship roles and study locations, and standardize the geopolitical data into ISO-3166-1 alpha-3 codes.
* **`Analysis/`**: Contains the core statistical analyses, including the extraction of macroeconomic data (`MSTI.r`), breakpoint analysis over time (`N.by.year.r`), and Generalized Linear Mixed Models (GLMM) evaluating scientific capacity (`MSTI.model.r`).
* **`Figures/`**: Scripts dedicated to generating the figures presented in the manuscript, including temporal trends (`N.article.yearly.r`), proportional authorship roles (`pie.chart.country.r`), and global spatial mismatches (`geo.map.r`).
* **`Others/`**: Miscellaneous helper scripts.

### Large Language Model (LLM) Configuration
The metadata extraction from full-text PDFs was performed using the **Gemini 2.5 Flash** large language model (Google DeepMind). To ensure reproducibility and transparency, the model was configured and accessed with the following specifications:
* **Access Method:** Programmatic access via the Google AI Studio API.
* **Query Period:** August 2025 to March 2026.
* **Key Parameters:** `temperature = 0.1` and `top_p = 0.95`.