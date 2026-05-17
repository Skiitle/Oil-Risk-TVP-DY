# Oil-Risk-TVP-DY (Public)

Official implementation for the paper:

> **"The Risk Spillover Mechanism of the Oil Industry Chain under the US-Israel-Iran Conflict: An Empirical Analysis Based on TVP-VAR-DY"** (Preprint, forthcoming on arXiv)

## Overview

We propose a two-stage empirical framework combining the Hidden Markov Model (HMM) with the TVP-VAR-DY spillover index method to analyze risk transmission in China's oil industry chain amid the 2026 US-Israel-Iran conflict. Using daily return data of CITIC tertiary industry indices (Oil Extraction III, Oil Refining, and Oil Product Sales & Trading) from January 2, 2025 to April 17, 2026, we identify volatility regimes and quantify dynamic risk spillovers.

### Key contributions:

-   **Methodological**: Integrates Student-t HMM for regime identification with TVP-VAR-DY for time-varying spillover measurement, enabling precise capture of structural breaks in risk transmission pathways.

-   **Empirical**: Identifies a systematic reversal in risk transmission during high-volatility periods: midstream refining emerges as the sole bilateral net transmitter, while upstream extraction shifts from net transmitter to net receiver — a pattern consistent across both "OPEC+" and US-Israel-Iran shocks.

-   **Practical**: Demonstrates that TCI exhibits abnormal declines prior to high-volatility regimes identified by HMM, suggesting potential early-warning properties. Provides a reproducible R pipeline for industry-level risk monitoring.

## Data

The data used in this paper are sourced from the **Wind database**. Due to licensing restrictions, the raw data cannot be distributed with this repository.

### Replication steps:

1.  Export the required variables from Wind terminal (see `data/variable_list.csv` for details):
    -   Oil Extraction III (中信三级: 石油开采III)
    -   Oil Refining (中信三级: 炼油)
    -   Oil Product Sales & Trading (中信三级: 油品贸易及销售)
    -   Time period: January 2, 2025 – April 17, 2026
2.  Calculate daily log returns: `r = ln(P_t / P_{t-1}) * 100`
3.  Save the data as `data/wind_data.csv` with columns: `date`, `extraction`, `refining`, `trading`
4.  Run `main.R`

## Environment

This code is written in R. Required packages:

\`\`\`r install.packages(c( "tidyverse", "MSwM", \# Hidden Markov Models "vars", \# VAR models "igraph", \# Network visualization "ggplot2", \# Plotting "zoo", \# Time series "fUnitRoots" \# Stationarity tests ))
