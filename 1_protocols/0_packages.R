# ==============================================================================
# Package installation
#
# Run only when setting up the analysis environment.
# These commands do not need to be run every time the analysis is executed.
# ==============================================================================

# CRAN packages

install.packages(
  c(
    "tidyverse",
    "lubridate",
    "scales",
    "patchwork",
    "gridExtra",
    "ggbreak",
    "ggpubr",
    "here",
    "dataRetrieval",
    "zoo",
    "brms",
    "NADA",
    "rlang",
    "knitr",
    "xtable",
    "nasapower",
    "remotes",
    "viridis",
    "truncdist",
    "future",
    "future.apply",
    "data.table",
    "matrixStats"
  )
)


# USGS and GitHub packages

remotes::install_gitlab(
  "water/analysis-tools/smwrData",
  host = "code.usgs.gov"
)

remotes::install_gitlab(
  "water/analysis-tools/smwrBase",
  host = "code.usgs.gov"
)

remotes::install_gitlab(
  "water/analysis-tools/smwrGraphs",
  host = "code.usgs.gov"
)

remotes::install_gitlab(
  "water/analysis-tools/smwrStats",
  host = "code.usgs.gov"
) # requires compilation

remotes::install_gitlab(
  "water/analysis-tools/smwrQW",
  host = "code.usgs.gov"
) # requires compilation

remotes::install_github(
  "appling/unitted"
)

remotes::install_github(
  "DOI-USGS/EGRET"
)

remotes::install_github(
  "USGS-R/rloadest"
)

remotes::install_github(
  "USGS-R/loadflex"
)


# ==============================================================================
# Load packages
# ==============================================================================

# Core data manipulation and plotting

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(gridExtra)
library(ggbreak)
library(ggpubr)
library(viridis)
library(here)


# Hydrologic data access and time-series tools

library(dataRetrieval)
library(zoo)


# Bayesian modeling

library(brms)


# Nutrient-load modeling

library(methods)
library(NADA)
library(loadflex)
library(rloadest)
library(rlang)


# Simulation and parallel processing

library(truncdist)
library(future)
library(future.apply)
library(data.table)
library(matrixStats)


# Tables and output formatting

library(knitr)
library(xtable)


# NASA POWER meteorological data

library(nasapower)


# Base R grid graphics

library(grid)


# Package installation tools

library(remotes)


# ==============================================================================
# Global options
# ==============================================================================

# Display large numbers without scientific notation

options(scipen = 999)