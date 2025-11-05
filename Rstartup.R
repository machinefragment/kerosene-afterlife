# setup.R
# project setup for Kerosene Afterlife 
library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(tmap)
library(here)

# Set options
options(stringsAsFactors = FALSE)

# Directory Paths
data_dir <- here("data")
scripts_dir <- here("scripts")
out_dir <- here("outputs")
notebooks_dir <- here("notebooks")

cat("Project Environment Initialized!")