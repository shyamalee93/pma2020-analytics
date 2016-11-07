#!/usr/bin/env Rscript

args <- commandArgs(TRUE)

print_usage <- function() {
  cat("usage: Rscript pma-analytics.R scripts_directory storage_directory form_id export_directory export_filename")
  cat("\n\n")
  cat("Positional arguments:")
  cat("\n")
  cat("  scripts_directory          ", "Directory where R scripts are kept. This will be the working directory")
  cat("\n")
  cat("  storage_directory          ", "ODK Briefcase storage directory")
  cat("\n")
  cat("  form_id                    ", "ODK form_id")
  cat("\n")
  cat("  export_directory           ", "Directory for CSV exports")
  cat("\n")
  cat("  export_filename            ", "Filename for exported PMA2020 analytics file")
  cat("\n")
}

if (length(args) < 1) {
  print_usage()
} else if (startsWith(args[1], '-h') | startsWith(args[1], '--h') | length(args) != 5) {
  print_usage()
} else {
  scripts_directory <- args[1]
  storage_directory <- args[2]
  form_id <- args[3]
  export_directory <- args[4]
  export_filename <- args[5]

  #------------------------- BEGIN PMA ANALYTICS ----------------------#
  setwd(scripts_directory)
  source("Instancetime.R")

  src_dir <- file.path(storage_directory, "forms", form_id, "instances")
  output <- file.path(export_directory, export_filename)
  
  file_size(src_dir, output)
}
