#!/usr/bin/env Rscript
# Convert student-facing .Rmd files to WebR-enabled .qmd for the Quarto website.
# Source .Rmd files are left unchanged for local RStudio use.

source_files <- c(
  "tutorials/Week1_install_r_rstudio.Rmd",
  "tutorials/Week2_data_structures.Rmd",
  "tutorials/Week3_functions_packages.Rmd",
  "tutorials/Week4_data_exploration.Rmd",
  "tutorials/Week5_data_manipulation.Rmd",
  "tutorials/Week6_descriptive_statistics.Rmd",
  "tutorials/Week7_statistical_analyses.Rmd",
  "tutorials/Week8-9_data_visualizations.Rmd",
  "tutorials/Week10_research_best_practices.Rmd",
  "hw/HW1_markdown_and_data_types.Rmd",
  "hw/HW2_data_structures_and_functions.Rmd",
  "hw/HW3_data_exploration_and_manipulation.Rmd",
  "hw/HW5_data_visualizations.Rmd",
  "final/Final_Project.Rmd"
)

sync_dengue_data <- function() {
  for (f in c("dengue_individual_data.csv", "dengue_household_data.csv")) {
    src <- file.path("final", f)
    dst <- file.path("data", f)
    if (file.exists(src)) {
      file.copy(src, dst, overwrite = TRUE)
      message("Synced ", src, " -> ", dst)
    }
  }
}

data_helper_chunk <- function() {
  c(
    "```{webr-r}",
    "#| context: setup",
    "# Helper: download bundled data into the WebR virtual filesystem",
    "webr_read_csv <- function(path) {",
    "  local_name <- basename(path)",
    "  if (!file.exists(local_name)) {",
    "    download.file(path, local_name, quiet = TRUE, mode = \"wb\")",
    "  }",
    "  readr::read_csv(local_name, show_col_types = FALSE)",
    "}",
    "```"
  )
}

needs_data_helper <- function(lines) {
  any(grepl("webr_read_csv\\(|read_csv\\(|read_excel\\(", lines))
}

convert_rmd_to_qmd <- function(rmd_path) {
  qmd_path <- sub("\\.Rmd$", ".qmd", rmd_path)
  lines <- readLines(rmd_path, warn = FALSE, encoding = "UTF-8")
  body_start <- which(grepl("^---\\s*$", lines))[2] + 1
  yaml_end <- body_start - 2
  yaml_lines <- lines[2:(yaml_end)]
  body_lines <- lines[body_start:length(lines)]

  title_line <- yaml_lines[grepl("^title:", yaml_lines)]
  title <- sub("^title:\\s*", "", title_line)
  title <- gsub("^['\"]|['\"]\\s*$", "", title)

  new_yaml <- c(
    "---",
    paste0("title: \"", title, "\""),
    "---"
  )

  depth <- nchar(dirname(rmd_path)) - nchar(gsub("/", "", dirname(rmd_path))) + 1
  data_prefix <- paste(rep("..", depth), collapse = "/")

  converted_body <- convert_body(body_lines, data_prefix)
  if (needs_data_helper(converted_body)) {
    converted_body <- c(data_helper_chunk(), "", converted_body)
  }

  writeLines(c(new_yaml, "", converted_body), qmd_path, useBytes = TRUE)
  message("Wrote ", qmd_path)
}

convert_body <- function(lines, data_prefix) {
  out <- character()
  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]

    if (grepl("^\\s*```\\{r", line)) {
      chunk_header <- line
      chunk_lines <- character()
      i <- i + 1
      while (i <= length(lines) && !grepl("^\\s*```\\s*$", lines[i])) {
        chunk_lines <- c(chunk_lines, lines[i])
        i <- i + 1
      }

      if (grepl("include\\s*=\\s*FALSE", chunk_header)) {
        out <- c(out, chunk_header, chunk_lines, sub("^\\s*", "", lines[i]))
      } else {
        new_header <- sub("\\{r", "{webr-r", chunk_header)
        new_chunk <- adapt_chunk_for_webr(chunk_lines, data_prefix)
        closing <- sub("^\\s*", "", lines[i])
        out <- c(out, new_header, new_chunk, closing)
      }
      i <- i + 1
      next
    }

    out <- c(out, line)
    i <- i + 1
  }

  out
}

adapt_chunk_for_webr <- function(chunk_lines, data_prefix) {
  text <- paste(chunk_lines, collapse = "\n")

  text <- gsub("library\\(tidyverse\\)", "# tidyverse packages are pre-loaded on this site", text)
  text <- gsub("library\\(here\\)", "# data paths are adjusted for the browser below", text)
  text <- gsub("here::here\\(", "here(", text)

  text <- gsub(
    "here\\(\"data\"\\s*,\\s*\"([^\"]+)\"\\)",
    paste0("\"", data_prefix, "/data/\\1\""),
    text
  )
  text <- gsub(
    "here\\(\"data/([^\"]+)\"\\)",
    paste0("\"", data_prefix, "/data/\\1\""),
    text
  )

  text <- gsub("file\\.exists\\([^)]+\\)", "TRUE  # data bundled with this site", text)
  text <- gsub("^here\\(\\)\\s*$", "# Working directory is managed by WebR on this site", text, perl = TRUE)

  text <- gsub(
    "read_csv\\(\\s*(?:file\\s*=\\s*)?(\"(?:\\.\\./)?data/[^\"]+\")(?:\\s*,[^)]+)?\\s*\\)",
    "webr_read_csv(\\1)",
    text
  )

  text <- gsub(
    "read_excel\\(\\s*(?:file\\s*=\\s*)?(\"(?:\\.\\./)?data/[^\"]+\")(?:\\s*,[^)]+)?\\s*\\)",
    "webr_read_excel(\\1)",
    text
  )

  strsplit(text, "\n", fixed = TRUE)[[1]]
}

sync_dengue_data()

for (f in source_files) {
  if (!file.exists(f)) {
    warning("Skipping missing file: ", f)
    next
  }
  convert_rmd_to_qmd(f)
}

validate_data_files <- function() {
  qmd_files <- c(
    "index.qmd",
    list.files(c("tutorials", "hw", "final"), pattern = "\\.qmd$", full.names = TRUE)
  )

  paths <- character()
  for (f in qmd_files) {
    text <- paste(readLines(f, warn = FALSE), collapse = "\n")
    found <- regmatches(
      text,
      gregexpr('(?<=webr_read_csv\\(")[^"]+(?=")', text, perl = TRUE)
    )[[1]]
    if (length(found) > 0) paths <- c(paths, found)
  }
  paths <- unique(paths)
  missing <- paths[!file.exists(file.path("data", basename(paths)))]

  if (length(missing) > 0) {
    stop(
      "Missing data files required by the website:\n",
      paste0("  - ", missing, collapse = "\n"),
      call. = FALSE
    )
  }

  message("Validated ", length(paths), " data file reference(s).")
}

validate_data_files()
