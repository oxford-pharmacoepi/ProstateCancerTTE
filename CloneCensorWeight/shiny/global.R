library(bslib)
library(shiny)
library(here)
library(readr)
library(purrr)
library(tools)
library(stringr)

files <- list.files(path = here("CloneCensorWeight", "Results"), full.names = TRUE, pattern = ".csv$")
nms <- basename(files) |>
  str_extract(pattern = ".*(?=_CPRD)")
names(files) <- nms
res <- unique(nms) |>
  set_names() |>
  map(\(x) bind_rows(map(files[names(files) == x], read_csv, show_col_types = FALSE)))
