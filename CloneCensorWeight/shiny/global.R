# copy files
file.copy(from = here::here("..", "figures", "diagram.png"), to = here::here("www", "diagram.png"), overwrite = TRUE)
file.copy(from = system.file("oxford.png", package = "OmopViewer"), to = here::here("www", "oxford.png"), overwrite = TRUE)
file.copy(from = system.file("logos/hds_logo.svg", package = "OmopViewer"), to = here::here("www", "hds_logo.svg"), overwrite = TRUE)

library(bslib)
library(shiny)
library(here)
library(readr)
library(purrr)
library(tools)
library(stringr)
library(shinyWidgets)
library(reactable)
library(markdown)

files <- list.files(path = here("data"), full.names = TRUE, pattern = ".csv$")
nms <- basename(files) |>
  str_extract(pattern = ".*(?=_CPRD)")
names(files) <- nms
res <- unique(nms) |>
  set_names() |>
  map(\(x) bind_rows(map(files[names(files) == x], read_csv, show_col_types = FALSE)))
