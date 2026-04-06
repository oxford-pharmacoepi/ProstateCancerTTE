cdm$primary_care <- CohortConstructor::conceptCohort(cdm,
                                           conceptSet = list("primary_care_visit" =  581477),
                                           name = "primary_care") |>
  CohortConstructor::requireIsFirstEntry()

cdm$hes <- CohortConstructor::conceptCohort(cdm,
                                            conceptSet = list("hes_visit" =  9201),
                                            name = "hes") |>
  CohortConstructor::requireIsFirstEntry()


cdm$ncrascr <- CohortConstructor::conceptCohort(cdm,
                                            conceptSet = list("ncrascr_visit" = 38004268),
                                            name = "ncrascr") |>
  CohortConstructor::requireIsFirstEntry()


cdm$rtds <- CohortConstructor::conceptCohort(cdm,
                                             conceptSet = list("rtds_visit" = 38004269),
                                             name = "rtds") |>
  CohortConstructor::requireIsFirstEntry()



cdm <- omopgenerics::bind(cdm$primary_care,cdm$hes, cdm$ncrascr, cdm$rtds, name = "visit_type" )

overlap <- CohortCharacteristics::summariseCohortOverlap(cdm$visit_type)
omopgenerics::exportSummarisedResult(overlap, fileName = "overlap_db_{cdm_name}_{date}.csv", path = here::here("Results"))
primary_ids <- cdm$primary_care |>
  dplyr::distinct(subject_id) |>
  dplyr::pull(subject_id)

hes_ids <- cdm$hes |>
  dplyr::distinct(subject_id) |>
  dplyr::pull(subject_id)

ncrascr_ids <- cdm$ncrascr |>
  dplyr::distinct(subject_id) |>
  dplyr::pull(subject_id)

rtds_ids <- cdm$rtds |>
  dplyr::distinct(subject_id) |>
  dplyr::pull(subject_id)


cohort_list <- list(
  PrimaryCare = as.character(primary_ids),
  HES = as.character(hes_ids),
  NCRASCR = as.character(ncrascr_ids),
  RTDS = as.character(rtds_ids)
)




# VennDiagram: show count (percent%) in each region
library(VennDiagram)
library(grid)

# 1) Prepare cohort_list (ensure unique vectors)
cohort_list <- lapply(cohort_list, function(x) {
  if (inherits(x, c("tbl_sql", "tbl_dbi", "tbl_lazy"))) x <- dplyr::collect(x)
  if (is.data.frame(x)) {
    # pick person_id if present, else first column
    id_col <- if ("person_id" %in% names(x)) "person_id" else names(x)[1]
    x <- x[[id_col]]
  }
  unique(as.character(x))
})

# 2) Totals and partitions
total_n <- length(unique(unlist(cohort_list)))
parts <- VennDiagram::get.venn.partitions(cohort_list)
counts <- as.integer(parts$..count..)

# Build labels "count (pct%)"
pct <- round(100 * counts / total_n, 2)
labels_count_pct <- paste0(counts, " (", pct, "%)")

# 3) Build venn grob (do not let venn.diagram write file)
glist <- venn.diagram(
  x = cohort_list,
  filename = NULL,
  fill = c("red", "blue", "green", "purple"),
  alpha = 0.5,
  cex = 1.2,
  cat.cex = 1.2,
  print.mode = "raw"
)

# 4) Create mapping from count-string -> "count (pct%)"
count_strs <- as.character(counts)
mapping <- setNames(labels_count_pct, count_strs)

# 5) Replacement function (walk grob list and replace ONLY label fields that exactly equal counts)
replace_labels_in_groblist <- function(glist, mapping) {
  for (i in seq_along(glist)) {
    g <- glist[[i]]
    # direct label slot
    if (!is.null(g$label) && is.character(g$label)) {
      new_lbls <- vapply(g$label, function(lbl) {
        if (lbl %in% names(mapping)) mapping[[lbl]] else lbl
      }, FUN.VALUE = character(1))
      glist[[i]]$label <- new_lbls
    }
    # For gTree children, iterate children and replace their labels if present
    if (inherits(g, "gTree") && length(g$children) > 0) {
      ch_names <- names(g$children)
      for (ch in ch_names) {
        child <- g$children[[ch]]
        if (!is.null(child$label) && is.character(child$label)) {
          new_lbls <- vapply(child$label, function(lbl) {
            if (lbl %in% names(mapping)) mapping[[lbl]] else lbl
          }, FUN.VALUE = character(1))
          glist[[i]]$children[[ch]]$label <- new_lbls
        }
      }
    }
  }
  glist
}

glist2 <- replace_labels_in_groblist(glist, mapping)


# 6) Draw & save using headless-friendly device
out_file <- paste0(here::here("Results"), "/venn_diagram_", dbName, ".png")
if (requireNamespace("ragg", quietly = TRUE)) {
  ragg::agg_png(out_file, width = 2000, height = 2000, res = 300)
  grid::grid.draw(glist2)
  dev.off()

} else if (requireNamespace("Cairo", quietly = TRUE)) {
  Cairo::CairoPNG(out_file, width = 2000, height = 2000, res = 300)
  grid::grid.draw(glist2)
  dev.off()

} else {
  # last resort: base png() — may fail on headless systems
  png(out_file, width = 2000, height = 2000, res = 300)
  grid::grid.draw(glist2)
  dev.off()
}

