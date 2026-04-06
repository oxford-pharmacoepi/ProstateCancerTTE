
char <- list(
  outcomes = characterisation(cdm$outcomes),
  treatments = characterisation(cdm$treatments),
  prostate_cancer = characterisation(cdm$prostate_cancer)
) |>
  bind()
