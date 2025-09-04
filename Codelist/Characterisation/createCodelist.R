ajcc_n0_codes <- c(
  1633440,
  1635023,
  1633720,
  1634780,
  1635600,
  1633938,
  1633621,
  1635244,
  1634145,
  1635597,
  1633686,
  1633527
)

ajcc_n1_codes <- c(
  1634434,
  1634448,
  1635103,
  1633688,
  1633735,
  1635130,
  1634620,
  1635335,
  1635729,
  1635613,
  1634321,
  1634788
)

ajcc_nx_codes <- c(
  1633885,
  1634163,
  1635816,
  1634216,
  1634722,
  1635170,
  1634356,
  1635503
)

ajcc_n2_codes <- c(
  1634119,
  1635645,
  1634240,
  1635585,
  1635644,
  1634134,
  1634080,
  1633512,
  1634523,
  1633864,
  1633564,
  1635061
)

ajcc_n3_codes <- c(
  1635320,
  1634563,
  1633684,
  1634147,
  1635590,
  1633422,
  1634735,
  1633914,
  1635706,
  1634102,
  1635094
)


x <- CodelistGenerator::getDescendants(cdm = cdm, conceptId = ajcc_n0_codes)
n0 <- omopgenerics::newCodelist(list(n0 = x$concept_id))
omopgenerics::exportCodelist(n0, path = here::here("Codelist/Characterisation"), type = "csv")

y <- CodelistGenerator::getDescendants(cdm = cdm, conceptId = ajcc_n1_codes)
n1 <- omopgenerics::newCodelist(list(n1 = y$concept_id))
omopgenerics::exportCodelist(n1, path = here::here("Codelist/Characterisation"), type = "csv")


z <- CodelistGenerator::getDescendants(cdm = cdm, conceptId = ajcc_nx_codes)
nx <- omopgenerics::newCodelist(list(nx = z$concept_id))
omopgenerics::exportCodelist(nx, path = here::here("Codelist/Characterisation"), type = "csv")


t <- CodelistGenerator::getDescendants(cdm = cdm, conceptId = ajcc_n2_codes)
n2 <- omopgenerics::newCodelist(list(n2 = t$concept_id))
omopgenerics::exportCodelist(n2, path = here::here("Codelist/Characterisation"), type = "csv")



p <- CodelistGenerator::getDescendants(cdm = cdm, conceptId = ajcc_n3_codes)
n3 <- omopgenerics::newCodelist(list(n3 = p$concept_id))
omopgenerics::exportCodelist(n3, path = here::here("Codelist/Characterisation"), type = "csv")


x <- CodelistGenerator::getDescendants(cdm = cdm, conceptId = 134057)
cvd <- omopgenerics::newCodelist(list(cvd = x$concept_id))
omopgenerics::exportCodelist(cvd, path = here::here("Codelist/Characterisation"), type = "csv")

