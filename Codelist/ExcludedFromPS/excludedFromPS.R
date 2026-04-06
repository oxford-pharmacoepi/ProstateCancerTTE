excluded_conditions <-  list(
  `Hormone therapy` = 4061650,
  `Seen in oncology clinic` = 4083427,
  `Implied consent for core Summary Care Record dataset upload` = 44805213,
  `Transurethral biopsy of prostate` = 4071781,
  `Primary malignant neoplasm of prostate` = 200962,
  `Depression screening using questions` = 44788755,
  `Informed consent for procedure` = 4082261,
  `Preparation for intensity modulated radiation therapy` = 44793209,
  `Injection` = 4241075,
  `Choice and booking enhanced services administration` = 44790074,
  `goserelin 3.6 MG Drug Implant [Zoladex] by Astrazeneca` = 36935860,
  `Echography of prostate, transrectal approach` = 4317528,
  `Leuprolide 11.3 MG Injectable Suspension [Prostap] by Takeda` = 35774257,
  `Subcutaneous injection of hormone antagonist` = 4193579,
  `bicalutamide 50 MG Oral Tablet` = 1344403,
  `Fast track referral for suspected urological cancer` = 44792638,
  `Goserelin 10.8 MG Drug Implant [Zoladex LA] by Astrazeneca` = 21089544,
  `Goserelin 3.6 MG Drug Implant [Zoladex] by Astrazeneca` = 21128764,
  `Injection into subcutaneous tissue` = 37151637,
  `Discharged from hospital` = 4084843,
  `Seen in urology clinic` = 4083414,
  `Number of neoplasms in excised tissue specimen` = 4078513,
  `Transperineal needle biopsy of prostate` = 4142388
) |> unlist() |> unname()

excluded_conditions<- omopgenerics::newCodelist(list(excluded_conditions = excluded_conditions))


drug_names <- c(
  "abarelix",
  "degarelix",
  "relugolix",
  "buserelin",
  "leuprolide",
  "goserelin",
  "triptorelin",
  "histrelin",
  "cyproterone",
  "megestrol",
  "medroxyprogesterone",
  "flutamide",
  "nilutamide",
  "bicalutamide",
  "abiraterone",
  "tamoxifen"
)
hormones <- CodelistGenerator::getCandidateCodes(cdm = cdm_a, keywords = drug_names, domains = "Drug")

rectal_solutions <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm,
                                          name = "sodium citrate",
                                          doseForm = "Rectal Solution")

lidocaine_injectable <- CodelistGenerator::getDrugIngredientCodes(cdm = cdm_a,
                                                                  name = "lidocaine",
                                                                  doseForm = "Injectable Solution")

excluded_drugs <- omopgenerics::newCodelist(list(hormones = hormones$concept_id,
                                                            rectal_solutions = rectal_solutions[[1]],
                                                            lidocaine = lidocaine_injectable[[1]]))

omopgenerics::exportCodelist(excluded_conditions, path = "~/ProstateCancerTTE/Codelist/ExcludedFromPS", type = "csv" )

omopgenerics::exportCodelist(excluded_drugs, path = "~/ProstateCancerTTE/Codelist/ExcludedFromPS", type = "csv" )

docusate <- CodelistGenerator::getCandidateCodes(
  cdm,
  keywords = "docusate",
  exclude = c("otic", "topical", "ear"),
  domains = "Drug",
  standardConcept = "Standard")

polyethilene_glycol <- CodelistGenerator::getDrugIngredientCodes(
  cdm,
  name = c(986417, 36808745, 40707795),
  type = "codelist",
  nameStyle = "{concept_name}"
  )
omopgenerics::exportCodelist(polyethilene_glycol, path = "~/ProstateCancerTTE/Codelist/ExcludedFromPS", type = "csv" )

