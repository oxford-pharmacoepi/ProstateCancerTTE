---
editor_options: 
  markdown: 
    wrap: sentence
---

# Prostate Cancer Study

The repo is organised as follows: - `Codelist` contains the different codelists used in the study.

-   `Cohorts` contains the code to instantiate the cohort of the study.
-   `Diagnostics` contains the diagnostic code.
-   `DatabaseCharacteristics` contains the code to characterise the database.
-   `MainStudy` contains the code to conduct the Target Trial Emulation study.
-   `DML` contains the code to conduct the Double Machine Learning study.
-   `CloneCensorWeight` contains the code to conduct the Cloning-Censoring-Weighting study.

## Instructions to run

### Diagnostics

The repository runs the **PhenotypeR** package for the Optima study: “Comparative effectiveness and safety in patients with prostate cancer who received radical prostatectomy vs. radiotherapy: target trial emulation.”

#### Steps

1.  Make sure to open the **Diagnostics** project in RStudio.

2.  Open the **Diagnostics** folder.

3.  Restore packages from `renv.lock`: with `renv::restore()`.

4.  Restart the R session.

5.  Open CodeToRun.R, fill in the required fields, and run the script.

6.  When finished, a results .csv file will be created in the **Results** folder.
    Share the .csv file when done.

### Main Study

### DML

### CCW
