

# Comparative effectiveness and safety in patients with prostate cancer who received radical prostatectomy vs. radiotherapy: target trial emulation.

The repo is organised as follows: 

- `Codelist` contains the different codelists used in the study.
-   `Cohorts` contains the code to instantiate the cohort of the study.
-   `Diagnostics` contains the PhenotypeR diagnostics code.
-   `DatabaseCharacteristics` contains the code to characterise the database (CPRD)
-   `MainStudy` contains the code to conduct the Target Trial Emulation study.
-   `DML` contains the code to conduct the Double Machine Learning study.
-   `CloneCensorWeight` contains the code to conduct the Cloning-Censoring-Weighting study.

## Instructions to run

### Diagnostics

The repository runs the **PhenotypeR** package for the Optima study: “Comparative effectiveness and safety in patients with prostate cancer who received radical prostatectomy vs. radiotherapy: target trial emulation.”

#### Steps

1.  Open the **Diagnostics** folder.
   
2.  Make sure to open the **Diagnostics** project in RStudio.

4.  Restore packages from `renv.lock`: with `renv::restore()`.

5.  Restart the R session.

6.  Open CodeToRun.R, fill in the required fields, and run the script.

7.  When finished, a results .csv file will be created in the **Results** folder.
    Share the .csv file when done.
8. OPTIONAL: Visualize Results in a Shiny

    -   Navigate to the `shiny-Diagnostics` folder and open the project file `shiny.Rproj` in RStudio.
    -   You should see the project name in the top-right corner of your RStudio session.
    -   Copy the generated result files (in .csv format) into the `data/raw` folder located within the `shiny-Diagnositcs` folder.
    -   Open the `global.R` script in the `shiny` folder.
    -   Click the *Run App* button in RStudio to launch the local Shiny app for interactive exploration of the results.


### Main Study

### DML

### CCW
