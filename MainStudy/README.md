# Optima Prostate Cancer TTE: Target Trial Emulation

This repository contains the analysis code for the study:

“Comparative effectiveness and safety in patients with prostate cancer who received radical prostatectomy vs. radiotherapy: target trial emulation.”

## Steps

1.  Make sure to open the **MainStudy** project in RStudio.

2.  Restore packages from `renv.lock`: with `renv::restore()`.

3.  Restart the R session.

4.  Open CodeToRun.R, fill in the required fields, and run the script.

5.  Set flags as needed:

    -   `createCohorts <- TRUE` to instantiate and characterise the study cohorts
    -   `runModel <- TRUE` to run the model

6.  When finished, a ZIP file containing the result files will be created in the **Results** folder. Share the zipped folder when done.

7.  OPTIONAL: Visualize Results in Shiny

    -   Navigate to the `shiny-MainStudy` folder and open the project file `shiny.Rproj` in RStudio.
    -   You should see the project name in the top-right corner of your RStudio session.
    -   Copy the generated result files (in .csv format) into the `data` folder located within the `shiny-MainStudy` folder.
    -   Open the `global.R` script in the `shiny` folder.
    -   Click the *Run App* button in RStudio to launch the local Shiny app for interactive exploration of the results.
