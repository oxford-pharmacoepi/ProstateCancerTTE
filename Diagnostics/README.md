# Optima Prostate Cancer TTE: Diagnostics

This repository runs the **PhenotypeR** package for the Optima study: “Comparative effectiveness and safety in patients with prostate cancer who received radical prostatectomy vs. radiotherapy: target trial emulation.”

## Steps

1. Make sure to open the **Diagnostics** project in RStudio.

2. Restore packages from `renv.lock`: with `renv::restore()`.

3. Restart the R session.

4. Open CodeToRun.R, fill in the required fields, and run the script.

5. When finished, a results .csv file will be created in the **Results** folder. Share the .csv file when done.

6. OPTIONAL: Visualize Results in Shiny

    -   Navigate to the `shiny-Diagnostics` folder and open the project file `shiny.Rproj` in RStudio.
    -   You should see the project name in the top-right corner of your RStudio session.
    -   Copy the generated result files (in .csv format) into the `data/raw` folder located within the `shiny-Diagnositcs` folder.
    -   Open the `global.R` script in the `shiny` folder.
    -   Click the *Run App* button in RStudio to launch the local Shiny app for interactive exploration of the results.
