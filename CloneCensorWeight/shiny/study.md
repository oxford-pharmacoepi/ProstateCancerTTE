### Details

**Title:** [Cloning-Censor-Weighting study](link)

**Authors:** Martí Catalá<sup>1</sup>

**Affiliations:** <sup>1</sup>Health Data Sciences, NDORMS, University of Oxford.

**Corresponding:** [marti.catalasabate\@ndorms.ox.ac.uk](mailto:marti.catalasabate@ndorms.ox.ac.uk)

<div style="text-align: center;">
  <img src="hds_logo.svg" alt="HDS Logo" height="70" style="vertical-align: middle; margin-right: 10px;">
  <img src="oxford.png" alt="Oxford Logo" height="70" style="vertical-align: middle;">
</div>

### Abstract

### Methods

**Cohort definition**

We conducted a cloning-censor-weighting study[CITE], we defined 3 cohorts: surveillance, prostatectomy and radiotheraphy. Index date is defined as the first occurrence of prostate cancer ever. Only records with at least 365 days of prior observation are included. Individuals need to have no record of prostatectomy or radiotheraphy before the index date. Individuals in surveillance and prostatectomy cohorts are not allowed to have a record of radiotheraphy on the index date. Individuals in surveillance and radiotheraphy cohorts are not allowed to have a record of prostatectomy on the index date. Counts and Attrition of each cohort can be seen in the [Cohort tab]().

**Follow-up**

All cohorts are censored at death or end of observation. Surveillance cohort is censored if they have an occurrence of *radiotherapy* or *prostatectomy*. Radiotherapy cohort is censored if they have an occurrence of *prostatectomy* or after 365 if no occurrence of *radiotherapy* is recorded. Prostatectomy cohort is censored if they have an occurrence of *radiotherapy* or after 365 if no occurrence of *prostatectomy* is recorded. Outcomes events are observed if they occur before or at the date of censoring. The follow-up details can be observed in Figure 1, individual 1 has an occurrence of radiotheraphy after the 365 days, so it is censored from prostatectomy and radiotheraphy cohorts at 365 days, it is censored from the surveillance cohort at the radiotheraphy date. Individual 2 has the outcome before 365 days and no treatment record, so it contributes the same to the three cohorts. Individual 3 has first a prostatectomy record and the the outcome of interest, this individual is censored from surveillance and radiotheraphy cohorts at the prostatectomy occurrence and the outcome is only seen for the clone in the prostatectomy cohort. Individual 4 has an occurrence of radiotheraphy and then an occurrence of prostatectomy, the clones in the prostatectomy and surveillance cohort are censored at the radiotheraphy occurrence, whereas the clone in the radiotheraphy cohort is censored at the prostatectomy occurrence. Individual 5 has a prostatectomy record and then the outcome of interest the clones in surveillance and radiotheraphy cohorts are censored at prostatectomy occurrence and then the outcome is only observed for the clone in the prostatectomy cohort. Individual 6 has an outcome after 365 days and no treatment record so the clones in the treatments arms are censored at 365 days and the outcome is only seen for the surveillance clone.

<div style="text-align: center;">
  <div style="display: inline-block; text-align: center;">
    <img src="diagram.png" alt="Diagram" height="400" style="vertical-align: middle;">
    <div style="text-align: center; font-style: italic; margin-top: 8px; max-width: 600px;">
      <strong>Figure 1.</strong> Study design. Index date for the three cohorts is the first diagnostics
      of prostate cancer. All cohorts are censored at death/end observation*. Surveillance cohort is 
      censored if they have an occurrence of *radiotherapy* or *prostatectomy*. Radiotherapy cohort is 
      censored if they have an occurrence of *prostatectomy* or after 365 if no occurrence of *radiotherapy*
      is recorded. Prostatectomy cohort is censored if they have an occurrence of *radiotherapy* or after 
      365 if no occurrence of *prostatectomy* is recorded. Outcomes events are observed if they occur before
      or at the date of censoring.
    </div>
  </div>
</div>

**Codelists**

The different codelists used to identify each occurrence are defined in the [Codelists tab](). In that tab we can see the definition for each 

**PS calculation**

Large Scale Propensity Scores (PS) are calculated using all conditions before index date (day of weight calculation) and all medications in the 365 days prior index date. Only covariates with a frequency higher than 0.5% in the overall cohort are included. We used lasso regression for feature selection. PS are calculated using a multinomial logistic regression. Weight of each individual is calculated using the Overlap Weighting (OW) formula [CITE]:

<center>

W_{surveillance} = 1 - P(surveillance|X)

W_{radiotherapy} = 1 - P(radiotherapy|X)

W_{prostatectomy} = 1 - P(prostatectomy|X)

</center>

Where W_T is the weights in the *T* arm cohort and P(T|X) is the PS probability to be in *T* arm cohort given the covariates at index date (X). Weights are re-calculated every 30 days, to account for the censoring of the different individuals.

- Should we consider the events that occur after the index date for weight calculation?
- Should weights be re-calculated when censoring only for outcome?

### Results

### Conclusions
