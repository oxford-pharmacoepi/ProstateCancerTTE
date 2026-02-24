# Prostate Cancer TTe 

## Treatment cohorts

This Shiny App displays the characterisation of two prostate cancer treatment cohorts:

-   **RT** (Radiotherapy)
-   **RP** (Radical Prostatectomy)

### Index date

Index date is the date of the **first recorded treatment**

## Trial Cohorts

### Inclusion Criteria

Subjects included in these cohorts meet the following criteria:

1.  **Prostate cancer diagnosis**: Males diagnosed with prostate cancer between the ages of 50 and 69, with the diagnosis recorded in the year prior to cohort entry
2.  **Hormonal therapy exclusions**:
    -   For Radical Prostatectomy: No prior use of hormones

    -   For EBRT: No ADT or antiandrogens recorded more than 6 months before treatment and no other hormones before the treatment.
3.  **PSA levels**: Last Prostate-Specific Antigen (PSA) value between 3.0 and 19.99 ng/mL within 180 days prior to cohort entry
4.  **Cancer stage**: Evidence of early-stage prostate cancer (CT1–T2 and M0) in prior clinical history
5.  **Advanced stage exclusion**: No evidence of **advanced-stage** prostate cancer (**CT3–T4 and M1**) prior to treatment
6.  **Other malignancies**: No prior history of cancer, except non-melanoma skin cancer
7.  **Comorbidities**: No diagnosis of COPD, heart failure, stroke, or myocardial infarction in the year before cohort entry
8.  **Renal health**: No history of kidney dialysis or transplantation
9.  **Orthopedic procedures**: No prior bilateral hip replacement
10. Exclusion of subjects with specific female condition records and duplicate treatment entries on the same day




## RWD Cohorts

### Inclusion Criteria

Subjects included in these cohorts meet the following criteria:

1.  One year of **prior history**
2.  Condition of **prostate cancer** any time prior index date
3.  Evidence of **early-stage** prostate cancer (CT1–T2 and M0) in the six months prior index date
4.  **No** evidence of **advanced-stage** prostate cancer (CT3–T4 and M1) before treatment
5.  Exclusion of subjects with specific female condition records and duplicate treatment entries on the same day

### Additional Cohort Versions

Cohorts were further stratified by age group:

-   50–69 years

-   70 years and older

## Lasso Regression

Variables included in the Lasso regression:

-   Any conditions any time prior to index date

-   Any drugs in the year prior to index date 

-   Number of inpatient and office visits

-   Age at index date

-   Year

## Matching

Exact match by

-   Age, within a caliper of 2 years.
-   Year group: \<2010, 2010-2014, 2015-2019, 2020+.
-   Latest n status: n0, nx, n1, n2, n3, missing.
-   Latest t status: t1, t2, missing.
-   Latest gleason score: \<2, 2-6, 7, 8-10, \>10, missing.
-   Latest psa value: \<3, 3-19.99, 20-39.99, ≥40, missing.
-   Source for merged cohorts: Gold or Aurum

## Outcomes

### Outcomes included in the analysis

-   All-cause death
-   Death due to prostate cancer
-   Death due to cardiovascular disease
-   Acute myocardial infarction
-   Angina
-   Anxiety
-   Atherosclerosis
-   Deep vein thrombosis
-   Depression
-   Erectile dysfunction
-   Any fracture\
-   Osteoporotic fracture
-   Hypertension
-   Incontinence
-   Rectal, bladder, bowel, or ureteric injury
-   Pulmonary embolism
-   Androgen deprivation therapy
-   Metastasis
-   Type 2 diabetes
-   Ischemic stroke

::: {style="display: flex; justify-content: center; align-items: center; height: 50vh;"}
<img src="CopyOfoptima.png" style="max-width: 250px; height: auto;"/>
:::
