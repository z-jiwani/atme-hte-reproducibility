# HTE analysis of digital well-being training for healthcare professionals

Reproducible code and de-identified data for a secondary heterogeneous-treatment-effect
analysis of a randomized trial of a 13-week digital well-being training among Mexican
healthcare professionals (N = 2,315; ClinicalTrials.gov NCT05767970). Requires R and Quarto.

```bash
quarto render atme_hte_secondary_analysis_script.qmd   # full analysis -> .html
```

`atme_hte_deid.csv` is a de-identified extract of the trial dataset, limited to the
variables used in these analyses. The source data are restricted and are not public.
