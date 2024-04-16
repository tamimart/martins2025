<!--
---
editor_options: 
  markdown: 
    wrap: 72
---
-->

## Title

This project contains the data and scripts from the publication
[**reference**].

### Abstract

The non-clinical assay forced swim test (FST) entails observing the suppression of immobility as a predictor of the antidepressant activity of substances in laboratory rodents. The present work investigates the influence of experimental conditions, study quality and risk of bias on the efficacy of antidepressants in the FST. For that, a systematic review and meta-analysis were conducted according to the protocol (PROSPERO: CRD42020200604). The search strategy in four databases (Embase, Pubmed, Scopus, Web of Science) retrieved 14,719 references, downsized to 5,137 unique references after deduplication and 2,588 relevant publications following the screening by two independent reviewers. A sample of publications (k = 200) was randomly selected for quality and risk of bias assessments and to guarantee a minimum number of studies to a well-powered meta-analysis. Some publications had more than one independent experiment comprising 561 relevant studies, exceeding the number required for the analytical power of 80%. One reviewer extracted information from the full texts, a second reviewer verified them, and a third one conciliated discrepancy. The standardised mean difference was calculated for each relevant study and merged into a combined effect size (CES) with a 95% confidence interval (95% CI). Anticipating high heterogeneity between studies, the random-effects model was used to estimate CES in global or stratified meta-analysis, publication bias, consistency and heterogeneity. The metafor R package was used for calculations and plots. SYRCLE’s RoB tool and CAMARADES checklist detected unclear risk of bias in most publications. Despite substantial inconsistency (I² = 81.5%), the global CES was very large and statistically significant, favouring antidepressant treatment (Hedges g = 1.66; 95% CI [1.53; 1.79]; k = 561). For most of the subgroups, CES was large, significant and positive. Small studies and publication bias inflated the CES in the global and subgroup mice meta-analyses. The high risk of bias seems to exacerbate CES in mice; but in rats, results were inconclusive. In summary, antidepressants consistently reduced the immobility of rats and mice in FST across various conditions. However, publication bias and unclear risk of bias in most studies jeopardise the appraisal of internal and external validities of this literature. This review underscores the need for improved reports in publications using FST. Moreover, the present review indicates that experimental conditions and quality influence FST outcomes in a species-specific fashion. 

Keywords: Immobility. Study quality. Publication bias. Forced swim test. Meta-analysis 

### How to use it

Run the scripts in order:

-   **1_wrangling**: The script includes code aimed at cleaning and
    standardizing data spreadsheets. Its functions involve consolidating
    multiple tabs into a single dataframe, converting each variable into
    its appropriate data type, standardizing categorical levels for
    consistency, and finally saving the processed data to the same
    folder for future use.

    *input*: DataExtraction_Raw

    *output*: Dataclean_200FST

-   **2_agreement**: The script contains code to calculate Cohen's kappa
    and agreement between reviewers during the selection stage (in two
    ways: inclusion decision and exclusion reason), as well as during
    data extraction, for both qualitative and quantitative data.

-   **3_analysis**: The script includes the code for all analyses, using
    the Dataclean_200FST file as *input*. This script runs power
    calculation, meta-analysis, sensitivity analysis, publication bias
    analysis, stratified and metaregression analysis, and finally plots
    the figures.

...
