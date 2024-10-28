<!--
---
editor_options: 
  markdown: 
    wrap: 72
---
-->

## Antidepressant effect or bias? Systematic review and meta-analysis of studies using the forced swimming test

This project contains the data and scripts from the publication
[**reference**].

### Abstract

The forced swim test (FST), a non-clinical assay, entails observing the suppression of immobility as a predictor of the antidepressant activity of substances in laboratory rodents. The present work investigated the influence of experimental conditions, study quality, and risk of bias on the efficacy of antidepressants in the FST. A systematic review and meta-analysis were conducted according to the protocol (PROSPERO: CRD42020200604). The search strategy spanned four databases (Embase, Pubmed, Scopus, Web of Science). It retrieved 14,719 publications, resulting in 5,137 after deduplication and 2,588 relevant publications following screening by two independent reviewers. A sample of publications (k = 200) was randomly selected for quality and risk of bias assessments and to guarantee a minimum number of studies for a well-powered meta-analysis. Due to anticipated high heterogeneity between studies, the random-effects model was chosen to estimate the CES, publication bias, consistency, and heterogeneity. The metafor R package was used for calculations and most plots, SYRCLE’s RoB tool and CAMARADES checklist were applied to the risk of bias appraisal. Some publications had more than one independent experiment, resulting in 561 relevant studies, exceeding the number required for an analytical power of 80%. One reviewer extracted information from the full texts, a second reviewer verified them, and a third one conciliated discrepancy. The standardised mean difference was calculated for each relevant study and merged into a combined effect size (CES) with a 95% confidence interval (95% CI). Despite substantial inconsistency (I² = 81.5%), the global CES was very large and statistically significant, favouring antidepressant treatment (Hedges g = 1.66, 95% CI [1.53; 1.79], k = 561). For most subgroups, CES was large, significant, and positive. Small studies and publication bias inflated the CES in the global and mouse subgroup meta-analyses. The high risk of bias seems to inflate the CES in data of mice. In rats, the results were inconclusive. In summary, antidepressants consistently reduced the immobility of rats and mice in the FST across various conditions. However, publication and reporting bias jeopardise the accuracy of CES estimation and preclude the appraisal of internal and external validities of this literature. Results underscore the need for improved reporting in publications using the FST. Moreover, the present review indicates that experimental conditions and qualities influence FST outcomes in a species-specific fashion.

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

-   **4_exploratory_extreme_es**: This script conducts an exploratory analysis of studies with extreme effect sizes and standard deviations to assess how these studies represent the     
    broader dataset, focusing on species, publication ID, and specific study features. Plots and summaries reveal patterns in extreme values across variables, including population, 
    intervention, outcome, and validity metrics. This analysis is not included in the paper but is provided as a supplementary .docx file in the same folder.

...
