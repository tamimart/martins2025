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

The forced swim test (FST) entails observing the suppression of immobility as a predictor of the antidepressant activity of substances in laboratory rodents. The present study investigated the literature to verify the influence of experimental conditions, study quality, and risk of bias on the efficacy of antidepressants in the FST (protocol PROSPERO: CRD42020200604). The search of Embase and MEDLINE (PubMed) resulted in 2,588 relevant publications following deduplication and screening by two independent reviewers. A sample of publications (k = 200) was randomly selected for analysis, providing 561 relevant studies. A reviewer extracted and a second one double-checked the data. A third reviewer conciliated discrepancies. Calculations of the combined effect size (CES), 95% confidence interval (95% CI), publication bias, inconsistency, and heterogeneity were with random-effects model (metafor R package). The risk of bias appraisal used SYRCLE’s RoB tool and CAMARADES checklist. Despite substantial inconsistency (I² = 81.5%), global CES was positive, very large, and significant (Hedges g = 1.66, 95% CI [1.53; 1.79], k = 561, power > 80 %). Similar to results estimated for most subgroups. Small studies’ effect and publication bias inflated the CES in the global meta-analyses and in the mouse subgroup. In rats, the estimates were inconclusive. In summary, antidepressants consistently reduced the immobility of mice in the FST across various experimental conditions. In rats, results seem less consistent across studies. However, publication bias and incomplete reporting jeopardise the accuracy of CES estimation and the appraisal of validities of this literature. Results underscore the need for improved reporting in publications using the FST.

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

-   **4_exploratory_extreme_es**: This script conducts an exploratory analysis of studies with extreme effect sizes and standard deviations to assess how these studies represent the   oader dataset, focusing on species, publication ID, and specific study features. Plots and summaries reveal patterns in extreme values across variables, including population,  intervention, outcome, and validity metrics. This analysis is not included in the paper but is provided as a supplementary .docx file in the same folder.

...
