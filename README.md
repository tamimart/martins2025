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

The forced swim test (FST) is a preclinical assay that assesses antidepressant activity by measuring suppression of immobility in rodents. This study systematically reviewed and meta-analyzed the influence of experimental conditions, study quality, and bias on antidepressant efficacy in the FST (PROSPERO: CRD42020200604). Searches across four databases (Embase, PubMed, Scopus, Web of Science) retrieved 14,719 publications, reduced to 2,588 relevant studies after screening. From these, 200 publications were randomly selected for quality and risk of bias assessments, providing sufficient data for a well-powered meta-analysis. A random-effects model addressed high heterogeneity among studies. Using the metafor R package, the standardized mean difference (SMD) was calculated and merged into a combined effect size (CES) with a 95% confidence interval (CI). A total of 561 relevant studies exceeded the number required for 80% analytical power. Despite high inconsistency (I² = 81.5%), the global CES was very large and statistically significant (Hedges g = 1.66, 95% CI [1.53; 1.79], k = 561), favouring antidepressant treatment. Subgroup analyses also showed large, significant effects. In summary, antidepressants consistently reduced the immobility of rats and mice in the FST across various conditions.Publication and small-study bias inflated the CES in global and mouse subgroup analyses. However, publication and reporting bias jeopardise the accuracy of CES estimation and preclude the appraisal of internal and external validities of this literature. Results underscore the need for improved reporting in publications using the FST. Moreover, the present review indicates that experimental conditions and qualities influence FST outcomes in a species-specific fashion.![image](https://github.com/user-attachments/assets/7f91bba1-0b55-401e-869d-0ff522e0c573)

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
