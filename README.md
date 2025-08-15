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

The forced swim test (FST) assesses antidepressant activity in rodents by measuring suppression of immobility. This study reviewed the literature to evaluate how experimental conditions, study quality, and bias influence antidepressant efficacy in the FST (PROSPERO: CRD42020200604). Systematic searches in Embase and MEDLINE (PubMed) identified 8247 relevant records. After being screened by two independent reviewers, 2588 records were included in the library. A random sample (k = 200) yielded 561 studies for meta-analysis. One reviewer extracted data, double-checked by a second; discrepancies were resolved by a third. Meta-analyses were conducted using a random-effects model (metafor R package) to estimate combined effect size (CES), 95% confidence intervals (CI), heterogeneity, and publication bias. Risk of bias was assessed via SYRCLE’s tool and the CAMARADES checklist. Despite high inconsistency (I² = 81.5%), the global CES was large and significant [Hedges’ g = 1.66, 95% CI (1.53; 1.79), k = 561, power > 80%], consistent across most subgroups. Small study effects and publication bias inflated CES estimates, especially in mice, while results in rats were more variable. Nonetheless, antidepressants consistently reduced immobility in mice across diverse conditions. In rats, findings were less consistent, though the most robust data showed a significant, dose-dependent antidepressant-like effect of imipramine in both species. However, publication bias and incomplete reporting compromise the accuracy of CES estimates and raise concerns about the validity of the FST literature. These findings highlight the need for more transparent reporting practices in FST-based antidepressant research.

Keywords: forced swim test, immobility, meta-analysis, publication bias, rat, mouse, study quality

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
