# Load packages
library(tidyverse)      # Data wrangling
library(bitops)         # Bitwise operations
library(metafor)        # Meta-analysis
library(Formula)        # Model formulae
library(readxl)         # Read Excel files
library(writexl)        # Save Excel files
library(extrafont)      # Additional fonts
library(cowplot)        # Plot annotation and alignment
library(metapower)      # Power calculation for meta-analysis
library(lubridate)      # Date manipulation
library(weightr)        # Test publication bias
library(patchwork)      # Combine plots
library(countrycode)    # Get countries' continents

# Import fonts (execute once)
font_import(paths = "C:/Windows/Fonts")

# POWER CALCULATION----

# Function to convert Hedges' g to Cohen's d
hedgesg_to_cohensd <- function(hedgesg, n) {
  cohensd <- hedgesg / (1 - 3 / (4 * (n) - 9))
  return(cohensd)
}

# Calculate global meta-analysis power
# Convert hedges g para cohens d
hedgesg_to_cohensd(hedgesg = 0.5, n = 12) # converter hedges g para cohens d
# Calculate power
global_power <- mpower(effect_size = .54166667, study_size = 6, k = 200, i2 = .90, es_type = "d") 
# Display power
print(global_power)
# Plot power
plot_mpower(global_power)

# DATA TREATMENT ----

# Import data from Excel
df <- read_excel("data/Dataclean_200FST.xlsx") 

# Change date type to numeric
df <- df  |>  
  mutate(year = as.numeric(format(as.Date(df$year, format = "%d/%m/%Y"),"%Y"))) 

# DESCRIPTION OF INCLUDED PUBLICATIONS ------

# year
df |> 
  summarise(min = min(year),
            max = max(year))

# continents
df$continent <- countrycode(sourcevar = df$country,
                            origin = "country.name",
                            destination = "continent")
df |> 
  group_by(id) |>
  slice(1) |> 
  group_by(continent) |> 
  count()

# n studies
df |> 
  group_by(id) |> 
  summarize(n_lines = n()) |> 
  arrange(desc(n_lines)) |> 
  filter(n_lines > 1)


# species
df |> 
  group_by(id) |>
  slice(1) |>
  group_by(species) |> 
  count()

# strain

levels(as.factor(df$strain))

df |> 
  group_by(id, strain) |>
  slice(1) |>
  ungroup() |> 
  filter(strain == "NA") |> 
  count() 

df |> 
  filter(species == "mice") |> 
  group_by(id, strain) |> 
  slice(1) |> 
  group_by(strain) |> 
  count() |> 
  arrange(desc(n))

df |> 
  filter(species == "rat") |> 
  group_by(id, strain) |> 
  slice(1) |> 
  group_by(strain) |> 
  count() |> 
  arrange(desc(n))


# sex
df |> 
  group_by(id, sex) |>
  slice(1) |>
  group_by(sex) |> 
  count() |> 
  arrange(desc(n))

# age
df |> 
  group_by(species) |> 
  summarise(min_age = min(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE))

df |> 
  filter(species == "mice") |> 
  group_by(id, age) |>
  slice(1) |> 
  ungroup() |> 
  filter(!is.na(age)) |> 
  count()

df |> 
  filter(species == "rat") |> 
  group_by(id, age) |>
  slice(1) |> 
  ungroup() |> 
  filter(!is.na(age)) |> 
  count()

# weight
df |> 
  group_by(species) |> 
  summarise(min_weight = min(weight, na.rm = TRUE),
            max_weight = max(weight, na.rm = TRUE))

df |> 
  filter(species == "mice") |> 
  group_by(id, weight) |>
  slice(1) |> 
  ungroup() |> 
  filter(!is.na(weight)) |> 
  count()

df |> 
  filter(species == "rat") |> 
  group_by(id, weight) |>
  slice(1) |> 
  ungroup() |> 
  filter(!is.na(weight)) |> 
  count()


# stress
df |> 
  group_by(id, model_phenotype) |>
  slice(1) |>
  ungroup() |> 
  filter(model_phenotype != "NA") |> 
  summarise(counts = n())

df |> 
  group_by(id, model_phenotype) |>
  slice(1) |>
  ungroup() |> 
  filter(model_phenotype != "NA") |>
  group_by(model_phenotype) |> 
  count()

# classes by publication/species
df |> 
  group_by(id, atd_class) |>
  slice(1) |>
  ungroup() |> 
  group_by(species, atd_class) |>
  count() |> 
  arrange(desc(n)) 


# classes by publication
df |> 
  group_by(id, atd_class) |>
  slice(1) |>
  group_by(atd_class) |> 
  count() |> 
  arrange(desc(n)) 

# dose
median_doses <- df |> 
  filter(dose_unit == "mg/kg") |> 
  group_by(atd_type) |> 
  summarise(median_dose = median(dose, na.rm = TRUE),
            n = n())
  
median_doses |> 
  summarise(min = min(median_dose),
            max = max(median_dose))

median_doses |> 
  group_by(median_dose) |> 
  count()

df |> filter(is.na(dose))
df$dose_unit

df |> group_by(id, dose) |> slice(1) |> filter(dose == 10) 
df |> filter(dose == 10) 

df |> filter(atd_type %in% c("imipramine", "fluoxetine"),
             dose_unit == "mg/kg") |> 
  group_by(atd_type, species) |> 
  summarise(avg = mean(dose))

# classes by publication/species


# route/via

df |> 
  group_by(id, treatment_via) |>
  slice(1) |>
  filter(treatment_via == "NA")

df |> 
  group_by(id, treatment_via) |>
  slice(1) |>
  ungroup() |> 
  group_by(species, treatment_via) |> 
  count() |> 
  arrange(desc(n))

# fst protocol

df |> 
  group_by(id, fst_protocol) |>
  slice(1) |>
  filter(fst_protocol == "NA")

df |> 
  group_by(id, fst_protocol) |>
  slice(1) |>
  ungroup() |> 
  group_by(species, fst_protocol) |> 
  count() |> 
  arrange(desc(n))

df |>  group_by(id, species) |>  slice(1) |>  filter(species == "mice") |>  count()

# fst outcome analysis
df |> 
  group_by(id, measurement_method) |>
  slice(1) |>
  ungroup() |> 
  group_by(species, measurement_method) |> 
  count() |> 
  arrange(desc(n))

# quality/validity

df_rob <- df |> 
  mutate(Study = str_c(first_author, ", ", year)) |> 
  select(starts_with("rob"), Study) 

df_rob <- df_rob |> 
  distinct() # keep one line per publication

# Create a function to count levels from each variable form a df
count_levels <- function(df) {
  # list to store the result for each variable
  results <- list()
  
  # Loop through each column in the dataframe
  for (col_name in colnames(df)) {
    # Group by the current column and count the frequencies
    result <- df %>%
      group_by_at(col_name) %>%
      summarise(count = n())
    
    # Store the result in the list
    results[[col_name]] <- result
  }
  
  # Return the list of results
  return(results)
}

# Apply the function to each variable from de df

nlevel_per_questions <- count_levels(df_rob)

# df_rob <- df_rob |> 
#   select(starts_with("rob")) 
# 
# transposed_df_rob <- as.data.frame(t(df_rob))
# 
# nlevel_per_publication <- count_levels(transposed_df_rob)




# META ANALYSIS ----

# Calculate effect size in standardized mean difference (Hedges' g)
Efeito <- escalc(measure = "SMD", n1i = ctr_n_corr, n2i = atd_n_round, m1i = ctr_mean, m2i = atd_mean, 
                 sd1i = ctr_sd, sd2i = atd_sd, data = df, 
                 append = TRUE)

# Meta-analysis by random effects model 
Teste <- rma(yi, vi, data = Efeito, slab = (paste(Efeito$first_author, as.character(Efeito$year), sep = ", ")))

Teste

# Multivariate (nested by paper and comparison - 3 lvl) Meta-analysis by random effects model 
Teste_mv <- rma.mv(yi, vi, data = Efeito, random = ~ 1 | id / line, slab = (paste(Efeito$first_author, as.character(Efeito$year), sep = ", ")))

Teste_mv

orchaRd::i2_ml(Teste_mv)

# Generate confidence  and prediction interval
predict(Teste, digits = 3)


# Plot and save forestplot 
pdf("figure/forest.pdf", height = 120, width = 25)
# Create forestplot 
floresta <- forest(
  Teste,
  cex = 1,
  ylim = c(-2, 567),
  slab = (paste(
    Efeito$first_author, as.character(Efeito$year), sep = ", "
  )),
  order = Efeito$yi,
  xlab = "Hedges g",
  xlim =  c(-20, 60),
  showweight = T,
  cex.lab = 2,
  cex.axis = 1.5,
  col = "blue",
  border = "blue",
  fonts = "sans"
)

# Add annotations
op <- par(cex = 0.75, font = 2, family = "sans")
text(c(-6, 7.75), 568, font = 2, cex = 2.5, c("Favours control", "Favours antidepressants"))
text(53, 568,font = 2.5, cex = 2.5, c("Weights Hedges g [95% CI]"))
text(-16, 568, font = 2.5, cex = 2.5, c("Author(s), year"))
text(0, -20, pos = 4, cex = 4, bquote(paste("RE Model (g = ", .(formatC(Teste$b, digits = 2, format = "f")), ", Q = ", .(formatC(Teste$QE, digits = 2, format = "f")),
                                             ", df = ", .(Teste$k - Teste$p),
                                             ", p ", .(metafor:::.pval(Teste$QEp, digits = 2, showeq = TRUE, sep = " ")), "; ",
                                             I^2, " = ", .(formatC(Teste$I2, digits = 1, format = "f")), "%, ",
                                             tau^2, " = ", .(formatC(Teste$tau2, digits = 2, format = "f")), ")")))

dev.off() 

# SENSITIVITY ANALYSIS ----

# Identify studies influencing the meta-analysis in various aspects 
png("figure/influence.png")

inf <- influence(Teste)
 plot(inf)
dev.off()

# Create table with influence results
tinf <- print(inf) 
tinf$id <- Efeito$line # add column id
tinf$sr <- Efeito$study_reference # add reference column
write_xlsx(tinf,"data/influence.xlsx") # save as excel file

# Perform leave-one-out sensitivity analysis and save results
leave1 <- leave1out(Teste, digits = 3) # put results into an object
leave1df <- as.data.frame(leave1) # transf object list into df
final_df <- as.data.frame(t(leave1df)) # invert lines and columns
copia_final_df <- final_df # create copy
copia_final_df$rn <- row.names(final_df) # add row name as column in copied df
copia_final_df <- copia_final_df |>
  select(rn, everything()) # bring column of names to front of df

write_xlsx(copia_final_df, "data/leave.xlsx") # save

# PUBLICATION ANALYSIS ----

# Perform regression tests for publication bias
regtest(Teste, model = "rma", predictor = "sei")
regtest(Teste, model = "rma", predictor = "sqrtninv")

# Subset data and perform regression tests
Teste_mice <- rma(yi, vi, subset = (species == "mice"), data = Efeito)
regtest(Teste_mice, model = "rma", predictor = "sei")
regtest(Teste_mice, model = "rma", predictor = "sqrtninv")

Teste_rat <- rma(yi, vi, subset = (species == "rat"), data = Efeito)
regtest(Teste_rat, model = "rma", predictor = "sei")
regtest(Teste_rat, model = "rma", predictor = "sqrtninv")

Teste_noCP <- rma(yi, vi, subset = (positive_control == 0), data = Efeito)
regtest(Teste_noCP, model = "rma", predictor = "sei")
regtest(Teste_noCP, model = "rma", predictor = "sqrtninv")

# Trim and fill
missing <- metafor::trimfill(
    Teste,
    side = "left",
    estimator = "R0",
    maxiter = 100,
    verbose = FALSE
  ) #R0 preferable when the MA has >k. Reference: Rothstein HR, Sutton AJ, Borenstein M. Publication Bias in Meta-Analysis: Prevention, Assessment and Adjustments. Chichester, UK: John Wiley & Sons; 2005. An advantage of estimator "R0" is that it provides a test of the null hypothesis that the number of missing studies (on the chosen side) is zero

missing_m <- metafor::trimfill(
    Teste_mice,
    side = "left",
    estimator = "R0",
    maxiter = 100,
    verbose = FALSE
  )

missing_r <- metafor::trimfill(
    Teste_rat,
    side = "left",
    estimator = "R0",
    maxiter = 100,
    verbose = FALSE
  )

missing_noCP <- metafor::trimfill(
    Teste_noCP,
    side = "left",
    estimator = "R0",
    maxiter = 100,
    verbose = FALSE
  )

missing
missing_m
missing_r 
missing_noCP

# Funnel plot
# Plot and save 
png("figure/figure7.png", height = 1200, width = 800)
tiff("figure/figure7.tiff", height = 1200, width = 800, type = "cairo")

par(mfrow = c(4, 2), oma = c(1,1,1,1), mar = c(5,5,3,1), cex = .8, font = 2, family = "sans")

# Generate funnel plots for different subsets of data
funil_global1 <- metafor::funnel(
  missing,
  yaxis = "sei",
  addtau2 = FALSE,
  main = "Global",
  xlab = "Effect size (Hedges' g)",
  ylab = "Standard error",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "#82c236", "#009c7e"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(13,0), 
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  legend = "topright",
  offset = 0.1,
  ci.res = 1000,
  cex.lab = 1.7,
  cex.axis = 1.4,
  cex.main = 1.7
)

mtext("A", side = 3, cex = 1.5, line = 1, adj = -.15, font = 1)

funil_global2 <- metafor::funnel(
  missing,
  yaxis = "sqrtninv",
  addtau2 = FALSE,
  main = "Global",
  xlab = "Effect size (Hedges' g)",
  ylab = "1/√n",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "aquamarine", "aquamarine3"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(0.500,0.100),
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  ci.res = 1000,
  cex.lab = 1.7,
  cex.axis = 1.4,
  cex.main = 1.7
)
mtext("B", side = 3, cex = 1.5, line = 1, adj = -.15, font = 1)

funil_m1 <- metafor::funnel(
  missing_m,
  yaxis = "sei",
  addtau2 = FALSE,
  main = "Mice",
  xlab = "Effect size (Hedges' g)",
  ylab = "Standard error",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "#ff9400", "#FE7700"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(13,0), 
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  legend = "topright",
  ci.res = 1000,
  cex.lab = 1.7,
  cex.axis = 1.4,
  cex.main = 1.7
)
mtext("C", side = 3, cex = 1.5, line = 1, adj = -.15, font = 1)

funil_m2 <- metafor::funnel(
  missing_m,
  yaxis = "sqrtninv",
  addtau2 = FALSE,
  main = "Mice",
  xlab = "Effect size (Hedges' g)",
  ylab = "1/√n",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "#ff9400", "#FE7700"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(0.500,0.100), 
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  ci.res = 1000,
  cex.lab = 1.7,
  cex.axis = 1.4,
  cex.main = 1.7
)
mtext("D", side = 3, cex = 1.5, line = 1, adj = -.15, font = 1)

funil_r1 <- metafor::funnel(
  missing_r,
  yaxis = "sei",
  addtau2 = FALSE,
  main = "Rat",
  xlab = "Effect size (Hedges' g)",
  ylab = "Standard error",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "#ec2b2b", "#a6243a"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(13,0), 
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  legend = "topright",
  ci.res = 1000,
  cex.lab = 1.7,
  cex.axis = 1.4,
  cex.main = 1.7
)
mtext("E", side = 3, cex = 1.5, line = 1, adj = -.15, font = 1)

funil_r2 <- metafor::funnel(
  missing_r,
  yaxis = "sqrtninv",
  addtau2 = FALSE,
  main = "Rat",
  xlab = "Effect size (Hedges' g)",
  ylab = "1/√n",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "#a6243a", "#ec2b2b"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(0.500,0.100), 
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  ci.res = 1000,
  cex.lab = 1.7,
  cex.axis = 1.4,
  cex.main = 1.7
)
mtext("F", side = 3, cex = 1.5, line = 1, adj = -.15, font = 1)

funil_global_noCP1 <- metafor::funnel(
  missing_noCP,
  yaxis = "sei",
  addtau2 = FALSE,
  main = "Global without positive controls",
  xlab = "Effect size (Hedges' g)",
  ylab = "Standard error",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "grey80", "grey60"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(13,0), 
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  legend = "topright",
  offset = 0.1,
  ci.res = 1000,
  cex.lab = 1.7,
  cex.axis = 1.4,
  cex.main = 1.7
)
mtext("G", side = 3, cex = 1.5, line = 1, adj = -.15, font = 1)

funil_global_noCP2 <- metafor::funnel(
  missing_noCP,
  yaxis = "sqrtninv",
  addtau2 = FALSE,
  main = "Global without positive controls",
  xlab = "Effect size (Hedges' g)",
  ylab = "1/√n",
  back = "gray94",
  level = c(90, 95, 99),
  shade = c("white", "aquamarine", "aquamarine3"),
  hlines = "white",
  xlim = c(-60,60),
  ylim = c(0.500,0.100),
  lty = 2,
  pch = 19,
  pch.fill = 1,
  col = 25,
  label = "F",
  offset = 0.1,
  ci.res = 1000,
  cex.lab = 1.7,
  cex.axis = 1.4,
  cex.main = 1.7
)
mtext("H", side = 3, cex = 1.5, line = 1, adj = -.15, font = 1)

dev.off()

# [IGNORE] 
# plot for a poster presented on IBRO 
# 
# png("figure/funil_ibro.png", height = 1000, width = 2400)
# 
# par(mfrow = c(1, 2), oma = c(1,1,1,1), mar = c(4,5,3,1), cex = 2, font = 2, family = "sans")
# 
# funil_global1 <- metafor::funnel(
#   missing,
#   yaxis = "sei",
#   addtau2 = FALSE,
#   main = "Complete sample (CS)",
#   xlab = "Effect size",
#   ylab = "Standard error",
#   back = "gray94",
#   level = c(90, 95, 99),
#   shade = c("white", "#3b446f", "#05064f"),
#   hlines = "white",
#   xlim = c(-60,60),
#   ylim = c(13,0), 
#   lty = 2,
#   pch = 19,
#   pch.fill = 1,
#   col = 25,
#   label = "F",
#   legend = "topright",
#   offset = 0.1,
#   ci.res = 1000,
#   cex.lab = 1.7,
#   cex.axis = 1.4,
#   cex.main = 1.7
# )
# 
# funil_global_noCP1 <- metafor::funnel(
#   missing_noCP,
#   yaxis = "sei",
#   addtau2 = FALSE,
#   main = "Excluding positive controls (-PC)",
#   xlab = "Effect size",
#   ylab = "Standard error",
#   back = "gray94",
#   level = c(90, 95, 99),
#   shade = c("white", "#b5948e", "#883b32"),
#   hlines = "white",
#   xlim = c(-60,60),
#   ylim = c(13,0), 
#   lty = 2,
#   pch = 19,
#   pch.fill = 1,
#   col = 25,
#   label = "F",
#   legend = "topright",
#   offset = 0.1,
#   ci.res = 1000,
#   cex.lab = 1.7,
#   cex.axis = 1.4,
#   cex.main = 1.7
# )
# dev.off()



# Weight function model

# Specific publication bias test - 
# increases the weight of studies that are less likely to be published and 
# decreases the weight of those that are more likely to be published - based on p-value
# likehood test alfa = 0.10

# Global
wf_global <- weightfunct(Efeito$yi, Efeito$vi, table = TRUE, steps = .05)

# Mice
wmice <- Efeito |> 
  filter(species == "mice")

wf_mice <- weightfunct(wmice$yi, wmice$vi, table = TRUE, steps = 0.05)
wf_mice

# rat
wrat <- Efeito |> 
  filter(species == "rat")

wf_rat <- weightfunct(wrat$yi, wrat$vi, table = TRUE, steps = 0.05)
wf_rat

# no positive control
wnoCP <- Efeito |> 
  filter(positive_control == 0)

wf_noCP <- weightfunct(wnoCP$yi, wnoCP$vi, table = TRUE, steps = 0.05)

# STRATIFIED ANALYSIS ----

# Population ----

# mice ----
Teste_mice <- rma(yi, vi, subset = (species == "mice"), data = Efeito)
Teste_mice

# sex
Teste_macho_m <- rma(yi, vi, subset = (sex == "M" & species == "mice"), data = Efeito)
Teste_macho_m

Teste_femea_m <- rma(yi, vi, subset = (sex == "F" & species == "mice"), data = Efeito)
Teste_femea_m

Teste_sexoambos_m <- rma(yi, vi, subset = (sex == "M and F" & species == "mice"), data = Efeito)
Teste_sexoambos_m

Efeito |> 
  filter(sex == "M and F" & species == "mice") |> 
  select(authors) # same publication?

Teste_sexoind_m <- rma(yi, vi, subset = (sex == "NA" & species == "mice"), data = Efeito)
Teste_sexoind_m

Efeito |> 
  filter(sex == "NA" & species == "mice") |> 
  select(authors) # same publication?

# strain
Teste_swiss <- rma(yi, vi, subset = (strain == "swiss" & species == "mice"), data = Efeito)
Teste_swiss

Teste_CD1 <- rma(yi, vi, subset = (strain == "CD-1" & species == "mice"), data = Efeito)
Teste_CD1

Teste_C57BL <- rma(yi, vi, subset = (strain == "C57BL" & species == "mice"), data = Efeito)
Teste_C57BL

Teste_BALB <- rma(yi, vi, subset = (strain == "BALB" & species == "mice"), data = Efeito)
Teste_BALB

Teste_ddY <- rma(yi, vi, subset = (strain == "ddY" & species == "mice"), data = Efeito)
Teste_ddY

Efeito |> 
  filter(strain == "ddY") |> 
  select(authors) # same publication?

Teste_laca <- rma(yi, vi, subset = (strain == "laca" & species == "mice"), data = Efeito)
Teste_laca

Efeito |> 
  filter(strain == "laca") |> 
  select(authors) # same publication?

Teste_OF1 <- rma(yi, vi, subset = (strain == "OF1" & species == "mice"), data = Efeito)
Teste_OF1

Efeito |> 
  filter(strain == "OF1") |> 
  select(authors) # same publication?

Teste_NMRI <- rma(yi, vi, subset = (strain == "NMRI" & species == "mice"), data = Efeito)
Teste_NMRI

Efeito |> 
  filter(strain == "NMRI") |> 
  select(authors) # same publication?

Teste_sabra <- rma(yi, vi, subset = (strain == "sabra" & species == "mice"), data = Efeito)
Teste_sabra

Efeito |> 
  filter(strain == "sabra") |> 
  select(authors) # same publication?

Teste_BKTO <- rma(yi, vi, subset = (strain == "BKTO" & species == "mice"), data = Efeito)
Teste_BKTO

Efeito |> 
  filter(strain == "BKTO") |> 
  select(authors) # same publication?

Teste_NA_m <- rma(yi, vi, subset = (strain == "NA" & species == "mice"), data = Efeito)
Teste_NA_m

Efeito |> 
  filter(strain == "NA") |> 
  select(authors) # same publication?

Teste_DBA2 <- rma(yi, vi, subset = (strain == "DBA/2" & species == "mice"), data = Efeito)
Teste_DBA2

Efeito |> 
  filter(strain == "DBA/2") |> 
  select(authors) # same publication?

Teste_B6SJL <- rma(yi, vi, subset = (strain == "B6SJL (R406W transgenic)" & species == "mice"), data = Efeito)
Teste_B6SJL 

Efeito |> 
  filter(strain == "B6SJL (R406W transgenic)") |> 
  select(authors) # same publication?

Teste_129S6 <- rma(yi, vi, subset = (strain == "129S6" & species == "mice"), data = Efeito)
Teste_129S6 # k < 3

Teste_SPF <- rma(yi, vi, subset = (strain == "SPF" & species == "mice"), data = Efeito)
Teste_SPF # k < 3

levels(Efeito$strain)

# stress
Teste_stress_m <- rma(yi, vi, subset = (model_phenotype != "NA" & species == "mice"), data = Efeito)
Teste_stress_m

Teste_nostress_m <- rma(yi, vi, subset = (model_phenotype == "NA" & species == "mice"), data = Efeito)
Teste_nostress_m

# light cycle
normal1212_m <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12 normal" & species == "mice"), data = Efeito)
normal1212_m

doze_doze_m <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12" & species == "mice"), data = Efeito)
doze_doze_m

inverso_m <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12 reverse" & species == "mice"), data = Efeito)
inverso_m

cycle_NA_m <- rma(yi, vi, subset = (bioterium_lightcycle == "NA" & species == "mice"), data = Efeito)
cycle_NA_m 

natural_m <- rma(yi, vi, subset = (bioterium_lightcycle == "natural" & species == "mice"), data = Efeito)
natural_m # k < 3

dez_q_m <- rma(yi, vi, subset = (bioterium_lightcycle == "10/14" & species == "mice"), data = Efeito)
dez_q_m # k < 3

# rat ----
Teste_rat <- rma(yi, vi, subset = (species == "rat"), data = Efeito)
Teste_rat

#sex
Teste_macho_r <- rma(yi, vi, subset = (sex == "M" & species == "rat"), data = Efeito)
Teste_macho_r

Teste_femea_r <- rma(yi, vi, subset = (sex == "F" & species == "rat"), data = Efeito)
Teste_femea_r

Teste_sexoambos_r <- rma(yi, vi, subset = (sex == "M and F" & species == "rat"), data = Efeito)
Teste_sexoambos_r

Efeito |> 
  filter(sex == "M and F" & species == "rat") |> 
  select(authors) # same publication?

Teste_sexoind_r <- rma(yi, vi, subset = (sex == "NA" & species == "rat"), data = Efeito)
Teste_sexoind_r

Efeito |> 
  filter(sex == "NA" & species == "rat") |> 
  select(authors) # same publication?

#strain
Teste_wistar <- rma(yi, vi, subset = (strain == "wistar" & species == "rat"), data = Efeito)
Teste_wistar

Teste_sd <- rma(yi, vi, subset = (strain == "sprague dawley" & species == "rat"), data = Efeito)
Teste_sd

Teste_LE <- rma(yi, vi, subset = (strain == "long-evans" & species == "rat"), data = Efeito)
Teste_LE

Efeito |> 
  filter(strain == "long-evans" & species == "rat") |> 
  select(authors) # same publication?

Teste_FS <- rma(yi, vi, subset = (strain == "flinders sensitive" & species == "rat"), data = Efeito)
Teste_FS

Efeito |> 
  filter(strain == "flinders sensitive" & species == "rat") |> 
  select(authors) # same publication?

Teste_CDCOBS <- rma(yi, vi, subset = (strain == "CD-COBS" & species == "rat"), data = Efeito)
Teste_CDCOBS

Efeito |> 
  filter(strain == "CD-COBS" & species == "rat") |> 
  select(authors) # same publication?

Teste_WK <- rma(yi, vi, subset = (strain == "wistar kyoto" & species == "rat"), data = Efeito)
Teste_WK 

Efeito |> 
  filter(strain == "wistar kyoto" & species == "rat") |> 
  select(authors) # same publication?

Teste_FR <- rma(yi, vi, subset = (strain == "flinders resistant" & species == "rat"), data = Efeito)
Teste_FR

Efeito |> 
  filter(strain == "flinders resistant" & species == "rat") |> 
  select(authors) # same publication?

Teste_BN <- rma(yi, vi, subset = (strain == "brown norway" & species == "rat"), data = Efeito)
Teste_BN # k < 3

Teste_NA_r <- rma(yi, vi, subset = (strain == "NA" & species == "rat"), data = Efeito)
Teste_NA_r # k < 3

Teste_CD1 <- rma(yi, vi, subset = (strain == "CD-1" & species == "rat"), data = Efeito)
Teste_CD1 # k < 3

# stress
Teste_stress_r <- rma(yi, vi, subset = (model_phenotype != "NA" & species == "rat"), data = Efeito)
Teste_stress_r

Teste_nostress_r <- rma(yi, vi, subset = (model_phenotype == "NA" & species == "rat"), data = Efeito)
Teste_nostress_r

# light cycle
normal1212_r <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12 normal" & species == "rat"), data = Efeito)
normal1212_r

doze_doze_r <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12" & species == "rat"), data = Efeito)
doze_doze_r

cycle_NA_r <- rma(yi, vi, subset = (bioterium_lightcycle == "NA" & species == "rat"), data = Efeito)
cycle_NA_r 

Efeito |> 
  filter(bioterium_lightcycle == "NA" & species == "rat") |> 
  select(authors) # same publication?

natural_r <- rma(yi, vi, subset = (bioterium_lightcycle == "natural" & species == "rat"), data = Efeito)
natural_r 

inverso_r <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12 reverse" & species == "rat"), data = Efeito)
inverso_r

Efeito |> 
  filter(bioterium_lightcycle == "12/12 reverse" & species == "rat") |> 
  select(authors) # same publication?

dez_q_r <- rma(yi, vi, subset = (bioterium_lightcycle == "10/14" & species == "rat"), data = Efeito)
dez_q_r 

Efeito |> 
  filter(bioterium_lightcycle == "10/14" & species == "rat") |> 
  select(authors) # same publication?

# Intervention---- 

# mice -----

#TCA
Teste_TCA_m <- rma(yi, vi, subset = (atd_class == "tricyclic" & species == "mice"), data = Efeito)
Teste_TCA_m

Teste_imi_m <- rma(yi, vi, subset = (atd_type == "imipramine" & species == "mice"), data = Efeito)
Teste_imi_m

Teste_des_m <- rma(yi, vi, subset = (atd_type == "desipramine" & species == "mice"), data = Efeito)
Teste_des_m

Teste_ami_m <- rma(yi, vi, subset = (atd_type == "amitriptyline" & species == "mice"), data = Efeito)
Teste_ami_m

Teste_clo_m <- rma(yi, vi, subset = (atd_type == "clomipramine" & species == "mice"), data = Efeito)
Teste_clo_m

Efeito |> 
  filter(atd_type == "clomipramine" & species == "mice") |> 
  select(authors) # same publication?

Teste_nor_m <- rma(yi, vi, subset = (atd_type == "nortriptyline" & species == "mice"), data = Efeito)
Teste_nor_m

Efeito |> 
  filter(atd_type == "nortriptyline" & species == "mice") |> 
  select(authors) # same publication?

#SSRI
Teste_SSRI_m <- rma(yi, vi, subset = (atd_class == "SSRI" & species == "mice"), data = Efeito)
Teste_SSRI_m

Teste_flu_m <- rma(yi, vi, subset = (atd_type == "fluoxetine" & species == "mice"), data = Efeito)
Teste_flu_m

Teste_par_m <- rma(yi, vi, subset = (atd_type == "paroxetine" & species == "mice"), data = Efeito)
Teste_par_m

Efeito |> 
  filter(atd_type == "paroxetine" & species == "mice") |> 
  select(authors) # same publication?

Teste_esc_m <- rma(yi, vi, subset = (atd_type == "escitalopram" & species == "mice"), data = Efeito)
Teste_esc_m

Efeito |> 
  filter(atd_type == "escitalopram" & species == "mice") |> 
  select(authors) # same publication?

Teste_cit_m <- rma(yi, vi, subset = (atd_type == "citalopram" & species == "mice"), data = Efeito)
Teste_cit_m

Efeito |> 
  filter(atd_type == "citalopram" & species == "mice") |> 
  select(authors) # same publication?

Teste_fluv_m <- rma(yi, vi, subset = (atd_type == "fluvoxamine" & species == "mice"), data = Efeito)
Teste_fluv_m

Efeito |> 
  filter(atd_type == "fluvoxamine" & species == "mice") |> 
  select(authors) # same publication?

#SNRI
Teste_SNRI_m <- rma(yi, vi, subset = (atd_class == "SNRI" & species == "mice"), data = Efeito, control = list(stepadj = 0.5, maxiter = 1000)) # add parametros para ajustar comprimento do passo e maximo de iterações para a convergencia ocorrer (https://stackoverflow.com/questions/68817204/why-did-the-fisher-scoring-algorithm-not-converge-after-adjusting)
Teste_SNRI_m

Teste_ven_m <- rma(yi, vi, subset = (atd_type == "venlafaxine" & species == "mice"), data = Efeito)
Teste_ven_m

Efeito |> 
  filter(atd_type == "venlafaxine" & species == "mice") |> 
  select(authors) # same publication?

Teste_tra_m <- rma(yi, vi, subset = (atd_type == "tramadol" & species == "mice"), data = Efeito)
Teste_tra_m

Efeito |> 
  filter(atd_type == "tramadol" & species == "mice") |> 
  select(authors) # same publication?

#IMAO
Teste_IMAO_m <- rma(yi, vi, subset = (atd_class == "IMAO" & species == "mice"), data = Efeito)
Teste_IMAO_m

Teste_sel_m <- rma(yi, vi, subset = (atd_type == "selegiline" & species == "mice"), data = Efeito)
Teste_sel_m

Efeito |> 
  filter(atd_type == "selegiline" & species == "mice") |> 
  select(authors) # same publication?

Teste_moc_m <- rma(yi, vi, subset = (atd_type == "moclobemide" & species == "mice"), data = Efeito)
Teste_moc_m

Efeito |> 
  filter(atd_type == "moclobemide" & species == "mice") |> 
  select(authors) # same publication?

#NDRI
Teste_NDRI_m <- rma(yi, vi, subset = (atd_class == "NDRI" & species == "mice"), data = Efeito)
Teste_NDRI_m

#TECA
Teste_TeCA_m <- rma(yi, vi, subset = (atd_class == "teca" & species == "mice"), data = Efeito)
Teste_TeCA_m

Teste_map_m <- rma(yi, vi, subset = (atd_type == "maprotiline" & species == "mice"), data = Efeito)
Teste_map_m

Efeito |> 
  filter(atd_type == "maprotiline" & species == "mice") |> 
  select(authors) # same publication?

Teste_mia_m <- rma(yi, vi, subset = (atd_type == "mianserin" & species == "mice"), data = Efeito)
Teste_mia_m

Efeito |> 
  filter(atd_type == "mianserin" & species == "mice") |> 
  select(authors) # same publication?

# VIA adm
Teste_IP_m <- rma(yi, vi, subset = (treatment_via == "IP" & species == "mice"), data = Efeito)
Teste_IP_m

Teste_oral_m <- rma(yi, vi, subset = (treatment_via == "oral" & species == "mice"), data = Efeito)
Teste_oral_m

Teste_gav_m <- rma(yi, vi, subset = (treatment_via == "gavage" & species == "mice"), data = Efeito)
Teste_gav_m

Teste_subc_m <- rma(yi, vi, subset = (treatment_via == "subcutaneous" & species == "mice"), data = Efeito)
Teste_subc_m

Teste_viaNA_m <- rma(yi, vi, subset = (treatment_via == "NA" & species == "mice"), data = Efeito)
Teste_viaNA_m

Efeito |> 
  filter(treatment_via == "NA" & species == "mice") |> 
  select(authors) # same publication?

# rat -----

#TCA
Teste_TCA_r <- rma(yi, vi, subset = (atd_class == "tricyclic" & species == "rat"), data = Efeito)
Teste_TCA_r

Teste_imi_r <- rma(yi, vi, subset = (atd_type == "imipramine" & species == "rat"), data = Efeito)
Teste_imi_r

Teste_des_r <- rma(yi, vi, subset = (atd_type == "desipramine" & species == "rat"), data = Efeito)
Teste_des_r

Teste_ami_r <- rma(yi, vi, subset = (atd_type == "amitriptyline" & species == "rat"), data = Efeito)
Teste_ami_r

Teste_clo_r <- rma(yi, vi, subset = (atd_type == "clomipramine" & species == "rat"), data = Efeito)
Teste_clo_r

Efeito |> 
  filter(atd_type == "clomipramine" & species == "rat") |> 
  select(authors) # same publication?

#SSRI
Teste_SSRI_r <- rma(yi, vi, subset = (atd_class == "SSRI" & species == "rat"), data = Efeito)
Teste_SSRI_r

Teste_flu_r <- rma(yi, vi, subset = (atd_type == "fluoxetine" & species == "rat"), data = Efeito)
Teste_flu_r

Teste_ser_r <- rma(yi, vi, subset = (atd_type == "sertraline" & species == "rat"), data = Efeito)
Teste_ser_r

Teste_par_r <- rma(yi, vi, subset = (atd_type == "paroxetine" & species == "rat"), data = Efeito)
Teste_par_r

Efeito |> 
  filter(atd_type == "paroxetine" & species == "rat") |> 
  select(authors) # same publication?

Teste_fluv_r <- rma(yi, vi, subset = (atd_type == "fluvoxamine" & species == "rat"), data = Efeito)
Teste_fluv_r

Efeito |> 
  filter(atd_type == "fluvoxamine" & species == "rat") |> 
  select(authors) # same publication?

Teste_cit_r <- rma(yi, vi, subset = (atd_type == "citalopram" & species == "rat"), data = Efeito)
Teste_cit_r

Efeito |> 
  filter(atd_type == "citalopram" & species == "rat") |> 
  select(authors) # same publication?

Teste_esc_r <- rma(yi, vi, subset = (atd_type == "escitalopram" & species == "rat"), data = Efeito)
Teste_esc_r

Efeito |> 
  filter(atd_type == "escitalopram" & species == "rat") |> 
  select(authors) # same publication?

#SNRI
Teste_SNRI_r <- rma(yi, vi, subset = (atd_class == "SNRI" & species == "rat"), data = Efeito)
Teste_SNRI_r

Teste_ven_r <- rma(yi, vi, subset = (atd_type == "venlafaxine" & species == "rat"), data = Efeito)
Teste_ven_r

Teste_desv_r <- rma(yi, vi, subset = (atd_type == "desvenlafaxine" & species == "rat"), data = Efeito)
Teste_desv_r

Efeito |> 
  filter(atd_type == "desvenlafaxine" & species == "rat") |> 
  select(authors) # same publication?

Teste_reb_r <- rma(yi, vi, subset = (atd_type == "reboxetine" & species == "rat"), data = Efeito)
Teste_reb_r

Efeito |> 
  filter(atd_type == "reboxetine" & species == "rat") |> 
  select(authors) # same publication?

Teste_sib_r <- rma(yi, vi, subset = (atd_type == "sibutramine" & species == "rat"), data = Efeito)
Teste_sib_r

Efeito |> 
  filter(atd_type == "sibutramine" & species == "rat") |> 
  select(authors) # same publication?

#TECA
Teste_TeCA_r <- rma(yi, vi, subset = (atd_class == "teca" & species == "rat"), data = Efeito)
Teste_TeCA_r

Teste_mia_r <- rma(yi, vi, subset = (atd_type == "mianserin" & species == "rat"), data = Efeito)
Teste_mia_r

Teste_amo_r <- rma(yi, vi, subset = (atd_type == "amoxapine" & species == "rat"), data = Efeito)
Teste_amo_r


#IMAO
Teste_IMAO_r <- rma(yi, vi, subset = (atd_class == "IMAO" & species == "rat"), data = Efeito)
Teste_IMAO_r


# VIA adm
Teste_IP_r <- rma(yi, vi, subset = (treatment_via == "IP" & species == "rat"), data = Efeito)
Teste_IP_r

Teste_oral_r <- rma(yi, vi, subset = (treatment_via == "oral" & species == "rat"), data = Efeito)
Teste_oral_r

Teste_subc_r <- rma(yi, vi, subset = (treatment_via == "subcutaneous" & species == "rat"), data = Efeito)
Teste_subc_r

Teste_gav_r <- rma(yi, vi, subset = (treatment_via == "gavage" & species == "rat"), data = Efeito)
Teste_gav_r

Teste_mi_r <- rma(yi, vi, subset = (treatment_via == "microinjection (dorsal hippocampus)" & species == "rat"), data = Efeito)
Teste_mi_r

Efeito |> 
  filter(treatment_via == "microinjection (dorsal hippocampus)" & species == "rat") |> 
  select(authors) # same publication?

Teste_od_r <- rma(yi, vi, subset = (treatment_via == "oral (dietary treatment)" & species == "rat"), data = Efeito)
Teste_od_r

Efeito |> 
  filter(treatment_via == "oral (dietary treatment)" & species == "rat") |> 
  select(authors) # same publication?

Teste_in_r <- rma(yi, vi, subset = (treatment_via == "intranasal" & species == "rat"), data = Efeito)
Teste_in_r

Efeito |> 
  filter(treatment_via == "intranasal" & species == "rat") |> 
  select(authors) # same publication?

Teste_viaNA_r <- rma(yi, vi, subset = (treatment_via == "NA" & species == "rat"), data = Efeito)
Teste_viaNA_r # k < 3

# Outcome -----

# mice----

# protocol
fst_pro_m <- Efeito |> 
  filter(species == "mice") |> 
  group_by(fst_protocol) |> 
  summarise(soma = n()) |> 
  arrange(desc(soma)) # Identify which protocols were used and leave only those used by at least 3 studies

Teste_T6S4_m <- rma(yi, vi, subset = (fst_protocol == "test6score4" & species == "mice"), data = Efeito)
Teste_T6S4_m

Teste_T6_m <- rma(yi, vi, subset = (fst_protocol == "test6" & species == "mice"), data = Efeito)
Teste_T6_m

Teste_PT15T6S4_m <- rma(yi, vi, subset = (fst_protocol == "pre15test6score4" & species == "mice"), data = Efeito)
Teste_PT15T6S4_m

Teste_PT15T6_m <- rma(yi, vi, subset = (fst_protocol == "pre15test6" & species == "mice"), data = Efeito)
Teste_PT15T6_m

Teste_PT15T5_m <- rma(yi, vi, subset = (fst_protocol == "pre15test5" & species == "mice"), data = Efeito)
Teste_PT15T5_m

Efeito |> 
  filter(fst_protocol == "pre15test5" & species == "mice") |> 
  select(authors) # same publication?

Teste_T5_m <- rma(yi, vi, subset = (fst_protocol == "test5" & species == "mice"), data = Efeito)
Teste_T5_m

Efeito |> 
  filter(fst_protocol == "test5" & species == "mice") |> 
  select(authors) # same publication?

Teste_PT5T5_m <- rma(yi, vi, subset = (fst_protocol == "pre5test5" & species == "mice"), data = Efeito)
Teste_PT5T5_m

Efeito |> 
  filter(fst_protocol == "pre5test5" & species == "mice") |> 
  select(authors) # same publication?

Teste_T5S4_m <- rma(yi, vi, subset = (fst_protocol == "test5score4" & species == "mice"), data = Efeito)
Teste_T5S4_m

Efeito |> 
  filter(fst_protocol == "test5score4" & species == "mice") |> 
  select(authors) # same publication?

Teste_T7S6_m <- rma(yi, vi, subset = (fst_protocol == "test7score6" & species == "mice"), data = Efeito)
Teste_T7S6_m

Efeito |> 
  filter(fst_protocol == "test7score6" & species == "mice") |> 
  select(authors) # same publication?

Teste_T6S5_m <- rma(yi, vi, subset = (fst_protocol == "test6score5" & species == "mice"), data = Efeito)
Teste_T6S5_m

Efeito |> 
  filter(fst_protocol == "test6score5" & species == "mice") |> 
  select(authors) # same publication?

Teste_T9_m <- rma(yi, vi, subset = (fst_protocol == "test9" & species == "mice"), data = Efeito)
Teste_T9_m

Efeito |> 
  filter(fst_protocol == "test9" & species == "mice") |> 
  select(authors) # same publication?

Teste_PT15T6S5_m <- rma(yi, vi, subset = (fst_protocol == "pre15test6score5" & species == "mice"), data = Efeito)
Teste_PT15T6S5_m

Efeito |> 
  filter(fst_protocol == "pre15test6score5" & species == "mice") |> 
  select(authors) # same publication?

Teste_T10_m <- rma(yi, vi, subset = (fst_protocol == "test10" & species == "mice"), data = Efeito)
Teste_T10_m

Efeito |> 
  filter(fst_protocol == "test10" & species == "mice") |> 
  select(authors) # same publication?

Teste_PT15TNA_m <- rma(yi, vi, subset = (fst_protocol == "pre15test?" & species == "mice"), data = Efeito)
Teste_PT15TNA_m

Efeito |> 
  filter(fst_protocol == "pre15test?" & species == "mice") |> 
  select(authors) # same publication?

Teste_T15S5_m <- rma(yi, vi, subset = (fst_protocol == "test15score5" & species == "mice"), data = Efeito)
Teste_T15S5_m

Efeito |> 
  filter(fst_protocol == "test15score5" & species == "mice") |> 
  select(authors) # same publication?

# method
Teste_metNA_m <- rma(yi, vi, subset = (measurement_method == "NA" & species == "mice"), data = Efeito)
Teste_metNA_m

Teste_metvideo_m <- rma(yi, vi, subset = (measurement_method == "video analysis" & species == "mice"), data = Efeito)
Teste_metvideo_m

Teste_metmanual_m <- rma(yi, vi, subset = (measurement_method == "manually" & species == "mice"), data = Efeito)
Teste_metmanual_m

# other tests
Teste_NOotherT_m <- rma(yi, vi, subset = (others_tests == "NA" & species == "mice" | others_tests == "No" & species == "mice"), data = Efeito)
Teste_NOotherT_m

Teste_otherT_m <- rma(yi, vi, subset = (others_tests != "NA" & others_tests != "No" & species == "mice"), data = Efeito)
Teste_otherT_m

# rat ----

# protocol
fst_pro_r <- Efeito |> 
  filter(species == "rat") |> 
  group_by(fst_protocol) |> 
  summarise(soma = n()) |> 
  arrange(desc(soma)) # Identify which protocols were used and leave only those used by at least 3 studies

Teste_PT15T5_r <- rma(yi, vi, subset = (fst_protocol == "pre15test5" & species == "rat"), data = Efeito)
Teste_PT15T5_r

Teste_T5_r <- rma(yi, vi, subset = (fst_protocol == "test5" & species == "rat"), data = Efeito)
Teste_T5_r

Teste_PT13T6_r <- rma(yi, vi, subset = (fst_protocol == "pre13test6" & species == "rat"), data = Efeito)
Teste_PT13T6_r

Efeito |> 
  filter(fst_protocol == "pre13test6" & species == "rat") |> 
  select(authors) # same publication?

Teste_PTNAT6S4_r <- rma(yi, vi, subset = (fst_protocol == "pre?test6score4" & species == "rat"), data = Efeito)
Teste_PTNAT6S4_r

Efeito |> 
  filter(fst_protocol == "pre?test6score4" & species == "rat") |> 
  select(authors) # same publication?

Teste_PT5T5_r <- rma(yi, vi, subset = (fst_protocol == "pre5test5" & species == "rat"), data = Efeito)
Teste_PT5T5_r

Efeito |> 
  filter(fst_protocol == "pre5test5" & species == "rat") |> 
  select(authors) # same publication?

Teste_T15_r <- rma(yi, vi, subset = (fst_protocol == "test15" & species == "rat"), data = Efeito)
Teste_T15_r

Efeito |> 
  filter(fst_protocol == "test15" & species == "rat") |> 
  select(authors) # same publication?

Teste_PT15T6S5_r <- rma(yi, vi, subset = (fst_protocol == "pre15test6score5" & species == "rat"), data = Efeito)
Teste_PT15T6S5_r

Efeito |> 
  filter(fst_protocol == "pre15test6score5" & species == "rat") |> 
  select(authors) # same publication?

# method
Teste_metNA_r <- rma(yi, vi, subset = (measurement_method == "NA" & species == "rat"), data = Efeito)
Teste_metNA_r

Teste_metvideo_r <- rma(yi, vi, subset = (measurement_method == "video analysis" & species == "rat"), data = Efeito)
Teste_metvideo_r

Teste_metmanual_r <- rma(yi, vi, subset = (measurement_method == "manually" & species == "rat"), data = Efeito)
Teste_metmanual_r

# other tests
Teste_NOotherT_r <- rma(yi, vi, subset = (others_tests == "NA" & species == "rat" | others_tests == "No" & species == "rat"), data = Efeito)
Teste_NOotherT_r

Teste_otherT_r <- rma(yi, vi, subset = (others_tests != "NA" & others_tests != "No" & species == "rat"), data = Efeito)
Teste_otherT_r

# STRATIFIED ANALYSIS | FIGURES ------

# I organized the results of the subgroups into an Excel spreadsheet.

# Load data
dfsubgroups <- read_excel("data/subgroupresults.xlsx")

# Transform variables
dfsubgroups <- dfsubgroups |> 
  rename(IC95LL = `IC95-L`,
         IC95UL = `IC95-U`,
         inconsistency = `I² (%)`) |> 
  separate(k, c("k", "nested")) |> 
  mutate(tau2 = round(dfsubgroups$tau2, digits = 1),
         moderator = as.factor(moderator),
         category = as.factor(category),
         category = fct_reorder(category, k),
         k = as.numeric(k),
         nested = case_when(nested == "" ~ "*"),
         nested = replace_na(nested,""),
         outline = 100) 

# Assign the order of levels
transform_levels <- function(data, column_name) { #function to modify the levels' order for factor variables
  data |> 
    mutate({{ column_name }} := fct_inorder({{ column_name }}))
}

dfsubgroups <- transform_levels(dfsubgroups, moderator)
dfsubgroups <- transform_levels(dfsubgroups, category)

# Set a global font
theme_set(theme_minimal(base_family = "Gadugi"))

# List specific settings
pio_info <- list(population = list(type = "Population", 
                               label = c("Species","Sex","Strain","Precondition","Light cycle"), 
                               label_position_m = c(28,24,15,5.5,1), 
                               label_y_m = c(1, 28),
                               label_position_r = c(28,24,16,9,3),
                               label_y_r = c(1, 28),
                               layout = "####GGGGGGGGGG##\nCCCCAAAAAAAAAABB\nCCCCAAAAAAAAAABB\nCCCCAAAAAAAAAABB\nCCCCAAAAAAAAAABB\nFFFFDDDDDDDDDDEE\nFFFFDDDDDDDDDDEE\nFFFFDDDDDDDDDDEE\nFFFFDDDDDDDDDDEE",
                               width = 8, 
                               height = 8),
             intervention = list(type = "Intervention", 
                                 label = c("Drug", "Route"),
                                 label_position_m = c(20, 1.5),
                                 label_y_m = c(1, 33),
                                 label_position_r = c(20, 2),
                                 label_y_r = c(1, 33),
                                 layout = "####GGGGGGGGGG##\nCCCCAAAAAAAAAABB\nCCCCAAAAAAAAAABB\nCCCCAAAAAAAAAABB\nCCCCAAAAAAAAAABB\nFFFFDDDDDDDDDDEE\nFFFFDDDDDDDDDDEE\nFFFFDDDDDDDDDDEE\nFFFFDDDDDDDDDDEE",
                                 width = 8, 
                                 height = 9),
             outcome = list(type = "Outcome", 
                            label = c("Protocol", "Scoring\n method", "Test\n battery"),
                            label_position_m = c(30, 5, .1),
                            label_y_m = c(0, 44),
                            label_position_r = c(22, 6, .1), 
                            label_y_r = c(0, 28),
                            layout = "####GGGGGGGGGG##\nCCCCAAAAAAAAAABB\nCCCCAAAAAAAAAABB\nCCCCAAAAAAAAAABB\nCCCCAAAAAAAAAABB\nCCCCAAAAAAAAAABB\nFFFFDDDDDDDDDDEE\nFFFFDDDDDDDDDDEE\nFFFFDDDDDDDDDDEE",
                            width = 7, 
                            height = 7))


# Create function to generate plot 
generate_subgroup_plot <- function(dfsubgroups, pio_info, pio){
  
  pio_info <- pio_info[[pio]]
  
  color_mice <- "#ff9400"
  color_rat <- "#ec2b2b"
  
  forest_m <- dfsubgroups |>
    filter(species == "Mice",
           type == pio_info$type) |>
    ggplot(aes(
      x = fct_reorder(category, k),
      y = GES,
      ymin = IC95LL,
      ymax = IC95UL,
      color = color_mice
    )) +
    geom_rect(fill = "white",xmin = 0,xmax = Inf,
              ymin = -Inf,ymax = 0, color = "white") +
    geom_rect(fill = "grey100",xmin = 0,xmax = Inf,
              ymin = 0.01,ymax = .19, color = "grey100") +
    geom_rect(fill = "grey96",xmin = 0,xmax = Inf,
              ymin = 0.2,ymax = .49, color = "grey96") +
    geom_rect(fill = "grey92",xmin = 0,xmax = Inf,
              ymin = 0.5,ymax = .79, color = "grey92") +
    geom_rect(fill = "grey88",xmin = 0,xmax = Inf,
              ymin = 0.8,ymax = 1.19, color = "grey88") +
    geom_rect(fill = "grey84",xmin = 0,xmax = Inf,
              ymin = 1.2,ymax = 1.99, color = "grey84") +
    geom_rect(fill = "grey82",xmin = 0,xmax = Inf,
              ymin = 2,ymax = Inf, color = "grey82") +
    geom_pointrange() +
    scale_y_continuous(limits = c(-2, 22)) +
    labs(x = "", y = "") +
    scale_colour_manual(values = color_mice) +
    geom_hline(yintercept = 0, lty = 2, linewidth = .2) +
    facet_grid(fct_inorder(moderator) ~ ., scales = "free", space = "free") +
    geom_text(
      aes(label = paste(
        "k = ",
        k,
        fct_reorder(nested, k),
        sep = ""
      )),
      y = Inf - 1,
      color = "black",
      size = 3,
      family = "Gadugi",
      hjust = 1
    ) +
    coord_flip() +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 9, color = "black"),
      plot.background = element_rect(colour = "white"),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  incon_m <- dfsubgroups |>
    filter(species == "Mice",
           type == pio_info$type) |>
    ggplot(aes(
      x = fct_reorder(category, k)
    )) +
    geom_bar(aes(y = inconsistency, fill = "inconsistency"), stat = "identity", position = "identity") + #HERE
    geom_bar(aes(y = outline, fill = "outline"), stat = "identity", position = "identity", alpha = 0, linewidth = .1, color = "black") + #HERE
    scale_y_continuous(limits = c(0, 200), position = "right") +
    labs(x = "", y = "") +
    scale_fill_manual(values = c("inconsistency" = color_mice, "outline" = "black"), guide = "none") + #HERE and the next
    geom_text(
      aes(label = tau2),
      y = 104,
      color = "black",
      size = 3,
      family = "Gadugi",
      hjust = -0.1
    ) +
    facet_grid(fct_inorder(moderator) ~ ., scales = "free", space = "free") +
    coord_flip() +
    theme_void() +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )

  
  label_m <- 
    ggplot() +
    labs(title = "A") + 
    annotate(
      "text", label = paste(pio_info$label),
      x = rep(1, length(pio_info$label_position_m)), y = pio_info$label_position_m,     
      size = 3,
      family = "Gadugi",
      hjust = 0,
      vjust = -0.5,
      fontface = "bold",
      colour = "black"
    ) +
    scale_y_continuous(limits = pio_info$label_y_m, position = "right") +
    theme_void() + 
    theme(plot.title = element_text(hjust = 0.5, vjust = 4, margin = margin(t = -5, r = -5, b = -5, l = -5, unit = "pt")))
  
  
  forest_r <- dfsubgroups |>
    filter(species == "Rat",
           type == pio_info$type) |>
    ggplot(aes(
      x = fct_reorder(category, k),
      y = GES,
      ymin = IC95LL,
      ymax = IC95UL,
      color = color_rat
    )) +
    geom_rect(fill = "white",xmin = 0,xmax = Inf,
              ymin = -Inf,ymax = 0, color = "white") +
    geom_rect(fill = "grey100",xmin = 0,xmax = Inf,
              ymin = 0.01,ymax = .19, color = "grey100") +
    geom_rect(fill = "grey96",xmin = 0,xmax = Inf,
              ymin = 0.2,ymax = .49, color = "grey96") +
    geom_rect(fill = "grey92",xmin = 0,xmax = Inf,
              ymin = 0.5,ymax = .79, color = "grey92") +
    geom_rect(fill = "grey88",xmin = 0,xmax = Inf,
              ymin = 0.8,ymax = 1.19, color = "grey88") +
    geom_rect(fill = "grey84",xmin = 0,xmax = Inf,
              ymin = 1.2,ymax = 1.99, color = "grey84") +
    geom_rect(fill = "grey82",xmin = 0,xmax = Inf,
              ymin = 2,ymax = Inf, color = "grey82") +
    geom_pointrange() +
    scale_y_continuous(limits = c(-2, 22)) +
    labs(x = "", y = "Combined Effect Size") +
    scale_colour_manual(values = color_rat) +
    geom_hline(yintercept = 0, lty = 2, linewidth = .2) +
    facet_grid(fct_inorder(moderator) ~ ., scales = "free", space = "free") +
    geom_text(
      aes(label = paste(
        "k = ",
        k,
        fct_reorder(nested, k),
        sep = ""
      )),
      y = Inf - 1,
      color = "black",
      size = 3,
      family = "Gadugi",
      hjust = 1
    ) +
    coord_flip() +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.title = element_text(size = 9, color = "black", vjust = -1),
      axis.text = element_text(size = 9, color = "black"),
      axis.ticks.length = unit(0.1,"cm"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(linewidth = .2, color = "black"),
      axis.line.x = element_line(linewidth = .2, colour = "black", linetype = 1),
      plot.background = element_rect(colour = "white")
    )
  
  incon_r <- dfsubgroups |>
    filter(species == "Rat",
           type == pio_info$type) |>
    ggplot(aes(
      x = fct_reorder(category, k)
    )) +
    geom_bar(aes(y = inconsistency, fill = "inconsistency"), stat = "identity", position = "identity") +
    geom_bar(aes(y = outline, fill = "outline"), stat = "identity", position = "identity", alpha = 0, linewidth = .1, color = "black") +
    scale_y_continuous(limits = c(0, 200), breaks = c(0, 100)) +
    labs(x = "", y = "I² (%) |𝜏² ") +
    scale_fill_manual(values = c("inconsistency" = color_rat, "outline" = "black"), guide = "none") + 
    geom_text(
      aes(label = tau2),
      y = 104,
      color = "black",
      size = 3,
      family = "Gadugi",
      hjust = -0.1
    ) +
    facet_grid(fct_inorder(moderator) ~ ., scales = "free", space = "free") +
    coord_flip() +
    theme_void() +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.ticks.length = unit(0.1,"cm"),
      axis.ticks.x = element_line(linewidth = .2, color = "black"),
      axis.title = element_text(size = 9, color = "black", hjust = 0.1),
      axis.text.x = element_text(size = 9, color = "black", vjust = 0)
    )
  
  label_r <- 
    ggplot() +
    labs(title = "B") + 
    annotate(
      "text", label = paste(paste(pio_info$label)),
      x = rep(1, length(pio_info$label_position_r)), y = pio_info$label_position_r,     
      size = 3,
      family = "Gadugi",
      hjust = 0,
      vjust = -0.5,
      fontface = "bold",
      colour = "black"
    ) +
    scale_y_continuous(limits = pio_info$label_y_r, position = "right") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, vjust = 4, margin = margin(t = -5, r = -5, b = -5, l = -5, unit = "pt")))
  
  label_direction <- 
    ggplot() +
    annotate(
      "text", label = paste(c("Favours to","control","antidepressants")),
      x = c(0,-0.5,0.5), y = c(4,2,2),     
      size = 3,
      family = "Gadugi",
      hjust = c(0.5,1,0),
      vjust = 0,
      fontface = "bold",
      colour = "black"
    ) +
    geom_segment(aes(x = -.5, y = 1, xend = -1.5, yend = 1), arrow = arrow(length = unit(0.1, 'cm'))) +
    geom_segment(aes(x = 0.5, y = 1, xend = 1.5, yend = 1), arrow = arrow(length = unit(0.1, 'cm'))) +
    scale_y_continuous(limits = c(1, 10)) +
    scale_x_continuous(limits = c(-2, 22)) +
    theme_void()
  
  
  plot <- forest_m + incon_m + label_m + forest_r + incon_r + label_r + label_direction + plot_layout(design = pio_info$layout) 
  
  plot
  
  ggsave(
    #filename = paste0(pio, ".png"),
    filename = paste0(pio, ".tiff"),
    plot = last_plot(),
    dpi = 600,
    path = "figure",
    height = pio_info$height,
    width =  pio_info$width,
    bg = "white",
    #device = ragg::agg_png()
    device = "tiff"
  )

}

# Create plot to population - stratified 
generate_subgroup_plot(dfsubgroups, pio_info, pio = "population")
# Create plot to intervention - stratified 
generate_subgroup_plot(dfsubgroups, pio_info, pio = "intervention")
# Create plot to outcome - stratified 
generate_subgroup_plot(dfsubgroups, pio_info, pio = "outcome")

# META-REGRESSION -----

# age and weight (population), dose (intervention), water depth (outcome) and year
metareg_age_m <- rma(yi, vi, subset = species == "mice", mods = ~ age, data = Efeito)
metareg_age_r <- rma(yi, vi, subset = species == "rat", mods = ~ age, data = Efeito)
metareg_weight_m <- rma(yi, vi, subset = species == "mice", mods = ~ weight, data = Efeito)
metareg_weight_r <- rma(yi, vi, subset = species == "rat", mods = ~ weight, data = Efeito)
metareg_wd_m <- rma(yi, vi, subset = species == "mice", mods = ~ water_depth, data = Efeito)
metareg_wd_r <- rma(yi, vi, subset = species == "rat", mods = ~ water_depth, data = Efeito)
metareg_imi_dose_m <- rma(yi, vi, subset = species == "mice" & atd_type == "imipramine" & dose_unit == "mg/kg", mods = ~dose, data = Efeito) 
metareg_imi_dose_r <- rma(yi, vi, subset = species == "rat" & atd_type == "imipramine" & dose_unit == "mg/kg", mods = ~dose, data = Efeito)
metareg_flx_dose_m <- rma(yi, vi, subset = species == "mice" & atd_type == "fluoxetine" & dose_unit == "mg/kg", mods = ~dose, data = Efeito) 
metareg_flx_dose_r <- rma(yi, vi, subset = species == "rat" & atd_type == "fluoxetine" & dose_unit == "mg/kg", mods = ~dose, data = Efeito)
metareg_year_m <- rma(yi, vi, subset = species == "mice", mods = ~ year, data = Efeito) 
metareg_year_r <- rma(yi, vi, subset = species == "rat", mods = ~ year , data = Efeito) 

# Figure 

# Set color for each species
color_mice <- "#ff9400"
color_rat <- "#ec2b2b"

# Create a function to generate meta-regression plots
generate_metareg_plot <- function(metareg_model, colour, xlim, ylim, xlab, title = NULL, type = "single"){
  
  # Name of authors + year of studies included at the metareg
  study <-  ifelse(
    metareg_model$subset == TRUE,
    paste0(metareg_model$data$first_author, metareg_model$data$year),
    NA
  )
  # List studies included in the metareg
  study <- study[!is.na(study)] 
  
  # Create dataframe with meta-reg data
  
  metareg_model_df <- data.frame(
    yi = metareg_model$yi.f,
    X = metareg_model$X.f[,2],
    size = 1/metareg_model$vi.f,
    study = study
  ) 
  
  # Remove rows with NA on moderator 
  metareg_model_df <- metareg_model_df[complete.cases(metareg_model_df$X), ]
  
  plot <- ggplot(
    data = metareg_model_df,
    aes(y = yi, x = X)
  ) +  
    scale_x_continuous(lim = xlim) +
    scale_y_continuous(lim = ylim) +
    geom_smooth(method = "lm", colour = colour) + 
    geom_point(shape = 1, size = metareg_model_df$size, alpha = .5) +
    labs(x = xlab, y = "Effect size\n(Hedges'g)", title = title) + 
    theme_linedraw() +
    theme(plot.title = element_text(face = "bold", hjust = .5),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)) 
  
  return(plot)
  
}

# Figure 6
plot_A <- generate_metareg_plot(metareg_age_m, color_mice, xlim = c(0, 600), ylim = c(-2,65), xlab = "Age (days)", title = "Mice")
plot_B <- generate_metareg_plot(metareg_age_r, color_rat, xlim = c(0, 600), ylim = c(-2,25), xlab = "Age (days)", title = "Rat")
plot_C <- generate_metareg_plot(metareg_weight_m, color_mice, xlim = c(0, 40), ylim = c(-2,65), xlab = "Weight (g)")
plot_D <- generate_metareg_plot(metareg_weight_r, color_rat, xlim = c(0, 600), ylim = c(-2,25), xlab = "Weight (g)")
plot_E <- generate_metareg_plot(metareg_wd_m, color_mice, xlim = c(0, 55), ylim = c(-2,65), xlab = "Water depth (cm)")
plot_F <- generate_metareg_plot(metareg_wd_r, color_rat, xlim = c(0, 55), ylim = c(-2,25), xlab = "Water depth (cm)")
plot_G <- generate_metareg_plot(metareg_imi_dose_m, color_mice, xlim = c(0, 70), ylim = c(-2,65), xlab = "imipramine dose (mg/kg)")
plot_H <- generate_metareg_plot(metareg_imi_dose_r, color_rat, xlim = c(0, 70), ylim = c(-2,25), xlab = "imipramine dose (mg/kg)")
plot_I <- generate_metareg_plot(metareg_flx_dose_m, color_mice, xlim = c(0, 70), ylim = c(-2,65), xlab = "fluoxetine dose (mg/kg)")
plot_J <- generate_metareg_plot(metareg_flx_dose_r, color_rat, xlim = c(0, 70), ylim = c(-2,25), xlab = "fluoxetine dose (mg/kg)")
plot_K <- generate_metareg_plot(metareg_year_m, color_mice, xlim = c(1985, 2018), ylim = c(-2,65), xlab = "Year")
plot_L <- generate_metareg_plot(metareg_year_r, color_rat, xlim = c(1985, 2018), ylim = c(-2,25), xlab = "Year")

plot_figure6 <- plot_A + plot_B + plot_C + plot_D + plot_E + plot_F + plot_G + plot_H + plot_I + plot_J + plot_K + plot_L + plot_layout(ncol = 2, nrow = 6) + plot_annotation(tag_levels = "A") + theme(plot.tag = element_text(face = "bold"))

ggsave(
  #filename = "figure6.png",
  filename = "figure6.tiff",
  plot = last_plot(),
  dpi = 600,
  path = "figure",
  height = 12,
  width =  8,
  bg = "white",
  #device = ragg::agg_png()
  device = "tiff"
)

#  Quality ----
metareg_quali_m <- rma(yi, vi, subset = species == "mice", mods = ~ rob1 + rob2 + rob3 + rob4 + rob5 + rob6 + rob7 + rob8 + rob9 + rob10, data = Efeito) 
metareg_quali_r <- rma(yi, vi, subset = species == "rat", mods = ~ rob1 + rob2 + rob3 + rob4 + rob5 + rob6 + rob7 + rob8 + rob9 + rob10, data = Efeito) 

# Test robustness of models
metafor::permutest(metareg_quali_m)
metafor::permutest(metareg_quali_r)

# Get proportions across moderators levels
colMeans(model.matrix(metareg_quali_m))[-1]
colMeans(model.matrix(metareg_quali_r))[-1]

# Check number of studies per level
summary_df_mice <- df %>%
  filter(species == "mice") |> 
  gather(key = "variable", value = "value", rob1:rob10) %>%
  count(variable, value) %>%
  arrange(variable, desc(n)) 

summary_df_rat <- df %>%
  filter(species == "rat") |> 
  gather(key = "variable", value = "value", rob1:rob10) %>%
  count(variable, value) %>%
  arrange(variable, desc(n))

# STUDY QUALITY FIGURE ----

# SYRCLE RoB

# Isolate variables 
df_rob <- df |> 
  mutate(Study = str_c(first_author, ", ", year)) |> 
  select(starts_with("rob"), Study) 

df_rob <- df_rob |> 
  distinct() # keep one line per publication

df_rob <- df_rob |> 
  rename("Allocation sequence adequately generated and applied" = rob1,
         "Groups similar at baseline" = rob2,
         "Allocation adequately concealed" = rob3,
         "Animals randomly housed" = rob4, 
         "Investigators blinded during the experiment" = rob5, 
         "Animals selected at random for outcome assessment" = rob6,
         "Outcome assessor blinded" = rob7, 
         "Incomplete outcome data adequately addressed" = rob8, 
         "Free of selective outcome reporting" = rob9,
         "Free of other problems" = rob10) |> 
  relocate(Study, everything())

df_rob_long <- df_rob |> # put into long format
  pivot_longer(!c(Study),
               names_to = "pergunta",
               values_to = "atribuicao",
  ) 

# Rename levels
df_rob_long$atribuicao <-
  factor(
    df_rob_long$atribuicao,
    levels = c("Yes", "No", "Unclear"),
    labels = c("Low", "High", "Unclear") 
  )

df_rob_long$pergunta <-
  fct_relevel(
    df_rob_long$pergunta,"Allocation sequence adequately generated and applied",
    "Groups similar at baseline",
    "Allocation adequately concealed",
    "Animals randomly housed", 
    "Investigators blinded during the experiment", 
    "Animals selected at random for outcome assessment",
    "Outcome assessor blinded", 
    "Incomplete outcome data adequately addressed", 
    "Free of selective outcome reporting",
    "Free of other problems") 

v_factor_levels <- c("High", "Unclear", "Low")

robplot <- df_rob_long |> 
  group_by(Study) |> 
  distinct(Study, pergunta, atribuicao) |> 
  ggplot(aes(x = fct_rev(fct_infreq(pergunta)), fill = factor(atribuicao, levels = v_factor_levels), y = after_stat(count))) +
  geom_bar(position = "fill") + 
  scale_fill_manual("SYRCLE", values = c("Low" = "#82c236", "Unclear" = "#fec200", "High" = "#ec2b2b"), guide = guide_legend(
    title.position = "top")) +
  scale_y_continuous(labels = scales::percent, ) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 60)
  ) +
  coord_flip()  +
  theme_classic() + 
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line(size = .3),
    axis.text = element_text(size = 8,
                             color = "black"),
    axis.text.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 9),
    plot.title.position = "plot",
    legend.position = "right",
    legend.text = element_text(size = 6, color = "black"),
    legend.title = element_text(size = 7, hjust = 0, face = "bold"),
    legend.margin = margin(l = -15),
    plot.margin = margin(-5, 0, 0, 0),
    legend.key.size = unit(.8, "line")
  )

# Visualize SYRCLE RoB resume
robplot

# CAMARADES

# Isolate variables from CAMARADES
df_camarades <- df |> 
  mutate(Study = str_c(first_author, ", ", year)) |> 
  select(starts_with("camarades"), Study) 

df_camarades <- df_camarades |> 
  distinct() 

df_camarades <- df_camarades |> 
  rename("Study is a peer-reviewed publication" = camarades1,
         "Study following ARRIVE (or other) guidelines" = camarades2,
         "Compliance with animal testing regulations and legislation" = camarades3,
         "Declaration of interest" = camarades4, 
         "Report of husbandry conditions and improve animal welfare" = camarades5, 
         "Report of species and lineage of animals" = camarades6,
         "Report of phenotypes of interest" = camarades7, 
         "Report of the age, weight or stage of animals" = camarades8, 
         "Report of the sex of animals" = camarades9,
         "Report of the methods to acess the outcomes" = camarades10,
         "Report sample size calculation" = camarades11) 

df_camarades_longo <- df_camarades |> 
  pivot_longer(!c(Study),
               names_to = "pergunta",
               values_to = "atribuicao",
  ) 

df_camarades_longo$pergunta <- 
  fct_relevel(
    df_camarades_longo$pergunta, "Study is a peer-reviewed publication",
    "Study following ARRIVE (or other) guidelines",
    "Compliance with animal testing regulations and legislation",
    "Declaration of interest", 
    "Report of husbandry conditions and improve animal welfare", 
    "Report of species and lineage of animals",
    "Report of phenotypes of interest", 
    "Report of the age, weight or stage of animals", 
    "Report of the sex of animals",
    "Report of the methods to acess the outcomes",
    "Report sample size calculation")

df_camarades_longo$atribuicao <-  
  factor(
    df_camarades_longo$atribuicao,
    levels = c("No", "Yes", "Unclear", "Yes, ARRIVE", "Yes, lab animals", "Yes, no conflict"),
    labels = c("No", "Yes", "Incomplete", "Yes", "Yes", "Yes") 
  )

df_camarades_longo$atribuicao <- 
  fct_relevel(
    df_camarades_longo$atribuicao, "No", "Incomplete", "Yes")

c_factor_levels <- c("No", "Incomplete", "Yes") 

camaradesplot <- df_camarades_longo |> 
  group_by(Study) |> 
  distinct(Study, pergunta, atribuicao) |> 
  ggplot(aes(x = fct_rev(fct_infreq(pergunta)), fill = factor(atribuicao, levels = c_factor_levels), y = after_stat(count))) +
  geom_bar(position = "fill") + 
  scale_fill_manual("CAMARADES", values = c("Yes" = "#82c236", "Incomplete" = "#fec200", "No" = "#ec2b2b"), guide = guide_legend(
    title.position = "top")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 65)
  ) +
  coord_flip()  + 
  theme_classic() +
  theme(axis.ticks.x = element_line(size = .3),
        axis.ticks.y = element_blank(),
        axis.line = element_line(size = .3),
        axis.text = element_text(
          size = 8,
          color = "black"
        ),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 10),
        plot.title.position = "plot",
        legend.position = "right",
        legend.text = element_text(size = 6, color = "black"),
        legend.title = element_text(size = 7, hjust = 0, face = "bold"),
        plot.margin = margin(0, 0, 0, 0),
        legend.margin = margin(l = -15),
        legend.key.size = unit(.8, "line")
  )

quality <- robplot / camaradesplot + plot_layout(heights = c(5,5), width = 5)

# Save plot
ggsave(
  #filename = "figure2.png",
  filename = "figure2.tiff",
          plot = quality,
          dpi = 600,
          path = "figure",
          height = 4,
          device = "tiff"
          #device = ragg::agg_png()
          )

