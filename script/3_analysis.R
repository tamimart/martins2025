
# load packages

library(tidyverse) # data wrangling
library(bitops)    # operators
library(metafor)   # meta-analysis
library(Formula)
library(readxl)    # read excel file
library(writexl)   # save excel
library(extrafont) # extra font
library(cowplot)   # plot annotation and alignment
library(metapower) # power calculation
library(lubridate) # date manipulation
library(weightr)   # test pub bias
library(patchwork) # to join plots



# power ----

# create function to convert hedges g to cohen's d

hedgesg_to_cohensd <- function(hedgesg, n) {
  cohensd <- hedgesg / (1 - 3 / (4 * (n) - 9))
  return(cohensd)
}

# calculate general power

hedgesg_to_cohensd(hedgesg = 0.5, n = 12) # converter hedges g para cohens d

poder_geral <- kmestrado <- mpower(effect_size = .54166667, study_size = 6, k = 200, i2 = .90, es_type = "d") # calculate power

print(poder_geral)
plot_mpower(poder_geral)


# data treatment ----


df <- read_excel("data/Data_200FST.xlsx") # load df

# change date type to numeric

df <- df %>% 
  mutate(year = as.numeric(format(as.Date(df$year, format = "%d/%m/%Y"),"%Y"))) 

# Calculate effect size in SDM hedges g

Efeito <- escalc(measure = "SMD", n1i = ctr_n_corr, n2i = atd_n_round, m1i = ctr_mean, m2i = atd_mean, 
                 sd1i = ctr_sd, sd2i = atd_sd, data = df, 
                 append = TRUE)


# Meta-analysis by random effects model ----

Teste <- rma(yi, vi, data = Efeito, slab = (paste(Efeito$first_author, as.character(Efeito$year), sep = ", ")))


Teste

# Generate confidence  and prediction interval

predict(Teste, digits = 3)


# Plot e save forestplot 

pdf("figure/floresta_all.pdf", height = 120, width = 25)

floresta <- forest(
  Teste,
  cex = 1,
  ylim = c(-2, 567),
  slab = (paste(
    Efeito$first_author, as.character(Efeito$year), sep = ", "
  )),
  mlab = "",
  order = Efeito$yi,
  xlab = "Hedges g",
  xlim =  c(-40, 40),
  showweight = T,
  cex.lab = 2,
  cex.axis = 1.5,
  col = "blue",
  border = "blue",
  fonts = "sans"
)

# Adicionar textos


op <- par(cex = 0.75, font = 2, family = "sans")
text(c(-6, 7.75),     568, font = 2.5,
     cex = 2, c("Control", "Antidepressant"))
text(c(90),
     568,
     font = 2.5,
     cex = 2,
     c("Weights Hedges g [95% CI]"))
text(c(-35),
     568,
     font = 2.5,
     cex = 2,
     c("Author(s), year"))

text(-40, -1, pos = 4, cex = 2, bquote(paste("RE Model (Q = ", .(formatC(Teste$QE, digits = 2, format = "f")),
                                             ", df = ", .(Teste$k - Teste$p),
                                             ", p ", .(metafor:::.pval(Teste$QEp, digits = 2, showeq = TRUE, sep = " ")), "; ",
                                             I^2, " = ", .(formatC(Teste$I2, digits = 1, format = "f")), "%, ",
                                             tau^2, " = ", .(formatC(Teste$tau2, digits = 2, format = "f")), ")")))

dev.off() 

# Sensitivity analysis ----

# See which studies are influencing in different aspects 

png("figure/influence.png")

inf <- influence(Teste)
plot(inf)
dev.off()

tinf <- print(inf) # create table with results
tinf$id <- Efeito$line # add id clumn
tinf$sr <- Efeito$study_reference # add reference column
write_xlsx(tinf,"data/influence.xlsx") # save as excel file



leave1 <- leave1out(Teste, digits = 3) # put results into an object
leave1df <- as.data.frame(leave1) # transf object list into df
final_df <- as.data.frame(t(leave1df)) # invert lines and columns
copia_final_df <- final_df # create copy
copia_final_df$rn <- row.names(final_df) # add row name as column in copied df
copia_final_df <- copia_final_df %>%
  select(rn, everything()) # bring column of names to front of df

write_xlsx(copia_final_df, "data/leave.xlsx") # save

# Publication Bias Analysis ----

regtest(Teste, model = "rma", predictor = "sei")
regtest(Teste, model = "rma", predictor = "sqrtninv")

Teste_mice <- rma(yi, vi, subset = (species == "mice"), data = Efeito)
regtest(Teste_mice, model = "rma", predictor = "sei")
regtest(Teste_mice, model = "rma", predictor = "sqrtninv")

Teste_rat <- rma(yi, vi, subset = (species == "rat"), data = Efeito)
regtest(Teste_rat, model = "rma", predictor = "sei")
regtest(Teste_rat, model = "rma", predictor = "sqrtninv")

# [trim and fill]

missing <-
  metafor::trimfill(
    Teste,
    side = "left",
    estimator = "R0",
    maxiter = 100,
    verbose = FALSE
  ) #R0 preferable when the MA has >k Rothstein HR, Sutton AJ, Borenstein M. Publication Bias in Meta-Analysis: Prevention, Assessment and Adjustments. Chichester, UK: John Wiley & Sons; 2005. An advantage of estimator "R0" is that it provides a test of the null hypothesis that the number of missing studies (on the chosen side) is zero

missing_m <-
  metafor::trimfill(
    Teste_mice,
    side = "left",
    estimator = "R0",
    maxiter = 100,
    verbose = FALSE
  )

missing_r <-
  metafor::trimfill(
    Teste_rat,
    side = "left",
    estimator = "R0",
    maxiter = 100,
    verbose = FALSE
  )

missing
missing_m
missing_r 


png("figure/funil.png", height = 800, width = 800)

par(mfrow = c(3, 2), oma = c(1,1,1,1), mar = c(4,5,3,1), cex = .8, font = 2, family = "sans")

funil_global1 <- metafor::funnel(
  missing,
  yaxis = "sei",
  addtau2 = FALSE,
  main = "Global",
  xlab = "Effect size",
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


funil_global2 <- metafor::funnel(
  missing,
  yaxis = "sqrtninv",
  addtau2 = FALSE,
  main = "Global",
  xlab = "Effect size",
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

funil_m1 <- metafor::funnel(
  missing_m,
  yaxis = "sei",
  addtau2 = FALSE,
  main = "Mice",
  xlab = "Effect Size",
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


funil_m2 <- metafor::funnel(
  missing_m,
  yaxis = "sqrtninv",
  addtau2 = FALSE,
  main = "Mice",
  xlab = "Effect size",
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

funil_r1 <- metafor::funnel(
  missing_r,
  yaxis = "sei",
  addtau2 = FALSE,
  main = "Rat",
  xlab = "Effect size",
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



funil_r2 <- metafor::funnel(
  missing_r,
  yaxis = "sqrtninv",
  addtau2 = FALSE,
  main = "Rat",
  xlab = "Effect size",
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


dev.off()



# [weight function model]
# Specific publication bias test - 
# increases the weight of studies that are less likely to be published and 
# decreases the weight of those that are more likely to be published - based on p-value

# likehood test alfa = 0.10

# global

wf_global <- weightfunct(Efeito$yi, Efeito$vi, table = TRUE, steps = .05)

# mice
wmice <- Efeito %>% 
  filter(species == "mice")

wf_mice <- weightfunct(wmice$yi, wmice$vi, table = TRUE, steps = 0.05)
wf_mice

# rat
wrat <- Efeito %>% 
  filter(species == "rat")

wf_rat <- weightfunct(wrat$yi, wrat$vi, table = TRUE, steps = 0.05)
wf_rat


# Subgroup analysis ----

# [POPULATION]------
#species [mice] ----

Teste_mice <- rma(yi, vi, subset = (species == "mice"), data = Efeito)
Teste_mice

#sex

Teste_macho_m <- rma(yi, vi, subset = (sex == "M" & species == "mice"), data = Efeito)
Teste_macho_m

Teste_femea_m <- rma(yi, vi, subset = (sex == "F" & species == "mice"), data = Efeito)
Teste_femea_m

Teste_sexoambos_m <- rma(yi, vi, subset = (sex == "M and F" & species == "mice"), data = Efeito)
Teste_sexoambos_m


Efeito %>% 
  filter(sex == "M and F" & species == "mice") %>% 
  select(authors) # same publication?


Teste_sexoind_m <- rma(yi, vi, subset = (sex == "NA" & species == "mice"), data = Efeito)
Teste_sexoind_m

Efeito %>% 
  filter(sex == "NA" & species == "mice") %>% 
  select(authors) # same publication?

#strain

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

Efeito %>% 
  filter(strain == "ddY") %>% 
  select(authors) # same publication?

Teste_laca <- rma(yi, vi, subset = (strain == "laca" & species == "mice"), data = Efeito)
Teste_laca

Efeito %>% 
  filter(strain == "laca") %>% 
  select(authors) # same publication?

Teste_OF1 <- rma(yi, vi, subset = (strain == "OF1" & species == "mice"), data = Efeito)
Teste_OF1

Efeito %>% 
  filter(strain == "OF1") %>% 
  select(authors) # same publication?

Teste_NMRI <- rma(yi, vi, subset = (strain == "NMRI" & species == "mice"), data = Efeito)
Teste_NMRI

Efeito %>% 
  filter(strain == "NMRI") %>% 
  select(authors) # same publication?

Teste_sabra <- rma(yi, vi, subset = (strain == "sabra" & species == "mice"), data = Efeito)
Teste_sabra

Efeito %>% 
  filter(strain == "sabra") %>% 
  select(authors) # same publication?

Teste_BKTO <- rma(yi, vi, subset = (strain == "BKTO" & species == "mice"), data = Efeito)
Teste_BKTO

Efeito %>% 
  filter(strain == "BKTO") %>% 
  select(authors) # same publication?

Teste_NA_m <- rma(yi, vi, subset = (strain == "NA" & species == "mice"), data = Efeito)
Teste_NA_m

Efeito %>% 
  filter(strain == "NA") %>% 
  select(authors) # same publication?

Teste_DBA2 <- rma(yi, vi, subset = (strain == "DBA/2" & species == "mice"), data = Efeito)
Teste_DBA2

Efeito %>% 
  filter(strain == "DBA/2") %>% 
  select(authors) # same publication?

Teste_B6SJL <- rma(yi, vi, subset = (strain == "B6SJL (R406W transgenic)" & species == "mice"), data = Efeito)
Teste_B6SJL 

Efeito %>% 
  filter(strain == "B6SJL (R406W transgenic)") %>% 
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


# species [rat] ----

Teste_rat <- rma(yi, vi, subset = (species == "rat"), data = Efeito)
Teste_rat

#sex


Teste_macho_r <- rma(yi, vi, subset = (sex == "M" & species == "rat"), data = Efeito)
Teste_macho_r

Teste_femea_r <- rma(yi, vi, subset = (sex == "F" & species == "rat"), data = Efeito)
Teste_femea_r

Teste_sexoambos_r <- rma(yi, vi, subset = (sex == "M and F" & species == "rat"), data = Efeito)
Teste_sexoambos_r

Efeito %>% 
  filter(sex == "M and F" & species == "rat") %>% 
  select(authors) # same publication?

Teste_sexoind_r <- rma(yi, vi, subset = (sex == "NA" & species == "rat"), data = Efeito)
Teste_sexoind_r

Efeito %>% 
  filter(sex == "NA" & species == "rat") %>% 
  select(authors) # same publication?

#strain

Teste_wistar <- rma(yi, vi, subset = (strain == "wistar" & species == "rat"), data = Efeito)
Teste_wistar

Teste_sd <- rma(yi, vi, subset = (strain == "sprague dawley" & species == "rat"), data = Efeito)
Teste_sd

Teste_LE <- rma(yi, vi, subset = (strain == "long-evans" & species == "rat"), data = Efeito)
Teste_LE

Efeito %>% 
  filter(strain == "long-evans" & species == "rat") %>% 
  select(authors) # same publication?

Teste_FS <- rma(yi, vi, subset = (strain == "flinders sensitive" & species == "rat"), data = Efeito)
Teste_FS

Efeito %>% 
  filter(strain == "flinders sensitive" & species == "rat") %>% 
  select(authors) # same publication?

Teste_CDCOBS <- rma(yi, vi, subset = (strain == "CD-COBS" & species == "rat"), data = Efeito)
Teste_CDCOBS

Efeito %>% 
  filter(strain == "CD-COBS" & species == "rat") %>% 
  select(authors) # same publication?

Teste_WK <- rma(yi, vi, subset = (strain == "wistar kyoto" & species == "rat"), data = Efeito)
Teste_WK 

Efeito %>% 
  filter(strain == "wistar kyoto" & species == "rat") %>% 
  select(authors) # same publication?

Teste_FR <- rma(yi, vi, subset = (strain == "flinders resistant" & species == "rat"), data = Efeito)
Teste_FR

Efeito %>% 
  filter(strain == "flinders resistant" & species == "rat") %>% 
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

Efeito %>% 
  filter(bioterium_lightcycle == "NA" & species == "rat") %>% 
  select(authors) # same publication?

natural_r <- rma(yi, vi, subset = (bioterium_lightcycle == "natural" & species == "rat"), data = Efeito)
natural_r 

inverso_r <- rma(yi, vi, subset = (bioterium_lightcycle == "12/12 reverse" & species == "rat"), data = Efeito)
inverso_r

Efeito %>% 
  filter(bioterium_lightcycle == "12/12 reverse" & species == "rat") %>% 
  select(authors) # same publication?

dez_q_r <- rma(yi, vi, subset = (bioterium_lightcycle == "10/14" & species == "rat"), data = Efeito)
dez_q_r 

Efeito %>% 
  filter(bioterium_lightcycle == "10/14" & species == "rat") %>% 
  select(authors) # same publication?


# [INTERVENTION]------ 
# [mice] -----

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

Efeito %>% 
  filter(atd_type == "clomipramine" & species == "mice") %>% 
  select(authors) # same publication?

Teste_nor_m <- rma(yi, vi, subset = (atd_type == "nortriptyline" & species == "mice"), data = Efeito)
Teste_nor_m

Efeito %>% 
  filter(atd_type == "nortriptyline" & species == "mice") %>% 
  select(authors) # same publication?

#SSRI

Teste_SSRI_m <- rma(yi, vi, subset = (atd_class == "SSRI" & species == "mice"), data = Efeito)
Teste_SSRI_m

Teste_flu_m <- rma(yi, vi, subset = (atd_type == "fluoxetine" & species == "mice"), data = Efeito)
Teste_flu_m

Teste_par_m <- rma(yi, vi, subset = (atd_type == "paroxetine" & species == "mice"), data = Efeito)
Teste_par_m

Efeito %>% 
  filter(atd_type == "paroxetine" & species == "mice") %>% 
  select(authors) # same publication?

Teste_esc_m <- rma(yi, vi, subset = (atd_type == "escitalopram" & species == "mice"), data = Efeito)
Teste_esc_m

Efeito %>% 
  filter(atd_type == "escitalopram" & species == "mice") %>% 
  select(authors) # same publication?

Teste_cit_m <- rma(yi, vi, subset = (atd_type == "citalopram" & species == "mice"), data = Efeito)
Teste_cit_m

Efeito %>% 
  filter(atd_type == "citalopram" & species == "mice") %>% 
  select(authors) # same publication?

Teste_fluv_m <- rma(yi, vi, subset = (atd_type == "fluvoxamine" & species == "mice"), data = Efeito)
Teste_fluv_m

Efeito %>% 
  filter(atd_type == "fluvoxamine" & species == "mice") %>% 
  select(authors) # same publication?

#SNRI

Teste_SNRI_m <- rma(yi, vi, subset = (atd_class == "SNRI" & species == "mice"), data = Efeito, control = list(stepadj = 0.5, maxiter = 1000)) # add parametros para ajustar comprimento do passo e maximo de iterações para a convergencia ocorrer (https://stackoverflow.com/questions/68817204/why-did-the-fisher-scoring-algorithm-not-converge-after-adjusting)
Teste_SNRI_m

Teste_ven_m <- rma(yi, vi, subset = (atd_type == "venlafaxine" & species == "mice"), data = Efeito)
Teste_ven_m

Efeito %>% 
  filter(atd_type == "venlafaxine" & species == "mice") %>% 
  select(authors) # same publication?

Teste_tra_m <- rma(yi, vi, subset = (atd_type == "tramadol" & species == "mice"), data = Efeito)
Teste_tra_m

Efeito %>% 
  filter(atd_type == "tramadol" & species == "mice") %>% 
  select(authors) # same publication?

#IMAO

Teste_IMAO_m <- rma(yi, vi, subset = (atd_class == "IMAO" & species == "mice"), data = Efeito)
Teste_IMAO_m

Teste_sel_m <- rma(yi, vi, subset = (atd_type == "selegiline" & species == "mice"), data = Efeito)
Teste_sel_m

Efeito %>% 
  filter(atd_type == "selegiline" & species == "mice") %>% 
  select(authors) # same publication?

Teste_moc_m <- rma(yi, vi, subset = (atd_type == "moclobemide" & species == "mice"), data = Efeito)
Teste_moc_m

Efeito %>% 
  filter(atd_type == "moclobemide" & species == "mice") %>% 
  select(authors) # same publication?

#NDRI

Teste_NDRI_m <- rma(yi, vi, subset = (atd_class == "NDRI" & species == "mice"), data = Efeito)
Teste_NDRI_m

#TECA

Teste_TeCA_m <- rma(yi, vi, subset = (atd_class == "teca" & species == "mice"), data = Efeito)
Teste_TeCA_m

Teste_map_m <- rma(yi, vi, subset = (atd_type == "maprotiline" & species == "mice"), data = Efeito)
Teste_map_m

Efeito %>% 
  filter(atd_type == "maprotiline" & species == "mice") %>% 
  select(authors) # same publication?

Teste_mia_m <- rma(yi, vi, subset = (atd_type == "mianserin" & species == "mice"), data = Efeito)
Teste_mia_m

Efeito %>% 
  filter(atd_type == "mianserin" & species == "mice") %>% 
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

Efeito %>% 
  filter(treatment_via == "NA" & species == "mice") %>% 
  select(authors) # same publication?

# [rat] -----
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

Efeito %>% 
  filter(atd_type == "clomipramine" & species == "rat") %>% 
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

Efeito %>% 
  filter(atd_type == "paroxetine" & species == "rat") %>% 
  select(authors) # same publication?

Teste_fluv_r <- rma(yi, vi, subset = (atd_type == "fluvoxamine" & species == "rat"), data = Efeito)
Teste_fluv_r

Efeito %>% 
  filter(atd_type == "fluvoxamine" & species == "rat") %>% 
  select(authors) # same publication?

Teste_cit_r <- rma(yi, vi, subset = (atd_type == "citalopram" & species == "rat"), data = Efeito)
Teste_cit_r

Efeito %>% 
  filter(atd_type == "citalopram" & species == "rat") %>% 
  select(authors) # same publication?

Teste_esc_r <- rma(yi, vi, subset = (atd_type == "escitalopram" & species == "rat"), data = Efeito)
Teste_esc_r

Efeito %>% 
  filter(atd_type == "escitalopram" & species == "rat") %>% 
  select(authors) # same publication?


#SNRI

Teste_SNRI_r <- rma(yi, vi, subset = (atd_class == "SNRI" & species == "rat"), data = Efeito)
Teste_SNRI_r

Teste_ven_r <- rma(yi, vi, subset = (atd_type == "venlafaxine" & species == "rat"), data = Efeito)
Teste_ven_r

Teste_desv_r <- rma(yi, vi, subset = (atd_type == "desvenlafaxine" & species == "rat"), data = Efeito)
Teste_desv_r

Efeito %>% 
  filter(atd_type == "desvenlafaxine" & species == "rat") %>% 
  select(authors) # same publication?

Teste_reb_r <- rma(yi, vi, subset = (atd_type == "reboxetine" & species == "rat"), data = Efeito)
Teste_reb_r

Efeito %>% 
  filter(atd_type == "reboxetine" & species == "rat") %>% 
  select(authors) # same publication?

Teste_sib_r <- rma(yi, vi, subset = (atd_type == "sibutramine" & species == "rat"), data = Efeito)
Teste_sib_r

Efeito %>% 
  filter(atd_type == "sibutramine" & species == "rat") %>% 
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


Efeito %>% 
  filter(treatment_via == "microinjection (dorsal hippocampus)" & species == "rat") %>% 
  select(authors) # same publication?


Teste_od_r <- rma(yi, vi, subset = (treatment_via == "oral (dietary treatment)" & species == "rat"), data = Efeito)
Teste_od_r

Efeito %>% 
  filter(treatment_via == "oral (dietary treatment)" & species == "rat") %>% 
  select(authors) # same publication?

Teste_in_r <- rma(yi, vi, subset = (treatment_via == "intranasal" & species == "rat"), data = Efeito)
Teste_in_r


Efeito %>% 
  filter(treatment_via == "intranasal" & species == "rat") %>% 
  select(authors) # same publication?

Teste_viaNA_r <- rma(yi, vi, subset = (treatment_via == "NA" & species == "rat"), data = Efeito)
Teste_viaNA_r # k < 3

# [OUTCOME] ------
# [mice]----

#protocol


fst_pro_m <- Efeito %>% 
  filter(species == "mice") %>% 
  group_by(fst_protocol) %>% 
  summarise(soma = n()) %>% 
  arrange(desc(soma)) # see which protocols were used and leave only those used by at least 3 studies


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

Efeito %>% 
  filter(fst_protocol == "pre15test5" & species == "mice") %>% 
  select(authors) # same publication?

Teste_T5_m <- rma(yi, vi, subset = (fst_protocol == "test5" & species == "mice"), data = Efeito)
Teste_T5_m

Efeito %>% 
  filter(fst_protocol == "test5" & species == "mice") %>% 
  select(authors) # same publication?

Teste_PT5T5_m <- rma(yi, vi, subset = (fst_protocol == "pre5test5" & species == "mice"), data = Efeito)
Teste_PT5T5_m

Efeito %>% 
  filter(fst_protocol == "pre5test5" & species == "mice") %>% 
  select(authors) # same publication?

Teste_T5S4_m <- rma(yi, vi, subset = (fst_protocol == "test5score4" & species == "mice"), data = Efeito)
Teste_T5S4_m

Efeito %>% 
  filter(fst_protocol == "test5score4" & species == "mice") %>% 
  select(authors) # same publication?

Teste_T7S6_m <- rma(yi, vi, subset = (fst_protocol == "test7score6" & species == "mice"), data = Efeito)
Teste_T7S6_m

Efeito %>% 
  filter(fst_protocol == "test7score6" & species == "mice") %>% 
  select(authors) # same publication?

Teste_T6S5_m <- rma(yi, vi, subset = (fst_protocol == "test6score5" & species == "mice"), data = Efeito)
Teste_T6S5_m

Efeito %>% 
  filter(fst_protocol == "test6score5" & species == "mice") %>% 
  select(authors) # same publication?

Teste_T9_m <- rma(yi, vi, subset = (fst_protocol == "test9" & species == "mice"), data = Efeito)
Teste_T9_m

Efeito %>% 
  filter(fst_protocol == "test9" & species == "mice") %>% 
  select(authors) # same publication?

Teste_PT15T6S5_m <- rma(yi, vi, subset = (fst_protocol == "pre15test6score5" & species == "mice"), data = Efeito)
Teste_PT15T6S5_m

Efeito %>% 
  filter(fst_protocol == "pre15test6score5" & species == "mice") %>% 
  select(authors) # same publication?

Teste_T10_m <- rma(yi, vi, subset = (fst_protocol == "test10" & species == "mice"), data = Efeito)
Teste_T10_m

Efeito %>% 
  filter(fst_protocol == "test10" & species == "mice") %>% 
  select(authors) # same publication?

Teste_PT15TNA_m <- rma(yi, vi, subset = (fst_protocol == "pre15test?" & species == "mice"), data = Efeito)
Teste_PT15TNA_m

Efeito %>% 
  filter(fst_protocol == "pre15test?" & species == "mice") %>% 
  select(authors) # same publication?

Teste_T15S5_m <- rma(yi, vi, subset = (fst_protocol == "test15score5" & species == "mice"), data = Efeito)
Teste_T15S5_m

Efeito %>% 
  filter(fst_protocol == "test15score5" & species == "mice") %>% 
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

# [rat] ----

#protocol

fst_pro_r <- Efeito %>% 
  filter(species == "rat") %>% 
  group_by(fst_protocol) %>% 
  summarise(soma = n()) %>% 
  arrange(desc(soma)) # ver quais protocolos foram usados e deixar só com pelo menos 3 estudos


Teste_PT15T5_r <- rma(yi, vi, subset = (fst_protocol == "pre15test5" & species == "rat"), data = Efeito)
Teste_PT15T5_r

Teste_T5_r <- rma(yi, vi, subset = (fst_protocol == "test5" & species == "rat"), data = Efeito)
Teste_T5_r

Teste_PT13T6_r <- rma(yi, vi, subset = (fst_protocol == "pre13test6" & species == "rat"), data = Efeito)
Teste_PT13T6_r

Efeito %>% 
  filter(fst_protocol == "pre13test6" & species == "rat") %>% 
  select(authors) # same publication?

Teste_PTNAT6S4_r <- rma(yi, vi, subset = (fst_protocol == "pre?test6score4" & species == "rat"), data = Efeito)
Teste_PTNAT6S4_r

Efeito %>% 
  filter(fst_protocol == "pre?test6score4" & species == "rat") %>% 
  select(authors) # same publication?

Teste_PT5T5_r <- rma(yi, vi, subset = (fst_protocol == "pre5test5" & species == "rat"), data = Efeito)
Teste_PT5T5_r

Efeito %>% 
  filter(fst_protocol == "pre5test5" & species == "rat") %>% 
  select(authors) # same publication?

Teste_T15_r <- rma(yi, vi, subset = (fst_protocol == "test15" & species == "rat"), data = Efeito)
Teste_T15_r

Efeito %>% 
  filter(fst_protocol == "test15" & species == "rat") %>% 
  select(authors) # same publication?

Teste_PT15T6S5_r <- rma(yi, vi, subset = (fst_protocol == "pre15test6score5" & species == "rat"), data = Efeito)
Teste_PT15T6S5_r

Efeito %>% 
  filter(fst_protocol == "pre15test6score5" & species == "rat") %>% 
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

# Forestplot subgroups ------

# I tabulated the results of the subgroups in an excel spreadsheet

dfsubgrupos <- read_excel("data/subgruposppt.xlsx")
dfsubgrupos <- dfsubgrupos %>% 
  rename(IC95LL = `IC95-L`,
         IC95UL = `IC95-U`,
         inconsistency = `I² (%)`)

dfsubgrupos$tau2 <- round(dfsubgrupos$tau2, digits = 1)

dfsubgrupos$moderator <-
  factor(
    dfsubgrupos$moderator,
    levels =  c(
      "Mice",
      "Rat",
      "Sex",
      "Strain",
      "Stress",
      "Light cycle",
      "All TCA",
      "All SSRI",
      "All SNRI",
      "All MAOI",
      "MAOI",
      "All NDRI",
      "NDRI",
      "All TeCA",
      "Route of administration",
      "FST Protocol",
      "Analysis method",
      "Tests before FST"))

dfsubgrupos$category <-
  factor(
    dfsubgrupos$category,
    levels =  c("Mice",
                "Male",
                "Female",
                "Both sexes",
                "No info",
                "Swiss",
                "CD-1",
                "C57BL",
                "ddY",
                "BALB",
                "LACA",
                "OF1",
                "NMRI",
                "Sabra",
                "BKTO",
                "DBA/2",
                "B6SJL (R406W)",
                "Stress",
                "No stress",
                "Rat",
                "Wistar",
                "Sprague Dawley",
                "Long Evans",
                "Flinders sensitive",
                "CD-COBS",
                "Wistar kyoto",
                "Flinders resistant",
                "12/12 normal",
                "12/12",
                "Natural",
                "12/12 reverse",
                "11/14",
                "All TCA",
                "imipramine",
                "desipramine",
                "amitriptiline",
                "clomipramine",
                "nortriptiline",
                "All SSRI",
                "fluoxetine",
                "sertraline",
                "paroxetine",
                "escitalopram",
                "citalopram",
                "fluvoxamine",
                "All SNRI",
                "venlafaxine",
                "tramadol",
                "desvenlafaxine",
                "reboxetine",
                "sibutramine",
                "All MAOI",
                "selegiline",
                "moclobemide",
                "bupropion",
                "All TeCA",
                "maprotiline",
                "mianserin",
                "amoxapine",
                "Intraperitoneal",
                "Oral",
                "Gavage",
                "Subcutaneous",
                "Microinjection",
                "Oral (food)",
                "Intranasal",
                "T6’ + S4’",
                "T6’",
                "PT15’ + T6’ + S4’",
                "PT15’ + T6’",
                "PT15’ + T5’",
                "T5’",
                "PT13’ + T6’",
                "PT5’ + T5’",
                "T5’ + S4’",
                "T7’ + S6’",
                "T6’ + S5’",
                "T9’",
                "T10’",
                "PT15’ + T?",
                "T15’ + S5’",
                "Video analysis",
                "Manual",
                "No",
                "Yes",
                "T15’", 
                "PT?’ + T6’ + S4’",
                "PT15’ + T6’ + S5’"))


theme_set(theme_minimal(base_family = "Gadugi"))


#populacao

ppt_sub_pop_c <- dfsubgrupos %>%
  filter(species == "Mice",
         type == "Population") %>%
  ggplot(aes(
    x = category,
    y = GES,
    ymin = IC95LL,
    ymax = IC95UL,
    color = "#ff9400"
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
  scale_y_continuous(limits = c(-1, 18)) +
  labs(x = "", y = "Effect Size") +
  scale_colour_manual(values = "#ff9400") +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  geom_text(
    aes(label = paste(
      "k = ",
      k,
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
    axis.text.y = element_text(size = 9, color = "black")
)

ppt_sub_pop_c_i <- dfsubgrupos %>%
  filter(species == "Mice",
         type == "Population") %>%
  ggplot(aes(
    x = category,
    y = inconsistency,
    fill = "#ff9400"
  )) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 200), position = "right") +
  labs(x = "", y = "I² (%) |𝜏² ") +
  scale_fill_manual(values = "#ff9400") +
  geom_hline(yintercept = 100, lty = 1, size = .2, color = "black") +
  geom_text(
    aes(label = tau2),
    y = 104,
    color = "black",
    size = 3,
    family = "Gadugi",
    hjust = -0.1
  ) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title = element_text(size = 9, color = "black", vjust = 400, hjust = 0.1))



sub_pop_c <- ppt_sub_pop_c + ppt_sub_pop_c_i + plot_layout(widths = c(6, 1))


save_plot(filename = "ppt_sub_pop_c.png",
          plot = sub_pop_c,
          dpi = 300,
          path = "figure")



ppt_sub_pop_r <- dfsubgrupos %>%
  filter(species == "Rat",
         type == "Population") %>%
  ggplot(aes(
    x = category,
    y = GES,
    ymin = IC95LL,
    ymax = IC95UL,
    color = "#ec2b2b"
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
  scale_y_continuous(limits = c(-1, 18)) +
  labs(x = "", y = "Effect size") +
  scale_colour_manual(values = "#ec2b2b") +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  geom_text(
    aes(label = paste(
      "k = ",
      k,
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
    axis.text = element_text(size = 9, color = "black")
  )

ppt_sub_pop_r_i <- dfsubgrupos %>%
  filter(species == "Rat",
         type == "Population") %>%
  ggplot(aes(
    x = category,
    y = inconsistency,
    fill = "#ec2b2b"
  )) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 200), breaks = c(0, 100)) +
  labs(x = "", y = "I² (%) |𝜏² ") +
  scale_fill_manual(values = "#ec2b2b") +
  geom_hline(yintercept = 100, lty = 1, size = .2, color = "black") +
  geom_text(
    aes(label = tau2),
    y = 104,
    color = "black",
    size = 3,
    family = "Gadugi",
    hjust = -0.1
  ) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title = element_text(size = 9, color = "black", hjust = -0.2),
    axis.text.x = element_text(size = 9, color = "black", vjust = -2)
  )


sub_pop_r <- ppt_sub_pop_r + ppt_sub_pop_r_i + plot_layout(widths = c(6, 1))

save_plot(filename = "ppt_sub_pop_r.png",
          plot = sub_pop_r,
          dpi = 300,
          path = "figure")


#intervencao 

ppt_sub_int_c <- dfsubgrupos %>%
  filter(species == "Mice",
         type == "Intervention") %>%
  ggplot(aes(
    x = category,
    y = GES,
    ymin = IC95LL,
    ymax = IC95UL,
    color = "#ff9400"
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
  scale_y_continuous(limits = c(-2, 18)) +
  labs(x = "", y = "") +
  scale_colour_manual(values = "#ff9400") +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  geom_text(
    aes(label = paste(
      "k = ",
      k,
      sep = ""
    )),
    y = Inf - 1,
    color = "black",
    size = 2.5,
    family = "Gadugi",
    hjust = 1
  ) +
  coord_flip() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 7, color = "black")
  )

ppt_sub_int_c_i <- dfsubgrupos %>%
  filter(species == "Mice",
         type == "Intervention") %>%
  ggplot(aes(
    x = category,
    y = inconsistency,
    fill = "#ff9400"
  )) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 200)) +
  labs(x = "", y = "") +
  scale_fill_manual(values = "#ff9400") +
  geom_hline(yintercept = 100, lty = 1, size = .2, color = "black") +
  geom_text(
    aes(label = tau2),
    y = 104,
    color = "black",
    size = 2.5,
    family = "Gadugi",
    hjust = -0.1
  ) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
    )
  


sub_int_c <- ppt_sub_int_c + ppt_sub_int_c_i + plot_layout(widths = c(6, 1))


save_plot(filename = "ppt_sub_int_c.png",
          plot = sub_int_c,
          dpi = 300,
          path = "figure")


ppt_sub_int_r <- dfsubgrupos %>%
  filter(species == "Rat",
         type == "Intervention") %>%
  ggplot(aes(
    x = category,
    y = GES,
    ymin = IC95LL,
    ymax = IC95UL,
    color = "#ec2b2b"
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
  scale_y_continuous(limits = c(-2, 18)) +
  labs(x = "", y = "Effect size") +
  scale_colour_manual(values = "#ec2b2b") +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  geom_text(
    aes(label = paste(
      "k = ",
      k,
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
    axis.title.y = element_text(size = 9, color = "black", vjust = -1),
    axis.text = element_text(size = 9, color = "black")
  )



ppt_sub_int_r_i <- dfsubgrupos %>%
  filter(species == "Rat",
         type == "Intervention") %>%
  ggplot(aes(
    x = category,
    y = inconsistency,
    fill = "#ec2b2b"
  )) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 200), breaks = c(0, 100)) +
  labs(x = "", y = "I² (%) |𝜏² ") +
  scale_fill_manual(values = "#ec2b2b") +
  geom_hline(yintercept = 100, lty = 1, size = .2, color = "black") +
  geom_text(
    aes(label = tau2),
    y = 104,
    color = "black",
    size = 3,
    family = "Gadugi",
    hjust = -0.1
  ) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title = element_text(size = 9, color = "black", hjust = -0.15),
    axis.text.x = element_text(size = 9, color = "black", vjust = -2)
  )


sub_int_r <- ppt_sub_int_r + ppt_sub_int_r_i + plot_layout(widths = c(6, 1))

save_plot(filename = "ppt_sub_int_r.png",
          plot = sub_int_r,
          dpi = 300,
          path = "figure")


#desfecho

ppt_sub_des_c <- dfsubgrupos %>%
  filter(species == "Mice",
         type == "Outcome") %>%
  ggplot(aes(
    x = category,
    y = GES,
    ymin = IC95LL,
    ymax = IC95UL,
    color = "#ff9400"
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
  scale_colour_manual(values = "#ff9400") +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  geom_text(
    aes(label = paste(
      "k = ",
      k,
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
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_blank()
  )

ppt_sub_des_c_i <- dfsubgrupos %>%
  filter(species == "Mice",
         type == "Outcome") %>% 
  ggplot(aes(
    x = category,
    y = inconsistency,
    fill = "#ff9400"
  )) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 200)) +
  labs(x = "", y = "I² (%) |𝜏² ") +
  scale_fill_manual(values = "#ff9400") +
  geom_hline(yintercept = 100, lty = 1, size = .2, color = "black") +
  geom_text(
    aes(label = tau2),
    y = 104,
    color = "black",
    size = 3,
    family = "Gadugi",
    hjust = -0.1
  ) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text = element_blank()
  )


sub_des_c <- ppt_sub_des_c + ppt_sub_des_c_i + plot_layout(widths = c(6, 1))


save_plot(filename = "ppt_sub_des_c.png",
          plot = sub_des_c,
          dpi = 300,
          path = "figure")


ppt_sub_des_r <- dfsubgrupos %>%
  filter(species == "Rat",
         type == "Outcome") %>%
  ggplot(aes(
    x = category,
    y = GES,
    ymin = IC95LL,
    ymax = IC95UL,
    color = "#ec2b2b"
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
  labs(x = "", y = "Effect size") +
  scale_colour_manual(values = "#ec2b2b") +
  geom_hline(yintercept = 0, lty = 2) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  geom_text(
    aes(label = paste(
      "k = ",
      k,
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
    axis.text = element_text(size = 9, color = "black")
  )


ppt_sub_des_r_i <- dfsubgrupos %>%
  filter(species == "Rat",
         type == "Outcome") %>% 
  ggplot(aes(
    x = category,
    y = inconsistency,
    fill = "#ec2b2b"
  )) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 200), breaks = c(0,  100)) +
  labs(x = "", y = "I² (%) |𝜏² ") +
  scale_fill_manual(values = "#ec2b2b") +
  geom_hline(yintercept = 100, lty = 1, size = .2, color = "black") +
  geom_text(
    aes(label = tau2),
    y = 104,
    color = "black",
    size = 3,
    family = "Gadugi",
    hjust = -0.1
  ) +
  facet_grid(moderator ~ ., scales = "free", space = "free") +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.title = element_text(size = 9, color = "black", hjust = -0.4),
    axis.text.x = element_text(size = 9, color = "black", vjust = -2)
  )



sub_des_r <- ppt_sub_des_r + ppt_sub_des_r_i + plot_layout(widths = c(6, 1))

save_plot(filename = "ppt_sub_des_r.png",
          plot = sub_des_r,
          dpi = 300,
          path = "figure")



# Metaregression -----


# age and weight (population), dose (intervention), water depth (outcome)

png("figure/metareg_pio.png", height = 1200, width = 1000)


metareg_age_c <- rma(yi, vi, subset = species == "mice", mods = ~ age, data = Efeito)
metareg_age_r <- rma(yi, vi, subset = species == "rat", mods = ~ age, data = Efeito)
metareg_peso_c <- rma(yi, vi, subset = species == "mice", mods = ~ weight, data = Efeito)
metareg_peso_r <- rma(yi, vi, subset = species == "rat", mods = ~ weight, data = Efeito)
metareg_dose_c <- rma(yi, vi, subset = species == "mice" & dose_unit == "mg/kg", mods = ~dose, data = Efeito) 
metareg_dose_r <- rma(yi, vi, subset = species == "rat" & dose_unit == "mg/kg", mods = ~dose, data = Efeito) 
metareg_pa_c <- rma(yi, vi, subset = species == "mice", mods = ~ water_depth, data = Efeito)
metareg_pa_r <- rma(yi, vi, subset = species == "rat", mods = ~ water_depth, data = Efeito)



par(mfrow = c(4, 2), oma = c(1,1,1,1), mar = c(5,5,2,2), cex = 1, font = 2, family = "sans")


regplot(metareg_age_c, xlab = "Age (days)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), main = "Mice", cex.main = 2, cex.lab = 2, cex.axis = 2, xlim = c(0, 600), ylim = c(0,65))
regplot(metareg_age_r, xlab = "Age (days)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), main = "Rat", cex.main = 2, cex.lab = 2, cex.axis = 2, xlim = c(0, 600), ylim = c(0,25))
regplot(metareg_peso_c, xlab = "Weight (g)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.lab = 2, cex.axis = 2, ylim = c(0,65))
regplot(metareg_peso_r, xlab = "Weight (g)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.lab = 2, cex.axis = 2, ylim = c(0,25))
regplot(metareg_dose_c, xlab = "Dose (mg/kg)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.lab = 2, cex.axis = 2, xlim = c(0, 100), ylim = c(0,65))
regplot(metareg_dose_r, xlab = "Dose (mg/kg)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.lab = 2, cex.axis = 2, xlim = c(0, 100), ylim = c(0,25))
regplot(metareg_pa_c, xlab = "Water depth (cm)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.main = 2, cex.lab = 2, cex.axis = 2, xlim = c(5, 50), ylim = c(0,65))
regplot(metareg_pa_r, xlab = "Water depth (cm)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.main = 2, cex.lab = 2, cex.axis = 2, xlim = c(5, 50), ylim = c(0,25))


dev.off()

metareg_age_c
metareg_age_r
metareg_peso_c
metareg_peso_r
metareg_dose_c
metareg_dose_r
metareg_pa_c 
metareg_pa_r




# year and quality

Efeito$rob1 <- ifelse(Efeito$rob1 == 'Unclear', 0, ifelse(Efeito$rob1 == 'Yes', 1, -1)) # turn assignments into points
Efeito$rob2 <- ifelse(Efeito$rob2 == 'Unclear', 0, ifelse(Efeito$rob2 == 'Yes', 1, -1))
Efeito$rob3 <- ifelse(Efeito$rob3 == 'Unclear', 0, ifelse(Efeito$rob3 == 'Yes', 1, -1))
Efeito$rob4 <- ifelse(Efeito$rob4 == 'Unclear', 0, ifelse(Efeito$rob4 == 'Yes', 1, -1))
Efeito$rob5 <- ifelse(Efeito$rob5 == 'Unclear', 0, ifelse(Efeito$rob5 == 'Yes', 1, -1))
Efeito$rob6 <- ifelse(Efeito$rob6 == 'Unclear', 0, ifelse(Efeito$rob6 == 'Yes', 1, -1))
Efeito$rob7 <- ifelse(Efeito$rob7 == 'Unclear', 0, ifelse(Efeito$rob7 == 'Yes', 1, -1))
Efeito$rob8 <- ifelse(Efeito$rob8 == 'Unclear', 0, ifelse(Efeito$rob8 == 'Yes', 1, -1))
Efeito$rob9 <- ifelse(Efeito$rob9 == 'Unclear', 0, ifelse(Efeito$rob9 == 'Yes', 1, -1))
Efeito$rob10 <- ifelse(Efeito$rob10 == 'Unclear', 0, ifelse(Efeito$rob10 == 'Yes', 1, -1))

Efeito <- Efeito %>% 
  mutate(pont_quali = rob1 + rob2 + rob3 + rob4 + rob5 + rob6 + rob7 + rob8 + rob9 + rob10) # New variable with rob score

png("figure/Reg_year_quality.png", height = 600, width = 1000)

metareg_quali_c <- rma(yi, vi, subset = species == "mice", mods = ~pont_quali, data = Efeito) 
metareg_quali_r <- rma(yi, vi, subset = species == "rat", mods = ~pont_quali, data = Efeito) 
metareg_ano_c <- rma(yi, vi, subset = species == "mice", mods = ~year, data = Efeito) 
metareg_ano_r <- rma(yi, vi, subset = species == "rat", mods = ~year, data = Efeito) 

par(mfrow = c(2, 2), oma = c(0,2,0,1), mar = c(5,5,3,2),  cex = 1, font = 2, family = "sans")

regplot(metareg_ano_c, xlab = "Year", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), main = "Mice", cex.main = 2, cex.lab = 2, cex.axis = 2)
regplot(metareg_ano_r, xlab = "Year", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"),  main = "Rat", cex.main = 2, cex.lab = 2, cex.axis = 2)
regplot(metareg_quali_c, xlab = "Quality Score (ROB SYRCLE)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.lab = 2, cex.axis = 2)
regplot(metareg_quali_r, xlab = "Quality Score (ROB SYRCLE)", ylab = "Hedges' g", lwd = 1.2, col = "black", pch = 1, pi = TRUE, shade = c("grey", "grey90"), cex.lab = 2, cex.axis = 2, xlim = c(0,7))

dev.off()

metareg_ano_c
metareg_ano_r
metareg_quali_c
metareg_quali_r

# Quality ROB/CAMARADES ----

# Isolate variables from ROB SYRCLE

df_rob <- df %>% 
  mutate(Study = str_c(first_author, ", ", year)) %>% 
  select(starts_with("rob"), Study) 


df_rob <- df_rob %>% 
  distinct() # keep one line per publication

df_rob <- df_rob %>% 
  rename("Allocation sequence adequately generated and applied (Q1)" = rob1,
         "Groups similar at baseline (Q2)" = rob2,
         "Allocation adequately concealed (Q3)" = rob3,
         "Animals randomly housed (Q4)" = rob4, 
         "Investigators blinded during the experiment (Q5)" = rob5, 
         "Animals selected at random for outcome assessment (Q6)" = rob6,
         "Outcome assessor blinded (Q7)" = rob7, 
         "Incomplete outcome data adequately addressed (Q8)" = rob8, 
         "Free of selective outcome reporting (Q9)" = rob9,
         "Free of other problems (Q10)" = rob10) %>% 
  relocate(Study, everything())




df_rob_long <- df_rob %>% # put into long format
  pivot_longer(!c(Study),
               names_to = "pergunta",
               values_to = "atribuicao",
  ) 


# rename levels

df_rob_long$atribuicao <-
  factor(
    df_rob_long$atribuicao,
    levels = c("Yes", "No", "Unclear"),
    labels = c("Low", "High", "Unclear") 
  )

df_rob_long$pergunta <-
  fct_relevel(
    df_rob_long$pergunta,"Allocation sequence adequately generated and applied (Q1)",
    "Groups similar at baseline (Q2)",
    "Allocation adequately concealed (Q3)",
    "Animals randomly housed (Q4)", 
    "Investigators blinded during the experiment (Q5)", 
    "Animals selected at random for outcome assessment (Q6)",
    "Outcome assessor blinded (Q7)", 
    "Incomplete outcome data adequately addressed (Q8)", 
    "Free of selective outcome reporting (Q9)",
    "Free of other problems (Q10)") 



# visualize ROB SYRCLE resume

v_factor_levels <- c("High", "Unclear", "Low")

robplot <- df_rob_long %>% 
  group_by(Study) %>% 
  distinct(Study, pergunta, atribuicao) %>% 
  ggplot(aes(x = fct_rev(fct_infreq(pergunta)), fill = factor(atribuicao, levels = v_factor_levels), y = ..count..)) +
  geom_bar(position = "fill") + 
  scale_fill_manual("RoB SYRCLE", values = c("Low" = "#82c236", "Unclear" = "#fec200", "High" = "#ec2b2b"), guide = guide_legend(
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
    axis.ticks.y = element_line(color = "black", size = .1),
    axis.line = element_line(size = .3),
    axis.text = element_text(size = 6,
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
    legend.key.size = unit(.8, "line"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )


robplot

# CAMARADES ----

# Isolate variables from CAMARADES

df_camarades <- df %>% 
  mutate(Study = str_c(first_author, ", ", year)) %>% 
  select(starts_with("camarades"), Study) 

df_camarades <- df_camarades %>% 
  distinct() 

df_camarades <- df_camarades %>% 
  select(everything(), -camarades1) %>% 
  rename("Studies following ARRIVE (or other) guidelines (I1)" = camarades2,
         "Compliance with animal testing regulations and legislation (I2)" = camarades3,
         "Declaration of interest (I3)" = camarades4, 
         "Report of husbandry conditions and improve animal welfare (I4)" = camarades5, 
         "Report of species and lineage of animals (I5)" = camarades6,
         "Report of phenotypes of interest (I6)" = camarades7, 
         "Report of the age, weight or stage of animals (I7)" = camarades8, 
         "Report of the sex of animals (I8)" = camarades9,
         "Report of the methods to acess the outcomes (I9)" = camarades10,
         "Report sample size calculation (I10)" = camarades11) 


df_camarades_longo <- df_camarades %>% 
  pivot_longer(!c(Study),
               names_to = "pergunta",
               values_to = "atribuicao",
  ) 


df_camarades_longo$pergunta <- 
  fct_relevel(
    df_camarades_longo$pergunta, "Studies following ARRIVE (or other) guidelines (I1)",
    "Compliance with animal testing regulations and legislation (I2)",
    "Declaration of interest (I3)", 
    "Report of husbandry conditions and improve animal welfare (I4)", 
    "Report of species and lineage of animals (I5)",
    "Report of phenotypes of interest (I6)", 
    "Report of the age, weight or stage of animals (I7)", 
    "Report of the sex of animals (I8)",
    "Report of the methods to acess the outcomes (I9)",
    "Report sample size calculation (I10)")


df_camarades_longo$atribuicao <-  
  factor(
    df_camarades_longo$atribuicao,
    levels = c("No", "Yes", "Unclear", "Yes, ARRIVE", "Yes, lab animals", "Yes, no conflict"),
    labels = c("No", "Yes", "Unclear", "Yes", "Yes", "Yes") 
  )




df_camarades_longo$atribuicao <- 
  fct_relevel(
    df_camarades_longo$atribuicao, "No", "Unclear", "Yes")



c_factor_levels <- c("No", "Unclear", "Yes") 


# visualize CAMARADES resume

camaradesplot <- df_camarades_longo %>% 
  group_by(Study) %>% 
  distinct(Study, pergunta, atribuicao) %>% 
  ggplot(aes(x = fct_rev(fct_infreq(pergunta)), fill = factor(atribuicao, levels = c_factor_levels), y = ..count..)) +
  geom_bar(position = "fill") + 
  scale_fill_manual("CAMARADES", values = c("Yes" = "#82c236", "Unclear" = "#fec200", "No" = "#ec2b2b"), guide = guide_legend(
    title.position = "top")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(
    labels = function(x)
      str_wrap(x, width = 65)
  ) +
  coord_flip()  + 
  theme_classic() +
  theme(axis.ticks.x = element_line(size = .3),
        axis.ticks.y = element_line(color = "black", size = .1),
        axis.line = element_line(size = .3),
        axis.text = element_text(
          size = 6,
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
        legend.key.size = unit(.8, "line"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()
  )


quality <- robplot / camaradesplot + plot_layout(heights = c(5,5), width = 5)


save_plot(filename = "quality.png",
          plot = quality,
          dpi = 300,
          path = "figure")

