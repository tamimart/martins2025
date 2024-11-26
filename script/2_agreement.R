
# load packages

library(remotes)   # download package from repository
library(pacman)    # agreement calculus
library(tidyverse) # handle data
library(readxl)    # read excel
library(diffdf)    # see differences between dfs
install_github("cran/rel", force = TRUE)
pacman::p_load(dplyr, rel, irr)

# Selection ----

# EMBASE -----

# Load data library 1 (EMBASE)
Selection_embase <- read_excel("data/Selection_embase.xlsx")

col_order <- c("full_r1",
               "full_r2",
               "reason1_r1",
               "reason1_r2",
               "reason2_r1",
               "reason2_r2")

Selection_embase <- Selection_embase[, col_order]

glimpse(Selection_embase)                             

## Assisgnment EMBASE

# Calculation of kappa and 95%CI:
rel::ckap(Selection_embase[1:2], conf.level = 0.95)

# Calculation of agreement
irr::agree(Selection_embase[1:2])

## Reason for exclusion EMBASE 

# Calculation of kappa and 95%CI:
rel::ckap(Selection_embase[3:4], conf.level = 0.95)

# Calculation of agreement
irr::agree(Selection_embase[3:4])



# WPS ------
# Load data library 2 (Web of Science, Pubmed, Scopus)

Selection_wps <- read_excel("data/Selection_wps.xlsx")

col_order <- c("absfull_r1",
               "absfull_r2",
               "reason_r1",
               "reason_r2")

Selection_wps <- Selection_wps[, col_order]

glimpse(Selection_wps)

## Assisgnment WPS

# Calculation of kappa and 95%CI:
rel::ckap(Selection_wps[1:2], conf.level = 0.95)

# Calculation of agreement
irr::agree(Selection_wps[1:2])

## Reason for exclusion  WPS 

# Calculation of kappa and 95%CI:
rel::ckap(Selection_wps[3:4], conf.level = 0.95)

# Calculation of agreement
irr::agree(Selection_wps[3:4])


# Extraction ----

# load tables

meus_dados_r1 <- read_excel("data/Dataclean_200FST_1sR.xlsx")

meus_dados_r2 <- read_excel("data/Dataclean_200FST_2sR.xlsx")


# transform all columns into character

meus_dados_r1 <- meus_dados_r1 |>
  mutate_all(as.character)

meus_dados_r2 <- meus_dados_r2 |>
  mutate_all(as.character)

# select variables from each category (info, quanti, quali) and put data in long format, to compare only one column with all data

#r1

meus_dados_r1_info <- meus_dados_r1 |>
  select(year, language, country, source, species:other_tests) |>
  pivot_longer(cols = everything(),
               values_to = "extraido",
               names_to = "coluna")

meus_dados_r1_quanti <- meus_dados_r1 |>
  select(source:N) |>
  pivot_longer(cols = everything(),
               values_to = "extraido",
               names_to = "coluna") 

meus_dados_r1_quali <- meus_dados_r1 |>
  select(rob1:camarades11) |>
  pivot_longer(cols = everything(),
               values_to = "extraido",
               names_to = "coluna")


#r2

meus_dados_r2_info <- meus_dados_r2 |>
  select(year, language, country, source, species:other_tests) |>
  pivot_longer(cols = everything(),
               values_to = "extraido",
               names_to = "coluna")

meus_dados_r2_quanti <- meus_dados_r2 |>
  select(source:N) |>
  pivot_longer(cols = everything(),
               values_to = "extraido",
               names_to = "coluna")

meus_dados_r2_quali <- meus_dados_r2 |>
  select(rob1:camarades11) |>
  pivot_longer(cols = everything(),
               values_to = "extraido",
               names_to = "coluna")



# join df from Reviewer 1 and Reviewer 2

concordancia_revisores_info <- data.frame(meus_dados_r1_info$extraido, meus_dados_r2_info$extraido)

concordancia_revisores_quanti <- data.frame(meus_dados_r1_quanti$extraido, meus_dados_r2_quanti$extraido)

concordancia_revisores_quali <- data.frame(meus_dados_r1_quali$extraido, meus_dados_r2_quali$extraido)

# ANALYSIS OF AGREEMENT BETWEEN REVIEWERS ------ 



#info
rel::ckap(concordancia_revisores_info[1:2], conf.level = 0.95) # Calculation of kappa and 95%CI
# for some reason it was not possible to calculate with rel package, so it was calculate with psych and irr packages as follows:

psych::cohen.kappa(concordancia_revisores_info)
irr::kappa2(concordancia_revisores_info)

irr::agree(concordancia_revisores_info[1:2]) # Calculation of agreement

diffdf(meus_dados_r1_info,
       meus_dados_r2_info) # see where the differences are

#quanti
rel::ckap(concordancia_revisores_quanti[1:2], conf.level = 0.95) # Calculation of kappa and 95%CI

irr::agree(concordancia_revisores_quanti[1:2]) # Calculation of agreement

diffdf(meus_dados_r1_quanti,
       meus_dados_r2_quanti) # see where the differences are

#quali
rel::ckap(concordancia_revisores_quali[1:2], conf.level = 0.95) # Calculation of kappa and 95%CI

irr::agree(concordancia_revisores_quali[1:2]) # Calculation of agreement

diffdf(meus_dados_r1_quali,
       meus_dados_r2_quali) # see where the differences are