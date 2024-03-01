# load packages

library(readxl)     # Read Excel files
library(writexl)    # Save Excel files
library(tidyverse)  # Data wrangling
library(lubridate)  # Date manipulation
library(skimr)      # Generate summary statistics
library(janitor)    # Clean and tidy dataframes

# IMPORT ----

# Import each spreadsheet tab from the excel file. 
# To clean the sheets of the 1st and 2nd reviewers, append "_1sR" and "_2sR" respectively to the input and output file names.
meus_dados_library <- read_excel("data/DataExtraction_Raw.xlsx", sheet = "Library") 

meus_dados_info <- read_excel("data/DataExtraction_Raw.xlsx", sheet = "Extraction info") 

meus_dados_outcome <- read_excel("data/DataExtraction_Raw.xlsx", sheet = "FST imm. duration") 

meus_dados_quality <- read_excel("data/DataExtraction_Raw.xlsx", sheet = "References Quality") 

# Remove lines from articles excluded during the extraction step.
meus_dados_library <- meus_dados_library |> 
  filter(Included == TRUE)

# Merge dataframes into a single table.
data1 <- dplyr::left_join(meus_dados_library, meus_dados_info, by = c("First author", "year"))

data2 <- dplyr::left_join(data1, meus_dados_outcome, by = "line")

data_geral <- dplyr::left_join(data2, meus_dados_quality, by = "ID")

# Display summary information about the dataframe.
glimpse(data_geral)

view(data_geral)

# CLEAN ----

# Select columns to be retained in the final worksheet.
data_geral <- data_geral |> 
  select(
    everything(),
    -...25,
    -Included,
    -`First author.y`,
    -comp.y,
    `First author`,
    -exclusion_reason,
    -FSTApparatus_conditions,
    -`Housing conditions`,
    -year.y,
    -source.y,
    `First author.y`,
    -comp.y,
    -`Escale (mm)`,
    -`Escale (s or %)`,
    -`mean CTRL or ATD (mm)`,
    -`SEM CTRL or ATD (mm)`,
    -`mean ADT (mm)` ,
    -`SEM ADT (mm)`
    ) 

# Rename columns/variables according to best practices.
data_geral <- data_geral |> 
  rename(first_author = `First author.x`,
         sex = `Sex (M, F)`,
         species = `Species (Rat, Mice)`,
         weight = `Body Weight (g)`,
         bioterium_temp = `Bioterium_temperature(C°)`,
         bioterium_umid = `Bioterium_umidity(%)`,
         comparator = `Comparator (CTRL or ATD: Antidepressant dose)`,
         atd_type = `Type ATD`,
         atd_class = `ATD class`,
         treatment_duration = `duration treatment (n° days)`,
         treatment_via = `Treatment type (IP, oral)`,
         treatment_freq = `Treatment frequency/day`,
         last_bf_outcome = `Last adm before outcome (h)`,
         fst_protocol = `FST protocol`,
         measurement_method = `Measurement method`,
         cylinder_height = `cylinder_height(cm)`,
         cylinder_diameter = `cylinder_diameter(cm)`,
         water_depth = `water_depth(cm)`,
         water_temperature = `water_temperature(C°)`,
         other_tests = `Others behavioural tests before  FST`,
         year = year.x,
         source = source.x,
         seq = comp.x,
         measure_unit = `Measure/Unity`,
         obs_design = Observations,
         n_comparisons = `N Comparisons`,
         atd_sd = `SDM ADT`,
         atd_n_round = `N ADT (rounded)`,
         atd_n_ext = `N ATD (extraction)`,
         atd_se = `SEM ADT`,
         atd_mean = `mean ADT (s ou %)`,
         ctr_sd = `SDM CTRL or ATD`,
         ctr_n_round = `N CTRL OR ATD (rounded)`,
         ctr_n_ext = `N CTRL OR ATD (extraction)`,
         ctr_se = `SEM CTRL or ATD`,
         ctr_mean = `mean CTRL or ATD(s or %)`,
         more2arms = `OBSERVAÇÃO`,
         study_reference = `Selected studies reference (style=Numbered)`,
         model_phenotype = `Stress-Model/phenotype`,
         ROB1 = `1- A alocação de tratamento foi adequadamente gerada e aplicada? (*)`,
         ROB2 = `2 - Os grupos (controle e tratado) eram similares no início do experimento?`,
         ROB3 = `3 - A alocação foi adequadamente escondida?`,
         ROB4 = `4 - Os animais foram acondicionados aleatoriamente?`,
         ROB5 = `5 - Os investigadores eram cegos quanto ao tratamento durante os experimentos?`,
         ROB6 = `6 - Os animais foram selecionados aleatoriamente para acessar o desfecho?`,
         ROB7 = `7 - A avaliação do resultado foi cega?`,
         ROB8 = `8- Dados incompletos foram adequadamente endereçados?  (*)`,
         ROB9 = `9 - Os relatos do estudo são livres de seleção de desfecho relatado? (*)`,
         ROB10 = `10 - O estudo está aparentemente livre de algum outro problema que poderia resultar em alto risco de viés? (*)`,
         CAMARADES1 = `11- Publicação revisada por pares (*)`,
         CAMARADES2 = `12- Estudo seguiu algum guia, e.g. ARRIVE guidelines.`,
         CAMARADES3 = `13- Declaração de conformidade com os regulamentos de bem-estar animal. (*)`,
         CAMARADES4 = `14- Declaração de possíveis conflitos de interesse. (*)`,
         CAMARADES5 = `15 - Relato das condições de acondicionamento ou ações para melhora do bem estar dos animais experimentais, e.g. ambiente enriquecido.`,
         CAMARADES6 = `16- Relato da espécie/linhagem ou características específicas dos animais, e.g. knockouts.`,
         CAMARADES7 = `17- Relato do fenótipo de interesse, e.g. estressado e/ou depressivo. (*)`,
         CAMARADES8 = `18- Relato da idade, peso ou estágio de vida dos animais.`,
         CAMARADES9 = `19- Relato do sexo dos animais.`,
         CAMARADES10 = `20- Relato sobre o método do teste comportamental e aquisição dos desfechos comportamentais.`,
         CAMARADES11 = `21- Relato do cálculo amostral. (*)`,
         obs_quali = `10`
         ) |> 
  rename_with( ~ tolower(gsub(".", "_", .x, fixed = TRUE))) |> # rename column with custom function.  fixed = TRUE to perform a literal matching. The dot . is a special character in regular expressions that matches any character. 
  clean_names()

# TRANSFORM ----

# Create function to transform variables with ranges (e.g., 35-50) into averages
process_ranges_variables <- function(data, variable){
  
 converted <- data %>%
    select({{ variable }}) %>%
    separate(col = {{ variable }}, sep = "-", into = c("v1", "v2")) %>%
    mutate(v1 = as.numeric(v1),
           v2 = as.numeric(v2)) %>%
    mutate(avarage = ifelse(!is.na(v2), ((v1 + v2) / 2), NA)) %>%
    mutate(value = coalesce(.$avarage, .$v1)) %>%
    select(value)
 
 return(converted)
 
}

# Apply function to columns
converted_age <- process_ranges_variables(data_geral, age)
converted_weight <- process_ranges_variables(data_geral, weight)
converted_bioterium_temp <- process_ranges_variables(data_geral, bioterium_temp)
converted_bioterium_umid <- process_ranges_variables(data_geral, bioterium_umid)
converted_water_temperature <- process_ranges_variables(data_geral, water_temperature)
converted_water_depth <- process_ranges_variables(data_geral, water_depth)

# Transform type of variables according to their characteristics: character, factor, numeric...
# In the case of numerics, if there was text, these will be transformed into NA.
data_geral <- data_geral |>
  mutate(ctr_n_round = as.numeric(ctr_n_round),
         atd_n_round = as.numeric(atd_n_round),
         id = as.character(id),
         idgeral = as.character(idgeral),
         year = lubridate::ymd(year, truncated = 2L),
         line = as.character(line),
         sex = as.factor(sex),
         dose = as.numeric(dose),
         dose_unit = as.character(dose_unit),
         strain = as.factor(strain),
         model_phenotype = as.factor(model_phenotype),
         treatment_via = as.factor(treatment_via),
         country = as.factor(country),
         species = as.factor(species),
         treatment_duration = as.numeric(treatment_duration),
         treatment_freq = as.numeric(treatment_freq),
         last_bf_outcome = as.numeric(last_bf_outcome),
         positive_control = as.factor(positive_control),
         cylinder_height = as.numeric(cylinder_height),
         cylinder_diameter = as.numeric(cylinder_diameter),
         water_temperature = converted_water_temperature$value,
         water_depth = converted_water_depth$value,
         comparator = as.factor(comparator),
         n_comparisons  = as.integer(n_comparisons),
         atd_class = as.factor(atd_class),
         atd_type = as.factor(atd_type),
         fst_protocol = as.factor(fst_protocol),
         measurement_method = as.factor(measurement_method),
         measure_unit = as.factor(measure_unit),
         atd_n_round = as.integer(atd_n_round),
         atd_se = as.numeric(atd_se),
         more2arms = as.factor(more2arms),
         seq = as.integer(seq),
         rob1 = as.factor(rob1),
         rob2 = as.factor(rob2),
         rob3 = as.factor(rob3),
         rob4 = as.factor(rob4),
         rob5 = as.factor(rob5),
         rob6 = as.factor(rob6),
         rob7 = as.factor(rob7),
         rob8 = as.factor(rob8),
         rob9 = as.factor(rob9),
         rob10 = as.factor(rob10),
         camarades1 = as.factor(camarades1),
         camarades2 = as.factor(camarades2),
         camarades3 = as.factor(camarades3),
         camarades4 = as.factor(camarades4),
         camarades5 = as.factor(camarades5),
         camarades6 = as.factor(camarades6),
         camarades7 = as.factor(camarades7),
         camarades8 = as.factor(camarades8),
         camarades9 = as.factor(camarades9),
         camarades10 = as.factor(camarades10),
         camarades11 = as.factor(camarades11),
         age = converted_age$value,
         weight = converted_weight$value,
         bioterium_temp = converted_bioterium_temp$value,
         bioterium_umid = converted_bioterium_umid$value,
         bioterium_lightcycle = as.factor(bioterium_lightcycle)
  )

# Create new column with comparator n corrected according to the number of comparisons and rounded
# Separate method from method detail (FST)
# change date type to numeric
data_geral <- data_geral |>
  separate(col = measurement_method, sep = ", ", into = c("measurement_method", "measurement_method_detail"))  |> # separate variable into two
  mutate(ctr_n_corr = as.integer(ctr_n_round / n_comparisons),
         N = as.integer(ctr_n_corr + atd_n_round),
         measurement_method_detail = as.factor(measurement_method_detail), # add separate variables in parent df
         measurement_method =  as.factor(measurement_method)) 

# Rearrange order of variables
# Get column nameS
colnames(data_geral) 

# Insert column names in the desired order
col_order <- c("line", 
               "idgeral",
               "id",
               "study_reference",
               "authors",
               "first_author",
               "year",
               "title",
               "language",
               "country",
               "source",
               "seq",
               "outcome",
               "measure_unit",
               "ctr_mean",
               "ctr_sd",
               "ctr_se",
               "ctr_n_ext",
               "ctr_n_round",
               "ctr_n_corr",
               "n_comparisons",
               "atd_mean",
               "atd_sd",
               "atd_se",
               "atd_n_ext",
               "atd_n_round",
               "N",
               "more2arms",
               "obs_design",
               "species",
               "strain",
               "sex",
               "age",
               "weight",
               "model_phenotype",
               "cage_measures",
               "animals_percage",
               "bioterium_lightcycle",
               "bioterium_temp",
               "bioterium_umid",
               "comparator",
               "atd_type",
               "atd_class",
               "dose",
               "dose_unit",
               "treatment_duration", 
               "treatment_freq",
               "treatment_via",
               "last_bf_outcome",
               "positive_control",
               "fst_protocol",
               "measurement_method",
               "measurement_method_detail",
               "cylinder_height",
               "cylinder_diameter",
               "water_depth",
               "water_temperature",
               "other_tests",
               "rob1",
               "rob2",
               "rob3",
               "rob4",
               "rob5",
               "rob6",
               "rob7",
               "rob8",
               "rob9",
               "rob10",
               "camarades1",
               "camarades2",
               "camarades3",
               "camarades4",
               "camarades5",
               "camarades6",
               "camarades7",
               "camarades8",
               "camarades9",
               "camarades10",
               "camarades11",
               "obs_quali")   

# Add new column sequence
data_geral_reord <- data_geral[, col_order]

# Check the levels of all variables
sapply(data_geral_reord, levels)

# Correct the wrongly written values and merge the ones that were written in different ways

#atd_type
levels(data_geral_reord$atd_type)[match("bupropiona",levels(data_geral_reord$atd_type))] <- "bupropion"

#camarades3
levels(data_geral_reord$camarades3)[match("yes",levels(data_geral_reord$camarades3))] <- "Yes"

#camarades1
levels(data_geral_reord$camarades1)[match("No",levels(data_geral_reord$camarades1))] <- "Yes"

#measurement_method
levels(data_geral_reord$measurement_method)[match("VIdeo analysis",levels(data_geral_reord$measurement_method))] <- "video analysis"

levels(data_geral_reord$measurement_method)[match("score5sinterval",levels(data_geral_reord$measurement_method))] <- "NA, score5sinterval"

levels(data_geral_reord$measurement_method)[match("manually, digital chronometers",levels(data_geral_reord$measurement_method))] <- "manually, chronometers"

levels(data_geral_reord$measurement_method)[match("MicroAct Scratching Test",levels(data_geral_reord$measurement_method))] <- "video analysis, automated"

levels(data_geral_reord$measurement_method)[match("video analysis, automatically analysis",levels(data_geral_reord$measurement_method))] <- "video analysis, automated"

# Strain
levels(data_geral_reord$strain)[match(c("balb/c", "BALB/C", "BALB/c", "balb/CJ", "BALB/CJ", "BALB/CByJ", "Balb/CJ"), levels(data_geral_reord$strain))] <- "BALB" 

levels(data_geral_reord$strain)[match(c("CB57BL/6J", "C57BL/6J", "C57/BL6","C57BL/6", "C57BL/6N"),levels(data_geral_reord$strain))] <- "C57BL" 

levels(data_geral_reord$strain)[match(c("CD", "CD1", "ICR"), levels(data_geral_reord$strain))] <- "CD-1" 

levels(data_geral_reord$strain)[match("Kumming", levels(data_geral_reord$strain))] <- "kunming" 

levels(data_geral_reord$strain)[match(c("SD", "sprague-dawley"), levels(data_geral_reord$strain))] <- "sprague dawley" 

levels(data_geral_reord$strain)[match("wistar-kyoto", levels(data_geral_reord$strain))] <- "wistar kyoto"

levels(data_geral_reord$strain)[match("Slc:ddY", levels(data_geral_reord$strain))] <- "ddY"

# model/phenotype
levels(data_geral_reord$model_phenotype)[match(c("CUMS", "UCMS"), levels(data_geral_reord$model_phenotype))] <- "CUMs"

levels(data_geral_reord$model_phenotype)[match(c("postOVX8m", "postOVX4m", "postOVX2w", "ovarieactomized"), levels(data_geral_reord$model_phenotype))] <- "ovariectomized"

levels(data_geral_reord$model_phenotype)[match(c("reserpine (6mg/Kg)", "reserpine (2mg/Kg)"), levels(data_geral_reord$model_phenotype))] <- "reserpine"

levels(data_geral_reord$model_phenotype)[match(c("streptozotocin (65mg/kg)", "streptozotocin (40mg/Kg)"), levels(data_geral_reord$model_phenotype))] <- "streptozotocin"

levels(data_geral_reord$model_phenotype)[match("strokeMCAOpos14", levels(data_geral_reord$model_phenotype))] <- "stroke (Middle Cerebral Artery occlusion)"

levels(data_geral_reord$model_phenotype)[match("antidepressant-withdrawl", levels(data_geral_reord$model_phenotype))] <- "antidepressant withdrawal"

levels(data_geral_reord$model_phenotype)[match("normal emotional", levels(data_geral_reord$model_phenotype))] <- NA

levels(data_geral_reord$model_phenotype)[match(c("mother exposed to o,p'-dichlorodiphenyltrichloro-ethane (DDT)", "mother exposed to p,p'-dichlorodiphenyltrichloro-ethane (DDT)"), levels(data_geral_reord$model_phenotype))] <- "mother exposed to DDT"

# country 
levels(data_geral_reord$country)[match("México", levels(data_geral_reord$country))] <- "Mexico"

levels(data_geral_reord$country)[match("United Kingdom", levels(data_geral_reord$country))] <- "UK"

levels(data_geral_reord$country)[match("Korea", levels(data_geral_reord$country))] <- "South Korea"

# treatmentvia
levels(data_geral_reord$treatment_via)[match("tablet", levels(data_geral_reord$treatment_via))] <- "oral"

# more2arms
summary(data_geral_reord$more2arms)

levels(data_geral_reord$more2arms)[match(c("NMAa", "NMAb", "NMA", "NMAc"), levels(data_geral_reord$more2arms))] <- "Yes"

levels(data_geral_reord$more2arms)[match(c("adminsitraçao espontanea", "descrição FST em outro paper"), levels(data_geral_reord$more2arms))] <- "No"

data_geral_reord$more2arms <- factor(data_geral_reord$more2arms, exclude = NULL, 
               levels = c("Yes", "No", NA), 
               labels = c("Yes", "No", "No"))

# EXPORT ----

# Save clean and transformed df for further analysis
write_xlsx(data_geral_reord,"data/Dataclean_200FST.xlsx")

saveRDS(data_geral_reord, "data/Dataclean_200FST.rds")

# Display the structure of the cleaned and transformed data
glimpse(data_geral_reord)
