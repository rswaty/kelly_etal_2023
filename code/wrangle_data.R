
## wrangle data for MZ2 late succession (aka "Kelly et al., 2023) project

## load packages

library(janitor)
library(tidyverse)

## read in raw data and sclass descriptions

raw_bps_scls <- read.csv("data/bps_scl_cmbn.csv")

sclass_descriptions <- read.csv("data/scls_descriptions.csv")

reference_percents <- read.csv("data/ref_con_long.csv")

bps_mz2 <- read.csv("data/bps_mz2.csv")

##  clean and prep raw combined data

clean_bps_scls_cmbn <- raw_bps_scls %>%
  clean_names() %>%
  select(-c(4, 5)) %>%
  unite("join_field", bps_model,label, sep = "_", remove = FALSE )

## clean and prep sclass descriptions

sclass_descriptions_clean <- sclass_descriptions %>%
  select(-c(4)) %>% # remove column
  rename("model_code" = "StratumID",
         "scls_label" = "ClassLabelID",
         "state_class_id" = "StateClassID" ) %>% # rename columns
  unite("join_field", model_code:scls_label, sep = "_", remove = FALSE ) %>%
  separate(state_class_id, into = c("age_category", "canopy_category"), sep = ":", remove = FALSE) 


## clean and prep reference percents

unique_sclass_labels_ref <- unique(reference_percents$refLabel)
print(unique_sclass_labels_ref)

unique_sclass_lables_cmbn <- unique(clean_bps_scls_cmbn$label)
print(unique_sclass_lables_cmbn)
# does not have barren/sparse and there are differences, e.g., Urban-Developed between this and sclass label
# will assume Barren/Sparse, NoData and Snow/Ice is minimal; will change "Developed" to "Urban" in reference df cleaning code 

clean_ref_percents <- reference_percents %>%
  clean_names() %>%
  mutate(across('ref_label', str_replace, 'Developed', 'Urban')) %>%
  mutate(across('model_label', str_replace, 'Developed', 'Urban')) %>%
  rename("join_field" = "model_label",
         "bps_name" = "bp_s_name" )

## need to winnow this df to only the bps model codes in MZ 2

clean_ref_percents_mz2 <- clean_ref_percents %>%
  filter(model_code %in% bps_mz2$BPS_MODEL)


## create 'final' dataframe with reference and current sclass percents, acres and labels

## first ref con and sclass descriptions
final_df <- left_join(clean_ref_percents_mz2, sclass_descriptions_clean) 


# looks OK, now full join to add reference percents then clean a bit

final_df2 <- full_join(final_df, clean_bps_scls_cmbn) %>%
  select(-c(6, 10, 13, 14)) %>%
  rename( "cur_scls_count" = "count")

# now for the math: need count/acres per bps, cur sclass percents and differences

final_df3 <- final_df2 %>%
  group_by(bps_name) %>%
  mutate(bps_count = sum(cur_scls_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(bps_acres = bps_count*0.2223945,
         ref_scls_acres = bps_acres*(ref_percent/100),
         cur_scls_acres = cur_scls_count*0.2223945,
         cur_percent = (cur_scls_acres/bps_acres)*100) %>%
  mutate(across(12:15, round, 0))

# save to csv to explore in Excel, and reorder columns

write.csv(final_df3, file = "data/final_df.csv", row.names=FALSE)

  