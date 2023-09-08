
## Wrangle data for MZ2 late succession (aka "Kelly et al., 2023) project

# Load the janitor and tidyverse packages, which include various data cleaning and manipulation tools
library(janitor)
library(tidyverse)

# Read data from CSV files into respective data frames
raw_bps_scls <- read.csv("data/bps_scl_cmbn.csv")            # Read bps_scls data
sclass_descriptions <- read.csv("data/scls_descriptions.csv")  # Read sclass_descriptions data
reference_percents <- read.csv("data/ref_con_long.csv")       # Read reference_percents data
bps_mz2 <- read.csv("data/bps_mz2.csv")                       # Read bps_mz2 data

# Clean the raw_bps_scls data frame
clean_bps_scls_cmbn <- raw_bps_scls %>%
  clean_names() %>%                                # Clean column names
  select(-c(4, 5)) %>%                             # Remove columns 4 and 5
  unite("join_field", bps_model, label, sep = "_", remove = FALSE )  # Combine columns and create 'join_field'

# Clean the sclass_descriptions data frame
sclass_descriptions_clean <- sclass_descriptions %>%
  select(-c(4)) %>%                                 # Remove column 4
  rename("model_code" = "StratumID",
         "scls_label" = "ClassLabelID",
         "state_class_id" = "StateClassID") %>%     # Rename columns
  unite("join_field", model_code:scls_label, sep = "_", remove = FALSE ) %>%  # Combine columns and create 'join_field'
  separate(state_class_id, into = c("age_category", "canopy_category"), sep = ":", remove = FALSE)  # Split 'state_class_id'

# Get unique sclass labels from reference_percents data
unique_sclass_labels_ref <- unique(reference_percents$refLabel)
print(unique_sclass_labels_ref)

# Get unique sclass labels from clean_bps_scls_cmbn data
unique_sclass_lables_cmbn <- unique(clean_bps_scls_cmbn$label)
print(unique_sclass_lables_cmbn)

# Clean the reference_percents data frame
clean_ref_percents <- reference_percents %>%
  clean_names() %>%                          # Clean column names
  mutate(across('ref_label', str_replace, 'Developed', 'Urban')) %>%  # Replace values in 'ref_label'
  mutate(across('model_label', str_replace, 'Developed', 'Urban')) %>%  # Replace values in 'model_label'
  rename("join_field" = "model_label",
         "bps_name" = "bp_s_name" )            # Rename columns

# Filter the clean_ref_percents data frame based on matching 'model_code' with bps_mz2
clean_ref_percents_mz2 <- clean_ref_percents %>%
  filter(model_code %in% bps_mz2$BPS_MODEL)

# Perform a left join between clean_ref_percents_mz2 and sclass_descriptions_clean
final_df <- left_join(clean_ref_percents_mz2, sclass_descriptions_clean)

# Perform a full join between final_df and clean_bps_scls_cmbn
final_df2 <- full_join(final_df, clean_bps_scls_cmbn) %>%
  select(-c(6, 10, 13, 14)) %>%   # Remove columns 6, 10, 13, and 14
  rename("cur_scls_count" = "count")  # Rename 'count' column to 'cur_scls_count'

# Group final_df2 by 'bps_name' and calculate 'bps_count'
final_df3 <- final_df2 %>%
  group_by(bps_name) %>%
  mutate(bps_count = sum(cur_scls_count, na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate additional columns related to acres and percentages
  mutate(
    bps_acres = bps_count * 0.2223945,
    ref_scls_acres = bps_acres * (ref_percent / 100),
    cur_scls_acres = cur_scls_count * 0.2223945,
    cur_percent = (cur_scls_acres / bps_acres) * 100
  ) %>%
  # Round columns 12 to 15 to whole numbers
  mutate(across(12:15, round, 0))

# Write the final data frame final_df3 to a CSV file
write.csv(final_df3, file = "data/final_df.csv", row.names = FALSE)
