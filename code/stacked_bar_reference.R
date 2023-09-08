

### This code loads libraries, reads data, processes it, and creates a stacked bar plot using the ggplot2 package in R. It also includes comments and explanations for each step.

## stacked bar, bps x age class

## stacked bar, bps x class label (early, mid, late)

## Load necessary libraries.
library(tidyverse)
library(scales)

## Set an option to prevent scientific notation in the output.

options(scipen = 9999999999)

## Read the raw data from a CSV file and filter it

raw_data <- read.csv("data/final_df.csv") %>%
  filter(!is.na(age_category)) %>%
  filter(bps_acres > 10000)

## Reorder the levels of the age_category factor

raw_data$age_category <- factor(raw_data$age_category, 
                                    levels = c(
                                      "Early1",
                                      "Mid1",
                                      "Mid2",
                                      "Late1",
                                      "Late2"
                                    ))
## Reverse the order of the age_category levels

raw_data$age_category <- factor(raw_data$age_category, levels = rev(levels(raw_data$age_category)))


## Create a stacked bar plot with ggplot
plot_acres <-
  ggplot(raw_data, aes(fill = age_category, y = ref_scls_acres, x = reorder(bps_name, -bps_acres))) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  labs(
    title = "Biophysical Settings of the area, with estimated reference acres",
    subtitle = "Split out by succession classes",
    caption = "Data from landfire.gov. BpSs with small footprint removed for clarity (> 10k acres)",
    x = "",
    y = "Acres",
    fill = "Succession Class")+
  scale_x_discrete(limits=rev) +
  scale_y_continuous(label=comma, n.breaks = 4) + 
  theme_bw(base_size = 22) + 
  scale_fill_manual(values = c("#f5922f", # orange
                               "#532a66", # purple
                               "#827c75", # grey
                               "#f5eb2f", # yellow
                               "#74a36f" # green-natural veg
  )) +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot") +
  theme(legend.position = c(0.8, 0.2)) + 
  theme(plot.margin = unit(c(0.2, 0.75, 0.2, 0.2),
                           "inches"))
## Display the created plot
plot_acres





