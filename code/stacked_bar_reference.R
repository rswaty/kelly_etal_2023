

## stacked bar, bps x age class

## stacked bar, bps x class label (early, mid, late)

# Load packages
library(tidyverse)
library(scales)

# Options

options(scipen = 9999999999)

raw_data <- read.csv("data/final_df.csv") %>%
  filter(!is.na(age_category))

# get labels ordered properly

raw_data$age_category <- factor(raw_data$age_category, 
                                    levels = c(
                                      "Early1",
                                      "Mid1",
                                      "Mid2",
                                      "Late1",
                                      "Late2"
                                    ))

raw_data$age_category <- factor(raw_data$age_category, levels = rev(levels(raw_data$age_category)))



plot_acres <-
  ggplot(raw_data, aes(fill = age_category, y = ref_scls_acres, x = reorder(bps_name, -bps_acres))) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  labs(
    title = "Biophysical Settings of the area, with estimated reference acres",
    subtitle = "Split out by age classes",
    caption = "Data from landfire.gov.",
    x = "",
    y = "Acres",
    fill = "Age Class")+
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

plot_acres





