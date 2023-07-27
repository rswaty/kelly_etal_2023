

## old growth loss, try arrow chart

## load packages

library(tidyverse)

## read in data

raw_data <- read.csv("data/final_df.csv")


## filter, group and add helper columns

old_classes <- c("Late1", "Late2")

old_growth_loss <- raw_data %>%
  filter(age_category %in% old_classes) %>%
  group_by(bps_name) %>%
  summarize(ref_percent = sum(ref_percent, na.rm = TRUE),
            cur_percent = sum(cur_percent, na.rm = TRUE),
            bps_acres = max(bps_acres)) %>%
  mutate(change = cur_percent - ref_percent,
       sign_change = (change >0)) 


# try arrow plot


arrow_plot <- old_growth_loss |> 
  ggplot(aes(
      x = ref_percent, xend = cur_percent, 
      y = reorder(bps_name, bps_acres), yend = bps_name,
      color = sign_change)) +
  geom_segment(
    arrow = arrow(angle = 30, length = unit(0.5, 'cm')),
    size = 3) +
  labs(
    x = 'Percent Change', 
    y = element_blank(),
    title = 'Changes in Late Succession Classes Historical to ~2020',
    subtitle = 'Arrows in descending order by total extent of ecosystem'
  ) +
  scale_color_manual(
    values = c("#fcba03", "#10692c")) +
  theme_bw(base_size = 22) + 
  theme(legend.position = "none")


arrow_plot
