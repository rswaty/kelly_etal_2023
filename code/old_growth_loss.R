

## old growth loss, try arrow chart.

## This code performs data manipulation and creates a plot to visualize changes in late succession classes' historical data to ~2020. It uses functions from the tidyverse package and ggplot2 for visualization. Each step is explained in the comments.

# Load the tidyverse package, which includes various data manipulation and visualization packages
library(tidyverse)

# Read the data from a CSV file located in the 'data' folder and store it in the 'raw_data' variable
raw_data <- read.csv("data/final_df.csv")

# Define a vector 'old_classes' containing specific age categories of interest
old_classes <- c("Late1", "Late2")

# Filter and summarize data related to old growth change in canopy categories
old_growth_chng_canopy <- raw_data %>%
  filter(age_category %in% old_classes) %>%  # Filter rows with age_category in 'old_classes'
  filter(canopy_category != 'ALL') %>%       # Filter rows where canopy_category is not 'ALL'
  group_by(bps_name, canopy_category) %>%     # Group data by 'bps_name' and 'canopy_category'
  summarize(
    ref_percent = sum(ref_percent, na.rm = TRUE),  # Calculate the sum of 'ref_percent' (removing NAs)
    cur_percent = sum(cur_percent, na.rm = TRUE),  # Calculate the sum of 'cur_percent' (removing NAs)
    bps_acres = max(bps_acres)                     # Calculate the maximum value of 'bps_acres'
  ) %>%
  mutate(
    change = cur_percent - ref_percent,      # Calculate the change as the difference between 'cur_percent' and 'ref_percent'
    sign_change = (change > 0)              # Create a binary variable indicating positive change
  )

# Define a mapping of facet labels for canopy categories
facet_names <- c(
  "CLS" = "Closed Canopy",
  "OPN" = "Open Canopy"
)

# Create a plot to visualize changes in late succession classes' canopy categories
canopy_arrow_plot <- old_growth_chng_canopy %>%
  ggplot(aes(
    x = ref_percent, xend = cur_percent,          # Set x and xend for the arrow segment
    y = reorder(bps_name, bps_acres), yend = bps_name,  # Set y and yend for the arrow segment
    color = sign_change                          # Color arrows based on 'sign_change'
  )) +
  geom_segment(
    arrow = arrow(angle = 30, length = unit(0.5, 'cm')),  # Add arrow segments with specific settings
    size = 3  # Set the size of the arrow segments
  ) +
  labs(
    x = 'Percent Change',              # Set x-axis label
    y = element_blank(),               # Remove y-axis label
    title = 'Changes in Late Succession Classes Historical to ~2020',  # Set plot title
    subtitle = 'Arrows in descending order by total extent of ecosystem'  # Set plot subtitle
  ) +
  scale_color_manual(
    values = c("#fcba03", "#10692c")  # Set custom colors for arrow segments based on 'sign_change'
  ) +
  theme_bw(base_size = 22) +          # Set the theme to black and white with a base font size of 22
  theme(legend.position = "none") +    # Remove the legend
  facet_wrap(
    ~ canopy_category,                 # Facet the plot by 'canopy_category'
    ncol = 2,                          # Arrange facets in 2 columns
    labeller = as_labeller(facet_names)  # Apply custom facet labels using 'facet_names'
  )

# Display the created canopy_arrow_plot
canopy_arrow_plot
