################################################################################
## Barberia et al.: "An Assessment of the Public Health Surveillance          ##
##                   Strategy based on Molecular Testing during Three         ##
##                   Major Pandemic Waves of COVID-19 in Brazil"              ##
##                                                                            ##
## Description: Script for figure 4                                           ##
################################################################################

# Load the required packages:

library(tidyverse)
library(hrbrthemes)
library(sf)
library(geobr)
library(tmap)


nlabs_all <- read.csv(
  "Data/labs_tests_groups_waves_Graphs.csv",
  dec = ",",
  sep = ";",
  header = TRUE,
  fileEncoding="UTF-8-BOM"
)



## barplot - Other labs

nlabs_others <-
  filter(nlabs_all, group == "Group 2 - Labs enabled during pandemic")

summary(nlabs_others)

fig4.4 <- ggplot(nlabs_others, aes(x = UF, y = average, fill = Lab.category)) +
  geom_col() +
  facet_wrap( ~ Waves) +
  coord_flip() +
  labs (y = "Weekly average of RT-PCR distributed tests",
        x = "Federative Units (FU)") +
  theme (
    legend.position = "bottom",
    panel.background = element_rect(
      fill = "white",
      colour = "white",
      size = 0.5,
      linetype = "solid"
    ),
    panel.grid.major =  element_line(
      size = 0.5,
      linetype = "dotted",
      colour = "gray"
    )
  ) +
  scale_fill_ipsum() +
  guides(fill = guide_legend("Laboratory category"))


## Maps

# read all states
states <- read_state(year = 2019,
                     showProgress = FALSE)


### First wave

nlabs_2cat_1w <-
  read.csv(
    "Data/map_nlabs_media_2cat_1w.csv",
    sep = ";",
    dec = ",",
    header = TRUE,
    fileEncoding="UTF-8-BOM"
  )

nlabs_2cat_1w_uf <-
  left_join(states, nlabs_2cat_1w , by = c("abbrev_state" = "UF"))

summary(nlabs_2cat_1w_uf)

fig4.1 <- tm_shape(nlabs_2cat_1w_uf,  bbox = c(-80,-35,-15, 10)) +
  tm_fill(
    col = "average_1w",
    style = "jenks",
    palette = "-viridis",
    title = "Weekly average of distributed tests",
    border.alpha = 0.5
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_borders(alpha = 0.3) +
  tm_bubbles(
    size = 'n_labs',
    col = 'white',
    style = "jenks",
    alpha = 0.6,
    title.size = 'Number of labs'
  ) +
  tm_layout(
    title = "First Wave",
    title.size = 1.2,
    title.position = c("left", "top"),
    legend.title.size = 0.9,
    legend.text.size = 0.7
  ) +
  tm_text("abbrev_state",
          scale = 0.8,
          xmod = 0.5,
          ymod = 0.5)

### Second wave

nlabs_2cat_2w <-
  read.csv(
    "Data/map_nlabs_media_2cat_2w.csv",
    sep = ";",
    dec = ",",
    header = TRUE,
    fileEncoding="UTF-8-BOM"
  )

nlabs_2cat_2w_uf <-
  left_join(states, nlabs_2cat_2w , by = c("abbrev_state" = "UF"))

summary(nlabs_2cat_2w_uf)

fig4.2 <- tm_shape(nlabs_2cat_2w_uf,  bbox = c(-80,-35,-15, 10)) +
  tm_fill(
    col = "average_2w",
    style = "jenks",
    palette = "-viridis",
    title = "Weekly average of distributed tests",
    border.alpha = 0.5
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_borders(alpha = 0.3) +
  tm_bubbles(
    size = 'n_labs',
    col = 'white',
    alpha = 0.6,
    title.size = 'Number of labs'
  ) +
  tm_layout(
    title = "Second Wave",
    title.size = 1.2,
    title.position = c("left", "top"),
    legend.title.size = 0.9,
    legend.text.size = 0.7
  ) +
  tm_text("abbrev_state",
          scale = 0.8,
          xmod = 0.5,
          ymod = 0.5)


## Third wave

nlabs_2cat_3w <-
  read.csv(
    "Data/map_nlabs_media_2cat_3w.csv",
    sep = ";",
    dec = ",",
    header = TRUE,
    fileEncoding="UTF-8-BOM"
  )

nlabs_2cat_3w_uf <-
  left_join(states, nlabs_2cat_3w , by = c("abbrev_state" = "UF"))

summary(nlabs_2cat_3w_uf)

fig4.3 <- tm_shape(nlabs_2cat_3w_uf,  bbox = c(-80,-35,-15, 10)) +
  tm_fill(
    col = "average_3w",
    style = "jenks",
    palette = "-viridis",
    title = "Weekly average of distributed tests",
    border.alpha = 0.5
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_borders(alpha = 0.3) +
  tm_bubbles(
    size = 'n_labs',
    col = 'white',
    alpha = 0.6,
    title.size = 'Number of labs'
  ) +
  tm_layout(
    title = "Third Wave",
    title.size = 1.2,
    title.position = c("left", "top"),
    legend.title.size = 0.9,
    legend.text.size = 0.7
  ) +
  tm_text("abbrev_state",
          scale = 0.8,
          xmod = 0.5,
          ymod = 0.5)