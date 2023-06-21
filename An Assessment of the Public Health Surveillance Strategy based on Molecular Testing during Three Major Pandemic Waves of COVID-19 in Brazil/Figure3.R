################################################################################
## Barberia et al.: "An Assessment of the Public Health Surveillance          ##
##                   Strategy based on Molecular Testing during Three         ##
##                   Major Pandemic Waves of COVID-19 in Brazil"              ##
##                                                                            ##
## Description: Script for figure 3                                           ##
################################################################################


# Load the required packages:

library(tidyverse)
library(hrbrthemes)
library(ggsci)
library(sf)
library(geobr)
library(tmap)
library(ggpubr)



nlabs_all <- read.csv(
  "Data/labs_tests_groups_waves_Graphs.csv",
  dec = ",",
  sep = ";",
  header = TRUE,
  fileEncoding="UTF-8-BOM"
)

summary(nlabs_all)


# SISLAB

nlabs_sislab <-
  filter(nlabs_all,
         group == "Group 1 - Public health labs prior to the pandemic")

summary(nlabs_sislab)


## barplot

fig3.4 <- ggplot(nlabs_sislab, aes(x = UF, y = average, fill = Lab.category)) +
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
  scale_fill_viridis_d () +
  guides(fill = guide_legend("Laboratory category"))

## Maps

# read all states
states <- read_state(year = 2019,
                     showProgress = FALSE)


# SISLAB - 1cat

### First wave
nlabs_1cat_1w <-
  read.csv(
    "Data/map_nlabs_media_1cat_1w.csv",
    sep = ";",
    dec = ",",
    header = TRUE,
    fileEncoding="UTF-8-BOM"
  )

nlabs_1cat_1w_uf <-
  left_join(states, nlabs_1cat_1w , by = c("abbrev_state" = "UF"))

summary(nlabs_1cat_1w_uf)


fig3.1 <- tm_shape(nlabs_1cat_1w_uf,  bbox = c(-80,-35,-15, 10)) +
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

nlabs_1cat_2w <-
  read.csv(
    "Data/map_nlabs_media_1cat_2w.csv",
    sep = ";",
    dec = ",",
    header = TRUE,
    fileEncoding="UTF-8-BOM"
  )

nlabs_1cat_2w_uf <-
  left_join(states, nlabs_1cat_2w , by = c("abbrev_state" = "UF"))

summary(nlabs_1cat_2w_uf)

fig3.2 <- tm_shape(nlabs_1cat_2w_uf,  bbox = c(-80,-35,-15, 10)) +
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

### Third wave

nlabs_1cat_3w <-
  read.csv(
    "Data/map_nlabs_media_1cat_3w.csv",
    sep = ";",
    dec = ",",
    header = TRUE,
    fileEncoding="UTF-8-BOM"
  )

nlabs_1cat_3w_uf <-
  left_join(states, nlabs_1cat_3w , by = c("abbrev_state" = "UF"))

summary(nlabs_1cat_3w_uf)

fig3.3 <- tm_shape(nlabs_1cat_3w_uf,  bbox = c(-80,-35,-15, 10)) +
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


