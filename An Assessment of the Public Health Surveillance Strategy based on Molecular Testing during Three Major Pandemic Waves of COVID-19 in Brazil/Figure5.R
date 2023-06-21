################################################################################
## Barberia et al.: "An Assessment of the Public Health Surveillance          ##
##                   Strategy based on Molecular Testing during Three         ##
##                   Major Pandemic Waves of COVID-19 in Brazil"              ##
##                                                                            ##
## Description: Script for figure 5                                           ##
################################################################################



# Load the required packages:
library(tidyverse)
library(hrbrthemes)
library(ggsci)



nlabs_all <- read.csv(
  "Data/labs_tests_groups_waves_Graphs.csv",
  dec = ",",
  sep = ";",
  header = TRUE,
  fileEncoding="UTF-8-BOM"
)

summary(nlabs_all)

# Figure 5 - HTC

nlabs_htc <-
  filter(nlabs_all, group == "Group 3 - High Testing Centers")


summary(nlabs_htc)


ggplot(nlabs_htc, aes(x = Waves, y = average, fill = Lab)) +
  geom_bar(stat = "identity") +
  labs (x = "COVID-19 waves in Brazil",
        y = "Weekly average of RT-PCR distributed tests") +
  guides(fill = guide_legend("High Testing Centers")) +
  theme_classic () +
  scale_fill_jama () +
  scale_colour_jama()