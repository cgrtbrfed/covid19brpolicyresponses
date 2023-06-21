################################################################################
## Barberia et al.: "An Assessment of the Public Health Surveillance          ##
##                   Strategy based on Molecular Testing during Three         ##
##                   Major Pandemic Waves of COVID-19 in Brazil"              ##
##                                                                            ##
## Description: Script for figures 1 and 2 of the Supplementary Material      ##
################################################################################



# Load the required packages:


library(tidyverse)
library(ggpubr)
library(ggsci)
library(ggrepel)

# Figure 1
## Scatter plot with correlation coefficient
## Correlation - Tests 2nd wave and Cases/Deaths 1th wave

testes2w_cases1w <- read.csv(
  "Data/SM_testes2w_cases1w.csv",
  dec = ".",
  sep = ";",
  header = TRUE,
  fileEncoding = "UTF-8-BOM"
)

summary(testes2w_cases1w)


### cases
ggscatter(
  testes2w_cases1w,
  x = "average_2w",
  y = "newCases_100k_inhab_1w",
  color = "Laboratory.group",
  palette = c("#00AFBB", "#FC4E07"),
  conf.int = FALSE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "pearson",
    label.x = 12500,
    label.y = 8000,
    label.sep = "\n"
  )
) +
  facet_wrap(~ Laboratory.group) +
  geom_text_repel(
    data = testes2w_cases1w,
    aes(label = UF),
    alpha = 0.9,
    size = 3,
    segment.size = .25,
    segment.alpha = .8,
    force = 1
  ) +
  labs(x = "Weekly average of RT-PCR distributed tests in the second wave",
       y = "COVID-19 cases in the first wave (100,000 inhab)") +
  theme(legend.position = "none") +
  scale_fill_jama()




### deaths
ggscatter(
  testes2w_cases1w,
  x = "average_2w",
  y = "newDeaths_100k_inhab_1w",
  color = "Laboratory.group",
  palette = c("#00AFBB", "#FC4E07"),
  conf.int = FALSE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "pearson",
    label.x = 12500,
    label.y = 115,
    label.sep = "\n"
  )
) +
  facet_wrap(~ Laboratory.group) +
  geom_text_repel(
    data = testes2w_cases1w,
    aes(label = UF),
    alpha = 0.9,
    size = 3,
    segment.size = .25,
    segment.alpha = .8,
    force = 1
  ) +
  labs(x = "Weekly average of RT-PCR distributed tests in the second wave",
       y = "COVID-19 deaths in the first wave (100,000 inhab)") +
  theme(legend.position = "none") +
  scale_fill_jama()


# Figure 2
## Correlation - Tests 3rd wave and Cases/Deaths 2nd wave

testes3w_cases2w <- read.csv(
  "Data/SM_testes3w_cases2w.csv",
  dec = ".",
  sep = ";",
  header = TRUE,
  fileEncoding = "UTF-8-BOM"
)

### cases

ggscatter(
  testes3w_cases2w,
  x = "average_3w",
  y = "newCases_100k_inhab_2w",
  color = "Laboratory.group",
  palette = c("#00AFBB", "#FC4E07"),
  conf.int = FALSE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "pearson",
    label.x = 25000,
    label.y = 13000,
    label.sep = "\n"
  )
) +
  facet_wrap(~ Laboratory.group) +
  geom_text_repel(
    data = testes3w_cases2w,
    aes(label = UF),
    alpha = 0.9,
    size = 3,
    segment.size = .25,
    segment.alpha = .8,
    force = 1
  ) +
  labs(x = "Weekly average of RT-PCR distributed tests in the third wave",
       y = "COVID-19 cases in the second wave (100,000 inhab)") +
  theme(legend.position = "none") +
  scale_fill_jama()



### deaths

ggscatter(
  testes3w_cases2w,
  x = "average_3w",
  y = "newDeaths_100k_inhab_2w",
  color = "Laboratory.group",
  palette = c("#00AFBB", "#FC4E07"),
  conf.int = FALSE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "pearson",
    label.x = 25000,
    label.y = 300,
    label.sep = "\n"
  )
) +
  facet_wrap(~ Laboratory.group) +
  geom_text_repel(
    data = testes3w_cases2w,
    aes(label = UF),
    alpha = 0.9,
    size = 3,
    segment.size = .25,
    segment.alpha = .8,
    force = 1
  ) +
  labs(x = "Weekly average of RT-PCR distributed tests in the third wave",
       y = "COVID-19 deaths in the second wave (100,000 inhab)") +
  theme(legend.position = "none") +
  scale_fill_jama()

