################################################################################
## Barberia et al.: "An Assessment of the Public Health Surveillance          ##
##                   Strategy based on Molecular Testing during Three         ##
##                   Major Pandemic Waves of COVID-19 in Brazil"              ##
##                                                                            ##
## Description: Script for the Table 2 Student's T-Test                       ##
################################################################################


# Load the required packages:

library(stats)
library(tidyverse)


# Test t Student - RevFinal

tests_all_b <- read.csv(
  "Data/labs_waves_Student t test.csv",
  dec = ",",
  sep = ";",
  header = TRUE,
  fileEncoding="UTF-8-BOM"
)


summary(tests_all_b)

group_by(tests_all_b, group) %>%
  summarise(
    count = n(),
    mean = mean(overall_average, na.rm = TRUE),
    sd = sd(overall_average, na.rm = TRUE)
  )

# T-test overall

First_w <- tests_all_b$First.wave_average
Second_w <- tests_all_b$Second.wave_average
Third_w <- tests_all_b$Third.wave_average

t.test (First_w, Second_w, paired = TRUE, alternative = "two.sided")

t.test (First_w, Third_w, paired = TRUE, alternative = "two.sided")

t.test (Second_w, Third_w, paired = TRUE, alternative = "two.sided")


# T-test Group 1 - SISLAB

tests_sislab <- filter(tests_all_b,
                       group == "Public health labs prior to the pandemic")

tests_sislab_1w <- tests_sislab$First.wave_average
tests_sislab_2w <- tests_sislab$Second.wave_average
tests_sislab_3w <- tests_sislab$Third.wave_average

t.test(tests_sislab_1w,
       tests_sislab_2w,
       paired = TRUE,
       alternative = "two.sided")

t.test(tests_sislab_1w,
       tests_sislab_3w,
       paired = TRUE,
       alternative = "two.sided")

t.test(tests_sislab_2w,
       tests_sislab_3w,
       paired = TRUE,
       alternative = "two.sided")

# T-test Group 2 - Others

tests_others <- filter(tests_all_b,
                       group == "Labs enabled during pandemic")

tests_others_1w <- tests_others$First.wave_average
tests_others_2w <- tests_others$Second.wave_average
tests_others_3w <- tests_others$Third.wave_average


t.test(tests_others_1w,
       tests_others_2w,
       paired = TRUE,
       alternative = "two.sided")

t.test(tests_others_1w,
       tests_others_3w,
       paired = TRUE,
       alternative = "two.sided")

t.test(tests_others_2w,
       tests_others_3w,
       paired = TRUE,
       alternative = "two.sided")