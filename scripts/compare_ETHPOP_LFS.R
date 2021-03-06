
# compare ETHPOP and LFS
# for inmigration


library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

# read in data ----

## ETHPOP

dir_ETHPOP <- "C:/Users/Nathan/Documents/R/cleanETHPOP/output_data/"

ethpop_inmigrants <- read_csv(paste0(dir_ETHPOP, "clean_inmigrants_Leeds2.csv"))
ethpop_outmigrants <- read_csv(paste0(dir_ETHPOP, "clean_outmigrants_Leeds2.csv"))
ethpop_births <- read_csv(paste0(dir_ETHPOP, "clean_births.csv"))

## LFS

lfs <- read_rds(paste0(dir_data, "formatted_LFS_2011_2016.rds"))


# combine data ----

# compare totals each year
# compare total each year by ethnic group
# compare total each year by age group
# compare total each year by sex

# time in uk < 1 year
# all ages
# all uk born/non-uk born


in_ethpop_sex_year <-
  ethpop_inmigrants %>%
  group_by(sex, year) %>%
  summarise(pop = sum(inmigrants))

lfs_sex_year <-
  lfs %>%
  filter(timeinUK == "(0,1]") %>%
  group_by(Sex, Year) %>%
  summarise(pop = sum(Weight))

lfs_sex_year$Sex <- paste0(lfs_sex_year$Sex, "_lfs")
in_ethpop_sex_year$sex <- paste0(in_ethpop_sex_year$sex, "_ethpop")
uk_inmigration$sex <- paste0(uk_inmigration$sex, "_ons")

#########
# plots #
#########

ggplot(in_ethpop_sex_year, aes(x = year, y = pop, colour = sex)) +
  geom_line() +
  theme_bw() +
  ylim(0, 400000) +
  geom_line(data = uk_inmigration, aes(x = year, y = inflow, colour = sex)) +
  geom_line(data = lfs_sex_year, aes(x = Year, y = pop, colour = Sex))


# run this to create html doc for GitHub
# rmarkdown::render("scripts/compare_ETHPOP_LFS.R", output_dir = here::here("docs"), output_format = "html_document")
