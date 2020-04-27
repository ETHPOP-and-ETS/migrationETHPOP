
# compare ETHPOP and LFS
# for inmigration


library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

# read in data ----

## ETHPOP

dir_ETHPOP <- "C:/Users/Nathan/Documents/R/cleanETHPOP/output_data/"

ethpop_inmigrants <- read_csv(paste0(dir_ETHPOP, "clean_inmigrants.csv"))
ethpop_outmigrants <- read_csv(paste0(dir_ETHPOP, "clean_outmigrants.csv"))
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


#########
# plots #
#########

ggplot(in_ethpop_sex_year, aes(x = year, y = pop, colour = sex)) +
  geom_line() +
  theme_bw() +
  geom_line(data = uk_inmigration, aes(x = year, y = inflow, colour = sex)) +
  geom_line(data = lfs_sex_year, aes(x = Year, y = pop, colour = Sex))

