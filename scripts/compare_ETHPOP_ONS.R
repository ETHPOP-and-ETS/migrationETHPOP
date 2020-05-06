
# compare ETHPOP projections
# and ONS projections
# for inmigration and outmigration
#


library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

# read in data ----

## ONS

dir_data <- "C:/Users/Nathan/Documents/R/tbinenglanddataclean/output_data/"

uk_inmigration <- readRDS(paste0(dir_data, "uk_inmigration.rds"))
uk_outmigration <- readRDS(paste0(dir_data, "uk_outmigration.rds"))
uk_births <- readRDS(paste0(dir_data, "uk_births.rds"))

## ETHPOP

dir_ETHPOP <- "C:/Users/Nathan/Documents/R/cleanETHPOP/output_data/"

ethpop_inmigrants <- read_csv(paste0(dir_ETHPOP, "clean_inmigrants.csv"))
ethpop_outmigrants <- read_csv(paste0(dir_ETHPOP, "clean_outmigrants.csv"))
ethpop_births <- read_csv(paste0(dir_ETHPOP, "clean_births.csv"))


# combine data ----

## sum over age and ethnic group
## to match ONS

in_ethpop_sex_year <-
  ethpop_inmigrants %>%
  group_by(sex, year) %>%
  summarise(pop = sum(inmigrants))

out_ethpop_sex_year <-
  ethpop_outmigrants %>%
  group_by(sex, year) %>%
  summarise(pop = sum(outmigrants))

births_ethpop_sex_year <-
  ethpop_births %>%
  select(-X1) %>%
  group_by(sex, year) %>%
  summarise(pop = sum(births))



# ggplot(in_ethpop_sex_year, aes(x = year, y = pop, colour = sex)) +
#   geom_line() +
#   theme_bw() +
#   geom_line(data = out_ethpop_sex_year, aes(x = year, y = pop, colour = sex))


#########
# plots #
#########

# compare inmigration

ggplot(in_ethpop_sex_year, aes(x = year, y = pop, colour = sex)) +
  geom_line() +
  theme_bw() +
  geom_line(data = uk_inmigration, aes(x = year, y = inflow, colour = sex)) +
  ggtitle("in-migration")

# compare outmigration

ggplot(out_ethpop_sex_year, aes(x = year, y = pop, colour = sex)) +
  geom_line() +
  theme_bw() +
  geom_line(data = uk_outmigration, aes(x = year, y = outflow, colour = sex)) +
  ggtitle("out-migration")

# compare births

ggplot(births_ethpop_sex_year, aes(x = year, y = pop, colour = sex)) +
  geom_line() +
  theme_bw() +
  geom_line(data = uk_births, aes(x = year, y = births, colour = sex)) +
  ggtitle("births")
