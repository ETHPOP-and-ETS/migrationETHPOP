
# compare ETHPOP projections
# and ONS projections
# for inmigration and outmigration
#


library(readr)
library(dplyr)
library(ggplot2)


# read in ONS

dir_data <- "D:/data/"

uk_inmigration <- readRDS(paste0(dir_data, "uk_inmigration.rds"))
uk_outmigration <- readRDS(paste0(dir_data, "uk_outmigration.rds"))

# read in ETHPOP

dir_ETHPOP <- "C:/Users/Nathan/Documents/R/cleanETHPOP/output_data/"

ethpop_inmigrants <- read_csv(paste0(dir_ETHPOP, "clean_inmigrants.csv"))
ethpop_outmigrants <- read_csv(paste0(dir_ETHPOP, "clean_outmigrants.csv"))

# combine data

in_ethpop_sex_year <-
  ethpop_inmigrants %>%
  group_by(sex, year) %>%
  summarise(pop = sum(inmigrants))

out_ethpop_sex_year <-
  ethpop_outmigrants %>%
  group_by(sex, year) %>%
  summarise(pop = sum(outmigrants))

# compare inmigration

ggplot(in_ethpop_sex_year, aes(x = year, y = pop, colour = sex)) +
  geom_line() +
  theme_bw() +
geom_line(data = uk_inmigration, aes(x = year, y = inflow, colour = sex))

# compare outmigration

ggplot(out_ethpop_sex_year, aes(x = year, y = pop, colour = sex)) +
  geom_line() +
  theme_bw() +
geom_line(data = uk_outmigration, aes(x = year, y = outflow, colour = sex))


