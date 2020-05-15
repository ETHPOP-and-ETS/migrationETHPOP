
# compare ETHPOP population projections
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
  summarise(pop = sum(inmigrants)) %>%
  ungroup() %>%
  mutate(resource = "ethpop")

out_ethpop_sex_year <-
  ethpop_outmigrants %>%
  group_by(sex, year) %>%
  summarise(pop = sum(outmigrants)) %>%
  ungroup() %>%
  mutate(resource = "ethpop")

births_ethpop_sex_year <-
  ethpop_births %>%
  select(-X1) %>%
  group_by(sex, year) %>%
  summarise(pop = sum(births)) %>%
  ungroup() %>%
  mutate(resource = "ethpop")


in_dat <-
  uk_inmigration %>%
  mutate(resource = "ons",
         sex = ifelse(sex == "female",
                      yes =  "F",
                      ifelse(sex =="male",
                             yes = "M",
                             no = sex))) %>%
  rename(pop = inflow) %>%
  bind_rows(in_ethpop_sex_year) %>%
  filter(sex %in% c("M","F")) %>%
  mutate(interact = interaction(resource, sex))

out_dat <-
  uk_outmigration %>%
  mutate(resource = "ons",
         sex = ifelse(sex == "female",
                      yes =  "F",
                      ifelse(sex =="male",
                             yes = "M",
                             no = sex))) %>%
  rename(pop = outflow) %>%
  bind_rows(out_ethpop_sex_year) %>%
  filter(sex %in% c("M","F")) %>%
  mutate(interact = interaction(resource, sex))

births_dat <-
  uk_births %>%
  mutate(resource = "ons",
         sex = ifelse(sex == "female",
                      yes =  "F",
                      ifelse(sex =="male",
                             yes = "M",
                             no = sex))) %>%
  rename(pop = births) %>%
  bind_rows(births_ethpop_sex_year) %>%
  filter(sex %in% c("M","F")) %>%
  mutate(interact = interaction(resource, sex))


#########
# plots #
#########

# ggplot(in_ethpop_sex_year, aes(x = year, y = pop, colour = sex)) +
#   geom_line() +
#   theme_bw() +
#   geom_line(data = out_ethpop_sex_year, aes(x = year, y = pop, colour = sex))


##TODO: don't know whay the shape argument doesn't work...
# compare inmigration

ggplot(in_dat, aes(x = year, y = pop, colour = interact, shape = interact, group = interact)) +
  geom_line() +
  theme_bw() +
  ggtitle("in-migration")

# compare outmigration

ggplot(out_dat, aes(x = year, y = pop, colour = interact, shape = interact, group = interact)) +
  geom_line() +
  theme_bw() +
  ggtitle("out-migration")

# compare births

ggplot(births_dat, aes(x = year, y = pop, colour = interact, shape = interact, group = interact)) +
  geom_line() +
  theme_bw() +
  ggtitle("births")
