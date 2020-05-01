
#' clean_migration_by_UKborn
#'
#' From ONS migration statistics table
#'
#' @param dir_path input data
#' @param input_file_name
#' @param rtn TRUE/FALSE
#' @param save_to_file TRUE/FALSE
#' @param save_path output data
#' @param save_format csv or RData
#'
#' @import readxl, dplyr, reshape2
#' @return
#' @export
#'
clean_migration_by_UKborn <- function(dir_path = here::here("rawdata"),
                                      input_file_name = "2.03ltimcountryofbirth2004to2018.xls",
                                      rtn = TRUE,
                                      save_to_file = TRUE,
                                      save_path = here::here("output_data"),
                                      save_format = "csv") {

  sheet_name <- "Table 2.03"
  data_path <- paste(dir_path, input_file_name, sep = "/")

  inflow <- readxl::read_xls(data_path,
                             sheet = sheet_name,
                             range = "A17:I40")

  outflow <- readxl::read_xls(data_path,
                              sheet = sheet_name,
                              range = "A17:I58")
  inflow <-
    inflow %>%
    select_if(~!(all(is.na(.)) | all(. == ""))) %>%  # remove empty columns
    select_at(vars(-contains(".."))) %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(!is.na(Year)) %>%
    rename(UKborn = `United Kingdom`,
           NonUKborn = `Non-United Kingdom`,
           total = `All countries`) %>%
    mutate(UKborn = as.numeric(UKborn),
           NonUKborn = as.numeric(NonUKborn),
           total = as.numeric(total),
           pUKborn = UKborn/total,
           pNonUKborn = NonUKborn/total)

  outflow <-
    outflow %>%
    select_if(~!(all(is.na(.)) | all(. == ""))) %>%
    select_at(vars(-contains(".."))) %>%
    mutate(Year = as.numeric(Year)) %>%
    rename(UKborn = `United Kingdom`,
           NonUKborn = `Non-United Kingdom`,
           total = `All countries`) %>%
    filter(total < 0) %>%
    mutate(UKborn = as.numeric(UKborn),
           NonUKborn = as.numeric(NonUKborn),
           total = as.numeric(total),
           pUKborn = UKborn/total,
           pNonUKborn = NonUKborn/total)

  if (save_to_file) {
    write.csv(inflow, file = here::here("output_data", "inflow.csv"))
    save(inflow, file = here::here("data", "inflow.RData"))

    write.csv(outflow, file = here::here("output_data", "outflow.csv"))
    save(outflow, file = here::here("data", "outflow.RData"))
  }

  if (rtn)
    return(list(inflow = inflow,
                outflow = outflow))
}
