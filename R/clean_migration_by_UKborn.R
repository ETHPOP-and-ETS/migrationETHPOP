
#' clean_migration_by_UKborn
#'
#' @param dir_path
#' @param input_file_name
#' @param rtn
#' @param save_to_file
#' @param save_path
#' @param save_format
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

  inflow <- readxl::read_xls(paste(dir_path, input_file_name, sep = "/"),
                             sheet = "Table 2.03",
                             range = "A17:I40")

  outflow <- readxl::read_xls(paste(dir_path, input_file_name, sep = "/"),
                              sheet = "Table 2.03",
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
           total = as.numeric(total)) %>%
    mutate(pUKborn = UKborn/total,
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
           total = as.numeric(total)) %>%
    mutate(pUKborn = UKborn/total,
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
