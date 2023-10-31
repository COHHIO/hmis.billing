#' Load Looker files and eLearning file
#'
#' @return A list of dataframes.
#' @export
#'
#' @examples
#' files <- load_csv_files()
load_csv_files <- function() {
  last_quarter <- get_previous_quarter()
  files <- list.files(path = paste0("data-raw/reports/", last_quarter),
                    pattern = "*.csv",
                    full.names = T) |>
    purrr::map(~readr::read_csv(., col_types = readr::cols(.default = "c"))) |>
    purrr::map(janitor::clean_names)

  # names of the data frames
  file_names <- c("agency_admin_address", "agency", "programs", "users",
                  "billing_address", "user_course_report")

  names(files) <- file_names

  # clean email fields
  files$users$email <- tolower(files$users$email)

  files$user_course_report$email <- tolower(files$user_course_report$email)

  return(files)
}



