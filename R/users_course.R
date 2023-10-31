create_course_user_list <- function(user_course_report) {
  # add column for completed quarter
  user_course <- user_course_report |>
    dplyr::select(user,
           email,
           completed_date,
           company_name) |>
    dplyr::mutate(completed_date = lubridate::mdy_hms(completed_date)) |>
    dplyr::mutate(completed_quarter = lubridate::quarter(completed_date, with_year = TRUE)) |>
    dplyr::mutate(completed_quarter = as.character(completed_quarter)) |>
    dplyr::mutate(completed_quarter = stringr::str_replace(completed_quarter, '\\.', "Q"))

  # get billing quarter
  billing_quarter <- get_previous_quarter()

  users_this_quarter <- user_course |>
    dplyr::filter(completed_quarter == billing_quarter)

  return(users_this_quarter)
}



summarize_course_users <- function(users, users_this_quarter) {
  user_course <- merge(x = users_this_quarter, y = users[ , c("assigned_staff_home_agency", "email")], by = "email", all.x=TRUE)

  user_course$assigned_staff_home_agency[is.na(user_course$assigned_staff_home_agency)] <- user_course$company_name[is.na(user_course$assigned_staff_home_agency)]

  summarized_list <- user_course %>%
    dplyr::group_by(assigned_staff_home_agency) |>
    dplyr::summarise(quarter_user_list = paste(user, collapse = ", "),
                     quarter_user_count = dplyr::n()) |>
    dplyr::rename(agency_name = assigned_staff_home_agency)

  return(summarized_list)
}

