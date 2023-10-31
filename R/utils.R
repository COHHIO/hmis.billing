get_previous_quarter <- function(date = lubridate::today(), year = TRUE) {
  # Ensure the input date is in Date format
  date <- as.Date(date)

  # Calculate the previous quarter
  previous_quarter <- lubridate::quarter(date) - 1
  if (previous_quarter == 0) {
    # If the previous quarter is Q0 (the 4th quarter of the previous year)
    # Adjust the year accordingly
    previous_year <- lubridate::year(date) - 1
    previous_quarter <- 4
  } else {
    previous_year <- lubridate::year(date)
  }

  # Format the result based on the 'year' argument
  if (year) {
    result <- paste0(previous_year, "Q", previous_quarter)
  } else {
    result <- paste0("Q", previous_quarter)
  }

  return(result)
}



#' Check if any active HMIS users are not in eLearning
#'
#' @param users a dataframe
#' @param user_course_report a dataframe
#'
#' @return Returns error and list if any active users aren't in eLearning
#' @export
#'
#' @examples
#' get_users_not_in_elearning(users = files$users,
#'                            user_course_report = files$user_course_report)

get_users_not_in_elearning <- function(users = NULL,
                                       user_course_report = NULL) {
  active_users_email <- users |>
    dplyr::filter(status == "Active") |>
    dplyr::filter(email != "hmisdataanalyst@cohhio.org") |>
    dplyr::select(email)

  not_in_elearning <- dplyr::anti_join(active_users_email,
                                       user_course_report["email"],
                                       by = dplyr::join_by(email))

  # error if any HMIS users are not in eLearning
  if (nrow(not_in_elearning) > 0) {
    stop(paste0("HMIS Users not in eLearning:", not_in_elearning))
  }
}

#' Check HMIS users overdue for renewal; give results to Monica
#'
#' @param users a dataframe
#' @param user_course_report a dataframe
#'
#' @return Returns message and list of users
#' @export
#'
#' @examples
#' get_users_overdue_renewal(users = users,
#'                            user_course_report = user_course_report)
get_users_overdue_renewal <- function(users = NULL,
                                      user_course_report = NULL) {
  not_compliant <- user_course_report |>
    dplyr::filter(active == "TRUE", compliant == "FALSE") |>
    dplyr::select(email)

  hmis_users_overdue_for_renewal <- dplyr::inner_join(users, not_compliant, by = "email") |>
    dplyr::filter(status == "Active") |>
    dplyr::filter(email != "hmis@cohhio.org") |>
    dplyr::select(full_name, email, assigned_staff_home_agency)

  if (nrow(hmis_users_overdue_for_renewal) > 0) {
    message("The below tibble are users overdue for renewal:")

    print(hmis_users_overdue_for_renewal)
  }
}


#' Return agencies that don't have an admin
#' Check if the agency admins are active
#'
#' @param agency a dataframe
#' @param agency_admin_address a dataframe
#'
#' @return Returns a message and list of agencies
#' @export
#'
#' @examples
#' get_agencies_no_admin <- function(agency = agency,
#'                                   agency_admin_address = agency_admin_address)
get_agencies_no_admin <- function(agency = agency,
                                  agency_admin_address = agency_admin_address) {
  # The following agencies have no HMIS Agency Administrator (Property Manager)
  # Add an error if list includes anything other than system
  # check if the agency admins are active users
  agencies_no_admin <- dplyr::anti_join(agency, agency_admin_address,
                                        by = dplyr::join_by(agency_name)) |>
    dplyr::filter(agency_name != "System")

  if (nrow(agencies_no_admin) > 0) {
    message("The below tibble are agencies without an admin:")

    print(agencies_no_admin)
  }

}







