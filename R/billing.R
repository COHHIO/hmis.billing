create_billing_list <- function(users, active_users, programs, agency, agency_admin_address) {

  # get a list of active users within users dataframe
  active_users <- users |>
    dplyr::filter(status == "Active") |>
    dplyr::select(assigned_staff_home_agency,
                  email,
                  full_name) |>
    dplyr::rename(agency_name = assigned_staff_home_agency)

  active_users <- active_users %>%
    dplyr::group_by(agency_name) %>%
    dplyr::summarise(user_list = paste(full_name, collapse = ", "))

  # get a list of programs within programs dataframe
  program_list <- programs |>
    dplyr::select(agency_name,
                  agency_type,
                  agency_active,
                  name) |>
    dplyr::group_by(agency_name) |>
    dplyr::summarise(program_list = paste(name, collapse = ", "))

  agency_admin_address <- agency_admin_address |>
    dplyr::select(agency_name, agency_admin, address, address2, city, state, zip_code) |>
    dplyr::distinct()

  # get agency admin name and address
  agency_admin_user_data <- users |>
    dplyr::select(id, email, full_name) |>
    dplyr::rename(agency_admin = id, agency_admin_email = email, agency_admin_name = full_name) |>
    dplyr::mutate(agency_admin = as.character(agency_admin))


  # join users, programs, and agency to billing dataframe
  billing_list <- dplyr::left_join(agency, active_users, by = "agency_name") |>
    dplyr::left_join(program_list, by = "agency_name") |>
    dplyr::left_join(agency_admin_address, by = "agency_name") |>
    dplyr::left_join(agency_admin_user_data, by = "agency_admin") |>
    dplyr::mutate_at("user_list", ~replace(., is.na(.),
                                           "No HMIS Users have this agency as their main agency."))

  return(billing_list)


}
