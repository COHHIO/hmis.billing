update_billing_statement <- function() {
  # get the data
  files <- load_csv_files()

  agency_admin_address <- files$agency_admin_address
  agency <- files$agency
  billing_address <- files$billing_address
  programs <- files$programs
  users <- files$users
  user_course_report <- files$user_course_report

  billing_list <- create_billing_list(users, active_users, programs, agency, agency_admin_address)
  users_this_quarter <- create_course_user_list(user_course_report)
  summarized_user_list <- summarize_course_users(users, users_this_quarter)

  billing_statement <- create_billing_statement(billing_list, summarized_user_list)
  save_billing_statement(billing_statement)
}
