create_billing_statement <- function(billing_list, summarized_user_list) {
  # join user list to billing list
  billing_statement <- dplyr::full_join(billing_list, summarized_user_list, by = "agency_name") |>
    dplyr::mutate_at("quarter_user_count", ~replace(., is.na(.), 0)) |>
    dplyr::mutate_at("quarter_user_list", ~replace(., is.na(.), "No HMIS Users renewed or were trained this quarter"))

  # Do math for quarter totals and invoice totals
  previous_quarter <- get_previous_quarter(year = FALSE)
  agency_fee <- 500 # Not sure what yearly fee is?

  billing_statement <- billing_statement %>%
    dplyr::mutate(quarter_total = quarter_user_count * 175,
           agency_fee = dplyr::if_else(previous_quarter == "Q1", agency_fee, 0),
           invoice_total = quarter_total+agency_fee,
           invoice_date = lubridate::today(),
           invoice_number = paste(previous_quarter,"-",agency_id, sep ="")
    ) |>
    dplyr::filter(invoice_total != 0)

  return(billing_statement)
}

save_billing_statement <- function(billing_statement) {
  # Create CSV for billing
  billing_statement['paid'] <- ""
  billing_statement['note'] <- ""
  billing_statement['agency_billing_name'] <- billing_statement$agency_name

  col_order <- c("paid",
                 "invoice_number",
                 "note",
                 "invoice_date",
                 "invoice_total",
                 "agency_name",
                 "agency_billing_name",
                 "address",
                 "city",
                 "state",
                 "zip_code",
                 "quarter_user_count",
                 "quarter_user_list",
                 "quarter_total",
                 "agency_id",
                 "agency_admin",
                 "agency_admin_email",
                 "agency_admin_name",
                 "user_list",
                 "program_list")

  billing_statement <- billing_statement[, col_order]

  previous_quarter <- get_previous_quarter()

  readr::write_csv(
    billing_statement,
    (paste("data-raw/statements/", previous_quarter, "_billing_statement.csv", sep = ""))
  )
}