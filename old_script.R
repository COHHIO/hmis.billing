library(tidyverse)
library(janitor)

warning("Check the invoice template to make sure that Agency Fees only appear on quarter 1", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,
        domain = NULL)


warning("Download all 6 reports", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,
        domain = NULL)

warning("You will need to enter the billing quarter like 202201", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,
        domain = NULL)

# billing_quarter <- readline("Enter billing year and quarter like 202204:")

billing_quarter <- "202301"

# import reports

agency_admin_address <- read.csv(paste("data/", billing_quarter, "/1_billing_agency_admin_address_table.csv", sep = "")) %>%
  clean_names()

agency <- read.csv(paste("data/", billing_quarter, "/2_billing_agency_table.csv", sep = "")) %>%
  clean_names()

programs <- read.csv(paste("data/", billing_quarter, "/3_billing_programs_table.csv", sep = "")) %>%
  clean_names()

users <- read.csv(paste("data/", billing_quarter, "/4_billing_user_table.csv", sep = "")) %>%
  clean_names()

billing_address <- read.csv(paste("data/", billing_quarter, "/5_billing_billing_address_table.csv", sep = "")) %>%
  clean_names()

user_course_report <- read.csv(paste("data/", billing_quarter,"/6_hmis-user-licensing-course-report.csv", sep = "")) %>%
  clean_names()

# prepare data

users$email <- tolower(users$email)

user_course_report$email <- tolower(user_course_report$email)

# Active HMIS Users not in eLearning
# Add error if list is longer than 1 (hmisdataanalyst account)
emails_hmis_active <- users %>%
  filter(status == "Active") %>%
  select(email)

emails_course <- user_course_report["email"]

not_in_eLearning <- anti_join(emails_hmis_active, emails_course)

not_in_eLearning

warning("This list of emails are HMIS Users who are in Clarity and not in Litmos.
        Confirm that email addresses match in both accounts.")

# # Who is stuck in the eLearning stage? This does not appear to be working correctly, harold.sparks@talberthouse.org should be on this stuck in eLearning list.
#
# emails_course_active_incompliant <- user_course_report[21] %>%
#   filter(user_course_report[13] == "True" & user_course_report[10] == "False")
#
# stuck_in_elearning <- anti_join(emails_course_active_incompliant, emails_hmis_active,)
#
# stuck_in_elearning
#
# warning("Above is stuck in eLearning list")

# Not billing specific but adjecent
# HMIS Users Overdue for Renewal

not_compliant <- user_course_report %>%
  filter(active == "TRUE", compliant == "FALSE") %>%
  select(email)

hmis_users_overdue_for_renewal <- inner_join(users, not_compliant, by = "email") %>%
  filter(status == "Active")

write_csv(
  hmis_users_overdue_for_renewal,
  (paste("data/", billing_quarter, "/output/", "hmis_users_overdue_for_renewal.csv", sep = ""))
)

warning("Above list is overdue for renewal. It has also been written to a csv")


# The following agencies have no HMIS Agency Administrator (Property Manager)
# Add an error if list includes anything other than system
all_agencies <- agency[2]
agencies_with_aas <- agency_admin_address[2]

setdiff(all_agencies, agencies_with_aas)


# Create a string of users for each agency.

active_users <- users %>%
  filter(
    status == "Active")

active_users <- active_users[,c(2,4)]

active_users <- active_users %>%
  group_by(assigned_staff_home_agency) %>%
  summarise(user_list = paste(full_name, collapse = ", "))

# Join user_list to main table (I should only join certain columns here to avoid dups)

colnames(active_users)[1] <- "agency_name"

billing_data <- left_join(agency, active_users, by = "agency_name")

# Add access role to differentiate payment levels
# Replace NAs in user_list with "No HMIS Users have this agency as their main agency."

billing_data <- mutate_at(billing_data, "user_list", ~replace(., is.na(.), "No HMIS Users have this agency as their main agency."))

#Create a string of programs for each agency.

program_list <- programs[,c(2,5)]

program_list <- program_list %>%
  group_by(agency_name) %>%
  summarise(program_list = paste(name, collapse = ", "))

# Join program_list to main table

billing_data <- left_join(billing_data, program_list, by = "agency_name")

# Join Property Manager and address to table by agency name. This one ruins everything.

agency_admin_address <- agency_admin_address %>%
  select(agency_name, agency_admin, address, city, state, zip_code)

billing_data <- left_join(billing_data, agency_admin_address, by = "agency_name")

# Join Agency Admin (Property Manager) name and email address by user id

agency_admin_user_data <- users %>%
  select(id, email, full_name)

colnames(agency_admin_user_data) <- c("agency_admin", "agency_admin_email", "agency_admin_name")

agency_admin_user_data$agency_admin <- as.character(agency_admin_user_data$agency_admin)

billing_data <- left_join(billing_data, agency_admin_user_data, by = "agency_admin")


# Select columns from HMIS User Licensing Course Report

user_course <- user_course_report %>%
  select(user,
         email,
         completed_date,
         email,
         company_name)

# change date column to quarter

user_course$completed_date <- lubridate::mdy(user_course$completed_date)

# user_course$completed_date <- lubridate::as_date(user_course$completed_date,
#                                                  format = "%m/%d/%Y")



user_course$completed_quarter <- lubridate::quarter(user_course$completed_date, with_year = TRUE)

user_course$completed_quarter <- as.character(user_course$completed_quarter)

user_course$completed_quarter <- str_replace(user_course$completed_quarter, '\\.', "0")

# filter for only this billing quarter

user_course <- user_course %>%
  filter(completed_quarter == billing_quarter)

user_course <- merge(x = user_course, y = users[ , c("assigned_staff_home_agency", "email")], by = "email", all.x=TRUE)

user_course$assigned_staff_home_agency[is.na(user_course$assigned_staff_home_agency)] <- user_course$company_name[is.na(user_course$assigned_staff_home_agency)]


# From user course, make a quarter 2 summary dataframe

quarter_user_count <- user_course %>%
  filter(completed_quarter == billing_quarter)%>%
  count(assigned_staff_home_agency)

quarter_user_list <- user_course %>%
  filter(completed_quarter == billing_quarter)%>%
  group_by(assigned_staff_home_agency) %>%
  summarise(quarter_user_list = paste(user, collapse = ", "))

colnames(quarter_user_count)[1] ="agency_name"
colnames(quarter_user_list)[1] ="agency_name"

quarter <- left_join(quarter_user_count, quarter_user_list, by = "agency_name")

colnames(quarter)[2] ="quarter_user_count"

# Make a new rule that every Colleen agency gets her address and agency billing name


# full join with billing data

billing_data <- full_join(billing_data, quarter, by = "agency_name")

# replace NAs in billing data

billing_data <- mutate_at(billing_data, "quarter_user_count", ~replace(., is.na(.), 0))
billing_data <- mutate_at(billing_data, "quarter_user_list", ~replace(., is.na(.), "No HMIS Users renewed or were trained in this quarter"))

# Do math for quarter totals and invoice totals

warning("Update invoice date", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,
        domain = NULL)

# billing_data <- billing_data %>%
#   mutate(quarter_total = quarter_user_count*175,
#          invoice_total = quarter_total,
#          invoice_date = lubridate::today(),
#          invoice_number = paste(billing_quarter,"-",agency_id, sep ="")
#   )

billing_data <- billing_data %>%
  mutate(quarter_total = quarter_user_count*175,
         agency_fee = 550,
         invoice_total = quarter_total+agency_fee,
         invoice_date = lubridate::today(),
         invoice_number = paste(billing_quarter,"-",agency_id, sep ="")
  )


# Eliminate Invoices with $0

billing_data <- billing_data %>%
  filter(invoice_total != 0)

# Overwrite address with billing addresses.

warning("This is not appearing in Looker, CH put in a ticket and update by hand", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,
        domain = NULL)

# billing_data <- rows_update(
#   billing_data,
#   select(billing_address, agency_name, address, city, state, zip_code),
#   by = "agency_name", unmatched = "ignore")


# Create CSV for billing

billing_data['paid'] <- ""
billing_data['note'] <- ""
billing_data['agency_billing_name'] <- billing_data$agency_name

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

billing_data <- billing_data[, col_order]


write_csv(
  billing_data,
  (paste("data/", billing_quarter, "/output/", billing_quarter, "_billing_data.csv", sep = ""))
)

