
df_enrollment_raw <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                               "Data")


# load the unwinding plans
df_unwindingPlans <- "State Approaches to the Unwinding Period" |>
  kffR::fetchIndicator() |>
  # filter(Category %in% c("Month in Which State Will Intiate Renewals"))
  filter(Category %in% c("Month of First Anticipated Terminations for Procedural Reasons")) |>
  select(State, Value_character) |>
  filter(State != "United States") |>
  rowwise() |>
  # Turn the month name into a date:
  mutate(Month = paste0("2023-", stringr::str_sub(paste0("000", which(Value_character == month.name)), -2)),
         Date = as.Date(paste(Month, "-01", sep="")),
         Date_baselineMonth = lubridate::floor_date(Date - months(1), "month")) |>
  ungroup() |>
  rename(Date_plannedDisenrollment = Date)


# Archive today's data
try(expr = {
df_enrollment_raw |>
  write_csv(paste0("L:/KCMU/Medicaid Unwinding Tracking/data archive/monthlyEnrollmentData/",Sys.Date(), "_MonthlyEnrollmentData.csv"))
})
  # "QMB/LMB/QWD"
# "QMB SLIMB QI"
# "ABD MBIWD Dual"
# Clean the raw data:
df_enrollment <- df_enrollment_raw %>%
  filter(!is.na(Date)) %>%
  filter(Date >= as.Date("2023-01-01")) %>%
  # Create total
  mutate(
    Total = case_when(
      # Exclude QMB/LMB/QWD/SLIMB/QI
      !is.na(`Total Enrollment`) & !is.na(`QMB/LMB/QWD`)  ~ `Total Enrollment` - `QMB/LMB/QWD`,
      !is.na(`Total Enrollment`) & !is.na(`QMB SLIMB QI`) ~ `Total Enrollment` - `QMB SLIMB QI`,
      !is.na(`Total Enrollment`)                          ~ `Total Enrollment`,
      !is.na(`Badgercare Plus`)                           ~ `Badgercare Plus`,
      !is.na(`Managed Care`)                              ~ `Managed Care`,
      !is.na(`Eligilible Population`)                     ~ `Eligilible Population`,
      !is.na(`Expansion`)                                 ~ `Expansion`,
#      !is.na(`Children`)                                  ~ `Children`,
      !is.na(`Medical plus buy-in individuals`)           ~ `Medical plus buy-in individuals`,
#      !is.na(`CHIP`)                                      ~ `CHIP`,
      !is.na(`Fee For Service`)                           ~ `Fee For Service`,
      TRUE ~ NA_real_
    ),
    Total_flag = case_when(
      !is.na(`Total Enrollment`) & !is.na(`QMB/LMB/QWD`)    ~ "Total Enrollment - QMB/LMB/QWD",
      !is.na(`Total Enrollment`) & !is.na(`QMB SLIMB QI`) ~ "Total Enrollment - QMB SLIMB QI",
      !is.na(`Total Enrollment`)                            ~ "Total Enrollment",
      !is.na(`Badgercare Plus`)                             ~ "Badgercare Plus",
      !is.na(`Managed Care`)                                ~ "Managed Care",
      !is.na(`Eligilible Population`)                       ~ "Eligilible Population",
      !is.na(`Expansion`)                                   ~ "Expansion",
    #  !is.na(`Children`)                        ~ "Children",
      !is.na(`Medical plus buy-in individuals`)             ~ "Medical plus buy-in individuals",
    #  !is.na(`CHIP`)                            ~ "CHIP",
      !is.na(`Fee For Service`)                             ~ "Fee For Service",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Total)) %>%
  group_by(State) %>%
  # Order data by date:
  arrange(Date) %>%
  mutate(
    percentChange_previousMonth = (Total - lag(Total, n = 1, default = NA))/ lag(Total, n = 1, default = NA),
    percentChange_averageMonthly = mean(percentChange_previousMonth, na.rm = TRUE),
    percentChange_colorGroup = ifelse(percentChange_previousMonth >= 0,  kffR::kff_colors$blue4, kffR::kff_colors$orange)
  ) %>%
  ungroup() %>%
  left_join(df_unwindingPlans, by = c("State")) %>%
  group_by(State) %>%
  mutate(isRefDate = Date == Date_baselineMonth,
         maxTotal = max(Total[!Total %in% c(1056630) ]), # this hard code is for CT
         maxDate = Date[Total == maxTotal],
         isAfterMaxDate = Date > maxDate,
         refTotal = sum(Total[which(isRefDate)], na.rm = TRUE),
         percentChange_fromReferenceData = (Total - maxTotal)/maxTotal) %>%
  ungroup() %>%
 # filter(State == "Alaska") |> select(State, Total, Date, maxTotal, maxDate) |> print()
  # Rank states by their average change
  left_join({.} %>%
              filter(!State %in% c("United States", "District of Columbia", "Puerto Rico")) %>%
              filter(!is.na(percentChange_previousMonth) & !is.nan(percentChange_previousMonth)) %>%
              select(State, percentChange_averageMonthly) %>%
              distinct() %>%
              arrange(desc(percentChange_averageMonthly)) %>%
              mutate(Rank_state = row_number()) %>%
              select(State, Rank_state),
            by = c("State")) %>%
  left_join(kffR::df_states) %>%
  relocate(State, Abbr)


#

