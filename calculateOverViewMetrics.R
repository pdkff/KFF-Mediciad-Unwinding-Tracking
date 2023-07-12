# Load resources ----
library(tidyverse)
library(googlesheets4)

# Datawrapper ids
dw_fig1_number <- "NY1hN"
dw_fig1_rate <- "BA0VO"
dw_fig2 <- "Sgez3"

dw_fig_cumPercentChange <- "xFIhx"
dw_fig_childrenVsAdults <- "AyL7P"

vec_figure1_statesWithNotes <- c("Ohio", "Idaho")

# Read in the raw datasets from the project's google sheets  ----

df_unwinding_raw <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                          "Data (unwinding)") |>
  filter(!is.na(Date))
df_unwinding_cms_raw <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                              "CMS Monthly data") |>
  filter(!is.na(Date))




# Which states are we using state reported sources? (non-cms report reports)
vec_states_withNonCMSSource <- c(# "Arizona", "Idaho", "Pennsylvania" # Have CMS reports
  "Alaska",
  "Arizona",
  "Arkansas",
  "Colorado",
  "District of Columbia",
  "Idaho",
  # "Iowa ????" , # where did the Iowa data come from?
  "Kansas",
  "Kentucky",
  "Maryland",
  "Massachusetts",
  "Minnesota",
  "Montana",
  "Nebraska",
  "Oklahoma",
  "Rhode Island",
  "Virginia",
  "South Carolina",
  "South Dakota",
  "Washington")

vec_states_useCMSforProceduralDisenrollments <- c("Arkansas")
# Which states report cumulative totals?
# For these states we will take the latest value, for others we will sum across all available data:
vec_states_takeLatestValue <- c(
  #"Minnesota", # ?
  "Idaho",
  "Virginia",
  "Nebraska",
  "South Carolina"
)
#states_toRemove_rateDisenrollmentRates <- c("District of Columbia")
states_toRemove_proceduralDisenrollmentRates <- c("District of Columbia")
vec_states_useEnrollmentData <- c(
#  "New Hampshire"
)

vec_states_withHardcodes <- c(
  "Arkansas"
)


# Clean up the Data ----
# State dashboard and reporting ----
df_unwinding <- df_unwinding_raw |>
  filter(State %in% vec_states_withNonCMSSource) |>
  mutate(Renewals = case_when(State %in% c("Alaska",
                                           "Arizona",
                                           "Kansas",
                                           "Kentucky",
                                           "Montana",
                                           "Nebraska",
                                           "Virginia") ~ `Renewals Approved`,
                              State %in% c("Colorado",
                                           "District of Columbia",
                                           "Maryland",
                                           "South Carolina") ~ `Renewed on an ex parte basis` + `Renewed via renewal form`,
                              State %in% c("Idaho") ~  `Determined eligible` ,
                              State %in% c("Massachusetts") ~ `Renewals Approved`, # Does not report
                              State %in% c("Minnesota") ~ `Auto-renewed` + `Coverage Extended` ,
                              State %in% c("Rhode Island") ~ `Renewed via exparte` + `Renewed via packet`,
                              State %in% c("South Dakota") ~ NA_real_,
                              TRUE ~ NA_real_)) |>
  mutate(
         Disenrollments = case_when(State %in% c("Arizona") ~ `Renewals Discontinued`,
                                    #State %in% c("District of Columbia") ~ `Renewals Discontinued`,
                                    State %in% c("Idaho") ~ `Determined ineligible`,
                                   State %in% c("Kentucky") ~ `Medicaid Terminations`,
                                    State %in% c("Alaska",
                                                 "Colorado",
                                                 "District of Columbia",
                                                 "Kansas",
                                                 "Maryland",
                                                 "Oklahoma",
                                                 "South Carolina",
                                                 "Rhode Island") ~ `Determined ineligible` + `Terminated for procedural reasons`,
                                    State %in% c("Massachusetts",
                                                 "Washington") ~ `Total Disenrolled`,
                                    State %in% c("Minnesota") ~ `Coverage Closed`,
                                    State %in% c("Montana","Nebraska") ~ `Renewals Discontinued`,
                                    State %in% c("Ohio") ~ `Closed Renewals (No response)`,
                                   # State %in% c() ~ `Determined ineligible` + `Terminated for procedural reasons`,
                                    State %in% c("Virginia") ~ `Renewals Discontinued`,
                                    State %in% c("South Dakota") ~ `Determined ineligible` + `Terminated for procedural reasons` + `Disenrolled due to a change in circumstances outside of renewal`,
                                    TRUE ~ 0
         )) |>
  select(State, Date, Renewals, Disenrollments) |>
  filter(!is.na(Disenrollments)) |>
  # combine the months of reporitng into a consolidated view of total disenrollments/renewals
  group_by(State) |>
  # Take latest value for states that report cumulative data
    arrange(desc(Date)) |>
    filter(!(State %in% vec_states_takeLatestValue) | row_number() == 1) |>
# Aggregate total renewals and disenrollments
    summarise(Dates = paste(min(Date), " to ", max(Date)),
              Date_latest = max(Date),
              Renewals = sum(Renewals),
              Disenrollments = sum(Disenrollments))|>
  # Add hardcodes:
  filter(!State %in% vec_states_withHardcodes) |>
  add_row(State = "Arkansas",
          Dates = "2023-06-01",
          Renewals = 61236 + 39848 + 50366,
          Disenrollments = 77468+ 66838+72802) |>
  mutate(Rate = case_when(is.na(Renewals) | is.na(Disenrollments) ~ NA_real_,
                          TRUE ~ Disenrollments/(Renewals + Disenrollments))) |>
print()

# Which columsn are used by each state?

# Nebraska: `Renewals Approved`, `Renewals Discontinued`
# Virginia: `Renewals Approved`, `Renewals Discontinued`
# South Dakota: `Disenrolled (determined ineligle)`, `Disenrolled for lack of response`, `Disenrolled due to a change in circumstances outside of renewal`

# CMS Reports -----
df_unwinding_cms <- df_unwinding_cms_raw |>
  filter(!is.na(Date)) |>
  filter(!is.na(`Renewed ex parte (5a1)`) & `Renewed ex parte (5a1)` != 0) |>
  group_by(State) |>
  # Sum up the reported renewals/disenrollments accross all reports
    summarise(
      Dates = paste(min(Date), " to ", max(Date)),
      Date_latest = max(Date),
      `Renewed ex parte (5a1)` = sum(`Renewed ex parte (5a1)`, na.rm = TRUE),
      `Renewed renewal form (5a2)` = sum(`Renewed renewal form (5a2)`, na.rm = TRUE),
      `Determined ineligible` = sum(`Ineligible transfered to marketplace (5b)`, na.rm = TRUE),
      `Terminated for procedural reasons` = sum(`Terminated procedural reasons (5c)`, na.rm = TRUE)) |>
  # Add columns for total renewals, and for total disenrollments
  mutate(Renewals = `Renewed ex parte (5a1)` + `Renewed renewal form (5a2)`,
         Disenrollments = `Determined ineligible` + `Terminated for procedural reasons`) |>
  # Hardcodes
  # mutate(Disenrollments = case_when(
  #           FALSE & State == c("Arkansas") ~ 72802,
  #           TRUE ~ Disenrollments)) |>
  # Calculate Rate of disenrollments
  mutate(Rate = ifelse(is.na(Renewals) | Disenrollments == 0, NA, Disenrollments/(Renewals+Disenrollments)),
         Rate_proceduralReason = ifelse(is.na(`Terminated for procedural reasons`) | `Terminated for procedural reasons` == 0, NA, `Terminated for procedural reasons`/(`Terminated for procedural reasons`+ `Determined ineligible`))) |>
  relocate(State,
           Dates, Date_latest,
           Renewals, Disenrollments, Rate, Rate_proceduralReason) |>
  print()

# Remove states where needed (ARKANSAS for EXAMPLE needs their CMS data for some things, but not others)
df_unwinding_cms_allStates <- df_unwinding_cms
df_unwinding_cms <- df_unwinding_cms |>
filter(!State %in% vec_states_withNonCMSSource)


# Calculate high level numbers----
df_cleanedAndCombined <- bind_rows(
  df_unwinding_cms |>
  mutate(Source = "CMS Report") |>
  select(State, Dates, Date_latest, Source, Renewals, Disenrollments, Rate ),
df_unwinding |>
  mutate(Source = "State Dashboard/Report")
) %>%
  # Remove any states with zero disenrllments: (ex. Nevada)
  filter(Disenrollments != 0) |>
  mutate(Rate_median = NA,
         NumStatesReporting = NA,
         Date_latestReporting = NA,
         Min_rate = NA, Min_state = NA,
         Max_rate = NA, Max_state = NA
         ) %>%
    add_row(State = "Overall",
          Renewals = sum(.$Renewals, na.rm = TRUE),
          Disenrollments = sum(.$Disenrollments, na.rm = TRUE),
          Rate = (sum(.$Disenrollments, na.rm = TRUE))/(sum(.$Renewals, na.rm = TRUE) + sum(.$Disenrollments, na.rm = TRUE)),
          Rate_median = round(median(.$Rate, na.rm = TRUE), 2),
          NumStatesReporting = paste(nrow(.) - 1, "states and DC"),
          Date_latestReporting = format(max(.$Date_latest), "%B, %Y"),
          Min_rate = round(100*min(.$Rate, na.rm = TRUE), 1),
          Min_state = .$State[which(round(100*.$Rate, 1) == Min_rate)],
          Max_rate =round(100*max(.$Rate, na.rm = TRUE), 1),
          Max_state = .$State[which(round(100*.$Rate, 1) == Max_rate)]
          ) |>
  select(-Date_latest)|>
  mutate(`Completed Renewals` = Renewals + Disenrollments) |>
  arrange(desc(State == "Overall"), State) |>
    print()

#   googlesheets4::range_write(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
#                              sheet = "Overview Page",
#                              range = "A1")


# Push data to our figures----

# Write text callouts for figures
df_forTitle_figure1 <- df_cleanedAndCombined |>
  filter(State == "Overall") |>
  mutate(Disenrollments_rounded = round(Disenrollments, -3),
         Disenrllments_roundedDown = Disenrollments - (Disenrollments %% 1000),
         HigherOrLowerAfterRounding = ifelse(Disenrollments_rounded > Disenrollments, "Nearly ", "Over")) |>
  select(Disenrollments,
         Disenrollments_rounded,
         Disenrllments_roundedDown,
         Rate,
         HigherOrLowerAfterRounding,
         NumStatesReporting,
         Min_state,Max_state,
         Min_rate, Max_rate,
         Date_latestReporting)

if(!"District of Columbia" %in% df_cleanedAndCombined$State) errorCondition("DC not present, change title!")

figureTitle_number <-  paste0(
      #df_forTitle_figure1$HigherOrLowerAfterRounding,
      "At least ",
      format(df_forTitle_figure1$Disenrllments_roundedDown, big.mark = ","),
      " Medicaid enrollees have been disenrolled in ",
      df_forTitle_figure1$NumStatesReporting,
      " with publicly available unwinding data, as of ",
      format(Sys.Date(), "%B %d, %Y")) |>
  print()

figureTitle_rate <- paste0(
  "There is wide variation in disenrollment rates across reporting states, ranging from ",
  paste0(round(df_forTitle_figure1$Max_rate, 0), "%"),
  " in ", df_forTitle_figure1$Max_state,
  " to ",
  paste0(round(df_forTitle_figure1$Min_rate, 0), "%"),
  " in ", df_forTitle_figure1$Min_state) |>
  print()
DatawRappr::dw_edit_chart(chart_id = dw_fig1_rate, title = figureTitle_rate)


# Write the data to charts
# Chart 1-----
library(DatawRappr)
DatawRappr::dw_edit_chart(chart_id = dw_fig1_number, title = figureTitle_number)
df_cleanedAndCombined |>
  filter(State != "Overall") |>
  left_join(df_states, by = "State") |>
  select(Abbr, Disenrollments) |>
  DatawRappr::dw_data_to_chart(chart_id = dw_fig1_number)


df_cleanedAndCombined |>
  filter(State != "Overall") |>
  filter(!is.na(Rate) & Rate != 0) |>
  mutate(Rate = 100*Rate) |>
  left_join(df_states, by = "State") |>
  select(Abbr, Rate) |>
  DatawRappr::dw_data_to_chart(chart_id = dw_fig1_rate)


# Chart 2----

# Data

# To do:
# add "Georgia", "Kansas", "Maryland", "Rhode Island", "South Dakota"



df_chart2 <- df_unwinding_cms |>
 filter(!State %in% c("Georgia", states_toRemove_proceduralDisenrollmentRates)) |> # Thought about dropping this due to its extreme value, and needing to add context on a short deadline
  filter(`Terminated for procedural reasons` != 0) |>
  bind_rows(
    df_unwinding_cms_allStates |>
      filter(State %in% vec_states_useCMSforProceduralDisenrollments)
  ) |>
  bind_rows(
    df_unwinding_raw |>
      filter(State %in% vec_states_withNonCMSSource) |>
      filter(!State %in% vec_states_useCMSforProceduralDisenrollments) |>
      filter(!State %in% c(states_toRemove_proceduralDisenrollmentRates)) |>
              mutate(`Determined ineligible` = ifelse(State == "Kentucky", `Medicaid Terminations` - `Terminated for procedural reasons`, `Determined ineligible`)) |>
              filter(!(is.na(`Determined ineligible`) | is.na(`Terminated for procedural reasons`))) |>
              select(State, `Determined ineligible`, `Terminated for procedural reasons`) |>
      group_by(State) |>
      summarise(`Determined ineligible` = sum(`Determined ineligible`, na.rm = TRUE),
                `Terminated for procedural reasons` = sum(`Terminated for procedural reasons`, na.rm = TRUE)) |>
              mutate(Rate_proceduralReason = `Terminated for procedural reasons` / (`Terminated for procedural reasons` + `Determined ineligible`))
    ) |>
  arrange(desc(Rate_proceduralReason)) |>
  print()



df_chart2 |>
  left_join(df_states, by = "State") |>
  select(Abbr, `Terminated for procedural reasons`, `Determined ineligible`) |>
  DatawRappr::dw_data_to_chart(dw_fig2)


#
df_proceduralReason <- df_chart2 |>
  select(State, Rate_proceduralReason, `Terminated for procedural reasons`, `Determined ineligible`) %>%
  filter(!State %in% states_toRemove_proceduralDisenrollmentRates) %>%
  add_row(State = "Overall",
          `Terminated for procedural reasons` = sum(.$`Terminated for procedural reasons`),
          `Determined ineligible` = sum(.$`Determined ineligible`),
          Rate_proceduralReason = sum(.$`Terminated for procedural reasons`)/(sum(.$`Terminated for procedural reasons`) + sum(.$`Determined ineligible`))) |>
  dplyr::arrange(desc(State == "Overall"), desc(Rate_proceduralReason))


# df_cleanedAndCombined %>%

# Text
# Medicaid procedural disenrollments range from a high of 89% to 33%, as of June 14, 2023
value_overallProceduralReason <- df_cleanedAndCombined |>
  left_join(df_proceduralReason, by = "State") |>
  filter(State == "Overall") |>
  pull(Rate_proceduralReason)

#Overall, 76% of disenrollments are due to procedural reasons, among states reporting as of June 14, 2023
chart2_title <- paste0(
  "Overall, ",
  paste0(round(100*value_overallProceduralReason, 0), "%"),
  " of disenrollments are due to procedural reasons, among states reporting as of ",
  format(Sys.Date(), "%B %d, %Y")
) |>
  print()
DatawRappr::dw_edit_chart(chart_id = dw_fig2,
                          title = chart2_title)










# ----
# Figure Enrollment Decline----
# Will need to add cms data eventually:
source("./load_monthlyEnrollmentData.R")
# New states
# AR, CO, DC, KS, KY, MA, and NC
df_forEnrollmentDeclineFigure <- df_enrollment %>%
  # Remove some states
  filter(!State %in% c("North Carolina")) |>
  # Take latest data for each state
  group_by(State) |>
    arrange(desc(Date)) |>
    filter(row_number() == 1) |>
  ungroup() |>
  # Remove states with very small declines |>
  filter(percentChange_fromReferenceData < -0.002) |>
  # Remove anything that is later than the reference date
  filter(Date > maxDate) |>
  mutate(PercentChange = percentChange_fromReferenceData * 100) |>
  filter(PercentChange < 0) |>
  # left_join(kffR::df_states, by = c("State")) |>
  print()
df_forEnrollmentDeclineFigure |>
  select(Abbr, Total_flag, Date, Total,  Date_baselineMonth, PercentChange) |>
  DatawRappr::dw_data_to_chart(dw_fig_cumPercentChange)

# Write the figures title:
value_enrollmentDecline <- sum(df_forEnrollmentDeclineFigure$maxTotal - df_forEnrollmentDeclineFigure$Total)
value_enrollmentDecline_roundedDown = value_enrollmentDecline - (value_enrollmentDecline %% 1000)
# Extract which states have the most extreme values:
df_lowestPercentDecline <- df_forEnrollmentDeclineFigure |>
  filter(percentChange_fromReferenceData == max(percentChange_fromReferenceData)) |>
  mutate(percentChange_fromReferenceData = -round(100*percentChange_fromReferenceData, 1)) |>
  select(State, percentChange_fromReferenceData) |>
  print()
df_highestPercentDecline <- df_forEnrollmentDeclineFigure |>
  filter(percentChange_fromReferenceData == min(percentChange_fromReferenceData)) |>
  mutate(percentChange_fromReferenceData = -round(100*percentChange_fromReferenceData)) |>
  select(State, percentChange_fromReferenceData) |>
  print()


numberStates <-nrow(df_forEnrollmentDeclineFigure)

textTitle <- paste0(
  "Net Medicaid enrollment declines range from ",
  df_highestPercentDecline$percentChange_fromReferenceData,
  "% in ",
  df_highestPercentDecline$State,
  " to ",
  df_lowestPercentDecline$percentChange_fromReferenceData,
  "% in ",
  df_lowestPercentDecline$State,
  " among states reporting as of ",
  format(Sys.Date(), "%B %d, %Y")
) |>
  print()

dw_fig_cumPercentChange |>
  DatawRappr::dw_edit_chart(title = textTitle)


# Export cleaned data to the google sheet---
# Write the overview page data to the google sheet:
df_cleanedAndCombined |>
  left_join(df_proceduralReason, by = "State") |>
  left_join(data.frame(State = "Overall",
                       ChildrenDisenrolled = value_childrenDisenrolled_roundedDown,
                       OverallDisenrolled = round(number_disenrollmentsInReportingStates, -3),
                       StatesWithChildrenBreakouts = numberOfStates), by = "State") |>
  googlesheets4::write_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                             sheet = "Overview Page")
df_enrollment |> group_by(State) |> arrange(desc(Date)) |>
  filter(row_number() == 1) |>
  select(State, Date, Total, percentChange_fromReferenceData) |>
  googlesheets4::write_sheet("1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                             sheet = "Enrollment Declines")

