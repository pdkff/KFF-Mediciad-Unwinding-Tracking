library(tidyverse)
library(DatawRappr)

# Fig kids vs adults----
# Check idaho is working properly for the "determined ineligible group"
df_childrenVsAdult_raw <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                                    "Data (children)")

# Clean up the raw dataset before getting ready to push to the visuals:
df_childrenVsAdult <- df_childrenVsAdult_raw |>
  #filter(State != "Oklahoma") |>
  filter(!is.na(Date)) |>
  filter(!is.na(Total) | !is.na(Children) | !is.na(Adults)) |>
  mutate(Adults = case_when(
    # Children and Total Present (e.g. Arizona)
    is.na(Adults) & !is.na(Total) & !is.na(Children) ~ Total - Children,
    # Percentages and Total (e.g. Indiana")
    !is.na(Total) & !is.na(`Children (Percent)`) ~ round(Total - Total*`Children (Percent)`, 0),
    TRUE ~ Adults
  ),

  Children = case_when(
    is.na(Children) & !is.na(Total) & !is.na(Adults) ~ Total - Adults,

    TRUE ~ Children
  ),
  `Adults (19 - 64)` = case_when(
    is.na(`Adults (19 - 64)`) ~ round(Total * `Adults (19 - 64) (Percent)`, 0),
    TRUE ~ `Adults (19 - 64)`
  ),
  `Elderly (65+)` = case_when(
    is.na(`Elderly (65+)`) ~ round(Total * `Elderly (65+) (Percent)`, 0),
    TRUE ~ `Elderly (65+)`
  )
  ) |>
  group_by(State) |>
  summarise(
    Date = max(Date),
    Total = sum(Total),
    Children = sum(Children),
    Adults = sum(Adults),
    `Adults (19 - 64)` = sum(`Adults (19 - 64)`),
    `Elderly (65+)` = sum(`Elderly (65+)`)
  ) |>
  ungroup() |>
  # Remove any states that don't really have data
  filter(!is.na(Children)) |>
  print()
# Write the clean dataset to the DW figure for Adults vs Children
df_childrenVsAdult |>
  arrange(desc(Children/Adults)) |>
  # Each state needs to either have "Adults", or the age breakouts. Remove extra Adult info where needed
  #  mutate(Adults = ifelse(is.na(`Adults (19 - 64)`), Adults, NA)) |>
  select(State, Children, Adults) |>
  # select(-Date) |>
  DatawRappr::dw_data_to_chart(dw_fig_childrenVsAdults)


# Create and write the title to the datawrapper figure
value_childrenDisenrolled <- round(df_childrenVsAdult$Children |> sum(), 0) |> print()
value_childrenDisenrolled_roundedDown = value_childrenDisenrolled - (value_childrenDisenrolled %% 1000)
value_adultDisenrolled <- round(df_childrenVsAdult$Adults |> sum(), 0) |> print()
numberOfStates <- nrow(df_childrenVsAdult) |> print()
titleDate_day   = max(df_childrenVsAdult$Date, na.rm = TRUE) |> lubridate::day()
titleDate_month = max(df_childrenVsAdult$Date, na.rm = TRUE) |> lubridate::month(label = TRUE, abbr = FALSE)
titleDate_year  = max(df_childrenVsAdult$Date, na.rm = TRUE) |> lubridate::year()

# number_renewalsInReportingStates <- df_cleanedAndCombined |>
#   filter(State %in% c(df_childrenVsAdult$State)) |>
#   pull(Renewals) |>
#   sum() |>
#   print()
number_disenrollmentsInReportingStates <- df_childrenVsAdult |>
  pull(Total) |>
  sum() |>
  print()

df_ratio_forText <- write_ratioText(value_childrenDisenrolled/(number_disenrollmentsInReportingStates))
digit_to_text <- function(digit) {
  digits <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  return(digits[digit + 1])
}
ordinal_number_to_text <- function(number) {
  digits <- c("zeroth", "first", "half", "third", "fourths", "fifths", "sixths", "sevenths", "eighths", "ninths")
  teens <- c("tenths", "elevenths", "twelfths", "thirteenths", "fourteenths", "fifteenths", "sixteenths", "seventeenths", "eighteenths", "nineteenths")
  tens <- c("", "tenths", "twentieth", "thirtieth", "fortieth", "fiftieth", "sixtieth", "seventieth", "eightieth", "ninetieth")

  if (number < 0 || number > 999) {
    stop("Invalid number. Please enter a positive integer between 0 and 999.")
  }

  if (number == 0) {
    return("zeroth")
  }

  text <- ""

  hundreds <- floor(number / 100)
  tens_units <- number %% 100

  if (hundreds > 0) {
    text <- paste0(digits[hundreds + 1], " hundred")
  }

  if (tens_units > 0) {
    if (hundreds > 0) {
      text <- paste0(text, " and ")
    }

    if (tens_units < 10) {
      text <- paste0(text, digits[tens_units + 1])
    } else if (tens_units < 20) {
      text <- paste0(text, teens[tens_units - 10 + 1])
    } else {
      tens_digit <- floor(tens_units / 10)
      units_digit <- tens_units %% 10

      if (tens_digit > 0) {
        text <- paste0(text, tens[tens_digit + 1])
      }

      if (units_digit > 0) {
        if (tens_digit > 0) {
          text <- paste0(text, "-")
        }

        text <- paste0(text, digits[units_digit + 1])
      }
    }
  }

  return(text)
}


titleText <- paste0("Children account for roughly ",
                    digit_to_text(df_ratio_forText$numerator),
                    "-",
                    ordinal_number_to_text(df_ratio_forText$denominator),
                    " (",
                    round(100*value_childrenDisenrolled/(number_disenrollmentsInReportingStates), 0),
                    "%) of Medicaid disenrollments in the ",
                    numberOfStates,
                    " states reporting age breakouts, as of ",
                    format(Sys.Date(), "%B %d, %Y")) |>
  print()

DatawRappr::dw_edit_chart(chart_id = dw_fig_childrenVsAdults,
                          title = titleText)
