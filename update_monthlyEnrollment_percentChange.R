
library(DatawRappr)
library(googlesheets4)
library(tidyverse)

# Load info about the charts
df_dataWrapper_chartInfo <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                                      "DW_statePercentChange") |>
  filter(!is.na(Id))
df_dataWrapper_chartInfo_value <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                                                                        "DW_stateBars")

source("./load_monthlyEnrollmentData.R")


df_stateDefinitions <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                                 "Cleaned Data")

df_stateLinks <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                           "State Links") |>
  select(State, `Enrollment data links`)


# Which states should we update?

vec_states_withDeclines <- df_enrollment |>
  filter(isAfterMaxDate) |>
  pull(State) |>
  unique() |>
  print()

#Massachusetts Utah


vec_states_withDataPostStart <- df_enrollment |>
  filter(Date > Date_baselineMonth) |>
  pull(State) |> unique() |> print()

thisState = "New Mexico"

# Push latest data to line charts
for(thisState in vec_states_withDeclines){

  i = which(df_dataWrapper_chartInfo$State == thisState) |> print()

  # Extract info about this state's datawrapper
  this_state <- df_dataWrapper_chartInfo$State[i] |> print()
  this_id <- df_dataWrapper_chartInfo$Id[i] |> print()


  # Push updated data to this state's datawrapper figure
  df_toPush <- df_enrollment |>
    filter(State %in% c(this_state)) |>
    #select(Date, Total, Total_flag) |>
    #filter(Date >= date_pheUnwinding) |>
    filter(Date > maxDate) |>
    mutate(Percent = percentChange_fromReferenceData * 100) |>
    #filter(Date >= Date_baselineMonth) |>
    select(Date, Percent, State, Total_flag, maxDate) |>
    print()

  # # If data are missing, create a dataframe to push to the chart:
  # if(! "2023-03-01" %in%  as.character(df_toPush$Date)){
  #   df_toPush <-  df_toPush |>
  #     add_row(Date = as.Date("2023-03-01"), Percent = 0, State = this_state)
  # }
  # if(! "2023-04-01" %in%  as.character(df_toPush$Date)){
  #   df_toPush <- df_toPush |>
  #     add_row(Date = as.Date("2023-04-01"), Percent = 0, State = this_state)
  # }

  this_baselineDate <- df_enrollment |>
    filter(State %in% c(this_state))|> pull(maxDate) |> unique()
  if(length(this_baselineDate) != 1) break
  this_baseline_month <- this_baselineDate |> lubridate::month(label = TRUE, abbr = FALSE)
  this_baseline_year   <-this_baselineDate |> lubridate::year()

  # Add text to the figures:
  text_forTitle <- paste0(this_state, " Cumulative Percent Change In Medicaid Monthly Enrollment From ", this_baseline_month, " ", this_baseline_year)
  text_forTitle |> print()


  # Set up the chart linkages:
  dw_id_chart1 <- df_dataWrapper_chartInfo_value |>
    filter(State %in% this_state) |>
    pull(Id)
  dw_id_chart2 <- this_id


  text_forSubtitle <- paste0("<i><i>
    <br>
    <span style='line-height:30px; width: 100%; display: flex; flex-direction: row; flex-wrap: wrap; justify-content: center; align-items: center;'>
    <a target='_self' href='https://datawrapper.dwcdn.net/" ,dw_id_chart1, "/' style='background:#cccccc; padding:1px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;' rel='nofollow noopener noreferrer'>Total Enrollment</a> &nbsp;
  <a target='_self' href='https://datawrapper.dwcdn.net/", dw_id_chart2, "/' style='background:#429ddd; padding:1px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;' rel='nofollow noopener noreferrer'>Percent Change</a> &nbsp;
  </span></i></i>") |>
    print()

  DatawRappr::dw_edit_chart(this_id,
                            title = text_forTitle,
                            intro = text_forSubtitle)

  df_toPush |>
    select(Date, Percent) |>
    DatawRappr::dw_data_to_chart(this_id)

}






# Add links the source field-----
for(i in 1:nrow(df_dataWrapper_chartInfo)){

  this_state <- df_dataWrapper_chartInfo$State[i] |> print()
  this_id <- df_dataWrapper_chartInfo$Id[i]
  this_link <- df_stateLinks |>
    filter(State %in% this_state) |>
    pull(`Enrollment data links`)
  # Add the link to the source url:
  DatawRappr::dw_edit_chart(this_id, source_url = this_link)

}

# # Add source of data/definition to the chart
# for(i in 1:nrow(df_dataWrapper_chartInfo)){
#
#   this_state <- df_dataWrapper_chartInfo$State[i]
#   this_id <- df_dataWrapper_chartInfo$Id[i]
#
#   this_def <- df_stateDefinitions |>
#     dplyr::filter(State %in% this_state) |>
#     dplyr::pull(Total_flag)
#
#   this_enrollmentChangeSinceUnwinding <-
#     # Publish all the charts:
#     DatawRappr::dw_edit_chart(this_id,
#                               title = paste0("Monthly Medicaid Enrollment in ", this_state),
#                               intro = paste0("Monthly enrollment has decreased {xxx}% since the unwinding period began."),
#                               annotate = paste0(this_state, ' defines their enrollment as "', this_def, '"'),
#                               source_name = "State Websites (available upon request)")
#
# }


# Publish the charts -----
for(i in 1:nrow(df_dataWrapper_chartInfo)){

  this_id <- df_dataWrapper_chartInfo$Id[i]

  # Publish all the charts:
  DatawRappr::dw_publish_chart(this_id)

}


