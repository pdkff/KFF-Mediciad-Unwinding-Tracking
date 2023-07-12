library(tidyverse)
library(googlesheets4)



# Check idaho is working properly for the "determined ineligible group"
df_childrenVsAdult_raw <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                              "Data (children)")
df_DW_childrenVsAdult <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                                   "DW Children")

# Code to copy the datawrapper figure for every state
# list_apiResponse <- df_DW_childrenVsAdult$State |> unique() |>
#   lapply(function(thisStateName){
#     dw_copy_chart(copy_from = "jS4e8")
#   })
#   dw_copy_chart()
# vec_dw_ids <- list_apiResponse |>
#   sapply(function(d) d$id)
#
# data.frame(State = df_DW_childrenVsAdult$State, Id = vec_dw_ids) |>
#   googlesheets4::write_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
#                              "DW Children")
# for(i in 1:nrow(df_DW_childrenVsAdult)){
#   DatawRappr::dw_edit_chart(df_DW_childrenVsAdult$Id[i], title = paste0(df_DW_childrenVsAdult$State[i],  " Medicaid Disenrollments by Age"))
# }

# Clean up the raw dataset before getting ready to push to the visuals:
df_childrenVsAdult <- df_childrenVsAdult_raw |>
  filter(!is.na(Date)) |>
  filter(!is.na(Total) | !is.na(Children) | !is.na(Adults)) |>
  mutate(Adults = case_when(
    # Children and Total Present (e.g. Arizona)
    is.na(Adults) & !is.na(Total) & !is.na(Children) ~ Total - Children,
    # Percentages and Total (e.g. Indiana")
    !is.na(Total) & !is.na(`Children (Percent)`) ~ Total - Total*`Children (Percent)`,
    TRUE ~ Adults
  ),

  Children = case_when(
    is.na(Children) & !is.na(Total) & !is.na(Adults) ~ Total - Adults,

    TRUE ~ Children
  ),
  `Adults (19 - 64)` = case_when(
    is.na(`Adults (19 - 64)`) ~ Total * `Adults (19 - 64) (Percent)`,
    TRUE ~ `Adults (19 - 64)`
  ),
  `Elderly (65+)` = case_when(
    is.na(`Elderly (65+)`) ~ Total * `Elderly (65+) (Percent)`,
    TRUE ~ `Elderly (65+)`
  )
  ) |>
  print()





updateState_kidsVsAdults <- function(thisState,
                                     hasElderly = FALSE){

  print(thisState)

  # Pull out the datawrapper id for this state's chart
  dw_id_thisState <- df_DW_childrenVsAdult |>
    filter(State %in% thisState) |>
    pull(Id) |>
    print()

  # Prep the data for datawrapper
  df_thisState <- df_childrenVsAdult |>
    filter(State == thisState) |>
    group_by(State) |>
      summarise(
        Date = max(Date),
        Total = sum(Total),
        Children = sum(Children),
                Adults = sum(Adults),
                `Adults (19 - 64)` = sum(`Adults (19 - 64)`),
                `Elderly (65+)` = sum(`Elderly (65+)`)
                ) |>
      print()


  # Create the dataset to push to datawrapper
  # Note: this is different if a state has elderly breakouts
  if(hasElderly == FALSE){
  df_forPlot <- df_thisState |>
    pivot_longer(cols = c("Children", "Adults"),
                 names_to = "Category",
                 values_to = "Value") |>
    select(Category, Value) |>
    print()
  } else {
    df_forPlot <- df_thisState |>
      pivot_longer(cols = c("Children", "Adults (19 - 64)", "Elderly (65+)"),
                   names_to = "Category",
                   values_to = "Value") |>
      select(Category, Value) |>
      print()
  }


  # Update the labels
  current_month = df_thisState$Date |> head(n = 1) |> lubridate::month(label = TRUE, abbr = FALSE)
  current_year = df_thisState$Date |> head(n = 1) |> lubridate::year()
  value_children <- round(df_thisState$Children, 0)
  value_adults <- round(df_thisState$Adults, 0)
  value_total <- round(df_thisState$Total, 0)


  # eg. In April 2023, 150,454 enrollees renewed their coverage and 39,831 enrollees were disenrolled.
  label_text <- paste0("As of ", current_month, " ", current_year, ", ",
                       format(value_total, big.mark = ","), " people have been disenrolled, including ",
                       format(value_children, big.mark = ","), " children and ",
                       format(value_adults, big.mark = ","), " adults") |>
    print()


  # Push the data to the chart:
  df_forPlot |>
      DatawRappr::dw_data_to_chart(chart_id = dw_id_thisState)

  DatawRappr::dw_edit_chart(chart_id = dw_id_thisState,intro =  label_text)



}



updateState_kidsVsAdults("Arkansas", hasElderly = FALSE)
updateState_kidsVsAdults("Arizona", hasElderly = TRUE)
updateState_kidsVsAdults("Indiana", hasElderly = TRUE)
updateState_kidsVsAdults("Oklahoma", hasElderly = TRUE)
updateState_kidsVsAdults("Virginia", hasElderly = FALSE)
updateState_kidsVsAdults("Washington", hasElderly = TRUE)

