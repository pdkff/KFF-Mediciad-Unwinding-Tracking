
break
# When to show percentage declined
# --(No before April)
# -- No very small declines
# -- Flag PA for enrollment delines form their next month of data. Are they weird? We are not seeing all of the declines we expect from their enrollment data
# Notes:
# Implement button selection logic


library(DatawRappr)
library(googlesheets4)
library(tidyverse)

# States for CMS data?
states_cmsData <- c("Alabama",
                    "Georgia",
                    "Idaho",
                    #"Rhode Island",
                   # "Texas",
                   "Hawaii",
                    "Vermont",
                    "Wyoming")


howManyMonthsInCMSData = 2

df_states <- kffR::df_states
df_indicators <- kffR::shf_listIndicators()


# Parameters ----
date_pheUnwinding <- as.Date("2023-03-01")
date_firstDateForStateLevelMonthlyBars <- as.Date("2023-01-01")

# Load the raw data from Google Sheets (GS)----
# Note: used for figure 2 and three
# Note: includes comparison calculations between the selected latest month, and phe unwinding date
source("./load_monthlyEnrollmentData.R")
source("./load_cmsData.R")

df_cms = load_cmsData(numberOfSheets = howManyMonthsInCMSData)

# Load info about the charts
googlesheets4::sheet_names(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c")
df_dataWrapper_chartInfo <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                                      "DW_stateBars")

df_stateLinks <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                           "State Links") |>
  select(State, `Enrollment data links`)

# Add buttons to switch between the value, and percentage change charts
df_dataWrapper_chartInfo_percentChange <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                                                    "DW_statePercentChange") |>
  filter(!is.na(Id))



df_stateDefinitions <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                                 "Cleaned Data")




df_unwindingPlans <- "State Approaches to the Unwinding Period" |>
  kffR::fetchIndicator() |>
 # filter(Category %in% c("Month in Which State Will Intiate Renewals"))
  filter(Category %in% c("Month of First Anticipated Terminations for Procedural Reasons"))

# If you'd like to update one state-- don't run the for loops, and run this:
singleStateName = "New Mexico"
i = which(df_dataWrapper_chartInfo$State == singleStateName) |> print()

# Which states need percent change charts
states_withPercentChangeChart <- df_enrollment |>
  filter(isAfterMaxDate) |>
  pull(State) |>
  unique() |>
  print()



# Push latest data to line charts
for(i in 1:nrow(df_dataWrapper_chartInfo)){

  # Extract info about this state's datawrapper
  this_state <- df_dataWrapper_chartInfo$State[i] |> print()
  this_id <- df_dataWrapper_chartInfo$Id[i] |> print()


  if(this_state %in% states_cmsData ){

    df_enrollmentThisState <- df_cms |>
      filter(State %in% this_state) |>
      rename(Total = Value)
  } else {
    df_enrollmentThisState <- df_enrollment |>
      filter(State %in% c(this_state))
  }


  # Push updated data to this state's datawrapper figure
  df_toPush <- df_enrollmentThisState |>
    #filter(Date >= date_pheUnwinding) |>
    filter(Date >= date_firstDateForStateLevelMonthlyBars) |>
    select(Date, Total, State) |>
    print()

  # If data are missing, create a dataframe to push to the chart:
  if(! "2023-01-01" %in%  as.character(df_toPush$Date)){
    df_toPush <-  df_toPush |>
      add_row(Date = as.Date("2023-01-01"), Total = 0, State = this_state)
  }

  # If data are missing, create a dataframe to push to the chart:
  if(! "2023-02-01" %in%  as.character(df_toPush$Date)){
    df_toPush <-  df_toPush |>
      add_row(Date = as.Date("2023-02-01"), Total = 0, State = this_state)
  }
  # If data are missing, create a dataframe to push to the chart:
  # if(! "2023-03-01" %in%  as.character(df_toPush$Date)){
  #   df_toPush <-  df_toPush |>
  #     add_row(Date = as.Date("2023-03-01"), Total = 0, State = this_state)
  # }
  # if(! "2023-04-01" %in%  as.character(df_toPush$Date)){
  #   df_toPush <- df_toPush |>
  #     add_row(Date = as.Date("2023-04-01"), Total = 0, State = this_state)
  # }



  df_toPush |>
    DatawRappr::dw_data_to_chart(this_id)

}

i = which(df_dataWrapper_chartInfo$State == "Nebraska") |> print()
# Add source of data/definition to the chart
for(i in 1:nrow(df_dataWrapper_chartInfo)){

  this_state <- df_dataWrapper_chartInfo$State[i] |> print()
  this_state_2 <-  ifelse(this_state == "District of Columbia", "the District of Columbia", this_state)
  this_id <- df_dataWrapper_chartInfo$Id[i]
  this_other_id <- df_dataWrapper_chartInfo_percentChange |>
    filter(State %in% this_state) |>
    pull(Id) |> print()

  this_def <- df_stateDefinitions |>
    dplyr::filter(State %in% this_state) |>
    dplyr::pull(Total_flag) |>
    print()

  if(this_state %in% states_cmsData ){
  print("CMS")
    df_enrollmentThisState <- df_cms |>
      filter(State %in% this_state) |>
      rename(Total = Value)
  } else {
    print("Enrollment")
    df_enrollmentThisState <- df_enrollment |>
      filter(State %in% c(this_state))
  }

  # Create the label for the month of expected disenrollments:
   this_plannedFirstDisenrollment <- df_unwindingPlans |>
     filter(State %in% gsub("the ", "", this_state_2)) |>
     pull(Value_character)

   # Covert the month to a date
   this_FirstDisenrollment_month_planned <- paste0("2023-", stringr::str_sub(paste0("000", which(this_plannedFirstDisenrollment == month.name)), -2))
   this_baseLineDate_planned <- lubridate::floor_date(as.Date(paste0(this_plannedFirstDisenrollment_month, "-01")) - months(1), "month")
   this_baseLineMonth_planned <- lubridate::month(this_baseLineDate,label = TRUE, abbr = FALSE) |> print()

  #this_firstDisenrollment_date <- df_enrollmentThisState$maxDate[1]
   if("maxDate" %in% names(df_enrollmentThisState)){
  this_baseLineDate <-  df_enrollmentThisState$maxDate[1] |> print()
  this_baseLineMonth <- lubridate::month(this_baseLineDate,label = TRUE, abbr = FALSE) |> print()
   } else {
  print("Using CMS Data")
     this_baseLineDate = this_baseLineDate_planned
}

  log_pastNotFuture <- case_when(
    # If there are already declines seen in enrollment data
    "isAfterMaxDate" %in% names(df_enrollmentThisState) & any(df_enrollmentThisState$isAfterMaxDate) ~ TRUE,
    # Otherwise use the state plans
    TRUE ~ Sys.Date() >= this_baseLineDate_planned
  ) |>
    print()
  # What is the date of the reference/baseline date? (one month prior to the first expected disenrollments)


  this_latestDate <- df_enrollmentThisState |>
    filter(State %in% c(this_state)) |>
    pull(Date) |> max(na.rm = TRUE)
  this_latestMonth <- lubridate::month(this_latestDate, label = TRUE, abbr = FALSE)

  value_baseLineEnrollment_thisState <- df_enrollmentThisState |>
    filter(State %in% c(this_state)) |>
    #filter(Date >= date_pheUnwinding) |>
    filter(Date == this_baseLineDate) |>
    pull(Total) |>
    print()
  value_latestMonthEnrollment_thisSTate <- df_enrollmentThisState |>
    filter(State %in% c(this_state)) |>
    #filter(Date >= date_pheUnwinding) |>
    filter(Date == max(Date)) |>
    pull(Total) |>
    print()






  value_enrollmentDecline_thisState <-  ifelse(log_pastNotFuture, value_baseLineEnrollment_thisState - value_latestMonthEnrollment_thisSTate, 0) |> print()
  text_increasedOrDecreased = ifelse(value_enrollmentDecline_thisState < 0, " increased ", " declined ") |> print()
  value_enrollmentDecline_thisState <- abs(value_enrollmentDecline_thisState) |> print()

textIntro <- case_when(


  !is.na(this_baselineDate) & this_baselineDate < this_latestDate & value_enrollmentDecline_thisState > 0 ~paste0("Disenrollments in ", this_state_2, " began in ", this_plannedFirstDisenrollment, ". Enrollment",text_increasedOrDecreased , "by ", format(value_enrollmentDecline_thisState ,big.mark = ","), " from ",this_baseLineMonth , " to ", this_latestMonth, " 2023"),
  this_baseLineDate_planned < this_latestDate ~paste0("Disenrollments in ", this_state_2, " began in ", this_plannedFirstDisenrollment, ". Enrollment",text_increasedOrDecreased , "by ", format(value_enrollmentDecline_thisState ,big.mark = ","), " from ",this_baseLineMonth , " to ", this_latestMonth, " 2023"),
  log_pastNotFuture ~  paste0("Disenrollments in ", this_state_2, " began in ", this_plannedFirstDisenrollment),
  TRUE ~ paste0("Disenrollments in ", this_state_2, " are expected to begin in ", this_plannedFirstDisenrollment, " 2023")
)
print(textIntro)



# Add the plot linkage if needed:
if(this_state %in% states_withPercentChangeChart){
text_forSubtitle <- paste0("<i><i>
    <br>
    <span style='line-height:30px; width: 100%; display: flex; flex-direction: row; flex-wrap: wrap; justify-content: center; align-items: center;'>
    <a target='_self' href='https://datawrapper.dwcdn.net/" ,this_id, "/' style='background:#429ddd; padding:1px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;' rel='nofollow noopener noreferrer'>Total Enrollment</a> &nbsp;
  <a target='_self' href='https://datawrapper.dwcdn.net/", this_other_id, "/' style='background:#cccccc; padding:1px 6px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;' rel='nofollow noopener noreferrer'>Percent Change</a> &nbsp;
  </span></i></i>") |>
  print()

textIntro <- paste0(textIntro, text_forSubtitle) |> print()
}

    # Update text of all the charts:
    DatawRappr::dw_edit_chart(this_id,
                              #title = paste0("Monthly Medicaid Enrollment in ", this_state),
                              title = paste0(this_state, " Medicaid/CHIP Monthly Enrollment"),
                              intro = textIntro,
                              #annotate = paste0(this_state, ' defines their enrollment as "', this_def, '"'),
                              source_name = "KFF Analysis of State Administrative Data")

}


# Add links the source field-----
for(i in 1:nrow(df_dataWrapper_chartInfo)){

  this_state <- df_dataWrapper_chartInfo$State[i] |> print()
  if(!this_state %in% states_cmsData){
  this_id <- df_dataWrapper_chartInfo$Id[i]
  this_link <- df_stateLinks |>
    filter(State %in% this_state) |>
    pull(`Enrollment data links`)
  # Add the link to the source url:
  DatawRappr::dw_edit_chart(this_id, source_url = this_link)
  }

}




# Publish the charts----
for(i in 1:nrow(df_dataWrapper_chartInfo)){

  this_id <- df_dataWrapper_chartInfo$Id[i]

  # Publish all the charts:
  DatawRappr::dw_publish_chart(this_id)

}

