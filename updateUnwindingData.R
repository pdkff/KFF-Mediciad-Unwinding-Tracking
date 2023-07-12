library(tidyverse)

publish = FALSE

# These states have CMS report data from Foia'd data--
c("Arizona", # ?
  "New Mexico",
 # "New Hampshire",
  "Oklahoma")


df_unwinding <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                                           "Data (unwinding)")
df_unwinding_cms <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                              "CMS Monthly data")
df_DW_unwinding <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                             "DW State Unwinding")


updateState <- function(thisState,
                        removeInProcess = FALSE,
                        latestOnly = FALSE){

  dw_id_thisState <- df_DW_unwinding |>
    filter(State %in% thisState) |>
    pull(Id) |>
    print()

  if(latestOnly){
    df_forPlot <- df_unwinding |>
      filter(State %in% c(thisState)) |>
      filter(!is.na(`Renewals Approved`)) |>
      #arrange(desc(Date)) |>
      filter(Date %in% max(Date)) |>
      select(Date,
             `Renewed` = `Renewals Approved`,
             `Disenrolled` = `Renewals Discontinued`,
             `In process` = `Renewals in Process`) |>
      print()
  } else {
    df_forPlot <- df_unwinding |>
      filter(State %in% c(thisState)) |>
      filter(!is.na(`Renewals Approved`)) |>
      arrange(desc(Date)) |>
      # filter(Date %in% max(Date)) |>
      select(Date,
             `Renewed` = `Renewals Approved`,
             `Disenrolled` = `Renewals Discontinued`,
             `In process` = `Renewals in Process`) |>
      print()
  }
  # |>
    # group_by(State, Date) |>
    # pivot_longer(names_to = "category",
    #              values_to = "Value",
    #              cols = c("Renewed", "Disenrolled", "In process")) |>
    # ungroup() |>
    # filter(!is.na(Value)) |>
    # select(category, Date, Value )

  # Update the labels
  current_month = df_forPlot$Date |> head(n = 1) |> lubridate::month(label = TRUE, abbr = FALSE)
  current_year = df_forPlot$Date |> head(n = 1) |> lubridate::year()
  value_renewed <- sum(df_forPlot$Renewed, na.rm = TRUE)
  value_disenrolled <- sum(df_forPlot$Disenrolled, na.rm = TRUE)

  # eg. In April 2023, 150,454 enrollees renewed their coverage and 39,831 enrollees were disenrolled.
  label_text <- paste0("As of ", current_month, " ", current_year, ", ",
                       format(value_renewed, big.mark = ","), " enrollees renewed their coverage and ",
                       format(value_disenrolled, big.mark = ","), " enrollees were disenrolled.")
  DatawRappr::dw_edit_chart(chart_id = dw_id_thisState, intro = label_text)

  # Add the data label and push the data to the chart
  if(removeInProcess){
    df_forPlot <- df_forPlot |>
      select(-`In process`)
  }

  df_forPlot |>
    DatawRappr::dw_data_to_chart(dw_id_thisState)

  if(publish)  DatawRappr::dw_publish_chart(dw_id_thisState)
}

updateState_protectedPopulation <- function(thisState,
                                            latestOnly = FALSE){

  dw_id_thisState <- df_DW_unwinding |>
    filter(State %in% thisState) |>
    pull(Id) |>
    print()

  # Update the data
  if(thisState == "Pennsylvania"){
    df_forPlot <- df_unwinding |>
      filter(State %in% c(thisState)) |>
      filter(!is.na(`Renewals Approved`)) |>
      select(State, Date,
             Renewed = `Renewals Approved`,
             `Disenrolled (Based on enrollee response)` = `Closed Renewals (Determined based on information individual provided)`,
             `Disenrolled (No response)` = `Closed Renewals (No response)`) |>
      group_by(State) |>
      summarise(Date = max(Date),
                Renewed = sum(Renewed),
                `Determined ineligible`  = sum(`Disenrolled (Based on enrollee response)`),
                `Terminated for procedural reasons` = sum(`Disenrolled (No response)`)) |>
      ungroup() |>
      select(-State)


    value_renewed <- sum(df_forPlot$Renewed, na.rm = TRUE)
    value_disenrolled <- sum(df_forPlot$`Determined ineligible`, na.rm = TRUE) + sum(df_forPlot$`Terminated for procedural reasons`, na.rm = TRUE)

    # Update the labels
    current_month = df_forPlot$Date |> head(n = 1) |> lubridate::month(label = TRUE, abbr = FALSE)
    current_year = df_forPlot$Date |> head(n = 1) |> lubridate::year()

    # eg. In April 2023, 150,454 enrollees renewed their coverage and 39,831 enrollees were disenrolled.
    label_text <- paste0("As of ", current_month, " ", current_year, ", ",
                         format(value_renewed, big.mark = ","), " enrollees renewed their coverage and ",
                         format(value_disenrolled, big.mark = ","), " enrollees were disenrolled")

  } else if(thisState == "Idaho") {

    df_forPlot <- df_unwinding |>
      filter(State %in% c(thisState)) |>
      filter(!is.na(`Determined eligible`)) |>
      arrange(desc(Date)) |>
     # filter(Date == max(Date)) |>
      select(Date,
             `Renewed (Cumulative)`  = `Determined eligible`,
             `Disenrolled (Cumulative)` = `Determined ineligible`)

    # Update the labels

    value_renewed <- head(df_forPlot$`Renewed (Cumulative)`, n = 1)
    value_disenrolled <- head(df_forPlot$`Disenrolled (Cumulative)`, n = 1)

    # Update the labels
    current_day = df_forPlot$Date |> head(n = 1) |> lubridate::day()
    current_month = df_forPlot$Date |> head(n = 1) |> lubridate::month(label = TRUE, abbr = FALSE)
    current_year = df_forPlot$Date |> head(n = 1) |> lubridate::year()

    # eg. In April 2023, 150,454 enrollees renewed their coverage and 39,831 enrollees were disenrolled.
    label_text <- paste0("As of ", current_month, " ", current_day, " ",  current_year, ", ",
                         format(value_renewed, big.mark = ","), " enrollees* renewed their coverage and ",
                         format(value_disenrolled, big.mark = ","), " enrollees* were disenrolled")
  }





  DatawRappr::dw_edit_chart(chart_id = dw_id_thisState, intro = label_text)

# Push data to the chart
  df_forPlot |>

    DatawRappr::dw_data_to_chart(dw_id_thisState)

  DatawRappr::dw_publish_chart(dw_id_thisState)
}

updateState_cms <- function(thisState,
                        removeInProcess = FALSE){

  print(thisState)
  dw_id_thisState <- df_DW_unwinding |>
    filter(State %in% thisState) |>
    pull(Id) |>
    print()

  # Update the data
  df_forPlot <- df_unwinding_cms |>
    filter(State %in% c(thisState)) |>
    filter(!`Renewed ex parte (5a1)` == 0 & !is.na(`Total Due (5)`)) |>
    arrange(desc(Date)) |>
    select(Date,
           `Renewed on an ex parte basis` = `Renewed ex parte (5a1)`,
           `Renewed via renewal form` = `Renewed renewal form (5a2)`,
           `Determined ineligible` = `Ineligible transfered to marketplace (5b)`,
           `Terminated for procedural reasons` = `Terminated procedural reasons (5c)`,
           `Renewal not completed` = `Renewal not completed (5d)`#,
          # `In process` = `Pending Applications`
           ) |>
    print()


  # group_by(State, Date) |>
  # pivot_longer(names_to = "category",
  #              values_to = "Value",
  #              cols = c("Renewed", "Disenrolled", "In process")) |>
  # ungroup() |>
  # filter(!is.na(Value)) |>
  # select(category, Date, Value )

  # Update the labels
  current_month = df_forPlot$Date |> head(n = 1) |> lubridate::month(label = TRUE, abbr = FALSE)
  current_year = df_forPlot$Date |> head(n = 1) |> lubridate::year()
  value_renewed <- sum(df_forPlot$`Renewed on an ex parte basis`, na.rm = TRUE) + sum(df_forPlot$`Renewed via renewal form`, na.rm = TRUE)
  value_disenrolled <- sum(df_forPlot$`Terminated for procedural reasons`, na.rm = TRUE) + sum(df_forPlot$`Determined ineligible`, na.rm = TRUE)
# note on sum: some states only have latest month filtered out above
  value_disenrolled_proceduralReasons <- sum(df_forPlot$`Terminated for procedural reasons`, na.rm = TRUE)
  value_disenrolled_ineligible <- sum(df_forPlot$`Determined ineligible`, na.rm = TRUE)

  # eg. In April 2023, 150,454 enrollees renewed their coverage and 39,831 enrollees were disenrolled.
  label_text <- paste0("As of ", current_month, " ", current_year, ", ",
                       format(value_renewed, big.mark = ","), " enrollees renewed their coverage and ",
                       format(value_disenrolled, big.mark = ","), " enrollees were disenrolled, including ",
                       format(value_disenrolled_ineligible, big.mark = ","), " who were determined ineligible and ",
                       format(value_disenrolled_proceduralReasons, big.mark = ","), " who were disenrolled for procedural reasons") |>
    print()
  DatawRappr::dw_edit_chart(chart_id = dw_id_thisState, intro = label_text)

  df_forPlot |>
    DatawRappr::dw_data_to_chart(dw_id_thisState)

  if(publish)  DatawRappr::dw_publish_chart(dw_id_thisState)

}

updateState_specialCases <- function(thisState,
                                     latestOnly = FALSE){

  dw_id_thisState <- df_DW_unwinding |>
    filter(State %in% thisState) |>
    pull(Id) |>
    print()

  if(thisState %in%  c("Alaska", "Florida", "Montana")){
# Renewals (general), and Unwinding (by type), and Pending
    df_forPlot <- df_unwinding |>
      filter(State %in% c(thisState)) |>
      filter(!is.na(`Determined ineligible`)) |>
      arrange(desc(Date)) |>
      # filter(Date %in% max(Date)) |>
      select(Date,
             `Renewals approved` = `Renewals Approved`,
             `Determined ineligible`,
             `Terminated for procedural reasons`,
             `Renewals in process` =  `Renewals in Process`) |>
      print()

    # Some of these states don't have in process renewals, remove the column if it is empty
    if(all(is.na(df_forPlot$`Renewals in process`))) df_forPlot <- df_forPlot |> select(-`Renewals in process`)

    current_month = df_forPlot$Date |> head(n = 1) |> lubridate::month(label = TRUE, abbr = FALSE)
    current_year = df_forPlot$Date |> head(n = 1) |> lubridate::year()


    value_renewed <-sum(df_forPlot$`Renewals approved`, na.rm = TRUE)
    value_proceduralReason <- sum(df_forPlot$`Terminated for procedural reasons`, na.rm = TRUE)
    value_determinedIneligible <- sum(df_forPlot$`Determined ineligible`, na.rm = TRUE)

    value_disenrolled <- value_proceduralReason + value_determinedIneligible

    # eg. In April 2023, 150,454 enrollees renewed their coverage and 39,831 enrollees were disenrolled.
    label_text <- paste0("As of ", current_month, " ", current_year, ", ",
                         format(value_renewed, big.mark = ","), " enrollees renewed their coverage and ",
                         format(value_disenrolled, big.mark = ","), " enrollees were disenrolled, including ",
                         format(value_determinedIneligible, big.mark = ","), " who were determined ineligible and ",
                         format(value_proceduralReason, big.mark = ","), " who were terminated for procedural reasons") |>
      print()

    } else if(thisState %in%  c( "District of Columbia",
                                 "Maryland",
                                 "Michigan",
                                 "South Carolina")){
# Full detail
      df_forPlot <- df_unwinding |>
        filter(State %in% c(thisState)) |>
        filter(!is.na(`Determined ineligible`) | !is.na(`Renewed via renewal form`)) |>
        arrange(desc(Date)) |>
        # filter(Date %in% max(Date)) |>
        select(Date,
               `Renewed on an ex parte basis` = `Renewed on an ex parte basis`,
               `Renewed via renewal form`,
               `Terminated for procedural reasons`,
               `Determined ineligible`,
               `Renewals in process` =  `Renewals in Process`) |>
        print()

      if(latestOnly){
        df_forPlot <- df_forPlot |>
          filter(Date %in% max(Date))
      }

      current_month = df_forPlot$Date |> head(n = 1) |> lubridate::month(label = TRUE, abbr = FALSE)
      current_year = df_forPlot$Date |> head(n = 1) |> lubridate::year()


      value_renewed <-sum(df_forPlot$`Renewed on an ex parte basis`, na.rm = TRUE) + sum(df_forPlot$`Renewed via renewal form`, na.rm = TRUE)
      value_proceduralReason <- sum(df_forPlot$`Terminated for procedural reasons`, na.rm = TRUE)
      value_determinedIneligible <- sum(df_forPlot$`Determined ineligible`, na.rm = TRUE)

      value_disenrolled <- value_proceduralReason + value_determinedIneligible

      # eg. In April 2023, 150,454 enrollees renewed their coverage and 39,831 enrollees were disenrolled.
      label_text <- paste0("As of ", current_month, " ", current_year, ", ",
                           format(value_renewed, big.mark = ","), " enrollees renewed their coverage and ",
                           format(value_disenrolled, big.mark = ","), " enrollees were disenrolled, including ",
                           format(value_determinedIneligible, big.mark = ","), " who were determined ineligible and ",
                           format(value_proceduralReason, big.mark = ","), " who were terminated for procedural reasons") |>
        print()

    } else  if(thisState %in% c("Oklahoma", "Washington")) {
      df_forPlot <- df_unwinding |>
        filter(State %in% c(thisState)) |>
        filter(!is.na(`Determined ineligible`)) |>
        arrange(desc(Date)) |>
        # filter(Date %in% max(Date)) |>
        select(Date,
               `Terminated for procedural reasons`,
               `Determined ineligible`) |>
        print()

      current_month = df_forPlot$Date |> head(n = 1) |> lubridate::month(label = TRUE, abbr = FALSE)
      current_year = df_forPlot$Date |> head(n = 1) |> lubridate::year()


      value_proceduralReason <- sum(df_forPlot$`Terminated for procedural reasons`, na.rm = TRUE)
      value_determinedIneligible <- sum(df_forPlot$`Determined ineligible`, na.rm = TRUE)

      value_disenrolled <- value_proceduralReason + value_determinedIneligible

      # eg. In April 2023, 150,454 enrollees renewed their coverage and 39,831 enrollees were disenrolled.
      label_text <- paste0("As of ", current_month, " ", current_year, ", ",
                           format(value_disenrolled, big.mark = ","), " enrollees were disenrolled, including ",
                           format(value_determinedIneligible, big.mark = ","), " who were determined ineligible and ",
                           format(value_proceduralReason, big.mark = ","), " who were terminated for procedural reasons") |>
        print()

      } else if(thisState == "South Dakota"){
    df_forPlot <- df_unwinding |>
      filter(State %in% c(thisState)) |>
      filter(!is.na(`Disenrolled (determined ineligle)`)) |>
      arrange(desc(Date)) |>
      # filter(Date %in% max(Date)) |>
      select(Date,
             `Determined ineligible` = `Disenrolled (determined ineligle)`,
             `Terminated for procedural reasons` = `Disenrolled for lack of response`,
             `Disenrolled due to a change in circumstances outside of renewal` = `Disenrolled due to a change in circumstances outside of renewal`,

      ) |>
      print()

    current_month = df_forPlot$Date |> head(n = 1) |> lubridate::month(label = TRUE, abbr = FALSE)
    current_year = df_forPlot$Date |> head(n = 1) |> lubridate::year()

    value_disenrolled <- (sum(df_forPlot$`Determined ineligible`, na.rm = TRUE) +
      sum(df_forPlot$`Terminated for procedural reasons`, na.rm = TRUE) +
      sum(df_forPlot$`Disenrolled due to a change in circumstances outside of renewal`, na.rm = TRUE))

    value_proceduralReason <- sum(df_forPlot$`Terminated for procedural reasons`, na.rm = TRUE)
    value_determinedIneligible <- sum(df_forPlot$`Determined ineligible`, na.rm = TRUE)
    value_otherReason <- sum(df_forPlot$`Disenrolled due to a change in circumstances outside of renewal`, na.rm = TRUE)

    # eg. In April 2023, 150,454 enrollees renewed their coverage and 39,831 enrollees were disenrolled.
    label_text <- paste0("As of ", current_month, " ", current_year, ", ",
                         format(value_disenrolled, big.mark = ","), " enrollees were disenrolled, including ",
                         format(value_determinedIneligible, big.mark = ","), " who were determined ineligible, ",
                         format(value_proceduralReason, big.mark = ","), " who were terminated for procedural reasons, and ",
                         format(value_otherReason, big.mark = ","), " people who were disenrolled outside of renewal due to a change in circumstances")

  } else if(thisState == "Idaho"){
      df_forPlot <- df_unwinding |>
        filter(State %in% c(thisState)) |>
        filter(Date == max(Date, na.rm = TRUE)) |>
        select(State, Date,`Determined ineligible`, `Determined eligible`) |>
        print()

      current_day = df_forPlot$Date |> head(n = 1) |> lubridate::day()
      current_month = df_forPlot$Date |> head(n = 1) |> lubridate::month(label = TRUE, abbr = FALSE)
      current_year = df_forPlot$Date |> head(n = 1) |> lubridate::year()

      value_disenrolled <- (sum(df_forPlot$`Determined ineligible`, na.rm = TRUE) )



      # eg. In April 2023, 150,454 enrollees renewed their coverage and 39,831 enrollees were disenrolled.
      label_text <- paste0("As of ", current_month, " ", current_day, " ", current_year, ", ",
                           format(value_disenrolled, big.mark = ","), " enrollees were disenrolled") |>
        print()

      df_forPlot <- df_forPlot |> select(`Determined ineligible`, `Determined eligible`) |>
        pivot_longer(cols = c("Determined ineligible", "Determined eligible"))
      names(df_forPlot) <- c("name", paste0(current_month, " ", current_day, ", " , current_year))

    } else if(thisState %in% c("Rhode Island")) {
      # Full detail
      df_forPlot <- df_unwinding |>
        filter(State %in% c(thisState)) |>
        filter(!is.na(`Determined ineligible`)) |>
        arrange(desc(Date)) |>
        # filter(Date %in% max(Date)) |>
        select(Date,
               `Renewed on an ex parte basis` =  `Renewed via exparte`,
               `Renewed via renewal form` = `Renewed via packet`,
               `Terminated for procedural reasons`,
               `Determined ineligible`,
               `Renewals in process` =  `Renewals in Process`,
               `Exclusions`) |>
        print()

      current_month = df_forPlot$Date |> head(n = 1) |> lubridate::month(label = TRUE, abbr = FALSE)
      current_year = df_forPlot$Date |> head(n = 1) |> lubridate::year()


      value_renewed <-sum(df_forPlot$`Renewed on an ex parte basis`, na.rm = TRUE) + sum(df_forPlot$`Renewed via renewal form`, na.rm = TRUE)
      value_proceduralReason <- sum(df_forPlot$`Terminated for procedural reasons`, na.rm = TRUE)
      value_determinedIneligible <- sum(df_forPlot$`Determined ineligible`, na.rm = TRUE)
      value_exclusions <- sum(df_forPlot$Exclusions, na.rm = TRUE)

      value_disenrolled <- value_proceduralReason + value_determinedIneligible #+ value_exclusions

      # eg. In April 2023, 150,454 enrollees renewed their coverage and 39,831 enrollees were disenrolled.
      label_text <- paste0("As of ", current_month, " ", current_year, ", ",
                           format(value_renewed, big.mark = ","), " enrollees renewed their coverage and ",
                           format(value_disenrolled, big.mark = ","), " enrollees were disenrolled, including ",
                           format(value_determinedIneligible, big.mark = ","), " who were determined ineligible and ",
                           format(value_proceduralReason, big.mark = ","), " who were terminated for procedural reasons") |>
        print()
    } else {
    break
  }


  # Update the labels
  DatawRappr::dw_edit_chart(chart_id = dw_id_thisState, intro = label_text)


  df_forPlot |>
    DatawRappr::dw_data_to_chart(dw_id_thisState)

  if(publish)  DatawRappr::dw_publish_chart(dw_id_thisState)
}


# Update the state dashboard figures ----
updateState(thisState = "Arizona",
            removeInProcess = FALSE,
            latestOnly = FALSE)
#updateState(thisState = "Montana", latestOnly = FALSE, removeInProcess = FALSE)
updateState(thisState = "Nebraska", latestOnly = TRUE, removeInProcess = TRUE)
updateState(thisState = "Virginia",  removeInProcess = TRUE,  latestOnly = TRUE)


updateState_specialCases(thisState = "Alaska")
updateState_specialCases(thisState = "District of Columbia")
updateState_specialCases(thisState = "Idaho")
updateState_specialCases(thisState = "Maryland")
updateState_specialCases(thisState = "Michigan")
updateState_specialCases(thisState = "Montana")
updateState_specialCases(thisState = "Oklahoma")
updateState_specialCases(thisState = "Rhode Island")
updateState_specialCases(thisState = "South Carolina", latestOnly = TRUE)
updateState_specialCases(thisState = "South Dakota")
updateState_specialCases(thisState = "Washington")

# Update the CMS report figures:
updateState_cms(thisState = "Indiana")
updateState_cms(thisState = "Iowa")
updateState_cms(thisState = "Florida")
updateState_cms(thisState = "Mississippi")
updateState_cms(thisState = "Nevada")
updateState_cms(thisState = "Pennsylvania")
# updateState_cms(thisState = "South Carolina")
# Note: for SC we are now using their dashboard which is updated more often the their cms report
#
# updateState_cms(thisState = "New Hampshire")
updateState_cms(thisState = "West Virginia")
updateState_cms(thisState = "Vermont")



# Protected enrollment:
#updateState_protectedPopulation(thisState = "Pennsylvania")
# updateState_protectedPopulation(thisSTate = "Idaho")
