
https://www.medicaid.gov/resources-for-states/downloads/ant-2023-time-init-unwin-reltd-ren-06292023.pdf
df_currentMap <- DatawRappr::dw_data_from_chart(chart_id = "0IxR5")
df_newMap <- googlesheets4::read_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                                       sheet = "Map Data New")

df_forOut <- left_join(
df_currentMap |>
  select(State, Value_character),
df_newMap |>
  mutate(State = gsub('\n', '', State)) |>
  select(State,
         "Effective Date of First Renewals" = "Effective Date\n of First\n Anticipated\n Terminations\n for First\n Cohort of\n Renewals^{1}"),
by = c("State")) |>
  mutate(isDiff = Value_character == `Effective Date of First Renewals`)

df_forOut |>
  googlesheets4::write_sheet(ss = "1IQboIi_eIcX5aPIdZGJk7YyPerBNF0tyhP_hXnDco9c",
                             sheet = "Map Data New")




  df_forOut |>
    select(State,
           Date = "Effective Date of First Renewals") |>
    group_by(Date) |>
      mutate(Date = case_when("District of Columbia" %in% State ~ paste0(Date, " (", n() - 1, " states & DC)"),
                              n() == 1 ~ paste0(Date, " (", n(), " state)"),
                              TRUE ~ paste0(Date, " (", n(), " states)"))) |>
    ungroup() |>
    DatawRappr::dw_data_to_chart("E0gW6")

  E0gW6
