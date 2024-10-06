library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)

national_presidential_polling <- read_csv('model/polling/national_presidential.csv')

avg_national_presidential_polling <- national_presidential_polling |>
    mutate(
        start_date = as.Date(start_date, format="%m/%d/%y"),
        end_date = as.Date(end_date, format="%m/%d/%y")
    ) |>
    filter(
        internal != "TRUE",
        !partisan %in% c("DEM", "REP"),
        state %in% c("Pennsylvania", "Michigan", "Wisconsin", "North Carolina", "Georgia", "Arizona", "Nevada"),
        candidate_name %in% c("Kamala Harris", "Donald Trump"),
        start_date >= '2024-07-21'
    ) |>
    summarize(
        a_pct = round(weighted.mean(pct, as.numeric(sample_size), na.rm = TRUE),1),
        .by = c(start_date,state,candidate_name)
    ) |>
    arrange(start_date)

swing_state_summary <- avg_national_presidential_polling |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1))

pennsylvania_summary <- avg_national_presidential_polling |> 
    filter(state == "Pennsylvania") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

michigan_summary <- avg_national_presidential_polling |> 
    filter(state == "Michigan") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

wisconsin_summary <- avg_national_presidential_polling |> 
    filter(state == "Wisconsin") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

georgia_summary <- avg_national_presidential_polling |> 
    filter(state == "Georgia") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

north_carolina_summary <- avg_national_presidential_polling |> 
    filter(state == "North Carolina") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

nevada_summary <- avg_national_presidential_polling |> 
    filter(state == "Nevada") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

arizona_summary <- avg_national_presidential_polling |> 
    filter(state == "Arizona") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

print(pennsylvania_summary)
print(michigan_summary)
print(wisconsin_summary)
print(georgia_summary)
print(north_carolina_summary)
print(nevada_summary)
print(arizona_summary)