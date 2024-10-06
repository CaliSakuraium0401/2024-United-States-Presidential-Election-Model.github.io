library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)

national_presidential_polling <- read_csv('model/polling/national_presidential.csv')

xnational_presidential_polling <- national_presidential_polling |>
    mutate(
        sample_size = as.numeric(sample_size),
        pollscore = as.numeric(pollscore),
        start_date = as.Date(start_date, format="%m/%d/%y"),
        end_date = as.Date(end_date, format="%m/%d/%y")
    ) |>
    filter(
        !is.na(sample_size),
        internal != "TRUE",
        !partisan %in% c("DEM", "REP"),
        candidate_name %in% c("Kamala Harris", "Donald Trump"),
        start_date >= '2024-07-21'
    )

avg_national_presidential_polling <- national_presidential_polling |>
    mutate(
        sample_size = as.numeric(sample_size),
        pollscore = as.numeric(pollscore),
        start_date = as.Date(start_date, format="%m/%d/%y"),
        end_date = as.Date(end_date, format="%m/%d/%y")
    ) |>
    filter(
        !is.na(sample_size),
        internal != "TRUE",
        !partisan %in% c("DEM", "REP"),
        candidate_name %in% c("Kamala Harris", "Donald Trump"),
        start_date >= '2024-07-21'
    ) |>
    summarize(
        pct = round(weighted.mean(pct, as.numeric(sample_size), na.rm = TRUE),1),
        .by = c(start_date,candidate_name)
    ) |>
    arrange(start_date)

avg_swing_presidential_polling <- national_presidential_polling |>
    mutate(
        sample_size = as.numeric(sample_size),
        pollscore = as.numeric(pollscore),
        start_date = as.Date(start_date, format="%m/%d/%y"),
        end_date = as.Date(end_date, format="%m/%d/%y")
    ) |>
    filter(
        !is.na(sample_size),
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

swing_state_summary <- avg_swing_presidential_polling |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1))

pennsylvania_summary <- avg_swing_presidential_polling |> 
    filter(state == "Pennsylvania") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

michigan_summary <- avg_swing_presidential_polling |> 
    filter(state == "Michigan") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

wisconsin_summary <- avg_swing_presidential_polling |> 
    filter(state == "Wisconsin") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

georgia_summary <- avg_swing_presidential_polling |> 
    filter(state == "Georgia") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

north_carolina_summary <- avg_swing_presidential_polling |> 
    filter(state == "North Carolina") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

nevada_summary <- avg_swing_presidential_polling |> 
    filter(state == "Nevada") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

arizona_summary <- avg_swing_presidential_polling |> 
    filter(state == "Arizona") |>
    group_by(state, candidate_name) |>
    summarize(pct = round(mean(a_pct, na.rm = TRUE),1)) |>
    arrange(desc(pct))

national_polling_chart <- ggplot(
    avg_national_presidential_polling,
    aes(x=start_date,y=pct,color=candidate_name,fill=candidate_name)
) + geom_smooth() + 
    geom_point(
        data=xnational_presidential_polling,
        mapping=aes(x=start_date,y=pct,alpha=pct)
    ) +
    scale_color_manual(values=c("Donald Trump" = "#D6090B","Kamala Harris" = "#031BBB")) +
    scale_fill_manual(values=c("Donald Trump" = "#D6090B","Kamala Harris" = "#031BBB")) 

swimg_state_summary_chart <- ggplot(
    swing_state_summary,
    aes(x=state, y=pct, fill=candidate_name, group=candidate_name)
) + 
    geom_col(position="dodge") + 
    scale_fill_manual(values=c(
        "Donald Trump" = "#D6090B",
        "Kamala Harris" = "#031BBB"
    )
    ) +
    coord_cartesian(ylim=c(40, 50)) +
    theme(axis.text.x = element_text(angle=90, hjust=0.5))

print(pennsylvania_summary)
print(michigan_summary)
print(wisconsin_summary)
print(georgia_summary)
print(north_carolina_summary)
print(nevada_summary)
print(arizona_summary)