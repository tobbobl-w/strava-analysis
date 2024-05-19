library(data.table)

dir()

activity_dt <- fread("strava_data/activities.csv") |>
    setnames(tolower) |>
    setnames(\(x) gsub(" ", "_", x))

activity_dt[, activate_date := as.IDateTime(activate_date)]

activity_dt[, activity_day := as.IDate(
    gsub(",.+$", "", activity_date),
    format = "%d %b %Y"
)]


activity_dt[, head(activity_day)]
