library(data.table)
library(ggplot2)

activity_dt <- fread("strava_data/activities.csv") |>
    setnames(tolower) |>
    setnames(\(x) gsub(" ", "_", x))

activity_dt[, activity_day := as.IDate(
    gsub(",.+$", "", activity_date),
    format = "%d %b %Y"
)]

istanbul_trip <- activity_dt[activity_day >= "2022-06-25" &
    activity_day <= "2022-08-05"]


istanbul_trip[, sum(distance)]
istanbul_trip[, sum(elevation_gain)]

istanbul_trip[distance < 0]
istanbul_trip[
    , .(daily_distance = sum(distance)),
    activity_day
] |>
    _[order(activity_day), cumlative_distance := cumsum(daily_distance)] |>
    ggplot(
        aes(
            x = activity_day,
            y = cumlative_distance
        )
    ) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    labs(
        x = "Date",
        y = "Kilometers",
        title = "Istanbul trip daily distance"
    )


gpx_files_to_get <- istanbul_trip[, filename]
