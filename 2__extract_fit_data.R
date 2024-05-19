library(data.table)
library(ggplot2)

remotes::install_github("grimbough/FITfileR")

library(FITfileR)

activity_dt <- fread("strava_data/activities.csv") |>
    setnames(tolower) |>
    setnames(\(x) gsub(" ", "_", x))

activity_dt[, activity_day := as.IDate(
    gsub(",.+$", "", activity_date),
    format = "%d %b %Y"
)]

istanbul_trip <- activity_dt[
    activity_day >= "2022-06-25" &
        activity_day <= "2022-08-05"
]

cumlative_km_plot <- istanbul_trip[
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
    ) +
    theme(plot.title = element_text(hjust = 0.5))

ggsave(
    "output/cumlative_distance_plot.pdf",
    cumlative_km_plot
)

# Now read all 'fit' data and save as a csv
gpx_files_to_get <- istanbul_trip[, paste0("strava_data/", filename)]

UnzipAndRead <- function(filename) {
    R.utils::gunzip(
        filename,
        remove = FALSE,
        overwrite = TRUE
    )

    FITfileR::readFitFile(
        gsub("\\.gz$", "", filename)
    )
}

fit_list <- lapply(
    gpx_files_to_get,
    UnzipAndRead
)

rbindlist(FITfileR::records(fit_list[[1]]),
    fill = TRUE
)

record_list <- lapply(
    fit_list,
    \(fit) FITfileR::records(fit)
)


record_dt <- record_list |>
    lapply(\(data){
        if (is.data.frame(data)) {
            return(data)
        } else {
            return(rbindlist(data, fill = T))
        }
    }) |>
    rbindlist(fill = TRUE)

fwrite(
    record_dt,
    "output/all_record_data.csv"
)
