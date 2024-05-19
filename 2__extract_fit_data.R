library(renv)
library(data.table)
library(ggplot2)
library(httpgd)

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
][order(activity_day)]

daily_data <- istanbul_trip[
    , .(
        daily_distance = sum(distance),
        daily_elevation = sum(elevation_gain),
        daily_ratio = sum(elevation_gain) / sum(distance)
    ),
    activity_day
] |>
    _[, ":="(
        cumlative_distance = cumsum(daily_distance),
        cumlative_elevation = cumsum(daily_elevation))] |>
    _[, cumlative_ratio := cumlative_elevation / cumlative_distance]

daily_data |>
    ggplot(aes(
        x = activity_day,
        y = cumlative_ratio
    )) +
    geom_point()

daily_data |>
    melt(
        id.var = "activity_day",
        measure.vars = patterns("cumlative")
    ) |>
    ggplot(
        aes(
            x = activity_day,
            y = value,
            colour = variable
        )
    ) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    facet_grid(
        rows = vars(variable),
        scales = "free"
    ) +
    labs(
        x = "Date",
        y = "Kilometers",
        title = "Istanbul trip statistics"
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
