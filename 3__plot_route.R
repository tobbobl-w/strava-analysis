library(leaflet)
library(data.table)
library(htmlwidgets)

record_dt <- fread("all_record_data.csv")

map <- record_dt[order(timestamp)] |>
    _[
        !is.na(speed) &
            gps_accuracy == 3 &
            !is.na(distance),
        .(position_long, position_lat)
    ] |>
    as.matrix() |>
    leaflet() |>
    addTiles() |>
    addPolylines()

saveWidget(
    map,
    file = "output/map_to_istanbul.html"
)
