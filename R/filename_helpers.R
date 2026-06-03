make_report_filename <- function(
    burn_name,
    forest,
    date = Sys.Date(),
    issued_time = Sys.time()
) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    safe_filename(burn_name),
    "-",
    short_forest_name(forest),
    "-i",
    format(issued_time, "%H%M"),
    ".html"
  )
}

make_pb_filename <- function(
    burn_name,
    forest,
    date = Sys.Date(),
    issued_time = Sys.time()
) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    safe_filename(burn_name),
    "-",
    short_forest_name(forest),
    "-i",
    format(issued_time, "%H%M"),
    "-pb-piedmont.html"
  )
}

make_kmz_filename <- function(
    burn_name,
    forest,
    date = Sys.Date(),
    issued_time = Sys.time()
) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    safe_filename(burn_name),
    "-",
    short_forest_name(forest),
    "-i",
    format(issued_time, "%H%M"),
    "-bsky-dispersion.kmz"
  )
}