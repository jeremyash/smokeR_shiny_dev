make_report_filename <- function(burn_name, forest, date = Sys.Date()) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    safe_filename(burn_name),
    "-",
    short_forest_name(forest),
    ".html"
  )
}

make_pb_filename <- function(burn_name, forest, date = Sys.Date()) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    safe_filename(burn_name),
    "-",
    short_forest_name(forest),
    "-pb-piedmont.html"
  )
}

make_kmz_filename <- function(burn_name, forest, date = Sys.Date()) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    safe_filename(burn_name),
    "-",
    short_forest_name(forest),
    "-bsky-dispersion.kmz"
  )
}