make_report_filename <- function(burn_name, forest, date = Sys.Date()) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    short_forest_name(forest),
    "-",
    safe_filename(burn_name),
    ".html"
  )
}

make_pb_filename <- function(burn_name, forest, date = Sys.Date()) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    short_forest_name(forest),
    "-",
    safe_filename(burn_name),
    "-pb-piedmont.html"
  )
}

make_kmz_filename <- function(burn_name, forest, date = Sys.Date()) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    short_forest_name(forest),
    "-",
    safe_filename(burn_name),
    "-bsky-dispersion.kmz"
  )
}