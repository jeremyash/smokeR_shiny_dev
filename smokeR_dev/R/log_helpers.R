append_smoke_app_log <- function(
    sheet_url,
    report_type = NA,
    region = NA,
    forest = NA,
    burn_name = NA,
    burn_date = NA,
    date_issued = NA,
    lat = NA,
    lon = NA,
    acreage = NA,
    run_id = NA,
    superfog_potential = NA,
    day_before_or_of = NA,
    report_url = NA,
    pb_map_url = NA
) {
  pg_link <- if (!is.na(run_id) && nzchar(run_id)) {
    paste0(
      "https://tools.airfire.org/playground/v3.5/dispersionresults.php?scenario_id=",
      run_id
    )
  } else {
    NA
  }
  
  log_row <- tibble::tibble(
    Region = region,
    Forest = forest,
    `Burn Unit` = burn_name,
    `Burn Date` = burn_date,
    `Date Issued` = date_issued,
    Latitude = lat,
    Longitude = lon,
    Acreage = acreage,
    `PG Link` = pg_link,
    `Superfog Potential` = superfog_potential,
    `Day before or of` = day_before_or_of,
    `Smoke Report Link` = report_url,
    `PB Piedmont Map Link` = pb_map_url,
    `Report Type` = report_type
  )
  
  googlesheets4::sheet_append(
    ss = sheet_url,
    data = log_row
  )
}