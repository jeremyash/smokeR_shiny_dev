get_pb_zip_burn_date <- function(pb_zip_path, burn_lat = NA, burn_lon = NA) {
  pb_hourly_dir <- tempfile("pb_hourly_date_")
  dir.create(pb_hourly_dir, showWarnings = FALSE, recursive = TRUE)
  
  unzip(pb_zip_path, exdir = pb_hourly_dir)
  
  pb_txt_files <- list.files(
    pb_hourly_dir,
    pattern = "^[0-9]{2}[A-Za-z]{3}[0-9]{4}_[0-9]{4}\\.txt$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(pb_txt_files) == 0) {
    return(as.Date(NA))
  }
  
  file_dates_utc <- as.POSIXct(
    stringr::str_to_title(
      stringr::str_remove(basename(pb_txt_files), "\\.txt$")
    ),
    format = "%d%b%Y_%H%M",
    tz = "UTC"
  )
  
  burn_tz <- tryCatch(
    lutz::tz_lookup_coords(
      lat = burn_lat,
      lon = burn_lon,
      method = "fast"
    ),
    error = function(e) "UTC"
  )
  
  if (is.na(burn_tz) || !nzchar(burn_tz)) {
    burn_tz <- "UTC"
  }
  
  min(as.Date(lubridate::with_tz(file_dates_utc, tzone = burn_tz)), na.rm = TRUE)
}


create_pb_piedmont_map <- function(
    burn_name,
    forest,
    run_id = NA,
    pb_zip_datapath,
    pb_zip_name = "hourly_output.zip",
    burn_lat = NA,
    burn_lon = NA,
    issued_at = NULL,
    owner = "jeremyash",
    repo = "smoke_reports",
    branch = "main",
    pages_dir = "docs/pb"
) {
  
  if (is.null(issued_at)) {
    issued_at <- get_burn_issued_time(
      lat = burn_lat,
      lon = burn_lon
    )
  }
  
  pb_map_filename <- make_pb_filename(
    burn_name = burn_name,
    date = as.Date(issued_at),
    issued_time = issued_at
  )
  
  pb_zip_copy <- file.path(
    tempdir(),
    paste0(
      tools::file_path_sans_ext(basename(pb_zip_name)),
      "-",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      ".zip"
    )
  )
  
  file.copy(pb_zip_datapath, pb_zip_copy, overwrite = TRUE)
  
  pb_burn_date <- get_pb_zip_burn_date(
    pb_zip_path = pb_zip_copy,
    burn_lat = burn_lat,
    burn_lon = burn_lon
  )
  
  pb_rendered_file <- file.path(tempdir(), pb_map_filename)
  
  rmarkdown::render(
    input = here::here(
      "templates",
      "pb_piedmont_particle_map.Rmd"
    ),
    output_file = pb_rendered_file,
    params = list(
      BURN_NAME = burn_name,
      FOREST = forest,
      RUN_ID = run_id,
      PB_HOURLY_ZIP = pb_zip_copy,
      BURN_DATE = NA,
      BURN_LAT = burn_lat,
      BURN_LON = burn_lon
    ),
    envir = new.env(parent = globalenv())
  )
  
  pb_map_url <- upload_report_to_github_pages(
    local_file = pb_rendered_file,
    owner = owner,
    repo = repo,
    branch = branch,
    pages_dir = pages_dir,
    report_filename = pb_map_filename,
    commit_message = paste(
      burn_name,
      "| PB Piedmont |",
      format(issued_at, "%Y-%m-%d"),
      "|",
      forest
    )
  )
  
  tryCatch(
    {
      update_index_page(
        owner = owner,
        repo = repo,
        report_filename = pb_map_filename,
        report_type = "pb",
        region = "R08",
        forest = forest,
        burn_name = burn_name,
        burn_date = pb_burn_date,
        issued_at = issued_at,
        branch = branch
      )
      
      message("PB Piedmont index updated successfully")
    },
    error = function(e) {
      message("PB Piedmont index update failed: ", conditionMessage(e))
    }
  )
  
  list(
    url = pb_map_url,
    filename = pb_map_filename,
    local_file = pb_rendered_file
  )
}