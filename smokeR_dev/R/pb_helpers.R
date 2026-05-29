create_pb_piedmont_map <- function(
    burn_name,
    forest,
    run_id = NA,
    pb_zip_datapath,
    pb_zip_name = "hourly_output.zip",
    burn_lat = NA,
    burn_lon = NA,
    owner = "jeremyash",
    repo = "smoke_reports",
    branch = "main",
    pages_dir = "docs/pb-piedmont"
) {
  burn_name_for_file <- burn_name |>
    as.character() |>
    safe_filename()
  
  forest_short_for_file <- forest |>
    as.character() |>
    short_forest_name()
  
  pb_map_filename <- make_pb_filename(
    burn_name = burn_name,
    forest = forest
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
  
  pb_rendered_file <- file.path(tempdir(), pb_map_filename)
  
  rmarkdown::render(
    input = here::here(
      "smokeR_dev",
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
      format(Sys.Date(), "%Y-%m-%d"),
      "|",
      forest
    )
  )
  
  list(
    url = pb_map_url,
    filename = pb_map_filename,
    local_file = pb_rendered_file
  )
}