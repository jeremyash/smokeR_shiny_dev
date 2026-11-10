
upload_report_to_github_pages <- function(
    local_file,
    owner,
    repo,
    branch = "main",
    pages_dir = "docs/sr",
    report_filename,
    commit_message = paste("Add smoke report", report_filename)
) {
  token <- get_github_pat()
  
  if (identical(token, "")) {
    stop("GITHUB_PAT is not set.")
  }
  
  github_path <- file.path(pages_dir, report_filename)
  
  existing <- tryCatch(
    gh::gh(
      "GET /repos/{owner}/{repo}/contents/{path}",
      owner = owner,
      repo = repo,
      path = github_path,
      ref = branch,
      .token = token
    ),
    error = function(e) NULL
  )
  
  sha <- if (!is.null(existing)) existing$sha else NULL
  
  gh::gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = repo,
    path = github_path,
    message = commit_message,
    content = base64enc::base64encode(local_file),
    sha = sha,
    branch = branch,
    .token = token
  )
  
  pages_url_dir <- pages_dir |>
    stringr::str_replace("^docs/?", "") |>
    stringr::str_replace_all("^/|/$", "")
  
  if (nzchar(pages_url_dir)) {
    paste0(
      "https://reports.rxsmoke.org/",
      pages_url_dir, "/",
      report_filename
    )
  } else {
    paste0(
      "https://reports.rxsmoke.org/",
      report_filename
    )
  }
}


make_github_pages_url <- function(
    owner,
    repo,
    pages_dir,
    report_filename
) {
  pages_url_dir <- pages_dir |>
    stringr::str_replace("^docs/?", "") |>
    stringr::str_replace_all("^/|/$", "")
  
  if (nzchar(pages_url_dir)) {
    paste0(
      "https://reports.rxsmoke.org/",
      pages_url_dir, "/",
      report_filename
    )
  } else {
    paste0(
      "https://reports.rxsmoke.org/",
      report_filename
    )
  }
}


log_standalone_pb_piedmont_map <- function(
    region,
    forest,
    burn_unit,
    latitude,
    longitude,
    pb_map_url
) {
  tryCatch(
    expr = {
      googledrive::drive_auth(path = ".secrets/smoke-report-logs-7ae50f5a86d1.json")
      googlesheets4::gs4_auth(path = ".secrets/smoke-report-logs-7ae50f5a86d1.json")
      
      log_url <- "https://docs.google.com/spreadsheets/d/1MR94IFlQSbBQ5mbh1nfnyPwT9Eu0GACjquya7rlg1KE/edit?gid=0#gid=0"
      
      model_run_df <- data.frame(
        "Region" = region,
        "Forest" = forest,
        "Burn Unit" = burn_unit,
        "Burn Date" = as.Date(NA),
        "Date Issued" = Sys.Date(),
        "Latitude" = latitude,
        "Longitude" = longitude,
        "Acreage" = NA_real_,
        "PG Link" = NA_character_,
        "Superfog Potential" = NA_character_,
        "Smoke Report Link" = NA_character_,
        "PB Piedmont Map Link" = pb_map_url,
        "Report Type" = "Standalone PB Piedmont Map",
        check.names = FALSE
      )
      
      googlesheets4::sheet_append(log_url, model_run_df, sheet = 1)
    },
    error = function(e) {
      message("FAILED TO WRITE STANDALONE PB PIEDMONT MAP LOG TO GOOGLE SHEETS: ", conditionMessage(e))
      return(NULL)
    }
  )
}

update_index_page <- function(
    owner,
    repo,
    report_filename,
    report_type = c("report", "pb"),
    region,
    forest,
    burn_name,
    burn_date,
    issued_at,
    branch = "main"
) {
  report_type <- match.arg(report_type)
  
  token <- get_github_pat()
  
  if (identical(token, "")) {
    stop("GITHUB_PAT is not set.")
  }
  
  index_path <- "docs/index.html"
  
  existing <- gh::gh(
    "GET /repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = repo,
    path = index_path,
    ref = branch,
    .token = token
  )
  
  index_html <- rawToChar(base64enc::base64decode(existing$content))
  sha <- existing$sha
  
  if (report_type == "report") {
    href <- paste0("sr/", report_filename)
    product_label <- "Smoke Report"
    start_marker <- "<!-- ===== BEGIN AUTO-GENERATED SMOKE REPORT LINKS ===== -->"
    empty_pattern <- "<p class=\"empty\">\\s*No reports available yet\\.\\s*</p>"
  } else {
    href <- paste0("pb/", report_filename)
    product_label <- "PB Piedmont"
    start_marker <- "<!-- ===== BEGIN AUTO-GENERATED PB PIEDMONT LINKS ===== -->"
    empty_pattern <- "<p class=\"empty\">\\s*No PB Piedmont maps available yet\\.\\s*</p>"
  }
  
  if (!grepl(start_marker, index_html, fixed = TRUE)) {
    stop("Index start marker not found: ", start_marker)
  }
  
  issued_label <- paste0(
    format(issued_at, "%Y-%m-%d %H:%M"),
    " ",
    format(issued_at, "%Z")
  )
  
  new_entry <- paste0(
    "    <div class='report-entry' ",
    "data-product='", htmltools::htmlEscape(product_label), "' ",
    "data-region='", htmltools::htmlEscape(region), "' ",
    "data-forest='", htmltools::htmlEscape(forest), "' ",
    "data-burn-name='", htmltools::htmlEscape(burn_name), "' ",
    "data-burn-date='", htmltools::htmlEscape(as.character(burn_date)), "' ",
    "data-issued='", htmltools::htmlEscape(format(issued_at, "%Y-%m-%dT%H:%M:%S")), "' ",
    "data-issued-label='", htmltools::htmlEscape(issued_label), "' ",
    "data-url='", htmltools::htmlEscape(href), "'>",
    "</div>\n"
  )
  
  index_html <- gsub(empty_pattern, "", index_html, perl = TRUE)
  
  index_html <- sub(
    start_marker,
    paste0(start_marker, "\n", new_entry),
    index_html,
    fixed = TRUE
  )
  
  gh::gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = repo,
    path = index_path,
    message = paste("Update index with", report_filename),
    content = base64enc::base64encode(charToRaw(index_html)),
    sha = sha,
    branch = branch,
    .token = token
  )
}




# 1. Upload report and archive index in one commit ----
# Purpose: Publish a Smoke Report and update docs/index.html
#          using one GitHub commit, with safe concurrency retries.
# Source: GitHub Git Data API via gh.
# Note: Existing upload functions remain unchanged.

upload_report_and_index <- function(
    local_file,
    owner,
    repo,
    branch = "main",
    pages_dir = "docs/sr",
    report_filename,
    region,
    forest,
    burn_name,
    burn_date,
    issued_at,
    commit_message = paste("Add smoke report", report_filename),
    max_attempts = 5
) {
  
  # 1. Validate inputs ----
  
  token <- get_github_pat()
  
  if (!nzchar(token)) {
    stop("GITHUB_PAT is not set.")
  }
  
  if (!file.exists(local_file)) {
    stop("Report file does not exist: ", local_file)
  }
  
  if (length(max_attempts) != 1L ||
      is.na(max_attempts) ||
      !is.finite(max_attempts) ||
      max_attempts < 1 ||
      max_attempts != as.integer(max_attempts)) {
    stop("max_attempts must be a positive integer.")
  }
  
  github_path <- paste0(
    sub("/+$", "", pages_dir),
    "/",
    report_filename
  )
  
  index_path <- "docs/index.html"
  
  # 2. Prepare report blob once ----
  # The report content does not change between retry attempts.
  
  report_blob <- gh::gh(
    "POST /repos/{owner}/{repo}/git/blobs",
    owner = owner,
    repo = repo,
    content = base64enc::base64encode(local_file),
    encoding = "base64",
    .token = token
  )
  
  # 3. Prepare index entry ----
  
  start_marker <- paste0(
    "<!-- ===== BEGIN AUTO-GENERATED ",
    "SMOKE REPORT LINKS ===== -->"
  )
  
  issued_label <- paste0(
    format(issued_at, "%Y-%m-%d %H:%M"),
    " ",
    format(issued_at, "%Z")
  )
  
  href <- paste0(
    sub("^docs/?", "", github_path)
  )
  
  new_entry <- paste0(
    "    <div class='report-entry' ",
    "data-product='Smoke Report' ",
    "data-region='", htmltools::htmlEscape(region), "' ",
    "data-forest='", htmltools::htmlEscape(forest), "' ",
    "data-burn-name='", htmltools::htmlEscape(burn_name), "' ",
    "data-burn-date='",
    htmltools::htmlEscape(as.character(burn_date)), "' ",
    "data-issued='",
    htmltools::htmlEscape(
      format(issued_at, "%Y-%m-%dT%H:%M:%S")
    ), "' ",
    "data-issued-label='",
    htmltools::htmlEscape(issued_label), "' ",
    "data-url='", htmltools::htmlEscape(href), "'>",
    "</div>\n"
  )
  
  # 4. Helper: identify branch conflicts ----
  # GitHub generally returns 409 or 422 for reference-update
  # conflicts. Other errors should be reported immediately.
  
  is_branch_conflict <- function(e) {
    
    status <- tryCatch(
      httr::status_code(e$response),
      error = function(err) NA_integer_
    )
    
    if (is.na(status)) {
      status <- tryCatch(
        as.integer(e$status),
        error = function(err) NA_integer_
      )
    }
    
    if (length(status) != 1L || is.na(status)) {
      return(FALSE)
    }
    
    status %in% c(409L, 422L)
  }
  
  # 5. Retry loop ----
  
  for (attempt in seq_len(max_attempts)) {
    
    message(
      "GitHub combined upload attempt ",
      attempt,
      " of ",
      max_attempts
    )
    
    # 5.1 Read current branch reference ----
    
    branch_info <- gh::gh(
      "GET /repos/{owner}/{repo}/git/ref/heads/{branch}",
      owner = owner,
      repo = repo,
      branch = branch,
      .token = token
    )
    
    parent_sha <- branch_info$object$sha
    
    # 5.2 Read exact parent commit and tree ----
    
    parent_commit <- gh::gh(
      "GET /repos/{owner}/{repo}/git/commits/{commit_sha}",
      owner = owner,
      repo = repo,
      commit_sha = parent_sha,
      .token = token
    )
    
    base_tree_sha <- parent_commit$tree$sha
    
    # 5.3 Read index from the same parent commit ----
    # Using ref = parent_sha prevents mixing an old branch
    # reference with a newer archive index.
    
    existing_index <- gh::gh(
      "GET /repos/{owner}/{repo}/contents/{path}",
      owner = owner,
      repo = repo,
      path = index_path,
      ref = parent_sha,
      .token = token
    )
    
    index_html <- rawToChar(
      base64enc::base64decode(
        gsub("\\s", "", existing_index$content)
      )
    )
    
    # 5.4 Update archive index ----
    
    if (!grepl(start_marker, index_html, fixed = TRUE)) {
      stop("Index start marker not found.")
    }
    
    index_html <- gsub(
      "<p class=\"empty\">\\s*No reports available yet\\.\\s*</p>",
      "",
      index_html,
      perl = TRUE
    )
    
    index_html <- sub(
      start_marker,
      paste0(start_marker, "\n", new_entry),
      index_html,
      fixed = TRUE
    )
    
    # 5.5 Create index blob ----
    
    index_blob <- gh::gh(
      "POST /repos/{owner}/{repo}/git/blobs",
      owner = owner,
      repo = repo,
      content = index_html,
      encoding = "utf-8",
      .token = token
    )
    
    # 5.6 Create tree containing both files ----
    
    new_tree <- gh::gh(
      "POST /repos/{owner}/{repo}/git/trees",
      owner = owner,
      repo = repo,
      base_tree = base_tree_sha,
      tree = list(
        list(
          path = github_path,
          mode = "100644",
          type = "blob",
          sha = report_blob$sha
        ),
        list(
          path = index_path,
          mode = "100644",
          type = "blob",
          sha = index_blob$sha
        )
      ),
      .token = token
    )
    
    # 5.7 Create commit against current parent ----
    
    new_commit <- gh::gh(
      "POST /repos/{owner}/{repo}/git/commits",
      owner = owner,
      repo = repo,
      message = commit_message,
      tree = new_tree$sha,
      parents = list(parent_sha),
      .token = token
    )
    
    # 5.8 Verify branch has not changed ----
    
    latest_branch <- gh::gh(
      "GET /repos/{owner}/{repo}/git/ref/heads/{branch}",
      owner = owner,
      repo = repo,
      branch = branch,
      .token = token
    )
    
    if (!identical(latest_branch$object$sha, parent_sha)) {
      
      if (attempt == max_attempts) {
        stop(
          "GitHub branch changed during upload after ",
          max_attempts,
          " attempts. Report was not published."
        )
      }
      
      delay <- runif(
        1,
        min = 0.5,
        max = min(5, attempt * 1.5)
      )
      
      message(
        "Another report was committed during upload. ",
        "Retrying in ",
        round(delay, 2),
        " seconds."
      )
      
      Sys.sleep(delay)
      
      next
    }
    
    # 5.9 Attempt non-force branch update ----
    # Only this operation is eligible for concurrency retries.
    
    update_result <- tryCatch(
      {
        gh::gh(
          "PATCH /repos/{owner}/{repo}/git/refs/heads/{branch}",
          owner = owner,
          repo = repo,
          branch = branch,
          sha = new_commit$sha,
          force = FALSE,
          .token = token
        )
        
        list(success = TRUE, error = NULL)
      },
      error = function(e) {
        list(success = FALSE, error = e)
      }
    )
    
    if (update_result$success) {
      
      message(
        "Report and index committed together: ",
        new_commit$sha
      )
      
      return(
        make_github_pages_url(
          owner = owner,
          repo = repo,
          pages_dir = pages_dir,
          report_filename = report_filename
        )
      )
    }
    
    # 5.10 Handle branch update failure ----
    
    err <- update_result$error
    
    if (!is_branch_conflict(err)) {
      stop(err)
    }
    
    if (attempt == max_attempts) {
      stop(
        "GitHub branch update conflict after ",
        max_attempts,
        " attempts. Report was not published."
      )
    }
    
    delay <- runif(
      1,
      min = 0.5,
      max = min(5, attempt * 1.5)
    )
    
    message(
      "GitHub branch changed during upload. ",
      "Retrying in ",
      round(delay, 2),
      " seconds."
    )
    
    Sys.sleep(delay)
  }
}


# 2. Upload smoke report to Cloudflare R2 ----
# Purpose: Publish a generated HTML smoke report directly to R2.
# Source: Cloudflare R2 S3-compatible API via paws.storage.
# Returns: Public URL of the uploaded report.

upload_report_to_r2 <- function(
    local_file,
    report_filename,
    folder = "sr",
    public_base_url = Sys.getenv("R2_PROD_PUBLIC_URL")
) {
  
  ## 2.1 Validate inputs ----
  
  if (!file.exists(local_file)) {
    stop("Report file does not exist: ", local_file)
  }
  
  required_vars <- c(
    "R2_PROD_ACCESS_KEY_ID",
    "R2_PROD_SECRET_ACCESS_KEY",
    "R2_ACCOUNT_ID",
    "R2_PROD_BUCKET"
  )
  
  missing_vars <- required_vars[
    !nzchar(Sys.getenv(required_vars))
  ]
  
  if (length(missing_vars) > 0) {
    stop(
      "Missing R2 environment variables: ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  if (!nzchar(public_base_url)) {
    stop("R2_PROD_PUBLIC_URL is not set.")
  }
  
  ## 2.2 Create R2 connection ----
  
  r2 <- paws.storage::s3(
    config = list(
      credentials = list(
        creds = list(
          access_key_id = Sys.getenv("R2_PROD_ACCESS_KEY_ID"),
          secret_access_key = Sys.getenv("R2_PROD_SECRET_ACCESS_KEY")
        )
      ),
      endpoint = paste0(
        "https://",
        Sys.getenv("R2_ACCOUNT_ID"),
        ".r2.cloudflarestorage.com"
      ),
      region = "auto",
      s3_force_path_style = TRUE,
      request_checksum_calculation = "when_required"
    )
  )
  
  ## 2.3 Upload HTML report ----
  
  object_key <- paste0(
    sub("/+$", "", folder),
    "/",
    report_filename
  )
  
  report_raw <- readBin(
    local_file,
    what = "raw",
    n = file.info(local_file)$size
  )
  
  upload_time <- system.time({
    
    r2$put_object(
      Bucket = Sys.getenv("R2_PROD_BUCKET"),
      Key = object_key,
      Body = report_raw,
      ContentType = "text/html; charset=utf-8"
    )
    
  })
  
  message(
    "R2 report uploaded in ",
    round(upload_time[["elapsed"]], 2),
    " seconds: ",
    object_key
  )
  
  ## 2.4 Return public URL ----
  
  paste0(
    sub("/+$", "", public_base_url),
    "/",
    object_key
  )
}

# 1. Synchronize GitHub archive index to Cloudflare R2 ----
#
# Purpose:
# Retrieve the latest committed GitHub archive index and
# publish it to the production Cloudflare R2 bucket.

sync_archive_index_to_r2 <- function(
    owner,
    repo,
    branch = "main"
) {
  
  ## 1.1 Retrieve latest GitHub index ----
  
  index_response <- gh::gh(
    "GET /repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = repo,
    path = "docs/index.html",
    ref = branch,
    .token = get_github_pat()
  )
  
  index_raw <- base64enc::base64decode(
    gsub("\\s", "", index_response$content)
  )
  
  ## 1.2 Connect to production R2 ----
  
  r2 <- paws.storage::s3(
    config = list(
      credentials = list(
        creds = list(
          access_key_id = Sys.getenv("R2_PROD_ACCESS_KEY_ID"),
          secret_access_key = Sys.getenv("R2_PROD_SECRET_ACCESS_KEY")
        )
      ),
      endpoint = paste0(
        "https://",
        Sys.getenv("R2_ACCOUNT_ID"),
        ".r2.cloudflarestorage.com"
      ),
      region = "auto",
      s3_force_path_style = TRUE,
      request_checksum_calculation = "when_required"
    )
  )
  
  ## 1.3 Upload archive index ----
  
  r2$put_object(
    Bucket = Sys.getenv("R2_PROD_BUCKET"),
    Key = "index.html",
    Body = index_raw,
    ContentType = "text/html; charset=utf-8",
    CacheControl = "no-cache"
  )
  
  message("Cloudflare R2 archive index synchronized successfully.")
  
  invisible(TRUE)
}


# 3. Upload report metadata to Cloudflare R2 ----
# Purpose: Publish JSON metadata for the searchable report archive.
# Source: Cloudflare R2 S3-compatible API via paws.storage.
# Returns: R2 object key of the uploaded metadata.

upload_report_metadata_to_r2 <- function(
    metadata,
    report_filename,
    folder = "sr"
) {
  
  ## 3.1 Validate inputs ----
  
  required_vars <- c(
    "R2_PROD_ACCESS_KEY_ID",
    "R2_PROD_SECRET_ACCESS_KEY",
    "R2_ACCOUNT_ID",
    "R2_PROD_BUCKET"
  )
  
  missing_vars <- required_vars[
    !nzchar(Sys.getenv(required_vars))
  ]
  
  if (length(missing_vars) > 0) {
    stop(
      "Missing R2 environment variables: ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  if (!folder %in% c("sr", "pb", "mb")) {
    stop("Invalid metadata folder: ", folder)
  }
  
  if (!is.list(metadata) || is.null(names(metadata))) {
    stop("Metadata must be a named list.")
  }
  
  ## 3.2 Create R2 connection ----
  
  r2 <- paws.storage::s3(
    config = list(
      credentials = list(
        creds = list(
          access_key_id = Sys.getenv("R2_PROD_ACCESS_KEY_ID"),
          secret_access_key = Sys.getenv("R2_PROD_SECRET_ACCESS_KEY")
        )
      ),
      endpoint = paste0(
        "https://",
        Sys.getenv("R2_ACCOUNT_ID"),
        ".r2.cloudflarestorage.com"
      ),
      region = "auto",
      s3_force_path_style = TRUE,
      request_checksum_calculation = "when_required"
    )
  )
  
  ## 3.3 Prepare metadata JSON ----
  
  metadata_key <- paste0(
    "metadata/",
    folder,
    "/",
    sub("\\.html$", ".json", basename(report_filename))
  )
  
  metadata_json <- jsonlite::toJSON(
    metadata,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  
  ## 3.4 Upload JSON metadata ----
  
  upload_time <- system.time({
    
    r2$put_object(
      Bucket = Sys.getenv("R2_PROD_BUCKET"),
      Key = metadata_key,
      Body = charToRaw(enc2utf8(as.character(metadata_json))),
      ContentType = "application/json; charset=utf-8"
    )
    
  })
  
  message(
    "R2 metadata uploaded in ",
    round(upload_time[["elapsed"]], 2),
    " seconds: ",
    metadata_key
  )
  
  ## 3.5 Return metadata object key ----
  
  metadata_key
}

