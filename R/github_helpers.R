upload_report_to_github_pages <- function(
    local_file,
    owner,
    repo,
    branch = "main",
    pages_dir = "docs/reports",
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
  
  pages_url_dir <- pages_dir %>%
    stringr::str_replace("^docs/?", "") %>%
    stringr::str_replace_all("^/|/$", "")
  
  if (nzchar(pages_url_dir)) {
    paste0("https://", owner, ".github.io/", repo, "/", pages_url_dir, "/", report_filename)
  } else {
    paste0("https://", owner, ".github.io/", repo, "/", report_filename)
  }
}


make_github_pages_url <- function(owner, repo, pages_dir, report_filename) {
  pages_url_dir <- pages_dir %>%
    stringr::str_replace("^docs/?", "") %>%
    stringr::str_replace_all("^/|/$", "")
  
  if (nzchar(pages_url_dir)) {
    paste0("https://", owner, ".github.io/", repo, "/", pages_url_dir, "/", report_filename)
  } else {
    paste0("https://", owner, ".github.io/", repo, "/", report_filename)
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
    report_label,
    branch = "main"
) {
  token <- get_github_pat()
  
  if (identical(token, "")) {
    stop("GITHUB_PAT is not set.")
  }
  
  index_path <- "docs/index.html"
  
  existing <- tryCatch(
    gh::gh(
      "GET /repos/{owner}/{repo}/contents/{path}",
      owner = owner,
      repo = repo,
      path = index_path,
      ref = branch,
      .token = token
    ),
    error = function(e) NULL
  )
  
  if (!is.null(existing)) {
    index_html <- rawToChar(base64enc::base64decode(existing$content))
    sha <- existing$sha
  } else {
    index_html <- paste0(
      "<!doctype html>\n",
      "<html>\n",
      "<head>\n",
      "  <meta charset='utf-8'>\n",
      "  <title>Smoke Reports</title>\n",
      "</head>\n",
      "<body>\n",
      "  <h1>Smoke Reports</h1>\n",
      "  <ul>\n",
      "  </ul>\n",
      "</body>\n",
      "</html>\n"
    )
    sha <- NULL
  }
  
  new_entry <- paste0(
    "    <li>",
    "<a href='reports/", report_filename, "' target='_blank'>",
    htmltools::htmlEscape(report_label),
    "</a>",
    "<br><span style='font-size:12px; color:#666;'>",
    htmltools::htmlEscape(report_filename),
    "</span>",
    "</li>\n"
  )
  
  if (grepl("</ul>", index_html, fixed = TRUE)) {
    index_html <- sub(
      "</ul>",
      paste0(new_entry, "</ul>"),
      index_html,
      fixed = TRUE
    )
  } else {
    index_html <- paste0(
      index_html,
      "\n<ul>\n",
      new_entry,
      "</ul>\n"
    )
  }
  
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
