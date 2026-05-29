`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

safe_filename <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "-") %>%
    stringr::str_replace_all("(^-|-$)", "")
}

short_forest_name <- function(x) {
  forest_safe <- safe_filename(x) %>%
    stringr::str_replace("national-forest", "nf") %>%
    stringr::str_replace("national-grassland", "ng")
  
  tokens <- stringr::str_split(forest_safe, "-", simplify = FALSE)[[1]]
  tokens <- tokens[tokens != ""]
  
  paste0(
    ifelse(tokens %in% c("nf", "ng"), tokens, stringr::str_sub(tokens, 1, 1)),
    collapse = ""
  )
}

get_github_pat <- function() {
  token <- Sys.getenv("GITHUB_PAT")
  
  if (!identical(token, "")) {
    return(token)
  }
  
  token_file <- ".secrets/github_pat.txt"
  
  if (file.exists(token_file)) {
    return(trimws(readLines(token_file, warn = FALSE)[1]))
  }
  
  stop("GitHub token not found.")
}