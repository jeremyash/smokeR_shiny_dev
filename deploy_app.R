options(rsconnect.http.timeout = 600)

deploy_dev <- function() {
  rsconnect::deployApp(
    appDir = ".",
    appName = "smoker_dev",
    appTitle = "smoker_dev",
    account = "r8-arm",
    forceUpdate = TRUE
  )
}

# 1. Deploy Smoke Report Generator ----
# Purpose: Deploy the Shiny app with its R2 environment variables.

options(rsconnect.http.timeout = 600)

deploy_prod <- function() {
  
  r2_vars <- c(
    "R2_ACCOUNT_ID",
    "R2_PROD_ACCESS_KEY_ID",
    "R2_PROD_SECRET_ACCESS_KEY",
    "R2_PROD_BUCKET",
    "R2_PROD_PUBLIC_URL"
  )
  
  # Confirm required variables exist locally
  missing_vars <- r2_vars[
    !nzchar(Sys.getenv(r2_vars))
  ]
  
  if (length(missing_vars) > 0) {
    stop(
      "Missing local environment variables: ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  # 1. Prepare deployment files ----
  
  deployment_files <- rsconnect::listDeploymentFiles(
    appDir = "."
  )
  
  deployment_files <- setdiff(
    deployment_files,
    ".secrets/github_pat.txt"
  )
  
  rsconnect::deployApp(
    appDir = ".",
    appName = "smoker",
    appTitle = "smoker",
    account = "jash",
    forceUpdate = TRUE,
    dependencyResolution = "library",
    envVars = r2_vars,
    appFiles = deployment_files
  )
}
