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

deploy_prod <- function() {
  rsconnect::deployApp(
    appDir = ".",
    appName = "smoker",
    appTitle = "smoker",
    account = "r8-arm",
    forceUpdate = TRUE
  )
}