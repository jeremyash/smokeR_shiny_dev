deploy_prod <- function() {
  rsconnect::deployApp(
    appDir = "smokeR_dev",
    appName = "smoker",
    account = "r8-arm"
  )
}

deploy_prod()
