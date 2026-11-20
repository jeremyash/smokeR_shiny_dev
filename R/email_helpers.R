
# email_helpers.R
# Purpose: Prepare standard smoke modeling email content.
# Source: Approved RxSmoke email templates.
# The output is plain text for initial validation.
# HTML formatting and Outlook clipboard support will be added later.


# 1. Shared email content ----

smoke_email_intro <- function(burn_name, report_url,
                              include_pb = FALSE, pb_url = NULL) {
  
  if (include_pb) {
    paste0(
      "The smoke modeling results for ", burn_name,
      ", based on the submitted parameters, are available ",
      "at the following links:\n\n",
      "Smoke Report with BlueSky Playground: ", report_url, "\n",
      "PB Piedmont – Nighttime Smoke and Superfog: ", pb_url
    )
  } else {
    paste0(
      "The smoke modeling results for ", burn_name,
      ", based on the submitted parameters, are available ",
      "in the Smoke Report. ", report_url
    )
  }
}


# 2. Shared PM2.5 explanation ----

smoke_email_pm_text <- function() {
  
  paste0(
    "I've also attached the Google Earth (KMZ) file for ",
    "viewing the BlueSky Playground smoke dispersion in ",
    "Google Earth.\n\n",
    "On the daily average map in the smoke outlook, ",
    "the lightest blue represents the lowest modeled ",
    "PM2.5 concentration category (1–9 µg/m³). ",
    "These low concentrations are included to help ",
    "illustrate the general direction and extent of ",
    "smoke transport. They are generally near background ",
    "levels and unlikely to be noticeable or represent ",
    "a significant air quality concern."
  )
}


# 3. Region 8 superfog statements ----

smoke_email_superfog_text <- function(superfog, pb_url = NULL) {
  
  if (identical(superfog, "Yes") &&
      !is.null(pb_url) && nzchar(pb_url)) {
    
    return(paste0(
      "Based on the weather parameters in your spot ",
      "forecast, I also ran PB Piedmont to evaluate ",
      "nighttime smoke dispersion and potential for ",
      "superfog formation. On the PB Piedmont map, ",
      "red points identify locations with elevated ",
      "superfog potential. These may be useful areas ",
      "and roadways to monitor for reduced visibility ",
      "during the overnight hours."
    ))
  }
  
  if (identical(superfog, "Yes")) {
    
    return(paste0(
      "Based on the weather parameters in your spot ",
      "forecast, the thresholds are met to evaluate ",
      "nighttime smoke dispersion and superfog formation. ",
      "I will review your updated spot forecast in the ",
      "morning and send PB Piedmont model results."
    ))
  }
  
  paste0(
    "Based on the weather parameters in your spot ",
    "forecast, conditions do not appear favorable ",
    "for superfog formation. However, if you would ",
    "like a PB Piedmont run to evaluate nighttime ",
    "smoke dispersion, please let me know."
  )
}


# 4. Assemble the email ----

build_smoke_email <- function(email_data) {
  
  stopifnot(
    !is.null(email_data$burn_name),
    !is.null(email_data$report_url)
  )
  
  region8 <- identical(email_data$region, "08")
  
  pb_url <- email_data$pb_map_url
  
  has_pb <- region8 &&
    identical(email_data$superfog, "Yes") &&
    !is.null(pb_url) &&
    length(pb_url) == 1L &&
    !is.na(pb_url) &&
    nzchar(pb_url)
  
  intro <- smoke_email_intro(
    burn_name = email_data$burn_name,
    report_url = email_data$report_url,
    include_pb = has_pb,
    pb_url = pb_url
  )
  
  sections <- c(
    "Good morning,",
    intro,
    smoke_email_pm_text()
  )
  
  if (region8) {
    sections <- c(
      sections,
      smoke_email_superfog_text(
        superfog = email_data$superfog,
        pb_url = if (has_pb) pb_url else NULL
      )
    )
  }
  
  sections <- c(
    sections,
    "Please let me know if you have any questions or need anything else.",
    "Jeremy"
  )
  
  list(
    subject = paste0(
      "Smoke Modeling Results – ",
      email_data$burn_name
    ),
    body = paste(sections, collapse = "\n\n")
  )
}


# 5. Build formatted HTML email ----

build_smoke_email_html <- function(email_data) {
  
  # Reuse the approved plain-text template.
  email <- build_smoke_email(email_data)
  
  # Escape text before introducing HTML markup.
  escape_html <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }
  
  html <- escape_html(email$body)
  
  # Format PM2.5 with a true HTML subscript.
  html <- gsub(
    "PM2.5",
    "PM<sub>2.5</sub>",
    html,
    fixed = TRUE
  )
  
  # Construct a blue, underlined hyperlink.
  make_link <- function(url, label) {
    paste0(
      '<a href="', escape_html(url),
      '" style="color:#0563C1;text-decoration:underline;">',
      escape_html(label),
      '</a>'
    )
  }
  
  report_url <- email_data$report_url
  pb_url <- email_data$pb_map_url
  
  region8 <- identical(email_data$region, "08")
  
  has_pb <- region8 &&
    identical(email_data$superfog, "Yes") &&
    !is.null(pb_url) &&
    length(pb_url) == 1L &&
    !is.na(pb_url) &&
    nzchar(pb_url)
  
  if (has_pb) {
    
    report_text <- paste0(
      "Smoke Report with BlueSky Playground: ",
      report_url
    )
    
    pb_text <- paste0(
      "PB Piedmont – Nighttime Smoke and Superfog: ",
      pb_url
    )
    
    html <- sub(
      escape_html(report_text),
      make_link(report_url, "Smoke Report with BlueSky Playground"),
      html,
      fixed = TRUE
    )
    
    html <- sub(
      escape_html(pb_text),
      make_link(pb_url, "PB Piedmont – Nighttime Smoke and Superfog"),
      html,
      fixed = TRUE
    )
    
  } else {
    
    report_text <- paste0(
      "Smoke Report. ",
      report_url
    )
    
    html <- sub(
      escape_html(report_text),
      make_link(report_url, "Smoke Report"),
      html,
      fixed = TRUE
    )
  }
  
  # Preserve the existing paragraph breaks.
  html <- gsub(
    "\n\n",
    "</p><p>",
    html,
    fixed = TRUE
  )
  
  # Preserve the line break between the two model links.
  html <- gsub(
    "\n",
    "<br>",
    html,
    fixed = TRUE
  )
  
  html <- paste0(
    '<div style="font-family:Arial,sans-serif;',
    'font-size:11pt;color:#222222;line-height:1.5;">',
    '<p>', html, '</p>',
    '</div>'
  )
  
  list(
    subject = email$subject,
    body_html = html
  )
}

