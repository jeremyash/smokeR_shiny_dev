require(shiny)
require(tidyverse)
require(rmarkdown)
require(knitr)
require(terra)
require(viridis)
require(RCurl)
require(rjson)
require(lubridate)
require(googlesheets4)
library(gh)
library(base64enc)
library(fs)


# load data ----------------------------------------------

# unit names
nfs <-readRDS('usfs_unit_list.RDS')

# author contact info
aq_contact <- tibble(name = c("Jeremy Ash", "Melanie Pitrolo", "Gisele Majidi-Weese", "Jacob Deal", "Alexia Prosperi"),
                     
                     email = c("jeremy.ash@usda.gov", "melanie.pitrolo@usda.gov", "ghazal.majidi-weese@usda.gov", "jacob.deal@usda.gov", "alexia.prosperi@usda.gov"),
                     phone = c("828-244-4751", "470-882-9854", "828-337-2323", "202-494-5127", NA)) %>% 
  arrange(name)


# helper functions ----------------------------------------------

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

upload_report_to_github_pages <- function(
    local_file,
    owner,
    repo,
    branch = "main",
    pages_dir = "docs/reports",
    report_filename
) {
  token <- get_github_pat()
  
  if (identical(token, "")) {
    stop("GITHUB_PAT is not set.")
  }
  
  github_path <- file.path(pages_dir, report_filename)
  
  gh::gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = repo,
    path = github_path,
    message = paste("Add smoke report", report_filename),
    content = base64enc::base64encode(local_file),
    branch = branch,
    .token = token
  )
  
  pages_url_dir <- pages_dir %>%
    stringr::str_replace("^docs/?", "") %>%
    stringr::str_replace_all("^/|/$", "")
  
  if (nzchar(pages_url_dir)) {
    paste0(
      "https://",
      owner,
      ".github.io/",
      repo,
      "/",
      pages_url_dir,
      "/",
      report_filename
    )
  } else {
    paste0(
      "https://",
      owner,
      ".github.io/",
      repo,
      "/",
      report_filename
    )
  }
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

###################################################################
ui <- fluidPage(
  tags$title("Prescribed Fire Smoke Report"),
  
  tags$head(
    
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "512x512",
      href = "favicon_512x512_rounded.png?v=120"
    ),
    tags$link(
      rel = "shortcut icon",
      type = "image/png",
      href = "favicon_512x512_rounded.png?v=120"
    ),
    
    # Optional Chrome theme color
    tags$meta(
      name = "theme-color",
      content = "#032B5B"
    ),
    
    # App styling
    tags$style(HTML("
      body {
        background: #f3f6f8;
        color: #1f2d3a;
      }

      .container-fluid {
        max-width: 1500px;
      }

      .app-title-banner {
        display: flex;
        align-items: center;
        gap: 18px;
        margin: 18px 0 24px 0;
        padding: 20px 24px;
        background: linear-gradient(90deg, #032B5B 0%, #0B3D73 100%);
        border-radius: 16px;
        border-left: 8px solid #F28C28;
        box-shadow: 0 3px 12px rgba(0,0,0,0.20);
      }

      .app-title-banner img {
        width: 72px;
        height: 72px;
        border-radius: 18px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.30);
        flex-shrink: 0;
      }

      .app-title-text {
        display: flex;
        flex-direction: column;
      }

      .app-title-main {
        color: white;
        font-size: 34px;
        font-weight: 800;
        line-height: 1.05;
        margin: 0;
      }

      .app-title-sub {
        color: rgba(255,255,255,0.84);
        font-size: 16px;
        margin-top: 7px;
        letter-spacing: 0.3px;
      }

      .sidebar-card, .main-card {
        background: white;
        border-radius: 12px;
        border: 1px solid #d8e0e6;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }

      .sidebar-card {
        padding: 18px 18px 10px 18px;
      }

      .main-card {
        padding: 22px 24px;
      }

      .app-section-title {
        margin: 18px 0 12px 0;
        padding: 8px 10px;
        background: #EAF1F5;
        color: #032B5B;
        border-left: 6px solid #F28C28;
        border-radius: 6px;
        font-weight: 800;
        font-size: 15px;
        text-transform: uppercase;
        letter-spacing: 0.4px;
      }

      .app-section-title:first-child {
        margin-top: 0;
      }

      .form-group label {
        color: #032B5B;
        font-weight: 700;
      }

      .form-control, .selectize-input {
        border-radius: 6px;
        border-color: #b8c5cf;
      }

      .btn {
        border-radius: 8px;
        font-weight: 700;
      }

      #report {
        background: #F28C28;
        border-color: #D8791F;
        color: white;
        margin-right: 8px;
        margin-bottom: 8px;
      }

      #kmz {
        background: #032B5B;
        border-color: #032B5B;
        color: white;
        margin-bottom: 8px;
      }

      .selected-summary {
        padding: 14px 16px;
        margin-bottom: 18px;
        background: linear-gradient(90deg, #EAF1F5, #ffffff);
        border-left: 6px solid #F28C28;
        border-radius: 8px;
      }

      .selected-summary h2 {
        margin: 0 0 5px 0;
        color: #032B5B;
        font-size: 24px;
        font-weight: 800;
      }

      .app-help-box {
        margin-top: 22px;
        padding: 16px 18px;
        background: #F8FAFB;
        border: 1px solid #d8e0e6;
        border-radius: 10px;
        color: #334;
      }

      .app-help-box a {
        color: #032B5B;
        font-weight: 700;
      }

      /* Keep the report link/status panel visible while long inputs scroll. */
      .app-layout-row {
        align-items: flex-start;
      }

      .app-scroll-sidebar {
        max-height: calc(100vh - 150px);
        overflow-y: auto;
        padding-right: 8px;
      }

      .app-fixed-main {
        position: sticky;
        top: 16px;
      }

      @media (max-width: 991px) {
        .app-scroll-sidebar {
          max-height: none;
          overflow-y: visible;
          padding-right: 0;
        }

        .app-fixed-main {
          position: static;
        }
      }
    "))
  ),
  
  # elements
  tags$div(
    class = "app-title-banner",
    tags$img(src = "favicon_512x512_rounded.png", alt = "Smoke Report Icon"),
    tags$div(
      class = "app-title-text",
      tags$div(class = "app-title-main", "Prescribed Fire Smoke Report Generator"),
      tags$div(class = "app-title-sub", "BlueSky Plaground dispersion results and air quality information")
    )
  ),
  
  fluidRow(
    class = "app-layout-row",
    
    ### sidebar
    column(
      width = 4,
      tags$div(
        class = "sidebar-card app-scroll-sidebar",
        
        tags$div(class = "app-section-title", "Burn information"),
        selectInput("REGION", "USFS Region", choices = c("02", "04", "06", "08", "09")),
        uiOutput("FOREST"),
        textInput("BURN_NAME", "Name of burn unit"),
        
        uiOutput("REGION_08_OPTIONS"),
        uiOutput("PB_HOURLY_UPLOAD"),
        
        tags$div(class = "app-section-title", "Model output"),
        textInput("RUN_ID", "Run ID from BlueSky Playground Dispersion Results page"),
        selectInput(
          "HOURLY_MAP_SELECT",
          "Include hourly smoke map?",
          choices = c("No", "Yes")
        ),
        
        tags$div(class = "app-section-title", "Contact information"),
        selectizeInput(
          "AUTHOR",
          "Your name",
          choices = c("Type your name or select from the list" = "", aq_contact$name),
          selected = "",
          options = list(
            create = TRUE,
            placeholder = "Type your name or select from the list"
          )
        ),
        textInput("EMAIL", "Your email (optional)"),
        textInput("PHONE", "Your phone number (optional)"),
        
        tags$div(class = "app-section-title", "Downloads"),
        downloadButton("report", "Download Smoke Report"),
        downloadButton("kmz", "Download Google Earth File")
      )
    ),
    
    ### main panel
    column(
      width = 8,
      tags$div(
        class = "main-card app-fixed-main",
        
        tags$div(
          class = "selected-summary",
          h2(textOutput('selected_unit')),
          h2(textOutput('selected_burn'))
        ),
        
        uiOutput("report_link_ui"),
        
        tags$div(
          class = "app-help-box",
          p('This site will create an HTML report showing the estimated smoke dispersion from BlueSky Playground and recent ambient air quality surrounding the proposed burn. Input the requested information to the left and click download to generate the report. Additionally, you can download the Google Earth output showing all of the dispersion results from BlueSky Playground.'),
          p('Questions or comments can be sent to:',
            a('jeremy.ash@usda.gov',
              href = 'mailto:jeremy.ash@usda.gov',
              target = '_blank')
          )
        )
      )
    )
  )
)
###################################################################


###################################################################
server <- function(input, output, session) {
  
  # github pages report link
  report_link <- reactiveVal(NULL)
  
  observeEvent(
    list(
      input$REGION,
      input$FOREST,
      input$BURN_NAME,
      input$RUN_ID,
      input$HOURLY_MAP_SELECT,
      input$AUTHOR,
      input$EMAIL,
      input$PHONE,
      input$FORECAST_AQI_SELECT,
      input$SUPERFOG_SCREEN_SELECT,
      input$PB_HOURLY_ZIP
    ),
    {
      report_link(NULL)
    },
    ignoreInit = TRUE
  )
  
  # reactive handler to capture unit name
  r_unit <- reactive({
    req(input$FOREST) # require it not to be empty
    as.character(input$FOREST)
  })
  
  # reactive handler to capture burn name
  r_burn <- reactive({
    req(input$BURN_NAME) # require it not to be empty
    as.character(input$BURN_NAME)
  })
  
  # reactive handler to capture run_id
  r_id <- reactive({
    req(input$RUN_ID) # require it not to be empty
    as.character(input$RUN_ID)
  })
  
  # subset Forest names based on Region
  output$FOREST <- renderUI({
    selectInput("FOREST", "Forest:", choices = nfs[nfs$region==input$REGION,"forests"])
  })
  
  
  # render text for unit
  output$selected_unit <- renderText({
    paste(input$FOREST)
  })
  
  # render text for burn name
  output$selected_burn <- renderText({
    paste(input$BURN_NAME)
  })
  
  # render text for run id
  output$run_id <- renderText({
    paste(input$RUN_ID)
  })
  
  # updating author and contact information
  observeEvent(input$AUTHOR, {
    if (is.null(input$AUTHOR) || input$AUTHOR == "") {
      updateTextInput(session, "EMAIL", value = "")
      updateTextInput(session, "PHONE", value = "")
      return()
    }
    
    matched_contact <- aq_contact %>%
      filter(str_to_lower(name) == str_to_lower(input$AUTHOR))
    
    if (nrow(matched_contact) == 1) {
      updateTextInput(session, "EMAIL", value = matched_contact$email[1])
      updateTextInput(session, "PHONE", value = matched_contact$phone[1])
    } else {
      updateTextInput(session, "EMAIL", value = "")
      updateTextInput(session, "PHONE", value = "")
    }
  }, ignoreInit = FALSE)
  
  # download file names
  forest_burn <- reactive({
    req(input$BURN_NAME)
    paste(input$BURN_NAME) %>%
      str_replace_all("[[:punct:]]", "") %>%
      str_squish() %>%
      str_replace_all(" ", "_")
  })
  
  yearmonday <- str_replace_all(Sys.Date(), "-", "")
  
  smoke_report_title <- reactive({
    paste(yearmonday,
          "_",
          forest_burn(), "_smoke_report.html", sep = "")
  })
  
  kmz_file <- reactive({
    paste(yearmonday,
          "_",
          forest_burn(), "_bsky_dispersion.kmz", sep = "")
  })
  
  # REGION 08 only AQI and superfog inputs.
  # This section is hidden for all non-08 regions. Keep this separate from
  # PB_HOURLY_UPLOAD so choosing "Yes" does not cause the selectInput to reset.
  output$REGION_08_OPTIONS <- renderUI({
    if (is.null(input$REGION) || input$REGION != "08") {
      return(NULL)
    }
    
    tagList(
      tags$div(class = "app-section-title", "Region 8 options"),
      selectInput(
        "FORECAST_AQI_SELECT",
        "Forecasted AQI downwind of ignition",
        choices = c(
          "Good",
          "Moderate",
          "USG",
          "Unhealthy",
          "Very Unhealthy",
          "Hazardous"
        ),
        selected = isolate(input$FORECAST_AQI_SELECT %||% "Good")
      ),
      selectInput(
        "SUPERFOG_SCREEN_SELECT",
        "Potential for superfog formation?",
        choices = c("No", "Yes"),
        selected = isolate(input$SUPERFOG_SCREEN_SELECT %||% "No")
      )
    )
  })
  
  # Optional PB Piedmont hourly output upload. This is intentionally separate
  # from REGION_08_OPTIONS so it can appear/disappear without recreating the
  # superfog selectInput and resetting it back to "No".
  output$PB_HOURLY_UPLOAD <- renderUI({
    req(input$REGION)
    
    if (
      input$REGION == "08" &&
      !is.null(input$SUPERFOG_SCREEN_SELECT) &&
      input$SUPERFOG_SCREEN_SELECT == "Yes"
    ) {
      fileInput(
        "PB_HOURLY_ZIP",
        "Upload PB Piedmont hourly_output.zip here (optional)",
        accept = c(".zip", "application/zip", "application/x-zip-compressed")
      )
    } else {
      NULL
    }
  })
  
  
  
  ### download handler for report
  output$report <- downloadHandler(
    filename = function() {
      smoke_report_title()
    },
    content = function(file) {
      
      withProgress(message = "Generating smoke report...", value = 0, {
        
        incProgress(0.10, detail = "Preparing report parameters")
        
        params_ls <- list(
          BURN_NAME = input$BURN_NAME,
          FOREST = input$FOREST,
          REGION = input$REGION,
          AUTHOR = input$AUTHOR,
          EMAIL = input$EMAIL,
          PHONE = input$PHONE,
          RUN_ID = input$RUN_ID,
          HOURLY_MAP_SELECT = input$HOURLY_MAP_SELECT,
          FORECAST_AQI_SELECT = if (input$REGION == "08") input$FORECAST_AQI_SELECT else NULL,
          SUPERFOG_SCREEN_SELECT = if (input$REGION == "08") input$SUPERFOG_SCREEN_SELECT else NULL,
          PB_MAP_URL = NULL
        )
        
        burn_name_for_file <- input$BURN_NAME %>%
          as.character() %>%
          safe_filename()
        
        forest_for_file <- input$FOREST %>%
          as.character() %>%
          safe_filename()
        
        report_filename <- paste0(
          format(Sys.time(), "%Y%m%d-%H%M%S"),
          "-",
          forest_for_file,
          "-",
          burn_name_for_file,
          ".html"
        )
        
        rendered_file <- file.path(tempdir(), report_filename)
        
        # If a PB Piedmont hourly ZIP was uploaded, render it as a separate
        # standalone HTML file and upload that heavier map to its own GitHub Pages directory.
        pb_map_url <- NULL
        pb_zip_available <- (
          input$REGION == "08" &&
            !is.null(input$SUPERFOG_SCREEN_SELECT) &&
            input$SUPERFOG_SCREEN_SELECT == "Yes" &&
            !is.null(input$PB_HOURLY_ZIP) &&
            !is.null(input$PB_HOURLY_ZIP$datapath) &&
            file.exists(input$PB_HOURLY_ZIP$datapath)
        )
        
        if (pb_zip_available) {
          incProgress(0.08, detail = "Rendering PB Piedmont particle map")
          
          pb_zip_copy <- file.path(
            tempdir(),
            paste0(tools::file_path_sans_ext(basename(input$PB_HOURLY_ZIP$name)), "-", format(Sys.time(), "%Y%m%d%H%M%S"), ".zip")
          )
          file.copy(input$PB_HOURLY_ZIP$datapath, pb_zip_copy, overwrite = TRUE)
          
          pb_map_filename <- paste0(
            format(Sys.time(), "%Y%m%d-%H%M%S"),
            "-",
            forest_for_file,
            "-",
            burn_name_for_file,
            "-pb-piedmont-map.html"
          )
          
          pb_rendered_file <- file.path(tempdir(), pb_map_filename)
          
          rmarkdown::render(
            "pb_piedmont_particle_map.Rmd",
            output_file = pb_rendered_file,
            params = list(
              BURN_NAME = input$BURN_NAME,
              FOREST = input$FOREST,
              RUN_ID = input$RUN_ID,
              PB_HOURLY_ZIP = pb_zip_copy,
              BURN_DATE = NA
            ),
            envir = new.env(parent = globalenv())
          )
          
          incProgress(0.07, detail = "Uploading PB Piedmont particle map")
          
          pb_map_url <- tryCatch({
            upload_report_to_github_pages(
              local_file = pb_rendered_file,
              owner = "jeremyash",
              repo = "smoke_reports",
              branch = "main",
              pages_dir = "docs/pb-piedmont",
              report_filename = pb_map_filename
            )
          }, error = function(e) {
            message("PB Piedmont map upload failed: ", conditionMessage(e))
            NULL
          })
        }
        
        params_ls$PB_MAP_URL <- pb_map_url
        
        incProgress(0.15, detail = "Loading burn information and BlueSky results")
        
        incProgress(0.15, detail = "Preparing air quality and monitoring data")
        
        incProgress(0.10, detail = "Building dispersion map layers")
        
        incProgress(0.30, detail = "Rendering full report (maps and analysis)")
        
        rmarkdown::render(
          "smoke_template_shiny_dev_external_pb_map.Rmd",
          output_file = rendered_file,
          params = params_ls,
          envir = new.env(parent = globalenv())
        )
        
        incProgress(0.10, detail = "Uploading report to GitHub Pages")
        
        # SAFE GitHub upload
        github_success <- tryCatch({
          
          report_url <- upload_report_to_github_pages(
            local_file = rendered_file,
            owner = "jeremyash",
            repo = "smoke_reports",
            branch = "main",
            pages_dir = "docs/reports",
            report_filename = report_filename
          )
          
          report_link(report_url)
          
          update_index_page(
            owner = "jeremyash",
            repo = "smoke_reports",
            report_filename = report_filename,
            report_label = paste(
              input$FOREST,
              "-",
              input$BURN_NAME,
              "-",
              format(Sys.time(), "%Y-%m-%d %H:%M")
            ),
            branch = "main"
          )
          
          message("Report uploaded and index updated: ", report_url)
          
          TRUE
          
        }, error = function(e) {
          message("GitHub upload or index update failed: ", conditionMessage(e))
          report_link(NULL)
          FALSE
        })
        
        incProgress(0.10, detail = "Finalizing report download")
        
        file.copy(rendered_file, file, overwrite = TRUE)
        
      })
    }
  )
  
  ### link to the report on github pages
  output$report_link_ui <- renderUI({
    req(report_link())
    
    tags$div(
      style = "
      margin: 18px 0 24px 0;
      padding: 16px 18px;
      border: 2px solid #4CAF50;
      border-radius: 8px;
      background: #f3fff3;
      font-size: 18px;
    ",
      
      tags$div(
        style = "font-weight:700; font-size:22px; margin-bottom:8px;",
        "Report available online"
      ),
      
      tags$div(
        style = "font-size:14px; color:#666; margin-bottom:10px;",
        "Note: the link may take 5–20 seconds to become available."
      ),
      
      tags$div(
        style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap;",
        tags$a(
          href = report_link(),
          target = "_blank",
          style = "word-break:break-all; font-size:18px;",
          report_link()
        ),
        tags$button(
          type = "button",
          class = "btn btn-success",
          onclick = sprintf(
            "navigator.clipboard.writeText('%s'); this.innerText='Copied!'; setTimeout(() => this.innerText='Copy URL', 1500);",
            report_link()
          ),
          "Copy URL"
        )
      )
    )
  })
  
  
  ### download handler for kmz
  output$kmz <- downloadHandler(
    # set up file names for downloads
    filename = function() {
      kmz_file()
    },
    content = function(file) {
      # general dispersion results link
      bsky_link <- paste("https://tools.airfire.org/playground/v3.5/dispersionresults.php?scenario_id=",
                         input$RUN_ID,
                         sep = "")
      
      # links for results output from both servers
      serv1_link <- paste("https://playground-1.airfire.org/bluesky-web-output/",
                          input$RUN_ID,
                          "-dispersion",
                          sep = "")
      
      serv2_link <- paste("https://playground-2.airfire.org/bluesky-web-output/",
                          input$RUN_ID,
                          "-dispersion",
                          sep = "")
      
      serv_links_ls <- list(serv1_link, serv2_link)
      names(serv_links_ls) <- c("serv1_link", 
                                "serv2_link")
      
      
      # get info on end time of simulation for each server
      date_info_1 <- if (url.exists(serv1_link)) {
        
        # pull end time from output.json
        end_time_val <- lubridate::as_datetime(fromJSON(file = paste(serv1_link, "/output.json", sep = ""))$runtime[["end"]])
        
        # create df
        end_time_df <- tibble(server = "serv1_link",
                              end_time = end_time_val)
        
      }else{
        # create df
        end_time_df <- tibble(server = NA,
                              end_time = NA)}
      
      
      
      date_info_2 <- if (url.exists(serv2_link)) {
        
        # pull end time from output.json
        end_time_val <- lubridate::as_datetime(fromJSON(file = paste(serv2_link, "/output.json", sep = ""))$runtime[["end"]])
        
        # create df
        end_time_df <- tibble(server = "serv2_link",
                              end_time = end_time_val)
        
      }else{
        # create df
        end_time_df <- tibble(server = NA,
                              end_time = NA)}
      
      # combine outputs, select most recent model and set results_link to correct url
      recent_server <- bind_rows(date_info_1, date_info_2) %>% 
        arrange(desc(end_time)) %>% 
        slice(1) %>% 
        pull(server)
      
      server_link <- serv_links_ls[[recent_server]]
      
      # smoke dispersion
      smoke_disp_link <- paste(server_link,
                               "/output/smoke_dispersion.kmz", sep = "")
      
      # download copy of google earth file into outlooks
      curl_download(smoke_disp_link, destfile = file)
      
      
    }
  )
}

###################################################################

### build it
shinyApp(ui=ui, server=server)