# LIBRARIES ----------------------------------------------

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
require(digest)
require(shinyjs)
require(here)


# LOAD DATA ----------------------------------------------

# source helper functions
source("R/helpers.R")
source("R/github_helpers.R")
source("R/log_helpers.R")
source("R/filename_helpers.R")
source("R/pb_helpers.R")
source("R/constants.R")

LOG_SHEET_URL <- get_log_sheet_url()

nfs_raw <- readRDS(
  here::here("smokeR_dev", "data", "usfs_unit_list.RDS")
)

if (!all(c("region", "forests") %in% names(nfs_raw))) {
  stop("usfs_unit_list.RDS must contain columns named 'region' and 'forests'.")
}

nfs <- data.frame(
  region = as.character(nfs_raw[["region"]]),
  forests = as.character(nfs_raw[["forests"]]),
  stringsAsFactors = FALSE
)

nfs <- nfs[
  !is.na(nfs$region) &
    !is.na(nfs$forests) &
    nzchar(nfs$region) &
    nzchar(nfs$forests),
  ,
  drop = FALSE
]

# Region 8 forests for standalone PB Piedmont map tab
r8_forests <- sort(unique(nfs$forests[nfs$region == "08"]))


# author contact info
aq_contact <- tibble(name = c("Jeremy Ash", "Melanie Pitrolo", "Gisele Majidi-Weese", "Jacob Deal", "Alexia Prosperi"),
                     
                     email = c("jeremy.ash@usda.gov", "melanie.pitrolo@usda.gov", "ghazal.majidi-weese@usda.gov", "jacob.deal@usda.gov", "alexia.prosperi@usda.gov"),
                     phone = c("828-244-4751", "470-882-9854", "828-337-2323", "202-494-5127", NA)) %>% 
  arrange(name)



# USER INTERFACE ----------------------------------------------

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  tags$title("Prescribed Fire Smoke Report"),
  
  # UI: Browser metadata and app styling --------------------
  tags$head(
    
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "app.css?v=1"
    ),
    
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
    
    tags$meta(
      name = "theme-color",
      content = "#032B5B"
    )
  ),
  
  # UI: App title banner ------------------------------------
  tags$div(
    class = "app-title-banner",
    tags$img(src = "favicon_512x512_rounded.png", alt = "Smoke Report Icon"),
    tags$div(
      class = "app-title-text",
      tags$div(class = "app-title-main", "Prescribed Fire Smoke Report Generator"),
      tags$div(class = "app-title-sub", "BlueSky Playground, PB Piedmont and Air Quality Information")
    )
  ),
  
  # UI: Main two-column layout ------------------------------
  fluidRow(
    class = "app-layout-row",
    
    # UI: Tool panel -----------------------------------------
    column(
      width = 5,
      tags$div(
        class = "sidebar-card app-scroll-sidebar",
        
        tabsetPanel(
          id = "tool_tab",
          
          # UI: Smoke Report tab ------------------------------------
          tabPanel(
            "Smoke Report",
            
            tags$div(class = "app-section-title", "Burn information"),
            selectInput(
              "REGION",
              "USFS Region",
              choices = REGION_CHOICES,
              selected = ""
            ),
            uiOutput("FOREST"),
            textInput("BURN_NAME", "Name of burn unit"),
            
            uiOutput("REGION_08_OPTIONS"),
            uiOutput("PB_HOURLY_UPLOAD"),
            
            tags$div(class = "app-section-title", "Model output"),
            textInput("RUN_ID", "Run ID from BlueSky Playground Dispersion Results page"),
            selectInput(
              "HOURLY_MAP_SELECT",
              "Include hourly smoke map?",
              choices = YES_NO_CHOICES
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
            uiOutput("report_required_msg"),
            downloadButton("report", "Download Smoke Report"),
            downloadButton("kmz", "Download Google Earth File")
          ),
          
          # UI: PB Piedmont Map tab ---------------------------------
          tabPanel(
            "PB Piedmont Map",
            
            tags$div(class = "app-section-title", "PB Piedmont map information"),
            selectizeInput(
              "PB_ONLY_FOREST",
              "Forest:",
              choices = c("Select a Region 8 forest" = "", r8_forests),
              selected = "",
              options = list(
                placeholder = "Select a Region 8 forest"
              )
            ),
            textInput("PB_ONLY_BURN_NAME", "Burn unit name"),
            
            tags$div(class = "app-section-title", "Burn location"),
            numericInput("PB_ONLY_LAT", "Latitude", value = NA, min = -90, max = 90, step = 0.0001),
            numericInput("PB_ONLY_LON", "Longitude", value = NA, min = -180, max = 180, step = 0.0001),
            
            tags$div(class = "app-section-title", "PB Piedmont output"),
            fileInput(
              "PB_ONLY_HOURLY_ZIP",
              "Upload PB Piedmont hourly_output.zip",
              accept = c(".zip", "application/zip", "application/x-zip-compressed")
            ),
            actionButton("create_pb_only_map", "Create PB Piedmont Map", class = "btn-primary")
          )
        )
      )
    ),
    
    # UI: Status and links panel ------------------------------
    column(
      width = 7,
      tags$div(
        class = "main-card app-fixed-main",
        
        tags$div(
          class = "selected-summary",
          h2(textOutput('selected_unit')),
          h2(textOutput('selected_burn'))
        ),
        
        uiOutput("report_link_ui"),
        uiOutput("pb_only_link_ui"),
        
        tags$div(
          class = "app-help-box",
          p('Use the Smoke Report tab to create a full HTML smoke report using BlueSky Playground output,  optional PB Piedmont mapping and ambient air quality monitoring data. Use the PB Piedmont Map tab to create only the standalone PB Piedmont map from an hourly_output.zip file.'),
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
# SERVER --------------------------------------------------

server <- function(input, output, session) {
  
  # SERVER: Shared reactive state ---------------------------
  report_link <- reactiveVal(NULL)
  pb_only_map_link <- reactiveVal(NULL)
  
  # SERVER: Required input validation -----------------------
  report_ready <- reactive({
    base_ready <- all(
      nzchar(input$REGION %||% ""),
      nzchar(input$FOREST %||% ""),
      nzchar(input$BURN_NAME %||% ""),
      nzchar(input$RUN_ID %||% ""),
      nzchar(input$HOURLY_MAP_SELECT %||% "")
    )
    
    r8_ready <- TRUE
    
    if (identical(input$REGION, "08")) {
      r8_ready <- all(
        nzchar(input$FORECAST_AQI_SELECT %||% ""),
        nzchar(input$SUPERFOG_SCREEN_SELECT %||% "")
      )
    }
    
    base_ready && r8_ready
  })
  
  observe({
    if (isTRUE(report_ready())) {
      shinyjs::enable("report")
      shinyjs::enable("kmz")
    } else {
      shinyjs::disable("report")
      shinyjs::disable("kmz")
    }
  })
  
  output$report_required_msg <- renderUI({
    missing <- c()
    
    if (!nzchar(input$REGION %||% "")) missing <- c(missing, "USFS Region")
    if (!nzchar(input$FOREST %||% "")) missing <- c(missing, "Forest")
    if (!nzchar(input$BURN_NAME %||% "")) missing <- c(missing, "Burn unit name")
    if (!nzchar(input$RUN_ID %||% "")) missing <- c(missing, "BlueSky Playground Run ID")
    if (identical(input$REGION, "08")) {
      if (!nzchar(input$FORECAST_AQI_SELECT %||% "")) missing <- c(missing, "Forecasted AQI")
      if (!nzchar(input$SUPERFOG_SCREEN_SELECT %||% "")) missing <- c(missing, "Superfog selection")
    }
    
    if (length(missing) == 0) {
      return(NULL)
    }
    
    tags$div(
      style = "
      margin-bottom: 10px;
      padding: 10px 12px;
      background: #fff7e6;
      border-left: 5px solid #F28C28;
      border-radius: 6px;
      font-size: 14px;
      color: #4a3a1a;
    ",
      tags$strong("Required before downloading: "),
      paste(missing, collapse = ", ")
    )
  })
  
  # SERVER: Reset links when inputs change ------------------
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
  
  observeEvent(
    list(
      input$PB_ONLY_FOREST,
      input$PB_ONLY_BURN_NAME,
      input$PB_ONLY_LAT,
      input$PB_ONLY_LON,
      input$PB_ONLY_HOURLY_ZIP
    ),
    {
      pb_only_map_link(NULL)
    },
    ignoreInit = TRUE
  )
  
  
  # SERVER: Forest selector ---------------------------------
  output$FOREST <- renderUI({
    req(nzchar(input$REGION))
    
    forest_choices <- nfs %>%
      dplyr::filter(region == input$REGION) %>%
      dplyr::pull(forests)
    
    selectizeInput(
      "FOREST",
      "Forest:",
      choices = c("Select a forest" = "", forest_choices),
      selected = "",
      options = list(
        placeholder = "Select a forest"
      )
    )
  })
  
  
  # SERVER: Selected summary outputs ------------------------
  output$selected_unit <- renderText({
    req(input$FOREST)
    input$FOREST
  })
  
  # render text for burn name
  output$selected_burn <- renderText({
    req(input$BURN_NAME)
    input$BURN_NAME
  })
  
  # render text for run id
  output$run_id <- renderText({
    paste(input$RUN_ID)
  })
  
  # SERVER: Author contact autofill -------------------------
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
  
  
  # SERVER: Download filenames ------------------------------
  smoke_report_title <- reactive({
    req(input$BURN_NAME, input$FOREST)
    
    make_report_filename(
      burn_name = input$BURN_NAME,
      forest = input$FOREST
    )
  })
  
  kmz_file <- reactive({
    req(input$BURN_NAME, input$FOREST)
    
    make_kmz_filename(
      burn_name = input$BURN_NAME,
      forest = input$FOREST
    )
  })
  
  # SERVER: Region 8 conditional inputs ---------------------
  output$REGION_08_OPTIONS <- renderUI({
    if (is.null(input$REGION) || input$REGION != "08") {
      return(NULL)
    }
    
    tagList(
      tags$div(class = "app-section-title", "Region 8 options"),
      selectInput(
        "FORECAST_AQI_SELECT",
        "Forecasted AQI downwind of ignition",
        choices = AQI_CHOICES,
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
  
  
  # SERVER: Standalone PB Piedmont map workflow -------------
  observeEvent(input$create_pb_only_map, {
    req(input$PB_ONLY_FOREST)
    req(input$PB_ONLY_BURN_NAME)
    req(input$PB_ONLY_LAT)
    req(input$PB_ONLY_LON)
    req(input$PB_ONLY_HOURLY_ZIP)
    
    pb_only_map_link(NULL)
    
    withProgress(message = "Creating PB Piedmont map...", value = 0, {
      
      incProgress(0.15, detail = "Preparing PB Piedmont map parameters")
      
      pb_result <- create_pb_piedmont_map(
        burn_name = input$PB_ONLY_BURN_NAME,
        forest = input$PB_ONLY_FOREST,
        run_id = NA,
        pb_zip_datapath = input$PB_ONLY_HOURLY_ZIP$datapath,
        pb_zip_name = input$PB_ONLY_HOURLY_ZIP$name,
        burn_lat = input$PB_ONLY_LAT,
        burn_lon = input$PB_ONLY_LON
      )
      
      pb_url <- pb_result$url
      
      pb_only_map_link(pb_url)
      
      tryCatch(
        {
          append_smoke_app_log(
            sheet_url = LOG_SHEET_URL,
            report_type = "Standalone PB Piedmont Map",
            region = "08",
            forest = input$PB_ONLY_FOREST,
            burn_name = input$PB_ONLY_BURN_NAME,
            burn_date = Sys.Date(),
            date_issued = Sys.Date(),
            lat = input$PB_ONLY_LAT,
            lon = input$PB_ONLY_LON,
            pb_map_url = pb_url
          )
          message("PB Piedmont map logged successfully")
          
        },
        error = function(e) {
          message("Standalone PB Piedmont log failed: ", conditionMessage(e))
        }
      )
      
      
      incProgress(0.05, detail = "PB Piedmont map ready")
    })
  })
  
  # SERVER: Smoke report workflow ---------------------------
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
          AUTHOR = {
            x <- input$AUTHOR %||% ""
            if (is.na(x)) "" else trimws(x)
          },
          EMAIL = {
            x <- input$EMAIL %||% ""
            if (is.na(x)) "" else trimws(x)
          },
          PHONE = {
            x <- input$PHONE %||% ""
            if (is.na(x)) "" else trimws(x)
          },
          RUN_ID = input$RUN_ID,
          HOURLY_MAP_SELECT = input$HOURLY_MAP_SELECT,
          FORECAST_AQI_SELECT = if (input$REGION == "08") input$FORECAST_AQI_SELECT else NULL,
          SUPERFOG_SCREEN_SELECT = if (input$REGION == "08") input$SUPERFOG_SCREEN_SELECT else NULL,
          LOG_SHEET_URL = get_log_sheet_url(),
          REPORT_URL = NULL,
          PB_MAP_URL = NULL
        )
        
        report_filename <- make_report_filename(
          burn_name = input$BURN_NAME,
          forest = input$FOREST
        )
        
        rendered_file <- file.path(tempdir(), report_filename)
        
        report_url <- make_github_pages_url(
          owner = APP_OWNER,
          repo = APP_REPO,
          pages_dir = REPORT_PAGES_DIR,
          report_filename = report_filename
        )
        
        params_ls$REPORT_URL <- report_url
        
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
          incProgress(0.08, detail = "Rendering and uploading PB Piedmont map")
          
          pb_map_url <- tryCatch({
            pb_result <- create_pb_piedmont_map(
              burn_name = input$BURN_NAME,
              forest = input$FOREST,
              run_id = input$RUN_ID,
              pb_zip_datapath = input$PB_HOURLY_ZIP$datapath,
              pb_zip_name = input$PB_HOURLY_ZIP$name,
              burn_lat = NA,
              burn_lon = NA
            )
            
            pb_result$url
          }, error = function(e) {
            message("PB Piedmont map render/upload failed: ", conditionMessage(e))
            NULL
          })
        }
        
        params_ls$PB_MAP_URL <- pb_map_url
        
        incProgress(0.15, detail = "Loading burn information and BlueSky results")
        
        incProgress(0.15, detail = "Preparing air quality and monitoring data")
        
        incProgress(0.10, detail = "Building dispersion map layers")
        
        incProgress(0.30, detail = "Rendering full report (maps and analysis)")
        
        rmarkdown::render(
          input = here::here(
            "smokeR_dev",
            "templates",
            "smoke_template_shiny_dev_external_pb_map.Rmd"
          ),
          output_file = rendered_file,
          params = params_ls,
          envir = new.env(parent = globalenv())
        )
        
        incProgress(0.10, detail = "Uploading report to GitHub Pages")
        
        # SAFE GitHub upload
        github_success <- tryCatch({
          
          report_url <- upload_report_to_github_pages(
            local_file = rendered_file,
            owner = APP_OWNER,
            repo = APP_REPO,
            branch = APP_BRANCH,
            pages_dir = PB_PAGES_DIR,
            report_filename = report_filename,
            commit_message = paste(
              input$BURN_NAME,
              "|",
              format(Sys.Date(), "%Y-%m-%d"),
              "|",
              input$FOREST
            )
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
  
  # SERVER: GitHub Pages link displays ----------------------
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
        "Note: the link may take 20-60 seconds to become available."
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
  
  
  ### link to standalone PB Piedmont map on GitHub Pages
  output$pb_only_link_ui <- renderUI({
    req(pb_only_map_link())
    
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
        "PB Piedmont map available online"
      ),
      tags$div(
        style = "font-size:14px; color:#666; margin-bottom:10px;",
        "Note: the link may take 20-60 seconds to become available."
      ),
      tags$div(
        style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap;",
        tags$a(
          href = pb_only_map_link(),
          target = "_blank",
          style = "word-break:break-all; font-size:18px;",
          pb_only_map_link()
        ),
        tags$button(
          type = "button",
          class = "btn btn-success",
          onclick = sprintf(
            "navigator.clipboard.writeText('%s'); this.innerText='Copied!'; setTimeout(() => this.innerText='Copy URL', 1500);",
            pb_only_map_link()
          ),
          "Copy URL"
        )
      )
    )
  })
  
  
  # SERVER: Google Earth KMZ workflow -----------------------
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

# RUN APP -------------------------------------------------

shinyApp(ui=ui, server=server)