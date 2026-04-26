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
                     phone = c("828-244-4751", "470-882-9854", "828-337-2323", "202-494-5127", "888-888-8888")) %>% 
  arrange(name)


# helper functions ----------------------------------------------

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
  
  paste0(
    "https://",
    owner,
    ".github.io/",
    repo,
    "/reports/",
    report_filename
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
  title = 'Prescribed Fire Smoke Report',
  titlePanel('Prescribed Fire Smoke Report'),
  br(), br(),
  sidebarLayout(
    ### sidebar
    sidebarPanel(
      
      # choices = c("01", "02", "03", "04", "05", "06", "08", "09")
      selectInput("REGION", "USFS Region", choices = c("02", "04", "06", "08", "09")),
      uiOutput("FORECAST_AQI"),
      uiOutput("SUPERFOG_SCREEN"),
      uiOutput("SUPERFOG_ZIP_UPLOAD"),
      # selectizeInput(
      #   "FOREST",
      #   'Choose your USFS unit',
      #   choices = nfs,
      #   options=list(
      #     placeholder='Begin typing',
      #     onInitialize = I('function() {
      #                                   this.setValue(""); }')
      #   )),
      uiOutput("FOREST"),
      textInput("BURN_NAME", "Name of burn unit"),
      textInput("RUN_ID", "Run ID from BlueSky Playground Dispersion Results page"),
      selectInput(
        "HOURLY_MAP_SELECT",
        "Include hourly smoke map?",
        choices = c("No", "Yes")
      ),
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
      downloadButton("report", "Download Smoke Report"),
      downloadButton("kmz", "Download Google Earth File")
    ),
    ### main panel
    mainPanel(
      ### text for selected burn 
      h2(textOutput('selected_unit')),
      h2(textOutput('selected_burn')),
      
      uiOutput("report_link_ui"),
      
      ### footer
      hr(),
      div(class='footer',
          p('This site will create an html report showing the estimated smoke dispersion from BlueSky Playground and recent ambient air quality surrounding the proposed burn. Input the requested information to the left and click download to generate the report. Additionally, you can download the Google Earth ouput showing all of the dispersion results from BlueSky Playground.'),
          p('Questions or comments can be sent to:',
            a('jeremy.ash@usda.gov',
              href='jeremy.ash@usda.gov',
              target='_blank')
          ),
          div(style='height:50px')
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
      input$SUPERFOG_ZIP
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
  
  # REGION 08 only AQI and superfog inputs
  output$FORECAST_AQI <- renderUI({
    if(input$REGION == "08"){
      selectInput("FORECAST_AQI_SELECT", "Forecasted AQI downwind of ignition", choices = c("Good",
                                                                                            "Moderate",
                                                                                            "USG",
                                                                                            "Unhealthy",
                                                                                            "Very Unhealthy",
                                                                                            "Hazardous"))
    } else {
      NULL
    }
  })
  
  
  output$SUPERFOG_SCREEN <- renderUI({
    if(input$REGION == "08"){
      selectInput("SUPERFOG_SCREEN_SELECT", "Potential for superfog formation?", choices = c("No",
                                                                                             "Yes"))
    } else {
      NULL
    }
  })
  
  # REGION 08 + superfog only PB Piedmont upload
  output$SUPERFOG_ZIP_UPLOAD <- renderUI({
    if (
      isTRUE(input$REGION == "08") &&
      !is.null(input$SUPERFOG_SCREEN_SELECT) &&
      input$SUPERFOG_SCREEN_SELECT == "Yes"
    ) {
      fileInput(
        "SUPERFOG_ZIP",
        "Upload PB Piedmont hourly_output.zip here",
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
        
        superfog_zip_path <- NULL
        
        if (
          isTRUE(input$REGION == "08") &&
          !is.null(input$SUPERFOG_SCREEN_SELECT) &&
          input$SUPERFOG_SCREEN_SELECT == "Yes"
        ) {
          validate(
            need(
              !is.null(input$SUPERFOG_ZIP),
              "Please upload PB Piedmont hourly_output.zip before generating the report."
            )
          )
          
          validate(
            need(
              grepl("\\.zip$", input$SUPERFOG_ZIP$name, ignore.case = TRUE),
              "The PB Piedmont upload must be a .zip file."
            )
          )
          
          superfog_zip_path <- file.path(
            tempdir(),
            paste0(
              "pb_piedmont_hourly_output_",
              format(Sys.time(), "%Y%m%d%H%M%S"),
              ".zip"
            )
          )
          
          file.copy(
            from = input$SUPERFOG_ZIP$datapath,
            to = superfog_zip_path,
            overwrite = TRUE
          )
        }
        
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
          SUPERFOG_ZIP_PATH = superfog_zip_path
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
        
        incProgress(0.15, detail = "Loading burn information and BlueSky results")
        
        incProgress(0.15, detail = "Preparing air quality and monitoring data")
        
        incProgress(0.10, detail = "Building dispersion map layers")
        
        incProgress(0.30, detail = "Rendering full report (maps and analysis)")
        
        rmarkdown::render(
          "smoke_template_shiny_dev.Rmd",
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