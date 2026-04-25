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


# helper functions ----------------------------------------------

safe_filename <- function(x) {
  x %>%
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
  token <- Sys.getenv("GITHUB_PAT")
  
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
  token <- Sys.getenv("GITHUB_PAT")
  
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
    "    <li><a href='reports/",
    report_filename,
    "' target='_blank'>",
    htmltools::htmlEscape(report_label),
    "</a></li>\n"
  )
  
  index_html <- sub(
    "  </ul>",
    paste0(new_entry, "  </ul>"),
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

###################################################################
ui <- fluidPage(
  title = 'Prescribed Fire Smoke Report',
  titlePanel('Prescribed Fire Smoke Report'),
  br(),br(),
  sidebarLayout(
    ### sidebar
    sidebarPanel(

      # choices = c("01", "02", "03", "04", "05", "06", "08", "09")
      selectInput("REGION", "USFS Region", choices = c("02", "04", "06", "08", "09")),
      uiOutput("FORECAST_AQI"),
      uiOutput("SUPERFOG_SCREEN"),
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
        textInput("AUTHOR", "Your name"),
        textInput("EMAIL", "Your email (optional)"),
        textInput("PHONE", "Your phone number (optional)"),
     downloadButton("report", "Download Smoke Report"),
     uiOutput("report_link_ui"),
     downloadButton("kmz", "Download Google Earth File")
    ),
    ### main panel
    mainPanel(
      ### text for selected burn 
      h2(textOutput('selected_unit')),
      h2(textOutput('selected_burn')),
      ### link to BSKy dispersion results
      
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
server <- function(input, output) {
  
  # github pages report link
  report_link <- reactiveVal(NULL)
  
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
  
  
  
  ### download handler for report
  output$report <- downloadHandler(
    filename = function() {
      smoke_report_title()
    },
    content = function(file) {
      
      withProgress(message = "Generating smoke report...", value = 0, {
        
        incProgress(0.15, detail = "Preparing inputs")
        
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
          SUPERFOG_SCREEN_SELECT = if (input$REGION == "08") input$SUPERFOG_SCREEN_SELECT else NULL
        )
        
        report_filename <- paste0(
          format(Sys.time(), "%Y%m%d-%H%M%S"),
          "-",
          safe_filename(input$FOREST),
          "-",
          safe_filename(input$BURN_NAME),
          ".html"
        )
        
        rendered_file <- file.path(tempdir(), report_filename)
        
        incProgress(0.40, detail = "Rendering report")
        
        rmarkdown::render(
          "smoke_template_shiny_dev.Rmd",
          output_file = rendered_file,
          params = params_ls,
          envir = new.env(parent = globalenv())
        )
        
        incProgress(0.25, detail = "Uploading (optional)")
        
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
          
          TRUE
          
        }, error = function(e) {
          message("GitHub upload failed: ", conditionMessage(e))
          report_link(NULL)
          FALSE
        })
        
        incProgress(0.20, detail = "Finalizing")
        
        file.copy(rendered_file, file, overwrite = TRUE)
        
      })
    }
  )
  
  ### link to the report on github pages
  output$report_link_ui <- renderUI({
    req(report_link())
    
    tags$div(
      style = "margin-top:12px; margin-bottom:12px;",
      tags$a(
        href = report_link(),
        target = "_blank",
        class = "btn btn-success",
        "View Report Online"
      ),
      tags$div(
        style = "font-size:12px; color:#666; margin-top:4px;",
        "Note: the link may take 5–20 seconds to become available."
      ),
      tags$div(
        style = "font-size:12px; color:#666; margin-top:4px; word-break:break-all;",
        report_link()
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