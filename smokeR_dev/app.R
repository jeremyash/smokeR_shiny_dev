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

# unit names
nfs <-readRDS('usfs_unit_list.RDS')


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
    # set up file names for downloads
    filename = function() {
      smoke_report_title()
    },
    content = function(file) {
      
      withProgress(message = "Generating smoke report...", value = 0, {
        
        incProgress(0.15, detail = "Preparing report parameters")
        
        params_ls <- list(
          BURN_NAME = input$BURN_NAME,
          FOREST = input$FOREST,
          REGION = input$REGION,
          AUTHOR = input$AUTHOR,
          EMAIL = input$EMAIL,
          PHONE = input$PHONE,
          RUN_ID = input$RUN_ID,
          
          FORECAST_AQI_SELECT = if (input$REGION == "08") {
            input$FORECAST_AQI_SELECT
          } else {
            NULL
          },
          
          SUPERFOG_SCREEN_SELECT = if (input$REGION == "08") {
            input$SUPERFOG_SCREEN_SELECT
          } else {
            NULL
          },
          
          HOURLY_MAP_SELECT = input$HOURLY_MAP_SELECT
        )
        
        incProgress(0.35, detail = "Rendering R Markdown report")
        
        rmarkdown::render(
          "smoke_template_shiny_dev.Rmd",
          output_file = file,
          params = params_ls
        )
        
        incProgress(0.50, detail = "Finalizing download")
      })
    }
  )
  
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