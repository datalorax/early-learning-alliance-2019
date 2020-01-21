library(shinydashboard)
library(viridisLite)
library(feather)
library(dplyr)
library(tidyr)
library(DT)
library(RColorBrewer)
library(ggplot2)
library(leaflet)
library(sf)
library(tidycensus)
theme_set(theme_minimal(15))

source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "Early Learning Alliance"),
    dashboardSidebar(
      selectInput("measure", "Measure:", choices = msrs),
      sidebarMenu(id = "tabs",
        county_sidebar,
        district_sidebar
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", 
                  type = "text/css", 
                  href = "custom.css")
      ),
      tabItems(
        tabItem(tabName = "all-stu-county",
          fluidRow(
            tabsetPanel(type = "tabs",
              tabPanel("Trends",
                box(title = "Trends by county and measure over time",
                    plotOutput("county_all_stu", height = 450),
                    downloadButton(outputId = "county_all_stu_dl", 
                                   label = "Download the plot")
                    ),
                box(title = HTML("Cells colored <font color='#e34755'><b>red</b></font> are > 1 SD below the mean for the corresponding year"),
                    h4("Click rows to highlight districts in the chart"),
                    dataTableOutput("county_tbl")
                )),
                tabPanel("Map",
                numericInput("years", 
                             "Select Year", 
                             min = 2014,
                             max = 2019,
                             value = 2019),
                leafletOutput("or_county_map")
                ),
                tabPanel("Within-County Comparison",          
                  box(title = "Trends between districts over time",
                      plotOutput("county_within_all", height = 450),
                      downloadButton(outputId = "county_within_all_dl", 
                                   label = "Download the plot")
                  ),
                  box(title = HTML("Cells colored <font color='red'><b>red</b></font> are > 1 SD below the mean for the corresponding year"),
                      h4("Click rows to highlight districts in the chart"),
                      dataTableOutput("within_county_all_tbl")
                  )
                )
              )
            )
        ),
        tabItem("race-eth-county",
          box(plotOutput("county_eth"),
              downloadButton(outputId = "county_eth_dl", 
                             label = "Download the plot"),
              width = 8),
          box(title = "Student Group",
              checkboxGroupInput("race_county", "Race/Ethnicity:",
                           choices = race,
                           selected = c("White", "Hispanic/Latino")),
             width = 4),
          dataTableOutput("county_within_race_tbl")
        ),
        tabItem("gender-county",
          box(plotOutput("county_gender"),
              downloadButton(outputId = "county_gender_dl", 
                             label = "Download the plot"),
              width = 8),
          box(title = "Select Groups",
               checkboxGroupInput("gender_county", "Gender:",
                           choices = gender,
                           selected = c("Male", "Female")),
             width = 4),
          dataTableOutput("county_within_gender_tbl")
        ),
        tabItem("other-county",
          box(plotOutput("county_other"), 
              downloadButton(outputId = "county_other_dl", 
                                   label = "Download the plot"),
              width = 8),
          box(title = "Trends by All Other Student Groups",
               checkboxGroupInput("other_county", "Student Group:",
                           choices = all_other,
                           selected = c("Total Population", 
                                        "Economically Disadvantaged")),
             width = 4),
          dataTableOutput("county_within_other_tbl")
        ),
        tabItem(tabName = "all-stu-dist",
          fluidRow(
             tabsetPanel(type = "tabs",
              tabPanel("Trends",
                box(title = "Trends by district and measure over time",
                    plotOutput("district_all", height = 450),
                    downloadButton(outputId = "district_all_stu_dl", 
                                   label = "Download the plot")
              ),
             box(title = HTML("Cells colored <font color='red'><b>red</b></font> are > 1 SD below the mean for the corresponding year"),
                 h4("Click rows to compare districts (add lines) in the chart"),
                 dataTableOutput("dist_tbl")
             )
           ),
              tabPanel("Within-District (between schools)",
                 box(plotOutput("district_within_all", height = 450),
                     downloadButton(outputId = "district_within_dl", 
                                   label = "Download the plot")
                     ),  
                 box(dataTableOutput("district_within_all_tbl")
                     )    
               )
            )
         )
        ),
        tabItem("race-eth-dist",
           h2("Trends by Race/Ethnicity"),
             box(plotOutput("district_eth"),
                 downloadButton(outputId = "district_eth_dl", 
                                   label = "Download the plot"), 
                 width = 8),
             box(title = "Student Group",
                 checkboxGroupInput("race_dist", "Race/Ethnicity:",
                                    choices = race,
                                    selected = c("White", "Hispanic/Latino")),
                 width = 4
             ),
             dataTableOutput("dist_within_race_tbl")
         ),
        tabItem(tabName = "gender-dist",
          h2("Trends by Gender"),
             box(plotOutput("district_gender"),
                 downloadButton(outputId = "district_gender_dl", 
                                   label = "Download the plot"),
                 width = 8),
             box(title = "Student Group",
                 checkboxGroupInput("gender_dist", "Student Gender:",
                                    choices = gender,
                                    selected = c("Male", "Female")),
                 width = 4
             ),
            dataTableOutput("dist_within_gender_tbl")
        ),
        tabItem(tabName = "all-other-dist",
          h2("Trends by All Other Student Groups"),
             box(plotOutput("district_other"),
                 downloadButton(outputId = "district_other_dl", 
                                   label = "Download the plot"),
                 width = 8),
             box(title = "Student Group",
                 checkboxGroupInput("other_dist", "Student Gender:",
                                    choices = all_other,
                                    selected = c("Total Population", 
                                                 "Economically Disadvantaged")),
                 width = 4
            ),
          dataTableOutput("dist_within_other_tbl")
        )
      )
    )
)

server <- function(input, output) {

  # County level
  output$county_all_stu <- renderPlot(county_all(input))
  output$county_all_stu_dl <- downloader(input, 
                                         county_all,
                                         input$county_name)

  output$county_within_all <- renderPlot(county_within_all(input))
  output$county_within_all_dl <- downloader(input, 
                                            county_within_all,
                                            paste0("within-", 
                                                   input$county_name))

  output$county_eth <- renderPlot(county_eth(input))
  output$county_eth_dl <- downloader(input, 
                                     county_eth,
                                     paste0(input$race, input$county_name))

  output$county_gender <- renderPlot(county_gender(input))
  output$county_gender_dl <- downloader(input, 
                                        county_gender,
                                        paste0(input$gender, input$county_name))

  output$county_other <- renderPlot(county_other(input))
  output$county_other_dl <- downloader(input, 
                                       county_other,
                                       paste0(input$gender, input$county_name))

  output$county_tbl <- county_tbl_fun(input)

  output$or_county_map <- or_county_all_map(input)

  output$within_county_all_tbl  <- county_within_all_tbl(input)

  output$county_within_race_tbl <- county_within_race_tbl(input)
  output$county_within_gender_tbl <- county_within_gender_tbl(input)
  output$county_within_other_tbl <- county_within_other_tbl(input)


  # District level
  output$dist_tbl <- district_all_tbl(input)
  output$district_within_all_tbl <- district_within_all_tbl(input)
   
  output$district_all <- renderPlot(district_all(input))
  output$district_all_stu_dl <- downloader(input, 
                                           district_all,
                                           input$dist_name)

  output$district_within_all <- renderPlot(district_within_all(input))
  output$district_within_dl <- downloader(input, 
                                           district_within_all,
                                           input$dist_name)

  output$district_eth <- renderPlot(district_eth(input))
  output$district_eth_dl <- downloader(input,
                                       district_eth,
                                       input$dist_name)

  output$district_gender <- renderPlot(district_gender(input))
  output$district_gender_dl <- downloader(input,
                                       district_gender,
                                       input$dist_name)

  output$district_other <- renderPlot(district_other(input))
  output$district_other_dl <- downloader(input,
                                       district_other,
                                       input$dist_name)
  
  output$dist_within_race_tbl <- dist_within_race_tbl(input)
  output$dist_within_gender_tbl <- dist_within_gender_tbl(input)
  output$dist_within_other_tbl <- dist_within_other_tbl(input)
}

shinyApp(ui, server)