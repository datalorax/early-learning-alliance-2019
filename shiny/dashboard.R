library(shiny)
library(shinydashboard)
library(feather)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(RColorBrewer)

theme_set(theme_minimal(15))
d <- read_feather(here::here("data", "ela_14-19.feather")) %>% 
  mutate(measure = as.character(measure))
lane <- filter(d, 
               county == "Lane" &
               dist_name != "Lane ESD")

means <- lane %>% 
  group_by(stu_type, stu_group, measure, year) %>% 
  summarize(mean = mean(score, na.rm = TRUE),
            se = sundry::se(score)) %>% 
  ungroup()

dist_means_by_stu <- lane %>% 
  group_by(dist_name, stu_type, stu_group, measure, year) %>% 
  summarize(mean = mean(score, na.rm = TRUE),
            se = sundry::se(score)) %>% 
  ungroup()

tot_pop <- filter(dist_means_by_stu, stu_type == "Total Population") %>%
  group_by(measure, year) %>%
  mutate(overall_mean = mean(mean, na.rm = TRUE),
         overall_sd   = sd(mean, na.rm = TRUE),
         below = ifelse(mean <= overall_mean - overall_sd, 1, 0)) 

dists <- unique(dist_means_by_stu$dist_name)
msrs <- unique(dist_means_by_stu$measure)
race <- filter(dist_means_by_stu, stu_type == "Race/Ethnicity")$stu_group %>% 
  unique()
gender <- filter(dist_means_by_stu, stu_type == "Gender")$stu_group %>% 
  unique()
all_other <- filter(dist_means_by_stu, 
                    stu_type != "Gender" &
                    stu_type != "Race/Ethnicity")$stu_group %>% 
  unique()

race_pal <- setNames(brewer.pal(length(race), "Set2"), race)
gender_pal <- setNames(brewer.pal(length(gender), "Set2"), gender)
other_pal <- setNames(brewer.pal(length(all_other), "Set2"), all_other)

dist_means <- lane %>% 
  select(dist_name, year, measure, score) %>% 
  group_by(dist_name, year, measure) %>% 
  summarize(mean = round(mean(score, na.rm = TRUE), 2))

append <- function(tbl, pred, pattern = "\\d") {
  nms <- names(tbl)
  keep <- names(tbl)[!grepl(pattern, nms)]
  replacement <- names(tbl)[grepl(pattern, nms)]
  
  replacement <- paste0(pred, replacement[grep(pattern, replacement)])
  names(tbl) <- c(keep, replacement)
  tbl
}

tbl1_means <- tot_pop %>%
  select(-overall_mean, -overall_sd, -se, -below) %>%
  mutate(mean = round(mean, 2)) %>%
  spread(year, mean) 

tbl2_below <- tot_pop %>%
  select(-overall_mean, -overall_sd, -se, -mean) %>%
  spread(year, below) %>%
  append("below_")

tbl <- left_join(tbl1_means, tbl2_below) %>%
  select(-stu_type, -stu_group)

rowCallback <- c(
  "function(row, dat, displayNum, index){",
  "  if(dat[9] > 0){",
  "    $('td:eq(2)', row).addClass('red');",
  "  }",
  "  if(dat[10] > 0){",
  "    $('td:eq(3)', row).addClass('red');",
  "  }",
  "  if(dat[11] > 0){",
  "    $('td:eq(4)', row).addClass('red');",
  "  }",
  "  if(dat[12] > 0){",
  "    $('td:eq(5)', row).addClass('red');",
  "  }",
  "  if(dat[13] > 0){",
  "    $('td:eq(6)', row).addClass('red');",
  "  }",
  "  if(dat[14] > 0){",
  "    $('td:eq(7)', row).addClass('red');",
  "  }",
  "  if(dat[15] > 0){",
  "    $('td:eq(8)', row).addClass('red');",
  "  }",
  "}"
)

css <- "
.red {
  background-color: #e34755;
}
table.dataTable tr.selected td.red {
  background-color: #e34755 !important;
  color: #ffffff !important;
}
"

ui <- dashboardPage(
  dashboardHeader(title = "Early Learning Alliance"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("All Students", tabName = "all-stu", icon = icon("school")),
      menuItem("By Group",  icon = icon("users"),
        menuSubItem("Race-Ethnicity", tabName = "race-eth"),
        menuSubItem("Gender", tabName = "gender"),
        menuSubItem("Other Groups", tabName = "all-other")
        ),
      selectInput("dist_name", "School District:", choices = dists),
      selectInput("measure", "Measure:", choices = msrs)
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(css))
    ),
    tabItems(
      tabItem(tabName = "all-stu",
        fluidRow(
          box(title = "Trends by district and measure over time",
             #div(style = "position:relative",
              plotOutput("plot1", height = 450)#,
                             # hover = hoverOpts("plot_hover", 
                             #                   delay = 100, 
                             #                   delayType = "debounce")
              #),
              #uiOutput("hover_info")
            #)
            ),
          box(title = HTML("Cells colored <font color='red'><b>red</b></font> are > 1 SD below the mean for the corresponding year"),
              h4("Click rows to highlight districts in the chart"),
              dataTableOutput("tbl")
          )
        )
      ),
      tabItem(tabName = "race-eth",
         h2("Trends by Race/Ethnicity"),
         fluidRow(
           
           box(plotOutput("plot_eth", height = 450)),

           box(
             title = "Student Group",
             checkboxGroupInput("race", "Race/Ethnicity:",
                         choices = race,
                         selected = c("White", "Hispanic/Latino"))
           )
         )
       ),
      tabItem(tabName = "gender",
        h2("Trends by Gender"),
                 fluidRow(
           
           box(plotOutput("plot_gen", height = 450)),

           box(
             title = "Student Group",
             checkboxGroupInput("gender", "Student Gender:",
                         choices = gender,
                         selected = c("Male", "Female"))
           )
         )
      ),
      tabItem(tabName = "all-other",
        h2("Trends by All Other Student Groups"),
                 fluidRow(
           
           box(plotOutput("plot_all_other", height = 450)),

           box(
             title = "Student Group",
             checkboxGroupInput("all_other", "Student Gender:",
                         choices = all_other,
                         selected = c("Total Population", 
                                      "Economically Disadvantaged"))
           )
         )
      )
    )
  )
)

server <- function(input, output) {

  output$tbl <- renderDataTable({
    tbl %>% 
      filter(measure == input$measure &
             dist_name != "Lane ESD") %>%
      rename(`School District` = dist_name) %>%
      datatable(options = list(rowCallback = JS(rowCallback),
                               columnDefs = list(list(targets = c(2, 9:14), 
                                                      visible = FALSE)),
                               scrollX = TRUE),
                caption = input$measure) 

  })

  output$plot1 <- renderPlot({
    rows <- input$tbl_rows_selected
    
    p <- tot_pop %>% 
      filter(measure == input$measure) %>% 
      ggplot(aes(year, mean)) +
      geom_line(aes(group = dist_name),
                lwd = 0.2,
                color = "gray40")  +
      geom_point(color = "gray70",
                 aes(year, mean),
                 data = filter(tot_pop, 
                               !dist_name %in% input$dist_name &
                                 measure == input$measure))  +
      geom_line(data = filter(means, 
                              stu_type == "Total Population",
                              measure == input$measure),
                aes(color = "black"),
                lwd = 1.1)  +
      geom_line(data = filter(tot_pop, 
                              dist_name == input$dist_name,
                              measure == input$measure),
                aes(color = "#58CABD"),
                lwd = 1.6) +
      geom_point(color = "#e34755",
                 aes(year, mean),
                 data = filter(tot_pop, 
                               !dist_name %in% input$dist_name &
                                 measure == input$measure &
                                 below == 1))  +
      labs(x = "Year",
           y = "Average Score",
           title = input$dist_name,
           subtitle = input$measure,
           caption = "Lane County, Oregon") +
      scale_color_identity(name = "",
                           breaks = c("black", "#58CABD"),
                           labels = c("Overall Average", input$dist_name),
                           guide = "legend") +
      theme(legend.position = c(0, -0.2),
            legend.direction = "horizontal",
            legend.justification = 'left',
            plot.margin = margin(0.3, 0.3, 1, 0.3, unit = "cm"))

      if(length(rows) > 0) {
        p <- p +
          geom_line(color = "#8895DC",
                    lwd = 1.6,
                    aes(group = dist_name),
                    data = filter(tot_pop, 
                                  dist_name %in% dists[rows],
                                  measure == input$measure)) +
          geom_point(color = "#e34755",
                     size = 2.5,
                     aes(year, mean),
                     data = filter(tot_pop, 
                                   dist_name %in% dists[rows] &
                                   measure == input$measure &
                                   below == 1)) 
      }
      p
  })

  # output$hover_info <- renderUI({
  #   hover <- input$plot_hover
  #   point <- nearPoints(tot_pop, hover, threshold = 5, maxpoints = 1, addDist =
  #                       TRUE)
  #   if (nrow(point) == 0) return(NULL)
    
  #   # calculate point position INSIDE the image as percent of total dimensions
  #   # from left (horizontal) and from top (vertical)
  #   left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  #   top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
  #   # calculate distance from left and bottom side of the picture in pixels
  #   left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  #   top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
  #   # create style property fot tooltip
  #   # background color is set so tooltip is a bit transparent
  #   # z-index is set so we are sure are tooltip will be on top
  #   style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
  #                   "left:", left_px, "px; top:", top_px, "px;")
    
  #   # actual tooltip created as wellPanel
  #   wellPanel(
  #     style = style,
  #     p(HTML(paste0("School District: ", point$dist_name)))
  #   )
  # })

  output$plot_eth <- renderPlot({
    dist_means_by_stu %>% 
      filter(stu_type == "Race/Ethnicity" &
             stu_group %in% input$race,
             measure == input$measure,
             dist_name == input$dist_name) %>% 
      ggplot(aes(year, mean)) +
        geom_line(aes(color = stu_group)) +
        geom_ribbon(aes(ymin = mean + qnorm(0.975)*se,
                        ymax = mean + qnorm(0.025)*se,
                        fill = stu_group),
                    alpha = 0.3) +
        geom_point(aes(color = stu_group)) +
        expand_limits(y = 0) +
      scale_fill_manual("Student Group", values = race_pal[input$race]) +
      scale_color_manual("Student Group", values = race_pal[input$race]) +
        labs(title = input$dist_name,
             subtitle = input$measure) +
        theme(legend.position = "bottom",
              legend.direction = "vertical",
              legend.justification = 'left') 
  })
  
  output$plot_gen <- renderPlot({
    dist_means_by_stu %>% 
      filter(stu_type == "Gender" &
               stu_group %in% input$gender,
             measure == input$measure,
             dist_name == input$dist_name) %>% 
      ggplot(aes(year, mean)) +
      geom_line(aes(color = stu_group)) +
      geom_ribbon(aes(ymin = mean + qnorm(0.975)*se,
                      ymax = mean + qnorm(0.025)*se,
                      fill = stu_group),
                  alpha = 0.3) +
      geom_point(aes(color = stu_group)) +
      expand_limits(y = 0) +
      scale_fill_manual("Student Group", values = gender_pal[input$gender]) +
      scale_color_manual("Student Group", values = gender_pal[input$gender]) +
      labs(title = input$dist_name,
           subtitle = input$measure) +
      theme(legend.position = c(0, -0.15),
            legend.direction = "horizontal",
            legend.justification = 'left',
            plot.margin = margin(0.3, 0.3, 1, 0.3, unit = "cm"))
  })

  output$plot_all_other <- renderPlot({
    dist_means_by_stu %>% 
      filter(stu_group %in% input$all_other &
             measure == input$measure,
             dist_name == input$dist_name) %>% 
      ggplot(aes(year, mean)) +
      geom_line(aes(color = stu_group)) +
      geom_ribbon(aes(ymin = mean + qnorm(0.975)*se,
                      ymax = mean + qnorm(0.025)*se,
                      fill = stu_group),
                  alpha = 0.3) +
      geom_point(aes(color = stu_group)) +
      expand_limits(y = 0) +
      scale_fill_manual("Student Group", values = other_pal[input$all_other]) +
      scale_color_manual("Student Group", values = other_pal[input$all_other]) +
      labs(title = input$dist_name,
           subtitle = input$measure) +
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            legend.justification = 'left')
  })
  
}

shinyApp(ui, server)