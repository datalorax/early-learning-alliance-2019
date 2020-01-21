# district_means <- d %>%
#   group_by(county, year, dist_name, stu_type, stu_group, measure) %>%
#   summarize(mean = mean(score, na.rm = TRUE),
#             se = sundry::se(score)) %>%
#   left_join(below_threshold) %>%
#   mutate(below = ifelse(mean < threshold, 1, 0)) %>% 
#   ungroup()

# write_feather(district_means,
#                        here::here("shiny", "data",
#                                   "district_means.feather"))

district_means <- read_feather("data/district_means.feather")
# 
# dist_means <- district_means %>% 
#   filter(stu_type == "Total Population",
#          dist_name != "Statewide") %>% 
#   select(county, year, dist_name, stu_type, 
#          stu_group, measure, mean) %>% 
#   spread(year, mean)
# 
# dist_below <- district_means %>% 
#   filter(stu_type == "Total Population",
#          dist_name != "Statewide") %>% 
#   select(county, year, dist_name, stu_type, 
#          stu_group, measure, below) %>% 
#   spread(year, below) %>% 
#   append("below_")

#dist_tbl <- left_join(dist_means, dist_below)
# write_feather(dist_tbl,
#               here::here("shiny", "data", "dist_tbl.feather"))

dist_tbl <- read_feather("data/dist_tbl.feather")

district_all_tbl <- function(input) { 
  renderDataTable({
    dist_tbl %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(stu_type == "Total Population",
             measure == input$measure) %>%
      select(-stu_type, -stu_group) %>% 
      rename(County = county,
             `School District` = dist_name) %>%
      datatable(extensions = 'Buttons',
                caption = paste0(input$dist_name, ": ", input$measure),
                options = list(rowCallback = JS(dist_callback),
                               columnDefs = list(list(targets = c(3, 10:15),
                                                      visible = FALSE)),
                               scrollX = TRUE,
                               dom = 'Bfrtip',
                               buttons = list('copy', 
                                              'print', 
                                              list(extend = 'collection',
                                                   buttons = c('csv', 'excel', 'pdf'),
                                                   text = 'Download')
                                              )
                               )
                ) 
  })
}

district_all <- function(input) {
    rows <- input$dist_tbl_rows_selected
    p <- district_means %>%
      filter(measure == input$measure, 
             stu_type == "Total Population",
             dist_name != "Statewide") %>% 
          ggplot(aes(year, mean)) +
          geom_ribbon(aes(x = year, ymin = -Inf, ymax = threshold),
                data = filter(district_means,
                              measure == input$measure, 
                              stu_type == "Total Population",
                              dist_name != "Statewide") %>%
                         distinct(year, threshold),
                inherit.aes = FALSE, 
                fill = "#e34755",
                alpha = 0.6) +
          geom_line(data = filter(district_means, 
                                  dist_name == "Statewide",
                                  stu_type == "Total Population",
                                  measure == input$measure), 
                    aes(color = "black"), 
                    lwd = 1.1)  +
          geom_line(data = filter(district_means, 
                                  stu_type == "Total Population",
                                  dist_name == input$dist_name, 
                                  measure == input$measure), 
                    aes(color = "#58CABD"),
                    lwd = 1.6) +
          geom_point(data = filter(district_means, 
                              stu_type == "Total Population",
                              dist_name == input$dist_name, 
                              measure == input$measure), 
                     aes(color = "#58CABD")) +
          geom_point(aes(year, mean),
                     color = "#e34755",
                     data = filter(district_means,
                                   measure == input$measure,
                                   dist_name == input$dist_name, 
                                   stu_type == "Total Population",
                                   below == 1))  +
          labs(x = "Year",
               y = "Average Score",
               title = input$measure,
               caption = "Oregon Kindergarten Entry Assessment Data") +
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
                  data = filter(district_means,
                                stu_type == "Total Population"  &
                                dist_name %in% unique(dist_tbl$dist_name)[rows],
                                measure == input$measure)) +
        geom_point(color = "#e34755",
                   size = 2.5,
                   aes(year, mean),
                   data = filter(district_means,
                                 stu_type == "Total Population",
                                 dist_name %in% unique(dist_tbl$dist_name)[rows],
                                 measure == input$measure,
                                 below == 1)) +
        ggrepel::geom_text_repel(aes(label = dist_name),
                                 min.segment.length = 0,
                                 box.padding = 0.5,
                                 data = filter(district_means,
                                               stu_type == "Total Population"  &
                                               dist_name %in% unique(dist_tbl$dist_name)[rows],
                                               measure == input$measure,
                                               year == 2018))
    }
  p
}


##### Within-district (between-school)
district_within_all_tbl <- function(input) { 
  renderDataTable({
    within_district_means_tbl %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(measure == input$measure,
             dist_name == input$dist_name, 
             inst_name != dist_name,
             stu_type == "Total Population") %>%
      select(-4:-6) %>%
      rename(`School District` = dist_name,
             `Institution Name` = inst_name,
             County = county) %>%
      datatable(caption = paste0(input$dist_name, ": ", input$measure),
                extensions = 'Buttons',
                options = list(rowCallback = JS(schl_callback),
                               columnDefs = list(list(targets = 10:15,
                                                      visible = FALSE)),
                               scrollX = TRUE,
                               dom = 'Bfrtip',
                               buttons = list('copy', 
                                              'print', 
                                              list(extend = 'collection',
                                                   buttons = c('csv', 'excel', 'pdf'),
                                                   text = 'Download')
                                              )
                               )
                ) 
  })
}

district_within_all <- function(input) { 
  rows <- input$district_within_all_tbl_rows_selected
  p <- within_district_means %>%
    filter(dist_name == input$dist_name,
           stu_type == "Total Population",
           measure == input$measure,
           inst_name != input$dist_name) %>%
    ggplot(aes(year, mean)) +
    geom_ribbon(aes(x = year, 
                    ymin = -Inf, 
                    ymax = threshold),
                inherit.aes = FALSE,
                data = filter(below_threshold, 
                              stu_group == "Total Population", 
                              measure == input$measure), 
                fill = "#e34755",
                alpha = 0.6) +
    geom_line(aes(group = inst_name),
              lwd = 0.2,
              color = "gray40")  +
    geom_point(color = "gray70")  + 
    geom_point(color = "#e34755",
               data = filter(within_district_means,
                             dist_name == input$dist_name,
                             stu_type == "Total Population",
                             measure == input$measure,
                             inst_name != input$dist_name,
                             below == 1)) +
    geom_line(aes(color = "black"), 
              data = filter(within_district_means,
                            stu_type == "Total Population",
                            measure == input$measure,
                            inst_name == input$dist_name),
              lwd = 1.1) + 
    labs(x = "Year",
         y = "Average Score",
         title = input$measure,
         caption = "Oregon Kindergarten Entry Assessment Data") +
    scale_color_identity(name = "",
                         breaks = "black",
                         labels = "District Average",
                         guide = "legend") +
    theme(legend.position = c(0, -0.2),
          legend.direction = "horizontal",
          legend.justification = 'left',
          plot.margin = margin(0.3, 0.3, 1, 0.3, unit = "cm"))
  if(length(rows) > 0) {
    highlighted <- pull_schl_from_dist(input$dist_name, rows)
    p <- p +
      geom_line(color = "#8895DC",
                lwd = 1.6,
                aes(group = inst_name),
                data = filter(within_district_means,
                              stu_type == "Total Population",
                              measure == input$measure,
                              inst_name %in% highlighted)) +
      geom_point(color = "#e34755",
                 size = 2.5,
                 aes(year, mean),
                 data = filter(within_district_means,
                               stu_type == "Total Population",
                               inst_name %in% highlighted,
                               measure == input$measure,
                               below == 1)) +
      ggrepel::geom_text_repel(aes(label = inst_name),
                               min.segment.length = 0,
                               box.padding = 0.5,
                               data = filter(within_district_means,
                                             stu_type == "Total Population",
                                             measure == input$measure,
                                             inst_name %in% highlighted,
                                             year == 2018))
  }
  p
}


###### By groups
district_eth <- function(input) {
  district_means %>% 
    filter(stu_type == "Race/Ethnicity" &
           stu_group %in% input$race_dist,
           measure == input$measure,
           dist_name == input$dist_name) %>% 
    ggplot(aes(year, mean)) +
    geom_ribbon(aes(x = year, 
                    ymin = -Inf, 
                    ymax = threshold),
                inherit.aes = FALSE,
                data = filter(below_threshold, 
                              stu_group %in% input$race_dist,
                              measure == input$measure), 
                fill = "#e34755",
                alpha = 0.3) +
    geom_line(aes(color = stu_group)) +
    geom_ribbon(aes(ymin = mean + qnorm(0.975)*se,
                    ymax = mean + qnorm(0.025)*se,
                    fill = stu_group),
                alpha = 0.3) +
    geom_point(aes(color = stu_group)) +
    geom_point(color = "#e34755",
               data = filter(district_means,
                             stu_group %in% input$race_dist,
                             measure == input$measure,
                             dist_name == input$dist_name,
                             below == 1)) +    
    facet_wrap(~stu_group) +
    scale_fill_manual("Student Group", 
                      values = race_pal[input$race_dist]) +
    scale_color_manual("Student Group", 
                       values = race_pal[input$race_dist]) +
    guides(fill = "none",
           color = "none") +    
    labs(title = input$dist_name,
         subtitle = input$measure) +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          legend.justification = 'left') 
}

district_gender <- function(input) {
  district_means %>% 
    filter(stu_group %in% input$gender_dist,
           measure == input$measure,
           dist_name == input$dist_name) %>% 
    ggplot(aes(year, mean)) +
    geom_ribbon(aes(x = year, 
                    ymin = -Inf, 
                    ymax = threshold),
                inherit.aes = FALSE,
                data = filter(below_threshold, 
                              stu_group %in% input$gender_dist,
                              measure == input$measure), 
                fill = "#e34755",
                alpha = 0.3) +
    geom_ribbon(aes(ymin = mean + qnorm(0.975)*se,
                    ymax = mean + qnorm(0.025)*se,
                    fill = stu_group),
                alpha = 0.3) +
    geom_line(aes(color = stu_group)) +
    geom_point(aes(color = stu_group)) +
    geom_point(color = "#e34755",
               data = filter(district_means,
                             stu_group %in% input$gender_dist,
                             measure == input$measure,
                             dist_name == input$dist_name,
                             below == 1)) +    
    facet_wrap(~stu_group) +
    scale_fill_manual("Student Group", 
                      values = gender_pal[input$gender_dist]) +
    scale_color_manual("Student Group", 
                       values = gender_pal[input$gender_dist]) +
    guides(fill = "none",
           color = "none") +
    labs(title = input$dist_name,
         subtitle = input$measure) +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          legend.justification = 'left') 
}

district_other <- function(input) {
  district_means %>% 
    filter(stu_type != "Race/Ethnicity" &
           stu_type != "Gender" &
           stu_group %in% input$other_dist,
           measure == input$measure,
           dist_name == input$dist_name) %>% 
    ggplot(aes(year, mean)) +
    geom_ribbon(aes(x = year, 
                    ymin = -Inf, 
                    ymax = threshold),
                inherit.aes = FALSE,
                data = filter(below_threshold, 
                              stu_type != "Race/Ethnicity",
                              stu_type != "Gender",
                              stu_group %in% input$other_dist, 
                              measure == input$measure), 
                fill = "#e34755",
                alpha = 0.3) +
    geom_line(aes(color = stu_group)) +
    geom_ribbon(aes(ymin = mean + qnorm(0.975)*se,
                    ymax = mean + qnorm(0.025)*se,
                    fill = stu_group),
                alpha = 0.3) +
    geom_point(aes(color = stu_group)) +
    geom_point(color = "#e34755",
               data = filter(district_means,
                             stu_group %in% input$other_dist,
                             measure == input$measure,
                             dist_name == input$dist_name,
                             below == 1)) +
    facet_wrap(~stu_group) +
    scale_fill_manual("Student Group", 
                      values = other_pal[input$other_dist]) +
    scale_color_manual("Student Group", 
                       values = other_pal[input$other_dist]) +
    guides(fill = "none",
           color = "none") +    
    labs(title = input$dist_name,
         subtitle = input$measure) +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          legend.justification = 'left') 
}


dist_within_race_tbl <- function(input) { 
  renderDataTable({
    district_means %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(stu_group %in% input$race_dist,
             dist_name == input$dist_name,
             measure == input$measure) %>%
      ungroup() %>%
      select(year, stu_group, mean) %>%
      spread(year, mean) %>%
      datatable(extensions = 'Buttons',
                caption = paste0(input$dist_name, ": ", input$measure),
                selection = "none",
                options = list(dom = "Bfrtip",
                               buttons = list("copy",
                                              "print",
                                              list(extend = "collection",
                                                buttons = c("csv", "excel", "pdf"),
                                                text = "Download")
                                              )
                               )
                )
  })
}

dist_within_gender_tbl <- function(input) {
  renderDataTable({
    district_means %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(stu_group %in% input$gender_dist,
             dist_name == input$dist_name,
             measure == input$measure) %>%
      ungroup() %>%
      select(year, stu_group, mean) %>%
      spread(year, mean) %>%
      datatable(extensions = 'Buttons',
                caption = paste0(input$dist_name, ": ", input$measure),
                selection = "none",
                options = list(dom = "Bfrtip",
                               buttons = list("copy",
                                              "print",
                                              list(extend = "collection",
                                                buttons = c("csv", "excel", "pdf"),
                                                text = "Download")
                                              )
                               )
                )

  })
}

dist_within_other_tbl <- function(input) {
  renderDataTable({
    district_means %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(stu_group %in% input$other_dist,
             dist_name == input$dist_name,
             measure == input$measure) %>%
      ungroup() %>%
      select(year, stu_group, mean) %>%
      spread(year, mean) %>%
      datatable(extensions = 'Buttons',
                caption = paste0(input$dist_name, ": ", input$measure),
                selection = "none",
                options = list(dom = "Bfrtip",
                               buttons = list("copy",
                                              "print",
                                              list(extend = "collection",
                                                buttons = c("csv", "excel", "pdf"),
                                                text = "Download")
                                              )
                               )
                )
    
  })
}
