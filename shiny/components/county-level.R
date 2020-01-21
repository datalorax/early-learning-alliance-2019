# county_means <- d %>%
#   group_by(county, year, stu_type, stu_group, measure) %>%
#   summarize(mean = mean(score, na.rm = TRUE),
#             se = sundry::se(score)) %>%
#   ungroup() %>%
#   left_join(below_threshold) %>% 
#   mutate(below = ifelse(mean <= threshold, 1, 0))

# write_feather(county_means,
#               here::here("shiny", "data", "county_means.feather"))

county_means <- read_feather("data/county_means.feather")

county_tbl_fun <- function(input) {
  renderDataTable({
    county_tbl %>% 
      mutate_if(is.numeric, round, 2) %>%
      filter(measure == input$measure,
             stu_type == "Total Population") %>%
      select(-stu_type, -stu_group) %>%
      rename(County = county) %>%
      datatable(caption = input$measure,
                extensions = "Buttons",
                options = list(dom = 'Bfrtip',
                               rowCallback = JS(dist_callback),
                               columnDefs = list(list(targets = c(2, 9:14), 
                                                      visible = FALSE)),
                               scrollX = TRUE,
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

county_all <- function(input) {
    rows <- input$county_tbl_rows_selected
    p <- county_means %>%
      filter(measure == input$measure, 
             county != "All Counties",
             stu_type == "Total Population") %>% 
          ggplot(aes(year, mean)) +
          geom_ribbon(aes(x = year, ymin = -Inf, ymax = threshold), 
                    inherit.aes = FALSE,
                    data = filter(below_threshold, 
                                  stu_group == "Total Population", 
                                  measure == input$measure), 
                    fill = "#e34755",
                    alpha = 0.6) +
          geom_line(aes(group = county),
                    lwd = 0.2,
                    color = "gray40")  +
          geom_point(color = "gray70",
                     aes(year, mean),
                     data = filter(county_means, 
                                   !county %in% input$county_name & 
                                   measure == input$measure,
                                   stu_type == "Total Population"))  + 
          geom_line(data = filter(d, 
                                  county == "All Counties",
                                  stu_type == "Total Population",
                                  measure == input$measure), 
                    aes(y = score, color = "black"), 
                    lwd = 1.1)  +
          geom_line(data = filter(county_means, 
                                  stu_type == "Total Population",
                                  county == input$county_name, 
                                  measure == input$measure), 
                    aes(color = "#58CABD"),
                    lwd = 1.6) +
          geom_point(aes(year, mean),
                     color = "#e34755",
                     data = filter(county_means, 
                                   !county %in% input$county_name & 
                                     measure == input$measure,
                                     stu_type == "Total Population",
                                     below == 1))  +
          labs(x = "Year",
               y = "Average Score",
               title = input$measure,
               caption = "Oregon Kindergarten Entry Assessment Data") +
          scale_color_identity(name = "",
                               breaks = c("black", "#58CABD"),
                               labels = c("Overall Average", input$county_name),
                               guide = "legend") +
          theme(legend.position = c(0, -0.2),
                legend.direction = "horizontal",
                legend.justification = 'left',
                plot.margin = margin(0.3, 0.3, 1, 0.3, unit = "cm"))
    
    if(length(rows) > 0) {
      p <- p +
        geom_line(color = "#8895DC",
                  lwd = 1.6,
                  aes(group = county),
                  data = filter(county_means,
                                stu_type == "Total Population"  &
                                county %in% counties[rows],
                                measure == input$measure)) +
        geom_point(color = "#e34755",
                   size = 2.5,
                   aes(year, mean),
                   data = filter(county_means,
                                 stu_type == "Total Population"  & 
                                 county %in% counties[rows] &
                                 measure == input$measure &
                                 below == 1)) +
        ggrepel::geom_text_repel(aes(label = county),
                                 min.segment.length = 0,
                                 box.padding = 0.5,
                                 data = filter(county_means,
                                               stu_type == "Total Population", 
                                                 county %in% counties[rows],
                                                 measure == input$measure,
                                                 year == 2018))
        
    }
  p
}

# or_pop <- tidycensus::get_acs(geography = "county", 
#                          variables = "B01003_001", 
#                          state = "OR",
#                          geometry = TRUE) %>%
#       mutate(county = gsub(" County, Oregon", "", NAME))

# saveRDS(or_pop, "data/or_county_pop.RDS")

or_pop <- readRDS("data/or_county_pop.RDS")

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 5px; 
    padding-right: 5px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 12px;
  }
"))

or_county_all_map <- function(input) { 
  renderLeaflet({
    or_map_data <- left_join(or_pop, 
                             filter(county_means, 
                                    stu_type == "Total Population",
                                    measure == input$measure,
                                    as.character(year) == input$years))

    pal <- colorNumeric(palette = "viridis", 
                        domain = na.omit(or_map_data$mean))

    pal_rev <- colorNumeric(palette = "viridis", 
                        domain = na.omit(or_map_data$mean),
                        reverse = TRUE)

    title <- tags$div(tag.map.title, HTML(input$measure))

    or_map_data %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(popup = ~ paste0(NAME, "<br/>", round(mean, 2)),
                  color = "gray",
                  weight = 2,
                  fillColor = pal(or_map_data$mean),
                  fillOpacity = 0.7) %>%
      addLegend(pal = pal_rev,
                values = na.omit(or_map_data$mean),
                labFormat = labelFormat(
                  transform = function(x) sort(x, decreasing = TRUE)),
                position = "bottomright",
                title = paste("Average Score")) %>%
      addControl(title, position = "topleft", className="map-title")
  })
}

############################## By groups

county_eth <- function(input) {
    county_means %>% 
      filter(stu_type == "Race/Ethnicity" &
             stu_group %in% input$race_county,
             measure == input$measure,
             county == input$county_name) %>% 
      ggplot(aes(year, mean)) +
        geom_ribbon(aes(x = year, 
                        ymin = -Inf, 
                        ymax = threshold),
                      inherit.aes = FALSE,
                      data = filter(below_threshold, 
                                    stu_group %in% input$race_county, 
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
                   data = filter(county_means,
                                 stu_group %in% input$race_county,
                                 measure == input$measure,
                                 county == input$county_name,
                                 below == 1)) +    
        facet_wrap(~stu_group) +
        scale_fill_manual("Student Group",
                          values = race_pal[input$race_county]) +
        scale_color_manual("Student Group",
                           values = race_pal[input$race_county]) +
        guides(fill = "none",
               color = "none") +
        labs(title = input$county_name,
             subtitle = input$measure) +
        theme(legend.position = "bottom",
              legend.direction = "vertical",
              legend.justification = 'left') 
}

county_gender <- function(input) {
    county_means %>% 
      filter(stu_group %in% input$gender_county,
             measure == input$measure,
             county == input$county_name) %>% 
      ggplot(aes(year, mean)) +
        geom_ribbon(aes(x = year, 
                        ymin = -Inf, 
                         ymax = threshold),
                    inherit.aes = FALSE,
                    data = filter(below_threshold, 
                                  stu_group %in% input$gender_county,
                                  measure == input$measure), 
                    fill = "#e34755",
                    alpha = 0.3) +
        geom_line(aes(color = stu_group)) +
        geom_ribbon(aes(ymin = mean + qnorm(0.975)*se,
                        ymax = mean + qnorm(0.025)*se,
                        fill = stu_group),
                    alpha = 0.3) +
        geom_point(aes(color = stu_group))  +
        geom_point(color = "#e34755",
                   data = filter(county_means,
                                 stu_group %in% input$gender_county,
                                 measure == input$measure,
                                 county == input$county_name,
                                 below == 1)) +    
        facet_wrap(~stu_group) +
        scale_fill_manual("Student Group", 
                          values = gender_pal[input$gender_county]) +
        scale_color_manual("Student Group", 
                           values = gender_pal[input$gender_county]) +
        guides(fill = "none",
               color = "none") +
        labs(title = input$county_name,
             subtitle = input$measure) +
        theme(legend.position = "bottom",
              legend.direction = "vertical",
              legend.justification = 'left') 
}


county_other <- function(input) {
    county_means %>% 
      filter(stu_type != "Race/Ethnicity" &
             stu_type != "Gender" &
             stu_group %in% input$other_county,
             measure == input$measure,
             county == input$county_name) %>% 
      ggplot(aes(year, mean)) +
        geom_ribbon(aes(x = year, 
                        ymin = -Inf, 
                        ymax = threshold),
                    inherit.aes = FALSE,
                    data = filter(below_threshold, 
                                  stu_type != "Race/Ethnicity",
                                  stu_type != "Gender",
                                  stu_group %in% input$other_county, 
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
                   data = filter(county_means,
                                 stu_type != "Race/Ethnicity" &
                                 stu_type != "Gender" &
                                 stu_group %in% input$other_county,
                                 measure == input$measure,
                                 county == input$county_name,
                                 below == 1)) +
        facet_wrap(~stu_group) +
        scale_fill_manual("Student Group", 
                          values = other_pal[input$other_county]) +
        scale_color_manual("Student Group", 
                           values = other_pal[input$other_county]) +
        guides(fill = "none",
               color = "none") +
        labs(title = input$county_name,
             subtitle = input$measure) +
        theme(legend.position = "bottom",
              legend.direction = "vertical",
              legend.justification = 'left') 
}


### Within-county comparisons
county_within_all <- function(input) { 
    rows <- input$within_county_all_tbl_rows_selected
    p <- within_county_means %>%
    filter(county == input$county_name,
           stu_type == "Total Population",
           measure == input$measure) %>%
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
          geom_line(aes(group = dist_name),
                    lwd = 0.2,
                    color = "gray40")  +
          geom_point(color = "gray70")  + 
          geom_line(data = filter(d, 
                                  county == "All Counties",
                                  stu_type == "Total Population",
                                  measure == input$measure), 
                    aes(y = score, color = "black"), 
                    lwd = 1.1)  +
          geom_point(aes(year, mean),
                     color = "#e34755",
                     data = filter(within_county_means, 
                                   county == input$county_name,
                                     measure == input$measure,
                                     stu_type == "Total Population",
                                     below == 1))  +
          labs(x = "Year",
               y = "Average Score",
               title = input$measure,
               caption = "Oregon Kindergarten Entry Assessment Data") +
          scale_color_identity(name = "",
                               breaks = c("black", "#58CABD"),
                               labels = c("Overall Average", input$county_name),
                               guide = "legend") +
          theme(legend.position = c(0, -0.2),
                legend.direction = "horizontal",
                legend.justification = 'left',
                plot.margin = margin(0.3, 0.3, 1, 0.3, unit = "cm"))

    if(length(rows) > 0) {
      highlighted <- pull_dists_from_county(input$county_name, rows)
      p <- p +
        geom_line(color = "#8895DC",
                  lwd = 1.6,
                  aes(group = dist_name),
                  data = filter(within_county_means,
                                stu_type == "Total Population"  &
                                dist_name %in% highlighted,
                                measure == input$measure)) +
        geom_point(color = "#e34755",
                   size = 2.5,
                   aes(year, mean),
                   data = filter(within_county_means,
                                 stu_type == "Total Population",
                                 dist_name %in% highlighted,
                                 measure == input$measure,
                                 below == 1)) +
        ggrepel::geom_text_repel(aes(label = dist_name),
                                 min.segment.length = 0,
                                 box.padding = 0.5,
                                 data = filter(within_county_means,
                                               stu_type == "Total Population",
                                               dist_name %in% highlighted,
                                               measure == input$measure,
                                               year == 2018))
                                 
    }
  p
}

county_within_all_tbl <- function(input) { 
  renderDataTable({
    within_county_means_tbl %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(stu_type == "Total Population",
             county == input$county_name,
             measure == input$measure) %>%
      select(-1, -3:-5) %>%
      rename(`School District` = dist_name) %>%
      datatable(caption = paste0(input$county_name, "County: ", input$measure),
                extensions = 'Buttons',
                options = list(rowCallback = JS(within_county_callback),
                               columnDefs = list(list(targets = 8:13,
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

county_within_race_tbl <- function(input) { 
  renderDataTable({
    county_means %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(stu_group %in% input$race_county,
             county == input$county_name,
             measure == input$measure) %>%
      ungroup() %>%
      select(year, stu_group, mean) %>%
      spread(year, mean) %>%
      datatable(extensions = 'Buttons',
                caption = paste0(input$county_name, "County: ", input$measure),
                selection = "none",
                options = list(dom = 'Bfrtip',
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

county_within_gender_tbl <- function(input) { 
  renderDataTable({
    county_means %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(stu_group %in% input$gender_county,
             county == input$county_name,
             measure == input$measure) %>%
      ungroup() %>%
      select(year, stu_group, mean) %>%
      spread(year, mean) %>%
      datatable(extensions = 'Buttons',
                caption = paste0(input$county_name, "County: ", input$measure),
                selection = "none",
                options = list(dom = 'Bfrtip',
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

county_within_other_tbl <- function(input) { 
  renderDataTable({
    county_means %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(stu_group %in% input$other_county,
             county == input$county_name,
             measure == input$measure) %>%
      ungroup() %>%
      select(year, stu_group, mean) %>%
      spread(year, mean) %>%
      datatable(extensions = 'Buttons',
                caption = paste0(input$county_name, "County: ", input$measure),
                selection = "none",
                options = list(dom = 'Bfrtip',
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