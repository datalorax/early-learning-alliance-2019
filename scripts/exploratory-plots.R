source(here::here("scripts", "_packages.R"))

theme_set(theme_minimal(10))

d <- read_feather(here::here("data", "ela_14-19.feather"))
lane <- filter(d, county == "Lane")

means <- lane %>% 
  group_by(stu_type, stu_group, measure, year) %>% 
  summarize(mean = mean(score, na.rm = TRUE),
            se = sundry::se(score)) %>% 
  ungroup()
  
#fs::dir_create((here::here("plots", "county-level")))

# Overall trends by measure
county_level <- means %>% 
  filter(stu_group == "Total Population") %>% 
ggplot(aes(year, mean)) +
  geom_line(color = "cornflowerblue") +
  geom_ribbon(aes(ymin = mean + qnorm(0.975)*se,
                  ymax = mean + qnorm(0.025)*se),
              fill = "cornflowerblue",
              alpha = 0.3) +
  geom_point(color = "gray40") +
  facet_wrap(~measure, scales = "free_y") +
  expand_limits(y = 0) +
  labs(x = "Year",
       y = "Average Score",
       title = "Trends by measure over time",
       subtitle = "Average scores across all student groups",
       caption = "Lane County, Oregon")
ggsave(here::here("plots", "county-level", "overall-trends.png"),
       county_level,
       width = 9,
       height = 6.5,
       dpi = 500)

# By race/ethnicity
means %>% 
  filter(stu_type == "Race/Ethnicity") %>% 
ggplot(aes(year, mean)) +
  geom_line(aes(color = stu_group)) +
  geom_ribbon(aes(ymin = mean + qnorm(0.975)*se,
                  ymax = mean + qnorm(0.025)*se,
                  fill = stu_group),
              alpha = 0.3) +
  geom_point(aes(color = stu_group)) +
  facet_wrap(~measure, scales = "free_y") +
  expand_limits(y = 0) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

# By Hispanic/Latino vs White
hw <- means %>% 
  filter(stu_type == "Race/Ethnicity",
         stu_group == "Hispanic/Latino" | 
           stu_group == "White") %>% 
ggplot(aes(year, mean)) +
  geom_line(aes(color = stu_group)) +
  geom_ribbon(aes(ymin = mean + qnorm(0.975)*se,
                  ymax = mean + qnorm(0.025)*se,
                  fill = stu_group),
              alpha = 0.3) +
  geom_point(aes(color = stu_group)) +
  facet_wrap(~measure, scales = "free_y") +
  expand_limits(y = 0) +
  scale_fill_brewer("Student Group", palette = "Set2") +
  scale_color_brewer("Student Group", palette = "Set2") +
  labs(x = "Year",
       y = "Average Score",
       title = "Trends by measure over time",
       subtitle = "Average scores for students coded as Hispanic/Latino and White displayed by year and measure",
       caption = "Lane County, Oregon") +
  theme(legend.position = c(0.15, -0.1),
        legend.direction = "horizontal",
        plot.margin = margin(0.3, 0.3, 1, 0.3, unit = "cm"))

ggsave(here::here("plots", "county-level", "hisp-white.png"),
       hw,
       width = 9,
       height = 6.5,
       dpi = 500)

# By gender
g <- means %>% 
  filter(stu_type == "Gender",
         stu_group != "Non-Binary") %>% 
ggplot(aes(year, mean)) +
  geom_line(aes(color = stu_group)) +
  geom_ribbon(aes(ymin = mean + qnorm(0.975)*se,
                  ymax = mean + qnorm(0.025)*se,
                  fill = stu_group),
              alpha = 0.3) +
  geom_point(aes(color = stu_group)) +
  facet_wrap(~measure, scales = "free_y") +
  expand_limits(y = 0) +
  scale_fill_brewer("Coded gender", palette = "Accent") +
  scale_color_brewer("Coded gender", palette = "Accent") +
  labs(x = "Year",
       y = "Average Score",
       title = "Trends by measure over time",
       subtitle = "Average scores displayed by gender, year, and measure",
       caption = "Lane County, Oregon") +
  theme(legend.position = c(0.15, -0.1),
        legend.direction = "horizontal",
        plot.margin = margin(0.3, 0.3, 1, 0.3, unit = "cm"))

ggsave(here::here("plots", "county-level", "gender.png"),
       g,
       width = 9,
       height = 6.5,
       dpi = 500)

###### By District
dist_means <- lane %>% 
  group_by(dist_name, stu_type, stu_group, measure, year) %>% 
  summarize(mean = mean(score, na.rm = TRUE),
            se = sundry::se(score)) %>% 
  ungroup()

tot_pop <- filter(dist_means, stu_type == "Total Population") 
dists <- unique(dist_means$dist_name)

dist_plots <- map(dists, ~
    ggplot(tot_pop, aes(year, mean)) +
      geom_line(aes(group = dist_name),
                lwd = 0.2,
                color = "gray40")  +
      geom_line(data = filter(means, stu_type == "Total Population"),
                aes(color = "black"),
                lwd = 1.1)  +
      geom_line(data = filter(tot_pop, dist_name == .x),
                aes(color = "#E34755"),
                lwd = 1.2) +
      facet_wrap(~measure, scales = "free_y") +
      labs(x = "Year",
           y = "Average Score",
           title = "Trends by district and measure over time",
           subtitle = .x,
           caption = "Lane County, Oregon") +
      scale_color_identity(name = "",
                          breaks = c("black", "#E34755"),
                          labels = c("Overall Average", .x),
                          guide = "legend") +
      theme(legend.position = c(0.15, -0.1),
        legend.direction = "horizontal",
        plot.margin = margin(0.3, 0.3, 1, 0.3, unit = "cm"))
)

#fs::dir_create((here::here("plots", "district-level")))
paths <- here::here("plots", "district-level")
paths <- glue("{paths}/{str_replace_all(dists, ' ', '-')}.png")
walk2(paths, dist_plots, ggsave, 
      width = 9,
      height = 6.5,
      dpi = 500)
