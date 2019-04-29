library(tidyverse)
library(glue)
library(rio)
library(janitor)
library(english)

theme_set(theme_minimal(10))
source(here::here("scripts", "clean-names.R"))

# School Years 2013-14 to 2018-19 currently available
get_data <- function(year) {
  if(year == 14) {
    link <- "https://www.oregon.gov/ode/educator-resources/assessment/Documents/oka-results_state-district-school_20140131.xlsx"
  }
  if(year > 14 & year <= 16) {
    link <- glue("https://www.oregon.gov/ode/educator-resources/assessment/Documents/oka_results_state-district-school_{year-1}{year}.xlsx")
  }
  if(year > 16) {
    link <- glue("https://www.oregon.gov/ode/educator-resources/assessment/Documents/KA_Media_{year-1}{year}.xlsx")
  }
  import(link,
         setclass = "tibble",
         skip = 5,
         na = "*") %>% 
    remove_empty("rows") %>% 
    setNames(nms[[as.character(english(year))]])
}
years <- setNames(14:19, 14:19)
d <- map_df(years, get_data, .id = "year") %>% 
  mutate(year = as.character(glue("{as.numeric(year)-1}-{year}")))

# Collapse demos
d %>% 
  count(stu_group)

d %>% 
  count(stu_type)

d <- d %>% 
  mutate(
    stu_group = case_when(
      stu_group == "American Indian/Alaska Native" ~ 
                   "American Indian/Alaskan Native",
      stu_group == "African American" ~ "Black/African American",
      stu_group == "Hispanic" ~ "Hispanic/Latino",
      stu_group == "Multi-Racial" ~ "Multi-Ethnic",
      stu_group == "Pacific Islander" ~ "Native Hawaiian/Pacific Islander",
      TRUE ~ stu_group),
    stu_type = ifelse(stu_type == "Ethnicity-Race", "Race/Ethnicity", stu_type)) 

d <- d %>% 
  gather(measure, val, selfreg_score:lowerln_n) %>% 
  separate(measure, c("measure", "type")) %>% 
  spread(type, val) 
