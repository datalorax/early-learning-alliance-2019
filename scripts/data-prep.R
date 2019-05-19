source(here::here("scripts", "_packages.R"))
source(here::here("scripts", "clean-names.R"))

# School Years 2013-14 to 2018-19 currently available
get_data <- function(year) {
  if(year < 19) {
    link <- glue("https://www.oregon.gov/ode/educator-resources/assessment/Documents/ka_20{year-1}-{year}_lookback_report.xlsx")
  } else if(year == 19) {
    link <- glue("https://www.oregon.gov/ode/educator-resources/assessment/Documents/KA_Media_{year-1}{year}.xlsx")
  }              
  d <- import(link,
         setclass = "tibble",
         skip = 5,
         na = "*") %>% 
    remove_empty("rows") %>% 
    setNames(nms[[as.character(english(year))]]) 
  
  if(!is.null(d$spanish_score)) {
    d <- d %>% 
      mutate(spanish_score = as.numeric(spanish_score))
  }
  d
}

years <- setNames(14:19, 14:19)
d <- map_df(years, get_data, .id = "year") %>% 
  mutate(year = as.character(glue("{as.numeric(year)-1}-{year}")))

# Collapse demos
# d %>%
#   count(stu_type)
# d %>%
#   count(stu_group)

d <- d %>% 
  mutate(
    stu_type = case_when(
      stu_type == "Economically Dis" ~ "Economically Disadvantaged",
      stu_type == "Limited English" ~ "Limited English Proficient",
      stu_type == "Migrant Educatio" ~ "Migrant Education",
      stu_type == "Ethnicity-Race" ~ "Race/Ethnicity",
      stu_type == "Students with Di" ~ "Students with Disabilities",
      TRUE ~ stu_type),
    stu_group = case_when(
      stu_group == "American Indian/Alaska Native" ~ 
                   "American Indian/Alaskan Native",
      stu_group == "African American" ~ "Black/African American",
      stu_group == "Hispanic" ~ "Hispanic/Latino",
      stu_group == "Multi-Racial" ~ "Multi-Ethnic",
      stu_group == "Pacific Islander" ~ "Native Hawaiian/Pacific Islander",
      TRUE ~ stu_group)) 

d <- d %>% 
  gather(measure, val, selfreg_score:lowerln_n) %>% 
  separate(measure, c("measure", "type")) %>% 
  spread(type, val)
  
measure_labels <- c("interpersonal" = "Approaches to Learning: Interpersonal Skills",
                    "selfreg" = "Approaches to Learning: Self Regulation",
                    "atl" = "Approaches to Learning: Total", 
                    "ln" = "Letter Names", 
                    "lowerln" = "Letter Names: Lower case",
                    "upperln" = "Letter Names: Upper Case",
                    "ls" = "Letter Sounds",
                    "math" = "Mathematics",
                    "spanish" = "Spanish")

d <- d %>% 
  mutate(year = parse_number(year) + 2001,
         measure = factor(measure,
                          levels = names(measure_labels),
                          labels = measure_labels))

#fs::dir_create(here::here("data"))
write_feather(d, here::here("data", "ela_14-19.feather"))

