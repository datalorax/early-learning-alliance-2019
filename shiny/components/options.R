msrs <- sort(unique(d$measure))
counties <- d %>%
  filter(county != "All Counties") %>%
  pull(county) %>%
  unique() %>%
  sort()

race <- d %>%
	filter(stu_type == "Race/Ethnicity") %>%
  pull(stu_group) %>% 
  unique() %>% 
  sort()

race_pal <- setNames(viridis(length(race) + 1)[-(length(race) + 1)], 
                     race)

gender <- d %>%
	filter(stu_type == "Gender") %>%
  pull(stu_group) %>% 
  unique() %>% 
  sort()
gender_pal <- setNames(viridis(length(gender) + 1)[-(length(gender) + 1)], 
                       gender)

all_other <- d %>%
	filter(stu_type != "Gender" &
	       stu_type != "Race/Ethnicity") %>%
  pull(stu_group) %>% 
  unique() %>% 
  sort()
other_pal <- setNames(viridis(length(all_other) + 1)[-(length(all_other) + 1)],
                      all_other)

dists <- d %>%
  pull(dist_name) %>% 
  unique() %>% 
  sort()

pull_dists_from_county <- function(county_name, row_num) {
  dists <- d %>%
    filter(county == county_name) %>%
    pull(dist_name) %>%
    unique() %>%
    sort()
  
  dists[row_num]
}

pull_schl_from_dist <- function(dist, row_number) {
  schl <- d %>%
    filter(dist_name == dist,
           inst_name != dist) %>%
    pull(inst_name) %>%
    unique() %>%
    sort()
  
  schl[row_number]
}


