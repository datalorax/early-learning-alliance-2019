append <- function(tbl, pred, pattern = "\\d") {
  nms <- names(tbl)
  keep <- names(tbl)[!grepl(pattern, nms)]
  replacement <- names(tbl)[grepl(pattern, nms)]
  
  replacement <- paste0(pred, replacement[grep(pattern, replacement)])
  names(tbl) <- c(keep, replacement)
  tbl
}


# # Means across counties
# tbl1_county_means <- county_means %>%
#   filter(county != "All Counties") %>%
#   select(-se:-below) %>%
#   spread(year, mean) %>%
#   ungroup()
# 
# tbl2_county_below <- county_means %>%
#   filter(county != "All Counties") %>%
#   select(-mean:-threshold) %>%
#   spread(year, below) %>%
#   ungroup() %>%
#   append("below_")
# 
# county_tbl <- left_join(tbl1_county_means, tbl2_county_below)
# 
# write_feather(county_tbl,
#               here::here("shiny", "data", "county_tbl.feather"))

county_tbl <- read_feather("data/county_tbl.feather")


### Means for all districts by county  
# 
# within_county_means <- d %>%
#   group_by(county, dist_name, year, stu_type, stu_group, measure) %>%
#   summarize(mean = mean(score, na.rm = TRUE),
#             se = sundry::se(score)) %>%
#   ungroup()
# 
# within_county_means <-
#   left_join(within_county_means, below_threshold) %>%
#   mutate(below = ifelse(mean <= threshold, 1, 0))
# 
# feather::write_feather(within_county_means,
#                        here::here("shiny", "data",
#                                   "within-county-means.feather"))

within_county_means <- read_feather("data/within-county-means.feather")

# within_county_means_tbl1 <- within_county_means %>% 
#   select(-se:-below) %>%
#   spread(year, mean)
# 
# within_county_means_tbl2 <- within_county_means %>% 
#   select(-mean:-threshold) %>%
#   spread(year, below) %>%
#   append("below_")
# 
# within_county_means_tbl <- left_join(within_county_means_tbl1,
#                                      within_county_means_tbl2)
# feather::write_feather(within_county_means_tbl,
#                        here::here("shiny", "data",
#                                   "within-county-means-tbl.feather"))
within_county_means_tbl <- read_feather("data/within-county-means-tbl.feather")

######### Within-District
# within_district_means <- d %>%
#   group_by(county, dist_name, inst_name, year, stu_type, stu_group, measure) %>%
#   summarize(mean = mean(score, na.rm = TRUE),
#             se = sundry::se(score)) %>%
#   ungroup()
# 
# within_district_means <-
#   left_join(within_district_means, below_threshold) %>%
#   mutate(below = ifelse(mean <= threshold, 1, 0))
# 
# feather::write_feather(within_district_means,
#                        here::here("shiny", "data",
#                                   "within-district-means.feather"))

within_district_means <- read_feather("data/within-district-means.feather")
# 
# 
# within_district_means_tbl1 <- within_district_means %>%
#   select(-se:-below) %>%
#   spread(year, mean)
# 
# within_district_means_tbl2 <- within_district_means %>%
#   select(-mean:-threshold) %>%
#   spread(year, below) %>%
#   append("below_")
# 
# within_district_means_tbl <- left_join(within_district_means_tbl1,
#                                        within_district_means_tbl2)
# 
# feather::write_feather(within_district_means_tbl,
#                        here::here("shiny", "data",
#                                   "within-district-means-tbl.feather"))

within_district_means_tbl <- read_feather("data/within-district-means-tbl.feather")