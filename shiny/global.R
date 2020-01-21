d <- read_feather(here::here("data", "ela_14-19.feather")) %>% 
  mutate(measure = as.character(measure))

within_district_means_tbl <- read_feather("data/within-district-means.feather")


# below_threshold <- d %>%
#   group_by(year, stu_type, stu_group, measure) %>%
#   mutate(overall_mean = mean(score, na.rm = TRUE),
#          overall_sd   = sd(score, na.rm = TRUE),
#          threshold = overall_mean - overall_sd) %>%
#   select(year, stu_type, stu_group, measure, threshold) %>%
#   distinct(.keep_all = TRUE) %>%
#   ungroup()
# 
# write_feather(below_threshold,
#               here::here("shiny", "data", "below_threshold.feather"))

below_threshold <- read_feather("data/below_threshold.feather")

downloader <- function(input, f, name) {
  downloadHandler(
    filename = function() { paste0(name, '.png') },
    content = function(file) {
      ggsave(file, 
             plot = f(input), 
             device = "png",
             width = 9,
             height = 6,
             dpi = 300)
    }
  )
}

source("components/county-level.R")
source("components/district-level.R")
source("components/options.R")
source("components/tables.R")
source("components/sidebar.R")

within_county_callback <- c(
  "function(row, dat, displayNum, index){",
  "  if(dat[8] > 0){",
  "    $('td:eq(2)', row).addClass('red');",
  "  }",
  "  if(dat[9] > 0){",
  "    $('td:eq(3)', row).addClass('red');",
  "  }",
  "  if(dat[10] > 0){",
  "    $('td:eq(4)', row).addClass('red');",
  "  }",
  "  if(dat[11] > 0){",
  "    $('td:eq(5)', row).addClass('red');",
  "  }",
  "  if(dat[12] > 0){",
  "    $('td:eq(6)', row).addClass('red');",
  "  }",
  "  if(dat[13] > 0){",
  "    $('td:eq(7)', row).addClass('red');",
  "  }",
  "}"
)

dist_callback <- c(
  "function(row, dat, displayNum, index){",
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

schl_callback <- c(
  "function(row, dat, displayNum, index){",
  "  if(dat[10] > 0){",
  "    $('td:eq(4)', row).addClass('red');",
  "  }",
  "  if(dat[11] > 0){",
  "    $('td:eq(5)', row).addClass('red');",
  "  }",
  "  if(dat[12] > 0){",
  "    $('td:eq(6)', row).addClass('red');",
  "  }",
  "  if(dat[13] > 0){",
  "    $('td:eq(7)', row).addClass('red');",
  "  }",
  "  if(dat[14] > 0){",
  "    $('td:eq(8)', row).addClass('red');",
  "  }",
  "  if(dat[15] > 0){",
  "    $('td:eq(9)', row).addClass('red');",
  "  }",
  "}"
)