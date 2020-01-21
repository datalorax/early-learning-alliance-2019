
county_sidebar <- 
  menuItem("County-Level", icon = icon("university"), startExpanded = TRUE,
    selectInput("county_name", "Select County:", 
                choices = counties,
                selected = "Lane"),
    menuItem("All Students", 
             tabName = "all-stu-county",
             icon = icon("user")),
    menuItem("By Group", icon = icon("users"), 
      menuSubItem("Race-Ethnicity", tabName = "race-eth-county"),
        menuSubItem("Gender", tabName = "gender-county"),
        menuSubItem("Other Groups", tabName = "other-county")
        )
    )

district_sidebar <- 
  menuItem("District-level", icon = icon("school"),
    menuItem("All Students",
             tabName = "all-stu-dist",
             icon = icon("user")), 
    menuItem("By Group", icon = icon("users"),
      menuSubItem("Race-Ethnicity", tabName = "race-eth-dist"),
      menuSubItem("Gender", tabName = "gender-dist"),
      menuSubItem("Other Groups", tabName = "all-other-dist")
      ),
    selectInput("dist_name", "School District:", 
                choices = dists, 
                selected = "Eugene SD 4J")
    )

school_sidebar <- 
  menuItem("School-level", icon = icon("chalkboard-teacher"),
    menuItem("All Students",tabName = "all-stu-schl"), 
    menuItem("By Group", icon = icon("users"),
      menuSubItem("Race-Ethnicity", tabName = "race-eth-schl"),
      menuSubItem("Gender", tabName = "gender-schl"),
      menuSubItem("Other Groups", tabName = "all-other-schl")
      ),
    selectInput("dist_name_school", "School District:", choices = dists)#,
    #selectInput("school_name", "School:", choices = schls)
    )