
# Day 1 -------------------------------------------------------------------


  # Site 1 uses 200 per month
  # Site 2 uses 550 per month
  # Site 3 has 2 sub-sites that each need 925 tests per month
  # You should order 10% extra as a buffer against higher demand
  # You have 420 tests extra remaining from last month, which you can use in the coming month
  
  ((200 + 550 + (925 * 2)) * 1.1) - 420
  site1 <- 250
  site2 <- 730
  site3 <- 2 * 1050
  tests_extra <- 37
  tests_to_order <- ((site1 + site2 + site3) * 1.1) - tests_extra
  
  
  confirmed_cases <- 44
  suspect_cases <- 12
  total_cases <- confirmed_cases + suspect_cases
  
  
  dog <- "Finn"
  dog
  
  vector <- c(3, 55, 9, -4, 33)
  min(vector)
  max(vector)
  
  patient_ages <- c(5, 10, 60, 25, 12)
  
  
  counties <- c("Dekalb", "Fulton", "Cobb", "Gwinett")
  toupper(counties)
  counties_up <- toupper(counties)
  
  tolower(counties_up)
  
  paste(counties)
  
  paste(counties, collapse = ", ")
  paste(counties, collapse = " - ")
  paste(counties)
  
  
  install.packages("pacman")
  library(pacman)
  # Load all the packages needed, installing if necessary
  pacman::p_load(
    rio,          # for importing data
    here,         # for file paths
    janitor,      # for data cleaning
    lubridate,    # for working with dates
    tidyverse     # for data management
  )
  
  # shortcuts ---------------------------------------------------------------
  
  # <- alt -
  # %>% ctrl shift m
  # section_header  ctrl shift r
    


# Day 2 ------------------------------------------------------------------------
  # File management, data import, tidy data, and exploratory analysis ------------

  # Recall that an RStudio project is a self-contained and portable R working environment - a folder for all the files associated with a distinct project (data files, R scripts, outputs, etc.).
  # 
  # You can zip an RStudio project and email it to someone else. They should be able to open it and replicate your work exactly!
  #   An RStudio project can be linked to a repository on version-control software like Git/Github

  # load packages
  pacman::p_load(
    rio,          # for importing data
    here,         # for file paths
    janitor,      # for data cleaning
    lubridate,    # for working with dates
    flextable,    # for making pretty tables
    tidyverse     # for data management
  )



# Practice importing data -------------------------------------------------

  import("surveillance_linelist_20141201.csv")
  
  # vroom package made for quickly importing very large datasets
  # vroom(file, delim = ",")
  
  # can do multiple files with import_list() and listtoenv()
  # import_list() has a bind argument to bind files together upon import
  # my_data_2 <- import_list(dir(here("data", "raw", "hospitals", "backup")
  #                              , pattern = ".csv"
  #                              , full.names = TRUE)
  #                          , rbind = TRUE)
  
  surv_raw <- import("surveillance_linelist_20141201.csv")
  
  # can also import google sheets (may need googlesheets4 package)
  
  here()
  
  here("data", "raw")
  
  here("data", "raw", "surveillance_linelist_20141201.csv")
  
  surv_raw2 <- import(here("data", "raw", "surveillance_linelist_20141201.csv"))
  
  surv_raw$hospital
  surv_raw$case_id
  
  # return the value at the 12th row and the 4th column
  surv_raw[12,4]
  
  # entire column 4
  surv_raw[ ,4]
  
  # entire row 12
  surv_raw[12, ]
  
  summary(surv_raw$age)
  
  # Return the 3rd element of the summary
  summary(surv_raw$age)[3]
  
  summary(surv_raw$age)[3] + 4
  
  
  # return a good summary of the dataset using 
  # the R package {skimr} and its function skim()
  pacman::p_load(skimr)
  
  skim(surv_raw)
  
  

# Tidy data ---------------------------------------------------------------

  partner_tracking <- import(here("data", "raw", "messy_data_examples.xlsx"), 
                             sheet = "messy_colors")
    
  site_coverage <- import(here("data", "raw", "messy_data_examples.xlsx"), 
                          sheet = "messy_site_coverage")
    
    
  site_coverage_tidy <- import(here("data", "raw", "messy_data_examples.xlsx"), 
                               sheet = "tidy_site_coverage") 
    
  messy <- import(here("data", "raw", "messy_data_examples.xlsx"), 
                  sheet = "messy_gis")  
    
    
    
  # for reading in excel files with merged cells:
  # The {openxlsx} R package handles Excel workbooks with 
  # more precision than {rio}. Its function read.xlsx() 
  # offers an argument fillMergedCells = which can be set to 
  # TRUE. For this function, the argument for the sheet name 
  # is also sheet =.
    
  sites <- openxlsx::read.xlsx(here("data", "raw", "messy_data_examples.xlsx"),
                               sheet = "messy_site_coverage",
                               fillMergedCells = TRUE)
    
      
    
  # cleans and expands this tidy data so that every possible date and site are present in the data.  
      
    # import the long data 
    site_coverage_clean <- site_coverage_tidy %>%    # create complete dataset 
      mutate(Date = ymd(Date)) %>%           # convert dates to proper class in R
      complete(                              # complete all sites and dates
        Date = seq.Date(
          from = min(Date),
          to = max(Date),
          by = "day"),
        Site = seq(1:14),
        fill = list(Status = "No")) %>%      # If not already listed in data, status is "No"
      mutate(Province = as_factor(ifelse(Site %in% 1:7, "A", "B")), # add Province
             Site = as_factor(Site)) 
  
  
  # we can use the {ggplot2} data visualization R package to create a “heat plot” that resembles the original Excel spreadsheet.
    
    # create heat tile plot
    ggplot(data = site_coverage_clean,
           mapping = aes(x = Date, y = fct_rev(Site),
                         fill = Status, label = Status))+
      geom_tile(color = "white")+
      geom_text()+
      scale_x_date(
        date_breaks = "day",
        labels = scales::label_date_short(),
        expand = c(0,0))+
      scale_fill_manual(
        values = c(
          "Yes" = "darkgreen",
          "No" = "orange"))+
      theme_minimal(base_size = 16)+
      labs(title = "Site coverage",
           y = "Site")+
      facet_wrap(~Province, ncol = 1, scales = "free_y")
    
    
    
  # tabulate Status by Province:
    site_coverage_clean %>% 
      tabyl(Province, Status) %>% 
      qflextable()
    
  # by Date:
    site_coverage_clean %>% 
      tabyl(Date, Status) %>% 
      arrange(desc(Yes)) %>% 
      qflextable()
    
  # Or the data can be aggregated into weeks and the 
    # number of unfilled spots tabulated:
    site_coverage_clean %>% 
      group_by(week_of = floor_date(Date, "week")) %>% 
      summarise(days_coverage_needed = sum(Status == "No")) %>% 
      qflextable()
    
  # Or the data can be used to quickly make other 
    # informative plots:  
    site_coverage_clean %>% 
      filter(Status == "Yes") %>% 
      ggplot(mapping = aes(x = fct_infreq(Site)))+
      geom_bar(fill = "dodgerblue")+
      coord_flip()+
      theme_minimal(base_size = 16)+
      labs(title = "Number of days 'covered', by site",
           x = "Site",
           y = "Number of days with coverage")
  
# Extras:
  pacman::p_load(praise)
  praise() # lol
  
  pacman::p_load(cowsay)
  say(what = "Hola", by = "frog")  # chicken, yoda, spider, ant, more?
  say(what = "Wazzup?", by = "yoda", by_color = "blue", what_color = "yellow")

  pacman::p_load(aRtsy)
  canvas_collatz(colors = colorPalette("tuscany3"))
  canvas_flow(colors = colorPalette("dark2"))  
  canvas_petri(colors = colorPalette("sooph"))
  
  
  
  
  
  
    