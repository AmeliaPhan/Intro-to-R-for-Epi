

# Script Info -------------------------------------------------------------

# Purpose: A script for analyzing Ebola outbreak data
# Author: Amelia Phan
# Organization: CDC
# Contact: fet0@cdc.gov
# Creation date: 11/16/2023
# Last update:



# Load Packages ----------------------------------------------------------------

pacman::p_load(
  rio, # for importing data
  here, # for locating files
  skimr, # for reviewing data
  janitor, # data cleaning
  lubridate, # working with dates # included in tidyverse
  epikit, # creating age categories
  gtsummary, # creating nice tables
  tidyverse, # data management and visualization
  tidylog, # records every data cleaning step that you run 
          # and will print a record of the changes in the console
  flextable, # create quick tables
  gtsummary, # create nice tables
  scales, # override default breaks, labels, transformations, palettes of ggplot2
  ggExtra,
  gghighlight,
  RColorBrewer,
  viridis,
  ggthemes
)



# Import Data ------------------------------------------------------------------

surv_raw <- import(here("data", "raw", "surveillance_linelist_20141201.csv"))

# import hospital linelist data
hosp_central <- import(here("data", "raw", "hospitals" , "2014-12-01_hosp_central.csv"))
hosp_military <-  import(here("data", "raw", "hospitals", "2014-12-01_hosp_military.csv"))
hosp_missing <- import(here("data", "raw", "hospitals", "2014-12-01_hosp_missing.csv"))
hosp_other <- import(here("data", "raw", "hospitals", "2014-12-01_hosp_other.csv"))
hosp_port <- import(here("data", "raw", "hospitals", "2014-12-01_hosp_port.csv"))
hosp_smmh <- import(here("data", "raw", "hospitals", "2014-12-01_hosp_smmh.csv"))

lab <- import(here("data", "raw", "lab_results_20141201.xlsx")) %>% 
  clean_names()

investigations <- import(here("data", "raw", "case_investigations_20141201.xlsx")) %>% 
  select(-c(age, age_unit, gender)) # dropping repeat columns before merging with combined df


## Exploratory Analysis ----------------------------------------------------

skim(surv_raw) # review data

names(surv_raw) # look at names of variables

surv_raw %>%
  tabyl(gender) # print the number of rows for each unique value in the column

# An early part of the data cleaning process is ensuring 
# that R correctly understands the class for each column
class(surv_raw$'onset date') # check variable type

surv_raw %>% 
  get_dupes() # see duplicate rows

surv_raw %>% 
  get_dupes(case_id) # see duplicates of a certain variable

surv_raw %>% 
  tabyl(`age unit`)

class(surv_raw)
class(surv_raw$age)
# The first concern when “cleaning” a date column in R, is which 
# “class” the column has been assigned by default
class(surv_raw$`onset date`) 
  # During the import of the data, R has read this column as character 
  # values (e.g. “11/03/2014”) and did not assume that it was a date. 

# histogram of age distribution
ggplot(data = surv_raw, mapping = aes(x = age)) + # take data and map age to x-axis
  geom_histogram() # produce histogram

# boxplot of age distribution
ggplot(data = surv_raw, mapping = aes(x = age)) +
  geom_boxplot()

ggplot(data = surv_raw, mapping = aes(x = `wt (kg)`))+
  geom_histogram()

ggplot(data = surv_raw, mapping = aes(x = `ht (cm)`))+
  geom_histogram()

# cross-tabs
surv_raw %>% 
  tabyl(adm3_name_res, adm3_name_det)



# Clean Data -------------------------------------------------------------------

  # For a given data set, it is best to store all the cleaning steps in 
  # one connected sequence of commands, all linked 
  # together by “pipe” operators %>%

surv <- surv_raw %>% 
  
  # automatically standardize column names
  clean_names() %>% 
  
  # manually clean column names
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det) %>% 
  
  # remove unnecessary column
  # There are other such “tidyselect” helpers available to you such as 
  # contains() and starts_with(), which you can write within the select() 
  # command like contains("date_")
  select(-row_num) %>% 
  
  # remove duplicate rows
  distinct() %>% 

  # modify columns to be converted to class date
  # similar to as.date function from baseR; as.Date() requires specifying 
  # the format of the date string if it's not in the 
  # default "yyyy-mm-dd" format.
  # read chapter in Epi Handbook re standardizing dates
  # parse_date is used with messy dates, i.e., dates in multple formats
  mutate(date_onset = mdy(date_onset)) %>% 
  mutate(date_report = mdy(date_report)) %>% 

  # modify class of age column from integer to numeric
  # we may want to perform calculations with this column that would result 
  # in it needing to accept decimal values. As class “integer”, this column 
  # cannot accept decimals, but as class “numeric” it can
  mutate(age = as.numeric(age)) %>% 
  
  # change 'Unknown'  values to NA for gender variable
  # Note: it is also useful to know the opposite function, replace_na(), 
    # which replaces NA with a specified value.
  mutate(gender = na_if(gender, "Unknown")) %>% 
  
  # good use of across function
  # Mutate all the columns that are character class, and use the 
  # function na_if() to convert any instances of "" to NA
  # old version of code:
    # mutate(across(.cols = where(is.character), .fns = na_if, ""))
  mutate(across(.cols = where(is.character), ~ na_if(.x, ""))) %>% 
  
  # recoding hospital names
  mutate(hospital = recode(hospital,
                           "Mitilary Hospital" = "Military Hospital",
                           "Port" = "Port Hospital",
                           "Port Hopital" = "Port Hospital",
                           "St. Mark's Maternity Hospital (SMMH)" = "SMMH")) %>% 
  
  # recoding gender
  mutate(gender = recode(gender,
                         "m" = "male",
                         "f" = "female")) %>% 
  
  # convert negative wt_kg values to NA
    # if_else() is particularly useful for recoding dates
    # replace() can be a shortcut for ifelse() logical recoding
  mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg)) %>% 
  
  # make a case_def column using case_when()
    # remember that in case_when(), all of the right-side values must be of the 
    # same class (numeric, integer, character, date, logical, etc). Sometimes, 
    # this requires assigning variations of NA such as NA_character_, 
    # NA_real_ (for numeric), and as.Date(NA)
  mutate(case_def = case_when(
    lab_confirmed == TRUE ~ "Confirmed",
    epilink == "yes" & fever == "yes" ~ "Suspect",
    TRUE ~ "To investigate"
  ), .after = case_id) %>% # choose where new column will be placed 
  
  # make an age_years column
  mutate(age_years = case_when(
    age_unit == "months"   ~ age/12,
    age_unit == "years"    ~ age,
    is.na(age_unit)        ~ age), # assume age_unit is years if it is missing
    .after = age) %>% 
  
  # make age_cat column with age_categories function
  # mutate(age_cat = age_categories(age_years, breakers = c(0, 5, 10, 15, 20, 30, 40, 50)), 
  mutate(age_cat = age_categories(age_years, lower = 0, upper = 70, by = 10), 
         .after = age_years) %>% 
  
  # add a column to calculate days lapsed b/w date_onset and date_report
  # mutate(diff = as.numeric(date_report - date_onset)) (for pure 
  # numeric output vs '4 days')
  # In your work context, you may want to add a +1 after the end of 
  # the as.numeric() command, if you prefer same day admission to 
  # be recorded as 1 and not 0.
  mutate(diff = date_report - date_onset) %>% 
  
  # creating column to tell us if district of residence is different
  # than district of detection
  # New column will be TRUE if the two values are different
  mutate(moved = district_res != district_det) %>% 
  
  # make new district column using district_det values. If NA, then values
  # from district_res are used
  mutate(district = coalesce(district_det, district_res)) %>% 
  
  # filtering rows where case_def is confirmed
  # filtering will by default remove missing values
  # To keep rows with missing values, add an “OR” bar and then the is.na()
  # filter(case_def == "Confirmed" | is.na(case_def))
  filter(case_def == "Confirmed") %>% 
  
  # reordering columns
  select(case_id, starts_with("date_"), diff, gender, age, age_unit, age_years,
         age_cat, hospital, district, contains("district"), moved, everything())

  



## Export data -------------------------------------------------------------

# Export cleaned file to the root folder of the R project 
export(surv, "surveillance_linelist_clean_20141201.csv")  



# If you save the file as .xlsx or .csv, any person using R to further analyze 
# the data will have to re-clean it by again specifying the column classes. 
# To avoid this, save the clean dataset as an “.rds” file. RDS is a file format 
# specific to R, and is very useful if you know you will work with the 
# exported data again in R.
export(surv, here("data", "clean", "surveillance_linelist_clean_20141201.rds"))


## Testing Code ------------------------------------------------------------
surv %>% 
  tabyl(moved)

# na_if()
surv1 <- surv %>% 
  mutate(gender = na_if(gender, "male")) %>% 
  mutate(hospital = na_if(hospital, "Other"))

names(surv)

surv %>% 
  # select also reorders columns
  select(epilink, age, gender)

surv %>% 
  # use a colon to keep the two named columns and all those in between
  select(fever:vomit)

surv %>% 
  # can also list some columns, to move them to the beginning, 
  # and finish with everything() to include all the other columns 
  select(fever, gender, date_report, everything())

surv %>% 
  # use minus sign to drop a column
  select(-epilink)

#surv %>% 
  # delete multiple columns by wrapping them in a vector
  #select(-c(row_num, admin3pcod, epilink))

surv %>% 
  # careful, this will delete all other columns beside case_id
  distinct(case_id)

surv %>% 
  # now all columns are retained, and only rows with duplicate case_id
  # are dropped
  distinct(case_id, .keep_all = T)

# look at the first six values of date_onset in the data frame surv
head(surv$date_report)

# plot a histogram of dates using the cleaned dateset and date column
ggplot(data = surv, mapping = aes(x = date_onset)) +
  geom_histogram()

class(surv$age)

surv_raw %>% 
  tabyl(gender)

surv %>% 
  tabyl(gender)

surv_raw %>% 
  tabyl(fever)

# number of rows where is.na(surv$date_onset) evaluates to TRUE
sum(is.na(surv$date_onset))

# number of rows where is.na(surv$date_report) evaluates to TRUE
sum(is.na(surv$date_report))

surv %>% 
  tabyl(hospital)


# row verb functions:
  # filter()
  # arrange() # alter order of rows
  # distinct() # remove duplicate rows

# column verb functions:
  # mutate()
  # select()
  # rename()

# mutate(new_column = logical, yes, no, .after =, .before =, .keep =, .by =)

summary(surv$wt_kg)


## Extra practice ----------------------------------------------------------

# Create a separate data frame for children and save it to the “outputs” 
# folder as “children_spotlight.csv”
  # Start from the clean data frame surv and assign this new data 
  # frame with the name children
  # Reduce the data frame to only contain cases with age less than 18 years
  # Create a new column infant that says “Infant” if the case is 
  # less than 1 year old, and says “Non-infant” otherwise
  # Reduce the data frame to only show the columns case_id, age_years, 
  # infant, gender, district, hospital, and date_report
  # In a separate command, save this children data frame to the root 
  # folder of your RStudio project as “children_spotlight.csv”


children <- surv %>% # start with cleaned surv file

  filter(age_years < 18) %>%
  
  mutate(infant = ifelse(age_years < 1, "infant", "non-infant")) %>%
  
  select(case_id, age_years, infant, gender, district, hospital, date_report)

# save
export(children, "children_spotlight.csv")
  


# Your GIS team needs a data frame to put into their routine maps for this 
# outbreak. They use R, so they expect a .rds file containin the case_id, 
# any columns containing information on location or whether the patient 
# moved significantly from their residence before being detected.

# They have access to your RStudio project and expect the file to be saved 
# into the “outputs” sub-folder with the current date of the data 
# at the end of the file name.
  
  
gis <- surv %>% 
  
  select(case_id, contains("district"), moved, lat, lon)

export(gis, here("outputs", "gis_ebola_cases_2014-12-01.rds"))


# stringr
  # str_detect() returns TRUE or FALSE depending on whether particular 
  # characters are found within a specified value
  # str_detect(string = column/text to search in, pattern = char pattern to search for)
  # str_detect(string = name, pattern = "Isha") will return TRUE if the 
  # characters “Isha” are found in a particular row of the colunm name
  # case sensitive!
  # To ignore case/capitalization, wrap the pattern within regex(), and within 
  # regex() add the argument ignore_case = TRUE (or T as shorthand).
  # str_detect(string = "Teacher", pattern = regex("teach", ignore_case = T))
  # to sum how many contain string: sum(str_detect(occupations, "teach"))
  

# Now, starting with the clean data frame surv, create a new data frame 
# that contains only the cases reported from district whose names contains 
# “West”. Then export this as “western_district_cases.csv” to the “outputs” folder.

west <- surv %>% 
  
  filter(str_detect(string = district, 
                    pattern = "West"))

export(west, here("outputs", "western_districts_cases.csv"))


# You can search for multiple patterns at the same time, for example: 
# “Doctor” OR “Physician” OR “Surgeon” by using the “OR bar” in 
# your pattern string

str_detect(string = district, pattern = "West|North|west|north")


# %in% operator

demo <-  surv %>% 
  filter(hospital %in% c("Central Hospital", 
                         "Military Hospital", 
                         "Port Hospital"))

demo <-  surv %>% 
  filter(!hospital %in% c("Central Hospital", 
                         "Military Hospital", 
                         "Port Hospital"))


# rename with select()
test <- surv %>% 
  select(ID = case_id, Age = age_years)



# Summary Tables ----------------------------------------------
  # misc: look up functions of janitor package(e.g., chisq.test)

# importing cleaned data frame
surv <- import(here("data", "clean", "backup", "surveillance_linelist_clean_20141201.rds"))


## janitor package---- 

# The tabyl() function from {janitor} does this quickly by tabulating 
# unique values of a column provided to it. Easy customization can 
# follow, using the related adorn_() functions.

surv %>% tabyl(district)
surv %>% tabyl(district, show_na = FALSE)


# summary of districts
surv %>% 
  
  # make table
  tabyl(district, show_na = FALSE) %>% 
  
  # arrange rows in descending order by count (n)
  arrange(desc(n)) %>%
  # arrange(-n)
  
  # adds total count and percentage
  adorn_totals() %>% 
  
  # turns percentage column into true percentages 
  adorn_pct_formatting() 


# cross-tab of district and hospital
surv %>% 
  
  # make cross-tab
  tabyl(district, hospital, show_na = FALSE) %>% 
  
  # adds row of totals (use where=both for column of totals)
  adorn_totals() %>% 
  
  # converts counts to decimal proportions
  adorn_percentages() %>% 
  
  # converts decimal percentages to % format
  adorn_pct_formatting() %>% 
  # adorn_pct_formatting(digits = 0)
  
  # counts AND percents
  adorn_ns()
  # adorn_ns("front")
  # adorn_ns("rear")



# exploring tabyl()
hosp_age <- surv %>% 
        tabyl(hospital, age_cat, show_na = FALSE) %>% 
        adorn_totals() %>% 
        adorn_percentages() %>% 
        adorn_pct_formatting(digits = 0)




## dplyr package ----

hospital_counts <- surv %>% 
  # tabulate counts
  # tabyl is this + proportions
  count(hospital)
  # This step is to show you, that you in are in fact creating another 
  # data frame! You could conduct analyses on this data frame, or even 
  # export it as a csv or xlsx file

surv %>% 
  
  # show counts of all combinations of 2 variables
  count(district, gender) %>% 
  arrange(-n)


# The functions group_by() and summarise() together are the most versatile 
# tool to make a new summary data frame to hold summary statistics.
surv %>% 
  group_by(hospital) %>% 
  
  summarise(
    # n() is a function that counts the number of rows
    # making a column for number of rows of each hosp group
    n_rows = n(),
    # mean age in each group
    age_avg = mean(age_years, na.rm = TRUE),
    # latest onset date
    max_onset = max(date_onset, na.rm = TRUE),
    female = sum(gender == "female", na.rm = TRUE),
    na_gender = sum(is.na(gender))) 
    # You can use sum() within summarise() to return the number of rows that 
    # meet a logical criteria. The expression within is counted if it 
    # evaluates to TRUE.EX:
      # sum(age_years < 18, na.rm=T)
      # sum(gender == "male", na.rm=T)
      # sum(response %in% c("Likely", "Very Likely"))


# PERCENTS
# Once you have defined a column within summarise(), you can reference in 
# lower parts of the same summarise() command.
surv %>% 
  group_by(hospital) %>% 
  summarise(
    n_cases = n(),
    male = sum(gender == "male", na.rm = TRUE),
    male_pct = percent(male/n_cases) # use percent function to get percent format
  )

# ROUNDING
surv %>% 
  group_by(hospital) %>% 
  summarise(
    mean_age = round(mean(age_years, na.rm = T), digits = 0)
  )


# CONDITIONAL STATS
# One of the more advanced calculation tools you can use are 
# subsetting brackets [ ]. These symbols can be used after a column 
# and will filter it according to whatever logical criteria you 
# write inside.
# For example, if placed within summarise(), 
# max_temp_fvr = max(temp[fever == "yes"], na.rm = T) 
# will return the maximum temperature recorded in the group, 
# but only among the cases that did report a fever.
surv %>% 
  group_by(hospital) %>% 
  summarise(
    max_temp_fvr = max(temp[fever == "yes"], na.rm = T),
    max_temp_nofvr = max(temp[fever == "no"], na.rm = T)) # %>% 
  # qflextable()


# Exercise:
# Create a data frame that summarises the following, for each hospital:
# Number of cases
# The most recent onset date
# Number of cases under age 5 years
# The percent of cases that reported vomiting
# The maximum weight, among male cases

hospital_info <- surv %>%
  
  # group by hospital
  group_by(hospital) %>% 
  
  # creating summary data
  summarise(
    n_cases = n(),
    max_onset = max(date_onset, na.rm = TRUE),
    n_under5 = sum(age_years < 5, na.rm = T),
    pct_vomit = percent(sum(vomit == "yes", na.rm = T)/n_cases),
    maxwt_male = max(wt_kg[gender == "male"], na.rm = TRUE)
  )



# FLEXTABLE
# The {flextable} package can accept a data frame in R and convert it into 
# an HTML table that can be saved as a pretty table file.
# This is useful if you create a table with {janitor} or with {dplyr}, 
# but want to put it in an HTML report, a Word document or PDF, or 
# even just print it as a PNG image.


table <- surv %>% 
  group_by(hospital) %>%                                     # get statistics for each hospital
  summarise(
    n_cases   = n(),                                         # number of rows (cases)
    max_onset = max(date_onset, na.rm = T),                  # latest onset date
    under5    = sum(age_years <= 5, na.rm = T),              # number of children under 5
    vomit_n   = sum(vomit == "yes", na.rm=T),                # number vomiting
    vomit_pct = percent(vomit_n / n_cases),                  # percent vomiting
    max_wt_male = max(wt_kg[gender == "male"], na.rm = T)) %>%     # max weight among men
  qflextable()


# borders and backgrounds for table:
# https://epirhandbook.com/en/tables-for-presentation.html#tables-for-presentation


# save table and export it
# dont pipe previous code to these functions
# save_as_docx()
# save_as_pptx()
# save_as_image()
# save_as_html()

save_as_docx(table, path = "hospital_table.docx")

# To save as a PNG image, you will need to install Phantom JS (free) 
# for the save_as_image() function to work. You can do this by 
# installing the package webshot with your pacman command, 
# then running the command webshot::install_phantomjs()
pacman::p_load(webshot)
webshot::install_phantomjs(force=TRUE)
save_as_image(table, path = "hospital_table.png")


# GTSUMMARY
# important to first select the columns you want

surv %>% 
  select(hospital, case_def, gender, age_years) %>% 
  tbl_summary()

# can also add a by = argument to the tbl_summary() command, and 
# designate a column to stratify the table (in columns)

table2 <- surv %>% 
  select(hospital, case_def, gender, age_years) %>% 
  tbl_summary(by = gender) %>% 
  # add_p() %>%  # add p values
  as_gt() # must turn into gt table to save

# save and export
gt::gtsave(table2, "table.docx")





# Extra practice:
# https://epirhandbook.com/en/tables-for-presentation.html#tables-for-presentation

pacman::p_load(
  rio,            # import/export
  here,           # file pathways
  flextable,      # make HTML tables 
  officer,        # helper functions for tables
  tidyverse)      # data management, summary, and visualization

# define style for border line
border_style = officer::fp_border(color="black", width=1)

surv %>% 
  group_by(hospital) %>%                                     # get statistics for each hospital
  summarise(
    n_cases   = n(),                                         # number of rows (cases)
    max_onset = max(date_onset, na.rm = T),                  # latest onset date
    under5    = sum(age_years <= 5, na.rm = T),              # number of children under 5
    vomit_n   = sum(vomit == "yes", na.rm=T),                # number vomiting
    vomit_pct = percent(vomit_n / n_cases),                  # percent vomiting
    max_wt_male = max(wt_kg[gender == "male"], na.rm = T)) %>%     # max weight among men
  qflextable() %>% 
  
  # specify widths of columns
  width(j = 1, width = 1) %>% 
  width(j = 7, width = .5) %>% 
  
  # add new meta header
  add_header_row(
    top = TRUE,
    values = c(
      "Location",
      "Case Data",
      "",
      "",
      "",
      "",
      "")) %>% 
  
  # label columns
  set_header_labels(
    hospital = "Hospital",
    n_cases = "Number of Cases",
    max_onset = "Latest Onset Date",
    under5 = "Cases Under 5yo",
    vomit_n = "Cases with Vomiting",
    vomit_pct = "% Cases with Vomiting",
    max_wt_male = "Max Weight (Males)") %>%
  
  merge_at(i=1, j=2:7, part = "header") %>% 
  
  # remove default border
  border_remove() %>% 
  
  # add lines from pre-defined functions
  # theme_alafoli()
  # theme_box()
  theme_booktabs() %>% 
  
  # add custome vertical line
  vline(part = "all", j=1, border = border_style) %>% 
  # add custom horizontal line
  hline(part = "header", border = border_style) %>% 
  
  
  # align text as you wish
  align(align = "center", j = c(2:7), part = "all") %>% 
  
  # bold and increase header font size
  fontsize(i = 1, size = 12, part = "header") %>% 
  bold(i = 1, bold = TRUE, part = "header") %>% 
  bold(j = 1, bold = TRUE, part = "body") %>% 
  
  # conditional formating of background colors
  # highlight values that meet certain codition
  bg(j = 6, i = ~ vomit_pct >= 50, part = "body", bg = "red") %>% 
  # highlight row that meet certain criterion
  bg(., i = ~ hospital == "Port Hospital", part = "body", bg = "#91c293")





# Data Visualization ------------------------------------------------------

## Simple plots ----

ggplot(data = surv,
       mapping = aes(
         x = district,
         y = age_years
       )) +
  # geom_point()
  geom_jitter(
    # color = "blue",
    # size = 2,
    # shape = 3
  ) +
  # geom_boxplot(
  #   alpha = .5
  # )
  geom_violin(
    alpha = .5
  )


ggplot(data = surv,
       mapping = aes(
         x = gender,
         y = ht_cm
       )) +
  geom_violin(
    alpha = .5, 
    size = .5)


# static aesthetics
ggplot(data = surv,
       mapping = aes(
         x = district, 
         y = age_years
       )) +
  geom_jitter() +
  geom_boxplot(
    fill = "blue",
    alpha = 0.5
  )


# dynamic aesthetics
ggplot(data = surv,
       mapping = aes(
         x = district,
         y = age_years, 
         fill = district,
         #color = district
       )) + 
  geom_jitter() +
  geom_boxplot(
    alpha = 0.5
  )


## labels (and indenting) ----
ggplot(                          # open the ggplot
  data = surv,                   # use surveillance linelist
       mapping = aes(            # assign columns to plot aesthetics
         x = district,             # district on the x-axis
         y = age_years,            # age on the y-axis
         fill = district,          # fill of boxplots by district
         # color = district
       )) + 
  
  geom_jitter() +                # add jittered points
  
  geom_boxplot(                  # add boxplots, semi-transparent
    alpha = 0.5
  ) +
  
  labs(                          # add labels to plot
    title = "My title",
    subtitle = "Subtitle here",
    x = "District",
    y = "Age (years)",
    caption = "Among an Ebola outbreak, 2014",
    fill = "District"
  )



# dynamic labels
ggplot(
  data = surv,
  mapping = aes(
    x = date_onset,
    fill = hospital
  )
) +
  
  geom_histogram(
    #alpha = .75
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks",
    labels = label_date_short()
    #date_labels = "%b %d \n %Y"
  ) +
  
  scale_fill_brewer(
    type = "qual",
    na.value = "grey50"
  ) +
  
  labs(
    title = "Epidemic curve of Ebola outbreak",
    subtitle = "Confirmed cases, 2014",
    x = str_glue("{fmt_count(surv, is.na(date_onset))} cases 
                 are missing onset date and are not shown"),
    y = "Number of cases",
    caption = str_glue("Data as of {Sys.Date()}"),
    fill = "Hospital"
  )



## Bar plots ----

# with linelist data:
# show cases by district
surv %>% 
  ggplot(
    aes(
      x = district
    )
  ) + 
  geom_bar(
    fill = "lightblue"
  )


# now add distribution of age by district
surv %>% 
  ggplot(
    aes(
      x = district,
      fill = age_cat
    )
  ) + 
  geom_bar(
    #fill = "lightblue"
  )

# now reverse order of the age categories
surv %>% 
  ggplot(
    aes(
      x = district,
      fill = fct_rev(age_cat)
    )
  ) + 
  geom_bar(
    #fill = "lightblue"
  )


# geom_bar(position = "dodge")
surv %>% 
  ggplot(
    aes(
      x = district,
      fill = fct_rev(age_cat)
    )
  ) + 
  geom_bar(
    position = "dodge" # to stack bars side by side
  )




# Bar plots from count data:
# Create a new dataset that includes only the total case counts per district.

case_counts_district <- 
  surv %>% 
  group_by(district) %>% 
  summarise(n_cases = n())
case_counts_district

# DONT use geom_bar() with count data:
ggplot(data = case_counts_district, 
       mapping = aes(x = district))+
  geom_bar()

# DO use geom_col():
ggplot(data = case_counts_district, 
       mapping = aes(x = district,
                     y = n_cases))+
  geom_col()
# The function geom_col() is built to handle count data - 
# it does not stack the dataset rows in top of each other (like geom_bar()), 
# but rather accepts a y = column of count values, and draws 
# the columns with heights that correspond to those values.



## Scatterplots ----

surv %>% 
  ggplot(
    aes(x = age_years,
        y = ht_cm,
        color = gender)
  )+
  geom_point()


## ggplot extension packages
# ggExtra
# draws distributions in the “margins” (outsides) of the plot

height_plot <- ggplot(data = surv, 
                      mapping = 
                        aes(x = age_years, 
                            y = ht_cm, 
                            color = gender))+
  geom_point()

ggMarginal(height_plot, groupFill = TRUE)


# add smoothed means to a scatterplot
# Finally, try another scatterplot, using date_report on the X-axis 
# and diff on the Y-axis. Add geom_smooth() to see a trend line.
surv %>% 
  ggplot(
    aes(
      x = date_report,
      y = diff
    )
  )+
  geom_point()+
  geom_smooth()


## Histograms ----
# used to show the distribution of continuous variables
# so If you have a column with numbers, or dates, it is better to use 
# geom_histogram() (not geom_bar() or geom_col()).

surv %>% 
  ggplot(
    aes(
      x = date_onset
    )
  )+
  geom_histogram(
    fill = "darkgreen"
  )


# stacked histograms
surv %>% 
  ggplot(
    aes(
      x = age_years,
      fill = gender
    )
  )+
  geom_histogram()


# histogram breaks
# size and freq of histogram bins can be adjusted
surv %>% 
  ggplot(
    aes(
      x = age_years,
      fill = gender
    )
  )+
  geom_histogram(
    bins = 30,
    #binwidth = 5
  )


## Facets ----
# small-multiples

surv %>% 
  ggplot(
    aes(
      x = date_onset,
      fill = gender
    )
  )+
  geom_histogram()+
  facet_wrap(~district)


## gghighlight ----
surv %>% 
  ggplot(
    aes(
      x = date_onset,
      fill = district
    )
  )+
  geom_histogram()+
  facet_wrap(~district)+
  gghighlight()
  
# now, no faceting, and highlight cases from West II
surv %>% 
  ggplot(
    aes(
      x = date_onset,
      fill = district
    )
  )+
  geom_histogram()+
  gghighlight(district == "West II")
  
  
## Color scales ----

# scale_AESTHETIC_METHOD()
 
# using the default colors and axis breaks
ggplot(
  data = surv,
  mapping = aes(
    x = district,
    fill = gender
  ))+
  geom_bar()

# adjust the fill color of the bars manually (scale_fill_manual())
ggplot(
  data = surv,
  mapping = aes(
    x = district,
    fill = gender
  ))+
  geom_bar()+
  scale_fill_manual(
    values = c(
      "male" = "dodgerblue",
      "female" = "tomato"),
    na.value = "grey")
    
  
# built-in color scales
ggplot(
  data = surv,
  mapping = aes(
    x = district,
    fill = age_cat)) +
  geom_bar() +
  scale_fill_brewer(
    palette = "Pastel2",
    na.value = "grey"
  )

# color-blind friendly palette
ggplot(
  data = surv,
  mapping = aes(
    x = district,
    fill = age_cat
  )
)+
  geom_bar()+
  scale_fill_viridis_d(# d for discrete
    na.value = "grey")

# continuous color scales
ggplot(
  data = surv,
  mapping = aes(
    x = age_years,
    y = wt_kg,
    color = temp
  )
)+
  geom_point()+
  scale_color_viridis_c(# c for continous
    # option = "inferno",
    # option = "plasma",
    na.value = "grey"
  )

# Viridis (try with option = "plasma" or “inferno”), and 
# colorbrewer palette functions can be added to any ggplot.



## Axes scales ----

# set y axis breaks
ggplot(
  data = surv,
  mapping = aes(
    x = district,
    fill = gender)) +
  geom_bar() +
  scale_fill_viridis_d(na.value = "grey")+
  scale_y_continuous(breaks = seq(from = 0, to = 250, by = 25))

# starting scales at 0
# set y axis breaks
ggplot(
  data = surv,
  mapping = aes(
    x = district,
    fill = gender)) +
  geom_bar() +
  scale_fill_viridis_d(na.value = "grey")+
  scale_y_continuous(breaks = seq(from = 0, to = 250, by = 25),
                     expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))
  
# flip axes
ggplot(
  data = surv,
  mapping = aes(
    x = district,
    fill = gender)) +
  geom_bar() +
  scale_fill_viridis_d(na.value = "grey") +
  scale_y_continuous(breaks = seq(from = 0,
                                  to = 250,
                                  by = 25),
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0))+
  coord_flip()


## Date axis labels ----
ggplot(
  data = surv,
  mapping = aes(
    x = date_onset
  )
)+
  geom_histogram()+
  
  # manual date breaks
  scale_x_date(
    date_breaks = "2 weeks",
    # format date labels with strptime syntax
    #date_labels = "%d %b \n %Y"
    # format date labels with built-in labels with scales package
    labels = label_date_short()
  )
    # %d = Day number of month (5, 17, 28, etc.)
    # %j = Day number of the year (Julian day 001-366)
    # %a = Abbreviated weekday (Mon, Tue, Wed, etc.)
    # %A = Full weekday (Monday, Tuesday, etc.)
    # %w = Weekday number (0-6, Sunday is 0)
    # %u = Weekday number (1-7, Monday is 1)
    # %W = Week number (00-53, Monday is week start)
    # %U = Week number (01-53, Sunday is week start)
    # %m = Month number (e.g. 01, 02, 03, 04)
    # %b = Abbreviated month (Jan, Feb, etc.)
    # %B = Full month (January, February, etc.)
    # %y = 2-digit year (e.g. 89)
    # %Y = 4-digit year (e.g. 1989)
    # %h = hours (24-hr clock)
    # %m = minutes
    # %s = seconds
    # %z = offset from GMT
    # %Z = Time zone (character)


## Display percents ----

# create a dataset using group_by() and summarise() that creates 
# a proportion - the weekly proportion of cases that have more 
# than 7 days delay between symptom onset and their report date.
delay_1wk <- surv %>% 
  mutate(diff_1wk = as.numeric(diff) > 7) %>% # create column that is TRUE is diff is greater than 7
  group_by(week = floor_date(date_report, "week")) %>% # create column "week" and group by it  
  summarise(
    cases = n(), # number of total cases that week
    delayed = sum(diff_1wk == TRUE, na.rm = TRUE), # number of delayed cases that week
    delayed_pct = delayed/cases) # proportion of delayed cases

# adjust axes to fluidly display percents
ggplot(
  data = delay_1wk,
  mapping = aes(
    x = week,
    y = delayed_pct
  )
)+
  geom_line(
    size = 2,
    color = "brown"
  )+
  scale_y_continuous(
    labels = percent
  )


## Plot labels ----

# static labels
ggplot(
  data = delay_1wk,
  mapping = aes(
    x = week,
    y = delayed_pct
  )
)+
  geom_line(
    size = 2,
    color = "brown")+
  scale_y_continuous(
    labels = percent
  )+
  labs(
    # use \n within the quotes to start a new line!
    caption = "n = 663. Report produced on 2022-04-02. \n Data collected from 5 major hospitals in the epidemic-affected area. \n Last reported case on 2014-12-21. \n 7 cases missing date of onset."
  )
  

# dynamic labels
# stringr::str_glue() allows us to embed code within character strings
ggplot(
  data = delay_1wk,
  mapping = aes(
    x = week,
    y = delayed_pct
  )
)+
  geom_line(
    size = 2,
    color = "brown")+
  scale_y_continuous(
    labels = percent
  )+
  labs(
    caption = str_glue("n = {nrow(surv)}. 
                        Report produced on {Sys.Date()}.
                        Data collected from {length(unique(surv$hospital))-2} major hospitals in the epidemic-affected area. 
                        Last reported case on {max(surv$date_report, na.rm = TRUE)}.
                       {fmt_count(surv, is.na(date_onset))} cases missing date of onset.")
  )


## Theme elements ----
# https://ggplot2.tidyverse.org/reference/ggtheme.html
ggplot(
  data = delay_1wk,
  mapping = aes(
    x = week,
    y = delayed_pct
  )
)+
  geom_line(
    size = 2,
    color = "brown")+
  scale_y_continuous(
    labels = percent
  )+
  #theme_bw()
  theme_classic(
    base_size = 16 #quickly increase text sizes
  )
  # theme_dark()
  # theme_gray()
  # theme_minimal()
  # theme_light()
  # theme_void()

# micro-adjustments to themes
# run, theme_get() in your R window to 
# get a list of all theme arguments in the console.
ggplot(data = surv,
       mapping = aes(
         x = age_years,
         y = ht_cm,
         color = gender)) +
  geom_point(
    alpha = 0.7) +
  scale_color_brewer(
    palette = "Pastel2",
    na.value = "grey") +
  labs(
    title = "Height and age",
    subtitle = "All hospitals",
    x = "Age (years)",
    y = "Height (cm)",
    caption = "Fictional Ebola data",
    color = "Gender"
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "bottom",                # move legend to bottom
    plot.title = element_text(color = "red",   # title color
                              size = 20,       # title font size
                              face = "bold"),  # title typeface
    axis.title.y = element_text(angle = 0))    # rotate y axis title to be horizontal
  # The legend position can also be set more specifically with c(x,y)
  # Most other theme elements can also be turned off using element_blank() 
  # e.g. to turn off minor y-axis grid lines and legend title:


# Export plots/ggsave
# ggsave(my_plot, "my_plot.png")
# ggsave("my_plot.png") - saves last plot that was printed
# can export as png, pdf, jpeg, tiff, bmp, svg, or several other file types
# width, height, units, dpi...all arguments of ggsave

ggsave(here("outputs", "testplotprint.png"))


# ggthemes
# {ggthemes} package contains themes that will make your plots look like they 
# are from The Economist, The Wall Street Journal, Tufte, and even STATA!
# https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
ggplot(data = surv,
       mapping = aes(
         x = age_years,
         y = ht_cm,
         color = gender)) +
  geom_point(
    alpha = 0.7) +
  scale_color_brewer(
    palette = "Pastel2",
    na.value = "grey") +
  labs(
    title = "Height and age",
    subtitle = "All hospitals",
    x = "Age (years)",
    y = "Height (cm)",
    caption = "Fictional Ebola data",
    color = "Gender"
  ) +
  theme_wsj()







# Joining data ------------------------------------------------------------

# Putting this here in writing so that everyone can refer to it:
#   - inner_join merges two datasets and keeps the rows that match 
#     in both, including all the columns from each dataset.
#   - semi_join filters the first dataset for rows that have a match 
#     in the second dataset but only keeps the columns from the first dataset.
# For an epidemiologist:
#   - Use inner_join to merge patient details with their test results 
#     into one comprehensive dataset.
#   - Use semi_join to extract the records of patients from a larger 
#     dataset who have specific characteristics found in another dataset, 
#     like those who have tested positive for a certain condition.


## Binding rows ----

# same as stacking rows on top of one another
# datasets must have the same columns and column names
# dplyr::bind_rows()
# columns do NOT need to be in the same order


# binding 6 hospital dfs into one 
hosp <- bind_rows(hosp_central, hosp_military, hosp_missing,
                  hosp_other, hosp_port, hosp_smmh) %>% 
  select(-c(age, age_unit, gender, hospital), # drop these
         case_id = ID) # select and rename



## Joining data ----

# To note, the most common joins in public health 
# contexts are left joins and anti joins.
# https://epirhandbook.com/en/joining-data.html#joining-data

# join hosp df with surv df
  # inspect before joining.

# review dimensions
# dim() returns # rows and columns
# can also use nrow() and ncol()
dim(hosp)
dim(surv)

# review duplicates
# show the surv case_ids that appear more than once
surv %>% 
  count(case_id) %>% 
  filter(n > 1)
# do the same for hosp
hosp %>% 
  count(ID) %>% 
  filter(n > 1)

# review identifier values
# it is extremely important to consider whether 
# the column(s) used to match the two data frames are clean.
# the values need to be exactly the same in both data frames to be matched.

# for each dataset, sort and print the first 10 identifier values
surv %>% 
  arrange(case_id) %>% 
  pull(case_id) %>% # returns only the case_id column
  head(10) # takes first 10 values

hosp %>% 
  arrange(ID) %>% 
  pull(ID) %>% 
  head(10)

# review overlap
anti_join1 <- hosp %>% 
  anti_join(surv, by = c("ID" = "case_id"))
  #  there are some patients in hosp that are not present in surv

anti_join2 <- surv %>% 
  anti_join(hosp, by = c("case_id" = "ID"))
  # returns 0 rows; all ids from hosp are present in surv


# review column names
surv %>% 
  colnames()
hosp %>% 
  colnames()


# join dataframes
test_join <- surv %>% 
  left_join(hosp, by = c("case_id" = "ID"))
dim(test_join)

# are the values in both gender.x and gender.y the same?
test_join %>% 
  tabyl(gender.x, gender.y)


# for this exercise, remove redundant columns before merging
# see binding rows, added select() to pipeline

# now combine hosp and surv
combined <- surv %>% 
  left_join(hosp, by = "case_id")

# join lab data with combined df

# review dimensions
dim(combined)
dim(lab)

# review duplicates
lab %>% 
  count(case_id) %>% 
  filter(n > 1)
combined %>% 
  count(case_id) %>% 
  filter(n > 1)

# review identifiers
lab %>% 
  arrange(case_id) %>% 
  pull(case_id) %>% 
  head(10)
combined %>% 
  arrange(case_id) %>% 
  pull(case_id) %>% 
  head(10)

# review overlap
combined %>% 
  anti_join(lab, by = "case_id")
  # so, 8 rows in combined that aren't in lab

lab %>% 
  anti_join(combined, by = "case_id")
  # all rows in lab are found in combined

combined <- combined %>% 
  left_join(lab, by = "case_id")
# sum(is.na(combined$blood_ct))


# join case investigations data frame
# import data - see import data section
# review data:
dim(investigations)
dim(combined)

combined %>% 
  count(case_id) %>% 
  filter(n > 1)
investigations %>% 
  count(case_id) %>% 
  filter(n > 1)

combined %>% 
  arrange(case_id) %>% 
  pull(case_id) %>% 
  head(10)
investigations %>% 
  arrange(case_id) %>% 
  pull(case_id) %>% 
  head(10)

combined %>% 
  anti_join(investigations, by = "case_id")
investigations %>% 
  anti_join(combined, by = "case_id")

# join investigations to combined
combined <- combined %>% 
  left_join(investigations, by = "case_id")


## Post-join cleaning ----
combined2 <- combined %>% 
  
  # ensure clean variable names
  clean_names() %>% 
  
  # change new date variables to date class
  mutate(date_hospitalisation = mdy(date_hospitalisation),
         date_infection = ymd(date_infection),
         date_outcome = mdy(date_outcome)) %>% 
  
  # fill in NA for missing values of outcome/hospital
  mutate(outcome = na_if(outcome, ""),
         hospital = na_if(hospital, ""))
  
  
  
# CLEAN SCRIPT
# Load packages
# Import data
# Clean primary data frame
# Join other data (and perform any residual cleaning)
# Tables and visualizations


# EXTRAS: TIPS AND TRICKS
  # File paths: 
    # press TAB when writing a file path for auto-complete options!
    import(here("data/")) # press tab after /
    
  # Keyboard shortcuts: 
    # https://epirhandbook.com/en/r-basics.html#keyboard-shortcuts



# Pivoting - patient timelines --------------------------------------------

# Import data
  combined <- import(here("data", "clean", "backup", "linelist_combined_20141201.rds"))

# Creating a small df to examine timelines of 5 patients
  # Select cases
    # Sort the dataset so the cases with earliest onset are the top
    # Filter to only the top 5 rows
    # Select only the columns case_id and any column that begins with “date”
  timelines <- combined %>% 
    arrange(date_onset) %>% 
    head(5) %>% 
    select(case_id, starts_with("date"))

## Pivot longer ----
  # to use this dataset in ggplot we need to pivot it to long format
  # results in df with 3 columns: case_id, date_type, date
  # pivot_longer(cols = vector of columns to pivot)
  timelines_long <- timelines %>% 
    pivot_longer(cols = starts_with("date"), # columns to pivot
                 names_to = "date_type",     # create name of first new column
                 values_to = "date")         # create name of other new column


# Plotting
    ggplot(
      data = timelines_long,
      aes(
        x = date,
        y = case_id,
        color = date_type,  # color of the points
        shape = date_type,  # shape of the points
        group = case_id))+  # make lines appear by color
      
      geom_point(size = 4)+ # show points
      geom_line()+          # show lines
      theme_minimal()


## Factors ----
  # https://epirhandbook.com/en/factors.html
  # A factor has “levels”, such that the 
  # values are ordered (first, second, third, fourth, etc.)
  # we want dates in certain order:
  # infection, onset, report, hospitalization, outcome
    
# check current class of date_type
  class(timelines_long$date_type)
    # currently character, not factor
    # change this using forcats::fct_relevel()
      # converts to factor and lets you set the order

# set as factor
    # Pivot dates longer
    timelines_long <- timelines %>% 
      
      # pivot the dataset longer
      pivot_longer(
        cols = starts_with("date"),
        names_to = "date_type",
        values_to = "date") %>% 
      
      # set new column date_type as class factor,
      # and define order to its values
      mutate(date_type = fct_relevel(date_type, 
                                     "date_infection",
                                     "date_onset",
                                     "date_report",
                                     "date_hospitalisation",
                                     "date_outcome"))

# check new factor/levels
  class(timelines_long$date_type)
  levels(timelines_long$date_type)

  # now, rerun the plot above

# fct_lump()
  # will aggregate together values in a column 
  # into an “Other” category based on frequency.
  
  # example:
  # following plot is hard to read, so many districts in the fill
  ggplot(data = combined, 
         mapping = aes(
           x = date_onset,
           fill = district))+
    geom_histogram(binwidth = 7)
  # use fct_lump() and its variations like 
  # fct_lump_n() to reduce the number of district that are shown
  ggplot(data = combined,
         mapping = aes(
           x = date_onset,
           # keep only the 3 most-common districts
           fill = fct_lump_n(district, 3)
         ))+
    geom_histogram(binwidth = 7)+
    labs(fill = "District")
  
  
## Pivot wider ----
  # not a focus here since it is less common
  # more info:
  # https://epirhandbook.com/en/pivoting-data.html#long-to-wide
  # https://epirhandbook.com/en/descriptive-tables.html#tbls_pivot_wider
  
  
  
  
  


















