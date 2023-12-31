

#############################################
# Ebola outbreak case study
# Script after pivoting exercise
# Your NAME here
#############################################


# Load packages -----------------------------------------------------------
pacman::p_load(
     rio,          # for importing data
     here,         # for locating files
     skimr,        # for reviewing the data
     janitor,      # for data cleaning  
     lubridate,    # for date cleaning  
     epikit,       # creating age categories
     gtsummary,    # creating tables  
     scales,       # percents in tables  
     flextable,    # for making pretty tables
     gghighlight,  # highlighting plot parts  
     ggExtra,      # special plotting functions
     tidyverse     # for data management and visualization
)




# Import data -------------------------------------------------------------

# surveillance dataset
surv_raw <- import(here("data", "raw", "surveillance_linelist_20141201.csv"))

# hospital datasets
hosp_central  <- import(here("data", "raw", "hospitals", "20141201_hosp_central.csv"))
hosp_military <- import(here("data", "raw", "hospitals", "20141201_hosp_military.csv"))
hosp_other    <- import(here("data", "raw", "hospitals", "20141201_hosp_other.csv"))
hosp_port     <- import(here("data", "raw", "hospitals", "20141201_hosp_port.csv"))
hosp_smmh     <- import(here("data", "raw", "hospitals", "20141201_hosp_smmh.csv"))
hosp_missing  <- import(here("data", "raw", "hospitals", "20141201_hosp_missing.csv"))

# laboratory dataset
lab <- import(here("data", "raw", "lab_results_20141201.xlsx")) %>% 
     clean_names()

# Import lab data 
investigations <- import(here("data", "raw", "case_investigations_20141201.xlsx")) %>% 
     # remove unnecessary columns  
     select(-c(age, age_unit, gender))








# Exploratory analysis ----------------------------------------------------
# (contents may vary)

# column names
surv_raw %>% names()

# gender values
surv_raw %>% 
     tabyl(gender)

# check class of certain columns
class(surv_raw$`onset date`)

# compare consistency across two location columns
surv_raw %>% 
     tabyl(adm3_name_res, adm3_name_det)









# Clean surveillance data -------------------------------------------------
surv <- surv_raw %>% 
     
     # automatically clean column names
     clean_names() %>% 
     
     # manually clean column names   
     rename(
          date_onset = onset_date,
          date_report = date_of_report,
          district_res = adm3_name_res,
          district_det = adm3_name_det) %>%
     
     # remove unnecessary column
     select(-row_num) %>% 
     
     # de-duplicate rows  
     distinct() %>% 
     
     # convert date_onset to date class
     mutate(date_onset = mdy(date_onset)) %>% 
     mutate(date_report = mdy(date_report)) %>% 
     
     # convert age to numeric class
     mutate(age = as.numeric(age)) %>% 
     
     # convert "Unknown" gender to NA
     mutate(gender = na_if(gender, "Unknown")) %>% 
     
     # properly record missing values in many character columns
     mutate(across(.cols = where(is.character), .fns = na_if, "")) %>% 
     
     # re-code hospital column
     mutate(hospital = recode(hospital,
                              # for reference: OLD = NEW
                              "Mitilary Hospital"  = "Military Hospital",
                              "Port"               = "Port Hospital",
                              "Port Hopital"       = "Port Hospital",
                              "St. Mark's Maternity Hospital (SMMH)" = "SMMH")) %>%
     
     # recode gender
     mutate(gender = recode(gender,
                            "m" = "male",
                            "f" = "female")) %>% 
     
     # convert negative weight values to NA
     mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg))  %>% 
     
     # create case definition
     mutate(case_def = case_when(
          lab_confirmed == TRUE             ~ "Confirmed",
          epilink == "yes" & fever == "yes" ~ "Suspect",
          TRUE                              ~ "To investigate")) %>% 
     
     # create age-in-years
     mutate(age_years = case_when(
          age_unit == "months" ~ age/12,   # if age is given in months
          age_unit == "years"  ~ age,      # if age is given in years
          is.na(age_unit)      ~ age)) %>% # if unit missing assume years, else NA
     
     # create age category column
     mutate(age_cat = age_categories(         # create new column
          age_years,                             # numeric column to make groups from
          lower = 0,
          upper = 70,
          by = 10)) %>% 
     
     # Make date-difference column  
     mutate(diff = date_report - date_onset) %>% 
     
     # create column marking TRUE if district of residence and detection differ
     mutate(moved = district_res != district_det) %>% 
     
     # create new column that prioritizes district of detection
     mutate(district = coalesce(district_det, district_res)) %>% 
     
     # remove suspect cases
     filter(case_def == "Confirmed") %>% 
     
     # re-arrange columns
     select(case_id, starts_with("date"), diff, gender, age, age_unit, age_years, age_cat, hospital, district, district_res, district_det, moved, everything())


# Testing area ------------------------------------------------------------
# (contents may vary)



# Export clean surveillance dataset ---------------------------------------
rio::export(surv, here("data", "clean", "surveillance_linelist_clean_20141201.rds"))




# Create combined dataset -------------------------------------------------
# Join other datasets to the surveillance linelist
hosp <- bind_rows(hosp_central, hosp_port, hosp_military, hosp_smmh, hosp_other, hosp_missing) %>% 
     # select specific columns from hosp, and re-name ID as case_ID
     select(
          case_id = ID,          # select and rename
          date_hospitalisation,  # select
          time_admission,        # select
          date_outcome,          # select
          outcome)               # select



# Join the two data frames with a left-join
combined <- left_join(surv, hosp, by = "case_id")



# Join the surveillance and hospital data frames with a left-join
# (place this in the Joining data section of your script)
combined <- left_join(combined, lab, by = "case_id")



# Join the two data frames with a left-join
combined <- left_join(combined, investigations, by = "case_id")



# Clean the new columns that have been joined to 'combined'
combined <- combined %>% 
     
     # convert all column names to lower case and remove spaces
     clean_names() %>% 
     
     # covert new columns to class date
     mutate(date_hospitalisation = mdy(date_hospitalisation),
            date_outcome         = mdy(date_outcome),
            date_infection       = ymd(date_infection)) %>% 
     
     # clean outcome and hospital missings
     mutate(outcome = na_if(outcome, ""),
            hospital = na_if(hospital, ""))


# save the combined dataset
export(combined, here("data", "linelist_combined_20141201.rds"))











# Descriptive tables ------------------------------------------------------
# (contents may vary)

## Table using {janitor} ---------------------------------------------------
# counts by district
district_table <- surv %>% 
     tabyl(district) %>% 
     arrange(desc(n)) %>% 
     adorn_totals() %>% 
     adorn_pct_formatting() %>% 
     qflextable()

# print
district_table

# save
save_as_docx(district_table, path = "district_table.docx")



# Table using {dplyr} -----------------------------------------------------
# hospital table
hospital_table <- surv %>% 
     group_by(hospital) %>%                                     # get statistics for each hospital
     summarise(
          n_cases   = n(),                                         # number of rows (cases)
          max_onset = max(date_onset, na.rm = T),                  # latest onset date
          under5    = sum(age_years <= 5, na.rm = T),              # number of children under 5
          vomit_n   = sum(vomit == "yes", na.rm=T),                # number vomiting
          vomit_pct = percent(vomit_n / n_cases),                  # percent vomiting
          max_wt_male = max(wt_kg[gender == "male"], na.rm = T)) %>%     # max weight among men
     flextable::qflextable()

# print
hospital_table

# save
save_as_docx(hospital_table, path = "hospital_table.docx")


# Table using {gtsummary} -------------------------------------------------
# summary table by gender
surv %>% 
     select(district, age_cat, fever, chills, cough, aches, vomit, wt_kg, ht_cm, gender) %>% 
     tbl_summary(by = gender)















# Simple plots ------------------------------------------------------------
# (contents may vary)
# histogram of cases, with district highlights
ggplot(data = surv, mapping = aes(x = date_onset, fill = district))+
     geom_histogram()+
     facet_wrap(~ district)+
     gghighlight()

# bar plot of case counts
ggplot(
     data = surv,
     mapping = aes(
          x = district,
          fill = gender)) +
     geom_bar() +
     scale_fill_viridis_d(na.value = "grey") +
     scale_y_continuous(breaks = seq(from = 0,
                                     to = 3000,
                                     by = 500),
                        expand = c(0,0)) +
     scale_x_discrete(expand = c(0,0))+
     coord_flip()


# weekly proportion of cases with more than 7 days delay between onset and report
delay_1wk <- surv %>%                                         # begin with surveillance linelist
     mutate(diff_1wk = as.numeric(diff) > 7) %>%              # create column that is TRUE is diff is greater than 7
     group_by(week = floor_date(date_report, "week")) %>%     # create column "week" and group by it  
     summarise(                                               # begin summarise command     
          cases = n(),                                           # number of cases in the week
          delayed = sum(diff_1wk == TRUE, na.rm=T),              # number of delayed cases in the week 
          delayed_pct = delayed / cases)                         # calculate proportion

ggplot(data = delay_1wk, mapping = aes(x = week, y = delayed_pct))+
     geom_line(size = 2, color = "brown")

# plot with dynamic labels
ggplot(data = delay_1wk, mapping = aes(x = week, y = delayed_pct))+
     geom_line(size = 2, color = "brown")+
     labs(caption = str_glue(
          "n = {nrow(surv)}.\nReport produced on {Sys.Date()}\nData collected from {length(unique(surv$hospital))-2} major hospitals in the epidemic-affected area.\nLast reported case on {max(surv$date_report, na.rm = TRUE)}.\n{fmt_count(surv, is.na(date_report))} cases missing date of onset and not shown."))
















# Patient timeline visualisation ------------------------------------------
# make data frame of only the first 5 cases in the epidemic
timelines <- combined %>% 
     arrange(date_onset) %>%                 # sort dataset so that earliest are at the top
     head(5) %>%                             # keep only the top 5 rows
     select(case_id, starts_with("date"))    # keep only certain columns 

# Pivot date columns longer
timelines_long <- timelines %>% 
     
     # pivot the dataset longer
     pivot_longer(
          cols = starts_with("date"),
          names_to = "date_type",
          values_to = "date") %>% 
     
     # set the new column date_type as class factor, and define order for its values
     mutate(date_type = fct_relevel(date_type, "date_infection", "date_onset", "date_report", "date_hospitalisation", "date_outcome"))

# plot the long dataset to show timelines for each patient
timelines_long %>% 
     ggplot(data = timelines_long,
            mapping = aes(
                 x = date,
                 y = case_id,
                 color = date_type,
                 shape = date_type,
                 group = case_id))+
     geom_point(size = 4)+
     geom_line()+
     theme_minimal()



# Plot lumping together districts for plot (only show 3 most common and put others in "Other")  
ggplot(data = combined, 
       mapping = aes(
            x = date_onset,
            fill = fct_lump_n(district, 3)))+
     geom_histogram(binwidth = 7)+
     labs(fill = "District")
