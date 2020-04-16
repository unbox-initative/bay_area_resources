Bay Area Resource Fliers
================
Chris LeBoa
2020-04-16

  - [Read in sheet](#read-in-sheet)
      - [Parse by County](#parse-by-county)
  - [Write to Google Sheet](#write-to-google-sheet)

``` r
# Libraries

devtools::install_github("tidyverse/googlesheets4") 
```

    ## 
    ##      checking for file ‘/private/var/folders/fb/hzwrc6r97yg62jsf82404d040000gn/T/RtmprfhXOY/remotesea8c4bd5bc93/tidyverse-googlesheets4-97f4144/DESCRIPTION’ ...  ✓  checking for file ‘/private/var/folders/fb/hzwrc6r97yg62jsf82404d040000gn/T/RtmprfhXOY/remotesea8c4bd5bc93/tidyverse-googlesheets4-97f4144/DESCRIPTION’
    ##   ─  preparing ‘googlesheets4’:
    ##      checking DESCRIPTION meta-information ...  ✓  checking DESCRIPTION meta-information
    ##   ─  installing the package to process help pages
    ##   ─  checking for LF line-endings in source and make files and shell scripts (3.9s)
    ##   ─  checking for empty or unneeded directories
    ##   ─  looking to see if a ‘data/datalist’ file should be added
    ##   ─  building ‘googlesheets4_0.1.1.9100.tar.gz’
    ##      
    ## 

``` r
#needs the devtools version 
#not the cran version to run 

library(tidyverse)
library(googlesheets4)
library(tidyr)

# Parameters
sheet_link <- "https://docs.google.com/spreadsheets/d/11tdkyX0kR8nRwMUktohdp2Wx1fGiMiHrlUP4i_8ctPM/edit?usp=sharing"
#===============================================================================

# Code
```

## Read in sheet

``` r
# col_types = cols(.default = "c")

resources <- 
  read_sheet(sheet_link, sheet = 2, col_types = "c") %>% 
  rename_all(str_to_lower) %>% 
  drop_na(organization) %>% 
  rename_all(~str_remove_all(.,"\\,|\\/")) %>% 
  rename_all(~str_replace_all(.,"\\ ","_")) %>% 
  mutate(
    location = str_replace_all(location, "\n", " "),
    days_hours = str_replace_all(days_hours, "\n", " ")
  ) %>% 
  select(-1, -c("open_time", "close_time", "date_verified"))
```

    ## Using an auto-discovered, cached token.
    ## To suppress this message, modify your code or options to clearly consent to the use of a cached token.
    ## See gargle's "Non-interactive auth" vignette for more details:
    ## https://gargle.r-lib.org/articles/non-interactive-auth.html
    ## The googlesheets4 package is using a cached token for cleboa@stanford.edu.

    ## Reading from "COVID-19 Comm Resources VOLUNTEER Input"

    ## Range "'MAIN SHEET -- UPDATE HERE'"

    ## New names:
    ## * `` -> ...1

``` r
#set to volunteer facing document right now

resources
```

    ## # A tibble: 124 x 13
    ##    type  organization location days_hours contact_info areas_served county
    ##    <chr> <chr>        <chr>    <chr>      <chr>        <chr>        <chr> 
    ##  1 Hous… Fair Oaks C… 2600 Mi… <NA>       "(650) 780-… Redwood Cit… San M…
    ##  2 Core… Samaritan H… 4031 Pa… <NA>       "(650) 347-… Belmont, Bu… San M…
    ##  3 Core… Samaritan H… 1852 Ba… <NA>       "(650) 294-… East Palo A… San M…
    ##  4 Core… West Valley… 10104 V… Monday - … "408-366-60… Santa Clara… Santa…
    ##  5 Core… Glide Memor… 330 Ell… Monday - … "(415) 674-… San Francis… San M…
    ##  6 Food  Ecumenical … 2411 Pu… Tuesday -… "(650) 323-… East Palo A… San M…
    ##  7 Food  Family Harv… St. Sam… Tuesday 1… "650-779-46… East Palo A… San M…
    ##  8 Food  Nuestra Casa 1842 W.… 1st and 3… "Kim Ruiz, … East Palo A… San M…
    ##  9 Food  Nuestra Casa 1991 Ma… 2nd and 4… "Kim Ruiz, … East Palo A… San M…
    ## 10 Food  Nuestra Casa 1805 E … 1st and 3… "Kim Ruiz, … East Palo A… San M…
    ## # … with 114 more rows, and 6 more variables: eligibility <chr>,
    ## #   notes_specifics <chr>, `links_(english)` <chr>, `links_(spanish)` <chr>,
    ## #   verification_method <chr>, include_in_pdf <chr>

``` r
#set to volunteer facing document right now
```

### Parse by County

``` r
#separate county column by comma 
resources_longer <- 
  resources %>% 
  separate_rows(county, sep = ", ") %>% 
  group_by(county)

# get group keys
group_name <- 
  resources_longer %>%
  group_keys() %>% 
  pull()

# assign name to each split table
group_split(resources_longer) %>%
    setNames(group_name ) %>% 
    list2env(.GlobalEnv)
```

    ## <environment: R_GlobalEnv>

## Write to Google Sheet

``` r
# This commented line finds the ID for our volunteer spreadsheet
# as_sheets_id("https://docs.google.com/spreadsheets/d/11tdkyX0kR8nRwMUktohdp2Wx1fGiMiHrlUP4i_8ctPM/edit#gid=0")

volunteer_sheet_id <- "11tdkyX0kR8nRwMUktohdp2Wx1fGiMiHrlUP4i_8ctPM"

# This commented line gives more info on the spreadsheet
# sheets_sheet_properties(volunteer_sheet_id)

# Write sheet to a new tab (must first create in the spreadsheet)
# This is for the filtered Santa Clara sheet

  write_sheet(`Santa Clara`, ss = volunteer_sheet_id, sheet = 5)
```

    ## Writing to "COVID-19 Comm Resources VOLUNTEER Input"

    ## Writing to sheet "Santa Clara"
