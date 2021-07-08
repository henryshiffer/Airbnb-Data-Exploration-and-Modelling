Initial Modelling Full Data
================
Henry Shiffer

### Creating/Cleaning Full Data Set:

> Before any modelling can take place, all the data must be loaded and
> fit together. Much of the data needs cleaning.

Loading all month’s data:

``` r
library(tidyverse) 
library(tidymodels) 
library(ggmap)
library(varhandle)
library(caret)
library(readr)
library(yardstick)
library(dplyr)
library(purrr)
library(stringr)
library(olsrr)
```

``` r
#February 2020 
feb_20_airbnb <- read_csv("AirbnbFebruary2020.csv", guess_max = 20000)
#March 2020
mar_20_airbnb <- read_csv("AirbnbMarch2020.csv", guess_max = 20000)
#April 2020
apr_20_airbnb <- read_csv("AirbnbApril2020.csv", guess_max = 20000)
#May 2020
may_20_airbnb <- read_csv("AirbnbMay2020.csv", guess_max = 20000)
#June 2020
jun_20_airbnb <- read_csv("AirbnbJune2020.csv", guess_max = 20000)
#July 2020
jul_20_airbnb <- read_csv("AirbnbJuly2020.csv", guess_max = 20000)
#August 2020
aug_20_airbnb <- read_csv("AirbnbAugust2020.csv", guess_max = 20000)
#September 2020
sept_20_airbnb <- read_csv("AirbnbSeptember2020.csv", guess_max = 20000)
#October 2020
oct_20_airbnb <- read_csv("AirbnbOctober2020.csv", guess_max = 20000)
#November 2020
nov_20_airbnb <- read_csv("AirbnbNovember2020.csv", guess_max = 20000)
#December 2020
dec_20_airbnb <- read_csv("AirbnbDecember2020.csv", guess_max = 20000)
#January 2021
jan_21_airbnb <- read_csv("AirbnbJanuary2021.csv", guess_max = 20000)
#Feb 2021
feb_21_airbnb <- read_csv("AirbnbFebruary2021.csv", guess_max = 20000)
#March 2021
mar_21_airbnb <- read_csv("AirbnbMarch2021.csv", guess_max = 20000)
#April 2021
apr_21_airbnb <- read_csv("AirbnbApril2021.csv", guess_max = 20000)
```

``` r
#adding a month/year variable to each data set
feb_20_airbnb$month_year = "February_2020"
mar_20_airbnb$month_year = "March_2020"
apr_20_airbnb$month_year = "April_2020"
may_20_airbnb$month_year = "May_2020"
jun_20_airbnb$month_year = "June_2020"
jul_20_airbnb$month_year = "July_2020"
aug_20_airbnb$month_year = "August_2020"
sept_20_airbnb$month_year = "September_2020"
oct_20_airbnb$month_year = "October_2020"
nov_20_airbnb$month_year = "November_2020"
dec_20_airbnb$month_year = "December_2020"
jan_21_airbnb$month_year = "January_2021"
feb_21_airbnb$month_year = "February_2021"
mar_21_airbnb$month_year = "March_2021"
apr_21_airbnb$month_year = "April_2021"
```

There are some months that have bathrooms listed and saved as character
elements. We must rename the bathroom column in these month’s to
“bathrooms”, as the original bathrooms column is corrupted.

``` r
aug_20_airbnb <- subset(aug_20_airbnb, select = -c(bathrooms))
colnames(aug_20_airbnb)[colnames(aug_20_airbnb) == 'bathrooms_text'] <- 'bathrooms'

sept_20_airbnb <- subset(sept_20_airbnb, select = -c(bathrooms))
colnames(sept_20_airbnb)[colnames(sept_20_airbnb) == 'bathrooms_text'] <- 'bathrooms'

oct_20_airbnb <- subset(oct_20_airbnb, select = -c(bathrooms))
colnames(oct_20_airbnb)[colnames(oct_20_airbnb) == 'bathrooms_text'] <- 'bathrooms'

nov_20_airbnb <- subset(nov_20_airbnb, select = -c(bathrooms))
colnames(nov_20_airbnb)[colnames(nov_20_airbnb) == 'bathrooms_text'] <- 'bathrooms'

dec_20_airbnb <- subset(dec_20_airbnb, select = -c(bathrooms))
colnames(dec_20_airbnb)[colnames(dec_20_airbnb) == 'bathrooms_text'] <- 'bathrooms'

jan_21_airbnb <- subset(jan_21_airbnb, select = -c(bathrooms))
colnames(jan_21_airbnb)[colnames(jan_21_airbnb) == 'bathrooms_text'] <- 'bathrooms'

mar_21_airbnb <- subset(mar_21_airbnb, select = -c(bathrooms))
colnames(mar_21_airbnb)[colnames(mar_21_airbnb) == 'bathrooms_text'] <- 'bathrooms'

apr_21_airbnb <- subset(apr_21_airbnb, select = -c(bathrooms))
colnames(apr_21_airbnb)[colnames(apr_21_airbnb) == 'bathrooms_text'] <- 'bathrooms'

feb_21_airbnb <- subset(feb_21_airbnb, select = -c(bathrooms))
colnames(feb_21_airbnb)[colnames(feb_21_airbnb) == 'bathrooms_text'] <- 'bathrooms'
```

However, first we know that the bathroom columns in these months are
characters and not numeric. We must convert these to numeric values
before we can merge rows.

``` r
glimpse(feb_21_airbnb$bathrooms)
```

    ##  chr [1:15832] "3 baths" "1.5 baths" "1 shared bath" "1 bath" "1.5 baths" ...

``` r
#parse_number() drops any non-numeric characters
aug_20_airbnb$bathrooms <- parse_number(aug_20_airbnb$bathrooms)
sept_20_airbnb$bathrooms <- parse_number(sept_20_airbnb$bathrooms)
oct_20_airbnb$bathrooms <- parse_number(oct_20_airbnb$bathrooms)
nov_20_airbnb$bathrooms <- parse_number(nov_20_airbnb$bathrooms)
dec_20_airbnb$bathrooms <- parse_number(dec_20_airbnb$bathrooms)
jan_21_airbnb$bathrooms <- parse_number(jan_21_airbnb$bathrooms)
mar_21_airbnb$bathrooms <- parse_number(mar_21_airbnb$bathrooms)
apr_21_airbnb$bathrooms <- parse_number(apr_21_airbnb$bathrooms)
feb_21_airbnb$bathrooms <- parse_number(feb_21_airbnb$bathrooms)
```

Each month’s data-set might not have the same exact set of variables. We
must first clean each month’s data by making sure each variable is both
named the same, and that each set has the same set of variables.

We can start first with Feb 2020. Using Feb 2021 as a reference, we can
see which variables are the same in both.

``` r
cols_to_keep <- intersect(colnames(feb_20_airbnb),colnames(feb_21_airbnb))
feb_20_airbnb <- subset(feb_20_airbnb, select = cols_to_keep)
feb_21_airbnb <- subset(feb_21_airbnb, select = cols_to_keep)
```

Using the intersecting of variables of February 2020 and February 2021,
we can drop the variables that are not intersecting.

Cleaning March 2020

``` r
cols_to_keep2 <- intersect(colnames(mar_20_airbnb), colnames(feb_21_airbnb))
mar_20_airbnb <- subset(mar_20_airbnb, select = cols_to_keep2)
```

Cleaning April 2020

``` r
cols_to_keep3 <- intersect(colnames(apr_20_airbnb), colnames(feb_21_airbnb))
apr_20_airbnb <- subset(apr_20_airbnb, select = cols_to_keep3)
```

Cleaning May 2020:

``` r
cols_to_keep4 <- intersect(colnames(may_20_airbnb), colnames(feb_21_airbnb))
may_20_airbnb <- subset(may_20_airbnb, select = cols_to_keep4)
```

Cleaning June 2020:

``` r
cols_to_keep5 <- intersect(colnames(jun_20_airbnb), colnames(feb_21_airbnb))
jun_20_airbnb <- subset(jun_20_airbnb, select = cols_to_keep5)
```

We can see that July 2020 does not have a price column. We can replace
this price column with Weekly price for now. We also see that July 2020
does not have a bedrooms column. We can set these as NA for now.

We can also see that July does not contain amenities, We will include
these as NA values. We can also see that there is no variable named
minimum and maximum nights in the July data set so we will create ones

``` r
colnames(jul_20_airbnb)[colnames(jul_20_airbnb) == 'weekly_price'] <- 'price'
jul_20_airbnb$bedrooms = NA
jul_20_airbnb$beds = NA
jul_20_airbnb$amenities = NA
colnames(jul_20_airbnb)[colnames(jul_20_airbnb) == 'minimum_minimum_nights'] <- 'minimum_nights'
colnames(jul_20_airbnb)[colnames(jul_20_airbnb) == 'maximum_maximum_nights'] <- 'maximum_nights'
jul_20_airbnb$minimum_minimum_nights = jul_20_airbnb$minimum_nights
jul_20_airbnb$maximum_maximum_nights = jul_20_airbnb$maximum_nights
cols_to_keep6 <- intersect(colnames(jul_20_airbnb), colnames(feb_21_airbnb))
jul_20_airbnb <- subset(jul_20_airbnb, select = cols_to_keep6)
```

Cleaning August 2020:

``` r
set_diffs <- setdiff(colnames(feb_21_airbnb), colnames(aug_20_airbnb))
cols_to_keep7 <- intersect(colnames(aug_20_airbnb), colnames(feb_21_airbnb))
aug_20_airbnb <- subset(aug_20_airbnb, select = cols_to_keep7)
```

Cleaning September 2020:

``` r
set_diffs <- setdiff(colnames(feb_21_airbnb), colnames(sept_20_airbnb))
cols_to_keep8 <- intersect(colnames(sept_20_airbnb), colnames(feb_21_airbnb))
sept_20_airbnb <- subset(sept_20_airbnb, select = cols_to_keep8)
```

Cleaning October 2020:

``` r
cols_to_keep9 <- intersect(colnames(oct_20_airbnb), colnames(feb_21_airbnb))
oct_20_airbnb <- subset(oct_20_airbnb, select = cols_to_keep9)
```

Cleaning November 2020:

``` r
cols_to_keep10 <- intersect(colnames(nov_20_airbnb), colnames(feb_21_airbnb))
nov_20_airbnb <- subset(nov_20_airbnb, select = cols_to_keep10)
```

Cleaning December 2020:

``` r
cols_to_keep11 <- intersect(colnames(dec_20_airbnb), colnames(feb_21_airbnb))
dec_20_airbnb <- subset(dec_20_airbnb, select = cols_to_keep11)
```

Cleaning January 2021:

``` r
cols_to_keep12 <- intersect(colnames(jan_21_airbnb), colnames(feb_21_airbnb))
jan_21_airbnb <- subset(jan_21_airbnb, select = cols_to_keep12)
```

Cleaning March 2021:

``` r
cols_to_keep13 <- intersect(colnames(mar_21_airbnb), colnames(feb_21_airbnb))
mar_21_airbnb <- subset(mar_21_airbnb, select = cols_to_keep13)
```

Cleaning April 2021:

``` r
cols_to_keep14 <- intersect(colnames(apr_21_airbnb), colnames(feb_21_airbnb))
apr_21_airbnb <- subset(apr_21_airbnb, select = cols_to_keep14)
```

Now that each data set has the same columns, we can merge this data to
create a full Airbnb data set.

``` r
merge_1 <- rbind(feb_20_airbnb, mar_20_airbnb)
merge_2 <- rbind(merge_1, apr_20_airbnb)
merge_3 <- rbind(merge_2, may_20_airbnb)
merge_4 <- rbind(merge_3, jun_20_airbnb)
merge_5 <- rbind(merge_4, jul_20_airbnb)
merge_6 <- rbind(merge_5, aug_20_airbnb)
merge_7 <- rbind(merge_6, sept_20_airbnb)
merge_8 <- rbind(merge_7, oct_20_airbnb)
merge_9 <- rbind(merge_8, nov_20_airbnb)
merge_10 <- rbind(merge_9, dec_20_airbnb)
merge_11 <- rbind(merge_10, jan_21_airbnb)
merge_12 <- rbind(merge_11, feb_21_airbnb)
merge_13 <- rbind(merge_12, mar_21_airbnb)
airbnb_data <- rbind(merge_13, apr_21_airbnb)
glimpse(airbnb_data)
```

    ## Rows: 292,765
    ## Columns: 73
    ## $ id                                           <dbl> 1419, 8077, 12604, 23691,…
    ## $ listing_url                                  <chr> "https://www.airbnb.com/r…
    ## $ scrape_id                                    <dbl> 2.020021e+13, 2.020021e+1…
    ## $ last_scraped                                 <dbl> 43875, 43876, 43875, 4387…
    ## $ name                                         <chr> "Beautiful home in amazin…
    ## $ description                                  <chr> "This large, family home …
    ## $ neighborhood_overview                        <chr> "The apartment is located…
    ## $ picture_url                                  <chr> "https://a0.muscache.com/…
    ## $ host_id                                      <dbl> 1565, 22795, 48239, 93825…
    ## $ host_url                                     <chr> "https://www.airbnb.com/u…
    ## $ host_name                                    <chr> "Alexandra", "Kathie & La…
    ## $ host_since                                   <dbl> 39668, 39986, 40111, 4025…
    ## $ host_location                                <chr> "Toronto, Ontario, Canada…
    ## $ host_about                                   <chr> "I live in Toronto, Canad…
    ## $ host_response_time                           <chr> "N/A", "within a day", "N…
    ## $ host_response_rate                           <chr> "N/A", "1", "N/A", "1", "…
    ## $ host_acceptance_rate                         <chr> "N/A", "N/A", "N/A", "0.9…
    ## $ host_is_superhost                            <lgl> FALSE, FALSE, FALSE, FALS…
    ## $ host_thumbnail_url                           <chr> "https://a0.muscache.com/…
    ## $ host_picture_url                             <chr> "https://a0.muscache.com/…
    ## $ host_neighbourhood                           <chr> "Commercial Drive", "Harb…
    ## $ host_listings_count                          <dbl> 1, 2, 1, 2, 7, 1, 2, 3, 2…
    ## $ host_total_listings_count                    <dbl> 1, 2, 1, 2, 7, 1, 2, 3, 2…
    ## $ host_verifications                           <chr> "['email', 'phone', 'revi…
    ## $ host_has_profile_pic                         <lgl> TRUE, TRUE, TRUE, TRUE, T…
    ## $ host_identity_verified                       <lgl> TRUE, FALSE, TRUE, TRUE, …
    ## $ neighbourhood                                <chr> "Little Portugal", "Downt…
    ## $ neighbourhood_cleansed                       <chr> "Little Portugal", "Water…
    ## $ neighbourhood_group_cleansed                 <lgl> NA, NA, NA, NA, NA, NA, N…
    ## $ latitude                                     <dbl> 43.64617, 43.64105, 43.66…
    ## $ longitude                                    <dbl> -79.42451, -79.37628, -79…
    ## $ property_type                                <chr> "House", "Apartment", "Ho…
    ## $ room_type                                    <chr> "Entire home/apt", "Priva…
    ## $ accommodates                                 <dbl> 10, 2, 1, 3, 5, 1, 2, 5, …
    ## $ bathrooms                                    <dbl> 3.0, 1.5, 1.5, 1.0, 1.0, …
    ## $ bedrooms                                     <dbl> 5, 1, 1, 1, 1, 0, 1, 2, 0…
    ## $ beds                                         <dbl> 7, 1, 1, 1, 2, 1, 2, 2, 1…
    ## $ amenities                                    <chr> "{TV,Internet,Wifi,\"Air …
    ## $ price                                        <chr> "469", "99", "66", "72", …
    ## $ minimum_nights                               <dbl> 4, 180, 1, 1, 4, 120, 180…
    ## $ maximum_nights                               <dbl> 730, 365, 10, 28, 730, 36…
    ## $ minimum_minimum_nights                       <dbl> 4, 180, 1, 1, 4, 120, 180…
    ## $ maximum_minimum_nights                       <dbl> 4, 180, 1, 1, 5, 120, 180…
    ## $ minimum_maximum_nights                       <dbl> 730, 365, 10, 28, 730, 36…
    ## $ maximum_maximum_nights                       <dbl> 730, 365, 10, 28, 730, 36…
    ## $ minimum_nights_avg_ntm                       <dbl> 4.0, 180.0, 1.0, 1.0, 4.0…
    ## $ maximum_nights_avg_ntm                       <dbl> 730, 365, 10, 28, 730, 36…
    ## $ calendar_updated                             <chr> "25 months ago", "17 mont…
    ## $ has_availability                             <lgl> TRUE, TRUE, TRUE, TRUE, T…
    ## $ availability_30                              <dbl> 0, 0, 0, 0, 30, 0, 30, 0,…
    ## $ availability_60                              <dbl> 0, 0, 0, 0, 60, 0, 60, 0,…
    ## $ availability_90                              <dbl> 0, 0, 0, 0, 90, 0, 90, 0,…
    ## $ availability_365                             <dbl> 0, 0, 0, 0, 365, 0, 365, …
    ## $ calendar_last_scraped                        <dbl> 43875, 43876, 43875, 4387…
    ## $ number_of_reviews                            <dbl> 7, 169, 0, 217, 39, 26, 1…
    ## $ number_of_reviews_ltm                        <dbl> 0, 0, 0, 10, 4, 0, 0, 11,…
    ## $ first_review                                 <dbl> 42204, 40045, NA, 40329, …
    ## $ last_review                                  <dbl> 43073, 41513, NA, 43821, …
    ## $ review_scores_rating                         <dbl> 100, 97, NA, 95, 95, 98, …
    ## $ review_scores_accuracy                       <dbl> 10, 10, NA, 10, 10, 10, N…
    ## $ review_scores_cleanliness                    <dbl> 10, 10, NA, 10, 10, 10, N…
    ## $ review_scores_checkin                        <dbl> 10, 10, NA, 10, 9, 10, NA…
    ## $ review_scores_communication                  <dbl> 10, 10, NA, 10, 10, 10, N…
    ## $ review_scores_location                       <dbl> 10, 10, NA, 9, 10, 10, NA…
    ## $ review_scores_value                          <dbl> 10, 10, NA, 10, 9, 10, NA…
    ## $ license                                      <chr> NA, NA, NA, NA, NA, NA, N…
    ## $ instant_bookable                             <lgl> FALSE, TRUE, FALSE, TRUE,…
    ## $ calculated_host_listings_count               <dbl> 1, 2, 1, 2, 5, 1, 2, 5, 2…
    ## $ calculated_host_listings_count_entire_homes  <dbl> 1, 1, 0, 0, 5, 1, 1, 5, 2…
    ## $ calculated_host_listings_count_private_rooms <dbl> 0, 1, 1, 2, 0, 0, 1, 0, 0…
    ## $ calculated_host_listings_count_shared_rooms  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ reviews_per_month                            <dbl> 0.13, 1.32, NA, 1.84, 0.3…
    ## $ month_year                                   <chr> "February_2020", "Februar…

The full data-set contains many locations that are not of interest as
they are far away from the downtown core of Toronto, we’re going to
subset the neighbourhoods around University of Toronto from the full
data set. We are going to pick the neighbourhoods:

> Kensington-Chinatown

> Annex

> University

> Church-Yonge Corridor

> Bay Street Corridor

``` r
#subsetting the data set to get the neighbourhoods of intrest
airbnb_data <- subset.data.frame(airbnb_data, neighbourhood_cleansed == c("Kensington-Chinatown",
                                                                          "Annex","University",
                                                                          "Church-Yonge Corridor",
                                                                          "Bay Street Corridor"))
```

Now we can replace the missing bedroom amount in July 2020 with the
bedroom amount in other months using the unique id column.

``` r
#getting july rows and ids
july_rows <- filter(airbnb_data, month_year == "July_2020")
july_ids <- c(july_rows$id)


#getting the bedroom amounts from other months
bedrooms_for_july_ids_from_other_months <-
    airbnb_data %>%
    filter(id %in% july_ids) %>%
    filter(month_year != "July_2020") %>%
    select(id, bedrooms) %>%
    distinct()



#updating the july data with bedroom amounts from other months
july_rows <-
  july_rows %>% 
    select(-bedrooms) %>%
    left_join(bedrooms_for_july_ids_from_other_months, by = "id")
```

``` r
glimpse(july_rows)
```

    ## Rows: 606
    ## Columns: 73
    ## $ id                                           <dbl> 12604, 655522, 682099, 83…
    ## $ listing_url                                  <chr> "https://www.airbnb.com/r…
    ## $ scrape_id                                    <dbl> 2.020071e+13, 2.020071e+1…
    ## $ last_scraped                                 <dbl> 18453, 18453, 18453, 1845…
    ## $ name                                         <chr> "Seaton Village Parlour B…
    ## $ description                                  <chr> "Comfortable sofa bed in …
    ## $ neighborhood_overview                        <chr> NA, "SUPER close to Kensi…
    ## $ picture_url                                  <chr> "https://a0.muscache.com/…
    ## $ host_id                                      <dbl> 48239, 3299954, 2034186, …
    ## $ host_url                                     <chr> "https://www.airbnb.com/u…
    ## $ host_name                                    <chr> "Rona", "Basil", "Jennife…
    ## $ host_since                                   <dbl> 14542, 15570, 15428, 1568…
    ## $ host_location                                <chr> "Toronto, Ontario, Canada…
    ## $ host_about                                   <chr> "I love travelling and ex…
    ## $ host_response_time                           <chr> "N/A", "N/A", "N/A", "wit…
    ## $ host_response_rate                           <chr> "N/A", "N/A", "N/A", "100…
    ## $ host_acceptance_rate                         <chr> "N/A", "N/A", "90%", "67%…
    ## $ host_is_superhost                            <lgl> FALSE, FALSE, TRUE, FALSE…
    ## $ host_thumbnail_url                           <chr> "https://a0.muscache.com/…
    ## $ host_picture_url                             <chr> "https://a0.muscache.com/…
    ## $ host_neighbourhood                           <chr> "The Annex", "Downtown To…
    ## $ host_listings_count                          <dbl> 1, 1, 1, 2, 1, 0, 1, 6, 2…
    ## $ host_total_listings_count                    <dbl> 1, 1, 1, 2, 1, 0, 1, 6, 2…
    ## $ host_verifications                           <chr> "['email', 'phone', 'revi…
    ## $ host_has_profile_pic                         <lgl> TRUE, TRUE, TRUE, TRUE, T…
    ## $ host_identity_verified                       <lgl> TRUE, FALSE, FALSE, TRUE,…
    ## $ neighbourhood                                <chr> "The Annex", "Downtown To…
    ## $ neighbourhood_cleansed                       <chr> "Annex", "Kensington-Chin…
    ## $ neighbourhood_group_cleansed                 <lgl> NA, NA, NA, NA, NA, NA, N…
    ## $ latitude                                     <dbl> 43.66724, 43.65805, 43.65…
    ## $ longitude                                    <dbl> -79.41598, -79.39601, -79…
    ## $ property_type                                <chr> "House", "Apartment", "Co…
    ## $ room_type                                    <chr> "Private room", "Entire h…
    ## $ accommodates                                 <dbl> 1, 2, 2, 2, 4, 2, 2, 5, 4…
    ## $ bathrooms                                    <dbl> 1.5, 1.0, 1.0, 1.0, 1.0, …
    ## $ beds                                         <dbl> NA, NA, NA, NA, NA, NA, N…
    ## $ amenities                                    <chr> NA, NA, NA, NA, NA, NA, N…
    ## $ price                                        <chr> "$401.00", NA, NA, "$760.…
    ## $ minimum_nights                               <dbl> 1, 4, 1, 28, 28, 180, 1, …
    ## $ maximum_nights                               <dbl> 10, 365, 200, 1000, 90, 7…
    ## $ minimum_minimum_nights                       <dbl> 1, 4, 1, 28, 28, 180, 1, …
    ## $ maximum_minimum_nights                       <dbl> 1, 4, 1, 28, 28, 180, 1, …
    ## $ minimum_maximum_nights                       <dbl> 10, 365, 200, 1000, 90, 7…
    ## $ maximum_maximum_nights                       <dbl> 10, 365, 200, 1000, 90, 7…
    ## $ minimum_nights_avg_ntm                       <dbl> 1.0, 4.0, 1.0, 28.0, 28.0…
    ## $ maximum_nights_avg_ntm                       <dbl> 10, 365, 200, 1000, 90, 7…
    ## $ calendar_updated                             <chr> "22 months ago", "24 mont…
    ## $ has_availability                             <lgl> TRUE, TRUE, TRUE, TRUE, T…
    ## $ availability_30                              <dbl> 0, 0, 29, 8, 2, 2, 0, 30,…
    ## $ availability_60                              <dbl> 0, 0, 59, 38, 32, 2, 6, 6…
    ## $ availability_90                              <dbl> 0, 0, 89, 68, 62, 2, 36, …
    ## $ availability_365                             <dbl> 0, 0, 179, 343, 152, 2, 3…
    ## $ calendar_last_scraped                        <dbl> 18453, 18453, 18453, 1845…
    ## $ number_of_reviews                            <dbl> 0, 29, 241, 6, 237, 0, 13…
    ## $ number_of_reviews_ltm                        <dbl> 0, 0, 14, 3, 34, 0, 7, 0,…
    ## $ first_review                                 <dbl> NA, 15577, 15592, 15918, …
    ## $ last_review                                  <dbl> NA, 17753, 18243, 18209, …
    ## $ review_scores_rating                         <dbl> NA, 96, 97, 88, 98, NA, 9…
    ## $ review_scores_accuracy                       <dbl> NA, 10, 10, 9, 10, NA, 10…
    ## $ review_scores_cleanliness                    <dbl> NA, 10, 10, 9, 10, NA, 10…
    ## $ review_scores_checkin                        <dbl> NA, 10, 10, 9, 10, NA, 10…
    ## $ review_scores_communication                  <dbl> NA, 10, 10, 10, 10, NA, 1…
    ## $ review_scores_location                       <dbl> NA, 10, 10, 10, 10, NA, 1…
    ## $ review_scores_value                          <dbl> NA, 9, 10, 9, 10, NA, 10,…
    ## $ license                                      <chr> NA, NA, NA, NA, NA, NA, N…
    ## $ instant_bookable                             <lgl> FALSE, FALSE, FALSE, FALS…
    ## $ calculated_host_listings_count               <dbl> 1, 1, 1, 1, 1, 1, 1, 4, 1…
    ## $ calculated_host_listings_count_entire_homes  <dbl> 0, 1, 0, 1, 1, 1, 0, 2, 1…
    ## $ calculated_host_listings_count_private_rooms <dbl> 1, 0, 1, 0, 0, 0, 1, 2, 0…
    ## $ calculated_host_listings_count_shared_rooms  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ reviews_per_month                            <dbl> NA, 0.30, 2.53, 0.07, 2.8…
    ## $ month_year                                   <chr> "July_2020", "July_2020",…
    ## $ bedrooms                                     <dbl> 1, 1, 1, 1, 2, NA, 1, 2, …

``` r
airbnb_data <- subset.data.frame(airbnb_data, month_year != "July_2020")
airbnb_data <- rbind(airbnb_data, july_rows)
```

Now for cleaning the price column by removing all dollar signs and
commas and converting these values to integers.

``` r
airbnb_data <- airbnb_data %>% 
  mutate(price = str_remove(price, "\\$"),
         price = str_remove(price, ","),
         price = as.integer(price)
         )
```

We can also see that host response rates are coded as characters. We can
code them as integers instead.

``` r
glimpse(airbnb_data$host_response_rate)
```

    ##  chr [1:7824] "1" "1" "1" "N/A" "N/A" "1" "1" "N/A" "1" "1" "1" "1" "0.83" ...

``` r
#stripping away % and from host_response_rate and converting to integer
airbnb_data <- airbnb_data %>% 
  mutate(host_response_rate = str_remove(host_response_rate, "\\%"),
         host_response_rate = as.integer(host_response_rate)
         )
```

### Variable Selection Full Data:

We can do an initial linear model.

Listing quality was defined by this project as bedroom and bathroom
amount, the rating from review scores, neighbourhood, and if a host was
a superhost or not. From here we can select indicators that measure
these attributes and examine their relationship with price, and the
trends that exist within them.

These variables are most consistent with Airbnb quality:

> bedrooms

> bathrooms

> neighbourhood\_cleansed

> review\_scores\_cleanliness

> review\_scores\_rating

> reviews\_per\_month

> host\_response\_rate

> minimum\_nights

> maximum\_nights

> host\_is\_superhost

We can now run both backwards selection and forwards selection stepwise
regression with AIC criteria to select the best variables of the Airbnb
quality variables to add to the model.

First running backwards selection stepwise regression:

``` r
#fitting a model with all Airbnb quality variables
quality_vs_price <- lm(price~ +
                         bedrooms +
                         bathrooms +
                         neighbourhood_cleansed +
                         month_year +
                         reviews_per_month +
                         host_is_superhost +
                         minimum_nights +
                         maximum_nights +
                         review_scores_rating +
                         host_response_rate +
                         review_scores_cleanliness, data = airbnb_data)
#backwards selection
ols_step_backward_p(quality_vs_price, details = TRUE)
```

    ## Backward Elimination Method 
    ## ---------------------------
    ## 
    ## Candidate Terms: 
    ## 
    ## 1 . bedrooms 
    ## 2 . bathrooms 
    ## 3 . neighbourhood_cleansed 
    ## 4 . month_year 
    ## 5 . reviews_per_month 
    ## 6 . host_is_superhost 
    ## 7 . minimum_nights 
    ## 8 . maximum_nights 
    ## 9 . review_scores_rating 
    ## 10 . host_response_rate 
    ## 11 . review_scores_cleanliness 
    ## 
    ## We are eliminating variables based on p value...
    ## 
    ## - review_scores_rating 
    ## 
    ## Backward Elimination: Step 1 
    ## 
    ##  Variable review_scores_rating Removed 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.227       RMSE                  368.454 
    ## R-Squared               0.051       Coef. Var             237.081 
    ## Adj. R-Squared          0.044       MSE                135758.070 
    ## Pred R-Squared          0.035       MAE                    84.982 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                   ANOVA                                    
    ## --------------------------------------------------------------------------
    ##                      Sum of                                               
    ##                     Squares          DF    Mean Square      F        Sig. 
    ## --------------------------------------------------------------------------
    ## Regression     22928976.538          26     881883.713    6.496    0.0000 
    ## Residual      422750628.548        3114     135758.070                    
    ## Total         445679605.086        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                      Parameter Estimates                                                      
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower       upper 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)      5.410        78.034                  0.069    0.945    -147.593     158.413 
    ##                                    bedrooms     63.789        10.098        0.129     6.317    0.000      43.989      83.588 
    ##                                   bathrooms     30.038        14.309        0.043     2.099    0.036       1.981      58.095 
    ##   neighbourhood_cleansedBay Street Corridor     13.163        18.988        0.014     0.693    0.488     -24.067      50.393 
    ## neighbourhood_cleansedChurch-Yonge Corridor     40.585        19.267        0.044     2.106    0.035       2.808      78.361 
    ##  neighbourhood_cleansedKensington-Chinatown    -41.129        19.025       -0.046    -2.162    0.031     -78.432      -3.825 
    ##            neighbourhood_cleansedUniversity    -34.376        26.921       -0.025    -1.277    0.202     -87.160      18.407 
    ##                        month_yearApril_2021    -18.859        36.351       -0.011    -0.519    0.604     -90.134      52.417 
    ##                       month_yearAugust_2020      4.311        34.756        0.003     0.124    0.901     -63.836      72.459 
    ##                     month_yearDecember_2020    -20.042        35.418       -0.012    -0.566    0.572     -89.487      49.403 
    ##                     month_yearFebruary_2020    -12.813        44.714       -0.011    -0.287    0.774    -100.484      74.858 
    ##                     month_yearFebruary_2021    -40.547        37.071       -0.023    -1.094    0.274    -113.232      32.139 
    ##                      month_yearJanuary_2021    -38.394        36.249       -0.022    -1.059    0.290    -109.469      32.681 
    ##                         month_yearJuly_2020    812.757       113.187        0.127     7.181    0.000     590.828    1034.687 
    ##                         month_yearJune_2020     41.685        31.128        0.030     1.339    0.181     -19.349     102.719 
    ##                        month_yearMarch_2020    -39.907        44.828       -0.034    -0.890    0.373    -127.803      47.988 
    ##                        month_yearMarch_2021    -41.536        39.017       -0.022    -1.065    0.287    -118.037      34.964 
    ##                          month_yearMay_2020     36.628        28.635        0.030     1.279    0.201     -19.518      92.774 
    ##                     month_yearNovember_2020    -13.017        33.861       -0.008    -0.384    0.701     -79.409      53.375 
    ##                      month_yearOctober_2020    -31.928        34.744       -0.020    -0.919    0.358    -100.052      36.197 
    ##                    month_yearSeptember_2020      5.819        33.018        0.004     0.176    0.860     -58.921      70.559 
    ##                           reviews_per_month     -8.494         3.583       -0.045    -2.370    0.018     -15.520      -1.468 
    ##                       host_is_superhostTRUE    -15.094        14.646       -0.020    -1.031    0.303     -43.810      13.622 
    ##                              minimum_nights     -0.103         0.296       -0.006    -0.347    0.729      -0.684       0.478 
    ##                              maximum_nights      0.010         0.013        0.013     0.740    0.460      -0.016       0.036 
    ##                          host_response_rate     -0.457         0.365       -0.052    -1.251    0.211      -1.173       0.259 
    ##                   review_scores_cleanliness      9.686         7.301        0.025     1.327    0.185      -4.628      24.001 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## - minimum_nights 
    ## 
    ## Backward Elimination: Step 2 
    ## 
    ##  Variable minimum_nights Removed 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.227       RMSE                  368.402 
    ## R-Squared               0.051       Coef. Var             237.048 
    ## Adj. R-Squared          0.044       MSE                135719.734 
    ## Pred R-Squared          0.035       MAE                    84.991 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                   ANOVA                                    
    ## --------------------------------------------------------------------------
    ##                      Sum of                                               
    ##                     Squares          DF    Mean Square      F        Sig. 
    ## --------------------------------------------------------------------------
    ## Regression     22912634.151          25     916505.366    6.753    0.0000 
    ## Residual      422766970.935        3115     135719.734                    
    ## Total         445679605.086        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                      Parameter Estimates                                                      
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower       upper 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)      5.226        78.021                  0.067    0.947    -147.752     158.204 
    ##                                    bedrooms     63.812        10.096        0.129     6.320    0.000      44.016      83.608 
    ##                                   bathrooms     30.104        14.306        0.043     2.104    0.035       2.053      58.154 
    ##   neighbourhood_cleansedBay Street Corridor     13.271        18.983        0.015     0.699    0.485     -23.948      50.491 
    ## neighbourhood_cleansedChurch-Yonge Corridor     40.532        19.263        0.044     2.104    0.035       2.762      78.302 
    ##  neighbourhood_cleansedKensington-Chinatown    -40.810        19.000       -0.045    -2.148    0.032     -78.064      -3.555 
    ##            neighbourhood_cleansedUniversity    -33.697        26.845       -0.024    -1.255    0.209     -86.333      18.940 
    ##                        month_yearApril_2021    -20.138        36.159       -0.012    -0.557    0.578     -91.036      50.759 
    ##                       month_yearAugust_2020      4.265        34.751        0.003     0.123    0.902     -63.873      72.402 
    ##                     month_yearDecember_2020    -20.319        35.404       -0.012    -0.574    0.566     -89.737      49.098 
    ##                     month_yearFebruary_2020    -12.894        44.707       -0.011    -0.288    0.773    -100.551      74.764 
    ##                     month_yearFebruary_2021    -42.163        36.772       -0.024    -1.147    0.252    -114.262      29.937 
    ##                      month_yearJanuary_2021    -39.032        36.198       -0.022    -1.078    0.281    -110.006      31.941 
    ##                         month_yearJuly_2020    811.626       113.124        0.127     7.175    0.000     589.820    1033.432 
    ##                         month_yearJune_2020     41.686        31.124        0.030     1.339    0.181     -19.340     102.711 
    ##                        month_yearMarch_2020    -40.106        44.818       -0.034    -0.895    0.371    -127.982      47.770 
    ##                        month_yearMarch_2021    -43.094        38.752       -0.023    -1.112    0.266    -119.076      32.888 
    ##                          month_yearMay_2020     36.555        28.630        0.030     1.277    0.202     -19.581      92.692 
    ##                     month_yearNovember_2020    -13.269        33.848       -0.008    -0.392    0.695     -79.636      53.099 
    ##                      month_yearOctober_2020    -32.114        34.735       -0.020    -0.925    0.355    -100.221      35.992 
    ##                    month_yearSeptember_2020      5.815        33.014        0.004     0.176    0.860     -58.916      70.546 
    ##                           reviews_per_month     -8.326         3.550       -0.044    -2.345    0.019     -15.287      -1.365 
    ##                       host_is_superhostTRUE    -15.412        14.615       -0.020    -1.055    0.292     -44.068      13.244 
    ##                              maximum_nights      0.010         0.013        0.013     0.724    0.469      -0.017       0.036 
    ##                          host_response_rate     -0.461         0.365       -0.052    -1.262    0.207      -1.176       0.255 
    ##                   review_scores_cleanliness      9.621         7.297        0.025     1.319    0.187      -4.686      23.929 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## - maximum_nights 
    ## 
    ## Backward Elimination: Step 3 
    ## 
    ##  Variable maximum_nights Removed 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.226       RMSE                  368.373 
    ## R-Squared               0.051       Coef. Var             237.030 
    ## Adj. R-Squared          0.044       MSE                135699.035 
    ## Pred R-Squared          0.036       MAE                    84.670 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                   ANOVA                                    
    ## --------------------------------------------------------------------------
    ##                      Sum of                                               
    ##                     Squares          DF    Mean Square      F        Sig. 
    ## --------------------------------------------------------------------------
    ## Regression     22841411.488          24     951725.479    7.014    0.0000 
    ## Residual      422838193.598        3116     135699.035                    
    ## Total         445679605.086        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                      Parameter Estimates                                                      
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower       upper 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     13.978        77.074                  0.181    0.856    -137.142     165.099 
    ##                                    bedrooms     63.788        10.096        0.129     6.318    0.000      43.994      83.583 
    ##                                   bathrooms     30.244        14.304        0.043     2.114    0.035       2.199      58.290 
    ##   neighbourhood_cleansedBay Street Corridor     13.058        18.979        0.014     0.688    0.491     -24.154      50.270 
    ## neighbourhood_cleansedChurch-Yonge Corridor     40.449        19.262        0.043     2.100    0.036       2.682      78.215 
    ##  neighbourhood_cleansedKensington-Chinatown    -41.290        18.987       -0.046    -2.175    0.030     -78.519      -4.061 
    ##            neighbourhood_cleansedUniversity    -33.006        26.826       -0.024    -1.230    0.219     -85.605      19.593 
    ##                        month_yearApril_2021    -20.401        36.154       -0.012    -0.564    0.573     -91.289      50.487 
    ##                       month_yearAugust_2020      4.003        34.747        0.002     0.115    0.908     -64.126      72.131 
    ##                     month_yearDecember_2020    -20.322        35.401       -0.012    -0.574    0.566     -89.734      49.090 
    ##                     month_yearFebruary_2020    -14.812        44.625       -0.013    -0.332    0.740    -102.309      72.685 
    ##                     month_yearFebruary_2021    -42.556        36.765       -0.024    -1.158    0.247    -114.642      29.530 
    ##                      month_yearJanuary_2021    -39.191        36.194       -0.023    -1.083    0.279    -110.158      31.776 
    ##                         month_yearJuly_2020    811.483       113.116        0.127     7.174    0.000     589.695    1033.272 
    ##                         month_yearJune_2020     41.404        31.119        0.030     1.331    0.183     -19.612     102.420 
    ##                        month_yearMarch_2020    -42.132        44.727       -0.035    -0.942    0.346    -129.830      45.566 
    ##                        month_yearMarch_2021    -42.799        38.747       -0.022    -1.105    0.269    -118.771      33.173 
    ##                          month_yearMay_2020     36.422        28.628        0.030     1.272    0.203     -19.709      92.553 
    ##                     month_yearNovember_2020    -14.176        33.823       -0.009    -0.419    0.675     -80.493      52.141 
    ##                      month_yearOctober_2020    -31.785        34.730       -0.019    -0.915    0.360     -99.880      36.311 
    ##                    month_yearSeptember_2020      6.184        33.007        0.004     0.187    0.851     -58.535      70.902 
    ##                           reviews_per_month     -8.645         3.523       -0.045    -2.454    0.014     -15.552      -1.738 
    ##                       host_is_superhostTRUE    -15.252        14.612       -0.020    -1.044    0.297     -43.903      13.399 
    ##                          host_response_rate     -0.487         0.363       -0.055    -1.340    0.180      -1.199       0.225 
    ##                   review_scores_cleanliness      9.708         7.296        0.025     1.331    0.183      -4.596      24.013 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## No more variables satisfy the condition of p value = 0.3
    ## 
    ## 
    ## Variables Removed: 
    ## 
    ## - review_scores_rating 
    ## - minimum_nights 
    ## - maximum_nights 
    ## 
    ## 
    ## Final Model Output 
    ## ------------------
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.226       RMSE                  368.373 
    ## R-Squared               0.051       Coef. Var             237.030 
    ## Adj. R-Squared          0.044       MSE                135699.035 
    ## Pred R-Squared          0.036       MAE                    84.670 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                   ANOVA                                    
    ## --------------------------------------------------------------------------
    ##                      Sum of                                               
    ##                     Squares          DF    Mean Square      F        Sig. 
    ## --------------------------------------------------------------------------
    ## Regression     22841411.488          24     951725.479    7.014    0.0000 
    ## Residual      422838193.598        3116     135699.035                    
    ## Total         445679605.086        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                      Parameter Estimates                                                      
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower       upper 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     13.978        77.074                  0.181    0.856    -137.142     165.099 
    ##                                    bedrooms     63.788        10.096        0.129     6.318    0.000      43.994      83.583 
    ##                                   bathrooms     30.244        14.304        0.043     2.114    0.035       2.199      58.290 
    ##   neighbourhood_cleansedBay Street Corridor     13.058        18.979        0.014     0.688    0.491     -24.154      50.270 
    ## neighbourhood_cleansedChurch-Yonge Corridor     40.449        19.262        0.043     2.100    0.036       2.682      78.215 
    ##  neighbourhood_cleansedKensington-Chinatown    -41.290        18.987       -0.046    -2.175    0.030     -78.519      -4.061 
    ##            neighbourhood_cleansedUniversity    -33.006        26.826       -0.024    -1.230    0.219     -85.605      19.593 
    ##                        month_yearApril_2021    -20.401        36.154       -0.012    -0.564    0.573     -91.289      50.487 
    ##                       month_yearAugust_2020      4.003        34.747        0.002     0.115    0.908     -64.126      72.131 
    ##                     month_yearDecember_2020    -20.322        35.401       -0.012    -0.574    0.566     -89.734      49.090 
    ##                     month_yearFebruary_2020    -14.812        44.625       -0.013    -0.332    0.740    -102.309      72.685 
    ##                     month_yearFebruary_2021    -42.556        36.765       -0.024    -1.158    0.247    -114.642      29.530 
    ##                      month_yearJanuary_2021    -39.191        36.194       -0.023    -1.083    0.279    -110.158      31.776 
    ##                         month_yearJuly_2020    811.483       113.116        0.127     7.174    0.000     589.695    1033.272 
    ##                         month_yearJune_2020     41.404        31.119        0.030     1.331    0.183     -19.612     102.420 
    ##                        month_yearMarch_2020    -42.132        44.727       -0.035    -0.942    0.346    -129.830      45.566 
    ##                        month_yearMarch_2021    -42.799        38.747       -0.022    -1.105    0.269    -118.771      33.173 
    ##                          month_yearMay_2020     36.422        28.628        0.030     1.272    0.203     -19.709      92.553 
    ##                     month_yearNovember_2020    -14.176        33.823       -0.009    -0.419    0.675     -80.493      52.141 
    ##                      month_yearOctober_2020    -31.785        34.730       -0.019    -0.915    0.360     -99.880      36.311 
    ##                    month_yearSeptember_2020      6.184        33.007        0.004     0.187    0.851     -58.535      70.902 
    ##                           reviews_per_month     -8.645         3.523       -0.045    -2.454    0.014     -15.552      -1.738 
    ##                       host_is_superhostTRUE    -15.252        14.612       -0.020    -1.044    0.297     -43.903      13.399 
    ##                          host_response_rate     -0.487         0.363       -0.055    -1.340    0.180      -1.199       0.225 
    ##                   review_scores_cleanliness      9.708         7.296        0.025     1.331    0.183      -4.596      24.013 
    ## -----------------------------------------------------------------------------------------------------------------------------

    ## 
    ## 
    ##                                    Elimination Summary                                    
    ## -----------------------------------------------------------------------------------------
    ##         Variable                              Adj.                                           
    ## Step          Removed           R-Square    R-Square     C(p)         AIC          RMSE      
    ## -----------------------------------------------------------------------------------------
    ##    1    review_scores_rating      0.0514      0.0435    -5.9803    46064.9710    368.4536    
    ##    2    minimum_nights            0.0514      0.0438    -7.8600    46063.0924    368.4016    
    ##    3    maximum_nights            0.0513      0.0439    -9.3355    46061.6215    368.3735    
    ## -----------------------------------------------------------------------------------------

Now we can run a forward selection stepwise regression.

``` r
#fitting an intercept only model
quality_vs_price_interceptonly <- lm(price ~ 1, data = airbnb_data)
#forward selection
ols_step_forward_p(quality_vs_price, details = TRUE)
```

    ## Forward Selection Method    
    ## ---------------------------
    ## 
    ## Candidate Terms: 
    ## 
    ## 1. bedrooms 
    ## 2. bathrooms 
    ## 3. neighbourhood_cleansed 
    ## 4. month_year 
    ## 5. reviews_per_month 
    ## 6. host_is_superhost 
    ## 7. minimum_nights 
    ## 8. maximum_nights 
    ## 9. review_scores_rating 
    ## 10. host_response_rate 
    ## 11. review_scores_cleanliness 
    ## 
    ## We are selecting variables based on p value...
    ## 
    ## 
    ## Forward Selection: Step 1 
    ## 
    ## - bathrooms 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.191       RMSE                  401.802 
    ## R-Squared               0.036       Coef. Var             249.720 
    ## Adj. R-Squared          0.036       MSE                161444.668 
    ## Pred R-Squared          0.033       MAE                    99.146 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                     ANOVA                                      
    ## ------------------------------------------------------------------------------
    ##                       Sum of                                                  
    ##                      Squares          DF     Mean Square       F         Sig. 
    ## ------------------------------------------------------------------------------
    ## Regression      44107905.681           1    44107905.681    273.208    0.0000 
    ## Residual      1169343731.559        7243      161444.668                      
    ## Total         1213451637.240        7244                                      
    ## ------------------------------------------------------------------------------
    ## 
    ##                                     Parameter Estimates                                     
    ## -------------------------------------------------------------------------------------------
    ##       model       Beta    Std. Error    Std. Beta      t        Sig       lower      upper 
    ## -------------------------------------------------------------------------------------------
    ## (Intercept)     -9.880        11.360                 -0.870    0.384    -32.148     12.388 
    ##   bathrooms    138.930         8.405        0.191    16.529    0.000    122.453    155.407 
    ## -------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## Forward Selection: Step 2 
    ## 
    ## - month_year 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.236       RMSE                  398.158 
    ## R-Squared               0.056       Coef. Var             247.455 
    ## Adj. R-Squared          0.054       MSE                158529.668 
    ## Pred R-Squared          0.049       MAE                    96.644 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                    ANOVA                                     
    ## ----------------------------------------------------------------------------
    ##                       Sum of                                                
    ##                      Squares          DF    Mean Square      F         Sig. 
    ## ----------------------------------------------------------------------------
    ## Regression      67440667.118          15    4496044.475    28.361    0.0000 
    ## Residual      1146010970.122        7229     158529.668                     
    ## Total         1213451637.240        7244                                    
    ## ----------------------------------------------------------------------------
    ## 
    ##                                           Parameter Estimates                                            
    ## --------------------------------------------------------------------------------------------------------
    ##                    model       Beta    Std. Error    Std. Beta      t        Sig       lower      upper 
    ## --------------------------------------------------------------------------------------------------------
    ##              (Intercept)    -29.704        19.490                 -1.524    0.128    -67.909      8.501 
    ##                bathrooms    139.008         8.337        0.191    16.675    0.000    122.666    155.350 
    ##     month_yearApril_2021     38.654        24.984        0.023     1.547    0.122    -10.323     87.631 
    ##    month_yearAugust_2020     15.425        24.166        0.010     0.638    0.523    -31.948     62.799 
    ##  month_yearDecember_2020      2.333        24.788        0.001     0.094    0.925    -46.257     50.924 
    ##  month_yearFebruary_2020     32.526        23.089        0.022     1.409    0.159    -12.735     77.786 
    ##  month_yearFebruary_2021    -11.036        25.555       -0.006    -0.432    0.666    -61.130     39.059 
    ##   month_yearJanuary_2021     -8.438        24.457       -0.005    -0.345    0.730    -56.380     39.505 
    ##      month_yearJuly_2020    719.837        63.612        0.134    11.316    0.000    595.139    844.534 
    ##      month_yearJune_2020     28.449        23.433        0.019     1.214    0.225    -17.487     74.386 
    ##     month_yearMarch_2020      5.144        23.026        0.004     0.223    0.823    -39.994     50.281 
    ##     month_yearMarch_2021    -19.366        25.575       -0.011    -0.757    0.449    -69.501     30.770 
    ##       month_yearMay_2020     36.864        23.062        0.025     1.598    0.110     -8.344     82.071 
    ##  month_yearNovember_2020     35.273        24.192        0.022     1.458    0.145    -12.150     82.696 
    ##   month_yearOctober_2020      9.133        24.821        0.005     0.368    0.713    -39.525     57.790 
    ## month_yearSeptember_2020     37.876        24.115        0.024     1.571    0.116     -9.397     85.150 
    ## --------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## Forward Selection: Step 3 
    ## 
    ## - bedrooms 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.246       RMSE                  412.609 
    ## R-Squared               0.060       Coef. Var             250.051 
    ## Adj. R-Squared          0.058       MSE                170246.102 
    ## Pred R-Squared          0.053       MAE                    99.214 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                    ANOVA                                     
    ## ----------------------------------------------------------------------------
    ##                       Sum of                                                
    ##                      Squares          DF    Mean Square      F         Sig. 
    ## ----------------------------------------------------------------------------
    ## Regression      73008842.449          16    4563052.653    26.803    0.0000 
    ## Residual      1134690266.859        6665     170246.102                     
    ## Total         1207699109.308        6681                                    
    ## ----------------------------------------------------------------------------
    ## 
    ##                                           Parameter Estimates                                            
    ## --------------------------------------------------------------------------------------------------------
    ##                    model       Beta    Std. Error    Std. Beta      t        Sig       lower      upper 
    ## --------------------------------------------------------------------------------------------------------
    ##              (Intercept)    -43.479        20.346                 -2.137    0.033    -83.364     -3.594 
    ##                bathrooms    100.296        10.320        0.137     9.719    0.000     80.066    120.525 
    ##     month_yearApril_2021     38.542        26.926        0.021     1.431    0.152    -14.242     91.325 
    ##    month_yearAugust_2020     12.267        25.970        0.007     0.472    0.637    -38.643     63.177 
    ##  month_yearDecember_2020     -2.970        26.756       -0.002    -0.111    0.912    -55.420     49.480 
    ##  month_yearFebruary_2020     34.270        23.928        0.023     1.432    0.152    -12.637     81.176 
    ##  month_yearFebruary_2021    -16.620        27.814       -0.009    -0.598    0.550    -71.144     37.904 
    ##   month_yearJanuary_2021    -17.464        26.436       -0.010    -0.661    0.509    -69.287     34.359 
    ##      month_yearJuly_2020    721.931        69.962        0.126    10.319    0.000    584.783    859.079 
    ##      month_yearJune_2020     29.895        24.285        0.020     1.231    0.218    -17.711     77.501 
    ##     month_yearMarch_2020      3.402        23.863        0.002     0.143    0.887    -43.377     50.182 
    ##     month_yearMarch_2021    -30.032        27.804       -0.016    -1.080    0.280    -84.536     24.472 
    ##       month_yearMay_2020     36.460        23.899        0.025     1.526    0.127    -10.389     83.309 
    ##  month_yearNovember_2020     33.507        26.014        0.020     1.288    0.198    -17.489     84.503 
    ##   month_yearOctober_2020      1.576        26.729        0.001     0.059    0.953    -50.822     53.974 
    ## month_yearSeptember_2020     31.872        26.150        0.019     1.219    0.223    -19.390     83.134 
    ##                 bedrooms     53.013         7.556        0.099     7.016    0.000     38.200     67.827 
    ## --------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## Forward Selection: Step 4 
    ## 
    ## - neighbourhood_cleansed 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.253       RMSE                  411.952 
    ## R-Squared               0.064       Coef. Var             249.653 
    ## Adj. R-Squared          0.061       MSE                169704.554 
    ## Pred R-Squared          0.056       MAE                    99.661 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                    ANOVA                                     
    ## ----------------------------------------------------------------------------
    ##                       Sum of                                                
    ##                      Squares          DF    Mean Square      F         Sig. 
    ## ----------------------------------------------------------------------------
    ## Regression      77297074.792          20    3864853.740    22.774    0.0000 
    ## Residual      1130402034.515        6661     169704.554                     
    ## Total         1207699109.308        6681                                    
    ## ----------------------------------------------------------------------------
    ## 
    ##                                                     Parameter Estimates                                                      
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower      upper 
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)    -24.302        22.139                 -1.098    0.272     -67.701     19.097 
    ##                                   bathrooms    100.522        10.362        0.137     9.701    0.000      80.208    120.835 
    ##                        month_yearApril_2021     42.030        26.910        0.023     1.562    0.118     -10.723     94.782 
    ##                       month_yearAugust_2020     14.510        25.937        0.009     0.559    0.576     -36.334     65.355 
    ##                     month_yearDecember_2020      0.516        26.739        0.000     0.019    0.985     -51.901     52.933 
    ##                     month_yearFebruary_2020     34.978        23.892        0.024     1.464    0.143     -11.858     81.815 
    ##                     month_yearFebruary_2021    -12.459        27.790       -0.007    -0.448    0.654     -66.936     42.018 
    ##                      month_yearJanuary_2021    -16.492        26.421       -0.009    -0.624    0.533     -68.286     35.302 
    ##                         month_yearJuly_2020    722.540        69.858        0.126    10.343    0.000     585.596    859.483 
    ##                         month_yearJune_2020     31.812        24.260        0.021     1.311    0.190     -15.746     79.370 
    ##                        month_yearMarch_2020      3.616        23.827        0.002     0.152    0.879     -43.092     50.324 
    ##                        month_yearMarch_2021    -27.109        27.778       -0.014    -0.976    0.329     -81.563     27.344 
    ##                          month_yearMay_2020     36.050        23.867        0.024     1.510    0.131     -10.737     82.836 
    ##                     month_yearNovember_2020     37.001        25.990        0.022     1.424    0.155     -13.948     87.950 
    ##                      month_yearOctober_2020      3.542        26.696        0.002     0.133    0.894     -48.790     55.873 
    ##                    month_yearSeptember_2020     33.908        26.120        0.020     1.298    0.194     -17.295     85.111 
    ##                                    bedrooms     54.974         7.572        0.103     7.261    0.000      40.131     69.816 
    ##   neighbourhood_cleansedBay Street Corridor    -13.119        14.724       -0.013    -0.891    0.373     -41.984     15.745 
    ## neighbourhood_cleansedChurch-Yonge Corridor    -10.130        14.625       -0.010    -0.693    0.489     -38.799     18.539 
    ##  neighbourhood_cleansedKensington-Chinatown    -61.938        14.703       -0.060    -4.213    0.000     -90.760    -33.115 
    ##            neighbourhood_cleansedUniversity    -61.293        20.048       -0.040    -3.057    0.002    -100.594    -21.993 
    ## ----------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## Forward Selection: Step 5 
    ## 
    ## - host_response_rate 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.210       RMSE                  386.156 
    ## R-Squared               0.044       Coef. Var             237.979 
    ## Adj. R-Squared          0.039       MSE                149116.113 
    ## Pred R-Squared          0.033       MAE                    92.460 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                   ANOVA                                    
    ## --------------------------------------------------------------------------
    ##                      Sum of                                               
    ##                     Squares          DF    Mean Square      F        Sig. 
    ## --------------------------------------------------------------------------
    ## Regression     26601999.975          21    1266761.904    8.495    0.0000 
    ## Residual      577675822.661        3874     149116.113                    
    ## Total         604277822.637        3895                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                      Parameter Estimates                                                      
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower       upper 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     75.609        39.998                  1.890    0.059      -2.810     154.027 
    ##                                   bathrooms     48.894        12.937        0.071     3.780    0.000      23.531      74.257 
    ##                        month_yearApril_2021    -10.150        33.228       -0.006    -0.305    0.760     -75.297      54.996 
    ##                       month_yearAugust_2020     11.388        32.245        0.007     0.353    0.724     -51.830      74.606 
    ##                     month_yearDecember_2020    -11.830        33.788       -0.007    -0.350    0.726     -78.074      54.414 
    ##                     month_yearFebruary_2020      5.012        39.868        0.004     0.126    0.900     -73.153      83.177 
    ##                     month_yearFebruary_2021    -17.003        33.337       -0.010    -0.510    0.610     -82.363      48.356 
    ##                      month_yearJanuary_2021    -33.793        33.537       -0.019    -1.008    0.314     -99.546      31.959 
    ##                         month_yearJuly_2020    821.330       113.258        0.116     7.252    0.000     599.279    1043.381 
    ##                         month_yearJune_2020     54.187        29.829        0.037     1.817    0.069      -4.294     112.669 
    ##                        month_yearMarch_2020    -43.306        40.071       -0.034    -1.081    0.280    -121.868      35.257 
    ##                        month_yearMarch_2021    -41.374        34.744       -0.022    -1.191    0.234    -109.493      26.745 
    ##                          month_yearMay_2020     50.030        27.811        0.038     1.799    0.072      -4.495     104.556 
    ##                     month_yearNovember_2020      4.514        31.917        0.003     0.141    0.888     -58.061      67.090 
    ##                      month_yearOctober_2020    -13.556        32.660       -0.008    -0.415    0.678     -77.589      50.476 
    ##                    month_yearSeptember_2020     12.911        31.260        0.008     0.413    0.680     -48.376      74.199 
    ##                                    bedrooms     51.028         9.244        0.104     5.520    0.000      32.905      69.151 
    ##   neighbourhood_cleansedBay Street Corridor     12.685        17.735        0.014     0.715    0.474     -22.085      47.455 
    ## neighbourhood_cleansedChurch-Yonge Corridor     24.432        18.305        0.025     1.335    0.182     -11.455      60.320 
    ##  neighbourhood_cleansedKensington-Chinatown    -41.230        17.800       -0.044    -2.316    0.021     -76.129      -6.331 
    ##            neighbourhood_cleansedUniversity    -39.941        25.383       -0.027    -1.574    0.116     -89.706       9.825 
    ##                          host_response_rate     -0.531         0.308       -0.057    -1.725    0.085      -1.134       0.073 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## Forward Selection: Step 6 
    ## 
    ## - reviews_per_month 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.224       RMSE                  366.512 
    ## R-Squared               0.050       Coef. Var             236.175 
    ## Adj. R-Squared          0.044       MSE                134331.294 
    ## Pred R-Squared          0.036       MAE                    84.026 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                   ANOVA                                    
    ## --------------------------------------------------------------------------
    ##                      Sum of                                               
    ##                     Squares          DF    Mean Square      F        Sig. 
    ## --------------------------------------------------------------------------
    ## Regression     22404376.811          22    1018380.764    7.581    0.0000 
    ## Residual      423546569.468        3153     134331.294                    
    ## Total         445950946.279        3175                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                      Parameter Estimates                                                      
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower       upper 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     97.692        44.114                  2.215    0.027      11.197     184.186 
    ##                                   bathrooms     30.087        14.090        0.044     2.135    0.033       2.459      57.714 
    ##                        month_yearApril_2021    -19.423        35.717       -0.011    -0.544    0.587     -89.455      50.609 
    ##                       month_yearAugust_2020      3.661        34.273        0.002     0.107    0.915     -63.539      70.860 
    ##                     month_yearDecember_2020    -19.979        35.006       -0.012    -0.571    0.568     -88.616      48.658 
    ##                     month_yearFebruary_2020    -11.130        43.187       -0.009    -0.258    0.797     -95.808      73.549 
    ##                     month_yearFebruary_2021    -40.266        36.453       -0.023    -1.105    0.269    -111.740      31.209 
    ##                      month_yearJanuary_2021    -38.070        35.683       -0.022    -1.067    0.286    -108.034      31.893 
    ##                         month_yearJuly_2020    814.895       112.466        0.128     7.246    0.000     594.382    1035.408 
    ##                         month_yearJune_2020     39.192        30.718        0.028     1.276    0.202     -21.036      99.421 
    ##                        month_yearMarch_2020    -39.073        43.390       -0.033    -0.901    0.368    -124.148      46.002 
    ##                        month_yearMarch_2021    -41.440        38.171       -0.022    -1.086    0.278    -116.283      33.402 
    ##                          month_yearMay_2020     37.032        28.264        0.031     1.310    0.190     -18.385      92.449 
    ##                     month_yearNovember_2020    -13.429        33.428       -0.009    -0.402    0.688     -78.971      52.113 
    ##                      month_yearOctober_2020    -29.905        34.338       -0.018    -0.871    0.384     -97.232      37.422 
    ##                    month_yearSeptember_2020      7.109        32.563        0.005     0.218    0.827     -56.737      70.956 
    ##                                    bedrooms     61.586         9.919        0.128     6.209    0.000      42.138      81.035 
    ##   neighbourhood_cleansedBay Street Corridor     12.345        18.725        0.014     0.659    0.510     -24.370      49.059 
    ## neighbourhood_cleansedChurch-Yonge Corridor     40.043        19.114        0.043     2.095    0.036       2.566      77.519 
    ##  neighbourhood_cleansedKensington-Chinatown    -43.549        18.721       -0.048    -2.326    0.020     -80.256      -6.842 
    ##            neighbourhood_cleansedUniversity    -36.713        26.345       -0.027    -1.394    0.164     -88.367      14.942 
    ##                          host_response_rate     -0.453         0.349       -0.052    -1.297    0.195      -1.137       0.232 
    ##                           reviews_per_month     -8.338         3.385       -0.045    -2.463    0.014     -14.976      -1.701 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## Forward Selection: Step 7 
    ## 
    ## - review_scores_cleanliness 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.226       RMSE                  368.379 
    ## R-Squared               0.051       Coef. Var             237.033 
    ## Adj. R-Squared          0.044       MSE                135702.930 
    ## Pred R-Squared          0.036       MAE                    84.192 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                   ANOVA                                    
    ## --------------------------------------------------------------------------
    ##                      Sum of                                               
    ##                     Squares          DF    Mean Square      F        Sig. 
    ## --------------------------------------------------------------------------
    ## Regression     22693571.707          23     986677.031    7.271    0.0000 
    ## Residual      422986033.379        3117     135702.930                    
    ## Total         445679605.086        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                      Parameter Estimates                                                      
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower       upper 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     36.068        74.113                  0.487    0.627    -109.246     181.382 
    ##                                   bathrooms     31.170        14.277        0.044     2.183    0.029       3.178      59.162 
    ##                        month_yearApril_2021    -22.244        36.111       -0.013    -0.616    0.538     -93.049      48.560 
    ##                       month_yearAugust_2020      2.887        34.731        0.002     0.083    0.934     -65.210      70.984 
    ##                     month_yearDecember_2020    -22.061        35.363       -0.013    -0.624    0.533     -91.398      47.275 
    ##                     month_yearFebruary_2020    -20.394        44.304       -0.017    -0.460    0.645    -107.262      66.473 
    ##                     month_yearFebruary_2021    -43.051        36.762       -0.024    -1.171    0.242    -115.132      29.030 
    ##                      month_yearJanuary_2021    -40.822        36.161       -0.023    -1.129    0.259    -111.724      30.080 
    ##                         month_yearJuly_2020    809.973       113.108        0.127     7.161    0.000     588.199    1031.746 
    ##                         month_yearJune_2020     39.591        31.071        0.028     1.274    0.203     -21.331     100.512 
    ##                        month_yearMarch_2020    -47.327        44.450       -0.040    -1.065    0.287    -134.482      39.827 
    ##                        month_yearMarch_2021    -44.592        38.709       -0.023    -1.152    0.249    -120.490      31.307 
    ##                          month_yearMay_2020     36.151        28.627        0.030     1.263    0.207     -19.978      92.281 
    ##                     month_yearNovember_2020    -15.883        33.784       -0.010    -0.470    0.638     -82.123      50.358 
    ##                      month_yearOctober_2020    -33.120        34.707       -0.020    -0.954    0.340    -101.170      34.931 
    ##                    month_yearSeptember_2020      3.827        32.931        0.003     0.116    0.907     -60.741      68.395 
    ##                                    bedrooms     63.204        10.080        0.128     6.270    0.000      43.440      82.968 
    ##   neighbourhood_cleansedBay Street Corridor     12.074        18.956        0.013     0.637    0.524     -25.093      49.240 
    ## neighbourhood_cleansedChurch-Yonge Corridor     40.500        19.262        0.043     2.103    0.036       2.733      78.267 
    ##  neighbourhood_cleansedKensington-Chinatown    -40.977        18.985       -0.045    -2.158    0.031     -78.202      -3.752 
    ##            neighbourhood_cleansedUniversity    -35.233        26.742       -0.025    -1.318    0.188     -87.666      17.201 
    ##                          host_response_rate     -0.537         0.360       -0.061    -1.492    0.136      -1.243       0.169 
    ##                           reviews_per_month     -9.198         3.483       -0.048    -2.641    0.008     -16.027      -2.370 
    ##                   review_scores_cleanliness      7.391         6.950        0.019     1.064    0.288      -6.235      21.018 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## Forward Selection: Step 8 
    ## 
    ## - host_is_superhost 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.226       RMSE                  368.373 
    ## R-Squared               0.051       Coef. Var             237.030 
    ## Adj. R-Squared          0.044       MSE                135699.035 
    ## Pred R-Squared          0.036       MAE                    84.670 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                   ANOVA                                    
    ## --------------------------------------------------------------------------
    ##                      Sum of                                               
    ##                     Squares          DF    Mean Square      F        Sig. 
    ## --------------------------------------------------------------------------
    ## Regression     22841411.488          24     951725.479    7.014    0.0000 
    ## Residual      422838193.598        3116     135699.035                    
    ## Total         445679605.086        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                      Parameter Estimates                                                      
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower       upper 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     13.978        77.074                  0.181    0.856    -137.142     165.099 
    ##                                   bathrooms     30.244        14.304        0.043     2.114    0.035       2.199      58.290 
    ##                        month_yearApril_2021    -20.401        36.154       -0.012    -0.564    0.573     -91.289      50.487 
    ##                       month_yearAugust_2020      4.003        34.747        0.002     0.115    0.908     -64.126      72.131 
    ##                     month_yearDecember_2020    -20.322        35.401       -0.012    -0.574    0.566     -89.734      49.090 
    ##                     month_yearFebruary_2020    -14.812        44.625       -0.013    -0.332    0.740    -102.309      72.685 
    ##                     month_yearFebruary_2021    -42.556        36.765       -0.024    -1.158    0.247    -114.642      29.530 
    ##                      month_yearJanuary_2021    -39.191        36.194       -0.023    -1.083    0.279    -110.158      31.776 
    ##                         month_yearJuly_2020    811.483       113.116        0.127     7.174    0.000     589.695    1033.272 
    ##                         month_yearJune_2020     41.404        31.119        0.030     1.331    0.183     -19.612     102.420 
    ##                        month_yearMarch_2020    -42.132        44.727       -0.035    -0.942    0.346    -129.830      45.566 
    ##                        month_yearMarch_2021    -42.799        38.747       -0.022    -1.105    0.269    -118.771      33.173 
    ##                          month_yearMay_2020     36.422        28.628        0.030     1.272    0.203     -19.709      92.553 
    ##                     month_yearNovember_2020    -14.176        33.823       -0.009    -0.419    0.675     -80.493      52.141 
    ##                      month_yearOctober_2020    -31.785        34.730       -0.019    -0.915    0.360     -99.880      36.311 
    ##                    month_yearSeptember_2020      6.184        33.007        0.004     0.187    0.851     -58.535      70.902 
    ##                                    bedrooms     63.788        10.096        0.129     6.318    0.000      43.994      83.583 
    ##   neighbourhood_cleansedBay Street Corridor     13.058        18.979        0.014     0.688    0.491     -24.154      50.270 
    ## neighbourhood_cleansedChurch-Yonge Corridor     40.449        19.262        0.043     2.100    0.036       2.682      78.215 
    ##  neighbourhood_cleansedKensington-Chinatown    -41.290        18.987       -0.046    -2.175    0.030     -78.519      -4.061 
    ##            neighbourhood_cleansedUniversity    -33.006        26.826       -0.024    -1.230    0.219     -85.605      19.593 
    ##                          host_response_rate     -0.487         0.363       -0.055    -1.340    0.180      -1.199       0.225 
    ##                           reviews_per_month     -8.645         3.523       -0.045    -2.454    0.014     -15.552      -1.738 
    ##                   review_scores_cleanliness      9.708         7.296        0.025     1.331    0.183      -4.596      24.013 
    ##                       host_is_superhostTRUE    -15.252        14.612       -0.020    -1.044    0.297     -43.903      13.399 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## No more variables to be added.
    ## 
    ## Variables Entered: 
    ## 
    ## + bathrooms 
    ## + month_year 
    ## + bedrooms 
    ## + neighbourhood_cleansed 
    ## + host_response_rate 
    ## + reviews_per_month 
    ## + review_scores_cleanliness 
    ## + host_is_superhost 
    ## 
    ## 
    ## Final Model Output 
    ## ------------------
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.226       RMSE                  368.373 
    ## R-Squared               0.051       Coef. Var             237.030 
    ## Adj. R-Squared          0.044       MSE                135699.035 
    ## Pred R-Squared          0.036       MAE                    84.670 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                   ANOVA                                    
    ## --------------------------------------------------------------------------
    ##                      Sum of                                               
    ##                     Squares          DF    Mean Square      F        Sig. 
    ## --------------------------------------------------------------------------
    ## Regression     22841411.488          24     951725.479    7.014    0.0000 
    ## Residual      422838193.598        3116     135699.035                    
    ## Total         445679605.086        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                      Parameter Estimates                                                      
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower       upper 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     13.978        77.074                  0.181    0.856    -137.142     165.099 
    ##                                   bathrooms     30.244        14.304        0.043     2.114    0.035       2.199      58.290 
    ##                        month_yearApril_2021    -20.401        36.154       -0.012    -0.564    0.573     -91.289      50.487 
    ##                       month_yearAugust_2020      4.003        34.747        0.002     0.115    0.908     -64.126      72.131 
    ##                     month_yearDecember_2020    -20.322        35.401       -0.012    -0.574    0.566     -89.734      49.090 
    ##                     month_yearFebruary_2020    -14.812        44.625       -0.013    -0.332    0.740    -102.309      72.685 
    ##                     month_yearFebruary_2021    -42.556        36.765       -0.024    -1.158    0.247    -114.642      29.530 
    ##                      month_yearJanuary_2021    -39.191        36.194       -0.023    -1.083    0.279    -110.158      31.776 
    ##                         month_yearJuly_2020    811.483       113.116        0.127     7.174    0.000     589.695    1033.272 
    ##                         month_yearJune_2020     41.404        31.119        0.030     1.331    0.183     -19.612     102.420 
    ##                        month_yearMarch_2020    -42.132        44.727       -0.035    -0.942    0.346    -129.830      45.566 
    ##                        month_yearMarch_2021    -42.799        38.747       -0.022    -1.105    0.269    -118.771      33.173 
    ##                          month_yearMay_2020     36.422        28.628        0.030     1.272    0.203     -19.709      92.553 
    ##                     month_yearNovember_2020    -14.176        33.823       -0.009    -0.419    0.675     -80.493      52.141 
    ##                      month_yearOctober_2020    -31.785        34.730       -0.019    -0.915    0.360     -99.880      36.311 
    ##                    month_yearSeptember_2020      6.184        33.007        0.004     0.187    0.851     -58.535      70.902 
    ##                                    bedrooms     63.788        10.096        0.129     6.318    0.000      43.994      83.583 
    ##   neighbourhood_cleansedBay Street Corridor     13.058        18.979        0.014     0.688    0.491     -24.154      50.270 
    ## neighbourhood_cleansedChurch-Yonge Corridor     40.449        19.262        0.043     2.100    0.036       2.682      78.215 
    ##  neighbourhood_cleansedKensington-Chinatown    -41.290        18.987       -0.046    -2.175    0.030     -78.519      -4.061 
    ##            neighbourhood_cleansedUniversity    -33.006        26.826       -0.024    -1.230    0.219     -85.605      19.593 
    ##                          host_response_rate     -0.487         0.363       -0.055    -1.340    0.180      -1.199       0.225 
    ##                           reviews_per_month     -8.645         3.523       -0.045    -2.454    0.014     -15.552      -1.738 
    ##                   review_scores_cleanliness      9.708         7.296        0.025     1.331    0.183      -4.596      24.013 
    ##                       host_is_superhostTRUE    -15.252        14.612       -0.020    -1.044    0.297     -43.903      13.399 
    ## -----------------------------------------------------------------------------------------------------------------------------

    ## 
    ##                                         Selection Summary                                         
    ## -------------------------------------------------------------------------------------------------
    ##         Variable                                   Adj.                                              
    ## Step             Entered             R-Square    R-Square      C(p)           AIC          RMSE      
    ## -------------------------------------------------------------------------------------------------
    ##    1    bathrooms                      0.0363      0.0362    1369.7265    107445.8632    401.8018    
    ##    2    month_year                     0.0556      0.0536    1199.9104    107327.8367    398.1578    
    ##    3    bedrooms                       0.0605      0.0582    1681.5479     99466.3651    412.6089    
    ##    4    neighbourhood_cleansed         0.0640      0.0612    1651.9705     99449.0646    411.9521    
    ##    5    host_response_rate             0.0440      0.0388     369.8463     57491.3310    386.1556    
    ##    6    reviews_per_month              0.0502      0.0436     -43.1192     46540.4263    366.5123    
    ##    7    review_scores_cleanliness      0.0509      0.0439     -10.2469     46060.7195    368.3788    
    ##    8    host_is_superhost              0.0513      0.0439      -9.3355     46061.6215    368.3735    
    ## -------------------------------------------------------------------------------------------------

The results from both stepwise regressions match.Both stepwise
regressions selected the variables:

> bedrooms

> bathrooms

> neighbourhood\_cleansed

> month\_year

> reviews\_per\_month

> host\_is\_superhost

> host\_response\_rate

> review\_scores\_cleanliness

for the model.

### Intial Linear Model Full Data:

Now that we have the selected variables, we can try running a linear
model with all selected variables.

``` r
#fitting the initial model
initial_quality_vs_price <- lm(price ~
                         bedrooms +
                         bathrooms +
                         neighbourhood_cleansed +
                         month_year +
                         reviews_per_month +
                         host_is_superhost +
                         host_response_rate +
                         review_scores_cleanliness, data = airbnb_data)
#summary of that model
summary(initial_quality_vs_price)
```

    ## 
    ## Call:
    ## lm(formula = price ~ bedrooms + bathrooms + neighbourhood_cleansed + 
    ##     month_year + reviews_per_month + host_is_superhost + host_response_rate + 
    ##     review_scores_cleanliness, data = airbnb_data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -438.9  -71.1  -28.7   17.1 9791.0 
    ## 
    ## Coefficients:
    ##                                             Estimate Std. Error t value
    ## (Intercept)                                  13.9785    77.0739   0.181
    ## bedrooms                                     63.7885    10.0955   6.318
    ## bathrooms                                    30.2445    14.3038   2.114
    ## neighbourhood_cleansedBay Street Corridor    13.0579    18.9788   0.688
    ## neighbourhood_cleansedChurch-Yonge Corridor  40.4488    19.2615   2.100
    ## neighbourhood_cleansedKensington-Chinatown  -41.2901    18.9874  -2.175
    ## neighbourhood_cleansedUniversity            -33.0057    26.8264  -1.230
    ## month_yearApril_2021                        -20.4010    36.1541  -0.564
    ## month_yearAugust_2020                         4.0029    34.7466   0.115
    ## month_yearDecember_2020                     -20.3223    35.4012  -0.574
    ## month_yearFebruary_2020                     -14.8124    44.6248  -0.332
    ## month_yearFebruary_2021                     -42.5564    36.7650  -1.158
    ## month_yearJanuary_2021                      -39.1907    36.1942  -1.083
    ## month_yearJuly_2020                         811.4833   113.1155   7.174
    ## month_yearJune_2020                          41.4041    31.1192   1.331
    ## month_yearMarch_2020                        -42.1321    44.7274  -0.942
    ## month_yearMarch_2021                        -42.7991    38.7467  -1.105
    ## month_yearMay_2020                           36.4222    28.6277   1.272
    ## month_yearNovember_2020                     -14.1758    33.8227  -0.419
    ## month_yearOctober_2020                      -31.7847    34.7297  -0.915
    ## month_yearSeptember_2020                      6.1837    33.0074   0.187
    ## reviews_per_month                            -8.6452     3.5226  -2.454
    ## host_is_superhostTRUE                       -15.2520    14.6123  -1.044
    ## host_response_rate                           -0.4869     0.3633  -1.340
    ## review_scores_cleanliness                     9.7082     7.2955   1.331
    ##                                             Pr(>|t|)    
    ## (Intercept)                                   0.8561    
    ## bedrooms                                    3.02e-10 ***
    ## bathrooms                                     0.0346 *  
    ## neighbourhood_cleansedBay Street Corridor     0.4915    
    ## neighbourhood_cleansedChurch-Yonge Corridor   0.0358 *  
    ## neighbourhood_cleansedKensington-Chinatown    0.0297 *  
    ## neighbourhood_cleansedUniversity              0.2187    
    ## month_yearApril_2021                          0.5726    
    ## month_yearAugust_2020                         0.9083    
    ## month_yearDecember_2020                       0.5660    
    ## month_yearFebruary_2020                       0.7400    
    ## month_yearFebruary_2021                       0.2471    
    ## month_yearJanuary_2021                        0.2790    
    ## month_yearJuly_2020                         9.07e-13 ***
    ## month_yearJune_2020                           0.1835    
    ## month_yearMarch_2020                          0.3463    
    ## month_yearMarch_2021                          0.2694    
    ## month_yearMay_2020                            0.2034    
    ## month_yearNovember_2020                       0.6752    
    ## month_yearOctober_2020                        0.3602    
    ## month_yearSeptember_2020                      0.8514    
    ## reviews_per_month                             0.0142 *  
    ## host_is_superhostTRUE                         0.2967    
    ## host_response_rate                            0.1802    
    ## review_scores_cleanliness                     0.1834    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 368.4 on 3116 degrees of freedom
    ##   (4683 observations deleted due to missingness)
    ## Multiple R-squared:  0.05125,    Adjusted R-squared:  0.04394 
    ## F-statistic: 7.014 on 24 and 3116 DF,  p-value: < 2.2e-16

We can see that the variables:

> bedrooms

> bathrooms

> neighbourhood\_cleansedChurch-Yonge Corridor

> neighbourhood\_cleansedKensington-Chinatown

> month\_yearJuly\_2020

> reviews\_per\_month

are all significant at the 0.05 significance level.
