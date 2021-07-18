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
#getting rid of characters in price column for July_2020
jul_20_airbnb <- jul_20_airbnb %>% 
  mutate(price = str_remove(price, "\\$"),
         price = str_remove(price, ","),
         price = as.integer(price)
         )
#Converting price to daily scale a from weekly scale
jul_20_airbnb$price <- jul_20_airbnb$price/7
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
    ## $ price                                        <chr> "57.2857142857143", NA, N…
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
    ## R                       0.187       RMSE                  367.739 
    ## R-Squared               0.035       Coef. Var             241.181 
    ## Adj. R-Squared          0.027       MSE                135231.820 
    ## Pred R-Squared          0.019       MAE                    83.949 
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
    ## Regression     15207467.643          26     584902.602    4.325    0.0000 
    ## Residual      421111887.493        3114     135231.820                    
    ## Total         436319355.135        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                     Parameter Estimates                                                      
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower      upper 
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     11.961        77.882                  0.154    0.878    -140.745    164.667 
    ##                                    bedrooms     62.778        10.078        0.129     6.229    0.000      43.017     82.539 
    ##                                   bathrooms     30.017        14.282        0.043     2.102    0.036       2.015     58.020 
    ##   neighbourhood_cleansedBay Street Corridor     12.113        18.951        0.013     0.639    0.523     -25.045     49.270 
    ## neighbourhood_cleansedChurch-Yonge Corridor     39.734        19.229        0.043     2.066    0.039       2.031     77.437 
    ##  neighbourhood_cleansedKensington-Chinatown    -42.064        18.988       -0.047    -2.215    0.027     -79.295     -4.832 
    ##            neighbourhood_cleansedUniversity    -33.149        26.868       -0.024    -1.234    0.217     -85.831     19.532 
    ##                        month_yearApril_2021    -19.044        36.281       -0.011    -0.525    0.600     -90.181     52.093 
    ##                       month_yearAugust_2020      4.117        34.689        0.003     0.119    0.906     -63.898     72.133 
    ##                     month_yearDecember_2020    -20.143        35.349       -0.012    -0.570    0.569     -89.453     49.167 
    ##                     month_yearFebruary_2020    -14.112        44.627       -0.012    -0.316    0.752    -101.613     73.389 
    ##                     month_yearFebruary_2021    -40.827        36.999       -0.023    -1.103    0.270    -113.372     31.717 
    ##                      month_yearJanuary_2021    -38.432        36.179       -0.022    -1.062    0.288    -109.369     32.506 
    ##                         month_yearJuly_2020    -26.459       112.968       -0.004    -0.234    0.815    -247.958    195.039 
    ##                         month_yearJune_2020     41.718        31.068        0.030     1.343    0.179     -19.198    102.634 
    ##                        month_yearMarch_2020    -41.231        44.741       -0.035    -0.922    0.357    -128.956     46.494 
    ##                        month_yearMarch_2021    -41.799        38.941       -0.022    -1.073    0.283    -118.152     34.553 
    ##                          month_yearMay_2020     36.586        28.580        0.030     1.280    0.201     -19.451     92.623 
    ##                     month_yearNovember_2020    -13.168        33.795       -0.008    -0.390    0.697     -79.432     53.095 
    ##                      month_yearOctober_2020    -31.973        34.677       -0.020    -0.922    0.357     -99.965     36.019 
    ##                    month_yearSeptember_2020      5.889        32.954        0.004     0.179    0.858     -58.725     70.504 
    ##                           reviews_per_month     -8.645         3.576       -0.046    -2.417    0.016     -15.657     -1.633 
    ##                       host_is_superhostTRUE    -15.418        14.617       -0.020    -1.055    0.292     -44.079     13.242 
    ##                              minimum_nights     -0.093         0.296       -0.006    -0.313    0.754      -0.673      0.487 
    ##                              maximum_nights      0.009         0.013        0.012     0.675    0.500      -0.017      0.035 
    ##                          host_response_rate     -0.471         0.365       -0.054    -1.293    0.196      -1.186      0.243 
    ##                   review_scores_cleanliness      9.429         7.286        0.024     1.294    0.196      -4.858     23.715 
    ## ----------------------------------------------------------------------------------------------------------------------------
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
    ## R                       0.187       RMSE                  367.686 
    ## R-Squared               0.035       Coef. Var             241.146 
    ## Adj. R-Squared          0.027       MSE                135192.665 
    ## Pred R-Squared          0.019       MAE                    83.949 
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
    ## Regression     15194205.140          25     607768.206    4.496    0.0000 
    ## Residual      421125149.996        3115     135192.665                    
    ## Total         436319355.135        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                     Parameter Estimates                                                      
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower      upper 
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     11.795        77.869                  0.151    0.880    -140.886    164.475 
    ##                                    bedrooms     62.799        10.077        0.129     6.232    0.000      43.041     82.557 
    ##                                   bathrooms     30.077        14.278        0.043     2.106    0.035       2.081     58.073 
    ##   neighbourhood_cleansedBay Street Corridor     12.211        18.946        0.014     0.645    0.519     -24.937     49.358 
    ## neighbourhood_cleansedChurch-Yonge Corridor     39.687        19.226        0.043     2.064    0.039       1.990     77.383 
    ##  neighbourhood_cleansedKensington-Chinatown    -41.776        18.964       -0.047    -2.203    0.028     -78.959     -4.594 
    ##            neighbourhood_cleansedUniversity    -32.537        26.793       -0.024    -1.214    0.225     -85.071     19.997 
    ##                        month_yearApril_2021    -20.197        36.088       -0.012    -0.560    0.576     -90.957     50.562 
    ##                       month_yearAugust_2020      4.075        34.684        0.003     0.117    0.906     -63.930     72.080 
    ##                     month_yearDecember_2020    -20.393        35.335       -0.012    -0.577    0.564     -89.675     48.890 
    ##                     month_yearFebruary_2020    -14.185        44.620       -0.012    -0.318    0.751    -101.672     73.302 
    ##                     month_yearFebruary_2021    -42.283        36.700       -0.024    -1.152    0.249    -114.242     29.676 
    ##                      month_yearJanuary_2021    -39.007        36.127       -0.023    -1.080    0.280    -109.842     31.829 
    ##                         month_yearJuly_2020    -27.479       112.904       -0.004    -0.243    0.808    -248.853    193.896 
    ##                         month_yearJune_2020     41.719        31.063        0.030     1.343    0.179     -19.188    102.625 
    ##                        month_yearMarch_2020    -41.410        44.731       -0.035    -0.926    0.355    -129.115     46.295 
    ##                        month_yearMarch_2021    -43.203        38.676       -0.023    -1.117    0.264    -119.037     32.631 
    ##                          month_yearMay_2020     36.521        28.575        0.030     1.278    0.201     -19.507     92.548 
    ##                     month_yearNovember_2020    -13.395        33.783       -0.009    -0.397    0.692     -79.633     52.844 
    ##                      month_yearOctober_2020    -32.142        34.668       -0.020    -0.927    0.354    -100.116     35.833 
    ##                    month_yearSeptember_2020      5.886        32.950        0.004     0.179    0.858     -58.719     70.491 
    ##                           reviews_per_month     -8.494         3.543       -0.045    -2.397    0.017     -15.442     -1.547 
    ##                       host_is_superhostTRUE    -15.704        14.587       -0.021    -1.077    0.282     -44.305     12.896 
    ##                              maximum_nights      0.009         0.013        0.012     0.661    0.509      -0.017      0.035 
    ##                          host_response_rate     -0.475         0.364       -0.054    -1.303    0.193      -1.189      0.240 
    ##                   review_scores_cleanliness      9.370         7.283        0.024     1.287    0.198      -4.910     23.650 
    ## ----------------------------------------------------------------------------------------------------------------------------
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
    ## R                       0.186       RMSE                  367.652 
    ## R-Squared               0.035       Coef. Var             241.125 
    ## Adj. R-Squared          0.027       MSE                135168.244 
    ## Pred R-Squared          0.020       MAE                    83.665 
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
    ## Regression     15135105.892          24     630629.412    4.666    0.0000 
    ## Residual      421184249.243        3116     135168.244                    
    ## Total         436319355.135        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                     Parameter Estimates                                                      
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower      upper 
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     19.768        76.923                  0.257    0.797    -131.057    170.592 
    ##                                    bedrooms     62.778        10.076        0.129     6.231    0.000      43.022     82.533 
    ##                                   bathrooms     30.205        14.276        0.044     2.116    0.034       2.214     58.196 
    ##   neighbourhood_cleansedBay Street Corridor     12.016        18.942        0.013     0.634    0.526     -25.123     49.155 
    ## neighbourhood_cleansedChurch-Yonge Corridor     39.611        19.224        0.043     2.061    0.039       1.918     77.303 
    ##  neighbourhood_cleansedKensington-Chinatown    -42.214        18.950       -0.047    -2.228    0.026     -79.370     -5.058 
    ##            neighbourhood_cleansedUniversity    -31.908        26.774       -0.023    -1.192    0.233     -84.404     20.589 
    ##                        month_yearApril_2021    -20.436        36.083       -0.012    -0.566    0.571     -91.186     50.313 
    ##                       month_yearAugust_2020      3.836        34.679        0.002     0.111    0.912     -64.159     71.832 
    ##                     month_yearDecember_2020    -20.396        35.332       -0.012    -0.577    0.564     -89.672     48.880 
    ##                     month_yearFebruary_2020    -15.933        44.537       -0.014    -0.358    0.721    -103.258     71.393 
    ##                     month_yearFebruary_2021    -42.642        36.693       -0.024    -1.162    0.245    -114.586     29.303 
    ##                      month_yearJanuary_2021    -39.151        36.123       -0.023    -1.084    0.279    -109.979     31.677 
    ##                         month_yearJuly_2020    -27.609       112.894       -0.004    -0.245    0.807    -248.963    193.746 
    ##                         month_yearJune_2020     41.462        31.058        0.030     1.335    0.182     -19.435    102.359 
    ##                        month_yearMarch_2020    -43.256        44.640       -0.037    -0.969    0.333    -130.782     44.271 
    ##                        month_yearMarch_2021    -42.934        38.671       -0.023    -1.110    0.267    -118.757     32.889 
    ##                          month_yearMay_2020     36.399        28.572        0.030     1.274    0.203     -19.622     92.420 
    ##                     month_yearNovember_2020    -14.221        33.756       -0.009    -0.421    0.674     -80.408     51.966 
    ##                      month_yearOctober_2020    -31.841        34.662       -0.020    -0.919    0.358     -99.803     36.121 
    ##                    month_yearSeptember_2020      6.222        32.943        0.004     0.189    0.850     -58.370     70.813 
    ##                           reviews_per_month     -8.785         3.516       -0.047    -2.499    0.013     -15.678     -1.891 
    ##                       host_is_superhostTRUE    -15.559        14.584       -0.021    -1.067    0.286     -44.154     13.036 
    ##                          host_response_rate     -0.499         0.363       -0.057    -1.376    0.169      -1.210      0.212 
    ##                   review_scores_cleanliness      9.449         7.281        0.025     1.298    0.194      -4.827     23.726 
    ## ----------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## - month_year 
    ## 
    ## Backward Elimination: Step 4 
    ## 
    ##  Variable month_year Removed 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.173       RMSE                  367.725 
    ## R-Squared               0.030       Coef. Var             241.172 
    ## Adj. R-Squared          0.027       MSE                135221.628 
    ## Pred R-Squared          0.023       MAE                    83.046 
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
    ## Regression     13075661.027          10    1307566.103     9.67    0.0000 
    ## Residual      423243694.109        3130     135221.628                    
    ## Total         436319355.135        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                     Parameter Estimates                                                      
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower      upper 
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     -0.816        69.470                 -0.012    0.991    -137.028    135.397 
    ##                                    bedrooms     59.796         9.995        0.122     5.983    0.000      40.198     79.393 
    ##                                   bathrooms     32.879        14.134        0.047     2.326    0.020       5.167     60.592 
    ##   neighbourhood_cleansedBay Street Corridor     12.738        18.925        0.014     0.673    0.501     -24.369     49.844 
    ## neighbourhood_cleansedChurch-Yonge Corridor     38.125        19.196        0.041     1.986    0.047       0.488     75.763 
    ##  neighbourhood_cleansedKensington-Chinatown    -43.278        18.911       -0.048    -2.288    0.022     -80.357     -6.199 
    ##            neighbourhood_cleansedUniversity    -33.854        26.621       -0.025    -1.272    0.204     -86.050     18.342 
    ##                           reviews_per_month     -7.579         3.417       -0.040    -2.218    0.027     -14.279     -0.879 
    ##                       host_is_superhostTRUE    -17.078        14.439       -0.023    -1.183    0.237     -45.388     11.233 
    ##                          host_response_rate     -0.239         0.156       -0.027    -1.532    0.126      -0.544      0.067 
    ##                   review_scores_cleanliness      8.566         7.237        0.022     1.184    0.237      -5.624     22.755 
    ## ----------------------------------------------------------------------------------------------------------------------------
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
    ## - month_year 
    ## 
    ## 
    ## Final Model Output 
    ## ------------------
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.173       RMSE                  367.725 
    ## R-Squared               0.030       Coef. Var             241.172 
    ## Adj. R-Squared          0.027       MSE                135221.628 
    ## Pred R-Squared          0.023       MAE                    83.046 
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
    ## Regression     13075661.027          10    1307566.103     9.67    0.0000 
    ## Residual      423243694.109        3130     135221.628                    
    ## Total         436319355.135        3140                                   
    ## --------------------------------------------------------------------------
    ## 
    ##                                                     Parameter Estimates                                                      
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower      upper 
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     -0.816        69.470                 -0.012    0.991    -137.028    135.397 
    ##                                    bedrooms     59.796         9.995        0.122     5.983    0.000      40.198     79.393 
    ##                                   bathrooms     32.879        14.134        0.047     2.326    0.020       5.167     60.592 
    ##   neighbourhood_cleansedBay Street Corridor     12.738        18.925        0.014     0.673    0.501     -24.369     49.844 
    ## neighbourhood_cleansedChurch-Yonge Corridor     38.125        19.196        0.041     1.986    0.047       0.488     75.763 
    ##  neighbourhood_cleansedKensington-Chinatown    -43.278        18.911       -0.048    -2.288    0.022     -80.357     -6.199 
    ##            neighbourhood_cleansedUniversity    -33.854        26.621       -0.025    -1.272    0.204     -86.050     18.342 
    ##                           reviews_per_month     -7.579         3.417       -0.040    -2.218    0.027     -14.279     -0.879 
    ##                       host_is_superhostTRUE    -17.078        14.439       -0.023    -1.183    0.237     -45.388     11.233 
    ##                          host_response_rate     -0.239         0.156       -0.027    -1.532    0.126      -0.544      0.067 
    ##                   review_scores_cleanliness      8.566         7.237        0.022     1.184    0.237      -5.624     22.755 
    ## ----------------------------------------------------------------------------------------------------------------------------

    ## 
    ## 
    ##                                    Elimination Summary                                    
    ## -----------------------------------------------------------------------------------------
    ##         Variable                              Adj.                                           
    ## Step          Removed           R-Square    R-Square     C(p)         AIC          RMSE      
    ## -----------------------------------------------------------------------------------------
    ##    1    review_scores_rating      0.0349      0.0268    -5.9853    46052.7716    367.7388    
    ##    2    minimum_nights            0.0348      0.0271    -7.8873    46050.8705    367.6856    
    ##    3    maximum_nights            0.0347      0.0273    -9.4504    46049.3113    367.6523    
    ##    4    month_year                 0.030      0.0269     3.7738    46036.6323    367.7249    
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
    ## R                       0.193       RMSE                  397.169 
    ## R-Squared               0.037       Coef. Var             253.597 
    ## Adj. R-Squared          0.037       MSE                157743.051 
    ## Pred R-Squared          0.034       MAE                    93.783 
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
    ## Regression      44051960.516           1    44051960.516    279.264    0.0000 
    ## Residual      1142532916.676        7243      157743.051                      
    ## Total         1186584877.192        7244                                      
    ## ------------------------------------------------------------------------------
    ## 
    ##                                     Parameter Estimates                                     
    ## -------------------------------------------------------------------------------------------
    ##       model       Beta    Std. Error    Std. Beta      t        Sig       lower      upper 
    ## -------------------------------------------------------------------------------------------
    ## (Intercept)    -14.059        11.229                 -1.252    0.211    -36.070      7.952 
    ##   bathrooms    138.842         8.308        0.193    16.711    0.000    122.555    155.129 
    ## -------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## Forward Selection: Step 2 
    ## 
    ## - bedrooms 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.207       RMSE                  411.763 
    ## R-Squared               0.043       Coef. Var             255.972 
    ## Adj. R-Squared          0.043       MSE                169548.540 
    ## Pred R-Squared          0.039       MAE                    96.619 
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
    ## Regression      50694133.856           2    25347066.928    149.497    0.0000 
    ## Residual      1132414698.750        6679      169548.540                      
    ## Total         1183108832.607        6681                                      
    ## ------------------------------------------------------------------------------
    ## 
    ##                                     Parameter Estimates                                     
    ## -------------------------------------------------------------------------------------------
    ##       model       Beta    Std. Error    Std. Beta      t        Sig       lower      upper 
    ## -------------------------------------------------------------------------------------------
    ## (Intercept)    -28.862        12.214                 -2.363    0.018    -52.806     -4.918 
    ##   bathrooms    101.967        10.276        0.140     9.923    0.000     81.822    122.111 
    ##    bedrooms     49.989         7.483        0.095     6.680    0.000     35.319     64.658 
    ## -------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## Forward Selection: Step 3 
    ## 
    ## - neighbourhood_cleansed 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.216       RMSE                  411.096 
    ## R-Squared               0.047       Coef. Var             255.557 
    ## Adj. R-Squared          0.046       MSE                168999.739 
    ## Pred R-Squared          0.042       MAE                    97.041 
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
    ## Regression      55035577.486           6    9172596.248    54.276    0.0000 
    ## Residual      1128073255.121        6675     168999.739                     
    ## Total         1183108832.607        6681                                    
    ## ----------------------------------------------------------------------------
    ## 
    ##                                                     Parameter Estimates                                                      
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig        lower      upper 
    ## ----------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     -7.990        15.295                 -0.522    0.601     -37.972     21.992 
    ##                                   bathrooms    102.136        10.318        0.141     9.898    0.000      81.908    122.363 
    ##                                    bedrooms     52.091         7.497        0.099     6.948    0.000      37.394     66.788 
    ##   neighbourhood_cleansedBay Street Corridor    -12.559        14.674       -0.012    -0.856    0.392     -41.325     16.207 
    ## neighbourhood_cleansedChurch-Yonge Corridor    -10.725        14.566       -0.011    -0.736    0.462     -39.279     17.829 
    ##  neighbourhood_cleansedKensington-Chinatown    -62.182        14.653       -0.061    -4.244    0.000     -90.908    -33.457 
    ##            neighbourhood_cleansedUniversity    -61.729        19.950       -0.041    -3.094    0.002    -100.837    -22.622 
    ## ----------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## Forward Selection: Step 4 
    ## 
    ## - host_response_rate 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.161       RMSE                  385.926 
    ## R-Squared               0.026       Coef. Var             241.645 
    ## Adj. R-Squared          0.024       MSE                148939.028 
    ## Pred R-Squared          0.021       MAE                    90.609 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                    ANOVA                                    
    ## ---------------------------------------------------------------------------
    ##                      Sum of                                                
    ##                     Squares          DF    Mean Square      F         Sig. 
    ## ---------------------------------------------------------------------------
    ## Regression     15493889.626           7    2213412.804    14.861    0.0000 
    ## Residual      579074941.555        3888     148939.028                     
    ## Total         594568831.180        3895                                    
    ## ---------------------------------------------------------------------------
    ## 
    ##                                                     Parameter Estimates                                                     
    ## ---------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig       lower      upper 
    ## ---------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     62.587        21.088                  2.968    0.003     21.243    103.931 
    ##                                   bathrooms     51.311        12.821        0.075     4.002    0.000     26.174     76.448 
    ##                                    bedrooms     47.371         9.175        0.098     5.163    0.000     29.383     65.359 
    ##   neighbourhood_cleansedBay Street Corridor     11.689        17.704        0.013     0.660    0.509    -23.021     46.399 
    ## neighbourhood_cleansedChurch-Yonge Corridor     21.361        18.258        0.022     1.170    0.242    -14.434     57.157 
    ##  neighbourhood_cleansedKensington-Chinatown    -42.810        17.762       -0.046    -2.410    0.016    -77.634     -7.986 
    ##            neighbourhood_cleansedUniversity    -41.995        25.227       -0.029    -1.665    0.096    -91.454      7.465 
    ##                          host_response_rate     -0.300         0.147       -0.032    -2.039    0.041     -0.589     -0.012 
    ## ---------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## Forward Selection: Step 5 
    ## 
    ## - reviews_per_month 
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.171       RMSE                  365.838 
    ## R-Squared               0.029       Coef. Var             240.239 
    ## Adj. R-Squared          0.027       MSE                133837.375 
    ## Pred R-Squared          0.023       MAE                    82.225 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                    ANOVA                                    
    ## ---------------------------------------------------------------------------
    ##                      Sum of                                                
    ##                     Squares          DF    Mean Square      F         Sig. 
    ## ---------------------------------------------------------------------------
    ## Regression     12723865.389           8    1590483.174    11.884    0.0000 
    ## Residual      423862966.087        3167     133837.375                     
    ## Total         436586831.476        3175                                    
    ## ---------------------------------------------------------------------------
    ## 
    ##                                                     Parameter Estimates                                                     
    ## ---------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig       lower      upper 
    ## ---------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     74.866        23.698                  3.159    0.002     28.401    121.331 
    ##                                   bathrooms     32.739        13.911        0.048     2.353    0.019      5.463     60.015 
    ##                                    bedrooms     57.823         9.815        0.121     5.891    0.000     38.579     77.067 
    ##   neighbourhood_cleansedBay Street Corridor     11.783        18.672        0.013     0.631    0.528    -24.828     48.393 
    ## neighbourhood_cleansedChurch-Yonge Corridor     37.650        19.048        0.041     1.977    0.048      0.303     74.997 
    ##  neighbourhood_cleansedKensington-Chinatown    -45.170        18.654       -0.051    -2.421    0.016    -81.745     -8.595 
    ##            neighbourhood_cleansedUniversity    -37.416        26.124       -0.027    -1.432    0.152    -88.638     13.806 
    ##                          host_response_rate     -0.235         0.153       -0.027    -1.537    0.124     -0.536      0.065 
    ##                           reviews_per_month     -7.466         3.274       -0.040    -2.280    0.023    -13.885     -1.047 
    ## ---------------------------------------------------------------------------------------------------------------------------
    ## 
    ## 
    ## 
    ## No more variables to be added.
    ## 
    ## Variables Entered: 
    ## 
    ## + bathrooms 
    ## + bedrooms 
    ## + neighbourhood_cleansed 
    ## + host_response_rate 
    ## + reviews_per_month 
    ## 
    ## 
    ## Final Model Output 
    ## ------------------
    ## 
    ##                           Model Summary                            
    ## ------------------------------------------------------------------
    ## R                       0.171       RMSE                  365.838 
    ## R-Squared               0.029       Coef. Var             240.239 
    ## Adj. R-Squared          0.027       MSE                133837.375 
    ## Pred R-Squared          0.023       MAE                    82.225 
    ## ------------------------------------------------------------------
    ##  RMSE: Root Mean Square Error 
    ##  MSE: Mean Square Error 
    ##  MAE: Mean Absolute Error 
    ## 
    ##                                    ANOVA                                    
    ## ---------------------------------------------------------------------------
    ##                      Sum of                                                
    ##                     Squares          DF    Mean Square      F         Sig. 
    ## ---------------------------------------------------------------------------
    ## Regression     12723865.389           8    1590483.174    11.884    0.0000 
    ## Residual      423862966.087        3167     133837.375                     
    ## Total         436586831.476        3175                                    
    ## ---------------------------------------------------------------------------
    ## 
    ##                                                     Parameter Estimates                                                     
    ## ---------------------------------------------------------------------------------------------------------------------------
    ##                                       model       Beta    Std. Error    Std. Beta      t        Sig       lower      upper 
    ## ---------------------------------------------------------------------------------------------------------------------------
    ##                                 (Intercept)     74.866        23.698                  3.159    0.002     28.401    121.331 
    ##                                   bathrooms     32.739        13.911        0.048     2.353    0.019      5.463     60.015 
    ##                                    bedrooms     57.823         9.815        0.121     5.891    0.000     38.579     77.067 
    ##   neighbourhood_cleansedBay Street Corridor     11.783        18.672        0.013     0.631    0.528    -24.828     48.393 
    ## neighbourhood_cleansedChurch-Yonge Corridor     37.650        19.048        0.041     1.977    0.048      0.303     74.997 
    ##  neighbourhood_cleansedKensington-Chinatown    -45.170        18.654       -0.051    -2.421    0.016    -81.745     -8.595 
    ##            neighbourhood_cleansedUniversity    -37.416        26.124       -0.027    -1.432    0.152    -88.638     13.806 
    ##                          host_response_rate     -0.235         0.153       -0.027    -1.537    0.124     -0.536      0.065 
    ##                           reviews_per_month     -7.466         3.274       -0.040    -2.280    0.023    -13.885     -1.047 
    ## ---------------------------------------------------------------------------------------------------------------------------

    ## 
    ##                                       Selection Summary                                        
    ## ----------------------------------------------------------------------------------------------
    ##         Variable                                Adj.                                              
    ## Step           Entered            R-Square    R-Square      C(p)           AIC          RMSE      
    ## ----------------------------------------------------------------------------------------------
    ##    1    bathrooms                   0.0371      0.0370    1205.0255    107277.8152    397.1688    
    ##    2    bedrooms                    0.0428      0.0426    1695.2279     99424.9512    411.7627    
    ##    3    neighbourhood_cleansed      0.0465      0.0457    1665.1344     99407.2845    411.0958    
    ##    4    host_response_rate          0.0261      0.0243     394.7360     57472.7556    385.9262    
    ##    5    reviews_per_month           0.0291      0.0267     -30.6483     46514.7980    365.8379    
    ## ----------------------------------------------------------------------------------------------

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
    ## -334.0  -71.1  -28.7   17.1 9790.7 
    ## 
    ## Coefficients:
    ##                                             Estimate Std. Error t value
    ## (Intercept)                                  19.7675    76.9230   0.257
    ## bedrooms                                     62.7776    10.0758   6.231
    ## bathrooms                                    30.2047    14.2758   2.116
    ## neighbourhood_cleansedBay Street Corridor    12.0160    18.9416   0.634
    ## neighbourhood_cleansedChurch-Yonge Corridor  39.6107    19.2238   2.061
    ## neighbourhood_cleansedKensington-Chinatown  -42.2139    18.9503  -2.228
    ## neighbourhood_cleansedUniversity            -31.9076    26.7738  -1.192
    ## month_yearApril_2021                        -20.4364    36.0833  -0.566
    ## month_yearAugust_2020                         3.8365    34.6786   0.111
    ## month_yearDecember_2020                     -20.3959    35.3319  -0.577
    ## month_yearFebruary_2020                     -15.9328    44.5374  -0.358
    ## month_yearFebruary_2021                     -42.6415    36.6930  -1.162
    ## month_yearJanuary_2021                      -39.1511    36.1233  -1.084
    ## month_yearJuly_2020                         -27.6086   112.8941  -0.245
    ## month_yearJune_2020                          41.4621    31.0583   1.335
    ## month_yearMarch_2020                        -43.2558    44.6399  -0.969
    ## month_yearMarch_2021                        -42.9339    38.6709  -1.110
    ## month_yearMay_2020                           36.3992    28.5716   1.274
    ## month_yearNovember_2020                     -14.2210    33.7565  -0.421
    ## month_yearOctober_2020                      -31.8413    34.6617  -0.919
    ## month_yearSeptember_2020                      6.2218    32.9427   0.189
    ## reviews_per_month                            -8.7847     3.5157  -2.499
    ## host_is_superhostTRUE                       -15.5589    14.5837  -1.067
    ## host_response_rate                           -0.4988     0.3625  -1.376
    ## review_scores_cleanliness                     9.4492     7.2812   1.298
    ##                                             Pr(>|t|)    
    ## (Intercept)                                   0.7972    
    ## bedrooms                                    5.27e-10 ***
    ## bathrooms                                     0.0344 *  
    ## neighbourhood_cleansedBay Street Corridor     0.5259    
    ## neighbourhood_cleansedChurch-Yonge Corridor   0.0394 *  
    ## neighbourhood_cleansedKensington-Chinatown    0.0260 *  
    ## neighbourhood_cleansedUniversity              0.2335    
    ## month_yearApril_2021                          0.5712    
    ## month_yearAugust_2020                         0.9119    
    ## month_yearDecember_2020                       0.5638    
    ## month_yearFebruary_2020                       0.7206    
    ## month_yearFebruary_2021                       0.2453    
    ## month_yearJanuary_2021                        0.2785    
    ## month_yearJuly_2020                           0.8068    
    ## month_yearJune_2020                           0.1820    
    ## month_yearMarch_2020                          0.3326    
    ## month_yearMarch_2021                          0.2670    
    ## month_yearMay_2020                            0.2028    
    ## month_yearNovember_2020                       0.6736    
    ## month_yearOctober_2020                        0.3584    
    ## month_yearSeptember_2020                      0.8502    
    ## reviews_per_month                             0.0125 *  
    ## host_is_superhostTRUE                         0.2861    
    ## host_response_rate                            0.1690    
    ## review_scores_cleanliness                     0.1945    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 367.7 on 3116 degrees of freedom
    ##   (4683 observations deleted due to missingness)
    ## Multiple R-squared:  0.03469,    Adjusted R-squared:  0.02725 
    ## F-statistic: 4.666 on 24 and 3116 DF,  p-value: 4.812e-13

We can see that the variables:

> bedrooms

> bathrooms

> neighbourhood\_cleansedChurch-Yonge Corridor

> neighbourhood\_cleansedKensington-Chinatown

> month\_yearJuly\_2020

> reviews\_per\_month

are all significant at the 0.05 significance level.
